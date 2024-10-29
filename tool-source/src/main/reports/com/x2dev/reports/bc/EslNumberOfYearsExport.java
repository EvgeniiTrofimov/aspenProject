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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports ESL program information for BC's GDE. These are programs with a code of "ESL".
 *
 * @author Follett Software Company
 */
public class EslNumberOfYearsExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_STD_LOCAL_ID = "Student Number";
    private static final String FIELD_STD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STD_GRADE = "Grade";
    private static final String FIELD_STD_HOMEROOM = "Homeroom";
    private static final String FIELD_STD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_PGM_ESL_YEARS = "ESL years";
    private static final String FIELD_PGM_START_DATE = "Start Date";
    private static final String FIELD_PGM_END_DATE = "End Date";

    // Aliases and program codes
    private static final String ALIAS_PGM_ESL_YEARS = "std-years-of-esl";
    private static final String VALUE_PROGRAM_CODE = "1700";

    // Other constants
    private static final int FIELD_COUNT = 9;

    private List<String> m_columns;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MMM-yyyy");
        NumberFormat yearFormatter = new DecimalFormat("0.#");

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

                    grid.append();
                    deleteRow = true;
                    grid.set(FIELD_STD_LOCAL_ID, student.getLocalId());
                    grid.set(FIELD_STD_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_STD_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_STD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_STD_HOMEROOM, student.getHomeroom());
                    grid.set(FIELD_PGM_START_DATE,
                            program.getStartDate() != null ? formatter.format(program.getStartDate()) : "");
                    grid.set(FIELD_PGM_END_DATE,
                            program.getEndDate() != null ? formatter.format(program.getEndDate()) : "");

                    /*
                     * ESL years
                     */
                    String eslYears = (String) student.getFieldValueByAlias(ALIAS_PGM_ESL_YEARS);

                    if (StringUtils.isNumeric(eslYears)) {
                        BigDecimal years = new BigDecimal(eslYears);
                        eslYears = yearFormatter.format(years.doubleValue());
                    }
                    grid.set(FIELD_PGM_ESL_YEARS, eslYears);

                    /*
                     * Set homeroom teacher name
                     */
                    Map<String, Staff> staffMap = getHomeroomToStaffMap(student.getSchool());
                    if (staffMap != null) {
                        Staff staff = staffMap.get(student.getHomeroom());
                        if (staff != null) {
                            grid.set(FIELD_STD_TEACHER_NAME, staff.getNameView());
                        }
                    }
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
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();

        // Set columns
        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STD_LOCAL_ID);
        m_columns.add(FIELD_STD_LAST_NAME);
        m_columns.add(FIELD_STD_FIRST_NAME);
        m_columns.add(FIELD_STD_GRADE);
        m_columns.add(FIELD_STD_HOMEROOM);
        m_columns.add(FIELD_STD_TEACHER_NAME);
        m_columns.add(FIELD_PGM_ESL_YEARS);
        m_columns.add(FIELD_PGM_START_DATE);
        m_columns.add(FIELD_PGM_END_DATE);

    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_SCHOOL,
                StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID));
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getCurrentContext().getEndDate());

        /*
         * Include "ESL" programs
         */
        Collection<String> codes = loadProgramCodes(VALUE_PROGRAM_CODE);
        if (!CollectionUtils.isEmpty(codes)) {
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes);
        } else {
            addNoMatchCriteria(criteria);
        }

        /*
         * Included programs can either have no end date or an end date since the current school
         * year
         */
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        X2Criteria endDateOrCriteria = new X2Criteria();
        endDateOrCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                getCurrentContext().getStartDate());
        endDateCriteria.addOrCriteria(endDateOrCriteria);

        criteria.addAndCriteria(endDateCriteria);

        return criteria;
    }


    /**
     * Loads the program participation program codes with the matching local code. If there are no
     * matching codes
     * and empty collection is returned.
     *
     *
     * @param stateCode String
     * @return Collection<String>
     */
    @SuppressWarnings("unchecked")
    private Collection<String> loadProgramCodes(String stateCode) {
        Collection<String> codes = new LinkedList<>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);

        if (field != null) {
            ReferenceTable table = field.getReferenceTable();
            if (table != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, table.getOid());
                criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, stateCode);

                SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                codes = getBroker().getSubQueryCollectionByQuery(query);
            }
        }

        return codes;
    }
}
