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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.sis.model.beans.AssessmentDefinition;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAssessment;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports Student Assessment information for BC's GDE. The export only includes records from 3
 * assessment definitions:
 * BC - FSA Grade 4, BC - FSA Grade 7, and Provincial Exam.
 * <p>
 * Test Name
 * <ul>
 * <li>For FSA assessment, either "BC - FSA Grade 4" or "BC - FSA Grade 7"
 * <li>For Provincial Exam assessment, the course name
 * </ul>
 * Score Type Column
 * <ul>
 * <li>Score Type, if an FSA assessment, will always be "Score"
 * <li>Score Type, if a Provincial Exam assessment, will be one of: "Blended" or "Exam"
 * </ul>
 * Score column
 * <ul>
 * <li>If FSA assessment will be FieldA002
 * <li>If Provincial Exam assessment, display transcript Exam mark and transcript Blended mark (note
 * these marks are
 * 3 characters so left pad with zero (e.g. 083)
 * </ul>
 * Test Date - change Test Date format to dd-mon-yyyy
 * <ul>
 * </ul>
 * Test Grade - the student's current grade (always two characters, so left pad with zero if
 * required)
 * <ul>
 * </ul>
 * Subtest Sequence
 * <ul>
 * <li>For Provincial Exam will always be 1
 * <li>For FSA, display 1 for Reading, 2 for Writing, 3 for Numeracy
 * </ul>
 * Subtest Name
 * <ul>
 * <li>For FSA, will be pulled from the Subtest field (see figure above)
 * <li>For Provincial Exam assessment, the course name
 * </ul>
 *
 * @author Follett Software Company
 */
public class StandardizedTestsExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Grid fields
    private static final String FIELD_STUDENT_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_HOMEROOM = "Homeroom";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_TEST_NAME = "Standardized Test Name";
    private static final String FIELD_SCORE_TYPE = "Score Type";
    private static final String FIELD_SCORE = "Score";
    private static final String FIELD_TEST_DATE = "Test Date";
    private static final String FIELD_TEST_GRADE = "Test Grade";
    private static final String FIELD_SUBTEST_SEQUENCE = "Subtest Sequence";
    private static final String FIELD_SUBTEST_NAME = "Subtest Name";

    // Assessment definition names
    private static final String ASSESSMENT_FSA_4 = "BC - FSA Grade 4";
    private static final String ASSESSMENT_FSA_7 = "BC - FSA Grade 7";
    private static final String ASSESSMENT_PROVINCIAL = "Provincial Exam";

    // Aliases
    private static final String ALIAS_BLEND_SCORE_1 = "blendedMark1";
    private static final String ALIAS_BLEND_SCORE_2 = "blendedMark2";
    private static final String ALIAS_EXAM_SCORE_1 = "examMark1";
    private static final String ALIAS_EXAM_SCORE_2 = "examMark2";
    private static final String ALIAS_FSA4_SCORE = "asm-fsa-grade4-scr";
    private static final String ALIAS_FSA7_SCORE = "asm-fsa-grade7-scr";
    private static final String ALIAS_SUBTEST = "asm-subtest-type";

    // Subtest constants
    private static final String SUBTEST_SEQUENCE_1 = "1";

    /**
     * The Enum SUBTEST_TYPES.
     */
    private enum SUBTEST_TYPES {
        READING, WRITING, NUMERACY
    }

    // Other constants
    private static final int FIELD_COUNT = 13;
    private static final String SCORE_TYPE_BLEND = "Blended";
    private static final String SCORE_TYPE_EXAM = "Exam";
    private static final String SCORE_TYPE_SCORE = "Score";

    private List<String> m_columns;
    private DateFormat m_dateFormat;
    private Map<String, DataDictionary> m_dictionaryMap;

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

        QueryByCriteria queryByCriteria = new QueryByCriteria(StudentAssessment.class, buildCriteria());
        QueryIterator tests = getBroker().getIteratorByQuery(queryByCriteria);
        try {
            while (tests.hasNext()) {
                StudentAssessment test = (StudentAssessment) tests.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = test.getStudent();

                    AssessmentDefinition definition = test.getAssessmentDefinition();
                    DataDictionary dictionary = getDictionary(definition);

                    if (student != null) {
                        if (ASSESSMENT_FSA_4.equals(definition.getName())) {
                            grid.append();
                            deleteRow = true;

                            setStudentFields(grid, student);
                            setFsaFields(grid, test, definition);

                            grid.set(FIELD_SCORE, test.getFieldValueByAlias(ALIAS_FSA4_SCORE, dictionary));

                        } else if (ASSESSMENT_FSA_7.equals(definition.getName())) {
                            grid.append();
                            deleteRow = true;

                            setStudentFields(grid, student);
                            setFsaFields(grid, test, definition);

                            grid.set(FIELD_SCORE, test.getFieldValueByAlias(ALIAS_FSA7_SCORE, dictionary));
                        } else {
                            String blendedScore =
                                    getHighScore((String) test.getFieldValueByAlias(ALIAS_BLEND_SCORE_1, dictionary),
                                            (String) test.getFieldValueByAlias(ALIAS_BLEND_SCORE_2, dictionary));
                            String examScore =
                                    getHighScore((String) test.getFieldValueByAlias(ALIAS_EXAM_SCORE_1, dictionary),
                                            (String) test.getFieldValueByAlias(ALIAS_EXAM_SCORE_2, dictionary));

                            /*
                             * Provincial exam
                             */
                            grid.append();
                            deleteRow = true;

                            setStudentFields(grid, student);
                            setProvincialFields(grid, test, definition, true);

                            if (!StringUtils.isEmpty(examScore)) {
                                grid.set(FIELD_SCORE, StringUtils.padLeft(examScore, 3, '0'));
                            }

                            /*
                             * Provincial blended
                             */
                            grid.append();
                            deleteRow = true;

                            setStudentFields(grid, student);
                            setProvincialFields(grid, test, definition, false);

                            if (!StringUtils.isEmpty(blendedScore)) {
                                grid.set(FIELD_SCORE, StringUtils.padLeft(blendedScore, 3, '0'));
                            }
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(queryByCriteria.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(test.getOid());
                    SisStudent student = test.getStudent();
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
            tests.close();
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
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
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

        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STUDENT_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_HOMEROOM);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_TEST_NAME);
        m_columns.add(FIELD_SCORE_TYPE);
        m_columns.add(FIELD_SCORE);
        m_columns.add(FIELD_TEST_DATE);
        m_columns.add(FIELD_TEST_GRADE);
        m_columns.add(FIELD_SUBTEST_SEQUENCE);
        m_columns.add(FIELD_SUBTEST_NAME);

        m_dictionaryMap = new HashMap<String, DataDictionary>(32);
        m_dateFormat = new SimpleDateFormat("dd-MMM-yyyy");
    }

    /**
     * Builds the criteria for pulling the student assessment records for the current school year.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(StudentAssessment.REL_SCHOOL,
                StudentAssessment.COL_SCHOOL_OID));

        /*
         * Only include specific assessment definitions
         */
        List<String> assessmentDefinitions = Arrays.asList(ASSESSMENT_FSA_4, ASSESSMENT_FSA_7, ASSESSMENT_PROVINCIAL);
        criteria.addIn(StudentAssessment.REL_ASSESSMENT_DEFINITION + ModelProperty.PATH_DELIMITER +
                AssessmentDefinition.COL_NAME, assessmentDefinitions);

        /*
         * Restrict by date range of the current context
         */
        DistrictSchoolYearContext context = getCurrentContext();
        criteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, context.getStartDate());
        criteria.addLessOrEqualThan(StudentAssessment.COL_DATE, context.getEndDate());

        return criteria;
    }

    /**
     * Formats the date to the BC format (dd-MMM-yyyy with all uppercase).
     *
     * @param date PlainDate
     * @return String
     */
    private String formatDate(PlainDate date) {
        String value = "";

        if (date != null) {
            value = m_dateFormat.format(date).toUpperCase();
        }

        return value;
    }

    /**
     * Returns the associated school course with the assessment. If there is an associated section,
     * the course on the
     * section is used. Otherwise use the related school course.
     *
     * @param test StudentAssessment
     * @return SchoolCourse
     */
    private SchoolCourse getCourse(StudentAssessment test) {
        SchoolCourse course = null;

        if (test.getMasterSchedule() != null) {
            course = test.getMasterSchedule().getSchoolCourse();
        } else {
            course = test.getSchoolCourse();
        }

        return course;
    }

    /**
     * Looks up the dictionary for the passed assessment definition, creating a new instance if
     * necessary.
     *
     * @param definition AssessmentDefinition
     * @return DataDictionary
     */
    private DataDictionary getDictionary(AssessmentDefinition definition) {
        DataDictionary dictionary = m_dictionaryMap.get(definition.getOid());

        if (dictionary == null) {
            dictionary = DataDictionary.getDistrictDictionary(definition, getBroker().getPersistenceKey());
            m_dictionaryMap.put(definition.getOid(), dictionary);
        }

        return dictionary;
    }

    /**
     * Compares the 2 strings and returns the higher grade.
     *
     * @param score1 String
     * @param score2 String
     * @return String
     */
    private String getHighScore(String score1, String score2) {
        String formatted1 = StringUtils.padLeft(score1, 3, '0');
        String formatted2 = StringUtils.padLeft(score2, 3, '0');

        int comparison = formatted1.compareTo(formatted2);

        return comparison < 1 ? score2 : score1;
    }

    /**
     * Sets the standard fields for the FSA assessments.
     *
     * @param grid DataGrid
     * @param test StudentAssessment
     * @param definition AssessmentDefinition
     */
    private void setFsaFields(DataGrid grid, StudentAssessment test, AssessmentDefinition definition) {
        grid.set(FIELD_SCORE_TYPE, SCORE_TYPE_SCORE);
        grid.set(FIELD_TEST_DATE, formatDate(test.getDate()));
        grid.set(FIELD_TEST_NAME, test.getAssessmentDefinition().getName());

        /*
         * Set subtest fields
         */
        String subtestName = (String) test.getFieldValueByAlias(ALIAS_SUBTEST);
        if (!StringUtils.isEmpty(subtestName)) {
            grid.set(FIELD_SUBTEST_NAME, subtestName);

            try {
                SUBTEST_TYPES subtest = SUBTEST_TYPES.valueOf(subtestName);
                grid.set(FIELD_SUBTEST_SEQUENCE, String.valueOf(subtest.ordinal()));
            } catch (IllegalArgumentException iae) {
                // Do nothing - just don't set the field
            }
        }
    }

    /**
     * Sets the Provincial Exam related fields on the grid.
     *
     * @param grid DataGrid
     * @param test StudentAssessment
     * @param definition AssessmentDefinition
     * @param exam boolean
     */
    private void setProvincialFields(DataGrid grid,
                                     StudentAssessment test,
                                     AssessmentDefinition definition,
                                     boolean exam) {
        SchoolCourse course = getCourse(test);
        if (course != null) {
            grid.set(FIELD_SUBTEST_NAME, course.getDescription());
            grid.set(FIELD_TEST_NAME, course.getDescription());
        }

        grid.set(FIELD_SCORE_TYPE, exam ? SCORE_TYPE_EXAM : SCORE_TYPE_BLEND);
        grid.set(FIELD_SUBTEST_SEQUENCE, SUBTEST_SEQUENCE_1);
        grid.set(FIELD_TEST_DATE, formatDate(test.getDate()));
    }

    /**
     * Sets the student-related fields on the grid.
     *
     * @param grid DataGrid
     * @param student SisStudent
     */
    private void setStudentFields(DataGrid grid, SisStudent student) {
        String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
        String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

        String homeroom = student.getHomeroom(getCurrentContext().getOid(), getBroker());
        SisSchool school = student.getSchool(getCurrentContext().getOid(), getBroker());

        grid.set(FIELD_STUDENT_ID, student.getLocalId());
        grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
        grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
        grid.set(FIELD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
        grid.set(FIELD_HOMEROOM, homeroom);
        grid.set(FIELD_TEST_GRADE, StringUtils.padLeft(gradeLevel, 2, '0'));

        /*
         * Pull the student's homeroom teacher
         */
        Map<String, Staff> staffMap = getHomeroomToStaffMap(school);
        if (staffMap != null) {
            Staff staff = staffMap.get(homeroom);
            if (staff != null) {
                grid.set(FIELD_TEACHER_NAME, staff.getNameView());
            }
        }
    }
}
