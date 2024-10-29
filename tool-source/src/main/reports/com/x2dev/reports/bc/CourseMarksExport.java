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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.reports.RubricTranscriptReportGrid;
import com.x2dev.sis.tools.reports.TranscriptReportGrid;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports transcript information by term for BC's GDE.
 *
 * @author Follett Software Company
 */
public class CourseMarksExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;
    // Input parameters
    private static final String PARAM_INCL_DL_INFO = "includeDLInfo";

    // Aliasees
    private static final String ALIAS_DL_START_DATE = "trn-start-date";
    private static final String ALIAS_DL_COURSE_DATE = "trn-course-active-date";
    private static final String ALIAS_DL_COMPLETION_DATE = "trn-completion-date";

    // Grid fields
    private static final String FIELD_DISTRICT_NUMBER = "District Number";
    private static final String FIELD_SCHOOL_NUMBER = "School Number";
    private static final String FIELD_SCHOOL_NAME = "School Name";
    private static final String FIELD_MASTER_ID = "Master Timetable ID";
    private static final String FIELD_TEACHER_ID = "Teacher ID";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_STUDENT_LOCAL_ID = "Student Number";
    private static final String FIELD_STUDENT_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUDENT_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STUDENT_GRADE = "Grade";
    private static final String FIELD_STUDENT_HOMEROM = "Homeroom";
    private static final String FIELD_COURSE_CODE = "Course Code";
    private static final String FIELD_REPORTING_DATE = "Reporting Date";
    private static final String FIELD_MARK = "Mark";
    private static final String FIELD_FINAL_MARK = "Final Mark";
    private static final String FIELD_REPORT_CYCLE_DESC = "Report Cycle Description";
    private static final String FIELD_STUDY_HABIT = "Study Habit";
    private static final String FIELD_COMMENTS = "Comments";
    private static final String FIELD_DL_START_DATE = "DL Start Date";
    private static final String FIELD_DL_COURSE_ACTIVE_DATE = "DL Course Active Date";
    private static final String FIELD_DL_COMPLETION_DATE = "DL Completion Date";
    private static final String FIELD_SEMESTER_AVERAGE = "Semester Average";
    private static final String FIELD_EXAM_MARK = "School Final Exam";
    private static final String FIELD_COURSE_DISTRICT_NUMBER = "Course District Number";
    private static final String FIELD_COURSE_SCHOOL_NUMBER = "Course School Number";
    private static final String FIELD_COURSE_SCHOOL_NAME = "Course School Name";

    /*
     * Column header constants
     */
    private static final String HEADER_AVERAGE = "Avg";
    private static final String HEADER_EXAM = "Exam";
    private static final String HEADER_HABIT = "WH";

    /*
     * Other constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";

    private List<String> m_columns;
    private int m_field_count;
    private Map m_transcriptColumns = new HashMap<String, HashMap<String, List>>();
    private Map<String, List> m_gradeTerms;
    private boolean m_includeDlInfo = false;
    private Map<String, GradeTermDate> m_gradeTermDates;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_field_count) {
            private static final long serialVersionUID = 1L;

            @Override
            public void set(String column, Object value) {
                super.set(column, wrap((String) value));
            }
        };

        SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT);

        String[] studentSort = new String[] {Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_SCHOOL_ID,
                Student.COL_LOCAL_ID};
        RubricTranscriptReportGrid transcriptGrid = new RubricTranscriptReportGrid(buildCriteria(),
                studentSort,
                false,
                false,
                false,
                getOrganization(),
                getBroker());

        Transcript lastTranscript = null;
        while (transcriptGrid.next()) {
            Transcript transcript = transcriptGrid.getTranscript();
            boolean deleteRow = false;

            try {
                SisStudent student = transcript.getStudent();
                SisSchool school = transcript.getSchool();
                SchoolCourse course = transcript.getSchoolCourse();

                if (!ObjectUtils.match(transcript, lastTranscript) && student != null && school != null
                        && course != null) {
                    List<GradeTerm> terms = getGradeTerms(transcript.getTranscriptDefinitionOid());
                    for (GradeTerm term : terms) {
                        String dateKey = transcript.getSchoolOid() + term.getOid();
                        GradeTermDate termDate = m_gradeTermDates.get(dateKey);

                        String mark = null;
                        String studyHabit = null;
                        String comment = null;
                        String semesterAverage = null;
                        String examMark = null;

                        HashMap<String, List> termMap = (HashMap<String, List>) m_transcriptColumns
                                .get(transcript.getTranscriptDefinitionOid());
                        List<TranscriptColumnDefinition> columns = termMap.get(term.getOid());

                        /*
                         * Load term grades
                         */
                        if (columns != null) {
                            for (TranscriptColumnDefinition column : columns) {
                                int columnType = column.getColumnTypeCode();
                                String header = column.getGradeColumnHeader();

                                if (columnType == TranscriptColumnDefinition.COLUMN_TYPE_GRADE &&
                                        !header.contains(HEADER_AVERAGE) && !header.contains(HEADER_EXAM)) {
                                    // Term grade
                                    String transFieldName = column.getDataFieldConfig().getDataField().getJavaName();
                                    mark = (String) transcript.getFieldValueByBeanPath(transFieldName);
                                } else if (columnType == TranscriptColumnDefinition.COLUMN_TYPE_REFERENCE
                                        && header.contains(HEADER_HABIT)) {
                                    // Habit of study
                                    String transFieldName = column.getDataFieldConfig().getDataField().getJavaName();
                                    studyHabit = (String) transcript.getFieldValueByBeanPath(transFieldName);
                                } else if (columnType == TranscriptColumnDefinition.COLUMN_TYPE_COMMENT) {
                                    // Term comment
                                    String transFieldName = column.getDataFieldConfig().getDataField().getJavaName();
                                    comment = (String) transcript.getFieldValueByBeanPath(transFieldName);
                                } else if (columnType == TranscriptColumnDefinition.COLUMN_TYPE_GRADE
                                        && header.contains(HEADER_AVERAGE)) {
                                    // Semester average
                                    String transFieldName = column.getDataFieldConfig().getDataField().getJavaName();
                                    semesterAverage = (String) transcript.getFieldValueByBeanPath(transFieldName);
                                } else if (columnType == TranscriptColumnDefinition.COLUMN_TYPE_GRADE
                                        && header.contains(HEADER_EXAM)) {
                                    // Exam
                                    String transFieldName = column.getDataFieldConfig().getDataField().getJavaName();
                                    examMark = (String) transcript.getFieldValueByBeanPath(transFieldName);
                                } else if (columnType == TranscriptColumnDefinition.COLUMN_TYPE_RUBRIC) {
                                    header = TranscriptReportGrid.RUBRIC_FIELD_PREFIX + header;
                                    mark = (String) transcriptGrid.get(header);
                                }
                            }

                            String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                            String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                            /*
                             * Set grid elements
                             */
                            grid.append();
                            deleteRow = true;
                            grid.set(FIELD_MARK, mark);
                            grid.set(FIELD_STUDY_HABIT, studyHabit);
                            grid.set(FIELD_COMMENTS, comment);
                            grid.set(FIELD_SEMESTER_AVERAGE, semesterAverage);
                            grid.set(FIELD_EXAM_MARK, examMark);
                            grid.set(FIELD_REPORT_CYCLE_DESC, term.getGradeTermId());
                            grid.set(FIELD_DISTRICT_NUMBER, school.getParentOrganization().getId());
                            grid.set(FIELD_SCHOOL_NUMBER, school.getSchoolId());
                            grid.set(FIELD_SCHOOL_NAME, school.getName());
                            grid.set(FIELD_STUDENT_LOCAL_ID, student.getLocalId());
                            grid.set(FIELD_STUDENT_LAST_NAME, student.getPerson().getLastName());
                            grid.set(FIELD_STUDENT_FIRST_NAME, student.getPerson().getFirstName());
                            grid.set(FIELD_STUDENT_GRADE,
                                    StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                            grid.set(FIELD_STUDENT_HOMEROM, student.getHomeroom());
                            grid.set(FIELD_COURSE_CODE, course.getNumber());
                            grid.set(FIELD_FINAL_MARK, transcript.getFinalGrade());
                            grid.set(FIELD_COURSE_DISTRICT_NUMBER, course.getSchool().getParentOrganization().getId());
                            grid.set(FIELD_COURSE_SCHOOL_NUMBER, course.getSchool().getSchoolId());
                            grid.set(FIELD_COURSE_SCHOOL_NAME, course.getSchool().getName());
                            grid.set(FIELD_MASTER_ID, transcript.getMasterScheduleOid());

                            if (termDate != null) {
                                grid.set(FIELD_REPORTING_DATE, dateFormat.format(termDate.getEndDate()));
                            }

                            SisStaff staff = transcript.getTeacher();
                            if (staff != null) {
                                grid.set(FIELD_TEACHER_ID, staff.getLocalId());
                                grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                            }

                            if (m_includeDlInfo) {
                                grid.set(FIELD_DL_START_DATE, transcript.getFieldValueByAlias(ALIAS_DL_START_DATE));
                                grid.set(FIELD_DL_COURSE_ACTIVE_DATE,
                                        transcript.getFieldValueByAlias(ALIAS_DL_COURSE_DATE));
                                grid.set(FIELD_DL_COMPLETION_DATE,
                                        transcript.getFieldValueByAlias(ALIAS_DL_COMPLETION_DATE));
                            }
                        }
                    }
                }
            } catch (NullPointerException npe) {
                StringBuilder strBldr = new StringBuilder();
                strBldr.append("Unable to export ");
                strBldr.append(Transcript.class.getName());
                strBldr.append(" with OID: [");
                strBldr.append(transcript.getOid());
                SisStudent student = transcript.getStudent();
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
                    grid.deleteRow(); // Delete the incomplete row that was appended to the grid.
                }

                strBldr.append("\n\n\nNullPointerException: \n");
                strBldr.append(ExceptionUtils.getStackTrace(npe));
                logToolMessage(Level.WARNING, strBldr.toString(), false);
            }

            lastTranscript = transcript;
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
        // Set field count depending on "include Ids" export parameter
        m_includeDlInfo = ((Boolean) getParameter(PARAM_INCL_DL_INFO)).booleanValue();

        // Set columns
        m_columns = new ArrayList<String>();
        m_columns.add(FIELD_DISTRICT_NUMBER);
        m_columns.add(FIELD_SCHOOL_NUMBER);
        m_columns.add(FIELD_SCHOOL_NAME);
        m_columns.add(FIELD_MASTER_ID);
        m_columns.add(FIELD_TEACHER_ID);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_STUDENT_LOCAL_ID);
        m_columns.add(FIELD_STUDENT_LAST_NAME);
        m_columns.add(FIELD_STUDENT_FIRST_NAME);
        m_columns.add(FIELD_STUDENT_GRADE);
        m_columns.add(FIELD_STUDENT_HOMEROM);
        m_columns.add(FIELD_COURSE_CODE);
        m_columns.add(FIELD_REPORTING_DATE);
        m_columns.add(FIELD_MARK);
        m_columns.add(FIELD_FINAL_MARK);
        m_columns.add(FIELD_REPORT_CYCLE_DESC);
        m_columns.add(FIELD_STUDY_HABIT);
        m_columns.add(FIELD_COMMENTS);

        if (m_includeDlInfo) {
            m_columns.add(FIELD_DL_START_DATE);
            m_columns.add(FIELD_DL_COURSE_ACTIVE_DATE);
            m_columns.add(FIELD_DL_COMPLETION_DATE);
        }

        m_columns.add(FIELD_SEMESTER_AVERAGE);
        m_columns.add(FIELD_EXAM_MARK);
        m_columns.add(FIELD_COURSE_DISTRICT_NUMBER);
        m_columns.add(FIELD_COURSE_SCHOOL_NUMBER);
        m_columns.add(FIELD_COURSE_SCHOOL_NAME);

        m_field_count = m_columns.size();

        loadTranscriptDefinitions();
        loadGradeTermDates();
    }

    /**
     * Build the criteria for pulling the transcript records included in the export.
     * 
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        /*
         * Pull both records for the selected schools and records for students in the selected
         * schools
         */
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(Transcript.REL_SCHOOL, Transcript.COL_SCHOOL_OID));

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_SCHOOL,
                Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SCHOOL_OID));

        schoolCriteria.addOrCriteria(studentCriteria);
        criteria.addAndCriteria(schoolCriteria);

        return criteria;
    }

    /**
     * Returns the list of grade terms associated with the passed transcript definition OID. If the
     * OID is empty,
     * an empty list is returned.
     *
     * @param transcriptDefinitionOid String
     * @return List<GradeTerm>
     */
    private List<GradeTerm> getGradeTerms(String transcriptDefinitionOid) {
        List<GradeTerm> terms = new LinkedList<GradeTerm>();

        if (!StringUtils.isEmpty(transcriptDefinitionOid)) {
            terms = m_gradeTerms.get(transcriptDefinitionOid);
        }

        return terms;
    }

    /**
     * Loads the grade terms dates for the current year into a Map keyed to the combination of
     * school/term OIDs.
     */
    private void loadGradeTermDates() {
        m_gradeTermDates = new HashMap<String, GradeTermDate>(4096);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
        query.addOrderByAscending(GradeTermDate.COL_SCHOOL_OID);
        query.addOrderByAscending(GradeTermDate.COL_START_DATE);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                GradeTermDate termDate = (GradeTermDate) iterator.next();
                String key = termDate.getSchoolOid() + termDate.getGradeTermOid();

                m_gradeTermDates.put(key, termDate);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the transcript definitions.
     */
    private void loadTranscriptDefinitions() {
        m_gradeTerms = new HashMap<String, List>(256);

        QueryByCriteria query = new QueryByCriteria(GradeTermDefinition.class);
        List<GradeTermDefinition> definitions = (List<GradeTermDefinition>) getBroker().getCollectionByQuery(query);

        /*
         * For each grade term definition, load the grade terms used for each transcript definition
         */
        for (GradeTermDefinition gradeTermDef : definitions) {
            List<GradeTerm> terms = (List<GradeTerm>) gradeTermDef.getGradeTerms(getBroker());
            List<TranscriptDefinition> transDefs =
                    (List<TranscriptDefinition>) gradeTermDef.getTranscriptDefinitions(getBroker());

            for (TranscriptDefinition transDef : transDefs) {
                m_gradeTerms.put(transDef.getOid(), terms);
            }
        }

        /*
         * Load term columns into a map
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(TranscriptColumnDefinition.COL_REPORT_TYPE,
                Integer.valueOf(TranscriptColumnDefinition.GRADE_TYPE_TERM));

        QueryByCriteria columnQuery = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        String[] columns = new String[] {TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID,
                TranscriptColumnDefinition.COL_GRADE_TERM_OID};

        m_transcriptColumns = getBroker().getGroupedCollectionByQuery(columnQuery, columns, new int[] {50, 50});
    }
}
