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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports Student Course Selection information (current year student schedule records) for BC's
 * GDE.
 *
 * @author Follett Software Company
 */
public class StudentCourseSelectionExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    // Input parameters
    private static final String PARAM_INCL_DL_INFO = "includeDLInfo";
    private static final String PARAM_INCL_IDS = "includeIds";

    // Grid fields
    private static final String FIELD_SKL_YEAR = "School Year";
    private static final String FIELD_SKL_ID = "School Number";
    private static final String FIELD_STUD_ID = "Student Number";
    private static final String FIELD_STUD_LAST_NAME = "Student Legal Last Name";
    private static final String FIELD_STUD_FIRST_NAME = "Student Legal First Name";
    private static final String FIELD_STUD_GRADE = "Grade";
    private static final String FIELD_TEACHER_NAME = "Teacher Name";
    private static final String FIELD_SCHEDULES_SEMESTER = "Semester";
    private static final String FIELD_COURSE_CODE = "Course Code";
    private static final String FIELD_COURSE_CODE_HOW_TAKEN = "How Taken Description";
    private static final String FIELD_COURSE_SECTION_LETTER = "Section";
    private static final String FIELD_TIMETABLE_ID = "Master Timetable ID";
    private static final String FIELD_DL_START_DATE = "DL Start Date";
    private static final String FIELD_DL_COURSE_ACTIVE_DATE = "DL Course Active Date";
    private static final String FIELD_DL_COMPLETION_DATE = "DL Completion Date";

    // Optional fields
    private static final int DL_INFO_FIELD_COUNT = 3;
    private static final int MIN_FIELD_COUNT = 11;

    // Course aliases
    private static final String ALIAS_ACTIVE_DATE = "trn-course-active-date";
    private static final String ALIAS_COMPLETION_DATE = "trn-completion-date";
    private static final String ALIAS_HOW_TAKEN_DESCR = "trn-how-taken";
    private static final String ALIAS_START_DATE = "trn-start-date";

    private List<String> m_columns;
    private DateAsStringConverter m_dateConverter;
    private boolean m_IsIncludeIds;
    private boolean m_IsIncludeDlInfo;

    private Map<String, Transcript> m_transcriptMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
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

        SimpleDateFormat formatter = new SimpleDateFormat("dd-MMM-yyyy");

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, buildCriteria());
        query.addOrderByAscending(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID);
        query.addOrderByAscending(StudentSchedule.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
        query.addOrderByAscending(StudentSchedule.REL_SECTION + PATH_DELIMITER + Section.REL_SCHOOL_COURSE
                + PATH_DELIMITER + SchoolCourse.COL_NUMBER);
        query.addOrderByAscending(StudentSchedule.COL_STUDENT_OID);

        QueryIterator studentSchedules = getBroker().getIteratorByQuery(query);
        try {
            int progressCount = 0;

            while (studentSchedules.hasNext()) {
                StudentSchedule schedule = (StudentSchedule) studentSchedules.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = schedule.getStudent();
                    SisSchool school = schedule.getSchedule().getSchool();
                    MasterSchedule section = schedule.getSection();
                    Transcript transcript = m_transcriptMap.get(buildKey(student, section));

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                    if (++progressCount % 10000 == 0) {
                        AppGlobals.getLog().info("GDE Student Course Selection processed " + progressCount
                                + " student schedule records");
                    }

                    grid.append();
                    deleteRow = true;

                    // Fill grid data list with export information
                    grid.set(FIELD_SKL_YEAR, String.valueOf(getCurrentContext().getSchoolYear()));
                    grid.set(FIELD_SKL_ID, school.getSchoolId());
                    grid.set(FIELD_STUD_ID, student.getLocalId());
                    grid.set(FIELD_STUD_LAST_NAME, student.getPerson().getLastName());
                    grid.set(FIELD_STUD_FIRST_NAME, student.getPerson().getFirstName());
                    grid.set(FIELD_STUD_GRADE, StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(FIELD_SCHEDULES_SEMESTER, schedule.getTermView());
                    grid.set(FIELD_COURSE_CODE, section.getSchoolCourse().getNumber());
                    grid.set(FIELD_COURSE_SECTION_LETTER, section.getSectionNumber());

                    SisStaff staff = section.getPrimaryStaff();
                    if (staff != null) {
                        grid.set(FIELD_TEACHER_NAME, staff.getNameView());
                    }

                    if (transcript != null) {
                        grid.set(FIELD_COURSE_CODE_HOW_TAKEN, transcript.getFieldValueByAlias(ALIAS_HOW_TAKEN_DESCR));

                        // Optional DL fields
                        if (m_IsIncludeDlInfo) {
                            grid.set(FIELD_DL_START_DATE,
                                    prepareDate((String) transcript.getFieldValueByAlias(ALIAS_START_DATE), formatter));
                            grid.set(FIELD_DL_COURSE_ACTIVE_DATE, prepareDate(
                                    (String) transcript.getFieldValueByAlias(ALIAS_ACTIVE_DATE), formatter));
                            grid.set(FIELD_DL_COMPLETION_DATE, prepareDate(
                                    (String) transcript.getFieldValueByAlias(ALIAS_COMPLETION_DATE), formatter));
                        }
                    }

                    if (m_IsIncludeIds) {
                        grid.set(FIELD_TIMETABLE_ID, section.getOid());
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
                    strBldr.append(" with OID: [");
                    strBldr.append(schedule.getOid());
                    SisStudent student = schedule.getStudent();
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
        }

        finally {
            studentSchedules.close();
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

        int fieldCount = MIN_FIELD_COUNT;

        // Set field count depending on "include Ids" export parameter
        m_IsIncludeIds = ((Boolean) getParameter(PARAM_INCL_IDS)).booleanValue();
        if (m_IsIncludeIds) {
            fieldCount++;
        }

        // Set field count depending on "includeDlInfo" export parameter
        m_IsIncludeDlInfo = ((Boolean) getParameter(PARAM_INCL_DL_INFO)).booleanValue();
        if (m_IsIncludeDlInfo) {
            fieldCount += DL_INFO_FIELD_COUNT;
        }

        m_dateConverter = (DateAsStringConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER,
                Locale.getDefault(), true);

        // Set columns
        m_columns = new ArrayList<String>(fieldCount);
        m_columns.add(FIELD_SKL_YEAR);
        m_columns.add(FIELD_SKL_ID);
        m_columns.add(FIELD_STUD_ID);
        m_columns.add(FIELD_STUD_LAST_NAME);
        m_columns.add(FIELD_STUD_FIRST_NAME);
        m_columns.add(FIELD_STUD_GRADE);
        m_columns.add(FIELD_TEACHER_NAME);
        m_columns.add(FIELD_SCHEDULES_SEMESTER);
        m_columns.add(FIELD_COURSE_CODE);
        m_columns.add(FIELD_COURSE_CODE_HOW_TAKEN);
        m_columns.add(FIELD_COURSE_SECTION_LETTER);

        if (m_IsIncludeIds) {
            m_columns.add(FIELD_TIMETABLE_ID);
        }

        if (m_IsIncludeDlInfo) {
            m_columns.add(FIELD_DL_START_DATE);
            m_columns.add(FIELD_DL_COURSE_ACTIVE_DATE);
            m_columns.add(FIELD_DL_COMPLETION_DATE);
        }
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();

        // Add criteria for the current school year context
        criteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());

        // select selected schools
        criteria.addNotNull(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_NAME);
        criteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL,
                StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID));

        // Select active student schedule
        criteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        loadRelatedTranscripts();

        return criteria;
    }

    /**
     * Combines the OIDs of the student and section for a unique key.
     *
     * @param student SisStudent
     * @param section MasterSchedule
     * @return String
     */
    private String buildKey(SisStudent student, MasterSchedule section) {
        String key = "dummy";

        if (student != null && section != null) {
            key = student.getOid() + section.getOid();
        }

        return key;
    }

    /**
     * Loads transcripts for the current year for sections in the selected schools. The records are
     * loaded into a map
     * keyed to the student and section OIDs.
     */
    private void loadRelatedTranscripts() {
        m_transcriptMap = new HashMap<String, Transcript>(2048);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addNotNull(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER + Section.REL_SCHEDULE + PATH_DELIMITER
                + Schedule.COL_NAME);
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        Section.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL,
                        Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                                Section.REL_SCHEDULE + PATH_DELIMITER + Schedule.COL_SCHOOL_OID));

        QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);
        query.addOrderByAscending(Transcript.COL_MASTER_SCHEDULE_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();
                SisStudent student = transcript.getStudent();
                MasterSchedule section = transcript.getMasterSchedule();

                if (student != null && section != null) {
                    m_transcriptMap.put(buildKey(student, section), transcript);
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Get String from UDF field convert it to date and format it.
     *
     * @param dateVal String
     * @param formatter SimpleDateFormat
     * @return String
     */
    private String prepareDate(String dateVal, SimpleDateFormat formatter) {
        String formatedDate = dateVal;

        if (formatter != null) {
            if (dateVal != null) {
                PlainDate date = (PlainDate) m_dateConverter.parseSystemString(dateVal);
                formatedDate = formatter.format(date);
            }
        }

        return formatedDate;
    }
}
