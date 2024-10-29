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
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.reports.StudentContextReportHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports historical curriculum marks (elementary transcript) information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class CurriculumMarksHistoryExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * The Enum REQUIRED_FIELDS.
     */
    // Grid fields
    private enum REQUIRED_FIELDS {
        FIELD_DIST_ID("Current District Number"), FIELD_CURRENT_SCHOOL_NAME(
                "Current School Name"), FIELD_HISTORICAL_SCHOOL_NUMBER("Historical School Number"), FIELD_STUD_ID(
                        "Student Number"), FIELD_STUD_LAST_NAME("Student Legal Last Name"), FIELD_STUD_FIRST_NAME(
                                "Student Legal First Name"), FIELD_STUD_CURRENT_GRADE("Grade"), FIELD_YEAR(
                                        "Year"), FIELD_REPORT_CYCLE_1_NAME("Report Cycle 1 Name", 1,
                                                "name"), FIELD_REPORT_CYCLE_1_MARK("Report Cycle 1 Performance", 1,
                                                        "mark"), FIELD_REPORT_CYCLE_1_DESCR(
                                                                "Report Cycle 1 Performance Description", 1,
                                                                "descr"), FIELD_REPORT_CYCLE_2_NAME(
                                                                        "Report Cycle 2 Name", 2,
                                                                        "name"), FIELD_REPORT_CYCLE_2_MARK(
                                                                                "Report Cycle 2 Performance", 2,
                                                                                "mark"), FIELD_REPORT_CYCLE_2_DESCR(
                                                                                        "Report Cycle 2 Performance Description",
                                                                                        2,
                                                                                        "descr"), FIELD_REPORT_CYCLE_3_NAME(
                                                                                                "Report Cycle 3 Name",
                                                                                                3,
                                                                                                "name"), FIELD_REPORT_CYCLE_3_MARK(
                                                                                                        "Report Cycle 3 Performance",
                                                                                                        3,
                                                                                                        "mark"), FIELD_REPORT_CYCLE_3_DESCR(
                                                                                                                "Report Cycle 3 Performance Description",
                                                                                                                3,
                                                                                                                "descr"), FIELD_REPORT_CYCLE_4_NAME(
                                                                                                                        "Report Cycle 4 Name",
                                                                                                                        4,
                                                                                                                        "name"), FIELD_REPORT_CYCLE_4_MARK(
                                                                                                                                "Report Cycle 4 Performance",
                                                                                                                                4,
                                                                                                                                "mark"), FIELD_REPORT_CYCLE_4_DESCR(
                                                                                                                                        "Report Cycle 4 Performance Description",
                                                                                                                                        4,
                                                                                                                                        "descr"), FIELD_REPORT_CYCLE_5_NAME(
                                                                                                                                                "Report Cycle 5 Name",
                                                                                                                                                5,
                                                                                                                                                "name"), FIELD_REPORT_CYCLE_5_MARK(
                                                                                                                                                        "Report Cycle 5 Performance",
                                                                                                                                                        5,
                                                                                                                                                        "mark"), FIELD_REPORT_CYCLE_5_DESCR(
                                                                                                                                                                "Report Cycle 5 Performance Description",
                                                                                                                                                                5,
                                                                                                                                                                "descr"), FIELD_REPORT_CYCLE_6_NAME(
                                                                                                                                                                        "Report Cycle 6 Name",
                                                                                                                                                                        6,
                                                                                                                                                                        "name"), FIELD_REPORT_CYCLE_6_MARK(
                                                                                                                                                                                "Report Cycle 6 Performance",
                                                                                                                                                                                6,
                                                                                                                                                                                "mark"), FIELD_REPORT_CYCLE_6_DESCR(
                                                                                                                                                                                        "Report Cycle 6 Performance Description",
                                                                                                                                                                                        6,
                                                                                                                                                                                        "descr"), FIELD_REPORT_CYCLE_7_NAME(
                                                                                                                                                                                                "Report Cycle 7 Name",
                                                                                                                                                                                                7,
                                                                                                                                                                                                "name"), FIELD_REPORT_CYCLE_7_MARK(
                                                                                                                                                                                                        "Report Cycle 7 Performance",
                                                                                                                                                                                                        7,
                                                                                                                                                                                                        "mark"), FIELD_REPORT_CYCLE_7_DESCR(
                                                                                                                                                                                                                "Report Cycle 7 Performance Description",
                                                                                                                                                                                                                7,
                                                                                                                                                                                                                "descr"), FIELD_REPORT_CYCLE_8_NAME(
                                                                                                                                                                                                                        "Report cycle 8 Name",
                                                                                                                                                                                                                        8,
                                                                                                                                                                                                                        "name"), FIELD_REPORT_CYCLE_8_MARK(
                                                                                                                                                                                                                                "Report Cycle 8 Performance",
                                                                                                                                                                                                                                8,
                                                                                                                                                                                                                                "mark"), FIELD_REPORT_CYCLE_8_DESCR(
                                                                                                                                                                                                                                        "Report Cycle 8 Performance Description",
                                                                                                                                                                                                                                        8,
                                                                                                                                                                                                                                        "descr"), FIELD_REPORT_CYCLE_9_NAME(
                                                                                                                                                                                                                                                "Report Cycle 9 Name",
                                                                                                                                                                                                                                                9,
                                                                                                                                                                                                                                                "name"), FIELD_REPORT_CYCLE_9_MARK(
                                                                                                                                                                                                                                                        "Report Cycle 9 Performance",
                                                                                                                                                                                                                                                        9,
                                                                                                                                                                                                                                                        "mark"), FIELD_REPORT_CYCLE_9_DESCR(
                                                                                                                                                                                                                                                                "Report Cycle 9 Performance Description",
                                                                                                                                                                                                                                                                9,
                                                                                                                                                                                                                                                                "descr"), FIELD_REPORT_CYCLE_FIN_NAME(
                                                                                                                                                                                                                                                                        "Final Report Cycle Name",
                                                                                                                                                                                                                                                                        10,
                                                                                                                                                                                                                                                                        "name"), FIELD_REPORT_CYCLE_FIN_MARK(
                                                                                                                                                                                                                                                                                "Final Report Cycle Performance",
                                                                                                                                                                                                                                                                                10,
                                                                                                                                                                                                                                                                                "mark"), FIELD_REPORT_CYCLE_FIN_DESCR(
                                                                                                                                                                                                                                                                                        "Final Report Cycle Performance Description",
                                                                                                                                                                                                                                                                                        10,
                                                                                                                                                                                                                                                                                        "descr"), FIELD_REPORT_CYCLE_1_COMMENTS(
                                                                                                                                                                                                                                                                                                "Report Cycle 1 Comments",
                                                                                                                                                                                                                                                                                                1,
                                                                                                                                                                                                                                                                                                "comments"), FIELD_REPORT_CYCLE_2_COMMENTS(
                                                                                                                                                                                                                                                                                                        "Report Cycle 2 Comments",
                                                                                                                                                                                                                                                                                                        2,
                                                                                                                                                                                                                                                                                                        "comments"), FIELD_REPORT_CYCLE_3_COMMENTS(
                                                                                                                                                                                                                                                                                                                "Report Cycle 3 Comments",
                                                                                                                                                                                                                                                                                                                3,
                                                                                                                                                                                                                                                                                                                "comments"), FIELD_REPORT_CYCLE_4_COMMENTS(
                                                                                                                                                                                                                                                                                                                        "Report Cycle 4 Comments",
                                                                                                                                                                                                                                                                                                                        4,
                                                                                                                                                                                                                                                                                                                        "comments"), FIELD_REPORT_CYCLE_5_COMMENTS(
                                                                                                                                                                                                                                                                                                                                "Report Cycle 5 Comments",
                                                                                                                                                                                                                                                                                                                                5,
                                                                                                                                                                                                                                                                                                                                "comments"), FIELD_REPORT_CYCLE_6_COMMENTS(
                                                                                                                                                                                                                                                                                                                                        "Report Cycle 6 Comments",
                                                                                                                                                                                                                                                                                                                                        6,
                                                                                                                                                                                                                                                                                                                                        "comments"), FIELD_REPORT_CYCLE_7_COMMENTS(
                                                                                                                                                                                                                                                                                                                                                "Report Cycle 7 Comments",
                                                                                                                                                                                                                                                                                                                                                7,
                                                                                                                                                                                                                                                                                                                                                "comments"), FIELD_REPORT_CYCLE_8_COMMENTS(
                                                                                                                                                                                                                                                                                                                                                        "Report Cycle 8 Comments",
                                                                                                                                                                                                                                                                                                                                                        8,
                                                                                                                                                                                                                                                                                                                                                        "comments"), FIELD_REPORT_CYCLE_9_COMMENTS(
                                                                                                                                                                                                                                                                                                                                                                "Report Cycle 9 Comments",
                                                                                                                                                                                                                                                                                                                                                                9,
                                                                                                                                                                                                                                                                                                                                                                "comments"), FIELD_REPORT_CYCLE_FIN_COMMENTS(
                                                                                                                                                                                                                                                                                                                                                                        "Final Report Cycle Comments",
                                                                                                                                                                                                                                                                                                                                                                        10,
                                                                                                                                                                                                                                                                                                                                                                        "comments"), FIELD_CURRICULUM_SUBJECT(
                                                                                                                                                                                                                                                                                                                                                                                "Subject");

        private String m_fieldName;
        private String m_fieldDef;
        private int m_cycleNum;

        protected final static int FINAL_CYCLE_NUMBER = 10;

        /**
         * Instantiates a new required fields.
         *
         * @param fieldName String
         */
        private REQUIRED_FIELDS(String fieldName) {
            this(fieldName, 0, null);
        }

        /**
         * Instantiates a new required fields.
         *
         * @param fieldName String
         * @param cycleNum int
         * @param fieldDef String
         */
        private REQUIRED_FIELDS(String fieldName, int cycleNum, String fieldDef) {
            m_fieldName = fieldName;
            m_fieldDef = fieldDef;
            m_cycleNum = cycleNum;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }

        /**
         * Gets the field def.
         *
         * @return String
         */
        private String getFieldDef() {
            return m_fieldDef;
        }

        /**
         * Gets the field cycle num.
         *
         * @return int
         */
        private int getFieldCycleNum() {
            return m_cycleNum;
        }

        /**
         * Gets the name field by cycle.
         *
         * @param cycleNum int
         * @return required fields
         */
        public static REQUIRED_FIELDS getNameFieldByCycle(int cycleNum) {
            return getFieldByDefAndCycleNum("name", cycleNum);
        }

        /**
         * Gets the mark field by cycle.
         *
         * @param cycleNum int
         * @return required fields
         */
        public static REQUIRED_FIELDS getMarkFieldByCycle(int cycleNum) {
            return getFieldByDefAndCycleNum("mark", cycleNum);
        }

        /**
         * Gets the description field by cycle.
         *
         * @param cycleNum int
         * @return required fields
         */
        public static REQUIRED_FIELDS getDescriptionFieldByCycle(int cycleNum) {
            return getFieldByDefAndCycleNum("descr", cycleNum);
        }

        /**
         * Gets the comments field by cycle.
         *
         * @param cycleNum int
         * @return required fields
         */
        public static REQUIRED_FIELDS getCommentsFieldByCycle(int cycleNum) {
            return getFieldByDefAndCycleNum("comments", cycleNum);
        }

        /**
         * Gets the field by def and cycle num.
         *
         * @param def String
         * @param cycleNum int
         * @return required fields
         */
        private static REQUIRED_FIELDS getFieldByDefAndCycleNum(String def, int cycleNum) {
            for (REQUIRED_FIELDS field : REQUIRED_FIELDS.values()) {
                if (field.getFieldDef() != null && def.compareTo(field.getFieldDef()) == 0) {
                    if (field.getFieldCycleNum() == cycleNum) {
                        return field;
                    }
                }
            }
            return null;
        }
    }

    // Other constants
    private static final String FINAL_CYCLE_NAME = "Final";

    // Members
    private GradesManager m_gradeManager;
    private StudentContextReportHelper m_helper;

    private Map<String, Collection<TranscriptColumnDefinition>> m_columnsByDefinition;
    private Map<String, Map<String, String>> m_gradeDescriptionMap;
    private Map<String, Collection<GradeTerm>> m_gradeTermsByDefinition;

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

        int count = 0;

        QueryByCriteria query = new QueryByCriteria(Transcript.class, buildCriteria());
        query.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship()
                + PATH_DELIMITER + School.COL_SCHOOL_ID);
        query.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);

        QueryIterator transcripts = getBroker().getIteratorByQuery(query);
        try {
            int cycleNumber = 1;

            while (transcripts.hasNext()) {
                Transcript transcript = (Transcript) transcripts.next();
                boolean deleteRow = false;
                try {
                    SisStudent student = transcript.getStudent();
                    SisSchool school = transcript.getSchool();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");
                    SisSchool currentSchool = student.getSchool(getCurrentContext().getOid(), getBroker());

                    if (++count % 10000 == 0) {
                        AppGlobals.getLog().info("GDE Curriculum Marks: processed " + count + " transcript records");
                    }

                    grid.append();
                    deleteRow = true;

                    grid.set(REQUIRED_FIELDS.FIELD_DIST_ID.getFieldName(), school.getParentOrganization().getId());
                    grid.set(REQUIRED_FIELDS.FIELD_HISTORICAL_SCHOOL_NUMBER.getFieldName(), school.getSchoolId());
                    grid.set(REQUIRED_FIELDS.FIELD_CURRENT_SCHOOL_NAME.getFieldName(), currentSchool.getName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_ID.getFieldName(), student.getLocalId());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_LAST_NAME.getFieldName(), student.getPerson().getLastName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_FIRST_NAME.getFieldName(), student.getPerson().getFirstName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_CURRENT_GRADE.getFieldName(),
                            StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(REQUIRED_FIELDS.FIELD_YEAR.getFieldName(),
                            String.valueOf(transcript.getDistrictContext().getSchoolYear()));
                    grid.set(REQUIRED_FIELDS.FIELD_CURRICULUM_SUBJECT.getFieldName(),
                            transcript.getCourseDescription());

                    // Grades by term
                    boolean finalColumnPopulated = false;

                    Collection<GradeTerm> gradeTerms = getGradeTerms(transcript.getTranscriptDefinition());
                    for (GradeTerm gradeTerm : gradeTerms) {
                        boolean isMarkColumnFounded = false;
                        cycleNumber = gradeTerm.getGradeTermNum();

                        Collection<TranscriptColumnDefinition> columns =
                                getTranscriptColumns(transcript.getTranscriptDefinition());
                        for (TranscriptColumnDefinition column : columns) {
                            if (!finalColumnPopulated
                                    && column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_FINAL) {
                                grid.set(REQUIRED_FIELDS.getNameFieldByCycle(REQUIRED_FIELDS.FINAL_CYCLE_NUMBER)
                                        .getFieldName(), FINAL_CYCLE_NAME);

                                String markValue = (String) transcript
                                        .getFieldValueByBeanPath(column.getTranscriptBeanAttribute());
                                grid.set(REQUIRED_FIELDS.getMarkFieldByCycle(REQUIRED_FIELDS.FINAL_CYCLE_NUMBER)
                                        .getFieldName(), markValue);

                                String gradeDescription = getGradeDescription(markValue, column.getGradeScale(),
                                        transcript.getSchoolCourse());
                                grid.set(REQUIRED_FIELDS.getDescriptionFieldByCycle(REQUIRED_FIELDS.FINAL_CYCLE_NUMBER)
                                        .getFieldName(), gradeDescription);

                                finalColumnPopulated = true;
                            } else if (!isMarkColumnFounded
                                    && column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_GRADE) {
                                if (gradeTerm.getOid().equals(column.getGradeTermOid())) {
                                    grid.set(REQUIRED_FIELDS.getNameFieldByCycle(cycleNumber).getFieldName(),
                                            gradeTerm.getGradeTermId());

                                    String markValue = (String) transcript
                                            .getFieldValueByBeanPath(column.getTranscriptBeanAttribute());
                                    grid.set(REQUIRED_FIELDS.getMarkFieldByCycle(cycleNumber).getFieldName(),
                                            markValue);

                                    String gradeDescription = getGradeDescription(markValue, column.getGradeScale(),
                                            transcript.getSchoolCourse());
                                    grid.set(REQUIRED_FIELDS.getDescriptionFieldByCycle(cycleNumber).getFieldName(),
                                            gradeDescription);

                                    isMarkColumnFounded = true;
                                }
                            } else if (column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_COMMENT) {
                                if (gradeTerm.getOid().equals(column.getGradeTermOid())) {
                                    grid.set(REQUIRED_FIELDS.getCommentsFieldByCycle(cycleNumber).getFieldName(),
                                            transcript.getFieldValueByBeanPath(column.getTranscriptBeanAttribute()));
                                }
                            }
                        }
                    }
                } catch (NullPointerException npe) {
                    StringBuilder strBldr = new StringBuilder();
                    strBldr.append("Unable to export ");
                    strBldr.append(query.getClass().getName());
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
                        grid.deleteRow(); // Delete the incomplete row that was appended to the
                                          // grid.
                    }

                    strBldr.append("\n\n\nNullPointerException: \n");
                    strBldr.append(ExceptionUtils.getStackTrace(npe));
                    logToolMessage(Level.WARNING, strBldr.toString(), false);
                }
            }
        } finally {
            transcripts.close();
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
    protected List<?> getColumnNames() {
        return getColumnNamesList();
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List<?> getColumnUserNames() {
        return getColumnNamesList();
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

        m_helper = new StudentContextReportHelper(getOrganization(), getCurrentContext(), getBroker());

        m_gradeManager = new GradesManager(getBroker());
        m_columnsByDefinition = new HashMap<String, Collection<TranscriptColumnDefinition>>(32);
        m_gradeDescriptionMap = new HashMap<String, Map<String, String>>(32);
        m_gradeTermsByDefinition = new HashMap<String, Collection<GradeTerm>>(32);
    }

    /**
     * Builds export criteria
     * <p>
     * Curriculum History extract retrieves data based on the selected criteria (school district and
     * school) and
     * includes all records from curriculum historical data stored in eSIS database for the students
     * in the
     * selected school. The students must be active in the current year's curriculum (mark or
     * comment entered) in
     * the selected school.
     * <p>
     * If the student attended other schools than the selected one the curriculum historical data
     * for these schools
     * are included in the extract as well.
     * <p>
     * The data elements for the curriculum history extract do not include the same elements as the
     * current year's
     * Curriculum Marks Information extract due to archiving process. The Year End Transition
     * process does not
     * archive all the data elements from the current school year curriculum.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addLessThan(
                Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(getCurrentContext().getSchoolYear()));

        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(getSchoolCriteria().copyWithAdjustedPath(
                Transcript.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolRelationship(),
                Transcript.REL_STUDENT + PATH_DELIMITER + m_helper.getSchoolOidField()));
        studentCriteria.addAndCriteria(m_helper.getActiveStudentCriteria(Transcript.REL_STUDENT + PATH_DELIMITER));

        if (getSchool() != null) {
            Criteria secondaryCriteria = StudentManager.getSecondaryStudentCriteria(
                    Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_STUDENT_SCHOOLS +
                            PATH_DELIMITER,
                    getCurrentContext().getOid(), getSchool().getOid(), null, null,
                    getBroker().getPersistenceKey());

            studentCriteria.addOrCriteria(secondaryCriteria);
        }

        criteria.addAndCriteria(studentCriteria);

        return criteria;
    }

    /**
     * Create list of Column names.
     *
     * @return List<String>
     */
    private List<String> getColumnNamesList() {
        List<String> columnNames = new ArrayList<String>(REQUIRED_FIELDS.values().length);

        for (REQUIRED_FIELDS field : REQUIRED_FIELDS.values()) {
            columnNames.add(field.getFieldName());
        }

        return columnNames;
    }

    /**
     * Pulls the grade description (fieldA001) of the passed grade (first translated to a letter
     * grade).
     *
     * @param gradeValue String
     * @param gradeScale GradeScale
     * @param schoolCourse SchoolCourse
     * @return String
     */
    private String getGradeDescription(String gradeValue, GradeScale gradeScale, SchoolCourse schoolCourse) {
        String description = "";

        if (gradeScale != null) {
            Map<String, String> gradeDescriptions = m_gradeDescriptionMap.get(gradeScale.getOid());

            if (gradeDescriptions == null) {
                gradeDescriptions = new HashMap<String, String>(32);
                m_gradeDescriptionMap.put(gradeScale.getOid(), gradeDescriptions);

                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(GradeScaleGradeDefinition.COL_GRADE_SCALE_OID, gradeScale.getOid());
                criteria.addNotEmpty(GradeScaleGradeDefinition.COL_FIELD_A001, getBroker().getPersistenceKey());

                QueryByCriteria query = new QueryByCriteria(GradeScaleGradeDefinition.class, criteria);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        GradeScaleGradeDefinition gradeDefinition = (GradeScaleGradeDefinition) iterator.next();
                        gradeDescriptions.put(gradeDefinition.getGradeCode(), gradeDefinition.getFieldA001());
                    }
                } finally {
                    iterator.close();
                }
            }

            String letterGrade = getLetterGrade(gradeValue, gradeScale, schoolCourse);

            description = gradeDescriptions.get(letterGrade);
        }

        return description;
    }

    /**
     * Returns a collection of grade terms associated with the provided transcript definition.
     *
     * @param definition TranscriptDefinition
     * @return Collection<GradeTerm>
     */
    private Collection<GradeTerm> getGradeTerms(TranscriptDefinition definition) {
        Collection<GradeTerm> terms = new LinkedList<GradeTerm>();
        if (definition != null) {
            terms = m_gradeTermsByDefinition.get(definition.getOid());

            if (terms == null) {
                terms = definition.getGradeTermDefinition().getGradeTerms(getBroker());
                m_gradeTermsByDefinition.put(definition.getOid(), terms);
            }
        }

        return terms;
    }

    /**
     * Returns the letter grade of the passed grade value. If the grade value is numeric, the letter
     * grade is obtained
     * via lookup on the grade scale.
     *
     * @param grade String
     * @param gradeScale GradeScale
     * @param schoolCourse
     * @return String
     */
    private String getLetterGrade(String grade, GradeScale gradeScale, SchoolCourse schoolCourse) {
        String letterGrade = grade;
        BigDecimal numericGrade = null;

        if (StringUtils.isNumeric(grade)) {
            try {
                numericGrade = new BigDecimal(grade);
                letterGrade = m_gradeManager.getLetterValue(numericGrade, gradeScale, schoolCourse.getSchool(),
                        schoolCourse.getOid());
            } catch (NumberFormatException e) {
                // DO nothing
            }
        }

        return letterGrade;
    }



    /**
     * Returns a collection of transcript columns associated with the provided transcript
     * definition.
     *
     * @param definition TranscriptDefinition
     * @return Collection<TranscriptColumnDefinition>
     */
    private Collection<TranscriptColumnDefinition> getTranscriptColumns(TranscriptDefinition definition) {
        Collection<TranscriptColumnDefinition> columns = m_columnsByDefinition.get(definition.getOid());

        if (columns == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, definition.getOid());
            criteria.addEqualTo(TranscriptColumnDefinition.COL_REPORT_TYPE,
                    Integer.valueOf(TranscriptColumnDefinition.GRADE_TYPE_TERM));
            criteria.addNotContains(TranscriptColumnDefinition.COL_GRADE_COLUMN_HEADER, "Exam");

            QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);

            columns = getBroker().getCollectionByQuery(query);
            m_columnsByDefinition.put(definition.getOid(), columns);
        }

        return columns;
    }
}
