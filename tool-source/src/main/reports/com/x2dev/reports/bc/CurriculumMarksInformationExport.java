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
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.assessment.RubricManager;
import com.x2dev.sis.tools.reports.RubricTranscriptReportGrid;
import com.x2dev.sis.tools.reports.TranscriptReportGrid;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
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
 * Exports curriculum marks (elementary transcript) information for BC's GDE.
 *
 * @author Follett Software Company
 */
public class CurriculumMarksInformationExport extends GdeExportJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * The Enum REQUIRED_FIELDS.
     */
    // Grid fields
    private enum REQUIRED_FIELDS {
        FIELD_DIST_ID("District Number"), FIELD_SKL_ID("School Number"), FIELD_SKL_FULL_NAME(
                "School Name"), FIELD_STUD_ID("Student Number"), FIELD_STUD_LAST_NAME(
                        "Student Legal Last Name"), FIELD_STUD_FIRST_NAME(
                                "Student Legal First Name"), FIELD_STUD_CURRENT_GRADE("Grade"), FIELD_HOMEROOM(
                                        "Homeroom"), FIELD_REPORT_CYCLE_1_NAME("Report Cycle 1 Name", 1,
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
                                                                                                                                                                                                                                                                                        "descr"), FIELD_TEACHER_COMMENTS(
                                                                                                                                                                                                                                                                                                "Comments"), FIELD_TEACHER_ID(
                                                                                                                                                                                                                                                                                                        "Teacher Id"), FIELD_TEACHER_NAME(
                                                                                                                                                                                                                                                                                                                "Teacher Name"), FIELD_CURRICULUM_SUBJECT(
                                                                                                                                                                                                                                                                                                                        "Subject"), FIELD_REPORT_CYCLE_1_OPTIONS(
                                                                                                                                                                                                                                                                                                                                "Report Cycle 1 Options",
                                                                                                                                                                                                                                                                                                                                1,
                                                                                                                                                                                                                                                                                                                                "options"), FIELD_REPORT_CYCLE_2_OPTIONS(
                                                                                                                                                                                                                                                                                                                                        "Report Cycle 2 Options",
                                                                                                                                                                                                                                                                                                                                        2,
                                                                                                                                                                                                                                                                                                                                        "options"), FIELD_REPORT_CYCLE_3_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                "Report Cycle 3 Options",
                                                                                                                                                                                                                                                                                                                                                3,
                                                                                                                                                                                                                                                                                                                                                "options"), FIELD_REPORT_CYCLE_4_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                        "Report Cycle 4 Options",
                                                                                                                                                                                                                                                                                                                                                        4,
                                                                                                                                                                                                                                                                                                                                                        "options"), FIELD_REPORT_CYCLE_5_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                                "Report Cycle 5 Options",
                                                                                                                                                                                                                                                                                                                                                                5,
                                                                                                                                                                                                                                                                                                                                                                "options"), FIELD_REPORT_CYCLE_6_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                                        "Report Cycle 6 Options",
                                                                                                                                                                                                                                                                                                                                                                        6,
                                                                                                                                                                                                                                                                                                                                                                        "options"), FIELD_REPORT_CYCLE_7_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                                                "Report Cycle 7 Options",
                                                                                                                                                                                                                                                                                                                                                                                7,
                                                                                                                                                                                                                                                                                                                                                                                "options"), FIELD_REPORT_CYCLE_8_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                                                        "Report Cycle 8 Options",
                                                                                                                                                                                                                                                                                                                                                                                        8,
                                                                                                                                                                                                                                                                                                                                                                                        "options"), FIELD_REPORT_CYCLE_9_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                                                                "Report Cycle 9 Options",
                                                                                                                                                                                                                                                                                                                                                                                                9,
                                                                                                                                                                                                                                                                                                                                                                                                "options"), FIELD_REPORT_CYCLE_FIN_OPTIONS(
                                                                                                                                                                                                                                                                                                                                                                                                        "Final Report Cycle Options",
                                                                                                                                                                                                                                                                                                                                                                                                        10,
                                                                                                                                                                                                                                                                                                                                                                                                        "options");

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
         * Gets the options field by cycle.
         *
         * @param cycleNum int
         * @return required fields
         */
        @SuppressWarnings("unused")
        public static REQUIRED_FIELDS getOptionsFieldByCycle(int cycleNum) {
            return getFieldByDefAndCycleNum("options", cycleNum);
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

    private Map<String, Collection<TranscriptColumnDefinition>> m_columnsByDefinition;
    private Map<String, Map<String, String>> m_gradeDescriptionMap;
    private Map<String, Collection<GradeTerm>> m_gradeTermsByDefinition;
    private Map<String, GradeTerm> m_recentGradeTermMap;
    private Map<String, Collection<GradeTermDate>> m_termDateMap;

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

        String[] studentSort = new String[] {Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_SCHOOL_ID,
                Student.COL_LOCAL_ID};
        RubricTranscriptReportGrid transcriptGrid = new RubricTranscriptReportGrid(buildCriteria(),
                studentSort,
                false,
                false,
                false,
                getOrganization(),
                getBroker());

        transcriptGrid.beforeTop();
        Transcript lastTranscript = null;

        while (transcriptGrid.next()) {
            Transcript transcript = transcriptGrid.getTranscript();
            boolean deleteRow = false;
            try {
                if (!ObjectUtils.match(transcript, lastTranscript)) {
                    SisStudent student = transcript.getStudent();
                    SisSchool school = transcript.getSchool();

                    String gradeLevel = student.getGradeLevel(getCurrentContext().getOid(), getBroker());
                    String subGradeLevel = (String) student.getFieldValueByAlias("std-sub-grade");

                    if (++count % 10000 == 0) {
                        AppGlobals.getLog().info("GDE Curriculum Marks: processed " + count + " transcript records");
                    }

                    grid.append();
                    deleteRow = true;
                    grid.set(REQUIRED_FIELDS.FIELD_DIST_ID.getFieldName(), school.getParentOrganization().getId());
                    grid.set(REQUIRED_FIELDS.FIELD_SKL_ID.getFieldName(), school.getSchoolId());
                    grid.set(REQUIRED_FIELDS.FIELD_SKL_FULL_NAME.getFieldName(), school.getName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_ID.getFieldName(), student.getLocalId());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_LAST_NAME.getFieldName(), student.getPerson().getLastName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_FIRST_NAME.getFieldName(), student.getPerson().getFirstName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_CURRENT_GRADE.getFieldName(),
                            StringUtils.isEmpty(subGradeLevel) ? gradeLevel : subGradeLevel);
                    grid.set(REQUIRED_FIELDS.FIELD_HOMEROOM.getFieldName(),
                            student.getHomeroom(getCurrentContext().getOid(), getBroker()));
                    grid.set(REQUIRED_FIELDS.FIELD_CURRICULUM_SUBJECT.getFieldName(),
                            transcript.getCourseDescription());

                    // Teacher information
                    MasterSchedule section = transcript.getMasterSchedule();
                    if (section != null) {
                        SisStaff teacher = section.getPrimaryStaff();

                        if (teacher != null) {
                            grid.set(REQUIRED_FIELDS.FIELD_TEACHER_ID.getFieldName(), teacher.getLocalId());
                            grid.set(REQUIRED_FIELDS.FIELD_TEACHER_NAME.getFieldName(), teacher.getNameView());
                        }
                    }

                    // Grades by term
                    boolean finalColumnPopulated = false;
                    Collection<GradeTerm> gradeTerms = getGradeTerms(transcript.getTranscriptDefinition());
                    GradeTerm mostRecentTerm = getMostRecentTerm(gradeTerms, school);

                    for (GradeTerm gradeTerm : gradeTerms) {
                        boolean isMarkColumnFounded = false;
                        boolean isMostRecentTerm = gradeTerm.equals(mostRecentTerm);

                        int cycleNumber = gradeTerm.getGradeTermNum();

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
                            } else if (gradeTerm.equals(column.getGradeTerm())) {
                                if (!isMarkColumnFounded
                                        && column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_GRADE) {
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
                                } else if (!isMarkColumnFounded && column
                                        .getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_RUBRIC) {
                                    String header =
                                            TranscriptReportGrid.RUBRIC_FIELD_PREFIX + column.getGradeColumnHeader();
                                    RubricCriterion criterion = (RubricCriterion) transcriptGrid
                                            .get(RubricTranscriptReportGrid.COL_CRITERION);

                                    grid.set(REQUIRED_FIELDS.getNameFieldByCycle(cycleNumber).getFieldName(),
                                            gradeTerm.getGradeTermId());

                                    String markValue = (String) transcriptGrid.get(header);
                                    grid.set(REQUIRED_FIELDS.getMarkFieldByCycle(cycleNumber).getFieldName(),
                                            markValue);

                                    if (criterion != null) {
                                        RubricRatingScalePoints scalePoint = RubricManager
                                                .getRatingScalePoint(criterion.getRubricRatingScale(), markValue);
                                        if (scalePoint != null) {
                                            grid.set(REQUIRED_FIELDS.getDescriptionFieldByCycle(cycleNumber)
                                                    .getFieldName(), scalePoint.getDescription());
                                        }
                                    }

                                    isMarkColumnFounded = true;
                                } else if (column.getColumnTypeCode() == TranscriptColumnDefinition.COLUMN_TYPE_COMMENT
                                        && isMostRecentTerm) {
                                    grid.set(REQUIRED_FIELDS.FIELD_TEACHER_COMMENTS.getFieldName(),
                                            transcript.getFieldValueByBeanPath(column.getTranscriptBeanAttribute()));
                                }
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
        return getColumnNamesList();
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
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

        loadGradeTermDates();

        m_gradeManager = new GradesManager(getBroker());
        m_columnsByDefinition = new HashMap<String, Collection<TranscriptColumnDefinition>>(32);
        m_gradeDescriptionMap = new HashMap<String, Map<String, String>>(32);
        m_gradeTermsByDefinition = new HashMap<String, Collection<GradeTerm>>(32);
        m_recentGradeTermMap = new HashMap<>(2048);
    }

    /**
     * Builds export criteria.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(Transcript.REL_SCHOOL, Transcript.COL_SCHOOL_OID));

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
     * @param schoolCourse SchoolCourse
     * @return String
     */
    private String getLetterGrade(String grade, GradeScale gradeScale, SchoolCourse schoolCourse) {
        String letterGrade = grade;
        BigDecimal numericGrade = null;

        if (StringUtils.isNumeric(grade)) {
            numericGrade = new BigDecimal(grade);
            letterGrade = m_gradeManager.getLetterValue(numericGrade, gradeScale, schoolCourse.getSchool(),
                    schoolCourse.getOid());
        }

        return letterGrade;
    }

    /**
     * Gets the most recently completed term for the school. If no terms are completed return term
     * 1.
     *
     *
     * @param terms Collection<GradeTerm>
     * @param school SisSchool
     * @return GradeTerm
     */
    private GradeTerm getMostRecentTerm(Collection<GradeTerm> terms, SisSchool school) {
        GradeTerm recentTerm = m_recentGradeTermMap.get(school.getOid());

        if (recentTerm == null) {
            PlainDate today = getPlainDate();
            PlainDate latestEndDate = null;

            // Lookup term
            for (GradeTerm term : terms) {
                if (recentTerm == null && term.getGradeTermNum() == 1) {
                    recentTerm = term;
                } else {
                    // Check grade term dates
                    for (GradeTermDate termDate : getTermDates(term, school)) {
                        PlainDate endDate = termDate.getEndDate();
                        if (!endDate.after(today)) {
                            if (latestEndDate == null || endDate.after(latestEndDate)) {
                                recentTerm = term;
                                latestEndDate = endDate;
                            }
                        }
                    }
                }
            }

            m_recentGradeTermMap.put(school.getOid(), recentTerm);
        }

        return recentTerm;
    }

    /**
     * Returns the grade term dates for the passed grade term/school combination. If none exist then
     * an empty
     * collection is returned.
     *
     * @param term GradeTerm
     * @param school SisSchool
     * @return Collection<GradeTermDate>
     */
    private Collection<GradeTermDate> getTermDates(GradeTerm term, SisSchool school) {
        Collection<GradeTermDate> dates = new LinkedList<GradeTermDate>();

        if (term != null && school != null) {
            String key = term.getGradeTermId() + school.getOid();

            dates = m_termDateMap.get(key);
            if (dates == null) {
                dates = new LinkedList<GradeTermDate>();
                m_termDateMap.put(key, dates);
            }
        }

        return dates;
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

    /**
     * Loads the grade term dates used in the current school year.
     */
    private void loadGradeTermDates() {
        m_termDateMap = new HashMap<String, Collection<GradeTermDate>>(2048);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        criteria.addAndCriteria(
                getSchoolCriteria().copyWithAdjustedPath(GradeTermDate.REL_SCHOOL, GradeTermDate.COL_SCHOOL_OID));

        QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
        query.addOrderByAscending(GradeTermDate.COL_SCHOOL_OID);
        query.addOrderByAscending(GradeTermDate.COL_GRADE_TERM_OID);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            String lastKey = null;
            Collection<GradeTermDate> dates = new LinkedList<GradeTermDate>();

            while (iterator.hasNext()) {
                GradeTermDate termDate = (GradeTermDate) iterator.next();
                String key = termDate.getGradeTerm().getGradeTermId() + termDate.getSchoolOid();

                if (!ObjectUtils.match(key, lastKey)) {
                    dates = new LinkedList<GradeTermDate>();
                    m_termDateMap.put(key, dates);
                }

                dates.add(termDate);

                lastKey = key;
            }
        } finally {
            iterator.close();
        }
    }
}
