/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.beans.TranscriptDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for Fujitsu's "Honor Roll" report.
 *
 * @author X2 Development Corporation
 */
public class HonorRollData extends ReportJavaSourceNet {
    // Input parameters
    private static final String PARAM_ACTIVE_ONLY = "activeOnly";
    private static final String PARAM_CONTEXT_OID = "contextOid";
    private static final String PARAM_MAX_GPA = "maxGpa";
    private static final String PARAM_MIN_COURSES = "minCourses";
    private static final String PARAM_MIN_GPA = "minGpa";
    private static final String PARAM_MIN_GRADE = "minGrade";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_SECONDARY_STUDENT = "secondaryStudent";
    private static final String PARAM_STUDENT_SORT = "studentSort";
    private static final String PARAM_TRN_COLUMN_DEF_OIDS = "trnColumnDefOids";

    // Report grid fields
    private static final String COL_GRADE_VALUE_PREFIX = "gradeValue";
    private static final String COL_STUDENT = "student";
    private static final String COL_TRANSCRIPT = "transcript";

    // Report parameters
    private static final String GRADE_NAME_PREFIX_PARAM = "gradeName";

    // Student GPA field
    private static final String STUDENT_GPA_FIELD = SisStudent.COL_FIELD_A009;

    // Private variables
    private String m_contextOid;
    private GradesManager m_gradesManager;
    private Integer m_minCourses;
    private String m_minGrade;
    private X2Criteria m_studentCriteria;
    private List<TranscriptColumnDefinition> m_transcriptColumns;
    private Map<String, List<Transcript>> m_transcriptsByStudentOid;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        String minGpa = (String) getParameter(PARAM_MIN_GPA);
        String maxGpa = (String) getParameter(PARAM_MAX_GPA);

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, m_studentCriteria);
        applyUserSort(query, (String) getParameter(PARAM_STUDENT_SORT));

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                String studentOid = student.getOid();

                boolean qualifiedGpa = true;
                if (StringUtils.isNumeric(minGpa) || StringUtils.isNumeric(maxGpa)) {
                    String gpa = (String) student.getFieldValueByBeanPath(STUDENT_GPA_FIELD);
                    if (!StringUtils.isNumeric(gpa)) {
                        qualifiedGpa = false;
                    } else {
                        double numericGpa = Double.parseDouble(gpa);

                        if ((StringUtils.isNumeric(minGpa) && numericGpa < Double.parseDouble(minGpa)) ||
                                (StringUtils.isNumeric(maxGpa) && numericGpa > Double.parseDouble(maxGpa))) {
                            qualifiedGpa = false;
                        }
                    }
                }

                if (qualifiedGpa &&
                        (StringUtils.isEmpty(m_minGrade) || (!StringUtils.isEmpty(m_minGrade)
                                && m_transcriptsByStudentOid.keySet().contains(studentOid)))
                        &&
                        (m_minCourses == null
                                || (m_minCourses != null && m_transcriptsByStudentOid.keySet().contains(studentOid)))) {
                    List<Transcript> transcripts = m_transcriptsByStudentOid.get(studentOid);
                    if (transcripts != null) {
                        for (Transcript transcript : transcripts) {
                            grid.append();
                            grid.set(COL_STUDENT, student);
                            grid.set(COL_TRANSCRIPT, transcript);

                            for (int i = 0; i < m_transcriptColumns.size(); i++) {
                                TranscriptColumnDefinition transcriptColumn = m_transcriptColumns.get(i);

                                String grade = (String) transcript.getFieldValueByBeanPath(
                                        transcriptColumn.getDataFieldConfig().getDataField().getJavaName());
                                if (StringUtils.isNumeric(m_minGrade) && !StringUtils.isNumeric(grade)) {
                                    grade = String.valueOf(getNumericGrade(grade, transcriptColumn.getGradeScale(),
                                            transcript.getSchoolCourse()));
                                }

                                grid.set(COL_GRADE_VALUE_PREFIX + i, grade);
                            }
                        }
                    } else {
                        grid.append();
                        grid.set(COL_STUDENT, student);
                    }
                }
            }
        } finally {
            iterator.close();
        }

        for (int i = 0; i < m_transcriptColumns.size(); i++) {
            addParameter(GRADE_NAME_PREFIX_PARAM + i, m_transcriptColumns.get(i).getGradeColumnHeader());
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_contextOid = (String) getParameter(PARAM_CONTEXT_OID);
        m_minGrade = (String) getParameter(PARAM_MIN_GRADE);
        m_minCourses = (Integer) getParameter(PARAM_MIN_COURSES);

        m_gradesManager = new GradesManager(getBroker());

        buildStudentCriteria();
        loadTranscriptColumns();
        loadTranscriptsByStudentOid();
    }

    /**
     * Builds the Student criteria.
     */
    private void buildStudentCriteria() {
        m_studentCriteria = new X2Criteria();

        Criteria schoolCriteria = new Criteria();
        schoolCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

        if (((Boolean) getParameter(PARAM_SECONDARY_STUDENT)).booleanValue()) {
            Criteria secondarySchoolCriteria = new Criteria();

            SubQuery subQuery = new SubQuery(StudentSchool.class,
                    StudentSchool.COL_STUDENT_OID,
                    StudentManager.getSecondaryStudentCriteria(m_contextOid,
                            getSchool().getOid(),
                            null,
                            null,
                            getBroker().getPersistenceKey()));

            secondarySchoolCriteria.addIn(X2BaseBean.COL_OID, subQuery);
            schoolCriteria.addOrCriteria(secondarySchoolCriteria);
        }

        m_studentCriteria.addAndCriteria(schoolCriteria);

        if (((Boolean) getParameter(PARAM_ACTIVE_ONLY)).booleanValue()) {
            m_studentCriteria.addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        }

        addUserCriteria(m_studentCriteria,
                (String) getParameter(PARAM_QUERY_BY),
                (String) getParameter(PARAM_QUERY_STRING),
                SisStudent.class,
                SisStudent.class,
                X2BaseBean.COL_OID);
    }

    /**
     * Returns the numeric value for the passed grade and grade scale.
     *
     * @param grade String
     * @param gradeScale GradeScale
     * @param schoolCourse SchoolCourse
     * @return double
     */
    private double getNumericGrade(String grade, GradeScale gradeScale, SchoolCourse schoolCourse) {
        double numericGrade = 0.0;

        if (grade != null) {
            if (StringUtils.isNumeric(grade)) {
                numericGrade = Double.parseDouble(grade);
            } else {
                BigDecimal numericValue = m_gradesManager.getNumericValue(grade, gradeScale,
                        schoolCourse.getSchool(), schoolCourse.getOid());
                if (numericValue != null) {
                    numericGrade = numericValue.doubleValue();
                }
            }
        }

        return numericGrade;
    }

    /**
     * Loads a map of Transcript lists keyed to the student OID.
     */
    private void loadTranscriptsByStudentOid() {
        m_transcriptsByStudentOid = new HashMap<String, List<Transcript>>(4096);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        criteria.addAndCriteria(
                m_studentCriteria.copyWithAdjustedPath(Transcript.REL_STUDENT, Transcript.COL_STUDENT_OID));

        Collection<String> trnDefinitionOids = new LinkedList<String>();
        for (TranscriptColumnDefinition transcriptColumn : m_transcriptColumns) {
            trnDefinitionOids.add(transcriptColumn.getTranscriptDefinitionOid());
        }

        if (trnDefinitionOids.size() > 0) {
            criteria.addIn(Transcript.COL_TRANSCRIPT_DEFINITION_OID, trnDefinitionOids);
        }

        QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);
        query.addOrderByAscending(Transcript.COL_STUDENT_OID);
        query.addOrderByAscending(Transcript.REL_TRANSCRIPT_DEFINITION + PATH_DELIMITER +
                TranscriptDefinition.COL_TRANSCRIPT_DEFINITION_NAME);
        query.addOrderByAscending(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_DESCRIPTION);
        query.addOrderByAscending(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

        SisStudent lastStudent = null;
        List<Transcript> allTranscripts = new ArrayList<Transcript>();
        List<Transcript> qualifyingTranscripts = new ArrayList<Transcript>();

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Transcript transcript = (Transcript) iterator.next();
                SisStudent student = transcript.getStudent();

                if (lastStudent != null && !lastStudent.equals(student)) {
                    if ((m_minCourses == null
                            || (m_minCourses != null && allTranscripts.size() >= m_minCourses.intValue())) &&
                            (StringUtils.isEmpty(m_minGrade) || (!StringUtils.isEmpty(m_minGrade)
                                    && qualifyingTranscripts.size() == allTranscripts.size()))) {
                        m_transcriptsByStudentOid.put(lastStudent.getOid(), allTranscripts);
                    }

                    allTranscripts = new ArrayList<Transcript>();
                    qualifyingTranscripts = new ArrayList<Transcript>();
                }

                if (!StringUtils.isEmpty(m_minGrade)) {
                    boolean qualifying = true;

                    for (TranscriptColumnDefinition transcriptColumn : m_transcriptColumns) {
                        if (transcriptColumn.getTranscriptDefinitionOid()
                                .equals(transcript.getTranscriptDefinitionOid())) {
                            double numericMinGrade = getNumericGrade(m_minGrade, transcriptColumn.getGradeScale(),
                                    transcript.getSchoolCourse());
                            double numericGrade =
                                    getNumericGrade((String) transcript.getFieldValueByBeanPath(
                                            transcriptColumn.getDataFieldConfig().getDataField().getJavaName()),
                                            transcriptColumn.getGradeScale(),
                                            transcript.getSchoolCourse());

                            if (numericGrade < numericMinGrade) {
                                qualifying = false;

                                break;
                            }
                        }
                    }

                    if (qualifying) {
                        qualifyingTranscripts.add(transcript);
                    }
                }

                allTranscripts.add(transcript);

                lastStudent = student;
            }

            if (lastStudent != null &&
                    (m_minCourses == null || (m_minCourses != null && allTranscripts.size() >= m_minCourses.intValue()))
                    &&
                    (StringUtils.isEmpty(m_minGrade) || (!StringUtils.isEmpty(m_minGrade)
                            && qualifyingTranscripts.size() == allTranscripts.size()))) {
                m_transcriptsByStudentOid.put(lastStudent.getOid(), allTranscripts);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads a list of Transcript Column Definitions as selected from the user input.
     */
    private void loadTranscriptColumns() {
        m_transcriptColumns = new ArrayList<TranscriptColumnDefinition>();

        String trnColumnDefinitionOids = (String) getParameter(PARAM_TRN_COLUMN_DEF_OIDS);
        if (!StringUtils.isEmpty(trnColumnDefinitionOids)) {
            Criteria criteria = new Criteria();
            criteria.addIn(X2BaseBean.COL_OID,
                    StringUtils.convertDelimitedStringToList(trnColumnDefinitionOids, ',', true));

            QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
            query.addOrderByAscending(TranscriptColumnDefinition.COL_GRADE_COLUMN_HEADER);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    TranscriptColumnDefinition transcriptColumn = (TranscriptColumnDefinition) iterator.next();

                    m_transcriptColumns.add(transcriptColumn);
                }
            } finally {
                iterator.close();
            }
        }
    }
}
