/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.Wizard;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.GradeTerm;
import com.x2dev.sis.model.beans.GradeTermDate;
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookColumnType;
import com.x2dev.sis.model.beans.GradebookScore;
import com.x2dev.sis.model.beans.GradebookSpecialCode;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.gradebook.AverageCalculator;
import com.x2dev.sis.model.business.gradebook.AverageCalculatorFactory;
import com.x2dev.sis.model.business.gradebook.CategoryAverageCalculator;
import com.x2dev.sis.model.business.gradebook.GradebookManager;
import com.x2dev.sis.model.business.gradebook.GradebookScoreManager;
import com.x2dev.sis.model.business.gradebook.OverallAverageCalculator;
import com.x2dev.sis.model.business.gradebook.TermAverageCalculator;
import com.x2dev.sis.tools.reports.GradebookReportDataSourceNet;
import com.x2dev.sis.web.gradebook.GradeInputUtils;
import com.x2dev.sis.web.gradebook.ProgressReportWizardKeys;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for gradebook progress reports.
 *
 * @author X2 Development Corporation
 */
public class GradebookProgressReportsData extends GradebookReportDataSourceNet implements ProgressReportWizardKeys {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String COL_ABSENCES = "absences";
    private static final String COL_ASSIGNMENT = "assignment";
    private static final String COL_AVERAGE = "average";
    private static final String COL_AVERAGE_LABEL = "averageLabel";
    private static final String COL_DISMISSALS = "dismissals";
    private static final String COL_GROUP = "group";
    private static final String COL_MESSAGE = "message";
    private static final String COL_SCORE = "score";
    private static final String COL_SECTION = "section";
    private static final String COL_STUDENT = "student";
    private static final String COL_STUDENT_FIELD_NAME_1 = "studentFieldName1";
    private static final String COL_STUDENT_FIELD_NAME_2 = "studentFieldName2";
    private static final String COL_STUDENT_FIELD_NAME_3 = "studentFieldName3";
    private static final String COL_STUDENT_FIELD_VALUE_1 = "studentFieldValue1";
    private static final String COL_STUDENT_FIELD_VALUE_2 = "studentFieldValue2";
    private static final String COL_STUDENT_FIELD_VALUE_3 = "studentFieldValue3";
    private static final String COL_TARDIES = "tardies";
    private static final String COL_TERM = "term";

    private static final String DEFAULT_ASSIGNMENT_SORT_OPTION = "0"; // Default assignment sort
    // order: Date due, then
    // category

    private static final String PARAM_CUSTOM_HEADER = "customHeader";
    private static final String PARAM_DEFAULT_TEACHER = "deafaultTeacherName";
    private static final String PARAM_INCLUDE_ADDRESS = "includeAddress";
    private static final String PARAM_INCLUDE_COMMENT_AREA = "includeCommentArea";
    private static final String PARAM_INCLUDE_LASID = "includeLasid";
    private static final String PARAM_INCLUDE_NAME = "includeName";
    private static final String PARAM_INCLUDE_SIGNATURE_LINE = "includeSignatureLine";
    private static final String PARAM_SHOW_CLASS_NAME = "showClassName";
    private static final String PARAM_SHOW_SCHOOL_NAME = "showSchoolName";
    private static final String PARAM_SHOW_TEACHER_NAME = "showTeacherName";
    private static final String PARAM_TEACHER_NAME = "teacherName";
    private static final String PARAM_USE_CUSTOM_HEADER = "useCustomHeader";

    private AssignmentComparator m_assignmentSortComparator;
    private Connection m_connection;
    private AverageCalculatorFactory m_calculatorFactory;
    private Collection m_studentsInSection;
    protected Wizard m_wizard;

    private PreparedStatement m_absentStatement;
    private PreparedStatement m_dismissedStatement;
    private PreparedStatement m_tardyStatement;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(100, 10);

        Iterator students = getStudentsToReport().iterator();
        while (students.hasNext()) {
            SisStudent student = (SisStudent) students.next();

            grid.append();
            grid.set(COL_STUDENT, student);

            // "Other" student fields
            addStudentFields(student, grid);

            // Message
            String message = (String) m_wizard.retrieveValue(3, S3_MESSAGE_KEY);
            if (!StringUtils.isEmpty(message)) {
                grid.set(COL_MESSAGE, message);
                grid.set(COL_STUDENT, student);
                grid.set(COL_GROUP, "0");
            }

            // Master object
            grid.set(COL_SECTION, getSection());

            // Assignments
            Map assignmentMap = getAssignments(student);
            Iterator assignments = assignmentMap.keySet().iterator();
            while (assignments.hasNext()) {
                GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();
                GradebookScore score = (GradebookScore) assignmentMap.get(assignment);

                grid.append();
                grid.set(COL_ASSIGNMENT, assignment);
                grid.set(COL_SCORE, score);
                grid.set(COL_STUDENT, student);
                grid.set(COL_GROUP, "1");
            }

            // Averages
            Map averageMap = getAverages(student);
            Iterator averageLabels = averageMap.keySet().iterator();
            while (averageLabels.hasNext()) {
                String averageLabel = (String) averageLabels.next();
                String averageValue = (String) averageMap.get(averageLabel);

                grid.append();
                grid.set(COL_AVERAGE_LABEL, averageLabel);
                grid.set(COL_AVERAGE, averageValue);
                grid.set(COL_STUDENT, student);
                grid.set(COL_GROUP, "2");
            }

            // Attendance
            Boolean includeAttendance =
                    Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_ATTENDANCE_SUMMARY_KEY));

            if (includeAttendance.booleanValue()) {
                addAttendanceSummary(student, grid);
            }
        }

        addReportParameters();

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.GradebookReportDataSource#initialize()
     */
    @Override
    @SuppressWarnings("deprecation")
    protected void initialize() {
        super.initialize();

        m_calculatorFactory = new AverageCalculatorFactory(getSection(),
                getDecimals(),
                getGradesManager(),
                getAverageScale(),
                getCalculationMode(),
                isCalculateFromStandards(),
                getStandards(),
                getBroker(),
                true);

        m_connection = getBroker().borrowConnection();

        prepareStatements();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web .
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_wizard = userData.getCurrentWizard().clone();
        m_assignmentSortComparator =
                new AssignmentComparator((String) m_wizard.retrieveValue(2, S2_ASSIGNMENT_SORT_KEY));
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#releaseResources()
     */
    @Override
    protected void releaseResources() {
        try {
            m_absentStatement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        }
        try {
            m_dismissedStatement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        }
        try {
            m_tardyStatement.close();
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        }

        getBroker().returnConnection();

        super.releaseResources();
    }

    /**
     * Adds attendance summary data for the passed student.
     *
     * @param student SisStudent
     * @param grid ReportDataGrid
     */
    private void addAttendanceSummary(SisStudent student, ReportDataGrid grid) {
        String dateString = (String) m_wizard.retrieveValue(4, S4_ATTENDANCE_SUMMARY_DATE_KEY);

        if (!StringUtils.isEmpty(dateString)) {
            Converter dateConverter = ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER, getLocale());
            PlainDate date = (PlainDate) dateConverter.stringToJava(dateString);

            if (date != null) {
                Iterator terms = getGradesManager().getGradeTerms(getSection()).iterator();
                while (terms.hasNext()) {
                    GradeTerm term = (GradeTerm) terms.next();
                    GradeTermDate termDate = GradesManager.getGradeTermDate(term.getOid(), getSchool().getOid(),
                            getCurrentContext().getOid(), getBroker());

                    if (termDate != null && !date.before(termDate.getStartDate())) {
                        try {
                            int absences = getSummaryAbsences(student, termDate);
                            int dismissals = getSummaryDismissals(student, termDate);
                            int tardies = getSummaryTardies(student, termDate);

                            grid.append();
                            grid.set(COL_TERM, term);
                            grid.set(COL_ABSENCES, Integer.valueOf(absences));
                            grid.set(COL_DISMISSALS, Integer.valueOf(dismissals));
                            grid.set(COL_TARDIES, Integer.valueOf(tardies));
                            grid.set(COL_STUDENT, student);
                            grid.set(COL_GROUP, "3");
                        } catch (SQLException sqle) {
                            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
                        }
                    }
                }
            }
        }
    }

    /**
     * Adds report paramters from the wizard.
     */
    private void addReportParameters() {
        // Wizard step 1
        Boolean includeName = Boolean.valueOf((String) m_wizard.retrieveValue(1, S1_INCLUDE_NAME_KEY));
        Boolean includeLasid = Boolean.valueOf((String) m_wizard.retrieveValue(1, S1_INCLUDE_ID_KEY));
        Boolean includeAddress = Boolean.valueOf((String) m_wizard.retrieveValue(1, S1_INCLUDE_ADDRESS_KEY));

        addParameter(PARAM_INCLUDE_NAME, includeName);
        addParameter(PARAM_INCLUDE_LASID, includeLasid);
        addParameter(PARAM_INCLUDE_ADDRESS, includeAddress);

        // Wizard step 4
        Boolean showSchoolName = Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_SCHOOL_NAME_KEY));
        Boolean showClassName = Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_CLASS_NAME_KEY));
        Boolean showTeacherName = Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_SHOW_TEACHER_NAME_KEY));
        String teacherName = (String) m_wizard.retrieveValue(4, S4_TEACHER_NAME_KEY);
        Boolean includeCommentArea = Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_COMMENT_AREA_KEY));
        Boolean includeSignatureLine = Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_SIGNATURE_LINE_KEY));
        Boolean useCustomHeader = Boolean.valueOf((String) m_wizard.retrieveValue(4, S4_USE_CUSTOM_HEADER_KEY));
        String customHeader = (String) m_wizard.retrieveValue(4, S4_CUSTOM_HEADER_KEY);

        addParameter(PARAM_SHOW_SCHOOL_NAME, showSchoolName);
        addParameter(PARAM_SHOW_CLASS_NAME, showClassName);
        addParameter(PARAM_SHOW_TEACHER_NAME, showTeacherName);
        addParameter(PARAM_DEFAULT_TEACHER, getSection().getPrimaryStaff().getNameView());
        addParameter(PARAM_TEACHER_NAME, teacherName);
        addParameter(PARAM_INCLUDE_COMMENT_AREA, includeCommentArea);
        addParameter(PARAM_INCLUDE_SIGNATURE_LINE, includeSignatureLine);
        addParameter(PARAM_USE_CUSTOM_HEADER, useCustomHeader);
        addParameter(PARAM_CUSTOM_HEADER, customHeader);
    }

    /**
     * Adds "other" student fields to the grid.
     *
     * @param student SisStudent
     * @param grid ReportDataGrid
     */
    private void addStudentFields(SisStudent student, ReportDataGrid grid) {
        DataFieldConfig districtField1 = (DataFieldConfig) m_wizard.retrieveValue(1, S1_OTHER_FIELD_1_KEY);
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        if (districtField1 != null) {
            Object rawValue1 = student.getFieldValueByBeanPath(districtField1.getDataField().getJavaName());
            String value1 = null;
            if (rawValue1 != null) {
                value1 = ReportUtils.getStringValue(rawValue1, districtField1.getDataFieldOid(), dictionary,
                        getLocale());
            }

            grid.set(COL_STUDENT_FIELD_VALUE_1, value1);
            grid.set(COL_STUDENT_FIELD_NAME_1, districtField1.getUserLongName());
        }

        DataFieldConfig districtField2 = (DataFieldConfig) m_wizard.retrieveValue(1, S1_OTHER_FIELD_2_KEY);

        if (districtField2 != null) {
            Object rawValue2 = student.getFieldValueByBeanPath(districtField2.getDataField().getJavaName());
            String value2 = null;
            if (rawValue2 != null) {
                value2 = ReportUtils.getStringValue(rawValue2, districtField2.getDataFieldOid(), dictionary,
                        getLocale());
            }

            grid.set(COL_STUDENT_FIELD_VALUE_2, value2);
            grid.set(COL_STUDENT_FIELD_NAME_2, districtField2.getUserLongName());
        }

        DataFieldConfig districtField3 = (DataFieldConfig) m_wizard.retrieveValue(1, S1_OTHER_FIELD_3_KEY);

        if (districtField3 != null) {
            Object rawValue3 = student.getFieldValueByBeanPath(districtField3.getDataField().getJavaName());
            String value3 = null;
            if (rawValue3 != null) {
                value3 = ReportUtils.getStringValue(rawValue3, districtField3.getDataFieldOid(), dictionary,
                        getLocale());
            }

            grid.set(COL_STUDENT_FIELD_VALUE_3, value3);
            grid.set(COL_STUDENT_FIELD_NAME_3, districtField3.getUserLongName());
        }
    }

    /**
     * Returns a Criteria that finds all assignments for the passed term/category selections.
     *
     * @param termSelection a grade term OID, or <code>OPTION_ALL</code>
     * @param categorySelection a category OID, or <code>OPTION_ALL</code>
     *
     * @return Criteria
     */
    private Criteria getAssignmentCriteria(String termSelection, String categorySelection) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, getSection().getOid());
        criteria.addEqualTo(GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);
        criteria.addNotEqualTo(GradebookColumnDefinition.COL_NOT_GRADED_INDICATOR, Boolean.TRUE);

        // Staff criteria - include both primary and secondary teachers
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(ScheduleTeacher.COL_STAFF_OID, getStaff().getOid());
        subCriteria.addEqualTo(ScheduleTeacher.COL_SECTION_OID, getSection().getOid());
        SubQuery subQuery = new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_SECTION_OID, subCriteria);

        criteria.addIn(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, subQuery);

        if (!OPTION_ALL.equals(termSelection)) {
            criteria.addEqualTo(GradebookColumnDefinition.COL_GRADE_TERM_OID, termSelection);
        }
        if (!OPTION_ALL.equals(categorySelection)) {
            criteria.addEqualTo(GradebookColumnDefinition.COL_COLUMN_TYPE_OID, categorySelection);
        }

        return criteria;
    }

    /**
     * Returns the assignments to display on the progress report for the passed student.
     *
     * @param student SisStudent
     * @return Map of GradebookScore objects keyed on GradebookColumnDefinition
     */
    private Map getAssignments(SisStudent student) {
        TreeMap assignments = new TreeMap(m_assignmentSortComparator);

        Boolean includeAll =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_ALL_ASSIGNMENTS_KEY));
        Boolean includeDropped =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_DROPPED_ASSIGNMENTS_KEY));
        Boolean includeFailed =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_FAILED_ASSIGNMENTS_KEY));
        Boolean includeMissing =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_MISSING_ASSIGNMENTS_KEY));
        Boolean includeSelected =
                Boolean.valueOf((String) m_wizard.retrieveValue(2,
                        S2_INCLUDE_SELECTED_ASSIGNMENTS_KEY + getSection().getOid()));

        if (includeAll.booleanValue()) {
            assignments.putAll(getAssignmentsAll(student));
        } else {
            if (includeDropped.booleanValue()) {
                assignments.putAll(getAssignmentsDropped(student));
            }

            if (includeFailed.booleanValue()) {
                assignments.putAll(getAssignmentsFailed(student));
            }

            if (includeMissing.booleanValue()) {
                assignments.putAll(getAssignmentsMissing(student));
            }

            if (includeSelected.booleanValue()) {
                assignments.putAll(getAssignmentsSelected(student));
            }
        }

        return assignments;
    }

    /**
     * Returns all assignments for the passed student.
     *
     * @param student SisStudent
     * @return Map of GradebookScore objects keyed on GradebookColumnDefinition objects
     */
    private Map getAssignmentsAll(SisStudent student) {
        Map assignmentsAll = new HashMap();

        String termSelection = (String) m_wizard.retrieveValue(2, S2_ALL_TERM_KEY);
        String categorySelection = (String) m_wizard.retrieveValue(2, S2_ALL_CATEGORY_KEY);

        Criteria scoreCriteria = getScoreCriteria(student, termSelection, categorySelection);
        QueryByCriteria scoreQuery = new QueryByCriteria(GradebookScore.class, scoreCriteria);
        Map scores = getBroker().getMapByQuery(scoreQuery, GradebookScore.COL_COLUMN_DEFINITION_OID, 50);

        Criteria assignmentCriteria = getAssignmentCriteria(termSelection, categorySelection);
        QueryByCriteria assignmentQuery = new QueryByCriteria(GradebookColumnDefinition.class, assignmentCriteria);
        QueryIterator assignments = getBroker().getIteratorByQuery(assignmentQuery);

        try {
            while (assignments.hasNext()) {
                GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();

                GradebookScore score = (GradebookScore) scores.get(assignment.getOid());

                boolean missing = false;
                if (score != null) {
                    GradebookSpecialCode specialCode = score.getSpecialCode(getBroker());
                    if (specialCode != null && specialCode.getMissingIndicator()) {
                        missing = true;
                    }
                } else {
                    missing = true;
                }

                if (missing) {
                    assignmentsAll.put(assignment, null);
                } else {
                    assignmentsAll.put(assignment, score);
                }
            }
        } finally {
            assignments.close();
        }

        return assignmentsAll;
    }

    /**
     * Returns dropped assignments for the passed student.
     *
     * @param student SisStudent
     * @return Map of GradebookScore objects keyed on GradebookColumnDefinition objects
     */
    private Map getAssignmentsDropped(SisStudent student) {
        Map dropped = new LinkedHashMap(30);

        String termSelection = (String) m_wizard.retrieveValue(2, S2_DROPPED_TERM_KEY);
        String categorySelection = (String) m_wizard.retrieveValue(2, S2_DROPPED_CATEGORY_KEY);

        Criteria scoreCriteria = getScoreCriteria(student, termSelection, categorySelection);
        scoreCriteria.addEqualTo(GradebookScore.COL_DROPPED_INDICATOR, Boolean.TRUE);

        QueryByCriteria scoreQuery = new QueryByCriteria(GradebookScore.class, scoreCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(scoreQuery);

        try {
            while (iterator.hasNext()) {
                GradebookScore score = (GradebookScore) iterator.next();

                dropped.put(score.getColumnDefinition(), score);
            }
        } finally {
            iterator.close();
        }

        return dropped;
    }


    /**
     * Returns the failed assignments for the passed student. A failed assignment is an assignment
     * with a grade corresponding to a grade scale code that does not earn credit.
     *
     * @param student SisStudent
     * @return Map of GradebookScore objects keyed on GradebookColumnDefinition objects
     */
    private Map getAssignmentsFailed(SisStudent student) {
        Map assignmentsFailed = new HashMap(30);

        String termSelection = (String) m_wizard.retrieveValue(2, S2_FAILED_TERM_KEY);
        String categorySelection = (String) m_wizard.retrieveValue(2, S2_FAILED_CATEGORY_KEY);

        Criteria criteria = getScoreCriteria(student, termSelection, categorySelection);
        QueryByCriteria query = new QueryByCriteria(GradebookScore.class, criteria);
        QueryIterator scores = getBroker().getIteratorByQuery(query);

        try {
            while (scores.hasNext()) {
                GradebookScore score = (GradebookScore) scores.next();
                GradebookColumnDefinition assignment = score.getColumnDefinition();

                if (assignment.getGradeScale() != null) {
                    GradeScaleGradeDefinition gradeDefinition = null;

                    if (StringUtils.isNumeric(score.getScore())) {
                        double scaledScore = GradesManager.scale(Double.parseDouble(score.getScore()),
                                0,
                                assignment.getTotalPoints().doubleValue(),
                                assignment.getGradeScale().getMinimumPoints().doubleValue(),
                                assignment.getGradeScale().getMaximumPoints().doubleValue(),
                                null,
                                null);

                        gradeDefinition = getGradesManager().getGradeDefinition(
                                new BigDecimal(String.valueOf(scaledScore)), assignment.getGradeScale(),
                                assignment.getMasterSchedule().getSchoolCourse().getSchoolOid(),
                                assignment.getMasterSchedule().getSchoolCourse().getOid());
                    } else {
                        gradeDefinition =
                                getGradesManager().getGradeDefinition(score.getScore(), assignment.getGradeScale(),
                                        assignment.getMasterSchedule().getSchoolCourse().getSchoolOid(),
                                        assignment.getMasterSchedule().getSchoolCourse().getOid());
                    }

                    if (gradeDefinition != null && !gradeDefinition.getCreditIndicator()) {
                        assignmentsFailed.put(assignment, score);
                    }
                }
            }
        } finally {
            scores.close();
        }

        return assignmentsFailed;
    }

    /**
     * Returns the missing assignments for the passed student. A missing assignment is an assignment
     * with no grade.
     *
     * @param student SisStudent
     * @return Map of null values keyed on GradebookColumnDefinition objects
     */
    private Map getAssignmentsMissing(SisStudent student) {
        Map assignmentsMissing = new LinkedHashMap(30);

        String termSelection = (String) m_wizard.retrieveValue(2, S2_MISSING_TERM_KEY);
        String categorySelection = (String) m_wizard.retrieveValue(2, S2_MISSING_CATEGORY_KEY);

        Criteria scoreCriteria = getScoreCriteria(student, termSelection, categorySelection);
        QueryByCriteria scoreQuery = new QueryByCriteria(GradebookScore.class, scoreCriteria);
        Map scores = getBroker().getMapByQuery(scoreQuery, GradebookScore.COL_COLUMN_DEFINITION_OID, 50);

        Criteria assignmentCriteria = getAssignmentCriteria(termSelection, categorySelection);
        QueryByCriteria assignmentQuery = new QueryByCriteria(GradebookColumnDefinition.class, assignmentCriteria);
        QueryIterator assignments = getBroker().getIteratorByQuery(assignmentQuery);

        try {
            while (assignments.hasNext()) {
                GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();

                GradebookScore score = (GradebookScore) scores.get(assignment.getOid());

                boolean missing = false;
                if (score != null) {
                    GradebookSpecialCode specialCode = score.getSpecialCode(getBroker());
                    if (specialCode != null && specialCode.getMissingIndicator()) {
                        missing = true;
                    }
                } else {
                    missing = true;
                }

                if (missing) {
                    assignmentsMissing.put(assignment, null);
                }
            }
        } finally {
            assignments.close();
        }

        return assignmentsMissing;
    }

    /**
     * Returns the user selected assignments for the passed student.
     *
     * @param student SisStudent
     * @return Map of GradebookScore objects keyed on GradebookColumnDefinition objects
     */
    private Map getAssignmentsSelected(SisStudent student) {
        Map assignmentsSelected = new LinkedHashMap(30);

        String selectedAssignments =
                (String) m_wizard.retrieveValue(2, S2_SELECTED_ASSIGNMENTS_KEY + getSection().getOid());
        Collection assignmentOidList = StringUtils.convertDelimitedStringToList(selectedAssignments, ',');

        if (!assignmentOidList.isEmpty()) {
            Criteria scoreCriteria = getScoreCriteria(student, OPTION_ALL, OPTION_ALL);
            scoreCriteria.addIn(GradebookScore.COL_COLUMN_DEFINITION_OID, assignmentOidList);

            QueryByCriteria scoreQuery = new QueryByCriteria(GradebookScore.class, scoreCriteria);
            Map scores = getBroker().getMapByQuery(scoreQuery, GradebookScore.COL_COLUMN_DEFINITION_OID, 50);

            Criteria assignmentCriteria = getAssignmentCriteria(OPTION_ALL, OPTION_ALL);
            assignmentCriteria.addIn(X2BaseBean.COL_OID, assignmentOidList);

            QueryByCriteria assignmentQuery = new QueryByCriteria(GradebookColumnDefinition.class, assignmentCriteria);
            QueryIterator assignments = getBroker().getIteratorByQuery(assignmentQuery);

            try {
                while (assignments.hasNext()) {
                    GradebookColumnDefinition assignment = (GradebookColumnDefinition) assignments.next();
                    GradebookScore score = (GradebookScore) scores.get(assignment.getOid());

                    assignmentsSelected.put(assignment, score);
                }
            } finally {
                assignments.close();
            }
        }

        return assignmentsSelected;
    }

    /**
     * Returns the overall average to display on the progress report.
     *
     * @param student SisStudent
     * @return single element Map containing the overall average (String) keyed on average label
     */
    private Map getAverageOverall(SisStudent student) {
        Map averages = new HashMap(1);

        AverageCalculator calculator = m_calculatorFactory.getOverallAverageCalculator(getStudentsAll());
        String averageKey = OverallAverageCalculator.getIdentifier(getSection().getOid());
        String averageLabel = GradeInputUtils.getAverageLabel(averageKey, false, false, getBroker(), getLocale());

        averages.put(averageLabel, calculator.getAverageView(student.getOid()));

        return averages;
    }

    /**
     * Returns the averages to display on the progress report.
     *
     * @param student SisStudent
     * @return Map of averages (Strings) keyed on average label
     */
    private Map getAverages(SisStudent student) {
        Map averages = new LinkedHashMap(15);

        Boolean includeCategory =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_CATEGORY_AVERAGES_KEY));
        Boolean includeTerm =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_TERM_AVERAGES_KEY));
        Boolean includeOverall =
                Boolean.valueOf((String) m_wizard.retrieveValue(2, S2_INCLUDE_OVERALL_AVERAGE_KEY));

        if (includeCategory.booleanValue()) {
            averages.putAll(getAveragesCategory(student));
        }

        if (includeTerm.booleanValue()) {
            averages.putAll(getAveragesTerm(student));
        }

        if (includeOverall.booleanValue()) {
            averages.putAll(getAverageOverall(student));
        }

        return averages;
    }

    /**
     * Returns the category averages to display on the progress report.
     *
     * @param student SisStudent
     * @return Map of averages (Strings) keyed on average label
     */
    private Map getAveragesCategory(SisStudent student) {
        Map averages = new LinkedHashMap(15);

        String termSelection = (String) m_wizard.retrieveValue(2, S2_CATEGORY_AVERAGE_TERM_KEY);
        String categorySelection = (String) m_wizard.retrieveValue(2, S2_CATEGORY_AVERAGE_CATEGORY_KEY);

        GradebookManager gbManager = new GradebookManager(getBroker());
        Collection terms = getGradesManager().getGradeTerms(getSection());
        Collection categories = gbManager.getCategories(getSection().getOid(), "");// By omitting
        // the Staff OID,
        // we ensure that
        // all categories
        // for the
        // section are
        // included

        Iterator termIterator = terms.iterator();
        while (termIterator.hasNext()) {
            GradeTerm term = (GradeTerm) termIterator.next();

            if (OPTION_ALL.equals(termSelection) || term.getOid().equals(termSelection)) {
                Iterator categoryIterator = categories.iterator();
                while (categoryIterator.hasNext()) {
                    GradebookColumnType category = (GradebookColumnType) categoryIterator.next();

                    if (OPTION_ALL.equals(categorySelection) || category.getOid().equals(categorySelection)) {
                        AverageCalculator calculator =
                                m_calculatorFactory.getCategoryAverageCalculator(category, term, getStudentsAll());
                        String averageKey = CategoryAverageCalculator.getIdentifier(category.getOid(), term.getOid());
                        String averageLabel =
                                GradeInputUtils.getAverageLabel(averageKey, false, false, getBroker(), getLocale());

                        averages.put(averageLabel, calculator.getAverageView(student.getOid()));
                    }
                }
            }
        }

        return averages;
    }

    /**
     * Returns the term averages to display on the progress report.
     *
     * @param student SisStudent
     * @return Map of averages (Strings) keyed on average label
     */
    private Map getAveragesTerm(SisStudent student) {
        Map averages = new LinkedHashMap(15);

        String termSelection = (String) m_wizard.retrieveValue(2, S2_TERM_AVERAGE_TERM_KEY);

        Iterator termIterator = getGradesManager().getGradeTerms(getSection()).iterator();
        while (termIterator.hasNext()) {
            GradeTerm term = (GradeTerm) termIterator.next();

            if (OPTION_ALL.equals(termSelection) || term.getOid().equals(termSelection)) {
                AverageCalculator calculator = m_calculatorFactory.getTermAverageCalculator(term, getStudentsAll());
                String averageKey = TermAverageCalculator.getIdentifier(term.getOid());
                String averageLabel =
                        GradeInputUtils.getAverageLabel(averageKey, false, false, getBroker(), getLocale());

                averages.put(averageLabel, calculator.getAverageView(student.getOid()));
            }
        }

        return averages;
    }

    /**
     * Returns a Criteria that finds all GradebookScore objects for the passed student and
     * term/category selections.
     *
     * @param student SisStudent
     * @param termSelection a grade term OID, or <code>OPTION_ALL</code>
     * @param categorySelection a category OID, or <code>OPTION_ALL</code>
     * @return Criteria
     */
    private Criteria getScoreCriteria(SisStudent student, String termSelection, String categorySelection) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(GradebookScore.COL_STUDENT_OID, student.getOid());
        criteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + "." +
                GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, getSection().getOid());
        criteria.addNotEqualTo(GradebookScore.COL_SCORE,
                GradebookScoreManager.EXCLUDE_STUDENT_FROM_ASSIGNMENT_SPECIAL_CODE);

        // Staff criteria - include both primary and secondary teachers
        Criteria subCriteria = new Criteria();
        subCriteria.addEqualTo(ScheduleTeacher.COL_STAFF_OID, getStaff().getOid());
        subCriteria.addEqualTo(ScheduleTeacher.COL_SECTION_OID, getSection().getOid());
        SubQuery subQuery = new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_SECTION_OID, subCriteria);

        criteria.addIn(GradebookColumnDefinition.COL_MASTER_SCHEDULE_OID, subQuery);


        criteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + "." +
                GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        if (!OPTION_ALL.equals(termSelection)) {
            criteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + "." +
                    GradebookColumnDefinition.COL_GRADE_TERM_OID, termSelection);
        }
        if (!OPTION_ALL.equals(categorySelection)) {
            criteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + "." +
                    GradebookColumnDefinition.COL_COLUMN_TYPE_OID, categorySelection);
        }

        return criteria;
    }

    /**
     * Returns a collection of all enrolled students in the section.
     *
     * @return Collection of Student objects
     */
    private Collection getStudentsAll() {
        if (m_studentsInSection == null) {
            Criteria criteria =
                    GradebookManager
                            .getStudentCriteria(GradebookManager.STUDENT_FILTER_ENROLLED, getSection(), getBroker());

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
            query.addOrderByAscending(SisStudent.COL_LOCAL_ID);

            m_studentsInSection = getBroker().getCollectionByQuery(query);
        }

        return m_studentsInSection;
    }

    /**
     * Returns a collection students meeting the user specified average criteria.
     *
     * @return Collection of Student objects
     */
    private Collection getStudentsByAverage() {
        Collection students = new LinkedList();

        String averageColumn =
                (String) m_wizard.retrieveValue(0, S0_SELECTED_AVERAGE_COLUMN_KEY + getSection().getOid());
        String operator = (String) m_wizard.retrieveValue(0, S0_SELECTED_AVERAGE_OPERATOR_KEY);
        String averageValue = (String) m_wizard.retrieveValue(0, S0_SELECTED_AVERAGE_VALUE_KEY);

        if (!StringUtils.isEmpty(averageColumn) &&
                !StringUtils.isEmpty(operator) &&
                !StringUtils.isEmpty(averageValue)) {
            Collection allStudents = getStudentsAll();

            double averageToCompare = -1;
            if (StringUtils.isNumeric(averageValue)) {
                averageToCompare = Double.parseDouble(averageValue);
            } else if (getAverageScale() != null) {
                BigDecimal convertedValue = getGradesManager().getNumericValue(averageValue, getAverageScale(),
                        getSection().getSchoolCourse().getSchool(), getSection().getSchoolCourseOid());
                if (convertedValue != null) {
                    averageToCompare = convertedValue.doubleValue();
                }
            }

            if (averageToCompare != -1) {
                AverageCalculator calculator = m_calculatorFactory.getAverageCalculator(averageColumn, allStudents);

                Iterator studentsIterator = allStudents.iterator();
                while (studentsIterator.hasNext()) {
                    SisStudent student = (SisStudent) studentsIterator.next();
                    Double average = calculator.getAverageNumeric(student.getOid());

                    if (average != null) {
                        boolean include = false;
                        switch (Integer.parseInt(operator)) {
                            case OPERATOR_AT_LEAST:
                                include = average.doubleValue() >= averageToCompare;
                                break;

                            case OPERATOR_AT_MOST:
                            default:
                                include = average.doubleValue() <= averageToCompare;

                        }

                        if (include) {
                            students.add(student);
                        }
                    }
                }
            }
        }

        return students;
    }

    /**
     * Returns a collection of students manually selected by the user.
     *
     * @return Collection of Student objects
     */
    private Collection getStudentsSelected() {
        Collection students = new ArrayList(0);

        String oidList = (String) m_wizard.retrieveValue(0, S0_SELECTED_STUDENTS_KEY + getSection().getOid());
        List oids = StringUtils.convertDelimitedStringToList(oidList, ',');

        if (!oids.isEmpty()) {
            Criteria criteria = new Criteria();
            criteria.addIn(X2BaseBean.COL_OID, oids);

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
            query.addOrderByAscending(SisStudent.COL_NAME_VIEW);
            query.addOrderByAscending(SisStudent.COL_LOCAL_ID);

            students = getBroker().getCollectionByQuery(query);
        }

        return students;
    }

    /**
     * Returns the students to report. The students returned are one of the following:
     * <ul>
     * <li>All students in the section
     * <li>A manual selection of students
     * <li>Students meeting a specified average criteria
     * </ul>
     *
     * @return Collection of Student objects
     */
    private Collection getStudentsToReport() {
        Collection students = null;

        String studentsToInclude =
                (String) m_wizard.retrieveValue(0, S0_STUDENTS_TO_INCLUDE_KEY + getSection().getOid());
        switch (Integer.parseInt(studentsToInclude)) {
            case 0: // selected
                students = getStudentsAll();
                break;

            case 1: // average criteria
                students = getStudentsSelected();
                break;

            case 2: // all
            default:
                students = getStudentsByAverage();
        }

        return students;
    }

    /**
     * Returns the number of absences for the passed student and term.
     *
     * @param student SisStudent
     * @param termDate GradeTermDate
     * @return int
     * @throws SQLException exception
     */
    private int getSummaryAbsences(SisStudent student, GradeTermDate termDate) throws SQLException {
        return getSummaryValue(student, termDate, m_absentStatement);
    }

    /**
     * Returns the number of dismissals for the passed student and term.
     *
     * @param student SisStudent
     * @param termDate GradeTermDate
     * @return int
     * @throws SQLException exception
     */
    private int getSummaryDismissals(SisStudent student, GradeTermDate termDate) throws SQLException {
        return getSummaryValue(student, termDate, m_dismissedStatement);
    }

    /**
     * Returns the number of tardies for the passed student and term.
     *
     * @param student SisStudent
     * @param termDate GradeTermDate
     * @return int
     * @throws SQLException exception
     */
    private int getSummaryTardies(SisStudent student, GradeTermDate termDate) throws SQLException {
        return getSummaryValue(student, termDate, m_tardyStatement);
    }

    /**
     * Returns an attendance summary value.
     *
     * @param student SisStudent
     * @param termDate GradeTermDate
     * @param statement the absent, dismissed, or tardy statement
     * @return int
     * @throws SQLException exception
     */
    private int getSummaryValue(SisStudent student, GradeTermDate termDate, PreparedStatement statement)
            throws SQLException {
        int value = 0;

        statement.clearParameters();

        statement.setString(1, student.getOid());
        statement.setString(2, getSection().getOid());
        statement.setDate(3, termDate.getStartDate());
        statement.setDate(4, termDate.getEndDate());

        ResultSet results = statement.executeQuery();
        try {
            if (results.next()) {
                value = results.getInt(1);
            }
        } finally {
            results.close();
        }

        return value;
    }

    /**
     * Initializes prepared statements used to query attendance summaries.
     */
    private void prepareStatements() {
        try {
            // Absent
            StringBuilder queryString = new StringBuilder(128);

            queryString.append("SELECT COUNT(*) AS total ");
            queryString.append("FROM STUDENT_PERIOD_ATTENDANCE ");
            queryString.append("WHERE PAT_STD_OID = ? ");
            queryString.append("AND PAT_MST_OID = ? ");
            queryString.append("AND PAT_DATE >= ? ");
            queryString.append("AND PAT_DATE <= ? ");
            queryString.append("AND PAT_ABSENT_IND = '1' ");
            queryString.append("GROUP BY PAT_STD_OID ");

            m_absentStatement = m_connection.prepareStatement(queryString.toString());

            // Tardy
            queryString = new StringBuilder(128);

            queryString.append("SELECT COUNT(*) AS total ");
            queryString.append("FROM STUDENT_PERIOD_ATTENDANCE ");
            queryString.append("WHERE PAT_STD_OID = ? ");
            queryString.append("AND PAT_MST_OID = ? ");
            queryString.append("AND PAT_DATE >= ? ");
            queryString.append("AND PAT_DATE <= ? ");
            queryString.append("AND PAT_TARDY_IND = '1' ");
            queryString.append("GROUP BY PAT_STD_OID ");

            m_tardyStatement = m_connection.prepareStatement(queryString.toString());

            // Dismissed
            queryString = new StringBuilder(128);

            queryString.append("SELECT COUNT(*) AS total ");
            queryString.append("FROM STUDENT_PERIOD_ATTENDANCE ");
            queryString.append("WHERE PAT_STD_OID = ? ");
            queryString.append("AND PAT_MST_OID = ? ");
            queryString.append("AND PAT_DATE >= ? ");
            queryString.append("AND PAT_DATE <= ? ");
            queryString.append("AND PAT_DISMISSED_IND = '1' ");
            queryString.append("GROUP BY PAT_STD_OID ");

            m_dismissedStatement = m_connection.prepareStatement(queryString.toString());
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        }
    }

    /**
     * Comparator for assignment requirements .
     *
     * @author X2 Development Corporation
     */
    private class AssignmentComparator implements Comparator, Serializable {
        private String m_sort;

        /**
         * Default constructor.
         *
         * @param sort String
         */
        public AssignmentComparator(String sort) {
            m_sort = sort;
        }

        /**
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(Object object1, Object object2) {
            GradebookColumnDefinition assignment1 = (GradebookColumnDefinition) object1;
            GradebookColumnDefinition assignment2 = (GradebookColumnDefinition) object2;

            String compareString1 = getComparisonString(assignment1);
            String compareString2 = getComparisonString(assignment2);

            return compareString1.compareTo(compareString2);
        }

        /**
         * Returns the comparision string.
         *
         * @param assignment GradebookColumnDefinition
         * @return String
         */
        private String getComparisonString(GradebookColumnDefinition assignment) {
            StringBuilder buffer = new StringBuilder(32);

            if (DEFAULT_ASSIGNMENT_SORT_OPTION.equals(m_sort)) {
                buffer.append(assignment.getDateDue().getTime());
                buffer.append(assignment.getColumnType().getColumnType());
                buffer.append(assignment.getColumnCode());
            } else {
                buffer.append(assignment.getColumnType().getColumnType());
                buffer.append(assignment.getDateDue().getTime());
                buffer.append(assignment.getColumnCode());
            }

            return buffer.toString();
        }
    }
}
