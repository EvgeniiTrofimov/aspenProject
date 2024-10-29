/**
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.md;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.md.MDStudentReportData;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Attendance for MSDE Quarterly Metrics" report for Allegany.
 *
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class MSDEQuarterlyMetricsFailingGradesData extends ReportJavaSourceNet {

    // Grid rows
    private static final String CATEGORY_ASIAN = "Asian";
    private static final String CATEGORY_BLACK = "Black/African-American";
    private static final String CATEGORY_FARMS = "Economically Disadvantaged";
    private static final String CATEGORY_FEMALE = "Female";
    private static final String CATEGORY_HISPANIC = "Hispanic";
    private static final String CATEGORY_IEP = "Students with Disabilities";
    private static final String CATEGORY_INDIAN = "American Indian/Alaskan Native";
    private static final String CATEGORY_LEP = "English Learners";
    private static final String CATEGORY_MALE = "Male";
    private static final String CATEGORY_MULTIRACE = "Two or More Races";
    private static final String CATEGORY_PACIFIC = "Hawaiian/Pacific Islander";
    private static final String CATEGORY_TOTAL = "All students";
    private static final String CATEGORY_WHITE = "White";

    private static final String TRANSCRIPT_NAMES_DELIMITER = ", ";

    private static final String DEPARTMENT_CODE_MATH = "Math";
    private static final String DEPARTMENT_CODE_ENGLISH = "English";
    private static final String DEPARTMENT_CODE_SCIENCE = "Science";
    private static final String DEPARTMENT_CODE_SOCIAL_STUDIES = "Social Studies";


    private static final String FAILING_GRADE_LETTER = "F";
    private static final Double FAILING_GRADE_NUMERIC = 55.0;

    private static final Pattern GRADE_RANGE_PATTERN_ELEMENTARY = Pattern.compile("0[0-5]");
    private static final Pattern GRADE_RANGE_PATTERN_MIDDLE = Pattern.compile("0[6-8]");

    // List of grades to include by state code
    private static final List<String> GRADES_INCLUDED =
            Arrays.asList("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12");

    // Report parameters
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_TRANSCRIPT_OIDS = "transcriptColumnDefinitionOids";

    private static final String MESSAGE_ALL_SCHOOLS = "report.shared.school.all";

    // Report parameters
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_SCHOOL_NAME = "schoolName";
    private static final String PARAM_TRANSCRIPT_COLUMNS = "transcriptColumns";

    // Grade level prefixes
    private static final String PREFIX_GRADE_ELEMENTARY = "elementary";
    private static final String PREFIX_GRADE_MIDDLE = "middle";
    private static final String PREFIX_GRADE_HIGH = "high";

    // Program codes
    private static final String PROGRAM_CODE_ELL = "ELL";
    private static final String PROGRAM_CODE_IEP = "IEP";

    // Race code constants
    private static final String RACE_ASIAN = "Asian";
    private static final String RACE_BLACK = "Black";
    private static final String RACE_INDIAN = "Am Indian or Alaska Nat";
    private static final String RACE_PACIFIC_ISLANDER = "NatHawaiian Pacific Is";
    private static final String RACE_WHITE = "White";

    private static final List<String> ROW_LIST =
            Arrays.asList(CATEGORY_TOTAL, CATEGORY_INDIAN, CATEGORY_ASIAN, CATEGORY_BLACK, CATEGORY_HISPANIC,
                    CATEGORY_MULTIRACE, CATEGORY_PACIFIC, CATEGORY_WHITE, CATEGORY_IEP, CATEGORY_LEP, CATEGORY_FARMS,
                    CATEGORY_MALE, CATEGORY_FEMALE);

    // Column suffixes
    private static final String SUFFIX_COUNT_FAILED = "_failed";
    private static final String SUFFIX_COUNT_TOTAL = "_total";
    private static final String SUFFIX_ENGLISH = "_english";
    private static final String SUFFIX_MATH = "_math";
    private static final String SUFFIX_SCIENCE = "_science";
    private static final String SUFFIX_SOCIAL_STUDIES = "_social";


    private Map<String, Collection<TranscriptColumnDefinition>> m_gtcByGtd;
    private Map<String, Collection<String>> m_raceCodes;
    private PlainDate m_reportDate;
    private List<String> m_transcriptColumnsNames;
    private Criteria m_transcriptCriteria;
    private Collection<String> m_studentPrograms;
    private UserDataContainer m_userData;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        Map<String, Integer> rowMap = initializeGrid(grid);

        QueryByCriteria transcriptQuery = new QueryByCriteria(Transcript.class, m_transcriptCriteria);

        QueryIterator transcriptIterator = null;
        try {
            transcriptIterator = getBroker().getIteratorByQuery(transcriptQuery);
            while (transcriptIterator.hasNext()) {
                Transcript trn = (Transcript) transcriptIterator.next();
                String gradeLevelPrefix = getGradeLevelPrefix(trn);
                String depSuffix = getDepSuffix(trn);
                List<String> categories = getStudentCategories(trn.getStudent());
                for (String category : categories) {
                    grid.gotoRow(rowMap.get(category));
                    if (hasGrade(trn)) {
                        incrementCount(grid, gradeLevelPrefix + depSuffix + SUFFIX_COUNT_TOTAL, 1.0);
                        if (isGradeFailed(trn)) {
                            incrementCount(grid, gradeLevelPrefix + depSuffix + SUFFIX_COUNT_FAILED, 1.0);
                        }
                    }
                }
            }
        } finally {
            if (transcriptIterator != null) {
                transcriptIterator.close();
            }
        }

        String schoolName = isSchoolContext() ? getSchool().getName()
                : WebUtils.getMessages(m_userData).getMessage(MESSAGE_ALL_SCHOOLS);
        addParameter(PARAM_SCHOOL_NAME, schoolName);
        addParameter(PARAM_REPORT_DATE, m_reportDate);
        addParameter(PARAM_TRANSCRIPT_COLUMNS,
                m_transcriptColumnsNames.stream().collect(Collectors.joining(TRANSCRIPT_NAMES_DELIMITER)));

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {

        m_reportDate = (PlainDate) getParameters().get(INPUT_PARAM_REPORT_DATE);

        MDStudentReportData reportData = new MDStudentReportData();
        reportData.setParameters(getParameters());
        reportData.setBroker(getBroker());
        reportData.setOrganization(getOrganization());
        reportData.setCurrentContext(getCurrentContext());
        reportData.setSchool(getSchool());
        reportData.setSchoolContext(isSchoolContext());
        reportData.setPrivilegeSet(getPrivilegeSet());

        StudentHistoryHelper studentHelper = new StudentHistoryHelper(reportData);
        studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                m_reportDate);

        m_transcriptCriteria = studentHelper.getStudentTranscriptCriteria();
        m_transcriptCriteria.addIn(Transcript.COL_GRADE_LEVEL, GRADES_INCLUDED);
        m_transcriptCriteria.addIn(
                Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.COL_DEPARTMENT_CODE,
                Arrays.asList(DEPARTMENT_CODE_MATH, DEPARTMENT_CODE_ENGLISH, DEPARTMENT_CODE_SCIENCE,
                        DEPARTMENT_CODE_SOCIAL_STUDIES));

        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentHelper.getStudentCriteria(), true);
        m_transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, subQuery);

        List<String> gtcOids =
                StringUtils.convertDelimitedStringToList((String) getParameter(INPUT_PARAM_TRANSCRIPT_OIDS), ",");
        X2Criteria gtcCriteria = new X2Criteria();
        gtcCriteria.addIn(X2BaseBean.COL_OID, gtcOids);

        QueryByCriteria gtcQuery = new QueryByCriteria(TranscriptColumnDefinition.class, gtcCriteria);
        m_gtcByGtd = getBroker().getGroupedCollectionByQuery(gtcQuery,
                TranscriptColumnDefinition.COL_TRANSCRIPT_DEFINITION_OID, 10);


        m_transcriptCriteria.addIn(Transcript.COL_TRANSCRIPT_DEFINITION_OID, m_gtcByGtd.keySet());

        SubQuery gtcNamesQuery =
                new SubQuery(TranscriptColumnDefinition.class, TranscriptColumnDefinition.COL_GRADE_NAME, gtcCriteria);
        m_transcriptColumnsNames = (List) getBroker().getSubQueryCollectionByQuery(gtcNamesQuery);

        loadRaceCodes();
        loadStudentPrograms();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_userData = userData;
    }

    /**
     * Returns a department name.
     *
     * @param transcript
     *
     * @return String
     */
    private String getDepSuffix(Transcript transcript) {
        String depCode = transcript.getSchoolCourse().getCourse().getDepartmentCode();
        if (DEPARTMENT_CODE_MATH.equals(depCode)) {
            return SUFFIX_MATH;
        } else if (DEPARTMENT_CODE_ENGLISH.equals(depCode)) {
            return SUFFIX_ENGLISH;
        } else if (DEPARTMENT_CODE_SCIENCE.equals(depCode)) {
            return SUFFIX_SCIENCE;
        } else if (DEPARTMENT_CODE_SOCIAL_STUDIES.equals(depCode)) {
            return SUFFIX_SOCIAL_STUDIES;
        }
        return null;
    }

    /**
     * Checks for grade.
     *
     * @param transcript Transcript
     * @return true, if successful
     */
    private boolean hasGrade(Transcript transcript) {
        boolean hasGrade = false;
        Collection<TranscriptColumnDefinition> columns = m_gtcByGtd.get(transcript.getTranscriptDefinitionOid());
        if (columns != null && !columns.isEmpty()) {
            for (TranscriptColumnDefinition column : columns) {
                if (!StringUtils.isEmpty((String) transcript
                        .getFieldValueByBeanPath(column.getDataFieldConfig().getDataField().getJavaName()))) {
                    hasGrade = true;
                    break;
                }
            }
        }
        return hasGrade;
    }

    /**
     * Returns true if student has failing grade.
     *
     * @param transcript
     *
     * @return String
     */
    private boolean isGradeFailed(Transcript transcript) {
        boolean failed = false;
        Collection<TranscriptColumnDefinition> columns = m_gtcByGtd.get(transcript.getTranscriptDefinitionOid());
        if (columns != null && !columns.isEmpty()) {
            for (TranscriptColumnDefinition column : columns) {
                String trnGrade = ((String) transcript
                        .getFieldValueByBeanPath(column.getDataFieldConfig().getDataField().getJavaName()));
                if (!StringUtils.isEmpty(trnGrade)) {
                    String grade = trnGrade.trim();
                    if (StringUtils.isNumeric(grade)) {
                        failed = Double.valueOf(grade).doubleValue() <= FAILING_GRADE_NUMERIC;
                    } else {
                        failed = FAILING_GRADE_LETTER.equals(grade);
                    }
                    break;
                }
            }
        }
        return failed;
    }

    /**
     * Returns a grade range name.
     *
     * @param transcript
     *
     * @return String
     */
    private String getGradeLevelPrefix(Transcript transcript) {
        String trnGrade = transcript.getGradeLevel();
        if (GRADE_RANGE_PATTERN_ELEMENTARY.matcher(trnGrade).matches()) {
            return PREFIX_GRADE_ELEMENTARY;
        } else if (GRADE_RANGE_PATTERN_MIDDLE.matcher(trnGrade).matches()) {
            return PREFIX_GRADE_MIDDLE;
        }
        return PREFIX_GRADE_HIGH;
    }

    /**
     * Returns a list of columns which apply to the passed student.
     *
     * @param student
     *
     * @return List<String>
     */
    private List<String> getStudentCategories(SisStudent student) {
        List<String> categories = new ArrayList<String>();
        categories.add(CATEGORY_TOTAL);

        // Add gender columns
        if (Person.GENDER_MALE.equals(student.getPerson().getGenderCode())) {
            categories.add(CATEGORY_MALE);
        } else if (Person.GENDER_FEMALE.equals(student.getPerson().getGenderCode())) {
            categories.add(CATEGORY_FEMALE);
        }

        // Add hispanic category
        if (student.getPerson().getHispanicLatinoIndicator()) {
            categories.add(CATEGORY_HISPANIC);
        }

        // Add race columns
        Collection<String> raceCodes = m_raceCodes.get(student.getPersonOid());
        if (!CollectionUtils.isEmpty(raceCodes)) {
            if (raceCodes.contains(RACE_ASIAN)) {
                categories.add(CATEGORY_ASIAN);
            }
            if (raceCodes.contains(RACE_BLACK)) {
                categories.add(CATEGORY_BLACK);
            }
            if (raceCodes.contains(RACE_INDIAN)) {
                categories.add(CATEGORY_INDIAN);
            }
            if (raceCodes.contains(RACE_PACIFIC_ISLANDER)) {
                categories.add(CATEGORY_PACIFIC);
            }
            if (raceCodes.contains(RACE_WHITE)) {
                categories.add(CATEGORY_WHITE);
            }
        }

        // Add multi race category
        if (raceCodes != null && raceCodes.size() > 1) {
            categories.add(CATEGORY_MULTIRACE);
        }

        // Add special education columns
        if (!StringUtils.isEmpty(student.getFieldA037())) {
            if (PROGRAM_CODE_IEP.equals(student.getFieldA037().trim())) {
                categories.add(CATEGORY_IEP);
            }
        }
        if ("1".equals(student.getFieldA003())) {
            categories.add(CATEGORY_FARMS);
        }
        if (m_studentPrograms.contains(student.getOid())) {
            categories.add(CATEGORY_LEP);
        }

        return categories;
    }

    /**
     * Increments the value at the passed column for the passed grid.
     *
     * @param grid
     * @param column
     * @param value
     */
    private void incrementCount(ReportDataGrid grid, String column, double value) {
        double currentValue = (Double) grid.get(column);
        grid.set(column, new Double(currentValue + value));
    }

    /**
     * Initializes the passed grid and returns a row map. The key of the map is a grade level code
     * and the value is the row number on the grid.
     *
     * @param grid
     *
     * @return Map<String, Integer>
     */
    private Map<String, Integer> initializeGrid(ReportDataGrid grid) {
        Map<String, Integer> rowMap = new HashMap<String, Integer>();
        for (String category : ROW_LIST) {

            grid.append();
            grid.set("studentCategory", category);

            for (String gradeLevel : Arrays.asList(PREFIX_GRADE_ELEMENTARY, PREFIX_GRADE_MIDDLE, PREFIX_GRADE_HIGH)) {
                for (String subject : Arrays.asList(SUFFIX_ENGLISH, SUFFIX_MATH, SUFFIX_SCIENCE,
                        SUFFIX_SOCIAL_STUDIES)) {
                    grid.set(gradeLevel + subject + SUFFIX_COUNT_FAILED, 0.0);
                    grid.set(gradeLevel + subject + SUFFIX_COUNT_TOTAL, 0.0);
                }
            }

            rowMap.put(category, grid.currentRowNumber());
        }

        grid.beforeTop();
        return rowMap;
    }

    /**
     * Loads a map of period OIDs keyed to collections of race codes.
     */
    private void loadRaceCodes() {
        SubQuery subQuery = new SubQuery(Transcript.class,
                Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_PERSON_OID,
                m_transcriptCriteria);

        Criteria criteria = new Criteria();
        criteria.addIn(Race.COL_PERSON_OID, subQuery);

        String[] columns = {Race.COL_PERSON_OID, Race.COL_RACE_CODE};
        ReportQueryByCriteria query = new ReportQueryByCriteria(Race.class, columns, criteria);

        m_raceCodes = getBroker().getGroupedColumnCollectionByQuery(query, 500);
    }

    /**
     * Loads a collection of students with an active ELL student program for the selected month.
     */
    private void loadStudentPrograms() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PROGRAM_CODE_ELL);
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

        Criteria endDateCriteria = new Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        endDateCriteria.addOrCriteria(orCriteria);
        criteria.addAndCriteria(endDateCriteria);

        SubQuery studentSubQuery = new SubQuery(Transcript.class,
                Transcript.REL_STUDENT + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, m_transcriptCriteria);
        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        SubQuery subQuery =
                new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID, criteria);
        m_studentPrograms = getBroker().getSubQueryCollectionByQuery(subQuery);
    }


}
