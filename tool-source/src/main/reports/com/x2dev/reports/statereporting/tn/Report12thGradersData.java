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

package com.x2dev.reports.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentMultiYearHelper;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.*;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source of Data sources for the "TN Discipline Report" report and sub reports.
 *
 * @author X2 Development Corporation
 */
public class Report12thGradersData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    public static final String INPUT_ALL_SCHOOLS = "allSchools";

    private static final String ALIAS_GRADUATION_DATE = "DOE COMPLETION DOCUMENT DATE";
    private static final String ALIAS_GRADUATION_PERIOD = "DOE COMPLETION DOCUMENT PER";
    private static final String ALIAS_GRADUATION_TYPE = "DOE COMPLETION DOCUMENT TYPE";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String DATE_FORMAT = "yyyy-MM-dd";

    private static final String FIELD_GRADUATION_DATE = "graduationDate";
    private static final String FIELD_GRADUATION_PERIOD = "graduationPeriod";
    private static final String FIELD_SCHOOL = "school";
    private static final String FIELD_TABLE_ROW_DESCRIPTION = "tableRowDescription";
    private static final String FIELD_TABLE_ROW_NUMBER = "tableRowNumber";

    private static final String GENDER_CODE_FEMALE = "F";
    private static final String GENDER_CODE_MALE = "M";

    private static final String INPUT_CATEGORY_SPECIAL_PROGRAM = "rcdCategorySpecialProgram";
    private static final String INPUT_CODES_12_GRADE = "codes12Grade";
    private static final String INPUT_IS_SPECIAL_EDUCATION = "isSpecialEducation";
    private static final String INPUT_LIST_DELIMITER = "listDelimiter";
    private static final String INPUT_SCHOOLS = "schoolOids";

    private static final String PARAM_IS_SPED = "isSped";
    private static final String PARAM_SCHOOL_YEAR = "schoolYear";

    private static final String RACE_CODE_ASIAN = "A";
    private static final String RACE_CODE_BLACK = "B";
    private static final String RACE_CODE_INDIAN = "I";
    private static final String RACE_CODE_PACIFIC = "P";
    private static final String RACE_CODE_WHITE = "W";
    private static final String RACE_HISPANIC = "hispanic";

    private static final String ROW_V_CODE = "5";

    String m_graduationField;
    TNStudentMultiYearHelper m_multiYearHelper;

    private List<String> m_codes12Grade;
    private SimpleDateFormat m_dateFormat;
    private DataDictionary m_dictionary;
    private String m_documentDateJavaName;
    private String m_documentTypeJavaName;
    private String m_documentPeriodJavaName;
    private Boolean m_isSpecialReport;
    private Set<String> m_raceCodeCheckOrder;
    private Collection<SisSchool> m_schools;
    private Collection<String> m_schoolOids;
    private DistrictSchoolYearContext m_schoolYear;
    private Collection m_spedStdOids;
    private Map<String, Map<String, Integer>> m_table;
    private Set<String> m_tableColumns;
    private TreeMap<String, String> m_tableRows;
    private Map<String, String> m_typeCodesMap;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid();


        ReportQueryByCriteria studentQuery = getStudentQuery();

        m_multiYearHelper.setCriteria(studentQuery.getCriteria());

        if (studentQuery == null || m_schools.size() == 0) {
            return null;
        }
        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);
        Map<String, Collection<SisStudent>> schoolStudentsMap = new HashMap<String, Collection<SisStudent>>();
        for (SisStudent student : students) {
            String schoolOid = (String) m_multiYearHelper.getFieldValueByBeanPath(student, SisStudent.COL_SCHOOL_OID);

            Collection<SisStudent> currentStudents = schoolStudentsMap.get(schoolOid);
            if (currentStudents == null) {
                currentStudents = new ArrayList<SisStudent>();
                schoolStudentsMap.put(schoolOid, currentStudents);
            }
            currentStudents.add(student);
        }

        List<SisSchool> schools = new ArrayList(getSchools());
        Collections.sort(schools, new Comparator<SisSchool>() {

            @Override
            public int compare(SisSchool o1, SisSchool o2) {
                return o1.getName().compareTo(o2.getName());
            }

        });

        SisStudent prevStudent = null;
        SisSchool prevSchool = null;
        SisSchool school = null;
        String instructionLastDate = null;
        String graduationDate = null;
        String graduationPeriod = null;
        String prevGraduationDate = null;
        String prevGraduationPeriod = null;
        resetTableCounts();

        for (SisSchool currentSchool : schools) {
            if (m_isSpecialReport != null && m_isSpecialReport.booleanValue()) {
                instructionLastDate = getLastDate(currentSchool).toString();
            }

            Collection<SisStudent> currentStudents = schoolStudentsMap.get(currentSchool.getOid());

            if (currentStudents != null) {
                List<SisStudent> studentsList = new ArrayList<SisStudent>(currentStudents);

                Collections.sort(studentsList, new Comparator<SisStudent>() {
                    @Override
                    public int compare(SisStudent o1, SisStudent o2) {
                        return ((String) m_multiYearHelper.getFieldValueByBeanPath(o1, m_graduationField)).compareTo(
                                (String) m_multiYearHelper.getFieldValueByBeanPath(o2, m_graduationField));
                    }

                });

                for (SisStudent student : studentsList) {
                    if ((m_isSpecialReport != null && m_isSpecialReport.booleanValue()) &&
                            (m_spedStdOids.isEmpty() || !m_spedStdOids.contains(student.getOid()))) {
                        continue;
                    }
                    school = currentSchool;
                    graduationPeriod =
                            (String) m_multiYearHelper.getFieldValueByBeanPath(student, m_documentPeriodJavaName);
                    graduationDate =
                            (String) m_multiYearHelper.getFieldValueByBeanPath(student, m_documentDateJavaName);

                    if ((m_isSpecialReport != null && m_isSpecialReport.booleanValue())
                            && (!ObjectUtils.matchStrict(school, prevSchool) && prevSchool != null)
                            || (!ObjectUtils.matchStrict(graduationDate, prevGraduationDate)
                                    && prevGraduationDate != null)) {
                        if (graduationDate != null) {
                            expandTable(grid, prevSchool, prevGraduationPeriod, prevGraduationDate);
                            resetTableCounts();

                        } else {
                            instructionLastDate = getLastDate(school).toString();
                            expandTable(grid, prevSchool, prevGraduationPeriod, instructionLastDate);
                            resetTableCounts();
                        }
                    } else if (((m_isSpecialReport != null && !m_isSpecialReport.booleanValue())
                            || m_isSpecialReport == null)
                            && (!ObjectUtils.matchStrict(school, prevSchool) && prevSchool != null)
                            || (!ObjectUtils.matchStrict(graduationPeriod, prevGraduationPeriod)
                                    && prevGraduationPeriod != null)) {
                        expandTable(grid, prevSchool, prevGraduationPeriod, prevGraduationDate);
                        resetTableCounts();
                    }
                    if (m_isSpecialReport != null && m_isSpecialReport.booleanValue()) {
                        if (!m_spedStdOids.isEmpty() && m_spedStdOids.contains(student.getOid())) {
                            addToCount(student);
                            prevStudent = student;
                            prevSchool = school;
                            prevGraduationDate = graduationDate;
                        } else {
                            prevStudent = student;
                            prevSchool = school;
                            prevGraduationDate = graduationDate;
                        }

                    } else {
                        addToCount(student);
                        prevStudent = student;
                        prevSchool = school;
                        prevGraduationPeriod = graduationPeriod;
                    }
                }

                if (prevStudent != null) {
                    expandTable(grid, school, graduationPeriod, graduationDate);
                    resetTableCounts();
                }
                prevStudent = null;
                prevSchool = null;
                prevGraduationDate = null;
                prevGraduationPeriod = null;
            } else {
                expandTable(grid, currentSchool, graduationPeriod, instructionLastDate);
                resetTableCounts();
            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_schools = getSchools();
        m_schoolOids = new ArrayList<String>();
        for (SisSchool school : m_schools) {
            m_schoolOids.add(school.getOid());
        }

        m_raceCodeCheckOrder = new LinkedHashSet();
        m_raceCodeCheckOrder.add(RACE_CODE_BLACK);
        m_raceCodeCheckOrder.add(RACE_CODE_INDIAN);
        m_raceCodeCheckOrder.add(RACE_CODE_PACIFIC);
        m_raceCodeCheckOrder.add(RACE_CODE_ASIAN);
        m_raceCodeCheckOrder.add(RACE_CODE_WHITE);

        m_tableColumns = new LinkedHashSet();
        for (String genderCode : new String[] {GENDER_CODE_MALE, GENDER_CODE_FEMALE}) {
            for (String raceCode : new String[] {RACE_HISPANIC, RACE_CODE_BLACK, RACE_CODE_INDIAN, RACE_CODE_PACIFIC,
                    RACE_CODE_ASIAN, RACE_CODE_WHITE}) {
                m_tableColumns.add(getColumnKey(genderCode, raceCode));
            }
        }

        m_schoolYear = getCurrentContext();
        addParameter(PARAM_SCHOOL_YEAR, m_schoolYear);

        m_isSpecialReport = Boolean.valueOf(Boolean.TRUE.equals(getParameter(INPUT_IS_SPECIAL_EDUCATION)));
        addParameter(PARAM_IS_SPED, m_isSpecialReport);
        char delimiter = ((String) getParameter(INPUT_LIST_DELIMITER)).charAt(0);
        String undelimitedCodes12Grade = (String) getParameter(INPUT_CODES_12_GRADE);
        m_codes12Grade = StringUtils.convertDelimitedStringToList(undelimitedCodes12Grade, delimiter);

        DataDictionaryField fieldDocumetType = m_dictionary.findDataDictionaryFieldByAlias(ALIAS_GRADUATION_TYPE);
        m_documentTypeJavaName = fieldDocumetType.getJavaName();
        handleTypesRefTable(fieldDocumetType);

        m_documentDateJavaName = translateAliasToJavaName(ALIAS_GRADUATION_DATE);

        m_documentPeriodJavaName = translateAliasToJavaName(ALIAS_GRADUATION_PERIOD);

        if (m_isSpecialReport != null && m_isSpecialReport.booleanValue()) {
            m_graduationField = m_documentDateJavaName;
        } else {
            m_graduationField = m_documentPeriodJavaName;
        }

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        m_multiYearHelper = new TNStudentMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
    }

    /**
     * Increment count for suitable table.
     *
     * @param student SisStudent
     */
    private void addToCount(SisStudent student) {
        String columnKey = getColumnKey(student.getPerson());
        if (columnKey == null) {
            return;
        }

        String rowKey = getDocumentType(student);
        if (rowKey == null) {
            return;
        }

        incTableValue(rowKey, columnKey);

    }

    /**
     * Generate dataGrid with values.
     *
     * @param grid ReportDataGrid
     * @param school SisSchool
     * @param graduationPeriod String
     * @param graduationDate String
     */
    private void expandTable(ReportDataGrid grid, SisSchool school, String graduationPeriod, String graduationDate) {
        for (Object rowKey : m_tableRows.keySet()) {
            Map row = m_table.get(rowKey);
            grid.append(row);
            grid.set(FIELD_TABLE_ROW_NUMBER, rowKey);
            grid.set(FIELD_TABLE_ROW_DESCRIPTION, m_tableRows.get(rowKey));
            grid.set(FIELD_SCHOOL, school);
            if (m_isSpecialReport.booleanValue()) {
                grid.set(FIELD_GRADUATION_DATE, graduationDate);
            } else {
                grid.set(FIELD_GRADUATION_PERIOD, graduationPeriod);
            }
        }
    }

    /**
     * Generate column key for given person.
     *
     * @param person SisPerson
     * @return String
     */
    private String getColumnKey(SisPerson person) {
        if (person == null) {
            return null;
        }
        String genderCode = person.getGenderCode();
        if (genderCode == null) {
            return null;
        }

        if (person.getHispanicLatinoIndicator()) {
            return getColumnKey(genderCode, RACE_HISPANIC);
        }

        Collection races = person.getRaces();
        Collection personRaceCodes = CollectionUtils.getPropertyCollection(races, Race.COL_RACE_CODE);

        for (String raceCode : m_raceCodeCheckOrder) {
            if (personRaceCodes.contains(raceCode)) {
                return getColumnKey(genderCode, raceCode);
            }
        }

        return null;
    }

    /**
     * Rule to calculate column parameter.
     *
     * @param genderCode String
     * @param raceCode String
     * @return String
     */
    private String getColumnKey(String genderCode, String raceCode) {
        return genderCode + "_" + raceCode;
    }

    /**
     * Calculate document type by student.
     *
     * @param student SisStudent
     * @return String
     */
    private String getDocumentType(SisStudent student) {
        Object documentDateValue = m_multiYearHelper.getFieldValueByBeanPath(student, m_documentDateJavaName);

        PlainDate documentDate = null;
        if (documentDateValue instanceof PlainDate) {
            documentDate = (PlainDate) documentDateValue;
        } else {

            try {
                documentDate = new PlainDate(m_dateFormat.parse(documentDateValue.toString()));
            } catch (Exception e) {
                // do nothing
            }
        }

        if (documentDate == null) {
            return ROW_V_CODE;
        }

        if (documentDate.after(m_schoolYear.getEndDate()) || documentDate.before(m_schoolYear.getStartDate())) {
            return null;
        }

        Object documentTypeValue = m_multiYearHelper.getFieldValueByBeanPath(student, m_documentTypeJavaName);

        if (m_typeCodesMap.keySet().contains(documentTypeValue) &&
                m_tableRows.keySet().contains(m_typeCodesMap.get(documentTypeValue))) {
            return (String) documentTypeValue;
        }

        return null;
    }

    /**
     * Last day of instruction of the school calendar year.
     *
     * @param school SisSchool
     * @return Plain date
     */
    private PlainDate getLastDate(SisSchool school) {
        PlainDate value = null;
        PlainDate m_districtEndDate = m_schoolYear.getEndDate();
        if (school.getActiveSchedule() != null) {
            for (ScheduleTerm term : school.getActiveSchedule().getScheduleTerms()) {
                for (ScheduleTermDate date : term.getScheduleTermDates()) {
                    if (value == null) {
                        value = date.getEndDate();
                    } else {
                        if (value.after(date.getEndDate())) {
                            value = date.getEndDate();
                        }
                    }
                }
            }
        }
        if (value == null) {
            value = m_districtEndDate;
        }
        return value;
    }

    /**
     * Create endDate criteria: <b>colEndDate >= endDate or colEndDate is null</b> .
     *
     * @param colEndDate String
     * @param endDate PlainDate
     * @return X 2 criteria
     */
    private X2Criteria getEndDateCriteria(String colEndDate, PlainDate endDate) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEmpty(colEndDate, getBroker().getPersistenceKey());

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addGreaterOrEqualThan(colEndDate, endDate);
        criteria.addOrCriteria(orCriteria);

        return criteria;
    }

    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                X2Criteria schoolCriteria = new X2Criteria();

                schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                schools = getBroker().getCollectionByQuery(schoolQuery);
            }
        }

        return schools;
    }

    /**
     * Create SubQuery for student oids who have special education in current context.
     * A student is a special education student if the have a StudentProgramParticipation
     * record with a program reference code with rcdCategory == "Options".
     *
     *
     * @return Sub query
     */
    private SubQuery getStudentOidsSpecialEducationSubQuery() {
        X2Criteria studentOidsCriteria = new X2Criteria();

        X2Criteria dateCriteria = new X2Criteria();
        {
            X2Criteria startIntersectionCriteria = new X2Criteria();
            startIntersectionCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                    m_schoolYear.getStartDate());
            startIntersectionCriteria.addAndCriteria(getEndDateCriteria(StudentProgramParticipation.COL_END_DATE,
                    m_schoolYear.getStartDate()));
            dateCriteria.addAndCriteria(startIntersectionCriteria);
        }

        {
            X2Criteria endIntersectionCriteria = new X2Criteria();
            endIntersectionCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                    m_schoolYear.getEndDate());
            endIntersectionCriteria.addAndCriteria(getEndDateCriteria(StudentProgramParticipation.COL_END_DATE,
                    m_schoolYear.getEndDate()));
            dateCriteria.addOrCriteria(endIntersectionCriteria);
        }

        {
            X2Criteria spanInsideCriteria = new X2Criteria();
            spanInsideCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                    m_schoolYear.getStartDate());
            spanInsideCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_schoolYear.getEndDate());
            dateCriteria.addOrCriteria(spanInsideCriteria);
        }

        studentOidsCriteria.addAndCriteria(dateCriteria);

        X2Criteria rcdCriteria = new X2Criteria();
        DataDictionaryField field = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey())
                .findDataDictionaryField(StudentProgramParticipation.class.getName(),
                        StudentProgramParticipation.COL_PROGRAM_CODE);
        rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        rcdCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, getParameter(INPUT_CATEGORY_SPECIAL_PROGRAM));
        SubQuery rcdQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, rcdCriteria);
        Collection codes = getBroker().getSubQueryCollectionByQuery(rcdQuery);
        if (codes.isEmpty()) {
            return null;
        }
        studentOidsCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, codes);

        return new SubQuery(StudentProgramParticipation.class, StudentProgramParticipation.COL_STUDENT_OID,
                studentOidsCriteria);
    }

    /**
     * Create query for student.
     *
     * @return Report query by criteria
     */
    private ReportQueryByCriteria getStudentQuery() {
        X2Criteria studentCriteria = new X2Criteria();

        m_multiYearHelper.adjustCriteria(studentCriteria, Strategy.IN, SisStudent.COL_GRADE_LEVEL, m_codes12Grade);

        m_multiYearHelper.adjustCriteria(studentCriteria, Strategy.IN,
                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, m_schoolOids);

        X2Criteria conditionCriteria = new X2Criteria();
        m_multiYearHelper.adjustCriteria(conditionCriteria, Strategy.IN, m_documentTypeJavaName, m_tableRows.keySet());
        m_multiYearHelper.adjustCriteria(conditionCriteria, Strategy.LESS_OR_EQUAL_THAN, m_documentDateJavaName,
                m_schoolYear.getEndDate());
        m_multiYearHelper.adjustCriteria(conditionCriteria, Strategy.GREATER_OR_EQUAL_THAN, m_documentDateJavaName,
                m_schoolYear.getStartDate());

        studentCriteria.addAndCriteria(conditionCriteria);

        String graduationField = null;
        if (m_isSpecialReport != null && m_isSpecialReport.booleanValue()) {
            graduationField = m_documentDateJavaName;
        } else {
            graduationField = m_documentPeriodJavaName;
        }
        m_multiYearHelper.adjustCriteria(studentCriteria, Strategy.NOT_EMPTY, graduationField,
                getUser().getPersistenceKey());

        if (m_isSpecialReport.booleanValue()) {
            SubQuery studentOidSubQuery = getStudentOidsSpecialEducationSubQuery();
            if (studentOidSubQuery == null) {
                return null;
            }
            m_spedStdOids = getBroker().getSubQueryCollectionByQuery(studentOidSubQuery);
            if (m_spedStdOids.isEmpty()) {
                return null;
            }
        }

        ReportQueryByCriteria studentQuery = new ReportQueryByCriteria(SisStudent.class, studentCriteria);
        return studentQuery;
    }

    /**
     * Increment cell value.
     *
     *
     * @param rowKey String
     * @param columnKey String
     * @return true, if successful
     */
    private boolean incTableValue(String rowKey, String columnKey) {
        Map<String, Integer> row = m_table.get(rowKey);
        if (row == null) {
            return false;
        }
        Integer value = row.get(columnKey);
        if (value == null) {
            return false;
        }
        row.put(columnKey, Integer.valueOf(value.intValue() + 1));
        return true;
    }

    /**
     * Loads two maps:
     * 1)rows of table with description where row number is state code of Completion Document Type.
     * 2)state codes of Completion Document Types
     *
     * Only include records for which a state value exists on the code.
     * Sort by sequence number and choose description from code with lowest sequence number.
     *
     * @param docTypeField DataDictionaryField
     */
    private void handleTypesRefTable(DataDictionaryField docTypeField) {
        m_typeCodesMap = new HashMap<String, String>();
        m_tableRows = new TreeMap<String, String>();

        ReferenceTable refTable = docTypeField.getReferenceTable();

        if (refTable != null) {
            List<ReferenceCode> codes = new ArrayList<ReferenceCode>(refTable.getReferenceCodes());
            Collections.sort(codes, new Comparator<ReferenceCode>() {
                @Override
                public int compare(ReferenceCode o1, ReferenceCode o2) {
                    return o1.getSequenceNumber() > o2.getSequenceNumber() ? 1
                            : o1.getSequenceNumber() == o2.getSequenceNumber() ? 0 : -1;
                }
            });
            for (ReferenceCode code : codes) {
                if (StringUtils.isEmpty(code.getStateCode())) {
                    continue;
                }
                if (m_typeCodesMap.get(code.getCode()) == null) {
                    m_typeCodesMap.put(code.getCode(), code.getStateCode());
                }
                if (m_tableRows.get(code.getStateCode()) == null) {
                    m_tableRows.put(code.getStateCode(), code.getDescription());
                }
            }
        }
    }

    /**
     * Fills table by zero values.
     */
    private void resetTableCounts() {
        m_table = new LinkedHashMap();
        for (String rowKey : m_tableRows.keySet()) {
            Map<String, Integer> row = new HashMap();
            for (String columnKey : m_tableColumns) {
                row.put(columnKey, Integer.valueOf(0));
            }
            m_table.put(rowKey, row);
        }
    }

    /**
     * Translates an alias into a Java bean path name.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaName(String alias) {
        String javaName = null;

        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        }

        return javaName;
    }
}
