/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2021 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DataGrid;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 *
 * Exports the EasyIEP Student data.
 *
 * @author X2 Development Corporation
 */
public class EasyIEPEdFiStudentExport extends ExportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String DISABLE_BEAN_PATH = "-9999";
    private static final String DOE_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String EMPTY_STRING = "";
    private static final int FIELD_COUNT = 14;
    private static final String MM_DD_YYYY = "MM/dd/yyyy";
    private static final String ALIAS_STUDENT_EIS_STATE_ID = "DOE EIS STATE ID";
    private static final String ALIAS_STUDENT_PIN_FIELD = "all-std-StudentPIN";
    private static final String RACE_CODE_HISPANIC = "H";
    private static final String REGEX_DIGITS = "\\D";
    private static final String REGEX_SSN = "[0-9,-]+";
    private static final long serialVersionUID = 1L;
    private static final String STRIP_CHARACTER_ZERO = "0";

    // Grid fields
    private static final String FIELD_DATE_OF_BIRTH = "DateOfBirth";
    private static final String FIELD_DISTRICT_CODE = "DistrictCode";
    private static final String FIELD_EIS_ENROLLMENT_YEAR = "Custom(EISEnrollmentYear)";
    private static final String FIELD_FIRST_NAME = "FirstName";
    private static final String FIELD_GENDER = "Gender";
    private static final String FIELD_GRADE = "Grade";
    private static final String FIELD_LAST_NAME = "LastName";
    private static final String FIELD_MIDDLE_NAME = "MiddleName";
    private static final String FIELD_RACE = "Race";
    private static final String FIELD_SCHOOL_CODE = "SchoolCode";
    private static final String FIELD_SSN = "SSNumber";
    private static final String FIELD_STATE_CODE = "StateCode";
    private static final String FIELD_STUDENT_CODE = "StudentCode";
    private static final String FIELD_EXTERNAL_STUDENT_ID = "ExternalStudentID";

    private List<String> m_columns;
    private Map<String, ReferenceCode> m_gradeLevels = new HashMap<>();
    private Map<String, String> m_hispanics;
    private Map<String, String> m_personRaces;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();
        Criteria criteria = buildCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                List<String> gridData = new ArrayList<String>(14);

                String ssn = student.getPerson().getPersonId();

                // Set SSN field equal to the Studet PIN field if blank
                if (StringUtils.isEmpty(ssn)) {
                    ssn = getStudentBeanValueByAlias(student, ALIAS_STUDENT_PIN_FIELD);
                }

                if (!StringUtils.isEmpty(ssn) && Pattern.matches(REGEX_SSN, ssn)) {
                    ssn = ssn.replaceAll(REGEX_DIGITS, EMPTY_STRING);
                }

                // Fill grid data list with student information
                grid.append();
                gridData.add(student.getLocalId());
                gridData.add(student.getPerson().getLastName());
                gridData.add(student.getPerson().getFirstName());
                gridData.add(student.getPerson().getMiddleName());
                gridData.add(student.getPerson().getGenderCode());
                gridData.add(m_personRaces.get(student.getPersonOid()));
                String gradeCode = student.getGradeLevel();
                if (!com.x2dev.utils.StringUtils.isEmpty(gradeCode) && m_gradeLevels.containsKey(gradeCode)) {
                    gradeCode = m_gradeLevels.get(gradeCode).getStateCode();
                }
                String formattedGrade = getFormattedGradeLevel(gradeCode);
                gridData.add(formattedGrade);
                gridData.add(formatDate(student.getPerson().getDob()));
                gridData.add(getOrganization().getId());
                String sklStateId = "";
                if (student.getSchool() != null
                        && student.getSchool().getFieldValueByAlias(DOE_SCHOOL_STATE_ID) != null) {
                    sklStateId = (String) student.getSchool().getFieldValueByAlias(DOE_SCHOOL_STATE_ID);
                }
                gridData.add(sklStateId);
                gridData.add(ssn);
                gridData.add(StringUtils.stripStart(getStudentBeanValueByAlias(student, ALIAS_STUDENT_EIS_STATE_ID),
                        STRIP_CHARACTER_ZERO));
                // e.g. 2014 if 2014-2015
                gridData.add(String.valueOf((getOrganization().getCurrentContext().getSchoolYear() - 1)));
                gridData.add(student.getStateId());

                // Fill grid row with appropriate data
                int gridSize = gridData.size();
                for (int i = 0; i < gridSize; i++) {
                    String data = (gridData.get(i) != null ? gridData.get(i) : "");
                    grid.set(m_columns.get(i), data);
                }
            }
        } finally {
            students.close();
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
        m_hispanics = new HashMap<String, String>();
        m_personRaces = new HashMap<String, String>();

        setIncludeHeaderRow(true);
        setLineSeparator("\r\n");
        setValueDelimiter(Character.valueOf('\t'));
        setUseValueWrappers(false);

        m_columns = new ArrayList<String>(FIELD_COUNT);
        m_columns.add(FIELD_STUDENT_CODE);
        m_columns.add(FIELD_LAST_NAME);
        m_columns.add(FIELD_FIRST_NAME);
        m_columns.add(FIELD_MIDDLE_NAME);
        m_columns.add(FIELD_GENDER);
        m_columns.add(FIELD_RACE);
        m_columns.add(FIELD_GRADE);
        m_columns.add(FIELD_DATE_OF_BIRTH);
        m_columns.add(FIELD_DISTRICT_CODE);
        m_columns.add(FIELD_SCHOOL_CODE);
        m_columns.add(FIELD_SSN);
        m_columns.add(FIELD_STATE_CODE);
        m_columns.add(FIELD_EIS_ENROLLMENT_YEAR);
        m_columns.add(FIELD_EXTERNAL_STUDENT_ID);

        loadHispanicStudents();
        loadStudentRaces();
        loadGradeCodes();
    }

    /**
     * Builds the criteria to include only active students.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        String activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, activeStatus);

        if (isSchoolContext()) {
            criteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        }

        return criteria;
    }

    /**
     * Formats the date.
     *
     * @param date Date
     * @return String
     */
    private String formatDate(Date date) {
        String formattedDate = "";

        if (date != null) {
            SimpleDateFormat outputFormat = new SimpleDateFormat(MM_DD_YYYY);

            formattedDate = outputFormat.format(date);
        }

        return formattedDate;
    }

    /**
     * Returns the formatted grade level where single digit grades are zero padded (e.g. 7 becomes
     * 07).
     *
     * @param gradeLevel String
     * @return String
     */
    private String getFormattedGradeLevel(String gradeLevel) {
        String formattedGrade = "";
        if (!StringUtils.isEmpty(gradeLevel)) {
            if (StringUtils.isNumeric(gradeLevel) && gradeLevel.length() == 1) {
                formattedGrade = StringUtils.leftPad(gradeLevel, 2, '0');
            } else {
                formattedGrade = gradeLevel;
            }
        }
        return formattedGrade;
    }

    /**
     * Returns the value of the field associated with the passed bean path for the Student object.
     *
     * @param student Student
     * @param beanPath String
     * @return String
     */
    private String getStudentBeanValue(Student student, String beanPath) {
        String value = "";

        if (beanPath != null && !beanPath.equals(DISABLE_BEAN_PATH)) {
            value = (String) student.getFieldValueByBeanPath(beanPath);
        }

        return value;
    }

    /**
     * Returns the value of the field associated with the passed alias for the Student object.
     *
     * @param student Student
     * @param alias String
     * @return String
     */
    private String getStudentBeanValueByAlias(Student student, String alias) {
        String value = "";

        if (alias != null) {
            value = (String) student.getFieldValueByAlias(alias);
        }

        return value;
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            m_gradeLevels = dictionaryField.getReferenceTable().getCodeMap();
        }
    }

    /**
     * Loads a list of Hispanics to a map keyed on personOid.
     */
    private void loadHispanicStudents() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Race.REL_PERSON + PATH_DELIMITER + Person.COL_HISPANIC_LATINO_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(Race.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Race race = (Race) iterator.next();

                m_hispanics.put(race.getPersonOid(), RACE_CODE_HISPANIC);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads races to m_personRaces ensuring that the Hispanic code always comes first in the list
     * of race codes.
     */
    private void loadStudentRaces() {
        // Adds m_hispanics to m_personRaces
        m_personRaces.putAll(m_hispanics);

        QueryByCriteria query = new QueryByCriteria(Race.class);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                Race race = (Race) iterator.next();
                String personOid = race.getPersonOid();
                String races = m_personRaces.get(personOid);

                if (races == null) {
                    races = race.getRaceCode();
                } else {
                    races += ", " + race.getRaceCode();
                }

                m_personRaces.put(personOid, races);
            }
        } finally {
            iterator.close();
        }
    }
}
