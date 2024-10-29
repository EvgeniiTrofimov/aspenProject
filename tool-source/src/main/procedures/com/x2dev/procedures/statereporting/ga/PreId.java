/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Georgia state report for Pre-ID labels export.
 * This class implements the data export for Pre-ID labels export.
 *
 * @author X2 Development Corporation
 */
public class PreId extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Pre-ID labels export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class PreIdEntity extends StateReportEntity {
        private PreId m_data;

        private SisStaff m_primaryStaff = null;
        private List<StudentSchedule> m_schedules = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PreIdEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the Race collection for the student in this entity.
         *
         * @return Collection
         */
        public Collection<Race> getRaces() {
            SisStudent student = (SisStudent) getBean();
            Collection<Race> races = null;
            SisPerson person = student.getPerson();
            if (person != null) {
                races = person.getRaces();
            }

            return races;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [ID: " + student.getLocalId() +
                    " [GTID: " + student.getStateId() +
                    "]";

            return name;
        }

        /**
         * Returns the student schedule object for the current row.
         *
         * @return StudentSchedule.
         */
        public StudentSchedule getRowSchedule() {
            StudentSchedule sched = null;
            if (m_schedules != null && getCurrentRow() < m_schedules.size()) {
                sched = m_schedules.get(getCurrentRow());
            }

            return sched;
        }

        public SisStaff getHomeroomStaff() {
            return m_primaryStaff;
        }

        /**
         * Initialize.
         * Increment count of records for trailer record count.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (PreId) data;
            /*
             * Logic for each test type
             * Test Type = EOC returns record for each course designated with EOC Flag = Y / PRL 022
             * = 12,
             * Teacher Last Name and First Name are pulled from the primary teacher for course
             * flagged as EOC
             * include Course Number and Section
             * Test Type = EOG = return a single record for each active student in grades 3-8,
             * from student schedule return Teacher Last Name and First Name from the homeroom
             * course begins with 00.
             * DO NOT Include Course Number and Section (NOTE: All students should have a homeroom
             * course in both Marietta and Laurens)
             * Test Type = GHSWT = return a record for each student in grade 11 who is taking an
             * English Language Course begins with 23.
             * look at Home Room begins with 00 and return Teacher Last Name and First Name from the
             * homeroom - DO NOT Include Course Number or Section
             */
            int rowCount = 1;
            if (TEST_TYPE_EOC.equals(m_data.m_test_Id) || TEST_TYPE_GHSWT.equals(m_data.m_test_Id)
                    || TEST_TYPE_EOG.equals(m_data.m_test_Id) || TEST_TYPE_GKIDS.equals(m_data.m_test_Id)) {
                if (m_data.m_scheduleMap.containsKey(bean.getOid())) {
                    Collection loadedSchedules = m_data.m_scheduleMap.get(bean.getOid());
                    m_schedules = new ArrayList<StudentSchedule>();
                    if (loadedSchedules != null) {
                        m_schedules.addAll(loadedSchedules);
                    }
                    if (TEST_TYPE_EOC.equals(m_data.m_test_Id)) {
                        rowCount = m_schedules.size();

                    } else if (TEST_TYPE_GHSWT.equals(m_data.m_test_Id)
                            || (TEST_TYPE_EOG.equals(m_data.m_test_Id))
                            || TEST_TYPE_GKIDS.equals(m_data.m_test_Id)) {
                        boolean isEnglishLearner = false;
                        for (StudentSchedule studentSchedule : m_schedules) {
                            if (null != studentSchedule) {
                                MasterSchedule section = studentSchedule.getSection();
                                if (null != section) {
                                    String course = section.getCourseView();
                                    if (!StringUtils.isEmpty(course)) {
                                        String firstTwoDigitsCourseNo = course.split("\\.")[0];
                                        if (firstTwoDigitsCourseNo.equals(HMRM_CRS_NO)) {
                                            m_primaryStaff = section.getPrimaryStaff();
                                        }
                                        if (firstTwoDigitsCourseNo.equals(ENG_LANG_CRS_NO)) {
                                            isEnglishLearner = true;
                                        }

                                    }
                                }
                            }
                        }
                        if (TEST_TYPE_GHSWT.equals(m_data.m_test_Id) && !isEnglishLearner) {
                            rowCount = 0;
                        }
                    }
                }
            }

            setRowCount(rowCount);
        }
    }

    /*
     * Pre-ID labels field alias constants.
     */
    protected static final String PREID_01_SYSTEM_CODE = "PRL001";
    protected static final String PREID_02_SCHOOL_CODE = "PRL002";
    protected static final String PREID_03_GRADE_LEVEL = "PRL003";
    protected static final String PREID_04_FIRST_NAME = "PRL004";
    protected static final String PREID_05_MIDDLE_NAME = "PRL005";
    protected static final String PREID_06_LAST_NAME = "PRL006";
    protected static final String PREID_07_STUDENT_ID = "PRL007";
    protected static final String PREID_075_EOCELA = "PRL075";
    protected static final String PREID_08_CYCLE = "PRL008";
    protected static final String PREID_09_GENDER = "PRL009";
    protected static final String PREID_10_BIRTHDATE = "PRL010";
    protected static final String PREID_11_FILLER_1 = "PRL011";
    protected static final String PREID_12_EXCEPTIONALITY = "PRL012";
    protected static final String PREID_13_ELL = "PRL013";
    protected static final String PREID_14_504 = "PRL014";
    protected static final String PREID_15_TITLE_I_READING = "PRL015";
    protected static final String PREID_16_TITLE_I_MATH = "PRL016";
    protected static final String PREID_17_MIGRANT = "PRL017";
    protected static final String PREID_18_EARLY_INT = "PRL018";
    protected static final String PREID_19_FILLER_2 = "PRL019";
    protected static final String PREID_20_TEACHER_LAST_NAME = "PRL020";
    protected static final String PREID_21_TEACHER_FIRST_NAME = "PRL021";
    protected static final String PREID_22_TEST_ID = "PRL022";
    protected static final String PREID_23_GTID = "PRL023";
    protected static final String PREID_24_COURSE_ID = "PRL024";
    protected static final String PREID_25_COURSE_SECTION = "PRL025";
    protected static final String PREID_26_ONLINE = "PRL026";
    protected static final String PREID_27_HISPANIC_IND = "PRL027";
    protected static final String PREID_28_INDIAN_IND = "PRL028";
    protected static final String PREID_29_ASIAN_IND = "PRL029";
    protected static final String PREID_30_BLACK_IND = "PRL030";
    protected static final String PREID_31_PACIFIC_IND = "PRL031";
    protected static final String PREID_32_WHITE_IND = "PRL032";

    /*
     * Test types codes.
     */
    protected static final String TEST_TYPE_GHSWT = "02";
    protected static final String TEST_TYPE_GKIDS = "11";
    protected static final String TEST_TYPE_EOC = "12";
    protected static final String TEST_TYPE_EOG = "13";

    /**
     * First two digits of the course number
     */
    protected static final String HMRM_CRS_NO = "00";
    protected static final String ENG_LANG_CRS_NO = "23";

    /*
     * Course information calc parameter for course info retriever.
     */
    protected static final String COURSE_INFO_COURSE = "COURSE";
    protected static final String COURSE_INFO_SECTION = "SECTION";
    protected static final String COURSE_INFO_FIRST_NAME = "FIRST";
    protected static final String COURSE_INFO_LAST_NAME = "LAST";
    protected static final String COURSE_INFO_ONLINE = "ONLINE";

    /*
     * Report values and aliases.
     */
    private static final String DOE_COURSE_EOC_FIELD = "EOC Course";
    private static final String DOE_COURSE_CODE_EOCT_FIELD = "EOCT Course Code";
    private static final String DOE_CRCT_M = "DOE CRCT-M";
    private static final String DOE_ONLINE_EOCT_FIELD = "EOCT Online";
    private static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    private static final String DOE_PRL012 = "DOE Primary Exceptionality";
    private static final String DOE_PRL013 = "DOE ELL";
    private static final String DOE_STATUS_FIELD = "DOE Status";
    private static final String DOE_YEAR_MONITORING_BEGAN = "DOE YEAR EL MONITORING BEGAN";

    /*
     * Parameters
     */
    private static final String PARAM_CYCLE = "cycle";
    private static final String PARAM_SORT = "sort";
    private static final String PARAM_TERM = "term";
    private static final String PARAM_EOG_ONLINE = "eogOnline";
    private static final String PARAM_TEST_ID = "testId";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";

    /*
     * Other internal constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String COMMA = ",";
    protected static final String YES = "Y";
    protected static final String NO = "N";


    /*
     * Instance variables.
     */
    protected String m_courseEOCField;
    protected String m_courseCodeEOCTField;
    protected String m_cycle;
    protected String m_doeStatusField;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected String m_onlineEOCTField;
    protected String m_overrideSchoolCodeField;
    protected String m_prl012;
    protected String m_prl013Field;
    protected String m_yearMonitorField;
    protected String m_CRCT_M;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected String m_termOid;
    protected String m_eogOnline;
    protected String m_test_Id;

    /**
     * A map of reference codes for race codes, for use in the race code retriever.
     */
    protected Map<String, ReferenceCode> m_raceCodes;

    /**
     * when PRL022 = 13, get values by Alias and return 1 or 0
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    public class RetrieveFor13TestId implements FieldRetriever {
        private static final String CALC_ID = "PREID-TEST-13";
        private static final String FIELD_NAME_PRL022 = "PRL022";
        private static final String FIELD_VALUE_PRL022 = "13";
        private final List VALID_GRADES = Arrays.asList("06", "07", "08");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Boolean value = null;
            SisStudent std = (SisStudent) entity.getBean();
            String gradeLevel = std.getGradeLevel();
            if (FIELD_VALUE_PRL022.equals(entity.getFieldValue(FIELD_NAME_PRL022))
                    && VALID_GRADES.contains(gradeLevel)) {
                SisStudent student = (SisStudent) entity.getBean();
                if (BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(field.getBeanPath()))) {
                    value = Boolean.valueOf(true);
                } else {
                    value = Boolean.valueOf(false);
                }
            }
            return value;
        }
    }

    /**
     * Retriver for PRL012 defined as follows:
     * If Std.SpedStatus maps to a state code of "R", data element PRL012 should be blank.
     */
    public class RetrievePRL012 implements FieldRetriever {
        private static final String CALC_ID = "PRL012";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String stdSpedStatus = student.getSpedStatusCode();
            String stdSpedState = null;
            if (!StringUtils.isEmpty(stdSpedStatus)) {
                stdSpedState = lookupStateValue(SisStudent.class, Student.COL_SPED_STATUS_CODE, stdSpedStatus);
            }

            if (StringUtils.isEmpty(stdSpedState) || !"R".equals(stdSpedState)) {
                String prl012Value = (String) student.getFieldValueByBeanPath(m_prl012);

                if (!StringUtils.isEmpty(prl012Value)) {
                    value = lookupStateValue(SisStudent.class, m_prl012, prl012Value);
                }
            }
            return value;
        }
    }

    /**
     * Retriver for PRL013 defined as follows:
     * If DOE ELL = M and DOE YEAR EL MONITORING BEGAN = current school year, return 1
     * If DOE ELL = M and DOE YEAR EL MONITORING BEGAN = current school year minus 1, return 2
     * If DOE ELL = M and DOE YEAR EL MONITORING BEGAN = current school year minus 2, return 3
     * If DOE ELL = M and DOE YEAR EL MONITORING BEGAN = current school year minus 3, return 4
     * If DOE ELL is not = M, return value found.
     * If DOE ELL is blank/null, return "N".
     */
    public class RetrievePRL013 implements FieldRetriever {
        private static final String CALC_ID = "PRL013";
        private static final String STATE_CODE_M = "M";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            SisStudent student = (SisStudent) entity.getBean();
            String prl013Value = (String) student.getFieldValueByBeanPath(m_prl013Field);
            String monitorValueBegan =
                    (lookupStateValue(SisStudent.class, m_yearMonitorField,
                            (String) student.getFieldValueByAlias(DOE_YEAR_MONITORING_BEGAN)) != null
                                    ? (lookupStateValue(
                                            SisStudent.class, m_yearMonitorField,
                                            (String) student.getFieldValueByAlias(DOE_YEAR_MONITORING_BEGAN)))
                                    : "");
            String doeEllValue = lookupStateValue(SisStudent.class, m_prl013Field, prl013Value);
            int currentContextYear = data.getCurrentContext().getSchoolYear();

            if (STATE_CODE_M.equals(doeEllValue)) {
                if ((monitorValueBegan.equals(currentContextYear + EMPTY_STRING))) {
                    value = "1";
                } else if ((monitorValueBegan.equals(currentContextYear - 1 + EMPTY_STRING))) {
                    value = "2";
                } else if ((monitorValueBegan.equals(currentContextYear - 2 + EMPTY_STRING))) {
                    value = "3";
                } else if ((monitorValueBegan.equals(currentContextYear - 3 + EMPTY_STRING))) {
                    value = "4";
                }
            } else {
                value = doeEllValue;
            }
            return value;
        }
    }

    /**
     * Retriver for PRL019 defined as follows:
     * When test type = Georgia milestones - End of Course (EOC) (12) PRL019 shall default to '1'.
     * When test type is NOT = Georgia milestones - End of Course (EOC) (12), PRL019 shall be blank.
     */
    protected class RetrievePRL019 implements FieldRetriever {
        private static final String TEST_TYPE_EOC_VALUE = "1";

        private String m_value = null;

        /**
         * Instantiates a new retrieve PRL 019.
         */
        public RetrievePRL019() {
            if (TEST_TYPE_EOC.equals(getParameter(PARAM_TEST_ID))) {
                m_value = TEST_TYPE_EOC_VALUE;
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return m_value;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In GA, this is:
     * "W" - White
     * "B" - Black
     * "S" - Asian
     * "I" - Indian/Native/Alaskan
     * "P" - Pacific
     *
     * Ex: "SNS" searches for the Asian code, returns "S" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);

            String raceCode = falseChar;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(requestCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = trueChar;
                        break;
                    }
                }
            }

            return raceCode;
        }
    }

    /**
     * Retrieve the school from the entity. This will be the
     * school on report date or last withdrawal.
     * Optionally, a student can have an override school code assigned.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String overrideSchoolCodeField = ((PreId) data).m_overrideSchoolCodeField;
            value = (String) getProperty(student, overrideSchoolCodeField);
            if (StringUtils.isEmpty(value)) {
                String schoolCodeField = field.getBeanPath();
                value = (String) getProperty(student, schoolCodeField);
            }
            return value;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());

            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * Retrieve course related values from the current course in the student course list.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourseInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            StudentSchedule studentSchedule = ((PreIdEntity) entity).getRowSchedule();
            SisStaff staffHomeroom = ((PreIdEntity) entity).getHomeroomStaff();
            if (studentSchedule != null) {
                MasterSchedule section = studentSchedule.getSection();
                if (section != null) {
                    if (TEST_TYPE_EOC.equals(m_test_Id) || TEST_TYPE_EOG.equals(m_test_Id)) {
                        if (!TEST_TYPE_EOG.equals(m_test_Id)) {
                            if (COURSE_INFO_COURSE.equals(param)) {
                                SchoolCourse schoolCourse = section.getSchoolCourse();
                                if (schoolCourse != null) {
                                    Course course = schoolCourse.getCourse();
                                    if (course != null) {
                                        String code = (String) course.getFieldValueByBeanPath(m_courseCodeEOCTField);
                                        value = code;
                                    }
                                }
                            } else if (COURSE_INFO_SECTION.equals(param)) {
                                value = section.getSectionNumber();
                            }
                        }
                        if (COURSE_INFO_ONLINE.equals(param)) {
                            if (TEST_TYPE_EOG.equals(m_test_Id)) {
                                String studentGradeLevel = studentSchedule.getStudent().getGradeLevel();
                                List<String> eogOnlineGradeLevels = new ArrayList<String>();
                                if (!StringUtils.isEmpty(m_eogOnline)) {
                                    String[] eogGrades = m_eogOnline.split(COMMA);
                                    for (String eogGrade : eogGrades) {
                                        ReferenceCode refCode =
                                                (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, eogGrade);
                                        eogOnlineGradeLevels.add(refCode.getCode());
                                    }
                                }
                                if (eogOnlineGradeLevels.contains(studentGradeLevel)) {
                                    value = YES;
                                } else {
                                    value = NO;
                                }
                            } else {
                                String code = (String) section.getFieldValueByBeanPath(m_onlineEOCTField);
                                value = Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(code));
                            }
                        }

                        else if (COURSE_INFO_FIRST_NAME.equals(param)) {
                            SisStaff staff = section.getPrimaryStaff();
                            if (TEST_TYPE_EOC.equals(m_test_Id) || TEST_TYPE_EOG.equals(m_test_Id)) {
                                if (staff != null) {
                                    value = staff.getPerson().getFirstName();
                                }
                            }
                        } else if (COURSE_INFO_LAST_NAME.equals(param)) {
                            if (TEST_TYPE_EOC.equals(m_test_Id) || TEST_TYPE_EOG.equals(m_test_Id)) {
                                SisStaff staff = section.getPrimaryStaff();
                                if (staff != null) {
                                    value = staff.getPerson().getLastName();
                                }
                            }
                        }

                    }

                    /**
                     * From student schedule return Teacher Last Name and First Name from the
                     * homeroom course begins with 00".
                     */
                    else if (TEST_TYPE_GHSWT.equals(m_test_Id) || TEST_TYPE_EOG.equals(m_test_Id)
                            || TEST_TYPE_GKIDS.equals(m_test_Id)) {
                        if (staffHomeroom != null) {
                            if (COURSE_INFO_FIRST_NAME.equals(param)) {
                                value = staffHomeroom.getPerson().getFirstName();
                            } else if (COURSE_INFO_LAST_NAME.equals(param)) {
                                value = staffHomeroom.getPerson().getLastName();
                            }
                        }
                    }

                }
            }

            return value;
        }
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        /*
         * Job parameters.
         */
        m_termOid = (String) getParameter(PARAM_TERM);
        m_eogOnline = (String) getParameter(PARAM_EOG_ONLINE);
        m_test_Id = (String) getParameter(PARAM_TEST_ID);
        m_cycle = (String) getParameter(PARAM_CYCLE);

        initializeFields();

        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user input.
             */
            Criteria studentCriteria = getStudentCriteria(m_test_Id);
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            int sort = ((Integer) getParameter(PARAM_SORT)).intValue();

            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 3: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(PreIdEntity.class);

            // Get race code reference codes for use in the race retriever.
            Criteria raceCriteria = new Criteria();
            raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

            /*
             * Load the race codes for all students included in the export.
             */
            SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
            raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PREID-CLEAN-NAME", new RetrieveStripNameChar());
            calcs.put("PREID-PRL019", new RetrievePRL019());
            calcs.put("PREID-RACE", new RetrieveRace());
            calcs.put("PREID-COURSE", new RetrieveCourseInfo());
            calcs.put("PREID-SCHOOL", new RetrieveSchool());
            calcs.put(RetrieveFor13TestId.CALC_ID, new RetrieveFor13TestId());
            calcs.put(RetrievePRL012.CALC_ID, new RetrievePRL012());
            calcs.put(RetrievePRL013.CALC_ID, new RetrievePRL013());
            super.addCalcs(calcs);
        }
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @param testId String
     * @return Criteria
     */
    private Criteria getStudentCriteria(String testId) {
        X2Criteria studentCriteria = new X2Criteria();

        /*
         * Active students
         */
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            studentCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }


        /*
         * Check student selection criteria user input.
         */
        selectStudentCriteria(studentCriteria);

        /*
         * Testing criteria by test type.
         * Generally this is by grade level.
         */
        ArrayList<String> gradeStateCodes = new ArrayList<String>();
        if (TEST_TYPE_GKIDS.equals(testId)) {
            gradeStateCodes.add("KK");
        } else if (TEST_TYPE_GHSWT.equals(testId)) {
            gradeStateCodes.add("11");
        } else if (TEST_TYPE_EOC.equals(testId)) {
            // Add grade levels to grade level criteria.
            gradeStateCodes.add("07");
            gradeStateCodes.add("08");
            gradeStateCodes.add("09");
            gradeStateCodes.add("10");
            gradeStateCodes.add("11");
            gradeStateCodes.add("12");
        } else if (TEST_TYPE_EOG.equals(testId)) {
            // grades 3-8.
            gradeStateCodes.add("03");
            gradeStateCodes.add("04");
            gradeStateCodes.add("05");
            gradeStateCodes.add("06");
            gradeStateCodes.add("07");
            gradeStateCodes.add("08");
        }

        gradeStateCodes = getGradeCodesForGrades(gradeStateCodes);
        if (gradeStateCodes != null) {
            studentCriteria.addIn(SisStudent.COL_GRADE_LEVEL, gradeStateCodes);
        }

        if (TEST_TYPE_GKIDS.equals(testId) || TEST_TYPE_GHSWT.equals(testId) || TEST_TYPE_EOC.equals(testId)
                || TEST_TYPE_EOG.equals(testId)) {
            getCourseCriteria(studentCriteria, testId);
        }

        return studentCriteria;
    }

    /**
     * Gets the course criteria.
     *
     * @param studentCriteria X2Criteria
     * @param testId String
     * @return void
     */
    private void getCourseCriteria(X2Criteria studentCriteria, String testId) {
        List<String> refCodeList = new ArrayList();
        // Add criteria for students taking courses that are testable.
        X2Criteria courseCriteria = new X2Criteria();

        courseCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);
        if (TEST_TYPE_EOC.equals(testId)) {
            courseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_courseEOCField, BooleanAsStringConverter.TRUE);

            if (!StringUtils.isEmpty(m_termOid)) {
                String[] terms = m_termOid.split(",");
                for (String term : terms) {
                    ReferenceCode refCode = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, term);
                    refCodeList.add(refCode.getCode());
                }
                if (refCodeList != null) {
                    courseCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                            ScheduleTerm.COL_CODE, refCodeList);
                }
            }
        }
        if (TEST_TYPE_EOG.equals(testId) || TEST_TYPE_GKIDS.equals(testId)) {
            if (!StringUtils.isEmpty(m_termOid)) {
                String[] terms = m_termOid.split(",");
                for (String term : terms) {
                    ReferenceCode refCode = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, term);
                    refCodeList.add(refCode.getCode());
                }
                if (refCodeList != null) {
                    courseCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.REL_SCHEDULE_TERM + PATH_DELIMITER +
                            ScheduleTerm.COL_CODE, refCodeList);
                }
            }
            courseCriteria.addBeginsWith(StudentSchedule.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_COURSE_VIEW,
                    HMRM_CRS_NO);
        }

        if (!(TEST_TYPE_EOG.equals(testId) || TEST_TYPE_GKIDS.equals(testId))) {
            SubQuery courseSubquery =
                    new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, courseCriteria);
            studentCriteria.addIn(X2BaseBean.COL_OID, courseSubquery);
        } else {
            courseCriteria.addIn(StudentSchedule.COL_STUDENT_OID,
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria));
        }

        QueryByCriteria courseQuery = new QueryByCriteria(StudentSchedule.class, courseCriteria);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(courseQuery, StudentSchedule.COL_STUDENT_OID, 100);
    }


    /**
     * Implementing the select student filter based on parameters.
     *
     * @param studentCriteria X2Criteria
     */
    private void selectStudentCriteria(X2Criteria studentCriteria) {
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                studentCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                studentCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }
    }

    /**
     * This method looks up the reference table for student grade level.
     * Then returns all reference code values that match the state code values passed.
     *
     * @param gradeStateCodes array of state code values.
     *
     * @return List of grade level reference code values.
     *         A null indicates the reference table was not found.
     */
    private ArrayList<String> getGradeCodesForGrades(List<String> gradeStateCodes) {
        ArrayList<String> refCodes = null;

        // Get maps of grade level codes by state code.
        String gradeLevelRefTableOid = null;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField gradeLevelField =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);

        if (gradeLevelField != null) {
            gradeLevelRefTableOid = gradeLevelField.getReferenceTableOid();
        } else {
            addSetupError("Grade level reference code lookup", "No grade level field found.");
        }

        if (gradeLevelRefTableOid != null) {
            Criteria gradeCriteria = new Criteria();

            gradeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, gradeLevelRefTableOid);
            gradeCriteria.addIn(ReferenceCode.COL_STATE_CODE, gradeStateCodes);
            ReportQueryByCriteria gradeQuery = new ReportQueryByCriteria(ReferenceCode.class,
                    new String[] {ReferenceCode.COL_CODE}, gradeCriteria);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(gradeQuery);

            try {
                refCodes = new ArrayList<String>();
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    refCodes.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }

            if (refCodes.size() == 0) {
                refCodes = null;
                addSetupError("Grade level reference code lookup",
                        "No reference codes for grade levels: " + gradeStateCodes.toString());
            }
        } else {
            addSetupError("Grade level reference code lookup", "No grade level reference table.");
        }

        return refCodes;
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }


    /**
     * Initialize instance variables, aliases, lookups.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD, true);
        m_courseEOCField = translateAliasToJavaName(DOE_COURSE_EOC_FIELD, true);
        m_courseCodeEOCTField = translateAliasToJavaName(DOE_COURSE_CODE_EOCT_FIELD, true);
        m_onlineEOCTField = translateAliasToJavaName(DOE_ONLINE_EOCT_FIELD, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_CRCT_M = translateAliasToJavaName(DOE_CRCT_M, true);
        m_prl012 = translateAliasToJavaName(DOE_PRL012, true);
        m_prl013Field = translateAliasToJavaName(DOE_PRL013, true);
        m_yearMonitorField = translateAliasToJavaName(DOE_YEAR_MONITORING_BEGAN, true);
    }

}
