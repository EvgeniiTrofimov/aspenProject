/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for GA Fitness Gram Export.
 */
public class GAFitnessGramProcedure extends StateReportData {

    /**
     * The Class GAFitnessGramEntity.
     */
    public static class GAFitnessGramEntity extends StateReportEntity {
        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public GAFitnessGramEntity() {
            // public no argument constructor for dynamic instantiation.
        }
    }

    /**
     * Retriever to get information about class bean.
     */
    protected class RetrieveClassInfo implements FieldRetriever {
        public static final String ALT = "ALT";
        public static final String CLASS_DESC = "CLASS_DESC";
        public static final String CLASS_ID = "CLASS_ID";
        public static final String CLASS_NAME = "CLASS_NAME";
        public static final String END_DATE = "END_DATE";
        public static final String SCHOOL_ID = "SCHOOL_ID";
        public static final String START_DATE = "START_DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            Object value = EMPTY_STRING;
            StudentSchedule stdSchedule = (StudentSchedule) entity.getBean();
            MasterSchedule section = stdSchedule.getSection();
            SchoolCourse course = section.getSchoolCourse();
            // SisStudent student = stdSchedule.getStudent();
            SisStaff staff = stdSchedule.getSection().getPrimaryStaff();

            if (SCHOOL_ID.equals(parameter)) {
                String sklId = (String) stdSchedule.getStudent().getFieldValueByBeanPath(m_stdFieldOverrideSklCode);
                if (StringUtils.isBlank(sklId)) {
                    sklId = course.getSchool().getSchoolId();
                }
                StringBuffer schoolId = new StringBuffer();
                schoolId.append(course.getSchool().getOrganization1().getId());
                schoolId.append('-');
                schoolId.append(sklId);
                value = schoolId.toString();

            } else if (CLASS_NAME.equals(parameter)) {
                // CourseName+SectionNumber+Semester ID+YY
                StringBuffer className = new StringBuffer();
                className.append(course.getShortDescription());
                className.append(EMPTY_STRING);
                className.append(section.getSectionNumber());
                className.append(EMPTY_STRING);
                className.append(section.getTermView());
                String year = String.valueOf(section.getSchedule().getDistrictContext().getSchoolYear());
                className.append(year.substring(2));
                value = className.toString();
            } else if (CLASS_ID.equals(parameter)) {
                // DistrictID+SchoolID+CourseNumber+Section#+Term+TeacherLastName+YY
                String teacherLastName = EMPTY_STRING;
                if (staff != null && staff.getPerson() != null) {
                    teacherLastName = staff.getPerson().getLastName();
                }
                StringBuffer classId = new StringBuffer();
                classId.append(course.getSchool().getOrganization1().getId());
                classId.append(course.getSchool().getSchoolId());
                classId.append(course.getNumber());
                classId.append(section.getSectionNumber());
                classId.append(section.getTermView());
                classId.append(teacherLastName);
                String year = String.valueOf(section.getSchedule().getDistrictContext().getSchoolYear());
                classId.append(year.substring(2));

                // DistrictID+SchoolID+CourseNumber+Section#+YY+ALT
                /*
                 * if
                 * (BooleanAsStringConverter.TRUE.equals(student.getFieldValueByAlias(ALIAS_ALT_ED))
                 * )
                 * {
                 * classId.append(ALT);
                 * }
                 */
                value = classId.toString();
            } else if (CLASS_DESC.equals(parameter)) {
                value = course.getDescription();
            } else if (START_DATE.equals(parameter)) {
                PlainDate startDate = course.getCourse().getDistrictContext().getStartDate();
                if (section.getScheduleTerm() != null) {
                    PlainDate termStartDate = null;
                    for (ScheduleTermDate termDate : section.getScheduleTerm().getScheduleTermDates()) {
                        if ((termStartDate == null) && (termDate.getStartDate() != null)) {
                            termStartDate = termDate.getStartDate();
                        } else if ((termDate.getStartDate() != null) && termDate.getStartDate().before(termStartDate)) {
                            termStartDate = termDate.getStartDate();
                        }

                    }
                    if (termStartDate != null) {
                        startDate = termStartDate;
                    }
                }
                value = startDate;
            } else if (END_DATE.equals(parameter)) {
                PlainDate endDate = course.getCourse().getDistrictContext().getEndDate();
                if (section.getScheduleTerm() != null) {
                    PlainDate termEndDate = null;
                    for (ScheduleTermDate termDate : section.getScheduleTerm().getScheduleTermDates()) {
                        if ((termEndDate == null) && (termDate.getEndDate() != null)) {
                            termEndDate = termDate.getEndDate();
                        } else if ((termDate.getEndDate() != null) && termDate.getEndDate().before(termEndDate)) {
                            termEndDate = termDate.getEndDate();
                        }

                    }
                    if (termEndDate != null) {
                        endDate = termEndDate;
                    }
                }
                value = endDate;
            }
            return value;
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

        private static final String CALC_ID_RACE = "FSGM-RACE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            StudentSchedule stdSchedule = (StudentSchedule) entity.getBean();
            SisStudent student = stdSchedule.getStudent();
            SisPerson person = student.getPerson();
            if ("HISPANIC".equals(param)) {
                return person.getHispanicLatinoIndicator() ? "1" : 0;
            }
            String trueChar = param.substring(0, 1);
            String falseChar = param.substring(1, 2);
            String requestCode = param.substring(2);
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());
            String raceCode = falseChar;

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
     * Retriever to get information from student bean.
     */
    protected class RetrieveStudentInfo implements FieldRetriever {
        public static final String PARENT_EMAIL1 = "PARENT_EMAIL1";
        public static final String PARENT_EMAIL2 = "PARENT_EMAIL2";
        public static final String PASSWORD = "PASSWORD";
        public static final String STUDENT_MIDDLE_INITIAL = "STUDENT_MIDDLE_INITIAL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            Object value = EMPTY_STRING;
            StudentSchedule stdSchedule = (StudentSchedule) entity.getBean();
            SisStudent student = stdSchedule.getStudent();
            SisPerson person = student.getPerson();

            if (STUDENT_MIDDLE_INITIAL.equals(parameter)) {
                String middle = person.getMiddleName();
                if (!StringUtils.isEmpty(middle)) {
                    value = middle.substring(0, 1);
                }
            } else if (PASSWORD.equals(parameter)) {
                /*
                 * Will be supplied where available by vendor,
                 * if not available First+Middle+LastInitial+Birthdate formula
                 * will be used in order to generate unique logins.
                 */
                StringBuffer password = new StringBuffer();
                password.append(person.getFirstName().substring(0, 1));
                if (!StringUtils.isEmpty(person.getMiddleName())) {
                    password.append(person.getMiddleName().substring(0, 1));
                }
                password.append(person.getLastName().substring(0, 1));
                PlainDate dob = person.getDob();
                if (dob != null) {
                    password.append(m_dateFormatForPass.format(dob));
                }
                password.append("!s");
                value = password.toString();
            } else if (PARENT_EMAIL1.equals(parameter) && (student.getPrimaryContact() != null)
                    && (student.getPrimaryContact().getPerson() != null)) {
                value = student.getPrimaryContact().getPerson().getEmail01();
            } else if (PARENT_EMAIL2.equals(parameter) && (student.getPrimaryContact() != null)
                    && (student.getPrimaryContact().getPerson() != null)) {
                value = student.getPrimaryContact().getPerson().getEmail02();
            }
            return value;
        }

    }

    /**
     * Retriever to get information from staff bean.
     */
    protected class RetrieveTeacherInfo implements FieldRetriever {
        public static final String MIDDLE_INITIAL = "MIDDLE_INITIAL";
        public static final String PASSWORD = "PASSWORD";
        public static final String TEACHER_ID = "TEACHER_ID";
        public static final String TEACHER_ID_PSC = "PSC";
        public static final String TEACHER_ID_SSN = "SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            Object value = EMPTY_STRING;
            StudentSchedule stdSchedule = (StudentSchedule) entity.getBean();
            SisStaff staff = stdSchedule.getSection().getPrimaryStaff();
            SisPerson person = null;
            if (staff != null && staff.getPerson() != null) {
                person = staff.getPerson();
            }

            if (TEACHER_ID.equals(parameter)) {
                // Teacher ID: the districts will have to choose either the
                // PSC # or the SSN for the teacher.
                // SSN - "DOE SSN" on psnPersonId
                String choice = (String) getParameter(PARAM_TEACHER_ID);
                if (TEACHER_ID_PSC.equals(choice)) {
                    String staffPsc = EMPTY_STRING;
                    if (staff != null) {
                        staffPsc = (String) staff.getFieldValueByAlias(ALIAS_DOE_PSC);
                    }
                    if (!StringUtils.isEmpty(staffPsc)) {
                        value = staffPsc;
                    }
                } else if (TEACHER_ID_SSN.equals(choice)) {
                    if (staff != null) {
                        value = staff.getStateId();
                    }
                }

            } else if (MIDDLE_INITIAL.equals(parameter)) {
                String middle = EMPTY_STRING;
                if (person != null) {
                    middle = person.getMiddleName();
                }
                if (!StringUtils.isEmpty(middle)) {
                    value = middle.substring(0, 1);
                }
            } else if (PASSWORD.equals(parameter)) {
                /*
                 * If a password is not available by vendor,
                 * Decision made to create a formula for unique passwords using birthdate
                 * and first and last initials.
                 * MMYYYYAB (2 digit birth month, 4 digit birth year, first and last initial).
                 */
                PlainDate dob = null;
                if (person != null) {
                    dob = person.getDob();
                    StringBuffer password = new StringBuffer();
                    if (dob != null) {
                        password.append(m_dateFormatForPass.format(dob));
                    }
                    password.append(person.getFirstName().substring(0, 1));
                    password.append(person.getLastName().substring(0, 1));
                    password.append("!t");
                    value = password.toString();
                }

            }
            return value;
        }
    }

    /**
     * Validate field does not contain invalid characters (comma or semicolon).
     */
    protected class ValidateInvalidCharacters implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (!StringUtils.isEmpty(value) && (value.contains(STRING_COMMA) || value.contains(STRING_SEMICOLON))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field contains invalid character (, or ;)", value));
            }

            return errors;
        }
    }

    /**
     * Validate field does not contain blank values.
     */
    protected class ValidateBlankValues implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(2);
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Field contains blank value", value));
            }

            return errors;
        }
    }

    protected static final String ALIAS_ALT_ED = "DOE ALT ED";
    protected static final String ALIAS_DEPARTMENT = "DOE Department";
    protected static final String ALIAS_DOE_PSC = "DOE PSC";
    protected static final String ALIAS_DOE_SSN = "DOE SSN";
    protected static final String ALIAS_GTID = "GTID";
    protected static final String ALIAS_MST_EXCL_FITNESS_GRAM = "DOE EXCLUDE FITNESS GRAM";
    protected static final String ALIAS_STD_OVERRIDE_SKL_CODE = "DOE Override School Code";

    private static final char CHAR_COMMA = ',';
    private static final String DATE_PATTERN = "MM/dd/yy";
    private static final String DATE_PATTERN_FOR_PASS = "ddMMyyyy";
    private static final String KEY_RETRIEVER_CLASS = "CLASS";
    private static final String KEY_RETRIEVER_STUDENT = "STUDENT";
    private static final String KEY_RETRIEVER_TEACHER = "TEACHER";
    private static final String KEY_VALIDATOR_INVALID_CHARS = "VAL-INVALID-CHARS";
    private static final String KEY_VALIDATOR_BLANK_VALUE = "VAL-BLANK-VALUE";
    private static final String PARAM_CONTEXT_YEAR = "contextYear";
    private static final String PARAM_INCLUDED_GRADES = "includedGrades";
    private static final String PARAM_TEACHER_ID = "teacherId";
    private static final String PARAM_TERM = "term";
    private static final String REF_CODE_SCHEDULE_TERMS = "rtbSchTermCode";
    private static final String STRING_COMMA = ",";
    private static final String STRING_SEMICOLON = ";";
    private static final String PARAM_DELIMITER = "charDelimiter";

    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat(DATE_PATTERN);
    protected SimpleDateFormat m_dateFormatForPass = new SimpleDateFormat(DATE_PATTERN_FOR_PASS);
    protected StudentHistoryHelper m_helper;
    protected List m_includedGrades = null;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected String m_mstFieldExclGram;
    protected ScheduleManager m_scheduleManager = null;
    protected String m_stdFieldOverrideSklCode;
    protected Map<String, String> m_termCodesMap = new HashMap();

    /**
     * Initialize the data set.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     *
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initializeFields();
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            String grades = (String) getParameter(PARAM_INCLUDED_GRADES);
            String delimiter = (String) getParameter(PARAM_DELIMITER);
            m_includedGrades = StringUtils.convertDelimitedStringToList(grades, delimiter.charAt(0));
            m_scheduleManager = new ScheduleManager(getBroker());

            loadTermCodes();
            // Get all the courses beginning with "36." This is the state department code
            String stateDeptCode = "36.%";
            Criteria studentScheduleCriteria = new X2Criteria();
            studentScheduleCriteria.addLike(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_NUMBER,
                    stateDeptCode);
            studentScheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER + m_mstFieldExclGram,
                    Boolean.TRUE);
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_MASTER_TYPE, SchoolCourse.MASTER_TYPE_CLASS);

            String contextOid = (String) getParameter(PARAM_CONTEXT_YEAR);

            Criteria contextCriteria = new X2Criteria();
            contextCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, contextOid);
            if (getSchool() != null) {
                contextCriteria.addEqualTo(SchoolScheduleContext.COL_SCHOOL_OID, getSchool().getOid());
            }
            SubQuery subQuery = new SubQuery(SchoolScheduleContext.class, SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    contextCriteria);

            studentScheduleCriteria.addIn(StudentSchedule.COL_SCHEDULE_OID,
                    subQuery);

            // include only k-12 grades
            studentScheduleCriteria.addIn(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_GRADE_LEVEL, m_includedGrades);
            // Get selected Term Codes
            String rcdOidList = (String) getParameter(PARAM_TERM);
            HashSet<String> rcdOids = new HashSet(StringUtils.convertDelimitedStringToList(rcdOidList, CHAR_COMMA));
            ArrayList<String> selectedTermCodes = new ArrayList();
            for (String rcdOid : rcdOids) {
                String termCode = m_termCodesMap.get(rcdOid);
                selectedTermCodes.add(termCode);
            }

            Criteria termCriteria = new X2Criteria();
            termCriteria.addIn(ScheduleTerm.COL_SCHEDULE_OID, subQuery);
            if (!selectedTermCodes.isEmpty()) {
                termCriteria.addIn(ScheduleTerm.COL_CODE, selectedTermCodes);
            } else {
                termCriteria.addEqualTo(X2BaseBean.COL_OID, "DOESNOTEXIST");
            }
            SubQuery termQuery = new SubQuery(ScheduleTerm.class, X2BaseBean.COL_OID, termCriteria);
            studentScheduleCriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.COL_SCHEDULE_TERM_OID,
                    termQuery);
            QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);
            studentScheduleQuery.addOrderByAscending(StudentSchedule.COL_STUDENT_OID);
            studentScheduleQuery.addOrderByAscending(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_NUMBER);

            applyInputSort(studentScheduleQuery, StudentSchedule.REL_STUDENT);

            // Set the query to be used for student selection.
            setQuery(studentScheduleQuery);
            setEntityClass(GAFitnessGramEntity.class);

            // Get race code reference codes for use in the race retriever.
            X2Criteria raceCodeCriteria = new X2Criteria();
            raceCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCodeCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

            // Load the race codes for all students included in the export.
            SubQuery studentQuery = new SubQuery(StudentSchedule.class,
                    StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_PERSON_OID,
                    studentScheduleCriteria);
            X2Criteria raceCriteria = new X2Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, studentQuery);
            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);


            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(KEY_RETRIEVER_STUDENT, new RetrieveStudentInfo());
            calcs.put(KEY_RETRIEVER_CLASS, new RetrieveClassInfo());
            calcs.put(KEY_RETRIEVER_TEACHER, new RetrieveTeacherInfo());
            calcs.put(RetrieveRace.CALC_ID_RACE, new RetrieveRace());
            super.addCalcs(calcs);

            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(KEY_VALIDATOR_INVALID_CHARS, new ValidateInvalidCharacters());
            validators.put(KEY_VALIDATOR_BLANK_VALUE, new ValidateBlankValues());
            super.addValidators(validators);
        }
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_mstFieldExclGram = translateAliasToJavaName(ALIAS_MST_EXCL_FITNESS_GRAM, true);
        m_stdFieldOverrideSklCode = translateAliasToJavaName(ALIAS_STD_OVERRIDE_SKL_CODE, true);
    }

    /**
     * Load in all the Schedule Term Codes.
     */
    private void loadTermCodes() {
        X2Criteria termCodesCriteria = new X2Criteria();
        termCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_SCHEDULE_TERMS);
        BeanQuery termCodesQuery = new BeanQuery(ReferenceCode.class, termCodesCriteria);
        Collection<ReferenceCode> termCodes = getBroker().getCollectionByQuery(termCodesQuery);

        for (ReferenceCode referenceCode : termCodes) {
            m_termCodesMap.put(referenceCode.getOid(), referenceCode.getCode());
        }
    }
}
