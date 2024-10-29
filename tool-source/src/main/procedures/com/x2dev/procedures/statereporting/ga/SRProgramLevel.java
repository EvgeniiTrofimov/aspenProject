/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2012 X2 Development Corporation.
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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
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

/**
 * This class implements the data export for Student Record Program Level export.
 *
 * @author X2 Development Corporation
 */
public class SRProgramLevel extends StateReportData {
    /**
     * Entity class for Student Record Student Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SRProgramLevelEntity extends StateReportEntity {

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRProgramLevelEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Student student = ((StudentProgramParticipation) getBean()).getStudent();

            String name = student.getNameView() +
                    " [Local ID: " + student.getLocalId() +
                    ", GTID: " + this.getFieldValue("GTID") +
                    "]";
            return name;
        }
    }

    /**
     * Codes
     */
    private static final String REC_CODES_PROGRAM_CODE = "Flexible Learning";
    /**
     * Aliases
     */
    private static final String ALIAS_DOE_SCHOOL_CODE = "DOE School";
    private static final String ALIAS_DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    private static final String ALIAS_DOE_PROGRAM_CODE = "DOE Student Program Code";
    private static final String ALIAS_DOE_PROGRAM_TYPE = "DOE Program Type";

    /**
     * Constants for reporting information.
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String PARAM_STARTING_DATE = "startingDate";
    protected static final String PARAM_QUERY_BY = "queryBy";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_SORT = "sort";
    protected static final String PROGRAM_TYPE_LOCAL_CODE = "K01";
    protected static final String PROGRAM_TYPE_05 = "05";

    protected static final String FIELD_PROGRAM_CODE = "Program Code";
    protected static final String FIELD_PROGRAM_TYPE = "Program Type";
    protected static final String FIELD_ALTERNATE_SCHOOL_NUM = "Alt School Number";
    protected static final String FIELD_ALTERNATE_SYSTEM_CODE = "Alt System Code";
    protected static final String FIELD_SYSTEM_CODE = "System Code";
    protected static final String FIELD_HOURS_OFFERED = "HOURS OFFERED";
    protected static final String FIELD_HOURS_ATTENDED = "HOURS ATTENDED";
    protected static final String FIELD_NUM_DAILY_GNETS = "Num Daily GNETS";
    protected static final String FIELD_SUBJECT_AREA = "SUBJECT AREA";

    /**
     * Export Retriever Field Names
     */
    private static final String GA_SR_CLEAN = "GA-SR-CLEAN";
    private static final String GA_SR_DAYS_ATTENDED = "GA-SR-DAYS-ATTEND";
    private static final String GA_SR_SESSION_ATTEND = "GA-SR-SESSION-ATTEND";
    private static final String GA_SR_SKL_CODE = "GA-SR-SKL-CODE";
    private static final String GA_SR_START_DATE = "GA-SR-START-DATE";
    private static final String GA_SR_END_DATE = "GA-SR-END-DATE";
    private static final String GA_SR_VAL_ALT_SKL_NUM = "GA-SR-VAL-ASKL-NUM";
    private static final String GA_SR_VAL_ALT_SYS_CODE = "GA-SR-VAL-ALT-SYS-CODE";
    private static final String GA_SR_VAL_GNETS = "GA-SR-VAL-GNETS";
    private static final String GA_SR_VAL_HOURS_OFF = "GA-SR-VAL-HOURS-OFF";
    private static final String GA_SR_VAL_HOURS_ATT = "GA-SR-VAL-HOURS-ATT";
    private static final String GA_SR_VAL_PGM_CODE = "GA-SR-VAL-PGM-CODE";
    private static final String GA_SR_VAL_PGM_TYPE = "GA-SR-VAL-PGM-TYPE";
    private static final String GA_SR_VAL_SBJ_AREA = "GA-SR-VAL-SBJ-AREA";

    /**
     * Local variables for reporting information.
     */
    protected String m_activeCode;
    protected PlainDate m_reportDate;
    protected PlainDate m_startingDate;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected PlainDate m_currentYearStartDate;
    protected PlainDate m_currentYearEndDate;
    protected String m_schoolCodeField;
    protected String m_overrideSchoolCodeField;
    protected String m_doeProgramCode;
    protected String m_programType;


    protected StudentHistoryHelper m_helper;

    /**
     * Returns the FLP Sessions Attended element only when the Program Type has a state code of 05.
     * Returns null for all else.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSessionsAttended implements FieldRetriever {
        private static final String STRING_05 = "05";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String programTypeStateCode = null;
            SRProgramLevelEntity studentProgram = (SRProgramLevelEntity) entity;
            String programType = (String) studentProgram.getBean().getFieldValueByBeanPath(m_programType);
            programTypeStateCode = lookupStateValue(StudentProgramParticipation.class, m_programType, programType);

            if (!StringUtils.isEmpty(programTypeStateCode) && STRING_05.equals(programTypeStateCode)) {
                value = (String) getProperty(entity.getBean(), field.getBeanPath());
            }
            return value;
        }
    }

    /**
     * Calculate Days Attended.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDaysAttended implements FieldRetriever {
        private Map<String, Collection<PlainDate>> m_attendedDatesMap = new HashMap<String, Collection<PlainDate>>();
        private ArrayList<PlainDate> m_attendedDatesList = new ArrayList<PlainDate>();

        /**
         * Instantiates a new retrieve days attended.
         */
        public RetrieveDaysAttended() {
            QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
            Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);
            PlainDate today = new PlainDate();

            for (SisStudent student : students) {
                String studentOid = student.getOid();
                List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(student, false);

                for (StudentEnrollmentSpan span : spans) {
                    PlainDate firstActiveDate = span.getFirstActiveDate();
                    if (firstActiveDate == null) {
                        firstActiveDate = m_currentYearStartDate;
                    }
                    PlainDate lastActiveDate = span.getLastActiveDate();
                    if (lastActiveDate == null) {
                        lastActiveDate = today;
                    }

                    // Get possible days within a student's enrollment span
                    Collection<PlainDate> inSessionDates =
                            CalendarManager.getInSessionDates(firstActiveDate, lastActiveDate, student, getBroker());

                    // Get Absences within a student's enrollment span
                    List<StudentAttendance> attendances = null;
                    try {
                        attendances = span.getStudentAttendance();
                    } catch (Exception e) {
                        attendances = new ArrayList<StudentAttendance>();
                    }

                    for (PlainDate sessiondate : inSessionDates) {
                        if (!sessiondate.before(firstActiveDate) && !sessiondate.after(lastActiveDate)) {
                            if (attendances.size() > 0) {
                                for (StudentAttendance attendance : attendances) {
                                    if (!attendance.getAbsentIndicator()) {
                                        m_attendedDatesList.add(sessiondate);
                                    }
                                }
                            } else {
                                m_attendedDatesList.add(sessiondate);
                            }
                        }
                    }
                }

                if (m_attendedDatesMap.containsKey(studentOid)) {
                    ArrayList<PlainDate> attendedDates = (ArrayList<PlainDate>) m_attendedDatesMap.get(studentOid);
                    attendedDates.addAll(m_attendedDatesList);
                    m_attendedDatesMap.put(studentOid, attendedDates);
                } else {
                    m_attendedDatesMap.put(studentOid, m_attendedDatesList);
                }
            }
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = Integer.valueOf(0);

            StudentProgramParticipation studentProgramParticipation = (StudentProgramParticipation) entity.getBean();
            String studentOid = studentProgramParticipation.getStudentOid();
            PlainDate startDate = studentProgramParticipation.getStartDate();
            PlainDate endDate = studentProgramParticipation.getEndDate();
            if (endDate == null) {
                endDate = new PlainDate();
            }
            String programCode = studentProgramParticipation.getProgramCode();

            int daysPossible = 0;
            if (REC_CODES_PROGRAM_CODE.equals(programCode)) {
                if (!StringUtils.isEmpty(studentOid) && m_attendedDatesMap.containsKey(studentOid)) {
                    ArrayList<PlainDate> attendedDates = (ArrayList<PlainDate>) m_attendedDatesMap.get(studentOid);

                    for (PlainDate attendedDate : attendedDates) {
                        if (!attendedDate.before(startDate) && !attendedDate.after(endDate)) {
                            daysPossible++;
                        }
                    }
                }
            }
            value = Integer.valueOf(daysPossible);

            return value;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(EMPTY_STRING);
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = EMPTY_STRING;
            }

            return cleanValue;
        }
    }


    /**
     * Retrieve the school code from the school on the row. A student
     * can generate multiple schools, so retrieve the code from the current row school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

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
            String value = null;
            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;
            Student student = ((StudentProgramParticipation) programLevelEntity.getBean()).getStudent();
            SisSchool school = (SisSchool) student.getSchool();
            if (null != student) {
                value = (String) student.getFieldValueByBeanPath(m_overrideSchoolCodeField);
            }
            if (StringUtils.isEmpty(value) && school != null) {
                value = (String) school.getFieldValueByBeanPath(m_schoolCodeField);
            }

            return value;
        }
    }

    /**
     * Retrieve the start date from student program participation.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStartDate implements FieldRetriever {

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
            PlainDate pgmStartDate = null;
            StudentProgramParticipation studentProgramParticipation = (StudentProgramParticipation) entity.getBean();
            String programCode = studentProgramParticipation.getProgramCode();
            String pgmCodeState = lookupReferenceCodeByAlias(ALIAS_DOE_PROGRAM_TYPE, programCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (!pgmCodeState.equals(PROGRAM_TYPE_05)) {
                pgmStartDate = studentProgramParticipation.getStartDate();
            }
            return pgmStartDate;
        }
    }

    /**
     * Retrieve the start date from student program participation.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEndDate implements FieldRetriever {

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
            PlainDate pgmEndDate = null;
            StudentProgramParticipation studentProgramParticipation = (StudentProgramParticipation) entity.getBean();
            String programCode = studentProgramParticipation.getProgramCode();
            String pgmCodeState = lookupReferenceCodeByAlias(ALIAS_DOE_PROGRAM_TYPE, programCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (!pgmCodeState.equals(PROGRAM_TYPE_05)) {
                pgmEndDate = studentProgramParticipation.getEndDate();
            }
            return pgmEndDate;
        }
    }

    /**
     * Validate Alternate School Number.
     */
    protected class ValidateAlternateSchoolNumber implements FieldValidator {
        private static final String CODE_REQUIRING_ALT_SCHOOL = "03";
        private static final String ERROR =
                SRProgramLevel.FIELD_ALTERNATE_SCHOOL_NUM + " not valid for " + SRProgramLevel.FIELD_PROGRAM_TYPE + " "
                        + CODE_REQUIRING_ALT_SCHOOL + ".";
        private static final String MESSAGE = SRProgramLevel.FIELD_ALTERNATE_SCHOOL_NUM + " not blank/null for "
                + SRProgramLevel.FIELD_PROGRAM_TYPE + " " + CODE_REQUIRING_ALT_SCHOOL;

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);

            // If the program type is '03' Alt School Number should be blank/null
            if ((programType.contentEquals(CODE_REQUIRING_ALT_SCHOOL) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field, ERROR, MESSAGE));
            }

            return errors;
        }
    }

    /**
     * Validate Alternate System Code.
     */
    protected class ValidateAlternateSystemCode implements FieldValidator {
        private static final String CODE_REQUIRING_SYSTEM_CODE = "03";

        private static final String ERROR_02_04 = "System Codes Not Matching";
        private static final String MESSAGE_02_04 =
                "System Code must match alternate system code for program types '02' and '04'";

        private static final String ERROR_03 =
                SRProgramLevel.FIELD_ALTERNATE_SYSTEM_CODE + " not valid for " + SRProgramLevel.FIELD_PROGRAM_TYPE + " "
                        + CODE_REQUIRING_SYSTEM_CODE + ".";
        private static final String MESSAGE_03 = SRProgramLevel.FIELD_ALTERNATE_SYSTEM_CODE + " not blank/null for "
                + SRProgramLevel.FIELD_PROGRAM_TYPE + " " + CODE_REQUIRING_SYSTEM_CODE;

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();


            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);
            String altSystemCode = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_ALTERNATE_SYSTEM_CODE);
            String systemCode = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_SYSTEM_CODE);

            // If the program type is '02' or '04' and the alternate system code does not equal the
            // system code add a validation error
            if ((programType.contentEquals("02") || programType.contentEquals("04")) &&
                    !altSystemCode.equals(systemCode)) {
                errors.add(new StateReportValidationError(entity, field, ERROR_02_04, MESSAGE_02_04));
            }

            // If the program type is '03' Alt School Number should be blank/null
            if ((programType.contentEquals(CODE_REQUIRING_SYSTEM_CODE) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field, ERROR_03, MESSAGE_03));
            }

            return errors;
        }
    }

    /**
     * The Class ValidateGnets.
     */
    protected class ValidateGnets implements FieldValidator {
        private static final String CODE_REQUIRING_GNETS = "02";

        private static final String ERROR_01_03_04 =
                SRProgramLevel.FIELD_NUM_DAILY_GNETS + " segments not valid for " + SRProgramLevel.FIELD_PROGRAM_TYPE
                        + " 01, 03 or 04";

        private static final String MESSAGE_01_03_04 =
                "Number of GNETS Segments not valid for " + SRProgramLevel.FIELD_PROGRAM_TYPE + " 01, 03 or 04.";

        private static final String ERROR_02 =
                SRProgramLevel.FIELD_NUM_DAILY_GNETS + " segment required for " + SRProgramLevel.FIELD_PROGRAM_TYPE
                        + " = " + CODE_REQUIRING_GNETS;

        private static final String MESSAGE_02 = "Number of GNETS segments required for GNETS Program.";

        private List<String> m_codesNotValid = null;

        /**
         *
         */
        public ValidateGnets() {
            m_codesNotValid = Arrays.asList("01", "03", "04");
        }

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);

            // If the program type is '02' Num Daily GNETS should be not blank/null
            if ((programType.contentEquals(CODE_REQUIRING_GNETS) && StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field, ERROR_02, MESSAGE_02));
            } else if (m_codesNotValid.contains(programType)
                    && (!StringUtils.isEmpty(value) && Integer.valueOf(value).intValue() > 0)) {
                errors.add(new StateReportValidationError(entity, field, ERROR_01_03_04, MESSAGE_01_03_04));
            }

            return errors;
        }
    }

    /**
     * Validate Hours Attended.
     */
    protected class ValidateHoursAttended implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            int hoursOffered = 0;
            int hoursAttended = 0;

            try {
                hoursOffered = Integer.parseInt(programLevelEntity.getFieldValue(SRProgramLevel.FIELD_HOURS_OFFERED));
                hoursAttended =
                        Integer.parseInt(programLevelEntity.getFieldValue(SRProgramLevel.FIELD_HOURS_ATTENDED));
            } catch (NumberFormatException e) {
                String error = "Invalid hours offered or attended";
                String message = "Non-numeric entry for either hours offered or attended";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);

            // If the program type is '05' than the hours offered must be less than 2000 and greater
            // than 0
            if (programType.contentEquals("05") && (hoursAttended > 2000 || hoursAttended < 0)) {
                String error = "Hours Attended Invalid";
                String message =
                        "When program code is '05' the hours attended must be less than 2000 and greater than 0";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            // Hours offered must be 0 when program type is not '05'
            if (!programType.contentEquals("05") && hoursAttended != 0) {
                String error = "Hours Offered Invalid";
                String message = "Hours offered must be 0 when program type is not '05'";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            if (hoursAttended > hoursOffered) {
                String error = "Hours Attended Invalid";
                String message = "Hours attended cannot exceed hours offered";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            return errors;
        }
    }

    /**
     * Validate Hours Offered.
     */
    protected class ValidateHoursOffered implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            int hoursOffered = 0;

            try {
                hoursOffered = Integer.parseInt(programLevelEntity.getFieldValue(SRProgramLevel.FIELD_HOURS_OFFERED));
            } catch (NumberFormatException e) {
                String error = "Invalid hours offered";
                String message = "Non-numeric entry for hours offered";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);

            // If the program type is '05' than the hours offered must be less than 2000 and greater
            // than 0
            if (programType.contentEquals("05") && (hoursOffered > 2000 || hoursOffered < 0)) {
                String error = "Hours Offered Invalid";
                String message =
                        "When program code is '05' the hours offered must be less than 2000 and greater than 0";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            // Hours offered must be 0 when program type is not '05'
            if (!programType.contentEquals("05") && hoursOffered != 0) {
                String error = "Hours Offered Invalid";
                String message = "Hours offered must be 0 when program type is not '05'";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            return errors;
        }
    }

    /**
     * Validate Program Code.
     */
    protected class ValidateProgramCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            String programCode = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_CODE);
            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);

            // If the program type is '04' the code must either be '04A' or '04C'
            if (programType.contentEquals("04") &&
                    !(programCode.contentEquals("04A") || programCode.contentEquals("04C"))) {
                String error = "Program Code Error";
                String message = "Program Code must either be 04A or 04C for program type 04";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            return errors;
        }
    }

    /**
     * Validate Program Type.
     */
    protected class ValidateProgramType implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);

            // This will not support a program at a secondary school
            Student student = ((StudentProgramParticipation) programLevelEntity.getBean()).getStudent();
            String schoolType = student.getSchool().getSchoolTypeCode();

            if ((programType.contentEquals("04") || programType.contentEquals("05"))
                    && !schoolType.equals("Alternative")) {
                String error = "Program Type Error";
                String message =
                        "If program type is '04' or '05' the student program must be associated with an Alternative school";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            if (programType.contentEquals("03") && !(schoolType.equals("Charter") || schoolType.equals("Magnet"))) {
                String error = "Program Type Error";
                String message =
                        "If program type is '03' the student program must be associated with an Charter or Magent school";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            return errors;
        }
    }

    /**
     * Validate Subject Area.
     */
    protected class ValidateSubjectArea implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            SRProgramLevelEntity programLevelEntity = (SRProgramLevelEntity) entity;

            String programType = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_PROGRAM_TYPE);
            String subjectArea = programLevelEntity.getFieldValue(SRProgramLevel.FIELD_SUBJECT_AREA);

            // If the program type is '05' the subject area must be one of the following codes
            if (programType.contentEquals("05") && !(subjectArea.contentEquals("01") ||
                    subjectArea.contentEquals("02") ||
                    subjectArea.contentEquals("03") ||
                    subjectArea.contentEquals("04") ||
                    subjectArea.contentEquals("05"))) {
                String error = "Subject Area Error";
                String message =
                        "If the program type is: " + programType + " the subject area must one of the following: "
                                + "'01 = Reading', '02 = English', '03 = Math', 04 = 'Science', '05 = Social Studies'";

                errors.add(new StateReportValidationError(entity, field, error, message));
            }

            return errors;
        }
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        // Set the report and starting date.
        m_reportDate = new PlainDate();

        // Lookup aliases
        m_schoolCodeField = translateAliasToJavaName(ALIAS_DOE_SCHOOL_CODE, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(ALIAS_DOE_OVERRIDE_SCHOOL, true);
        m_doeProgramCode = translateAliasToJavaName(ALIAS_DOE_PROGRAM_CODE, true);
        m_programType = translateAliasToJavaName(ALIAS_DOE_PROGRAM_TYPE, true);

        // Use current school year dates
        int currentSchoolYear = getOrganization().getCurrentContext().getSchoolYear();
        X2Criteria ctxCriteria = new X2Criteria();
        ctxCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(currentSchoolYear));
        BeanQuery contextQuery = new BeanQuery(DistrictSchoolYearContext.class, ctxCriteria);
        DistrictSchoolYearContext currentYearContext =
                (DistrictSchoolYearContext) getBroker().getBeanByQuery(contextQuery);
        m_currentYearStartDate = currentYearContext.getStartDate();
        m_currentYearEndDate = currentYearContext.getEndDate();

        // Lookup aliases
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        Criteria studentCriteria = getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        SubQuery programTypesSubQuery = getProgramTypesSubQuery();

        Criteria programCriteria = new Criteria();
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        programCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programTypesSubQuery);
        m_startingDate = (PlainDate) getParameter(PARAM_STARTING_DATE);
        if (m_startingDate != null) {
            programCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_startingDate);
        }
        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);

        Integer sort = (Integer) getParameter(PARAM_SORT);
        switch (sort != null ? sort.intValue() : 0) {
            case 0: // Name
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;

            case 1: // YOG
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_YOG);
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;

            case 2: // School
                programQuery.addOrderByAscending(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER + School.COL_NAME);
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_STATE_ID);
                break;

            default:
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + Student.COL_NAME_VIEW);
                break;
        }

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_currentYearStartDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_currentYearEndDate);
        m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, studentSubQuery);

        // Set the query to be used for student selection.
        setQuery(programQuery);
        setEntityClass(SRProgramLevelEntity.class);

        // Add any retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(GA_SR_CLEAN, new RetrieveStripNameChar());
        calcs.put(GA_SR_DAYS_ATTENDED, new RetrieveDaysAttended());
        calcs.put(GA_SR_SKL_CODE, new RetrieveSchoolCode());
        calcs.put(GA_SR_START_DATE, new RetrieveStartDate());
        calcs.put(GA_SR_END_DATE, new RetrieveEndDate());
        calcs.put(GA_SR_SESSION_ATTEND, new RetrieveSessionsAttended());
        super.addCalcs(calcs);

        // Add any validators
        HashMap vals = new HashMap<String, FieldValidator>();
        vals.put(GA_SR_VAL_PGM_CODE, new ValidateProgramCode());
        vals.put(GA_SR_VAL_ALT_SYS_CODE, new ValidateAlternateSystemCode());
        vals.put(GA_SR_VAL_HOURS_OFF, new ValidateHoursOffered());
        vals.put(GA_SR_VAL_HOURS_ATT, new ValidateHoursAttended());
        vals.put(GA_SR_VAL_SBJ_AREA, new ValidateSubjectArea());
        vals.put(GA_SR_VAL_PGM_TYPE, new ValidateProgramType());
        vals.put(GA_SR_VAL_ALT_SKL_NUM, new ValidateAlternateSchoolNumber());
        vals.put(GA_SR_VAL_GNETS, new ValidateGnets());
        super.addValidators(vals);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Students to include.
         *
         * 1. The student is active and in an active school.
         * or
         * 2. The student has (E,W) enrollment records within the school year.
         *
         */

        // Select students with enrollment activity (E,W) in the school this year.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        X2Criteria enrollCriteria2 = new X2Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        X2Criteria activityCriteria = new X2Criteria();
        PlainDate startDate = getOrganization().getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, m_activeCode);

        primaryCriteria.addOrCriteria(enrollCriteria);

        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria userCriteria = new X2Criteria();

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            userCriteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            userCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            userCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        /*
         * Check student selection criteria user input.
         */
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Get program type codes that have "K01" as their local code.
     *
     * @return program type codes
     */
    private SubQuery getProgramTypesSubQuery() {
        DataDictionaryField field = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryField(StudentProgramParticipation.class.getName(),
                        StudentProgramParticipation.COL_PROGRAM_CODE);

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, PROGRAM_TYPE_LOCAL_CODE);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);
        if (codes.size() == 0) {
            addSetupError("Program types setup", "There are no program types with local code '" + STYLE_BOLD
                    + PROGRAM_TYPE_LOCAL_CODE + STYLE_END + "'");
        }

        SubQuery subQuery = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
        return subQuery;
    }
}
