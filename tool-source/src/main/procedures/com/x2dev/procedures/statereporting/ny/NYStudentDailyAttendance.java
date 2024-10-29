/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Student Daily Attendance export.
 *
 * @author X2 Development Corporation
 */

public class NYStudentDailyAttendance extends StateReportData {
    /**
     * Entity class for Student Daily Attendance export.
     *
     * @author X2 Development Corporation
     */

    public static class StudentDailyAttendanceEntity extends StateReportEntity {
        private static final String MODALITY_BOTH = "B";
        private static final String MODALITY_INPERSON = "IN";
        private static final String MODALITY_NONE = "";
        private static final String MODALITY_REMOTE = "R";

        /**
         * Codes
         */
        private static final String CODE_ABSENT = "A";
        private static final String CODE_EXCUSED = "E";
        private static final String CODE_ISS = "ISS";
        private static final String CODE_OSS = "OSS";
        private static final String CODE_PRESENT = "PRSNT";
        private static final List<String> CODE_SUSPENSIONS = Arrays.asList("ISS", "OSS");
        private static final String CODE_TARDY = "T";

        /**
         * Suffixes
         */
        private static final String SUFFIX_IN = "-IN";
        private static final String SUFFIX_OUT = "-OUT";

        /**
         * Local variables for reporting information.
         */
        protected Map<String, StudentAttendance> m_attendances;
        protected NYStudentDailyAttendance m_data;
        protected Map<PlainDate, School> m_insessionDays;
        protected List<AttendanceRowData> m_rows;
        protected Student m_student;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentDailyAttendanceEntity() {
            // no argument constructor
        }

        /**
         * Gets the modality for current record.
         *
         * @return String
         */
        public String getAdjustedModality() {
            AttendanceRowData attendanceRow = getAttendanceRowData();
            String modality = attendanceRow.getModality();
            if (attendanceRow.isVirtualPgmExists() && MODALITY_INPERSON.equals(modality)) {
                String code = attendanceRow.getCode();
                if (!StringUtils.isEmpty(code) && CODE_PRESENT.equals(code) || CODE_TARDY.equals(code)) {
                    modality = MODALITY_REMOTE;
                }
            }
            return modality;
        }

        /**
         * returns the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            AttendanceRowData ard = m_rows.get(getCurrentRow());
            return ard.getDate() + ", " + ard.getCode() + ", " + m_student.getNameView();
        }

        /**
         * Gets the attendance.
         *
         * @return Student attendance
         */
        public AttendanceRowData getAttendanceRowData() {
            return m_rows.get(getCurrentRow());
        }

        /**
         * Gets the state code.
         *
         * @return String
         */
        public String getStateCode() {
            return getAdjustedAttendanceCode(getAttendanceRowData());
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_student = (Student) bean;
            m_data = (NYStudentDailyAttendance) data;
            m_attendances = new HashMap<String, StudentAttendance>();

            initializeInsessionDays();

            X2Criteria studentAttendanceCriteria = new X2Criteria();
            studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_data.m_reportRunDate);
            studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_data.m_reportDate);
            studentAttendanceCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, m_student.getOid());

            QueryByCriteria attQuery = new QueryByCriteria(StudentAttendance.class, studentAttendanceCriteria);

            List<StudentAttendance> attendances = new ArrayList<StudentAttendance>();
            attendances.addAll(m_data.getBroker().getCollectionByQuery(attQuery));

            m_rows = new ArrayList<AttendanceRowData>();
            for (StudentAttendance att : attendances) {
                if (!StringUtils.isEmpty(att.getCodeView())) {
                    m_attendances.put(att.getDate().toString(), att);
                }
            }
            for (PlainDate sessionDate : m_insessionDays.keySet()) {
                if (m_attendances.containsKey(sessionDate.toString())) {
                    StudentAttendance att = m_attendances.get(sessionDate.toString());
                    Map<String, AttendanceRowData> attCodes = new HashMap<String, AttendanceRowData>();
                    for (String code : att.getCodeView().split(" ")) {
                        if (m_data.m_attendanceStateCodes.containsKey(code)) {
                            RefAttendanceStudent refValue = m_data.m_attendanceStateCodes.get(code);

                            if (refValue.getAbsentIndicator() && attCodes.containsKey(CODE_ABSENT)) {
                                attCodes.remove(CODE_ABSENT);
                            }
                            String stateCode =
                                    (String) refValue.getFieldValueByBeanPath(m_data.m_attendanceStateCodeField);
                            boolean isVirtual = StringUtils.equals(
                                    (String) refValue.getFieldValueByBeanPath(m_data.m_isVirtualField),
                                    BooleanAsStringConverter.TRUE);
                            boolean isModalityBoth = StringUtils.equals(
                                    (String) refValue.getFieldValueByBeanPath(m_data.m_isModalityBothField),
                                    BooleanAsStringConverter.TRUE);
                            String modality;
                            if (isModalityBoth) {
                                modality = MODALITY_BOTH;
                            } else if (isVirtual) {
                                modality = MODALITY_REMOTE;
                            } else if (CODE_SUSPENSIONS.contains(stateCode)) {
                                modality = MODALITY_NONE;
                            } else {
                                modality = MODALITY_INPERSON;
                            }

                            attCodes.put(code, m_data.new AttendanceRowData(sessionDate,
                                    m_insessionDays.get(sessionDate), stateCode, modality,
                                    getVirtualProgramExists(sessionDate)));
                        }
                    }
                    if (StringUtils.equals(CODE_ISS, att.getCodeView())) {
                        m_rows.add(m_data.new AttendanceRowData(sessionDate, m_insessionDays.get(sessionDate),
                                CODE_PRESENT,
                                MODALITY_INPERSON,
                                getVirtualProgramExists(sessionDate)));
                    }
                    if (StringUtils.equals(CODE_OSS, att.getCodeView())) {
                        m_rows.add(m_data.new AttendanceRowData(sessionDate, m_insessionDays.get(sessionDate),
                                CODE_EXCUSED,
                                MODALITY_REMOTE,
                                getVirtualProgramExists(sessionDate)));
                    }
                    m_rows.addAll(attCodes.values());
                } else {
                    m_rows.add(m_data.new AttendanceRowData(sessionDate, m_insessionDays.get(sessionDate),
                            CODE_PRESENT,
                            MODALITY_INPERSON,
                            getVirtualProgramExists(sessionDate)));
                }
            }
            setRowCount(m_rows.size());
        }

        /**
         * Adds the member days.
         *
         * @param span StudentEnrollmentSpan
         */
        private void addMemberDays(StudentEnrollmentSpan span) {
            Set<PlainDate> insessionDates =
                    m_data.m_helper.getCalendarDays(span.getSchool(), m_student.getCalendarCode());
            if (insessionDates == null
                    && !StudentEnrollmentSpan.DEFAULT_CALENDAR_ID.equals(m_student.getCalendarCode())) {
                insessionDates = m_data.m_helper.getCalendarDays(span.getSchool(),
                        StudentEnrollmentSpan.DEFAULT_CALENDAR_ID);
            }
            if (insessionDates == null) {
                insessionDates =
                        m_data.m_helper.getCalendarDays(span.getSchool(), StudentHistoryHelper.CALENDAR_ANY);
            }
            // Count in session days between (and including) first and last active dates.
            PlainDate endDate = span.getLastActiveDate();
            if (endDate == null) {
                endDate = m_data.m_reportDate;
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (DateUtils.isBetween(date, m_data.m_reportRunDate, m_data.m_reportDate)) {
                        if (span.getFirstActiveDate() != null && !date.before(span.getFirstActiveDate())
                                && !date.after(endDate)) {
                            m_insessionDays.put(date, span.getSchool());
                        }
                    }
                }
            }
        }

        /**
         * Gets the adjusted attendance code.
         *
         * @param attendanceRow AttendanceRowData
         * @return String
         */
        private String getAdjustedAttendanceCode(AttendanceRowData attendanceRow) {
            String code = attendanceRow.getCode();
            if (!StringUtils.isEmpty(code) && CODE_PRESENT.equals(code) || CODE_TARDY.equals(code)) {
                code = code + (attendanceRow.isVirtualPgmExists() ? SUFFIX_OUT : SUFFIX_IN);
            }
            return code;
        }

        /**
         * Check if student has virtual program on given date.
         *
         * @param sessionDate PlainDate
         * @return String
         */
        private boolean getVirtualProgramExists(PlainDate sessionDate) {
            boolean isVirtualExists = false;
            if (m_student != null && m_data.m_stdPgmMap != null) {
                List<StudentProgramParticipation> stdPgm = m_data.m_stdPgmMap.get(m_student.getOid());
                if (stdPgm != null) {
                    isVirtualExists = stdPgm.stream()
                            .filter(pgm -> !pgm.getStartDate().after(sessionDate)
                                    && (pgm.getEndDate() == null || !pgm.getEndDate().before(sessionDate)))
                            .findFirst()
                            .map(pgm -> true)
                            .orElse(false);
                }
            }
            return isVirtualExists;
        }

        /**
         * Initialize in session days.
         */
        private void initializeInsessionDays() {
            m_insessionDays = new HashMap<PlainDate, School>();
            List<StudentEnrollmentSpan> spans =
                    new ArrayList(m_data.m_helper.getStudentEnrollmentSpans(m_student, false));
            if (spans != null && !spans.isEmpty()) {
                spans.removeIf(stdSpan -> stdSpan.getFirstActiveEnrollment() != null
                        && stdSpan.getFirstActiveEnrollment().getEnrollmentCode() != null
                        && "8300".equals(m_data.lookupStateValue(StudentEnrollment.class,
                                StudentEnrollment.COL_ENROLLMENT_CODE,
                                stdSpan.getFirstActiveEnrollment().getEnrollmentCode())));
                for (StudentEnrollmentSpan span : spans) {
                    if (span.getSchool() != null && m_data.m_includedSchoolOids.contains(span.getSchool().getOid())
                            && span.getFirstActiveEnrollment() != null) {
                        addMemberDays(span);
                    }
                }
            }
        }
    }

    /**
     * The Class AttendanceRowData.
     */
    protected class AttendanceRowData {
        private String m_code;
        private PlainDate m_date;
        private String m_modality;
        private School m_skl;
        private boolean m_virtualPgmExists;

        /**
         * Instantiates a new attendance row data.
         *
         * @param date PlainDate
         * @param skl School
         * @param code String
         * @param modality String
         * @param virtualPgmExists boolean
         */
        AttendanceRowData(PlainDate date, School skl, String code, String modality, boolean virtualPgmExists) {
            m_date = date;
            m_skl = skl;
            m_code = code;
            m_modality = modality;
            m_virtualPgmExists = virtualPgmExists;
        }

        /**
         * Gets the date.
         *
         * @return the date
         */
        public PlainDate getDate() {
            return m_date;
        }

        /**
         * Gets the school.
         *
         * @return the school
         */
        public School getSchool() {
            return m_skl;
        }

        /**
         * Gets the code.
         *
         * @return the code
         */
        public String getCode() {
            return m_code;
        }

        /**
         * Gets the modality.
         *
         * @return the modality
         */
        public String getModality() {
            return m_modality;
        }

        /**
         * Checks if is virtual pgm exists.
         *
         * @return the virtual program exists indicator
         */
        public boolean isVirtualPgmExists() {
            return m_virtualPgmExists;
        }
    }


    /**
     * Retrieves the Attendance Code field.
     */
    protected class RetrieveAttendanceCode implements FieldRetriever {

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
            StudentDailyAttendanceEntity sdaEntity = (StudentDailyAttendanceEntity) entity;
            String stateValue = sdaEntity.getStateCode();
            return StringUtils.isEmpty(stateValue) ? ATTENDANCE_CODE_UNEXCUSED : stateValue;
        }
    }

    /**
     * Retrieves the Attendance Date field.
     */
    protected class RetrieveAttendanceDate implements FieldRetriever {

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
            StudentDailyAttendanceEntity sdaEntity = (StudentDailyAttendanceEntity) entity;
            return sdaEntity.getAttendanceRowData().getDate();

        }
    }

    /**
     * Retrieves the Attendance Modality field.
     */
    protected class RetrieveAttendanceModality implements FieldRetriever {

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
            StudentDailyAttendanceEntity sdaEntity = (StudentDailyAttendanceEntity) entity;
            return sdaEntity.getAdjustedModality();

        }
    }

    /**
     * Retrieves the location code for the attendance record.
     */
    protected class RetrieveLocationCode implements FieldRetriever {

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
            StudentDailyAttendanceEntity sdaEntity = (StudentDailyAttendanceEntity) entity;
            SisStudent std = (SisStudent) entity.getBean();
            School school = sdaEntity.getAttendanceRowData().getSchool();
            PlainDate attDate = sdaEntity.getAttendanceRowData().getDate();
            StudentEnrollment stdEnrByDate = m_helper.getEnrollmentForDate(std.getOid(), attDate,
                    StudentEnrollment.ENTRY + StudentEnrollment.YOG_CHANGE + StudentEnrollment.STATUS_CHANGE);
            String enrLocCode = null;
            if (stdEnrByDate != null && !StringUtils
                    .isEmpty(enrLocCode = (String) stdEnrByDate.getFieldValueByBeanPath(m_fieldEnrLocOverride))) {
                value = lookupStateValue(StudentEnrollment.class, m_fieldEnrLocOverride, enrLocCode);
            } else if (StringUtils.isEmpty(value) && school != null) {
                String locationCode = (String) school.getFieldValueByBeanPath(m_schoolLocationCodeField);
                value = locationCode;
            }
            return value;
        }
    }

    /**
     * Validates attendance code.
     *
     * @author Follett Software Company
     */
    private class ValidateAttendanceCode implements FieldValidator {

        public static final String VAL_ID = "VAL_ATT_CODE";

        private List<String> m_attCode;
        private Map<String, Set<ValidatingFields>> m_studentValidatingFieldsMap;


        /**
         * Container of fields to determine if record duplacated (if the same location, code and
         * date).
         *
         * @author Follett Software Company
         */
        private class ValidatingFields {
            String m_code;
            String m_date;
            String m_location;

            /**
             * Instantiates a new validating fields.
             *
             * @param location String
             * @param code String
             * @param date String
             */
            public ValidatingFields(String location, String code, String date) {
                m_code = code;
                m_date = date;
                m_location = location;
            }

            /**
             * Equals.
             *
             * @param obj Object
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                if (!(obj instanceof ValidatingFields)) {
                    return false;
                }
                if (obj == this) {
                    return true;
                }
                ValidatingFields rhs = (ValidatingFields) obj;
                return new EqualsBuilder().append(m_code.toString(), rhs.m_code.toString())
                        .append(m_date.toString(), rhs.m_date.toString())
                        .append(m_location.toString(), rhs.m_location.toString()).isEquals();
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return new HashCodeBuilder(17, 31).append(m_code).append(m_date).append(m_location).toHashCode();
            }
        }

        /**
         * Instantiates a new validate attendance code.
         */
        public ValidateAttendanceCode() {
            m_attCode = Arrays.asList("E", "ISS", "OSS", "T", "U");
            m_studentValidatingFieldsMap = new HashMap<String, Set<ValidatingFields>>();
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String studentOid = ((StudentAttendance) entity.getBean()).getStudentOid();
            Set<ValidatingFields> validatingFields = m_studentValidatingFieldsMap.get(studentOid);
            if (validatingFields == null) {
                validatingFields = new HashSet<ValidatingFields>();
                m_studentValidatingFieldsMap.put(studentOid, validatingFields);
            }
            String attDateValue = entity.getFieldValue(FIELD_ID_ATTENDANCE_DATE);
            String locationCode = entity.getFieldValue(FIELD_ID_LOCATION_CODE);
            if (!validatingFields.add(new ValidatingFields(locationCode, value, attDateValue))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Duplicate State code reported for the same date",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                FIELD_ID_ATTENDANCE_DATE + " = " + STYLE_BOLD + attDateValue + STYLE_END + ", " +
                                FIELD_ID_LOCATION_CODE + " = " + STYLE_BOLD + locationCode + STYLE_END));
            }

            if (!m_attCode.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be valid code " + m_attCode,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validates attendance date.
     *
     * @author Follett Software Company
     */
    private class ValidateAttendanceDate implements FieldValidator {
        public static final String VAL_ID = "VAL_ATT_DATE";

        private PlainDate m_july01;
        private PlainDate m_june30;

        /**
         * Instantiates a new validate attendance date.
         */
        public ValidateAttendanceDate() {
            Calendar julyCal = Calendar.getInstance();
            julyCal.set(Calendar.DATE, 1);
            julyCal.set(Calendar.MONTH, 6);
            julyCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear() - 1);
            m_july01 = new PlainDate(julyCal.getTime());

            Calendar juneCal = Calendar.getInstance();
            juneCal.set(Calendar.DATE, 30);
            juneCal.set(Calendar.MONTH, 5);
            juneCal.set(Calendar.YEAR, getCurrentContext().getSchoolYear());
            m_june30 = new PlainDate(juneCal.getTime());
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            Date attendanceDate = null;
            try {
                attendanceDate = m_dateFormat.parse(value);
            } catch (ParseException e) {
                e.printStackTrace();
            }

            if (attendanceDate != null) {
                if (attendanceDate.before(m_july01) || attendanceDate.after(m_june30)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " must fall between " + m_dateFormat.format(m_july01) +
                                    " and " + m_dateFormat.format(m_june30),
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
                if (attendanceDate.after(m_reportDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " cannot be a future date",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    ", Report Date = " + STYLE_BOLD + m_dateFormat.format(m_reportDate) + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_ENR_LOC_OVERRIDE = "DOE LOCATION OVERRIDE";
    protected static final String ALIAS_EXCLUDE_SKL_SA129 = "all-skl-SA129Exclude";
    protected static final String ALIAS_EXCLUDE_SKL = "all-skl-ExcludeFromAttendanceExtract";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_NUMERIC_GRADE = "NumericGradeLevel";
    protected static final String ALIAS_RAT_ATTENDANCE_STATE_CODE = "NY Attendance State Code";
    protected static final String ALIAS_RAT_IS_MODALITY_BOTH = "all-rat-ModalityBoth";
    protected static final String ALIAS_RAT_IS_VIRTUAL = "all-rat-VirtualFlag";
    protected static final String ALIAS_SCHOOL_LOCATION_CODE = "LOCATION CODE";

    /**
     * Constants for reporting information.
     */
    protected static final String ATTENDANCE_CODE_UNEXCUSED = "U";
    protected static final String CODE_PGM_CODE_VIRTUAL = "VIRTUAL";
    protected static final String EXTENDED_DICTIONARY_ID_GRADE_NUMERIC = "REF-GRADE-LEVELS";

    /**
     * Fields
     */
    protected static final String FIELD_ATTENDANCE_CODE_LONG = "Attendance Code Long";
    protected static final String FIELD_ID_ATTENDANCE_CODE = "Attendance Code Long";
    protected static final String FIELD_ID_ATTENDANCE_DATE = "Attendance Date";
    protected static final String FIELD_ID_LOCATION_CODE = "Location Code";

    /**
     * Parameters
     */
    protected static final String PARAM_EXCLUDE_PREK = "excludePREK";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_RUN_DATE = "runDate";

    /**
     * Local variables for reporting information.
     */
    protected String m_attendanceStateCodeField;
    protected Map<String, RefAttendanceStudent> m_attendanceStateCodes = new HashMap<String, RefAttendanceStudent>();
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");
    protected String m_excludeStdField;
    protected String m_excludeSklField;
    protected String m_excludeSklSA129Field;
    protected String m_fieldEnrLocOverride;
    protected StudentHistoryHelper m_helper;
    protected Set<String> m_includedSchoolOids = new HashSet();
    protected String m_isModalityBothField;
    protected String m_isVirtualField;
    protected boolean m_removeHeaderIndicator;
    protected PlainDate m_reportDate;
    protected PlainDate m_reportRunDate;
    protected String m_schoolLocationCodeField;
    protected Map<String, List<StudentProgramParticipation>> m_stdPgmMap;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        initializeSchoolSet();

        initializeAttendanceCodeMap();

        initPgmMap();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_reportRunDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        boolean excludePrek = true;
        if (getParameter(PARAM_EXCLUDE_PREK) != null) {
            excludePrek = ((Boolean) getParameter(PARAM_EXCLUDE_PREK)).booleanValue();
        }
        Collection<String> stdGrades = getReportedGrades(excludePrek);
        m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, stdGrades);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(true));
            setEntityClass(StudentDailyAttendanceEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STDATTEND-CODE", new RetrieveAttendanceCode());
            calcs.put("STDATTEND-DATE", new RetrieveAttendanceDate());
            calcs.put("STDATTEND-LOCATION", new RetrieveLocationCode());
            calcs.put("STDATTEND-MODALITY", new RetrieveAttendanceModality());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateAttendanceDate.VAL_ID, new ValidateAttendanceDate());
            validators.put(ValidateAttendanceCode.VAL_ID, new ValidateAttendanceCode());
            super.addValidators(validators);
        }
    }

    /**
     * Adds the school exclusion criteria.
     *
     * @param criteria X2Criteria
     * @param path String
     */
    private void addSchoolExclusionCriteria(X2Criteria criteria, String path) {
        if (!StringUtils.isEmpty(m_excludeSklField)) {
            criteria.addNotEqualTo(path + m_excludeSklField, BooleanAsStringConverter.TRUE);
        }
        if (!StringUtils.isEmpty(m_excludeSklSA129Field)) {
            criteria.addNotEqualTo(path + m_excludeSklSA129Field, BooleanAsStringConverter.TRUE);
        }
    }

    /**
     * Gets the reported grades.
     *
     * @param excludePrek boolean
     * @return Collection
     */
    private Collection<String> getReportedGrades(boolean excludePrek) {
        Collection<String> gradesToReturn = new ArrayList<>();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, EXTENDED_DICTIONARY_ID_GRADE_NUMERIC);
        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker()
                .getBeanByQuery(new QueryByCriteria(ExtendedDataDictionary.class, criteria));
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
        DataDictionaryField gradeLevelField = dictionary.findDataDictionaryField(Student.class.getName(),
                Student.COL_GRADE_LEVEL);

        DataDictionaryField numericGradeLevelField = dictionary.findDataDictionaryFieldByAlias(ALIAS_NUMERIC_GRADE);
        if (gradeLevelField.hasReferenceTable()) {
            for (ReferenceCode code : gradeLevelField.getReferenceTable().getReferenceCodes(getBroker())) {
                if (!code.getDisabledIndicator()) {
                    String numericString =
                            (String) code.getFieldValueByBeanPath(numericGradeLevelField.getJavaName());

                    if (!StringUtils.isEmpty(numericString)) {
                        int numericValue = 1000;
                        try {
                            numericValue = Integer.parseInt(numericString);
                        } catch (NumberFormatException e) {
                            continue;
                        }
                        if (numericValue < 1000) {
                            if (excludePrek && numericValue < 0) {
                                continue;
                            }
                            gradesToReturn.add(code.getCode());
                        }
                    }
                }
            }
        }
        return gradesToReturn;
    }

    /**
     * Initialize the program participation map.
     */
    private void initPgmMap() {
        X2Criteria pgmCriteria = new X2Criteria();
        pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getCurrentContext().getEndDate());

        X2Criteria emptyEndDateCriteria = new X2Criteria();
        emptyEndDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                getCurrentContext().getStartDate());
        endDateCriteria.addOrCriteria(emptyEndDateCriteria);
        pgmCriteria.addAndCriteria(endDateCriteria);

        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);

        X2Criteria refCodeCriteria = new X2Criteria();
        refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        refCodeCriteria.addEqualTo(ReferenceCode.COL_CODE, CODE_PGM_CODE_VIRTUAL);
        pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE,
                new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, refCodeCriteria));

        QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, pgmCriteria);
        pgmQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_stdPgmMap =
                getBroker().getGroupedCollectionByQuery(pgmQuery, StudentProgramParticipation.COL_STUDENT_OID, 200);
    }

    /**
     * The attendance reference map is somewhat strange and isn't able to be used with lookupValue
     * from StateReportData.java
     * This method pulls all attendance codes and stores them with their state value for translation
     * later.
     */
    private void initializeAttendanceCodeMap() {
        Map<String, RefAttendanceStudent> refTableMap = null;
        Criteria dailyRefCodeCriteria = new Criteria();
        dailyRefCodeCriteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE,
                Integer.valueOf(RefAttendanceStudent.ATTENDANCE_TYPE_DAILY)); // Student Daily
                                                                              // Attendance Code
        QueryByCriteria dailyRefCodeQuery = new QueryByCriteria(RefAttendanceStudent.class, dailyRefCodeCriteria);

        refTableMap = getBroker().getMapByQuery(dailyRefCodeQuery, RefAttendanceStudent.COL_ATTENDANCE_CODE, 30);
        if (refTableMap != null) {
            for (String code : refTableMap.keySet()) {
                RefAttendanceStudent refValue = refTableMap.get(code);
                String stateCode = (String) refValue.getFieldValueByBeanPath(m_attendanceStateCodeField);
                if (!StringUtils.isEmpty(stateCode)) {
                    m_attendanceStateCodes.put(refValue.getAttendanceCode(), refValue);
                }
            }
        }
    }

    /**
     * Initialize Fields.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_reportRunDate = (PlainDate) getParameter(PARAM_RUN_DATE);
        if (m_reportRunDate == null) {
            m_reportRunDate = getCurrentContext().getStartDate();
        }
        if (m_reportRunDate.after(m_reportDate)) {
            m_reportRunDate = m_reportDate;
        }

        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, false);
        m_excludeSklSA129Field = translateAliasToJavaName(ALIAS_EXCLUDE_SKL_SA129, false);
        m_attendanceStateCodeField = translateAliasToJavaName(ALIAS_RAT_ATTENDANCE_STATE_CODE, true);
        m_schoolLocationCodeField = translateAliasToJavaName(ALIAS_SCHOOL_LOCATION_CODE, true);
        m_isVirtualField = translateAliasToJavaName(ALIAS_RAT_IS_VIRTUAL, true);
        m_isModalityBothField = translateAliasToJavaName(ALIAS_RAT_IS_MODALITY_BOTH, true);
        m_fieldEnrLocOverride = translateAliasToJavaName(ALIAS_ENR_LOC_OVERRIDE, true);
    }

    /**
     * Initialize school set.
     */
    private void initializeSchoolSet() {
        X2Criteria criteria = new X2Criteria();
        addSchoolExclusionCriteria(criteria, "");

        if (isSchoolContext()) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else {
            criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        ColumnQuery query = new ColumnQuery(SisSchool.class, new String[] {X2BaseBean.COL_OID}, criteria);
        try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                m_includedSchoolOids.add((String) row[0]);
            }
        }
    }
}
