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
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Maryland State Report: September Enrollment export.
 * This class implements the data export for MD September Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class Enrollment extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD September Enrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class EnrollmentEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        float m_attendance = 0;
        Enrollment m_attendanceData = null;
        PlainDate m_beginDate = null;
        PlainDate m_endDate = null;
        String m_enrollCode = null;
        int m_membership = 0;
        EnrollmentSnapshot m_snapshot = null;
        String m_withdrawCode = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EnrollmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment school/membership
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            if (m_membership == 0 || m_enrollCode == null) {
                error = new StateReportValidationError(getEntityName(), "Membership", "No membership days", "");
            }

            return error;
        }

        /**
         * Returns the students days in attendance, based on membership days and absences.
         *
         * @return float
         */
        public float getAttendance() {
            return m_attendance;
        }

        /**
         * Returns the students enrollment status code and reason.
         *
         * @return String
         */
        public String getEnrollmentCode() {
            return m_enrollCode;
        }

        /**
         * Returns the students enrollment entry date, or year begin date.
         *
         * @return PlainDate
         */
        public PlainDate getEnrollmentDate() {
            return m_beginDate;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() + "]";

            return name;
        }

        /**
         * Returns the students days in membership.
         *
         * @return int
         */
        public int getMembership() {
            return m_membership;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot() {
            return m_snapshot;
        }

        /**
         * Returns the students withdraw status code and reason.
         *
         * @return String
         */
        public String getWithdrawCode() {
            return m_withdrawCode;
        }

        /**
         * Returns the students withdraw date.
         *
         * @return PlainDate
         */
        public PlainDate getWithdrawDate() {
            return m_endDate;
        }

        /**
         * Intitialize.
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

            m_attendanceData = (Enrollment) data;

            // Get enrollment snapshot on report as of date.
            SisStudent student = (SisStudent) bean;
            m_snapshot =
                    getSnapshot(student, m_attendanceData.m_reportDate, data.getFieldDefinition(DOE_14_ENTRY_STATUS));

            // Get membership days and attendance days.
            SisSchool school = m_snapshot.getSchool();
            PlainDate firstDate = m_attendanceData.getCurrentContext().getStartDate();
            m_beginDate = m_attendanceData.getCurrentContext().getStartDate();
            m_enrollCode = "R02";

            List<StudentEnrollment> enrollments = m_attendanceData.m_enrollmentManager.getOrderedEnrollment(student,
                    null,
                    m_attendanceData.m_reportDate,
                    null,
                    false);

            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                    // If the prior enrollment record is before the beginning of the school year,
                    // use the default values instead. (R02, BOY)
                    if (m_beginDate.before(enrollment.getEnrollmentDate())) {
                        m_beginDate = enrollment.getEnrollmentDate();
                        m_enrollCode = enrollment.getEnrollmentCode();
                    }
                    break;
                } else if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    m_endDate = enrollment.getEnrollmentDate();
                    m_withdrawCode = enrollment.getEnrollmentCode();
                }
            }

            // Get membership days and attendance days.
            m_membership = m_attendanceData.m_enrollmentManager.getMembershipTotal(
                    student,
                    m_attendanceData.getCalendarDays(school, student.getCalendarCode()),
                    true,
                    firstDate,
                    m_attendanceData.m_reportDate,
                    school);

            Float absenceCount = m_attendanceData.m_absences.get(student.getOid());
            float absenceDays = 0;
            if (absenceCount != null) {
                absenceDays = absenceCount.floatValue();
            }
            m_attendance = m_membership - absenceDays;

        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate, FieldDefinition field) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            if (!snapshot.isPrecise()) {
                addRetrievalError(DOE_14_ENTRY_STATUS, new StateReportValidationError(this, field,
                        "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise", ""));
            }

            return snapshot;
        }
    }

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the report format parameter. This indicated CSV or Column delimited report.
     */
    public static final String REPORT_FORMAT_PARAM = "reportFormat";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * DOE field alias constants.
     *
     * Fields without aliases must have a bean path or retriever specified in the Field Definition.
     */
    private static final String DOE_01_LEA_ID = "DOE DISTRICT CODE";
    private static final String DOE_02_SCHOOL_ID = "DOE SCHOOL CODE";
    private static final String DOE_03_GRADE = "DOE GRADE";
    private static final String DOE_04_FULL_TIME = "DOE FULL TIME";
    private static final String DOE_05_STUDENT_LOCAL_ID = "DOE LASID";
    private static final String DOE_06_SSN = "DOE SSN";
    private static final String DOE_07_LAST_NAME = "DOE 05";
    private static final String DOE_08_FIRST_NAME = "DOE 06";
    private static final String DOE_09_MIDDLE_NAME = "DOE 07";
    private static final String DOE_10_BIRTH_DATE = "DOE 09";
    private static final String DOE_11_RACE = "DOE RACE";
    private static final String DOE_12_FILLER = "DOE FILLER1";
    private static final String DOE_13_GENDER = "DOE 11";
    private static final String DOE_14_ENTRY_STATUS = "DOE ENTRY STATUS";
    private static final String DOE_15_ENTRY_CODE = "DOE ENTRY CODE";
    private static final String DOE_16_ENTRY_DATE = "DOE ENTRY DATE";
    private static final String DOE_17_WITHDRAW_STATUS = "DOE WITHDRAW STATUS";
    private static final String DOE_18_WITHDRAW_CODE = "DOE WITHDRAW CODE";
    private static final String DOE_19_WITHDRAW_DATE = "DOE WITHDRAW DATE";
    private static final String DOE_20_ATTENDANCE_DAYS = "DOE ATTENDANCE";
    private static final String DOE_21_MEMBER_DAYS = "DOE MEMBER";
    private static final String DOE_22_STATE_AID = "DOE STATE AID";
    private static final String DOE_23_EVENING_STUDENT = "DOE EVENING";
    private static final String DOE_24_EVENING_COURSES = "DOE EV COURSE";
    private static final String DOE_25_PT_STUDENT = "DOE PARTTIME";
    private static final String DOE_26_PT_COURSES = "DOE PT COURSE";
    private static final String DOE_27_STATE_ID = "DOE SASID";

    /*
     * display label for discontinued fields.
     */
    private static final String RESERVED = "label.state.report.reserved";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_STATUS_FIELD = "DOE Status";
    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String DATE_FORMAT = "yyyyMMdd";
    private static final String NUMBER_FORMAT = "0000";
    private static final String REGEX_NUMERIC = "[0123456789]*";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Float> m_absences;
    protected SimpleDateFormat m_dateFormat;
    protected String m_doeStatusField;
    protected EnrollmentManager m_enrollmentManager;
    protected TreeMap m_gradeLevelMap;
    protected Collection<ReferenceCode> m_gradeRefCodes;
    protected Pattern m_illegalNameCharacters;
    protected String m_leaId;
    protected DecimalFormat m_numberFormat;
    protected PlainDate m_reportDate;
    protected Integer m_reportFormat;
    protected String m_schoolCodeField;
    protected HashMap m_schoolsToCalendars;

    /**
     * Retrieves a boolean field from the bean/bean path.
     * If it is a boolean or logical field, return the value as "Y" / "N".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveBooleanAsYN implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object oValue = getProperty(entity.getBean(), field.getBeanPath());
            String result = "N";
            if (oValue instanceof Boolean) {
                Boolean value = (Boolean) oValue;
                if (value.booleanValue()) {
                    result = "Y";
                }
            } else if (oValue instanceof String && "0".equals(oValue)) {
                result = "N";
            } else if (oValue instanceof String && "1".equals(oValue)) {
                result = "Y";
            }
            return result;
        }
    }

    /**
     * Returns the state equivalent for the enrollment status, code or date of the student based on
     * the membership period.
     * The student may have an multiple membership periods. The membership period comes from the
     * entry and current index.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollmentStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            EnrollmentEntity emrEnt = (EnrollmentEntity) entity;
            Object value = null;
            if (DOE_14_ENTRY_STATUS.equals(field.getFieldId())) {
                String code = emrEnt.getEnrollmentCode();
                if (code != null && code.length() > 0) {
                    value = code.substring(0, 1);
                }
            } else if (DOE_15_ENTRY_CODE.equals(field.getFieldId())) {
                String code = emrEnt.getEnrollmentCode();
                if (code != null && code.length() > 2) {
                    value = code.substring(1, 3);
                }
            } else if (DOE_16_ENTRY_DATE.equals(field.getFieldId())) {
                value = emrEnt.getEnrollmentDate();
            } else if (DOE_17_WITHDRAW_STATUS.equals(field.getFieldId())) {
                String code = emrEnt.getWithdrawCode();
                if (code != null && code.length() > 0) {
                    value = code.substring(0, 1);
                }
            } else if (DOE_18_WITHDRAW_CODE.equals(field.getFieldId())) {
                String code = emrEnt.getWithdrawCode();
                if (code != null && code.length() > 2) {
                    value = code.substring(1, 3);
                }
            } else if (DOE_19_WITHDRAW_DATE.equals(field.getFieldId())) {
                String code = emrEnt.getWithdrawCode();
                if (code != null) {
                    value = emrEnt.getWithdrawDate();
                }
            }
            return value;
        }
    }

    /**
     * Retrieve Student full time status from the grade level code.
     * For the reference code of the grade level, local code is Y/N
     * for K and PK grades indicating full time.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFullTimeStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String gradeCode = entity.getFieldValue(DOE_03_GRADE);
            String value = " ";
            SisStudent student = (SisStudent) entity.getBean();
            if ("91".equals(gradeCode) || "92".equals(gradeCode)) {
                // Is a K or PK grade level. Default "Y".
                value = "Y";
                String gradeLevel = student.getGradeLevel();
                if (m_gradeRefCodes != null) {
                    for (ReferenceCode code : m_gradeRefCodes) {
                        if (code.getCode().equals(gradeLevel)) {
                            if (code.getLocalCode() != null) {
                                // Get the local code.
                                value = code.getLocalCode();
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the gende code for the student.
     * Translate into report required values (1 or 2).
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGender implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            String gender = "0";
            if (value != null) {
                value = value.toUpperCase();
                if ("M".equals(value)) {
                    gender = "1";
                }
                if ("F".equals(value)) {
                    gender = "2";
                }
            }
            return gender;
        }
    }

    /**
     * Returns the grade level for the YOG in the given snapshot.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            EnrollmentSnapshot snapshot = ((EnrollmentEntity) entity).getSnapshot();
            SisStudent student = (SisStudent) entity.getBean();
            int schoolYear = data.getCurrentContext().getSchoolYear();

            String gradeLevel = student.getGradeLevel();
            if (snapshot.getYog() != student.getYog()) {
                List gradeLevels =
                        StudentManager.getMatchingGradeLevels(StudentManager.getMaxGradeLevel(getBroker()),
                                snapshot.getYog(), schoolYear, m_gradeLevelMap);

                gradeLevel = (String) gradeLevels.get(0);

                if (gradeLevels.size() > 1) {
                    entity.addRetrievalError(DOE_03_GRADE, new StateReportValidationError(entity, field,
                            "WARNING: Calculated grade level is not precise",
                            DOE_03_GRADE + "=" + STYLE_BOLD + gradeLevels.toString() + STYLE_END));
                }
            }

            return gradeLevel;
        }
    }

    /**
     * Retrieve membership days for a student.
     * This applies to one school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembershipDays implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            EnrollmentEntity membEnt = (EnrollmentEntity) entity;
            Integer param = (Integer) field.getParameter();
            float memberCount = 0;

            if (param != null && param.intValue() == 1) {
                // Get days in attendance count.
                memberCount = membEnt.getAttendance();
            } else if (param != null && param.intValue() == 2) {
                // Get membership days.
                memberCount = membEnt.getMembership();
            }

            /*
             * Export format is four digits with implied decimal. ( 23.5 ==> "0235" )
             * Multiply by ten to get the single decimal place above the decimal, and format as four
             * digit integer.
             */
            String membershipCountAsString = ((Enrollment) data).m_numberFormat.format(memberCount * 10.0);

            return membershipCountAsString;
        }
    }

    /**
     * Returns the school code for the given student.
     * <p>
     * Students can attend multiple schools in a year. Get the membership attendance record
     * for the current student instance to get the school
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            EnrollmentEntity attEnt = (EnrollmentEntity) entity;
            EnrollmentSnapshot snapshot = attEnt.getSnapshot();
            SisSchool school = snapshot.getSchool();
            String schoolCode = "XXXX";
            if (school != null) {
                // Left pad with zeros.
                schoolCode = "0000" + getProperty(school, m_schoolCodeField);
                schoolCode = schoolCode.substring(schoolCode.length() - 4);

            } else {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field,
                                "Could not find School Code", null));
            }

            return schoolCode;
        }
    }

    /**
     * Retrieve the student SSN.
     * If one is not available, generate one from the district ID and student local Id.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSSN implements FieldRetriever {

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (StringUtils.isEmpty(value)) {
                value = "000000" + ((SisStudent) entity.getBean()).getLocalId();
                value = "9" + m_leaId + value.substring(value.length() - 6);
            } else {
                value = value.replace("-", "").replace(" ", "");
            }
            return value;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * Trim the resulting string to the field maximum length.
     * For first and last names, and middle initial.
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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            Boolean upper = (Boolean) field.getParameter();
            // Strip illegal characters (punctuation).
            if (value != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(value);
                value = matcher.replaceAll("");

                // Map to upper case if required.
                if (upper != null && upper.booleanValue()) {
                    value = value.toUpperCase();
                }

                // Trim to valid field length.
                if (value.length() > field.getMaxLength()) {
                    value = value.substring(0, field.getMaxLength());
                }
            } else {
                value = "";
            }
            return value;
        }
    }

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "MSDE Enrollment Export";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Sets the value delimiter in the export.
     * The user parameter "Report format" determines whether to include delimiters (CSV)
     * or use column delimited.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueDelimiters() {
        boolean useDelims = false;
        if (m_reportFormat != null && m_reportFormat.intValue() == 1) {
            useDelims = true;
        }
        return useDelims;
    }

    /**
     * Sets the value wrapper in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueWrappers() {
        return false;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {

        // Load initialization data
        initializeFields();

        /*
         * Get core parameters
         */
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_reportFormat = (Integer) getParameter(REPORT_FORMAT_PARAM);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_numberFormat = new DecimalFormat(NUMBER_FORMAT);
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        // Load codes and support data from database.
        loadEnrollmentData();

        // Set the field definition array.
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(52);
        fieldDefinitions.add(getDOE01_leaid());
        fieldDefinitions.add(getDOE02_schoolid());
        fieldDefinitions.add(getDOE03_grade());
        fieldDefinitions.add(getDOE04_studentfullTimeStatus());
        fieldDefinitions.add(getDOE05_studentLocalId());
        fieldDefinitions.add(getDOE06_ssn());
        fieldDefinitions.add(getDOE07_lastName());
        fieldDefinitions.add(getDOE08_firstName());
        fieldDefinitions.add(getDOE09_middleName());
        fieldDefinitions.add(getDOE10_birthDate());
        fieldDefinitions.add(getDOE11_race());
        fieldDefinitions.add(getDOE12_filler());
        fieldDefinitions.add(getDOE13_gender());
        fieldDefinitions.add(getDOE14_entryStatus());
        fieldDefinitions.add(getDOE15_entryCode());
        fieldDefinitions.add(getDOE16_entryDate());
        fieldDefinitions.add(getDOE17_withdrawalStatus());
        fieldDefinitions.add(getDOE18_withdrawalCode());
        fieldDefinitions.add(getDOE19_withdrawalDate());
        fieldDefinitions.add(getDOE20_attendance());
        fieldDefinitions.add(getDOE21_member());
        fieldDefinitions.add(getDOE22_stateAid());
        fieldDefinitions.add(getDOE23_evening());
        fieldDefinitions.add(getDOE24_eveningCourseCount());
        fieldDefinitions.add(getDOE25_partTime());
        fieldDefinitions.add(getDOE26_partTimeCourseCount());
        fieldDefinitions.add(getDOE27_studentStateId());
        setFieldDefinitions(fieldDefinitions);
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
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
            setEntityClass(EnrollmentEntity.class);

            // Load attendance maps.
            loadAbsenceDaysMaps(studentCriteria);
        }
    }

    /**
     * Build Field definition for DOE 01, district identifier (LEA).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE01_leaid() {
        String districtIdField = translateAliasToJavaName(DOE_01_LEA_ID, true);
        m_leaId = (String) getOrganization().getFieldValueByBeanPath(districtIdField);
        FieldDefinition field = new FieldDefinition(DOE_01_LEA_ID,
                LABEL_PREFIX_CHAR + DOE_01_LEA_ID,
                m_leaId, 0, 2, 2, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 02, school identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE02_schoolid() {
        FieldDefinition field = new FieldDefinition(DOE_02_SCHOOL_ID,
                SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_schoolCodeField,
                null, 0, 4, 4, REGEX_NUMERIC,
                null, new RetrieveSchoolCode(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 03, student grade.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE03_grade() {
        FieldDefinition field = new FieldDefinition(DOE_03_GRADE, translateAliasToJavaName(DOE_03_GRADE, true),
                null, 1, 2, 2, "0[123456789]|1[012]|9[123456]", null,
                new RetrieveGradeLevel(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 04, Student full time status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE04_studentfullTimeStatus() {
        FieldDefinition field = new FieldDefinition(DOE_04_FULL_TIME,
                translateAliasToJavaName(DOE_04_FULL_TIME, false),
                null, 0, 1, 1, "[YN ]",
                null, new RetrieveFullTimeStatus(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 05, LASID Student local identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE05_studentLocalId() {
        FieldDefinition field = new FieldDefinition(DOE_05_STUDENT_LOCAL_ID,
                SisStudent.COL_LOCAL_ID,
                null, 0, 1, 10, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 06, student gender.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE06_ssn() {
        FieldDefinition field = new FieldDefinition(DOE_06_SSN,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + translateAliasToJavaName(DOE_06_SSN, true),
                null, 0, 9, 9, REGEX_NUMERIC,
                null, new RetrieveSSN(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 07, student last name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE07_lastName() {
        FieldDefinition field = new FieldDefinition(DOE_07_LAST_NAME,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, 0, 1, 25, null, null,
                new RetrieveStripNameChar(),
                null, Boolean.FALSE);
        return field;
    }

    /**
     * Build Field definition for DOE 08, student first name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE08_firstName() {
        FieldDefinition field = new FieldDefinition(DOE_08_FIRST_NAME,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, 0, 1, 15, null, null,
                new RetrieveStripNameChar(),
                null, Boolean.FALSE);
        return field;
    }

    /**
     * Build Field definition for DOE 09, student middle initial.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE09_middleName() {
        FieldDefinition field = new FieldDefinition(DOE_09_MIDDLE_NAME,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME,
                null, 0, 0, 1, null, null,
                new RetrieveStripNameChar(), null, Boolean.TRUE);
        return field;
    }

    /**
     * Build Field definition for DOE 10, student birth date.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE10_birthDate() {
        FieldDefinition field = new FieldDefinition(DOE_10_BIRTH_DATE,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB,
                null, 0, 8, 8, null, m_dateFormat, null,
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 11, student race code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE11_race() {
        FieldDefinition field = new FieldDefinition(DOE_11_RACE,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + translateAliasToJavaName(DOE_11_RACE, true),
                null, 1,
                1, 1, "[12345]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 12, filler.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE12_filler() {
        String reserved = LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(RESERVED);
        FieldDefinition field = new FieldDefinition(DOE_12_FILLER,
                LABEL_PREFIX_CHAR + reserved,
                "    ", 0, 4, 4, "    ",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 13, student gender.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE13_gender() {
        FieldDefinition field = new FieldDefinition(DOE_13_GENDER,
                SisStudent.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_GENDER_CODE,
                null, 0, 1, 1, "[12]",
                null, new RetrieveGender(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 14, Entry Status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE14_entryStatus() {
        FieldDefinition field = new FieldDefinition(DOE_14_ENTRY_STATUS, LABEL_PREFIX_CHAR + DOE_14_ENTRY_STATUS,
                "", 0, 1, 1, "[ENR]", null,
                new RetrieveEnrollmentStatus(),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 15, Entry Code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE15_entryCode() {
        FieldDefinition field = new FieldDefinition(DOE_15_ENTRY_CODE, LABEL_PREFIX_CHAR + DOE_15_ENTRY_CODE,
                "00", 0, 2, 2, "0[126789]|1[0345678]|2[124567]", null,
                new RetrieveEnrollmentStatus(),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 16, Entry date.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE16_entryDate() {
        FieldDefinition field = new FieldDefinition(DOE_16_ENTRY_DATE, LABEL_PREFIX_CHAR + DOE_16_ENTRY_DATE,
                null, 0, 8, 8, REGEX_NUMERIC, m_dateFormat,
                new RetrieveEnrollmentStatus(),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 17, Withdraw Status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE17_withdrawalStatus() {
        FieldDefinition field = new FieldDefinition(DOE_17_WITHDRAW_STATUS, LABEL_PREFIX_CHAR + DOE_17_WITHDRAW_STATUS,
                " ", 0, 1, 1, "[T ]", null,
                new RetrieveEnrollmentStatus(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 18, Withdraw Code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE18_withdrawalCode() {
        FieldDefinition field = new FieldDefinition(DOE_18_WITHDRAW_CODE, LABEL_PREFIX_CHAR + DOE_18_WITHDRAW_CODE,
                "  ", 0, 2, 2, "  |10|2[156]", null,
                new RetrieveEnrollmentStatus(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 19, Withdraw date.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE19_withdrawalDate() {
        FieldDefinition field = new FieldDefinition(DOE_19_WITHDRAW_DATE, LABEL_PREFIX_CHAR + DOE_19_WITHDRAW_DATE,
                "        ", 0, 8, 8, "        |" + REGEX_NUMERIC, m_dateFormat,
                new RetrieveEnrollmentStatus(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 20, student attendance days.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE20_attendance() {
        FieldDefinition field = new FieldDefinition(DOE_20_ATTENDANCE_DAYS, LABEL_PREFIX_CHAR + DOE_20_ATTENDANCE_DAYS,
                null, 0, 4, 4, REGEX_NUMERIC, null,
                new RetrieveMembershipDays(),
                null, Integer.valueOf(1));
        return field;
    }

    /**
     * Build Field definition for DOE 21, student member days.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE21_member() {
        FieldDefinition field = new FieldDefinition(DOE_21_MEMBER_DAYS, LABEL_PREFIX_CHAR + DOE_21_MEMBER_DAYS,
                null, 0, 4, 4, REGEX_NUMERIC, null,
                new RetrieveMembershipDays(),
                null, Integer.valueOf(2));
        return field;
    }


    /**
     * Build Field definition for DOE 22, Eligibility for state aid.
     * This will include the
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE22_stateAid() {
        FieldDefinition field = new FieldDefinition(DOE_22_STATE_AID, translateAliasToJavaName(DOE_22_STATE_AID, true),
                "01", 1, 2, 2, "0[12345678]",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 23, evening student status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE23_evening() {
        FieldDefinition field =
                new FieldDefinition(DOE_23_EVENING_STUDENT, translateAliasToJavaName(DOE_23_EVENING_STUDENT, true),
                        null, 0, 1, 1, "[YN]",
                        null, new RetrieveBooleanAsYN(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 24, evening course count.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE24_eveningCourseCount() {
        FieldDefinition field =
                new FieldDefinition(DOE_24_EVENING_COURSES, translateAliasToJavaName(DOE_24_EVENING_COURSES, true),
                        null, 0, 0, 1, "|[1234]",
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 25, part time student status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE25_partTime() {
        FieldDefinition field =
                new FieldDefinition(DOE_25_PT_STUDENT, translateAliasToJavaName(DOE_25_PT_STUDENT, true),
                        null, 0, 1, 1, "[YN]",
                        null, new RetrieveBooleanAsYN(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 26, part time course count.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE26_partTimeCourseCount() {
        FieldDefinition field =
                new FieldDefinition(DOE_26_PT_COURSES, translateAliasToJavaName(DOE_26_PT_COURSES, true),
                        null, 0, 0, 1, "|[1234]",
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 27, SASID Student state identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE27_studentStateId() {
        FieldDefinition field = new FieldDefinition(DOE_27_STATE_ID, SisStudent.COL_STATE_ID,
                null, 0, 1, 10, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    protected Set getCalendarDays(SisSchool school, String calendar) {
        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            PlainDate startDate = school.getActiveSchedule().getStartDate();
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
            m_schoolsToCalendars.put(school.getOid(), calendarData);
        }

        return (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

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
         * Who should be included? Primary students and students with enrollment activity within the
         * school year.
         *
         * The export is being run for either (A) the entire district or (B) a single school
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case A:
         *
         * Students in an active, non-archived school in the district
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case B:
         *
         * Students with enrollment activity (E,W) within the year.
         *
         */

        /*
         * Primary students
         */
        // Select students with primary school, or students with
        // enrollment activity (E,W) in the school this year.
        Criteria enrollCriteria = new Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, StudentEnrollment.WITHDRAWAL);
        Criteria enrollCriteria2 = new Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_CODE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        // With Enrollment records within the active date range.
        Criteria activityCriteria = new Criteria();
        PlainDate startDate = getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(
                    SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }
        primaryCriteria.addOrCriteria(enrollCriteria);

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            userCriteria.addEqualTo(m_doeStatusField, DOE_STATUS_FIELD_REPORT_CODE);
        }

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
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
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD, true);
        m_schoolCodeField = translateAliasToJavaName(DOE_02_SCHOOL_ID, true);

    }

    /**
     * Loads a map by student of absence days for that student.
     * Loads school membership day counts for all schools.
     *
     * @param studentCriteria Criteria
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                new String[] {StudentAttendance.COL_STUDENT_OID, "SUM(ATT_PORTION_ABSENT)"}, criteria);
        reportQuery.addGroupBy(StudentAttendance.COL_STUDENT_OID);

        // Build the map of student to absences.
        m_absences = new HashMap<String, Float>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                Float absencesCount = Float.valueOf(row[1].toString());
                m_absences.put(studentOid, absencesCount);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Loads the enrollment data required by this export.
     */
    private void loadEnrollmentData() {
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field =
                dictionary.findDataDictionaryField(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);
        if (field != null) {
            ReferenceTable gradeTbl = field.getReferenceTable();
            if (gradeTbl != null) {
                m_gradeRefCodes = gradeTbl.getReferenceCodes(getBroker());
            }
        }
    }
}
