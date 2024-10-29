/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
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
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Connecticut PSIS State Report
 * The PSIS state report is run 3 times a year, October, January, and June (end of school).
 * The report captures students that are active on the date the report is run.
 *
 *
 * @author X2 Development Corporation
 */
public class Psis extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the PSIS Export.
     *
     * @author X2 Development Corporation
     */
    public static class PsisEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        Psis m_psisData = null;
        EnrollmentSnapshot m_snapshot = null;
        int m_membershipDays;
        int m_attendanceDays;

        /*
         * Placeholders for calculated unmapped fields. These can be written
         * back to the database in postProcess if update flag is set.
         * Also, holds some calculated values that have been overridden with
         * default or related values.
         *
         * Map key should be field alias constant.
         */
        Map<String, Object> m_updateValues = null;

        /**
         * Instantiates a new psis entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public PsisEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment membership count and membership days parameter
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            if (m_psisData.m_requireFacility != null && m_psisData.m_requireFacility.booleanValue()) {
                // Check the student outplaced or sped school. If there is no state code then
                // exclude the student.
                SisStudent student = (SisStudent) getBean();
                SisSchool school = student.getSchool();
                String temp = (String) school.getFieldValueByBeanPath(this.m_psisData.m_fieldSklOutpl);
                if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                    String schoolId = (String) student.getFieldValueByBeanPath(this.m_psisData.m_fieldStdOutplFacility);
                    if (!StringUtils.isEmpty(schoolId)) {
                        String stateSchoolId = this.m_psisData.lookupStateValue(SisStudent.class,
                                this.m_psisData.m_fieldStdOutplFacility, schoolId);
                        if (StringUtils.isEmpty(stateSchoolId)) {
                            error = new StateReportValidationError(this,
                                    getData().getFieldDefinition(PSIS_5_FACILITY_1),
                                    "Outplaced Facility has no state code", schoolId);
                        }
                    } else {
                        error = new StateReportValidationError(this, getData().getFieldDefinition(PSIS_5_FACILITY_1),
                                "No Outplaced Facility for student", student.getLocalId());
                    }
                }

                temp = (String) school.getFieldValueByBeanPath(this.m_psisData.m_fieldSklSped);
                if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                    String schoolId = (String) student.getFieldValueByBeanPath(this.m_psisData.m_fieldStdSpedFacility);
                    if (!StringUtils.isEmpty(schoolId)) {
                        String stateSchoolId = this.m_psisData.lookupStateValue(SisStudent.class,
                                this.m_psisData.m_fieldStdSpedFacility, schoolId);
                        if (StringUtils.isEmpty(stateSchoolId)) {
                            error = new StateReportValidationError(this,
                                    getData().getFieldDefinition(PSIS_5_FACILITY_1),
                                    "Special Ed Facility has no state code", schoolId);
                        }
                    } else {
                        error = new StateReportValidationError(this, getData().getFieldDefinition(PSIS_5_FACILITY_1),
                                "No Special Ed Facility was found for student", student.getLocalId());
                    }
                }
            }

            if (m_psisData.m_requireMemberDay != null) {
                // Get membership days parameter.
                int membershipCountAsInt = 0;

                // Get enrollment count (DOE 29). (Force calculation of membership days and storage
                // of count).
                String membershipCount = getFieldValue(PSIS_29_MEMBERSHIP_DAYS);

                // Get saved value before 555 change up.
                membershipCount = getUpdateValue(PSIS_29_MEMBERSHIP_DAYS).toString();

                if (membershipCount != null) {
                    try {
                        membershipCountAsInt = Integer.parseInt(membershipCount);
                    } catch (NumberFormatException nfe) {
                        // invlaid format, will be reported elsewhere.
                    }
                }

                // check enrollment count and membership days parameter.
                if ((m_psisData.m_requireMemberDay.booleanValue() && membershipCountAsInt > 0)
                        || !m_psisData.m_requireMemberDay.booleanValue()) {
                    // No filtering.
                } else {
                    // Student filtered.
                    error = new StateReportValidationError(this, getData().getFieldDefinition(PSIS_29_MEMBERSHIP_DAYS),
                            "0 member days - excluded from export", "");
                }
            }

            return error;
        }

        /**
         * Returns a field value saved before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @return Object
         */
        public Object getUpdateValue(String doeId) {
            Object value = null;
            if (m_updateValues != null) {
                value = m_updateValues.get(doeId);
            }
            return value;
        }

        /**
         * Returns the display name of the represented entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();

            SisStudent student = (SisStudent) getBean();

            buffer.append(student.getNameView());
            buffer.append(" [");
            buffer.append(student.getLocalId());
            buffer.append("]");

            return buffer.toString();
        }

        /**
         * Gets the membership days.
         *
         * @return int
         */
        public int getMembershipDays() {
            return m_membershipDays;
        }

        /**
         * Gets the attendance days.
         *
         * @return int
         */
        public int getAttendanceDays() {
            return m_attendanceDays;
        }

        /**
         * Sets the attendance days.
         *
         * @param attendanceDays void
         */
        protected void setAttendanceDays(int attendanceDays) {
            m_attendanceDays = attendanceDays;
        }

        /**
         * Sets the membership days.
         *
         * @param membershipDays void
         */
        protected void setMembershipDays(int membershipDays) {
            m_membershipDays = membershipDays;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @param reportDate PlainDate
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot(PlainDate reportDate) {
            if (m_snapshot == null) {
                m_snapshot = getSnapshot((SisStudent) getBean(), reportDate);
            }
            return m_snapshot;
        }

        /**
         * Initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            SisStudent student = (SisStudent) bean;
            m_psisData = (Psis) data;
            boolean activeStudent = false;

            List<StudentEnrollmentSpan> spans =
                    new ArrayList(m_psisData.m_helper.getStudentEnrollmentSpans(student, false));
            if (!spans.isEmpty()) {
                // remove prior overlapping spans caused by lack of withdrawal records
                Collections.reverse(spans);
                Iterator<StudentEnrollmentSpan> iterator = spans.iterator();
                StudentEnrollmentSpan firstSpan = iterator.next();
                while (firstSpan.getFirstActiveDate() == null && iterator.hasNext()) {
                    iterator.remove();
                    firstSpan = iterator.next();
                }
                while (iterator.hasNext()) {
                    StudentEnrollmentSpan span = iterator.next();
                    // Only most recent span can have null last active date
                    if (span.getLastActiveDate() == null) {
                        iterator.remove();
                        continue;
                    }
                    // Spans with no active dates should be skipped
                    if (span.getFirstActiveDate() == null && iterator.hasNext()) {
                        iterator.remove();
                        continue;
                    }

                    if (span.getLastActiveDate().after(firstSpan.getFirstActiveDate())) {
                        iterator.remove();
                        continue;
                    }
                    firstSpan = span;
                }
            }

            Set<PlainDate> memberDaysSet = new HashSet();
            for (StudentEnrollmentSpan span : spans) {
                if (span.getFirstActiveEnrollment() != null) {
                    addMemberDays(memberDaysSet, student, span);
                    if (!m_psisData.m_reportDate.before(span.getFirstActiveDate()) &&
                            (span.getLastActiveDate() == null
                                    || !m_psisData.m_reportDate.after(span.getLastActiveDate()))) {
                        activeStudent = true;
                    }
                }
            }
            if (activeStudent) {
                Float absences = Float.valueOf(0f);
                Object absencesObj = m_psisData.m_absences.get(student.getOid());
                if (absencesObj != null) {
                    absences = (Float) absencesObj;
                }
                int attendanceDays = Float.valueOf(memberDaysSet.size() - absences.floatValue()).intValue();
                setAttendanceDays(attendanceDays);
                setMembershipDays(memberDaysSet.size());
            } else {
                setRowCount(0);
                return;
            }
        }

        /**
         * If update calculated fields is set, save new values into the bean.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            Boolean updateRecords = (Boolean) getData().getParameter(UPDATE_RECORDS_PARAM);

            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (updateRecords != null ? updateRecords.booleanValue() : false) {
                try {
                    Converter integerConverter = ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER,
                            Locale.getDefault(), true);

                    FieldDefinition field = getData().getFieldDefinition(PSIS_29_MEMBERSHIP_DAYS);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(PSIS_29_MEMBERSHIP_DAYS)));

                    field = getData().getFieldDefinition(PSIS_30_MEMBERSHIP_DAYS);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(PSIS_30_MEMBERSHIP_DAYS)));

                    if (getBean().isDirty()) {
                        getData().getBroker().saveBeanForced(getBean());
                    }
                } catch (IllegalAccessException e) {
                    // conversion errors. Cannot save student.
                } catch (InvocationTargetException e) {
                    // conversion errors. Cannot save student.
                } catch (NoSuchMethodException e) {
                    // conversion errors. Cannot save student.
                }
            }
        }

        /**
         * Sets a field value before mapping.
         *
         * Certain calculated data fields (Sped valued) can be stored
         * and retrieved before reference code mapping by DOE field constant.
         *
         * @param doeId String
         * @param value Object
         */
        public void setUpdateValue(String doeId, Object value) {
            if (m_updateValues == null) {
                m_updateValues = new HashMap<String, Object>();
            }

            m_updateValues.put(doeId, value);
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
         * Adds the member days.
         *
         * @param days Set<PlainDate>
         * @param student SisStudent
         * @param span StudentEnrollmentSpan
         */
        private void addMemberDays(Set<PlainDate> days, SisStudent student, StudentEnrollmentSpan span) {
            Set<PlainDate> insessionDates =
                    m_psisData.m_helper.getCalendarDays(span.getSchool(), student.getCalendarCode());
            if (insessionDates == null
                    && !StudentEnrollmentSpan.DEFAULT_CALENDAR_ID.equals(student.getCalendarCode())) {
                insessionDates = m_psisData.m_helper.getCalendarDays(span.getSchool(),
                        StudentEnrollmentSpan.DEFAULT_CALENDAR_ID);
            }
            if (insessionDates == null) {
                insessionDates =
                        m_psisData.m_helper.getCalendarDays(span.getSchool(), StudentHistoryHelper.CALENDAR_ANY);
            }

            // Count in session days between (and including) first and last active dates.
            PlainDate endDate = span.getLastActiveDate();
            if (endDate == null) {
                endDate = m_psisData.m_reportDate;
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (span.getFirstActiveDate() != null && !date.before(span.getFirstActiveDate())
                            && !date.after(endDate)) {
                        days.add(date);
                    }
                }
            }
        }

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate) {
            return new EnrollmentSnapshot(student, reportDate, getData().getBroker());
        }
    }

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the "require facility code" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_FACILITY_PARAM = "requireFacility";

    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";

    /**
     * DOE ALIASES
     */
    private static final String PSIS_5_FACILITY_1 = "FACILITY_CODE1";
    private static final String PSIS_29_MEMBERSHIP_DAYS = "MEMBERSHIP_DAYS";
    private static final String PSIS_30_MEMBERSHIP_DAYS = "ATTENDANCE_DAYS";

    /**
     * Code values for fields (RetrieveGiftedandTalented).
     */
    private static final String NOT_GIFTED_OR_TALENTED = "01";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "update student records" parameter. The value is a Boolean.
     */
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the include membership and attendance days parameter. The Membership and Attendance
     * fields are optional for the October and January collection period, but required in June.
     * Some districts can optional provide this information in October and January and others
     * (Greenwich)
     * do not want to provide this info on the October and January collection period.
     *
     */
    public static final String INCLUDE_MEMBERSHIP_AND_ATTENDANCE_DAYS_PARAM = "includeMembershipandAttendanceDays";

    /**
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

    private static final String GIFTED_AND_TALENTED_PROGRAM = "ALP";
    private static final String ENGLISH_LANGUAGE_LERNER_PROGRAM = "ESL";
    private static final String IMMIGRANT_STATUS_PROGRAM = "IMMIGRANT";
    private static final String MILITARY_FAMILY_PROGRAM = "MILITARY";

    // PSIS aliases
    public static final String ELL_PROGRAM_ALIAS = "PSIS15";
    public static final String RESIDENT_TOWN_ALIAS = "PSIS04";
    public static final String GIFTED_TALENTED_ALIAS = "PSIS27";
    public static final String SPED_CODE_ALIAS = "PSIS16";
    public static final String STD_PREK_STATUS_ALIAS = "PSIS20";

    // on student
    public static final String NEXUS_ALIAS = "NEXUS";

    // on student, the facility a student is placed at
    public static final String OUTSIDE_PLACEMENT_FACILITY_ALIAS = "Outside Placement Facility";
    public static final String SPECIAL_EDUCATION_FACILITY_ALIAS = "Special Education Facility";

    // on school
    public static final String OUTSIDE_PLACEMENT_SCHOOL_ALIAS = "Outplacement School";
    public static final String SPECIAL_EDUCATION_SCHOOL_ALIAS = "Special Education School";
    public static final String SCHOOL_STATE_ID_ALIAS = "StateId";

    // on student program participation
    public static final String ELL_CODE_ALIAS = "ELL_CODE";

    // other constants
    private static final String REFTABLE_OID_ATTENDANCE_REASONS = "rtbAttReason";
    private static final String CODE_ATTENDANCE_PRESENT = "Present";
    private static final String PREK_GRADE_1 = "PK";
    private static final String PREK_GRADE_2 = "P3";


    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Float> m_absences;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_fieldSklStateId;
    protected String m_fieldStdEllProgram;
    protected String m_fieldStdGifTal;
    protected String m_fieldStdNexus;
    protected String m_fieldStdOutplFacility;
    protected String m_fieldStdSped;
    protected String m_fieldStdSpedFacility;
    protected PlainDate m_firstDayDate;
    protected Set m_firstDayMembers;
    protected Pattern m_illegalNameCharacters;
    protected PlainDate m_reportDate;
    protected Boolean m_includeMembershipAndAttendanceDays;
    protected Boolean m_requireMemberDay;
    protected Boolean m_requireFacility;
    protected HashMap m_schoolsToCalendars;

    /**
     * Maps
     */
    protected Map<String, ReferenceCode> m_gradeLevelCodes;
    protected Map<String, Map<String, StateReportValidationError>> m_pgmsErrorsMap =
            new HashMap<String, Map<String, StateReportValidationError>>();

    protected StudentHistoryHelper m_helper;

    protected String m_fieldSklOutpl;

    protected String m_fieldSklSped;

    protected String m_fieldStdPrekStatus;



    /**
     * Retriever for a student's days in attendance. Assumes values are already loaded into the
     * absence map.
     */
    protected class RetrieveAttendanceDays implements FieldRetriever {
        private static final String CALC_ID = "PSIS-ATTENDANCE-DAYS";

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
            PsisEntity psisEntity = (PsisEntity) entity;
            int attendanceDays = psisEntity.getAttendanceDays();
            psisEntity.setUpdateValue(field.getFieldId(), Integer.valueOf(attendanceDays));

            Object attendance = null;
            if (m_includeMembershipAndAttendanceDays.booleanValue()) {
                attendance = Integer.valueOf(attendanceDays);
            } else {
                attendance = "";
            }

            return attendance;
        }
    }

    /**
     * Retrieve if student has Ell program.
     */
    protected class RetrieveEnglishLanguageLearner extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-EL";
        private static final String ELL_ALIAS = "PSIS14";
        private static final String NO = "N";
        private static final String YES = "Y";
        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        protected String m_fieldStdEll;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve english language learner.
         */
        public RetrieveEnglishLanguageLearner() {
            super();

            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ELL_ALIAS);
            if (dictionaryField != null) {
                m_fieldStdEll = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, ENGLISH_LANGUAGE_LERNER_PROGRAM);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            Object item = getPropertyAsJavaType(entity.getBean(), m_fieldStdEll);
            String value = NO;
            if (item != null) {
                if (item instanceof Boolean) {
                    if (((Boolean) item).booleanValue()) {
                        value = YES;
                    }
                } else if (item instanceof String) {
                    String strValue = (String) item;
                    if (m_hasRefTable) {
                        strValue = lookupStateValue(SisStudent.class, m_fieldStdEll, strValue);
                    }
                    if (YES_VALUES.contains(strValue)) {
                        value = YES;
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /**
     * Retrieves ELL program state code.
     */
    protected class RetrieveEnglishLanguageLearnerProgramCode extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-EL-PROG";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, ENGLISH_LANGUAGE_LERNER_PROGRAM);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) {
            SisStudent student = (SisStudent) entity.getBean();

            String ellPgmValue = StringUtils.isEmpty(m_fieldStdEllProgram) ? null
                    : (String) student.getFieldValueByBeanPath(m_fieldStdEllProgram);
            String value = null;

            if (!StringUtils.isEmpty(ellPgmValue)) {
                value = lookupStateValue(SisStudent.class, m_fieldStdEllProgram, ellPgmValue);
            }

            return value != null ? value : "";
        }
    }

    /**
     * Inner class implementing a field retriever for gifted and talented.
     */
    protected class RetrieveGiftedandTalented extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-GIFTED-TALENTED";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, GIFTED_AND_TALENTED_PROGRAM);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) {
            SisStudent student = (SisStudent) entity.getBean();

            String giftValue = StringUtils.isEmpty(m_fieldStdGifTal) ? null
                    : (String) student.getFieldValueByBeanPath(m_fieldStdGifTal);
            String value = null;

            if (!StringUtils.isEmpty(giftValue)) {
                value = lookupStateValue(SisStudent.class, m_fieldStdGifTal, giftValue);
            }

            return value != null ? value : NOT_GIFTED_OR_TALENTED;
        }
    }

    /**
     * The Class RetrieveImmigrantStatus.
     */
    protected class RetrieveImmigrantStatus extends RetrieveProgramCode {
        private static final String ALIAS_IMMIGRANT_STATUS = "all-std-ImmigrantStatus";
        private static final String CALC_ID = "PSIS-IMMIGRANT";
        private static final String NO = "N";
        private static final String YES = "Y";

        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        private String m_fieldImmigrantStatus;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve immigrant status.
         */
        public RetrieveImmigrantStatus() {
            super();

            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_IMMIGRANT_STATUS);
            if (dictionaryField != null) {
                m_fieldImmigrantStatus = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, IMMIGRANT_STATUS_PROGRAM);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            String value = NO;
            if (!StringUtils.isEmpty(m_fieldImmigrantStatus)) {
                Object item = getPropertyAsJavaType(entity.getBean(), m_fieldImmigrantStatus);
                if (item != null) {
                    if (item instanceof Boolean) {
                        if (((Boolean) item).booleanValue()) {
                            value = YES;
                        }
                    } else if (item instanceof String) {
                        String strValue = (String) item;
                        if (m_hasRefTable) {
                            strValue = lookupStateValue(SisStudent.class, m_fieldImmigrantStatus, strValue);
                        }
                        if (YES_VALUES.contains(strValue)) {
                            value = YES;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /**
     * Retriever for a student's days in membership. Uses the EnrollmentManager for all logic.
     */
    protected class RetrieveMembershipDays implements FieldRetriever {
        private static final String CALC_ID = "PSIS-MEMBERSHIP-DAYS";

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
            PsisEntity psisEntity = (PsisEntity) entity;
            int membershipCount = psisEntity.getMembershipDays();
            psisEntity.setUpdateValue(field.getFieldId(), Integer.valueOf(membershipCount));
            Object membershipDays = null;

            if (m_includeMembershipAndAttendanceDays.booleanValue()) {
                membershipDays = Integer.valueOf(membershipCount);
            } else {
                membershipDays = "";
            }
            return membershipDays;
        }
    }

    /**
     * The Class RetrieveMilitaryFamily.
     */
    protected class RetrieveMilitaryFamily extends RetrieveProgramCode {
        private static final String ALIAS_MILITARY_FAMILY = "all-std-MilitaryFamily";
        private static final String CALC_ID = "PSIS-MILITARY";
        private static final String NO = "N";
        private static final String YES = "Y";

        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        private String m_fieldMilitaryFamily;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve military family.
         */
        public RetrieveMilitaryFamily() {
            super();

            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MILITARY_FAMILY);
            if (dictionaryField != null) {
                m_fieldMilitaryFamily = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, MILITARY_FAMILY_PROGRAM);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            String value = NO;
            if (!StringUtils.isEmpty(m_fieldMilitaryFamily)) {
                Object item = getPropertyAsJavaType(entity.getBean(), m_fieldMilitaryFamily);
                if (item != null) {
                    if (item instanceof Boolean) {
                        if (((Boolean) item).booleanValue()) {
                            value = YES;
                        }
                    } else if (item instanceof String) {
                        String strValue = (String) item;
                        if (m_hasRefTable) {
                            strValue = lookupStateValue(SisStudent.class, m_fieldMilitaryFamily, strValue);
                        }
                        if (YES_VALUES.contains(strValue)) {
                            value = YES;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /***
     * For Greenwich, nexus district is a student is SPED or a value is entered in a nexus field.
     * If there is not value in the nexus field for a SPED value, use the district code.
     * The parameter passed in is the default nexus code for students that do not have one set.
     */
    protected class RetrieveNexus implements FieldRetriever {
        private static final String CALC_ID = "PSIS-NEXUS";

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
            String nexus = null;

            SisStudent student = (SisStudent) entity.getBean();
            String defaultNexusDistrict = student.getSchool().getOrganization1().getId();

            String sped = (String) student.getFieldValueByBeanPath(m_fieldStdSped);
            if (!StringUtils.isEmpty(sped)) {
                if ("Y".equals(lookupStateValue(SisStudent.class, m_fieldStdSped, sped))) {
                    nexus = defaultNexusDistrict;
                }
            }

            String nexusFieldValue = (String) student.getFieldValueByBeanPath(m_fieldStdNexus);
            if (!StringUtils.isEmpty(nexusFieldValue)) {
                String stateNexusCode = lookupStateValue(SisStudent.class, m_fieldStdNexus, nexusFieldValue);
                if (!StringUtils.isEmpty(stateNexusCode)) {
                    nexus = stateNexusCode;
                }
            }

            return nexus;
        }
    }

    /**
     * The Class RetrievePrekNumericValue.
     */
    protected class RetrievePrekNumericValue implements FieldRetriever {
        private static final String CAL_ID = "PSIS-PREK-NUMERIC";
        private static final String FIELD_GRADE_CODE = "GRADE_CODE";

        private final List<String> REPORTABLE_GRADES = Arrays.asList("P3", "PK");

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
            Object value = null;
            String gradeCode = entity.getFieldValue(FIELD_GRADE_CODE);
            if (REPORTABLE_GRADES.contains(gradeCode)) {
                SisStudent student = (SisStudent) entity.getBean();
                String beanPath = data.translateAliasToJavaName((String) field.getParameter(), false);
                value = data.getPropertyAsJavaType(student, beanPath);
            }
            return value;
        }

    }

    /**
     * Retrieve if a student is in PREK program.
     */
    protected class RetrievePrekProgramStatus implements FieldRetriever {
        private static final String CAL_ID = "PSIS-PREK-PROGRAM";

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
            SisStudent student = (SisStudent) entity.getBean();
            String gradeLevel = student.getGradeLevel();
            String preKProgramStatus = null;

            boolean setPreKProgramStatus = false;
            if (PREK_GRADE_1.equals(gradeLevel) || PREK_GRADE_2.equals(gradeLevel)) {
                setPreKProgramStatus = true;
            }

            if (setPreKProgramStatus) {
                if (m_gradeLevelCodes.containsKey(gradeLevel)) {
                    ReferenceCode preKReferenceCode = m_gradeLevelCodes.get(gradeLevel);
                    String programStatus = preKReferenceCode.getFederalCode();
                    if (!StringUtils.isEmpty(programStatus)) {
                        preKProgramStatus = programStatus;
                    }
                }
            }

            if (StringUtils.isEmpty(preKProgramStatus)) {
                String status = StringUtils.isEmpty(m_fieldStdPrekStatus) ? null
                        : (String) student.getFieldValueByBeanPath(m_fieldStdPrekStatus);

                if (!StringUtils.isEmpty(status)) {
                    status = lookupStateValue(SisStudent.class, m_fieldStdPrekStatus, status);
                }

                preKProgramStatus = !StringUtils.isEmpty(status) ? status : "00";
            }

            return preKProgramStatus;
        }
    }

    /**
     * The rule for the getValue function is first test if the student is contained in the set of
     * programs.
     * If they are, return the proper value from the program. If they are not, return the value
     * based on the student alias.
     *
     * @author Follett Software Company
     */
    protected abstract class RetrieveProgramCode implements FieldRetriever {
        protected X2Criteria m_pgmCriteria;
        protected Map<String, List<StudentProgramParticipation>> m_stdPgmMap;

        /**
         * Instantiates a new retrieve program code.
         */
        public RetrieveProgramCode() {
            applyCriteria();
            initPgmMap();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData data, StateReportEntity entity, FieldDefinition field) throws
         *      X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdPgmMap.containsKey(student.getOid())) {
                if (isIndicator()) {
                    value = "Y";
                } else {
                    List<StudentProgramParticipation> pgms = m_stdPgmMap.get(student.getOid());
                    StudentProgramParticipation programParticipation = pgms.get(0);
                    String programCode = programParticipation.getProgramCode();

                    if (pgms.size() > 1) {
                        if (!m_pgmsErrorsMap.containsKey(student.getOid())) {
                            Map<String, StateReportValidationError> errorMap =
                                    new HashMap<String, StateReportValidationError>();
                            errorMap.put(programCode, new StateReportValidationError(entity, field,
                                    "Student has more that one active programs with code = " + programCode,
                                    "LASID=" + STYLE_BOLD + student.getLocalId() + STYLE_END));
                            m_pgmsErrorsMap.put(student.getOid(), errorMap);
                        } else {
                            Map<String, StateReportValidationError> errorMap = m_pgmsErrorsMap.get(student.getOid());
                            errorMap.put(programCode, new StateReportValidationError(entity, field,
                                    "Student has more that one active programs with code = " + programCode,
                                    "LASID=" + STYLE_BOLD + student.getLocalId() + STYLE_END));
                        }
                    }

                    if (!StringUtils.isEmpty(programCode)) {
                        value = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
                    }
                }
            } else {
                value = getCustomValue(entity);
            }

            return value;
        }

        /**
         * Adds the equal to pgm field.
         *
         * @param beanPath String
         * @param value String
         */
        protected void addEqualToPgmField(String beanPath, String value) {
            if (m_pgmCriteria == null) {
                m_pgmCriteria = new X2Criteria();

                m_pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
                X2Criteria emptyEndDateCriteria = new X2Criteria();
                emptyEndDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE,
                        getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(emptyEndDateCriteria);

                m_pgmCriteria.addAndCriteria(endDateCriteria);
            }

            m_pgmCriteria.addEqualTo(beanPath, value);
        }

        /**
         * Apply criteria.
         */
        protected abstract void applyCriteria();

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         */
        protected abstract String getCustomValue(StateReportEntity entity) throws X2BaseException;

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         */
        protected boolean isIndicator() {
            return false;
        }

        /**
         * Inits the pgm map.
         */
        private void initPgmMap() {
            QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, m_pgmCriteria);
            pgmQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
            m_stdPgmMap =
                    getBroker().getGroupedCollectionByQuery(pgmQuery, StudentProgramParticipation.COL_STUDENT_OID, 200);
        }
    }

    /**
     * Retrieves school.
     */
    protected class RetrieveSchool implements FieldRetriever {
        private static final String CACL_ID = "PSIS-SCHOOL";

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
            SisStudent student = (SisStudent) entity.getBean();
            String schoolIdState = null;
            SisSchool school = student.getSchool();

            String temp = (String) school.getFieldValueByBeanPath(m_fieldSklOutpl);

            if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                String schoolId = (String) student.getFieldValueByBeanPath(m_fieldStdOutplFacility);
                if (!StringUtils.isEmpty(schoolId)) {
                    schoolIdState = lookupStateValue(SisStudent.class, m_fieldStdOutplFacility, schoolId);
                }
            }

            temp = (String) school.getFieldValueByBeanPath(m_fieldSklSped);
            if (BooleanAsStringConverter.TRUE.equalsIgnoreCase(temp)) {
                String schoolId = (String) student.getFieldValueByBeanPath(m_fieldStdSpedFacility);
                if (!StringUtils.isEmpty(schoolId)) {
                    schoolIdState = lookupStateValue(SisStudent.class, m_fieldStdSpedFacility, schoolId);

                }
            }

            return schoolIdState != null ? schoolIdState
                    : student.getSchool().getFieldValueByBeanPath(m_fieldSklStateId);
        }
    }

    /**
     * Retrieve for a student's SPED placement status. Returns 'Y' if the student has an active SPED
     * status, 'N' otherwise.
     */
    protected class RetrieveSpedPlacement implements FieldRetriever {
        private static final String CAL_ID = "PSIS-SPED";

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
            boolean isSped = false;
            SisStudent student = (SisStudent) entity.getBean();
            String spedActiveCode =
                    PreferenceManager.getPreferenceValue(student.getSchool(), SisPreferenceConstants.SPED_ACTIVE_CODE);

            if (spedActiveCode.equals(student.getSpedStatusCode())) {
                isSped = true;
            }

            return Boolean.valueOf(isSped);
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "PSIS-STRIP-CHARS";

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            Matcher matcher = m_illegalNameCharacters.matcher(value);

            return matcher.replaceAll("");
        }
    }

    /**
     * Validate report to identify students who have a NEXUS value and N for Student.PSIS16.
     */
    protected class ValidateNexus implements FieldValidator {
        private static final String VAL_ID = "VAL-NEXUS";

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
            SisStudent student = (SisStudent) entity.getBean();
            String nexusFieldValue = (String) student.getFieldValueByBeanPath(m_fieldStdNexus);

            if (!StringUtils.isEmpty(nexusFieldValue)) {
                String spedCode = (String) student.getFieldValueByBeanPath(m_fieldStdSped);
                String spedState = lookupStateValue(SisStudent.class, m_fieldStdSped, spedCode);
                if (!StringUtils.isEmpty(spedState)) {
                    if ("N".equalsIgnoreCase(spedState)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Student has \"N\" for field with alias = PSIS16 and not empty value for field with alias = NEXUS",
                                "LASID=" + STYLE_BOLD + student.getLocalId() + STYLE_END));
                    }
                } else {
                    errors.add(new StateReportValidationError(entity, field,
                            "Student has no value for field with alias = PSIS16",
                            "LASID=" + STYLE_BOLD + student.getLocalId() + STYLE_END));

                }
            }

            return errors;
        }
    }

    /**
     * Validate for duplicate programs of the same type on a single student.
     */
    protected class ValidatePgm implements FieldValidator {
        private static final String VAL_ID = "VAL-PGMS";

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
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();

            if (m_pgmsErrorsMap.get(student.getOid()) != null
                    && m_pgmsErrorsMap.get(student.getOid()).get(param) != null) {
                errors.add(m_pgmsErrorsMap.get(student.getOid()).get(param));
            }

            return errors;
        }
    }


    /**
     * Returns the heading with an end of line character appended.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder sb = new StringBuilder(super.getHeading());

        return sb.toString();
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        if (getSetupErrors().size() == 0) {
            setQuery(m_helper.getStudentQuery(true));

            // Get the map of absences
            // TODO possibly replace with helper's get absence method.
            loadAbsenceDaysMaps(m_helper.getStudentCriteria());

            // Set the entity class
            setEntityClass(PsisEntity.class);

            // Build maps of retriever functions and validator functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveSpedPlacement.CAL_ID, new RetrieveSpedPlacement());
            calcs.put(RetrieveMembershipDays.CALC_ID, new RetrieveMembershipDays());
            calcs.put(RetrieveAttendanceDays.CALC_ID, new RetrieveAttendanceDays());
            calcs.put(RetrievePrekNumericValue.CAL_ID, new RetrievePrekNumericValue());
            calcs.put(RetrievePrekProgramStatus.CAL_ID, new RetrievePrekProgramStatus());
            calcs.put(RetrieveGiftedandTalented.CALC_ID, new RetrieveGiftedandTalented());
            calcs.put(RetrieveSchool.CACL_ID, new RetrieveSchool());
            calcs.put(RetrieveNexus.CALC_ID, new RetrieveNexus());
            calcs.put(RetrieveEnglishLanguageLearner.CALC_ID, new RetrieveEnglishLanguageLearner());
            calcs.put(RetrieveEnglishLanguageLearnerProgramCode.CALC_ID,
                    new RetrieveEnglishLanguageLearnerProgramCode());
            calcs.put(RetrieveImmigrantStatus.CALC_ID, new RetrieveImmigrantStatus());
            calcs.put(RetrieveMilitaryFamily.CALC_ID, new RetrieveMilitaryFamily());

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateNexus.VAL_ID, new ValidateNexus());
            validators.put(ValidatePgm.VAL_ID, new ValidatePgm());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Gets the excused absences.
     *
     * @return Sets the
     */
    private Set<String> getExcusedAbsences() {
        Set<String> excusedAbsences = null;

        Criteria excusedAbsenceCriteria = new X2Criteria();
        excusedAbsenceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REFTABLE_OID_ATTENDANCE_REASONS);
        excusedAbsenceCriteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_ATTENDANCE_PRESENT);
        BeanQuery excusedAbsenceQuery = new BeanQuery(ReferenceCode.class, excusedAbsenceCriteria);
        Map<String, ReferenceCode> excusedAbsencesMap =
                getBroker().getGroupedCollectionByQuery(excusedAbsenceQuery, ReferenceCode.COL_CODE, 32);

        if (excusedAbsencesMap != null) {
            excusedAbsences = excusedAbsencesMap.keySet();
        }

        return excusedAbsences;
    }

    /**
     * Gets the reference codes for columns.
     *
     * @param className String
     * @param colName String
     * @return Map of RCD keyed on code for given column.
     */
    private Map<String, ReferenceCode> getReferenceCodesForColumns(String className, String colName) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryField(className, colName);

        Map<String, ReferenceCode> refCodes = new HashMap<String, ReferenceCode>();

        if (dictionaryField == null) {
            addSetupError("data dictionary column is missing",
                    "column: " + colName + " is missing from the data dictionary");
        } else {
            String refTableOid = dictionaryField.getReferenceTableOid();
            if (StringUtils.isEmpty(refTableOid)) {
                addSetupError("missing reference table", "reference table not found for the " + colName + " column ");
            } else {
                refCodes = getReferenceCodes(refTableOid);
            }
        }

        return refCodes;
    }

    /**
     * Initialize member fields.
     */
    private void initializeFields() {
        m_enrollmentManager = new EnrollmentManager(getBroker(), null, getOrganization());
        m_schoolsToCalendars = new HashMap();
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_fieldStdEllProgram = translateAliasToJavaName(ELL_PROGRAM_ALIAS, false);
        m_fieldStdGifTal = translateAliasToJavaName(GIFTED_TALENTED_ALIAS, false);
        m_fieldStdOutplFacility = translateAliasToJavaName(OUTSIDE_PLACEMENT_FACILITY_ALIAS, true);
        m_fieldStdPrekStatus = translateAliasToJavaName(STD_PREK_STATUS_ALIAS, false);
        m_fieldStdSpedFacility = translateAliasToJavaName(SPECIAL_EDUCATION_FACILITY_ALIAS, true);
        m_fieldSklStateId = translateAliasToJavaName(SCHOOL_STATE_ID_ALIAS, true);
        m_fieldSklOutpl = translateAliasToJavaName(OUTSIDE_PLACEMENT_SCHOOL_ALIAS, true);
        m_fieldStdNexus = translateAliasToJavaName(NEXUS_ALIAS, true);
        m_fieldSklSped = translateAliasToJavaName(SPECIAL_EDUCATION_SCHOOL_ALIAS, true);
        m_fieldStdSped = translateAliasToJavaName(SPED_CODE_ALIAS, true);
        m_includeMembershipAndAttendanceDays = (Boolean) getParameter(INCLUDE_MEMBERSHIP_AND_ATTENDANCE_DAYS_PARAM);
        m_requireFacility = (Boolean) getParameter(REQUIRE_FACILITY_PARAM);
        m_requireMemberDay = (Boolean) getParameter(REQUIRE_MEMBER_DAY_PARAM);
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        DistrictCalendar districtCalender = CalendarManager.getDistrictInSessionStartEndDate(getOrganization(),
                getCurrentContext(), true, getBroker());
        m_firstDayDate = districtCalender.getDate();
        m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_firstDayDate, getOrganization());
        m_gradeLevelCodes = getReferenceCodesForColumns(SisStudent.class.getName(), SisStudent.COL_GRADE_LEVEL);
    }

    /**
     * Loads a map by student of absence days for that student.
     *
     * @param studentCriteria Criteria
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // get codes for unexcused absences
        Set<String> excusedAbsences = getExcusedAbsences();

        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
        if (!excusedAbsences.isEmpty()) {
            Criteria unexcusedAbsencesCriteria = new X2Criteria();
            unexcusedAbsencesCriteria.addNotIn(StudentAttendance.COL_REASON_CODE, excusedAbsences);
            Criteria emptyReasonCodeCriteria = new X2Criteria();
            emptyReasonCodeCriteria.addIsNull(StudentAttendance.COL_REASON_CODE);
            unexcusedAbsencesCriteria.addOrCriteria(emptyReasonCodeCriteria);
            criteria.addAndCriteria(unexcusedAbsencesCriteria);
        }

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                new String[] {StudentAttendance.COL_STUDENT_OID, "SUM(" + StudentAttendance.COL_PORTION_ABSENT + ")"},
                criteria);
        reportQuery.addGroupBy(StudentAttendance.COL_STUDENT_OID);

        // Build the map of student to courses.
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
}
