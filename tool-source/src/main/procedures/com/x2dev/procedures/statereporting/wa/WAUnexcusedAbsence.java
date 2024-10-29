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

package com.x2dev.procedures.statereporting.wa;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Student Absence File.
 *
 * @author X2 Development Corporation
 */
public class WAUnexcusedAbsence extends StateReportData {
    /**
     * Entity class for Student Absence export.
     *
     */
    public static class WAStudentAbsenceEntity extends StateReportEntity {
        private static final String ALIAS_EXIT_REASON = "DOE EXIT REASON";
        private static final String ALIAS_DISABILITY = "DOE DISABILITY";
        private static final String PGM_INELIGIBLE_EXIT = "Ineligible";
        private static final String PGM_NO_DISABILITY = "00";

        List<StudentAttendance> m_attendance;
        Boolean m_isAprilMember = Boolean.FALSE;
        Boolean m_isEll = Boolean.FALSE;
        Boolean m_isFiveAbsences = Boolean.FALSE;
        Boolean m_isLowIncome = Boolean.FALSE;
        Boolean m_isOctoberMember = Boolean.FALSE;
        Boolean m_isSped = Boolean.FALSE;
        int[] m_truancyCount;
        int m_ueAbsences = 0;
        WAUnexcusedAbsence m_waData = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WAStudentAbsenceEntity() {
            // Must have public no argument constructor
        }

        /**
         * Returns the StudentAttendance record for the current index.
         *
         * @return StudentAttendance
         */
        public List<StudentAttendance> getAbsences() {
            return m_attendance;
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
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

        /**
         * Gets the ue absences.
         *
         * @return Integer
         */
        public Integer getUeAbsences() {
            return Integer.valueOf(m_ueAbsences);
        }

        /**
         * Gets the truancy count.
         *
         * @return int[]
         */
        public int[] getTruancyCount() {
            return m_truancyCount;
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

            SisStudent student = (SisStudent) bean;
            m_waData = (WAUnexcusedAbsence) data;
            m_truancyCount = m_waData.getTruancyCounts(student);

            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, m_waData);

            // Get attendance from enrollment spans rather than all form helper.
            // This has the effect of skipping attendance records that were
            // entered after the withdrawal date by mistake.
            m_attendance = new ArrayList<StudentAttendance>();
            if (districtSpans.size() > 0) {
                List<StudentEnrollmentSpan> spans =
                        m_waData.getStdHistoryHelper().getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    // Process Attendance Records
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        String withdrawalCode = enrollment.getEnrollmentCode();
                        withdrawalCode = data.lookupReferenceCodeByRefTbl(m_waData.m_refTableWithdrawalCode,
                                withdrawalCode,
                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        if (!CODE_NO_SHOW.equals(withdrawalCode)) {
                            m_attendance.addAll(span.getStudentAttendance());
                        }
                    } else {
                        m_attendance.addAll(span.getStudentAttendance());
                    }

                    // Set membership flags
                    PlainDate startDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                    if (startDate != null) {
                        PlainDate endDate = m_waData.m_context.getEndDate();
                        if (span.getFirstInactiveEnrollment() != null) {
                            endDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                        }
                        if (!m_waData.getFirstOctoberDay().before(startDate)
                                && !m_waData.getFirstOctoberDay().after(endDate)) {
                            m_isOctoberMember = Boolean.TRUE;
                        }
                        if (!m_waData.getFirstAprilDay().before(startDate)
                                && !m_waData.getFirstAprilDay().after(endDate)) {
                            m_isAprilMember = Boolean.TRUE;
                        }
                    }

                }
            }
            m_ueAbsences = countUnexcusedAbsences();

            initProgramFlags(student);
        }

        /**
         * Checks if is april member.
         *
         * @return Boolean
         */
        public Boolean isAprilMember() {
            return m_isAprilMember;
        }

        /**
         * Checks if is ell.
         *
         * @return the m_isEll
         */
        public Boolean isEll() {
            return m_isEll;
        }

        /**
         * Checks if is five absences.
         *
         * @return Boolean
         */
        public Boolean isFiveAbsences() {
            return m_isFiveAbsences;
        }

        /**
         * Checks if is low income.
         *
         * @return the m_isLowIncome
         */
        public Boolean isLowIncome() {
            return m_isLowIncome;
        }

        /**
         * Checks if is october member.
         *
         * @return Boolean
         */
        public Boolean isOctoberMember() {
            return m_isOctoberMember;
        }

        /**
         * Checks if is sped.
         *
         * @return the m_isSped
         */
        public Boolean isSped() {
            return m_isSped;
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
         * Count unexcused absences.
         *
         * @return int
         */
        private int countUnexcusedAbsences() {
            int totalAbsences = 0;
            Map<Integer, List<PlainDate>> attendancesInMonths = new HashMap<Integer, List<PlainDate>>();
            Calendar calendar = new GregorianCalendar();
            for (StudentAttendance absence : getAbsences()) {
                if (!absence.getExcusedIndicator()) {
                    if (absence.getPortionAbsent().floatValue() > 0.5) {
                        totalAbsences++;
                        PlainDate absenceDate = absence.getDate();
                        calendar.setTime(absenceDate);
                        Integer month = Integer.valueOf(calendar.get(Calendar.MONTH));
                        List<PlainDate> dates = attendancesInMonths.get(month);
                        if (dates == null) {
                            dates = new ArrayList<PlainDate>();
                            attendancesInMonths.put(month, dates);
                        }
                        dates.add(absenceDate);
                    }
                }
            }
            for (List<PlainDate> dates : attendancesInMonths.values()) {
                if (dates.size() > 5) {
                    m_isFiveAbsences = Boolean.TRUE;
                }
            }
            return totalAbsences;
        }


        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. indistrict
         * transfer.
         *
         * @param student Student
         * @param sdData WAUnexcusedAbsence
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAUnexcusedAbsence sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans.
                 * A no-show span represents an enrollment where the student never showed up.
                 * It is identified by a withdrawal code that has NS in the local code of the
                 * reference table.
                 */
                boolean noShow = false;
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode,
                            withdrawalCode,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        noShow = true;
                    }
                }

                if (!noShow) {
                    // Check the span for entry type (internal or external)
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode,
                                code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(stateCode) || "2".equals(stateCode)) {
                            if ((currentSpan != null) &&
                                    ((currentSpan.m_exitDate == null) ||
                                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
                                districtSpans.add(currentSpan);
                            }
                            currentSpan = null;

                            currentSpan = new DistrictEnrollmentSpan();
                            currentSpan.m_entryEnrollment = enrollment;
                            currentSpan.m_exitDate = span.getLastActiveDate();
                        } else {
                            if (currentSpan == null) {
                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                            }
                            currentSpan.m_exitDate = span.getLastActiveDate();
                        }
                    }
                }
            }
            if ((currentSpan != null) &&
                    ((currentSpan.m_exitDate == null) ||
                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
        }

        /**
         * Checks for valid dates.
         *
         * @param program StudentProgramParticipation
         * @return true, if successful
         */
        private boolean hasValidDates(StudentProgramParticipation program) {
            PlainDate startDate =
                    program.getStartDate() == null ? m_waData.m_context.getStartDate() : program.getStartDate();
            PlainDate endDate = program.getEndDate() == null ? m_waData.m_context.getEndDate() : program.getEndDate();
            if ((m_isAprilMember.booleanValue() && !m_waData.getFirstAprilDay().before(startDate)
                    && !m_waData.getFirstAprilDay().after(endDate)) ||
                    (m_isOctoberMember.booleanValue() && !m_waData.getFirstOctoberDay().before(startDate)
                            && !m_waData.getFirstOctoberDay().after(endDate))) {
                return true;
            }
            return false;
        }

        /**
         * Special Education: local code SPED
         * Limited English: local code ELL
         * Low-Income: State code 19.
         *
         * @param student SisStudent
         */
        private void initProgramFlags(SisStudent student) {
            Collection<StudentProgramParticipation> programs =
                    ((WAUnexcusedAbsence) getData()).m_stdPgmsMap.get(student.getOid());
            if (programs != null) {
                for (StudentProgramParticipation program : programs) {
                    if (hasValidDates(program)) {
                        String programCode = program.getProgramCode();
                        String stateCode = getData().lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE,
                                programCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        String localCode = getData().lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE,
                                programCode,
                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        if (isValidSped(program)) {
                            m_isSped = Boolean.TRUE;
                        }
                        if (LOCAL_CODE_ELL.equals(localCode)) {
                            m_isEll = Boolean.TRUE;
                        }
                        if (STATE_CODE_LOW_INCOME.equals(stateCode)) {
                            m_isLowIncome = Boolean.TRUE;
                        }
                    }
                }
            }
        }

        /**
         * Checks if is valid sped.
         *
         * @param pgm StudentProgramParticipation
         * @return true, if is valid sped
         */
        private boolean isValidSped(StudentProgramParticipation pgm) {
            if (m_waData.getSpedCodes().contains(pgm.getProgramCode())) {
                String pgmExitReason = (String) pgm.getFieldValueByAlias(ALIAS_EXIT_REASON);
                String pgmDisability = (String) pgm.getFieldValueByAlias(ALIAS_DISABILITY);

                // convert to state code and pad left (length 2) with 0's
                if (pgmDisability != null) {
                    int stateCodeRefTypeOrdinal = ExportFormatField.ReferenceMapTypeCode.STATE.ordinal();
                    String pgmDisabilityStateCode = getData().lookupReferenceCodeByAlias(ALIAS_DISABILITY,
                            pgmDisability,
                            stateCodeRefTypeOrdinal);
                    pgmDisability = StringUtils.padLeft(pgmDisabilityStateCode, 2, '0');
                }

                if (!(PGM_INELIGIBLE_EXIT.equals(pgmExitReason) && PGM_NO_DISABILITY.equals(pgmDisability))) {
                    return true;
                }
            }
            return false;
        }
    }

    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    protected static class DistrictEnrollmentSpan {
        StudentEnrollment m_entryEnrollment;
        PlainDate m_exitDate;
    }

    /**
     * Retrieve a value from the attendance record.
     */
    protected class RetrieveReportValue implements FieldRetriever {
        protected static final String PARAM_LOCATION_ID = "LOCATION-ID";
        protected static final String PARAM_UNEXCUSED_ABSENCE = "UA";
        protected static final String PARAM_RACE_CODE = "RACE_CODE";
        protected static final String PARAM_FIVE_ABSENCES = "FIVE_ABSENCES";
        protected static final String PARAM_APR_MEMBER = "APR_MEMBER";
        protected static final String PARAM_OCT_MEMBER = "OCT_MEMBER";
        protected static final String PARAM_TRUANCY_YEAR = "TRUANCY_YEAR";
        protected static final String PARAM_TRUANCY_SUMMER = "TRUANCY_SUMMER";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            WAStudentAbsenceEntity waEntity = (WAStudentAbsenceEntity) entity;
            Student student = (Student) waEntity.getBean();
            if (PARAM_LOCATION_ID.equals(param)) {
                value = student.getSchool().getSchoolId();
            } else if (PARAM_OCT_MEMBER.equals(param)) {
                value = waEntity.isOctoberMember().booleanValue() ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            } else if (PARAM_APR_MEMBER.equals(param)) {
                value = waEntity.isAprilMember().booleanValue() ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            } else if (PARAM_UNEXCUSED_ABSENCE.equals(param)) {
                value = waEntity.getUeAbsences();
            } else if (PARAM_TRUANCY_YEAR.equals(param)) {
                value = Integer.toString(waEntity.getTruancyCount()[0]);
            } else if (PARAM_TRUANCY_SUMMER.equals(param)) {
                value = Integer.toString(waEntity.getTruancyCount()[1]);
            } else if (PARAM_RACE_CODE.equals(param)) {
                // Hispanic/Latino of any race FEDERAL CODE H
                // American Indian/Alaskan Native FEDERAL CODE N
                // Asian FEDERAL CODE A
                // Black FEDERAL CODE B
                // Native Hawaiian/Other Pacific Islander FEDERAL code P
                // Caucasion/White FEDERAL CODE W
                // Two or More Races: this have to be based upon the number
                // of race records for each student
                WAUnexcusedAbsence waData = (WAUnexcusedAbsence) data;
                List<String> races = new ArrayList<String>();
                for (Race personRace : student.getPerson().getRaces()) {
                    ReferenceCode refCode = waData.getRaceReferenceMap().get(personRace.getRaceCode());
                    if (refCode != null) {
                        String raceCode = refCode.getFederalCode();
                        if (RACE_CODES_MAP.keySet().contains(raceCode)) {
                            races.add(RACE_CODES_MAP.get(raceCode));
                        }
                    }
                }
                if (races.contains(RACE_CODE_HISPANIC) || student.getPerson().getHispanicLatinoIndicator()) {
                    value = RACE_CODE_HISPANIC;
                } else if (races.size() > 1) {
                    value = RACE_CODE_TWO_OR_MORE; // stands for "TWO OR MORE"
                } else if (!races.isEmpty()) {
                    value = races.get(0);
                } else {
                    value = RACE_CODE_DEFAULT;
                }
            } else if (PARAM_FIVE_ABSENCES.equals(param)) {
                value = waEntity.isFiveAbsences().booleanValue() ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            }
            return value;
        }
    }

    /**
     * Retrieve a value from the attendance record.
     */
    protected class RetrieveProgramValue implements FieldRetriever {
        private final String CALC_PARAM_IS_SPED = "SPED";
        private final String CALC_PARAM_IS_LOW_INCOME = "LOW_INCOME";
        private final String CALC_PARAM_IS_ELL = "ELL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = EMPTY_STRING;
            String parameter = (String) field.getParameter();
            WAStudentAbsenceEntity myEntity = (WAStudentAbsenceEntity) entity;

            if (CALC_PARAM_IS_ELL.equals(parameter)) {
                value = myEntity.isEll().booleanValue() ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            } else if (CALC_PARAM_IS_LOW_INCOME.equals(parameter)) {
                value = myEntity.isLowIncome().booleanValue() ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            } else if (CALC_PARAM_IS_SPED.equals(parameter)) {
                value = myEntity.isSped().booleanValue() ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;
            }
            return value.trim();
        }
    }

    /*
     * Constants: parameters, Ids
     */
    protected static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";
    protected static final String ALIAS_TRUANCY = "DOE TRUANCY";
    protected static final String CALCULATION_ID = "EXPDATA-WA-UA";
    protected static final String CALCULATION_ID_PROGRAM = "WA-UA-PG";
    protected static final String CODE_EXCUSED_FULL = "EF";
    protected static final String CODE_EXCUSED_HALF = "EP";
    protected static final String CODE_UNEXCUSED_FULL = "UF";
    protected static final String CODE_UNEXCUSED_HALF = "UP";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String CODE_SPED = "SPED";
    protected static final String STATE_CODE_SPED = "SPED";
    protected static final String LOCAL_CODE_ELL = "ELL";
    protected static final String STATE_CODE_LOW_INCOME = "19";
    private static final String INPARAM_CONTEXT_YEAR = "contextYear";
    private static final String INPARAM_INCLUDED_GRADES = "includedGrades";
    private static final String INPARAM_DELIMITER = "charDelimiter";
    protected static final String RACE_CODE_ASIAN = "A";
    protected static final String RACE_CODE_HISPANIC = "H";
    protected static final String RACE_CODE_NATIVE = "N";
    protected static final String RACE_CODE_BLACK = "B";
    protected static final String RACE_CODE_PACIFIC = "P";
    protected static final String RACE_CODE_WHITE = "W";
    protected static final String RACE_CODE_TWO_OR_MORE = "T";
    protected static final String RACE_CODE_DEFAULT = "X";

    protected static Map<String, String> RACE_CODES_MAP;
    static {
        RACE_CODES_MAP = new HashMap<String, String>();
        RACE_CODES_MAP.put(RACE_CODE_ASIAN, RACE_CODE_ASIAN);
        RACE_CODES_MAP.put(RACE_CODE_HISPANIC, RACE_CODE_HISPANIC);
        RACE_CODES_MAP.put(RACE_CODE_NATIVE, RACE_CODE_NATIVE);
        RACE_CODES_MAP.put(RACE_CODE_BLACK, RACE_CODE_BLACK);
        RACE_CODES_MAP.put(RACE_CODE_PACIFIC, RACE_CODE_PACIFIC);
        RACE_CODES_MAP.put(RACE_CODE_WHITE, RACE_CODE_WHITE);
        RACE_CODES_MAP.put("3", RACE_CODE_ASIAN);
        RACE_CODES_MAP.put("1", RACE_CODE_HISPANIC);
        RACE_CODES_MAP.put("2", RACE_CODE_NATIVE);
        RACE_CODES_MAP.put("4", RACE_CODE_BLACK);
        RACE_CODES_MAP.put("5", RACE_CODE_PACIFIC);
        RACE_CODES_MAP.put("6", RACE_CODE_WHITE);
    }
    /*
     * Instance variables
     */
    protected StudentHistoryHelper m_helper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected List m_includedGrades = null;
    protected Map<String, Collection<StudentProgramParticipation>> m_stdPgmsMap;
    protected Map<String, ReferenceCode> m_raceReferenceMap = null;
    protected DistrictSchoolYearContext m_context = null;

    private static final String SPLIT_DATE_EXPRESSION = "\\s*[;,|,\\s+]\\s*";

    private PlainDate m_firstAprilDay;
    private PlainDate m_firstOctDay;
    private Collection<String> m_spedCodes;

    private PlainDate m_truancyStartDate;
    private PlainDate m_truancyBreakDate;
    private PlainDate m_truancyEndDate;
    private String m_truancyFieldPath;
    private SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    /**
     * Gets the first april day.
     *
     * @return The first business day in April
     */
    protected PlainDate getFirstAprilDay() {
        if (m_firstAprilDay == null) {
            Calendar calendar = new GregorianCalendar();
            calendar.set(m_context.getSchoolYear(), Calendar.APRIL, 1);
            m_firstAprilDay = getNextInSessionDate(new PlainDate(calendar.getTime()));
        }
        return m_firstAprilDay;
    }

    /**
     * Gets the first october day.
     *
     * @return The first business day in April
     */
    protected PlainDate getFirstOctoberDay() {
        if (m_firstOctDay == null) {
            Calendar calendar = new GregorianCalendar();
            calendar.set(m_context.getSchoolYear() - 1, Calendar.OCTOBER, 1);
            m_firstOctDay = getNextInSessionDate(new PlainDate(calendar.getTime()));
        }
        return m_firstOctDay;
    }

    /**
     * Gets the race reference map.
     *
     * @return the m_raceReferenceMap
     */
    protected Map<String, ReferenceCode> getRaceReferenceMap() {
        return m_raceReferenceMap;
    }

    /**
     * Gets the sped codes.
     *
     * @return Collection
     */
    protected Collection<String> getSpedCodes() {
        if (m_spedCodes == null) {
            DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE);
            String referenceTableOid = field.getReferenceTableOid();
            m_spedCodes = new ArrayList<String>();
            if (!StringUtils.isEmpty(referenceTableOid)) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
                criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPED);
                SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
                m_spedCodes = getBroker().getSubQueryCollectionByQuery(query);
            }
        }
        return m_spedCodes;
    }

    /**
     * Gets the std history helper.
     *
     * @return the m_helper
     */
    protected StudentHistoryHelper getStdHistoryHelper() {
        return m_helper;
    }


    /**
     * Gets the truancy counts.
     *
     * @param student SisStudent
     * @return int[]
     */
    protected int[] getTruancyCounts(SisStudent student) {
        int schoolCount = 0;
        int summerCount = 0;
        if (m_truancyStartDate == null) {
            Calendar calendar = new GregorianCalendar();
            calendar.set(m_context.getSchoolYear() - 1, Calendar.SEPTEMBER, 1);
            m_truancyStartDate = new PlainDate(calendar.getTime());
            calendar.set(m_context.getSchoolYear(), Calendar.JUNE, 1);
            m_truancyBreakDate = new PlainDate(calendar.getTime());
            calendar.set(m_context.getSchoolYear(), Calendar.AUGUST, 31);
            m_truancyEndDate = new PlainDate(calendar.getTime());
            m_truancyFieldPath = translateAliasToJavaName(ALIAS_TRUANCY, false);
        }
        if (m_truancyFieldPath != null) {
            String datesAsText = (String) student.getFieldValueByBeanPath(m_truancyFieldPath);
            if (!StringUtils.isEmpty(datesAsText)) {
                String[] dates = datesAsText.split(SPLIT_DATE_EXPRESSION);
                for (String text : dates) {
                    try {
                        Date date = m_dateFormat.parse(text);
                        if (!date.before(m_truancyStartDate) && !date.after(m_truancyEndDate)) {
                            if (!date.before(m_truancyBreakDate)) {
                                ++summerCount;
                            } else {
                                ++schoolCount;
                            }
                        }
                    } catch (ParseException e) {
                        // No action if date does not parse
                    }
                }
            }
        }
        return new int[] {schoolCount, summerCount};
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_refTableEnrollmentCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        String delimiter = (String) getParameter(INPARAM_DELIMITER);
        String grades = (String) getParameter(INPARAM_INCLUDED_GRADES);
        m_includedGrades = StringUtils.convertDelimitedStringToList(grades, delimiter.charAt(0));

        String contextOid = (String) getParameter(INPARAM_CONTEXT_YEAR);
        m_context = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                contextOid);

        m_raceReferenceMap = getReferenceCodes("rtbRaceCodes");
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        Calendar calendar = new GregorianCalendar();
        calendar.set(m_context.getSchoolYear(), Calendar.MAY, 31);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, new PlainDate(calendar.getTime()));
        m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, m_includedGrades);

        // Setup OK
        if (getSetupErrors().size() == 0) {
            // create query - use the appropriate class
            setQuery(m_helper.getStudentQuery(false));

            // Set Custom Entity
            setEntityClass(WAStudentAbsenceEntity.class);

            X2Criteria programCriteria = new X2Criteria();
            // include records that start before first april day
            programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getFirstAprilDay());

            // include records that end after first october day
            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, getFirstOctoberDay());
            criteria2.addOrCriteria(criteria3);
            programCriteria.addAndCriteria(criteria2);

            String fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, false);
            if (fieldExcludePgm != null) {
                programCriteria.addNotEqualTo(fieldExcludePgm, BooleanAsStringConverter.TRUE);
            }

            BeanQuery query = new BeanQuery(StudentProgramParticipation.class, programCriteria);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
            m_stdPgmsMap =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 128);

            // Build and attach retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALCULATION_ID, new RetrieveReportValue());
            calcs.put(CALCULATION_ID_PROGRAM, new RetrieveProgramValue());
            super.addCalcs(calcs);
        }

    }

    /**
     * Gets the next in session date.
     *
     * @param date PlainDate
     * @return Plain date
     */
    private PlainDate getNextInSessionDate(PlainDate date) {
        PlainDate retValue = null;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, m_context.getOid());
        criteria.addGreaterOrEqualThan(DistrictCalendar.COL_DATE, date);
        criteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderBy(DistrictCalendar.COL_DATE, true);

        QueryIterator dates = getBroker().getIteratorByQuery(query);
        try {
            if (dates.hasNext()) {
                retValue = ((DistrictCalendar) dates.next()).getDate();
            }
        } finally {
            if (dates != null) {
                dates.close();
            }
        }
        return retValue;
    }
}
