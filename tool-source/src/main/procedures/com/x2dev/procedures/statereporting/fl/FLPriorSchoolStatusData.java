/*
 * ==================================================================== X2 Development Corporation
 * Copyright (c)
 * 2002-2006 X2 Development Corporation. All rights reserved. Redistribution and use in source and
 * binary forms, with or
 * without modification, is not permitted without express written agreement from X2 Development
 * Corporation.
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentAttendanceDataset;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentEnrollmentSpanInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentInfo;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper;
import com.x2dev.procedures.statereporting.fl.FLStudentHelper.StudentScheduleHelper.StudentScheduleInfo;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import jersey.repackaged.com.google.common.collect.ImmutableSet;

/**
 * FL Prior School Status report
 *
 * http://www.fldoe.org/accountability/data-sys/database-manuals-updates/2016-17-student-info-system
 * /prior-school-status-student-attendance.stml
 *
 * @author Follett Software Company
 */
public class FLPriorSchoolStatusData extends FLStateReportData {

    /**
     * The Class FLPriorSchoolStatusEntity.
     */
    public static class FLPriorSchoolStatusEntity extends FLStateReportEntity {
        private List<StudentEnrollmentSpanInfo> m_enrollments;

        /**
         * Instantiates a new FL prior school status entity.
         */
        public FLPriorSchoolStatusEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current record.
         *
         * @return Student enrollment span info
         */
        public StudentEnrollmentSpanInfo getCurrentRecord() {
            return m_enrollments.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the
         * entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            return student.getNameView() + " [LASID: " + student.getLocalId() + "] ";
        }

        /**
         * Gets the previous record.
         *
         * @return Student enrollment span info
         */
        public StudentEnrollmentSpanInfo getPreviousRecord() {
            return getCurrentRow() > 0 ? m_enrollments.get(getCurrentRow() - 1) : null;
        }

        /**
         * Intitialize.
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

            FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;
            SisStudent student = (SisStudent) getBean();
            if (flData.getStudentHelper().isStudentEligible(student)) {
                List<StudentEnrollmentSpan> spans = flData.getStudentHelper().getStudentEnrollmentSpans(student, true);
                if (spans != null && !spans.isEmpty()) {
                    m_enrollments = new ArrayList<StudentEnrollmentSpanInfo>(spans.size());
                    for (StudentEnrollmentSpan span : spans) {
                        StudentEnrollmentSpanInfo info =
                                flData.getStudentHelper().new StudentEnrollmentSpanInfo(student, span);
                        if (info.getGradeLevel().matches(VALID_GRADE_LEVEL_PATTERN)) {
                            m_enrollments.add(info);
                        }
                    }
                }
            }
            setRowCount(m_enrollments != null ? m_enrollments.size() : 0);
        }
    }

    /**
     * Field retriever for unexcused absent days number, annual
     * http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-112031.pdf
     */
    protected class RetrieveDaysAbsentUnexcused implements FieldRetriever {
        public static final String CALC_ID = "ABSENT_UNEXCUSED";

        private static final String CODE_OSS = "OSS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;

            if (SURVEY_PERIOD_2.equals(flData.getSurveyPeriodCode())) {
                return null; // For Survey Periods 2, this field should be zero-filled.
            }

            int unexcused = 0;
            SisStudent student = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudent();
            StudentAttendanceDataset dataset = getStudentHelper().getStudentAttendanceDataset(
                    flData.getCurrentContext().getStartDate(), flData.getCurrentContext().getEndDate());
            List<StudentAttendance> attendances = dataset.getAttendances(student.getOid());
            if (attendances != null && !attendances.isEmpty()) {
                for (StudentAttendance attendance : attendances) {
                    if (attendance.getAbsentIndicator() && (attendance.getOtherCode() == null ||
                            !attendance.getOtherCode().contains(CODE_OSS)) && !attendance.getExcusedIndicator()) {
                        unexcused++;
                    }
                }
            }
            return Integer.valueOf(unexcused);
        }
    }

    /**
     * Field retriever for absent and present days number, annual.
     */
    protected class RetrieveDaysAnnual implements FieldRetriever {
        public static final String CALC_ID = "DAYS_ANNUAL";

        private static final String PARAM_ABSENT = "ABSENT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudent();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            int value = 0;
            if (!SURVEY_PERIOD_2.equals(getSurveyPeriod().getCode())
                    && !TERM_CODE_NONENROLLMENT.equals(entity.getFieldValue(FIELD_NAME_TERM))) {
                value = info.getAbsentDates(getCurrentContext()).size();
                if (!PARAM_ABSENT.equals(field.getParameter().toString())) {
                    value = info.getMemberDates(getCurrentContext()).size() - value;
                }
            }
            return Integer.valueOf(value);
        }
    }

    /**
     * Field retriever for absent and present days number, summer terms.
     */
    protected class RetrieveDaysSummer implements FieldRetriever {
        public static final String CALC_ID = "DAYS_SUMMER";

        private static final String PARAM_ABSENT = "ABSENT";

        private DataDictionaryField m_fieldSheduleTermCode;

        /**
         * Instantiates a new retrieve CTE teacher cours fields.
         */
        public RetrieveDaysSummer() {
            m_fieldSheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;

            SisStudent student = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudent();
            StudentInfo stdInfo = getStudentHelper().getStudentInfo(student);

            Set<ScheduleTermDate> summerDates = new HashSet<ScheduleTermDate>();
            List<StudentScheduleInfo> schInfoList = m_studentScheduleHelper.getStudentScheduleInfo(student);
            for (int i = 0; i < schInfoList.size(); i++) {
                StudentScheduleInfo schInfo = schInfoList.get(i);
                ScheduleTerm term = schInfo.getSection().getScheduleTerm();
                String termCode = (String) flData.getFieldValue(term, m_fieldSheduleTermCode);
                if (m_summerTermCodes.contains(termCode)) {
                    summerDates.addAll(term.getScheduleTermDates());
                }
            }

            int value = 0;
            if (SURVEY_PERIOD_5.equals(getSurveyPeriod().getCode())) {
                value = getSummerDaysNumber(stdInfo.getAbsentDates(getCurrentContext()), summerDates);
                if (!PARAM_ABSENT.equals(field.getParameter().toString())) {
                    value = getSummerDaysNumber(stdInfo.getMemberDates(getCurrentContext()), summerDates) - value;
                }
            }

            return Integer.valueOf(value);
        }

        /**
         * Gets the summer days number.
         *
         * @param dates ImmutableSet<PlainDate>
         * @param summerDates Set<ScheduleTermDate>
         * @return int
         */
        private int getSummerDaysNumber(ImmutableSet<PlainDate> dates, Set<ScheduleTermDate> summerDates) {
            int res = 0;
            for (PlainDate date : dates) {
                for (ScheduleTermDate termDate : summerDates) {
                    if (date.compareTo(termDate.getStartDate()) >= 0 && date.compareTo(termDate.getEndDate()) <= 0) {
                        res++;
                        break;
                    }
                }
            }
            return res;
        }
    }

    /**
     * Field retriever for Disaster Affected
     * http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-131545.pdf
     *
     */
    protected class RetrieveDisasterAffected implements FieldRetriever {
        public static final String CALC_ID = "DISASTER_AFFECTED";

        private static final String ALIAS_DISASTER_AFFECTED = "all-enr-DisasterAffected";

        private static final String DEFAULT_VALUE = "Z";

        private DataDictionaryField m_fieldDisasterAffected;

        /**
         * Instantiates a new retrieve disaster affected.
         */
        public RetrieveDisasterAffected() {
            m_fieldDisasterAffected = translateAliasToDictionaryField(ALIAS_DISASTER_AFFECTED, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = DEFAULT_VALUE;
            StudentEnrollmentSpan span = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord()
                    .getStudentEnrollmentSpan();
            if (span != null && span.getEnrollments() != null && !span.getEnrollments().isEmpty()) {
                FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;
                for (StudentEnrollment enr : span.getEnrollments()) {
                    if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType())) {
                        String fvalue = (String) flData.getFieldValue(enr, m_fieldDisasterAffected);
                        if (fvalue != null && !fvalue.isEmpty()) {
                            value = fvalue;
                            break;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Educational Choice.
     */
    protected class RetrieveEducationalChoice implements FieldRetriever {
        public static final String CALC_ID = "EDUCATIONAL_CHOICE";

        private static final String ALIAS_EDUCATIONAL_CHOICE = "all-enr-EducationalChoice";

        private DataDictionaryField m_fieldEducationalChoice;

        /**
         * Instantiates a new retrieve educational choice.
         */
        public RetrieveEducationalChoice() {
            m_fieldEducationalChoice = translateAliasToDictionaryField(ALIAS_EDUCATIONAL_CHOICE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentEnrollmentSpan span = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord()
                    .getStudentEnrollmentSpan();
            if (span != null && span.getEnrollments() != null && !span.getEnrollments().isEmpty()) {
                FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;
                for (StudentEnrollment enr : span.getEnrollments()) {
                    if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType()) ||
                            StudentEnrollment.STATUS_CHANGE.equals(enr.getEnrollmentType())) {
                        String fvalue = (String) flData.getFieldValue(enr, m_fieldEducationalChoice);
                        if (fvalue != null && !fvalue.isEmpty()) {
                            value = fvalue;
                            break;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Entry Code and Date.
     */
    protected class RetrieveEntry implements FieldRetriever {
        public static final String CALC_ID = "ENTRY";

        private static final String PARAM_CODE = "CODE";

        private static final String STATE_ENTRY_CODE_DEFAULT = "R01";
        private static final String STATE_ENTRY_CODE_E = "E";
        private static final String STATE_ENTRY_CODE_R = "R";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentEnrollmentSpan span = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord()
                    .getStudentEnrollmentSpan();
            if (span != null) {
                if (!SURVEY_PERIOD_2.equals(getSurveyPeriodCode())
                        && TERM_CODE_NONENROLLMENT.equals(entity.getFieldValue(FIELD_NAME_TERM))) {
                    return null;
                }
                StudentEnrollment enr = span.getFirstActiveEnrollment();
                if (enr != null) {
                    if (PARAM_CODE.equals(field.getParameter().toString())) {
                        // get the state reference code for enrEnrCode
                        value = data.lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                                enr.getEnrollmentCode());
                        if (!enr.getEnrollmentType().equals(StudentEnrollment.ENTRY)) {
                            String code = value != null ? value.toString() : "";
                            value = (code.startsWith(STATE_ENTRY_CODE_E) || code.startsWith(STATE_ENTRY_CODE_R)) ? code
                                    : STATE_ENTRY_CODE_DEFAULT;
                        }
                    } else {
                        value = enr.getEnrollmentDate();
                    }
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Grade Level.
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        public static final String CALC_ID = "GRADE_LEVEL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentEnrollmentSpanInfo info = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord();
            return info.getGradeLevel();
        }
    }

    /**
     * Field retriever for habutial truant
     * http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-197263.pdf
     */
    protected class RetrieveHabitualTruant implements FieldRetriever {
        public static final String CALC_ID = "HABITUAL_TRUANT";

        private static final String ALIAS_HABITUAL_TRUANT = "all-std-HabitualTruant";

        private static final String VALUE_DEFAULT = "Z";
        private static final String VALUE_NO = "N";
        private static final String VALUE_YES = "Y";

        private DataDictionaryField m_fieldHabitualTruant;

        /**
         * Instantiates a new retrieve habitual truant.
         */
        public RetrieveHabitualTruant() {
            m_fieldHabitualTruant = translateAliasToDictionaryField(ALIAS_HABITUAL_TRUANT, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            String value = VALUE_DEFAULT;

            FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;

            if (!SURVEY_PERIOD_2.equals(flData.getSurveyPeriodCode()) &&
                    !SURVEY_PERIOD_3.equals(flData.getSurveyPeriodCode())) {

                SisStudent student = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudent();
                Boolean bvalue = (Boolean) flData.getFieldValue(student, m_fieldHabitualTruant);
                value = bvalue != null && bvalue.booleanValue() ? VALUE_YES : VALUE_NO;
            }
            return value;
        }
    }

    /**
     * Field retriever for zoned district and school
     * Data Element Number: 115629, 173174.
     *
     * @author Follett Software Company
     */
    protected class RetrieveNumberZoned implements FieldRetriever {
        public static final String CALC_ID = "NUMBER_ZONED";

        private static final String ALIAS_DISTRICT_NUMBER = "all-org-StateId";
        private static final String ALIAS_DISTRICT_NUMBER_Z = "all-std-ZonedDistrict";
        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";
        private static final String ALIAS_SCHOOL_NUMBER_Z = "all-std-ZonedSchool";

        private static final String PARAM_DISTRICT = "DISTRICT";
        private static final String PARAM_SCHOOL = "SCHOOL";

        private DataDictionaryField m_fieldDistrictNumber;
        private DataDictionaryField m_fieldDistrictNumberZ;
        private DataDictionaryField m_fieldSchoolNumber;
        private DataDictionaryField m_fieldSchoolNumberZ;

        /**
         * Instantiates a new retrieve number zoned.
         */
        public RetrieveNumberZoned() {
            m_fieldDistrictNumber = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER, true);
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
            m_fieldDistrictNumberZ = translateAliasToDictionaryField(ALIAS_DISTRICT_NUMBER_Z, true);
            m_fieldSchoolNumberZ = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER_Z, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;
            SisStudent student = (SisStudent) entity.getBean();
            if (PARAM_DISTRICT.equals(field.getParameter().toString())) {
                value = (String) flData.getFieldValue(student, m_fieldDistrictNumberZ);
                if (StringUtils.isEmpty(value)
                        && FIELD_VALUE_EDU_CHOICE_NOT_ZEROS.equals(entity.getFieldValue(FIELD_NAME_EDU_CHOICE))) {
                    value = (String) flData.getFieldValue(student.getOrganization1(), m_fieldDistrictNumber);
                }
            } else if (PARAM_SCHOOL.equals(field.getParameter().toString())) {
                value = (String) flData.getFieldValue(student, m_fieldSchoolNumberZ);
                if (StringUtils.isEmpty(value)
                        && FIELD_VALUE_EDU_CHOICE_NOT_ZEROS.equals(entity.getFieldValue(FIELD_NAME_EDU_CHOICE))) {
                    StudentInfo info = getStudentHelper().getStudentInfo(student);
                    SisSchool school = info.getSchool(getSurveyPeriod().getSnapshotDate());
                    if (school != null) {
                        value = (String) FLPriorSchoolStatusData.this.getFieldValue(school, m_fieldSchoolNumber);
                    }
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for Offender Transfer
     * http://www.fldoe.org/core/fileparse.php/15229/urlt/1617-175633.pdf
     *
     */
    protected class RetrieveOffender implements FieldRetriever {
        public static final String CALC_ID = "OFFENDER";

        private static final String ALIAS_OFFENDER = "all-enr-Offender";

        private static final String DEFAULT_VALUE = "N";
        private static final String Y_VALUE = "Y";

        private DataDictionaryField m_fieldOffender;

        /**
         * Instantiates a new retrieve offender.
         */
        public RetrieveOffender() {
            m_fieldOffender = translateAliasToDictionaryField(ALIAS_OFFENDER, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = DEFAULT_VALUE;
            StudentEnrollmentSpan span = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord()
                    .getStudentEnrollmentSpan();
            if (span != null && span.getEnrollments() != null && !span.getEnrollments().isEmpty()) {
                FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;
                for (StudentEnrollment enr : span.getEnrollments()) {
                    if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType())) {
                        Boolean bvalue = (Boolean) flData.getFieldValue(enr, m_fieldOffender);
                        if (bvalue != null && bvalue.booleanValue()) {
                            value = Y_VALUE;
                            break;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Prior School parameters.
     */
    protected class RetrievePriorSchool implements FieldRetriever {
        public static final String CALC_ID = "PRIOR_SCHOOL";

        private static final String ALIAS_PRIOR_SCHOOL_COUNTRY = "all-enr-PriorSchoolCountry";
        private static final String ALIAS_PRIOR_SCHOOL_COUNTY = "all-enr-PriorSchoolCounty";
        private static final String ALIAS_PRIOR_SCHOOL_STATE = "all-enr-PriorSchoolState";

        private static final String PARAM_COUNTY = "COUNTY";
        private static final String PARAM_STATE = "STATE";

        private DataDictionaryField m_fieldCountry;
        private DataDictionaryField m_fieldCounty;
        private DataDictionaryField m_fieldState;

        /**
         * Instantiates a new retrieve prior school.
         */
        public RetrievePriorSchool() {
            m_fieldCounty = translateAliasToDictionaryField(ALIAS_PRIOR_SCHOOL_COUNTY, true);
            m_fieldCountry = translateAliasToDictionaryField(ALIAS_PRIOR_SCHOOL_COUNTRY, true);
            m_fieldState = translateAliasToDictionaryField(ALIAS_PRIOR_SCHOOL_STATE, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            FLPriorSchoolStatusEntity flPssEntity = (FLPriorSchoolStatusEntity) entity;
            StudentEnrollmentSpanInfo span = flPssEntity.getCurrentRecord();
            StudentEnrollment enr = span.getPriorDistrictEnrollment();
            if (enr != null) {
                FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;
                String param = field.getParameter().toString();
                DataDictionaryField ddf = null;
                if (PARAM_COUNTY.equals(param)) {
                    ddf = m_fieldCounty;
                } else if (PARAM_STATE.equals(param)) {
                    ddf = m_fieldState;
                } else if (!TERM_CODE_NONENROLLMENT.equals(entity.getFieldValue(FIELD_NAME_TERM))) {
                    ddf = m_fieldCountry;
                }
                if (enr != null && ddf != null) {
                    value = flData.getFieldValue(enr, ddf);
                }
            }
            return value;
        }
    }

    /**
     * Retriever for student legal name.
     *
     * @author Follett Software Company
     */
    protected class RetrieveSchool implements FieldRetriever {
        public static final String CALC_ID = "STD_SCHOOL";

        private static final String ALIAS_SCHOOL_NUMBER = "all-skl-StateId";

        private DataDictionaryField m_fieldSchoolNumber;

        /**
         * Instantiates a new retrieve school.
         */
        public RetrieveSchool() {
            m_fieldSchoolNumber = translateAliasToDictionaryField(ALIAS_SCHOOL_NUMBER, true);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SisSchool school =
                    ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudentEnrollmentSpan().getSchool();
            if (school != null) {
                value = FLPriorSchoolStatusData.this.getFieldValue(school, m_fieldSchoolNumber);
            }
            return value;
        }
    }

    /**
     * Retriever for student legal name.
     *
     * @author Follett Software Company
     */
    protected class RetrieveStudentName implements FieldRetriever {
        public static final String CALC_ID = "STD_NAME";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudent();
            StudentInfo info = getStudentHelper().getStudentInfo(student);
            return info.formatStudentLegalName();
        }
    }

    /**
     * Field retriever for term code.
     */
    protected class RetrieveTermCode implements FieldRetriever {
        public static final String CALC_ID = "TERM_CODE";

        private static final String TERM_CODE_ANNUAL = "3";
        private static final String TERM_CODE_SUMMER = "S";

        private static final String WDRAW_CODE_DNE = "DNE";

        private DataDictionaryField m_fieldSheduleTermCode;

        /**
         * Instantiates a new retrieve term code.
         */
        public RetrieveTermCode() {
            m_fieldSheduleTermCode =
                    getDataDictionary().findDataDictionaryField(ScheduleTerm.class.getName(), ScheduleTerm.COL_CODE);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            FLPriorSchoolStatusData flData = (FLPriorSchoolStatusData) data;

            Object value = null;
            SisStudent student = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord().getStudent();
            PlainDate startDate = flData.getCurrentContext().getStartDate();
            PlainDate surveyEnd = flData.getSurveyPeriod().getEndDate();

            if (!SURVEY_PERIOD_2.equals(getSurveyPeriod().getCode())) {
                List<StudentScheduleInfo> schInfoList = m_studentScheduleHelper.getStudentScheduleInfo(student);
                StudentEnrollmentSpan span = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord()
                        .getStudentEnrollmentSpan();
                StudentEnrollment inactiveEnr = span.getFirstInactiveEnrollment();
                String wdrawCode = null;
                if (inactiveEnr != null) {
                    wdrawCode = inactiveEnr.getEnrollmentCode();
                }
                if (WDRAW_CODE_DNE.equals(wdrawCode)) {
                    value = TERM_CODE_NONENROLLMENT;
                } else {
                    for (StudentScheduleInfo schInfo : schInfoList) {
                        ScheduleTerm term = schInfo.getSection().getScheduleTerm();
                        for (ScheduleTermDate date : term.getScheduleTermDates()) {
                            if ((date.getStartDate().compareTo(startDate) >= 0 &&
                                    date.getStartDate().compareTo(surveyEnd) <= 0) ||
                                    (date.getEndDate().compareTo(startDate) >= 0 &&
                                            date.getEndDate().compareTo(surveyEnd) <= 0)) {
                                value = flData.getFieldValue(term, m_fieldSheduleTermCode);
                                if (m_summerTermCodes.contains(value)
                                        && SURVEY_PERIOD_5.equals(getSurveyPeriod().getCode())) {
                                    value = TERM_CODE_SUMMER;
                                } else {
                                    if (span != null) {
                                        StudentEnrollment activeEnr = span.getFirstActiveEnrollment();
                                        PlainDate entryDate = activeEnr.getEnrollmentDate();
                                        if (entryDate.before(startDate)) {
                                            value = TERM_CODE_NONENROLLMENT;
                                        } else {
                                            value = TERM_CODE_ANNUAL;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Field retriever for Withdrawal Code and Date.
     */
    protected class RetrieveWithdrawal implements FieldRetriever {
        public static final String CALC_ID = "WITHDRAWAL";

        private static final String PARAM_CODE = "CODE";

        private static final String STATE_WITHDRAWAL_CODE_DEFAULT = "W01";
        private static final String STATE_WITHDRAWAL_CODE_D = "D";
        private static final String STATE_WITHDRAWAL_CODE_W = "W";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentEnrollmentSpan span = ((FLPriorSchoolStatusEntity) entity).getCurrentRecord()
                    .getStudentEnrollmentSpan();
            if (span != null) {
                StudentEnrollment enr = span.getFirstInactiveEnrollment();
                if (enr != null) {
                    if (PARAM_CODE.equals(field.getParameter().toString())) {
                        // get the state reference code for enrEnrCode
                        value = data.lookupStateValue(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE,
                                enr.getEnrollmentCode());
                        if (!enr.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                            String code = value != null ? value.toString() : "";
                            value = (code.startsWith(STATE_WITHDRAWAL_CODE_D)
                                    || code.startsWith(STATE_WITHDRAWAL_CODE_W)) ? code : STATE_WITHDRAWAL_CODE_DEFAULT;
                        }
                    } else {
                        value = enr.getEnrollmentDate();
                    }
                }
            }
            return value;
        }
    }

    protected static final List<String> ENR_SURVEY_PERIOD_VALID_CODES = Arrays.asList(FLStateReportData.SURVEY_PERIOD_2,
            FLStateReportData.SURVEY_PERIOD_3, FLStateReportData.SURVEY_PERIOD_5);

    protected FLScheduleHelper m_scheduleHelper;
    protected StudentScheduleHelper m_studentScheduleHelper;
    private final Collection<String> m_summerTermCodes = Arrays.asList("4", "5", "S", "T", "U", "V", "W", "X");

    private static final String FIELD_NAME_EDU_CHOICE = "Educational Choice";
    private static final String FIELD_NAME_TERM = "Term";

    private static final String FIELD_VALUE_EDU_CHOICE_NOT_ZEROS = "A";

    private static final String TERM_CODE_NONENROLLMENT = "Y";

    private static final String VALID_GRADE_LEVEL_PATTERN = "^PK|KG|10|11|12|0[1-9]$";

    /**
     * Initialize the data module. Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(
     *      java.util.Map, com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        if (getSetupErrors().size() != 0) {
            return;
        }

        X2Criteria stdExtractCriteria = FLStudentExtractData.getExportStudentHelper(this).getStudentCriteria();
        SubQuery stdExtractSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdExtractCriteria);

        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                getCurrentContext().getStartDate());
        getStudentHelper().setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        getStudentHelper().getStudentCriteria().addIn(X2BaseBean.COL_OID, stdExtractSubQuery);

        setQuery(getStudentHelper().getStudentQuery(false));
        setEntityClass(FLPriorSchoolStatusEntity.class);

        m_scheduleHelper =
                new FLScheduleHelper(this, getCurrentContext().getStartDate(), getSurveyPeriod().getEndDate());
        m_studentScheduleHelper = getStudentHelper().new StudentScheduleHelper(m_scheduleHelper,
                getCurrentContext().getStartDate(), getSurveyPeriod().getEndDate());

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the valid survey periods.
     *
     * @return the valid survey periods
     */
    @Override
    public Collection<String> getValidSurveyPeriods() {
        return ENR_SURVEY_PERIOD_VALID_CODES;
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveFiscalYear.CALC_ID, new RetrieveFiscalYear());
        calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
        calcs.put(RetrieveStudentName.CALC_ID, new RetrieveStudentName());
        calcs.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        calcs.put(RetrieveEntry.CALC_ID, new RetrieveEntry());
        calcs.put(RetrievePriorSchool.CALC_ID, new RetrievePriorSchool());
        calcs.put(RetrieveWithdrawal.CALC_ID, new RetrieveWithdrawal());
        calcs.put(RetrieveDaysAnnual.CALC_ID, new RetrieveDaysAnnual());
        calcs.put(RetrieveDaysSummer.CALC_ID, new RetrieveDaysSummer());
        calcs.put(RetrieveTermCode.CALC_ID, new RetrieveTermCode());
        calcs.put(RetrieveEducationalChoice.CALC_ID, new RetrieveEducationalChoice());
        calcs.put(RetrieveOffender.CALC_ID, new RetrieveOffender());
        calcs.put(RetrieveDisasterAffected.CALC_ID, new RetrieveDisasterAffected());
        calcs.put(RetrieveDaysAbsentUnexcused.CALC_ID, new RetrieveDaysAbsentUnexcused());
        calcs.put(RetrieveHabitualTruant.CALC_ID, new RetrieveHabitualTruant());
        calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
        calcs.put(RetrieveNumberZoned.CALC_ID, new RetrieveNumberZoned());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(RetrieveSurveyPeriod.CALC_ID, new RetrieveSurveyPeriod());
        super.addValidators(validators);
    }
}
