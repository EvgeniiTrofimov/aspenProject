/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportValidationError;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscriptColumnDefinition;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.CsvField;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolClass.ClassWrapper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisStudentClassEnrolment.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisStudentClassEnrolment extends OnsisStudentScheduleCommon {

    /**
     * The Class OnsisStudentClassEnrollmentEntity.
     */
    public static class OnsisStudentClassEnrollmentEntity extends OnsisStudentScheduleCommonEntity {
        public static final String ELEMENT_AVERAGE_DAILY_ENROLMENT = "AVERAGE_DAILY_ENROLMENT";
        public static final String ELEMENT_COURSE_DELIVERY_TYPE = "COURSE_DELIVERY_TYPE";
        public static final String ELEMENT_COURSE_END_DATE = "COURSE_END_DATE";
        public static final String ELEMENT_COURSE_START_DATE = "COURSE_START_DATE";
        public static final String ELEMENT_NEW_COURSE_START_DATE = "NEW_COURSE_START_DATE";

        public static final String FIELD_ID_COURSE_END_DATE = "CourseEndDate";
        public static final String FIELD_ID_COURSE_START_DATE = "CourseStartDate";


        private static final String CODE_DIPLOMA_REQ_MET_ELECTIVE = "ELE";

        private static final String FIELD_ID_COURSE_INCOMPLETE_FLAG = "CourseIncompleteFlag";
        private static final String FIELD_ID_CREDIT_VALUE = "Credit Value";
        private static final String FIELD_ID_FINAL_MARK = "FinalMark";
        private static final String FIELD_ID_INSTITUTION_TYPE = "InstitutionType";
        private static final String FIELD_ID_WITHDRAWAL_TYPE = "WithdrawalType";

        private static final String FINAL_GRADE_I = "I";
        private static final String FINAL_MARK_0 = "0";

        private static final String STD_PERIOD_ATT_G_DAY_CODE = "G";
        private static final String STD_PERIOD_ATT_C_CODE = "C";

        private static final String VALUE_FLAG_TRUE = "T";

        private Range<Date>[] m_dateRange;
        private OnsisStudentClassEnrolment m_reportData;

        /**
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            OnsisStudentScheduleSpan span = getSpan();
            boolean include = span.getExitDate() == null
                    || span.getExitDate().after((getGlobalData().getDateRange().getStart()))
                    || getCsvCourseStartDate() != null;
            // used for skip only - no error reporting
            return include ? null : new StateReportValidationError("", "", "", "");
        }

        /**
         * Gets the earned credit value.
         *
         * @return Big decimal
         */
        public BigDecimal getAttemptedCreditValue() {
            BigDecimal value = null;

            try {
                value = getSpan().getSection().getCredit();
            } catch (Exception e1) {
                // ignore exception
            }

            if (getSpan().getTranscript() != null) {
                try {
                    value = new BigDecimal(getSpan().getTranscript().getPotentialCredit());
                } catch (Exception e1) {
                    // ignore exception
                }
            }

            return value;
        }

        /**
         * Gets the classes missed.
         *
         * @return int
         */
        public String getClassesAbsent() {
            String value = null;
            boolean isMCE0089 = isMCE0089();
            // String wType = getFieldValue(FIELD_ID_WITHDRAWAL_TYPE);
            // String crsEndDt = getFieldValue(FIELD_ID_COURSE_END_DATE);

            if (isMCE0089) {
                StudentScheduleSpanSet spanSet = getSpanSet();
                OnSection section = (OnSection) spanSet.getSubmissionSpan().getSection();
                if (section != null) {
                    Map<PlainDate, List<ToolStudentPeriodAttendance>> attendanceValues = section
                            .getStudentPeriodAttendances(getBroker())
                            .getGroup(ToolStudentPeriodAttendance.FIELD_STUDENT_OID, getStudent().getOid()).stream()
                            .filter(pat -> pat.getAbsentIndicator()
                                    || STD_PERIOD_ATT_C_CODE.equals(pat.getOtherCode())
                                    || STD_PERIOD_ATT_C_CODE.equals(pat.getOtherCode02())
                                    || STD_PERIOD_ATT_G_DAY_CODE.equals(pat.getOtherCode())
                                    || STD_PERIOD_ATT_G_DAY_CODE.equals(pat.getOtherCode02()))
                            .collect(Collectors.groupingBy(ToolStudentPeriodAttendance::getDate));
                    value = String.valueOf(getPeriodAttendanceNum(attendanceValues));
                }
            }

            // getReportData().log("Calculated CLASSES_MISSED[" + value + "] isMCE0089[" + isMCE0089
            // + "] wType[" + wType + "] crsEndDt[" + crsEndDt + "]");
            return value;
        }

        /**
         * Gets the classes absent for closure.
         *
         * @return String
         */
        public String getClassesAbsentForClosure() {
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            String value = null;
            boolean isMCE0089 = isMCE0089();
            // String wType = getFieldValue(FIELD_ID_WITHDRAWAL_TYPE);
            // String crsEndDt = getFieldValue(FIELD_ID_COURSE_END_DATE);

            if (isMCE0089) {
                StudentScheduleSpanSet spanSet = getSpanSet();
                OnSection section = (OnSection) spanSet.getSubmissionSpan().getSection();
                if (section != null) {
                    Map<PlainDate, List<ToolStudentPeriodAttendance>> attendanceValues = section
                            .getStudentPeriodAttendances(getBroker())
                            .getGroup(ToolStudentPeriodAttendance.FIELD_STUDENT_OID, getStudent().getOid()).stream()
                            .map(pat -> {
                                if (debugOutput != null) {
                                    debugOutput.append("getClassesAbsentForClosure candidate: " + pat + "\n");
                                }
                                return pat;
                            })
                            .filter(pat -> (STD_PERIOD_ATT_G_DAY_CODE.equals(pat.getOtherCode())
                                    || STD_PERIOD_ATT_G_DAY_CODE.equals(pat.getOtherCode02()))
                                    && m_reportData.getClosureReasonCodes().contains(pat.getReasonCode()))
                            .map(pat -> {
                                if (debugOutput != null) {
                                    debugOutput.append("getClassesAbsentForClosure selected: " + pat + "\n");
                                }
                                return pat;
                            })
                            .collect(Collectors.groupingBy(ToolStudentPeriodAttendance::getDate));
                    value = String.valueOf(getPeriodAttendanceNum(attendanceValues));
                }
            }

            // getReportData().log("Calculated CLASSES_MISSED[" + value + "] isMCE0089[" + isMCE0089
            // + "] wType[" + wType + "] crsEndDt[" + crsEndDt + "]");
            if (debugOutput != null) {
                getReportData().log(debugOutput.toString());
            }
            return value;
        }

        /**
         * Gets the classes tardy.
         *
         * @return int
         */
        public String getClassesTardy() {
            String value = null;
            if (isMCE0089()) {
                StudentScheduleSpanSet spanSet = getSpanSet();
                OnSection section = (OnSection) spanSet.getSubmissionSpan().getSection();
                if (section != null) {
                    Map<PlainDate, List<ToolStudentPeriodAttendance>> attendanceValues = section
                            .getStudentPeriodAttendances(getBroker())
                            .getGroup(ToolStudentPeriodAttendance.FIELD_STUDENT_OID, getStudent().getOid()).stream()
                            .filter(pat -> pat.getTardyIndicator())
                            .collect(Collectors.groupingBy(ToolStudentPeriodAttendance::getDate));
                    value = String.valueOf(getPeriodAttendanceNum(attendanceValues));
                }
            }
            return value;
        }

        /**
         * Gets the compulsory course flag.
         *
         * Check the credit by requirement detail: GP1:1.0|ELE:0.5 -> {GP1=1.0,ELE=0.5}
         *
         * Course is compulsory if the credit requirement detail:
         * 1. Not blank
         * 2. AND a credit wasn't given for elective
         *
         * @return Boolean
         */
        public Boolean getCompulsoryCourseFlag() {
            if (isDCCCourse()) {
                return Boolean.FALSE;
            }

            OnTranscript transcript = (OnTranscript) getSpan().getTranscript();
            if (transcript == null) {
                return Boolean.FALSE;
            }

            // Credit requirement is not blank
            String creditByReqMetValue = transcript.getCreditByRequirementDetail();
            if (StringUtils.isBlank(creditByReqMetValue)) {
                return Boolean.FALSE;
            }

            // Elective credit wasn't given
            char delimiter = '|';
            char subDelimiter = ':';
            boolean trim = true;
            Map<String, String> creditByReqMet =
                    StringUtils.convertDelimitedStringToMap(creditByReqMetValue, delimiter, subDelimiter, trim);
            if (!creditByReqMet.containsKey(CODE_DIPLOMA_REQ_MET_ELECTIVE)) {
                return Boolean.TRUE;
            }

            return Boolean.FALSE;
        }

        /**
         * This field is not populated unless there is a transcript.
         * If the transcript has a transcript definition with a column where gtcColumnType =
         * TranscriptColumnDefinition.COLUMN_TYPE_DATE
         * and gtcDateType = TranscriptColumnDefinition.DATE_TYPE_COMPLETION_DATE,
         * then use the value from the transcript. If the value is on or before the submission date,
         * then the complete flag is true.
         * If this column does not exist, then determine if the student schedule span end date
         * includes the last day of the section.
         * If the last day is included and the submission date is on or after the last day, the
         * complete flag is true.
         *
         * 2020-11-03 Where FINAL_MARK= I in Aspen:
         * - set FINAL_MARK = 0
         * - COURSE_COMPLETE_FLAG = F
         * - and COURSE_INCOMPLETE_FLAG=T.
         *
         *
         * @return Course Complete Flag
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getEntityBean()
         */
        public Boolean getCourseCompleteFlag() {
            if (FINAL_GRADE_I.equals(getAspenFinalGrade())) {
                return Boolean.FALSE;
            }
            if (VALUE_FLAG_TRUE.equals(getFieldValue(FIELD_ID_COURSE_INCOMPLETE_FLAG))) {
                return Boolean.FALSE;
            }

            OnTranscript transcript = (OnTranscript) getSpan().getTranscript();
            if (transcript != null) {
                PlainDate completionDate = transcript.getDateCompleted();
                if (completionDate != null) {
                    return !completionDate.after(getGlobalData().getEndDate());
                } else if (isConed() && !StringUtils.isEmpty(getFieldValue(FIELD_ID_COURSE_END_DATE))
                        && getFinalMarkDouble() > 0.0) {
                    // MCE0035
                    return true;
                } else {
                    PlainDate exitDate = getSpan().getExitDate();
                    if (exitDate != null) {
                        List<ToolMasterTerm> masterTerms = getSpan().getSection().getMasterTerms(getBroker());
                        boolean includesLastDateOfTheSection = false;
                        PlainDate lastDate = null;
                        for (ToolMasterTerm masterTerm : masterTerms) {
                            Collection<ToolScheduleTermDate> scheduleTermDates =
                                    masterTerm.getScheduleTerm(getBroker()).getScheduleTermDates(getBroker());
                            for (ToolScheduleTermDate scheduleTermDate : scheduleTermDates) {
                                PlainDate termEndDate = scheduleTermDate.getEndDate();
                                if (lastDate == null || lastDate.before(termEndDate)) {
                                    lastDate = termEndDate;
                                }
                            }
                        }
                        if (lastDate != null) {
                            if (!exitDate.before(lastDate)) {
                                includesLastDateOfTheSection = true;
                            }
                        }
                        return includesLastDateOfTheSection && !lastDate.after(getGlobalData().getEndDate());
                    }
                }
            }
            return isConed() ? Boolean.FALSE : null;
        }

        /**
         * Gets the span.
         *
         * @return Student schedule span
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getEntityBean()
         */
        public PlainDate getCourseEndDate() {
            PlainDate endDate = (PlainDate) getDateRange().getEnd();

            if (endDate != null && endDate.after(getGlobalData().getEndDate())) {
                endDate = null;
            }

            return endDate;
        }

        /**
         * Gets the course incomplete flag.
         *
         * 2020-11-03 Where FINAL_MARK= I in Aspen:
         * - set FINAL_MARK = 0
         * - COURSE_COMPLETE_FLAG = F
         * - and COURSE_INCOMPLETE_FLAG=T.
         *
         * @return boolean
         */
        public boolean getCourseIncompleteFlag() {
            if (FINAL_GRADE_I.equals(getAspenFinalGrade())) {
                return Boolean.TRUE;
            }
            if (isConed() && getSpan().getTranscript() == null) {
                return Boolean.TRUE;
            }
            return !StringUtils.isEmpty(getFieldValue(FIELD_ID_WITHDRAWAL_TYPE));
        }

        /**
         * Gets the course repeated flag.
         *
         * @return boolean
         */
        public boolean getCourseRepeatedFlag() {
            StudentScheduleSpan span = getSpan();

            if (span != null) {
                OnTranscript transcript = (OnTranscript) span.getTranscript();

                if (transcript != null) {
                    return OnTranscript.FLAG_REPEATED.equals(transcript.getCourseRepeated());
                }
            }

            return false;
        }

        /**
         * Get Course Entry Date for export.
         *
         * @return Plain date
         */
        public PlainDate getCourseStartDate() {
            return (PlainDate) getDateRange().getStart();
        }

        /**
         * Gets the course sbst flag.
         *
         * @return boolean
         */
        public boolean getCourseSbstFlag() {
            Boolean value = Boolean.FALSE;
            if (!isDCCCourse()) {
                OnTranscript transcript = (OnTranscript) getSpan().getTranscript();
                if (transcript != null && !StringUtils.isEmpty(transcript.getEquivalentSchoolCourseOid())) {
                    value = Boolean.TRUE;
                }
            }
            return value;
        }

        /**
         * Gets the crs delivery type.
         *
         * @return String
         */
        public String getCrsDeliveryType() {
            String value = null;
            OnTranscript transcript = (OnTranscript) getSpan().getTranscript();
            if (transcript != null) {
                value = transcript.getCourseDeliveryType();
            }
            if (StringUtils.isEmpty(value)) {
                OnSection section = (OnSection) getSpan().getSection();
                if (section != null) {
                    value = section.getCourseDeliveryType();
                }
            }
            return value;
        }

        /**
         * Gets the earned credit value.
         *
         * @return Big decimal
         */
        public BigDecimal getEarnedCreditValue() {
            BigDecimal value = null;
            // MCE0039 - If Final Mark is less than 50, then Earned Credit must be equal to 0.
            if (isFailingMark()) {
                return BigDecimal.ZERO;
            }

            // MCE0026 - If Withdrawal/Dropped Type Code = Dropped,
            // then Earned Credit Value must be = 0.
            if (OnTranscript.WITHDRAWAL_TYPE_D.equals(getFieldValue(FIELD_ID_WITHDRAWAL_TYPE))) {
                return BigDecimal.ZERO;
            }
            if (!StringUtils.isEmpty(getFieldValue(FIELD_ID_COURSE_END_DATE))) {
                if (getSpan().getTranscript() != null) {
                    value = getSpan().getTranscript().getTotalCredit();
                }
            }

            /*
             * If the COURSE_REPEAT_FLAG=T and FINAL_MARK>=50,
             * then export EARNED_CREDIT_VALUE pull value from CREDIT_VALUE
             */
            if (getCourseRepeatedFlag()) {
                if (isPassingMark()) {
                    String creditValue = getFieldValue(FIELD_ID_CREDIT_VALUE);
                    value = BigDecimal.valueOf(0);

                    try {
                        value = new BigDecimal(creditValue);
                    } catch (Exception e) {
                        // TODO: handle exception
                    }
                }
            }

            if (value == null && isConed() && getCourseIncompleteFlag()) {
                // MCE0047
                return BigDecimal.ZERO;
            }

            return value;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            return getSpan().toString();
        }

        /**
         * Gets the final mark.
         *
         * 2020-11-03 Where FINAL_MARK= I in Aspen:
         * - set FINAL_MARK = 0
         * - COURSE_COMPLETE_FLAG = F
         * - and COURSE_INCOMPLETE_FLAG=T.
         *
         * @return String
         */
        public String getFinalMark() {
            String value = null;
            if (!StringUtils.isEmpty(getFieldValue(FIELD_ID_COURSE_END_DATE)) || isConed()) {
                value = getAspenFinalGrade();
                if (FINAL_GRADE_I.equals(value)
                        || OnTranscript.WITHDRAWAL_TYPE_D.equals(getFieldValue(FIELD_ID_WITHDRAWAL_TYPE))) {
                    value = FINAL_MARK_0;
                }
                if (!StringUtils.isEmpty(value)) {
                    value = StringUtils.isNumeric(value) ? value : null;
                }
            }
            return value;
        }

        /**
         * Gets the final mark double.
         *
         * @return Double
         */
        public Double getFinalMarkDouble() {
            Double finalMark = 0.0;
            String finalMarkStr = getFinalMark();
            if (StringUtils.isNumeric(finalMarkStr)) {
                try {
                    finalMark = Double.valueOf(finalMarkStr);
                } catch (Exception e) {
                    // return null
                }
            }
            return finalMark;
        }

        /**
         * Gets the iep flag.
         *
         * @return Boolean
         */
        public Boolean getIepFlag() {
            Boolean iepFlag = null;
            if (isMCE0089()) {
                iepFlag = Boolean.FALSE;
                OnsisStudentScheduleSpan span = getSpan();
                ToolGradeTermDate gta = span.getGradeTermDate(getBroker(), getGlobalData().getDateRange());

                if (gta == null || span.getTranscript() == null) {
                    return Boolean.FALSE;
                }

                ToolTranscriptDefinition gtd = span.getTranscript().getTranscriptDefinition(getBroker());
                if (gtd == null) {
                    return Boolean.FALSE;
                }

                OnTranscriptColumnDefinition iepGtc = gtd.getTranscriptColumnDefinitions(getBroker()).stream()
                        .map(gtc -> (OnTranscriptColumnDefinition) gtc)
                        .filter(gtc -> gta.getGradeTermOid().equals(gtc.getGradeTermOid())
                                && "IEP".equals(gtc.getReferenceEntryType()))
                        .findAny().orElse(null);

                if (iepGtc != null) {

                    Object value = ((OnTranscript) span.getTranscript()).getAliasValue(iepGtc.getAlias(),
                            getDictionaryExtractor());
                    if (value instanceof String && !StringUtils.isEmpty((String) value)) {
                        iepFlag = Boolean.TRUE;
                    } else if (value != null) {
                        iepFlag = Boolean.TRUE;
                    }
                }
            }
            return iepFlag;
        }

        /**
         * Gets the language program.
         *
         * @return String
         */
        public String getLanguageProgram() {
            String result = null;
            if (isMCE0089()) {
                ClassWrapper classWrapper = getClassEntity();
                ToolBean classEntity = classWrapper.getClassEntity();

                OnSection firstSection = classWrapper.getFirstSection();

                if (classEntity instanceof OnSection) {
                    result = firstSection.getLanguageProgram();
                }

                if (classEntity instanceof ToolScheduleClass) {
                    ToolScheduleClass scheduleClass = (ToolScheduleClass) classEntity;
                    Collection<ToolSection> sections = scheduleClass.getSections(getBroker());
                    if (!sections.isEmpty()) {
                        result = firstSection.getLanguageProgram();
                    }
                }
            }

            return result;
        }

        /**
         * Get New Course Entry Date for export.
         *
         * @return Plain date
         */
        public PlainDate getNewCourseStartDate() {
            /*
             * NEW_COURSE_START_DATE should only export
             * if this is an Update (Course Entry Date is in the CSV),
             * and the current calculated Course Entry Date
             * is different from the original CSV Course Entry Date.
             */
            PlainDate csvCourseStartDate = getCsvCourseStartDate();
            if (csvCourseStartDate == null) {
                return null;
            }

            PlainDate calculatedCourseStartDate = (PlainDate) getDateRange().getStart();
            if (!csvCourseStartDate.equals(calculatedCourseStartDate)) {
                if (getGlobalData().getDateRange().contains(calculatedCourseStartDate)) {
                    return calculatedCourseStartDate;
                }
            }

            return null;
        }

        /**
         * Gets the number of periods.
         *
         * @return int
         */
        public String getNumberOfPeriods() {
            String value = null;

            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            if (isMCE0089()) {
                PlainDate startDate = getFieldAdjustedStartDate();
                StudentScheduleSpanSet spanSet = getSpanSet();
                OnSection section = (OnSection) spanSet.getSubmissionSpan().getSection();

                value = String.valueOf(getSpanSet().getSpans().stream()
                        .mapToInt(span -> {
                            PlainDate start = startDate.after(span.getEntryDate()) ? startDate : span.getEntryDate();
                            int numPeriods = section.getTotalNumberPeriods(
                                    start,
                                    span.getLastMembershipDate(), getBroker(), debugOutput);
                            if (debugOutput != null) {
                                debugOutput.append("getTotalPeriodsNumber() dates are: " + start + "-"
                                        + span.getLastMembershipDate() + " containing " + numPeriods + " periods\n");
                            }
                            return numPeriods;
                        })
                        .sum());
            }

            if (debugOutput != null) {
                debugOutput.append("getTotalPeriodsNumber() is : " + value + "\n");
                m_reportData.log(debugOutput.toString());
            }
            return value;
        }

        /**
         * Gets the schedule term code.
         *
         * @return String
         */
        public String getScheduleTermCode() {
            ToolScheduleTerm term = getSpan().getSection().getScheduleTerm(getBroker());
            return getDictionaryExtractor().getStateValue(term, ToolScheduleTerm.FIELD_CODE);
        }

        /**
         * Gets the withdrawal date.
         *
         * @return Plain date
         */
        public PlainDate getWithdrawalDate() {
            StudentScheduleSpan span = getSpan();

            if (!OnTranscript.WITHDRAWAL_TYPE_D.equals(getWithdrawalType()) && checkWithdrawalPopulation(span)) {
                return span.getExitDate();
            }
            return null;
        }

        /**
         * Gets the withdrawal type.
         *
         * @return String
         */
        public String getWithdrawalType() {
            return getWithdrawalType(getSpan());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            /*
             * Primary query returns a Student.
             * Entity rows are one per enrollment span.
             */
            super.intitialize(data, bean);

            m_reportData = (OnsisStudentClassEnrolment) data;
            GlobalData globalData = getGlobalData();
            Range<Date> enrollmentDateRange = Range.of(m_reportData.getParentEntity().getOnsisEnrollmentStartDate(),
                    m_reportData.getParentEntity().getOnsisEnrollmentEndDate());
            List<OnsisStudentScheduleSpan> scheduleSpans =
                    getStudent().getStudentScheduleSpans(getBroker(), globalData, enrollmentDateRange);

            if (getGlobalData().getDebugDetail()) {
                StringBuilder debugOutput = new StringBuilder();
                debugOutput.append("OnsisStudentClassEnrollmentEntity.intitialize - candidate spans\n");
                debugOutput.append("Extract schoolOids: " + globalData.getCurrentSchoolOids() + "\n");
                debugOutput.append("Enrollment date range: " + enrollmentDateRange + "\n");
                getStudent().getStudentScheduleSpans(getBroker()).forEach(sp -> {
                    OnsisStudentScheduleSpan span = (OnsisStudentScheduleSpan) sp;
                    debugOutput.append("Span: " + span.toString() + "\n");
                    debugOutput
                            .append("schoolOid: " + span.getSection().getSchedule(getBroker()).getSchoolOid() + "\n");
                    debugOutput.append("dateRange: " + span.getDateRange() + "\n");
                    debugOutput.append("isSectionIncluded: "
                            + globalData.isSectionIncluded().test((OnSection) span.getSection()) + "\n");
                    debugOutput.append("lastMembershipDate: " + span.getLastMembershipDate() + "\n");
                    debugOutput.append("exitDate: " + span.getExitDate() + "\n");
                    debugOutput.append("calculateCourseStartDate: " + span.calculateCourseStartDate(getBroker(),
                            globalData.getDateRange(), m_reportData.getParentEntity().getOnsisEnrollmentStartDate())
                            + "\n");
                });
                getReportData().log(debugOutput.toString());
            }

            m_studentScheduleSpans.addAll(scheduleSpans.stream()
                    .filter(scheduleSpan -> {
                        if (globalData.getSubmissionType().isECPPSubmission()
                                && scheduleSpan.getTranscript() != null) {
                            String bsid = null;
                            bsid = ((OnTranscript) scheduleSpan.getTranscript()).getBsidCreditEarned();
                            if (!StringUtils.isEmpty(bsid)) {
                                Collection<OnSchool> schools = globalData.getSchoolByBsid(bsid);
                                String specialCondition = schools == null || schools.isEmpty() ? null
                                        : schools.stream()
                                                .map(skl -> skl.getSpecialCondition())
                                                .filter(condition -> OnSchool.SPECIAL_CONDITION_EXCLUDE_FROM_TRANSCRIPT
                                                        .contains(condition))
                                                .findAny().orElse(null);
                                if (specialCondition == null) {
                                    return false;
                                }
                            }
                        }
                        return true;
                    })
                    .collect(Collectors.groupingBy(span -> getKey(span))).values().stream()
                    .map(item -> new StudentScheduleSpanSet(item)).collect(Collectors.toList()));

            // for consistency between runs
            Collections.sort(m_studentScheduleSpans, new Comparator<StudentScheduleSpanSet>() {
                @Override
                public int compare(StudentScheduleSpanSet o1List, StudentScheduleSpanSet o2List) {
                    StudentScheduleSpan o1 = o1List.getSubmissionSpan();
                    StudentScheduleSpan o2 = o2List.getSubmissionSpan();

                    // CourseView
                    int compare = o1.getSection().getCourseView().compareTo(o2.getSection().getCourseView());

                    // Entry Date
                    if (compare == 0) {
                        compare = o1.getEntryDate().compareTo(o2.getEntryDate());
                    }

                    // Withdrawal Type
                    if (compare == 0) {
                        compare = getWithdrawalType(o1).compareTo(getWithdrawalType(o2));
                    }

                    return compare;
                }
            });

            m_dateRange = (Range<Date>[]) Array.newInstance(Range.class, m_studentScheduleSpans.size());
            setRowCount(m_studentScheduleSpans.size());
        }

        /**
         * After render row fields.
         *
         * @param entityElement Element
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#afterRenderRowFields(org.w3c.dom.Element)
         */
        @Override
        protected void afterRenderRowFields(Element entityElement) {
            super.afterRenderRowFields(entityElement);


            List<Element> adeElements =
                    OnsisStateReportData.getChildElements(ELEMENT_AVERAGE_DAILY_ENROLMENT, entityElement);
            if (adeElements != null && !adeElements.isEmpty()) {
                String startDateToCheck =
                        OnsisStateReportData.getChildText(ELEMENT_NEW_COURSE_START_DATE, entityElement);
                if (StringUtils.isBlank(startDateToCheck)) {
                    startDateToCheck = OnsisStateReportData.getChildText(ELEMENT_COURSE_START_DATE, entityElement);
                }
                String courseEndDateStr = OnsisStateReportData.getChildText(ELEMENT_COURSE_END_DATE, entityElement);

                if (!StringUtils.isBlank(startDateToCheck)
                        && startDateToCheck.equals(courseEndDateStr)) {
                    PlainDate endDate = OnsisConstants.parseDate(courseEndDateStr,
                            OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES);
                    endDate = DateUtils.add(endDate, 1);
                    Element endDateElement =
                            OnsisStateReportData.getChildElement(ELEMENT_COURSE_END_DATE, entityElement);
                    endDateElement.setTextContent(
                            OnsisConstants.formatDate(endDate, OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES));
                }
            }

            /*
             * Don't send <OTHER_COURSE_INFO> that have Action=Update.
             * Because OTHER_COURSE_INFO is an all-key object
             * so it's either Add or Delete but never changed via Update.
             *
             * Updates are removed here by the parent
             * after call to OnsisSscOtherCourseInfo.generateDeletes()
             * so that Deletes won't be generated
             * for OtherCourseInfo that were Updates.
             */
            List<Element> otherCourseInfoUpdates =
                    getElementsWithChildValue(OnsisSscOtherCourseInfo.ELEMENT_OTHER_COURSE_INFO, entityElement,
                            OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_UPDATE);
            for (Element otherCourseInfoUpdate : otherCourseInfoUpdates) {
                entityElement.removeChild(otherCourseInfoUpdate);
            }

            /*
             * If COURSE_DELIVERY_TYPE=4 (CO-OP) AND OTHER_COURSE_INFO/OTHER_COURSE_INFO_TYPE=1
             * (Credit Recovery),
             * then export COURSE_DELIVERY_TYPE=1 (Regular Day)
             */
            // String courseDeliveryTypeValue = null;
            // Element courseDeliveryType = getChildElement(ELEMENT_COURSE_DELIVERY_TYPE,
            // entityElement);
            // if (courseDeliveryType != null) {
            // courseDeliveryTypeValue = courseDeliveryType.getTextContent();
            // if (courseDeliveryTypeValue != null) {
            // courseDeliveryTypeValue = courseDeliveryTypeValue.trim();
            // }
            // }
            //
            // if (COURSE_DELIVERY_TYPE_COOP.equals(courseDeliveryTypeValue)) {
            // List<Element> otherCourseInfoWithTypeCreditRecovery =
            // getElementsWithChildValue(ELEMENT_OTHER_COURSE_INFO, entityElement,
            // ELEMENT_OTHER_COURSE_INFO_TYPE, BeanHelperTranscripts.OCI_TYPE_CREDIT_RECOVERY);
            // if (otherCourseInfoWithTypeCreditRecovery.size() > 0) {
            // courseDeliveryType.setTextContent(COURSE_DELIVERY_TYPE_REGULAR_DAY);
            // }
            // }
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement Element
         * @param parentElement Element
         * @return true, if is row canceled
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.w3c.dom.Element,
         *      org.w3c.dom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            /*
             * Don't export STUDENT_CLASS_ENROLMENT
             * if adjusted COURSE_START_DATE==COURSE_END_DATE
             */

            String startDateToCheck = OnsisStateReportData.getChildText(ELEMENT_NEW_COURSE_START_DATE, entityElement);
            if (StringUtils.isBlank(startDateToCheck)) {
                startDateToCheck = OnsisStateReportData.getChildText(ELEMENT_COURSE_START_DATE, entityElement);
            }
            String courseEndDateStr = OnsisStateReportData.getChildText(ELEMENT_COURSE_END_DATE, entityElement);

            if (!StringUtils.isBlank(startDateToCheck)
                    && startDateToCheck.equals(courseEndDateStr)) {
                return true;
            }
            return false;
        }

        /**
         * Return the Final Grade from the Transcript,
         * subject to the date checks.
         *
         * @return String
         */
        private String getAspenFinalGrade() {
            if (getSpan().getTranscript() == null
                    || StringUtils.isEmpty(getFieldValue(FIELD_ID_COURSE_END_DATE))) {
                return null;
            }
            return getSpan().getTranscript().getFinalGrade();
        }

        /**
         * Check withdrawal population.
         *
         * @param span StudentScheduleSpan
         * @return true, if successful
         */
        private boolean checkWithdrawalPopulation(StudentScheduleSpan span) {
            boolean value = false;
            if (!span.getExitDate().after(m_reportData.getGlobalData().getEndDate())) {
                PlainDate termEndDate = span.getSection().getTermEndDate(getBroker());
                value = span.getExitDate().before(termEndDate);
                if (span.getTranscript() != null) {
                    OnTranscript transcript = (OnTranscript) span.getTranscript();
                    value = transcript.getDateCompleted() == null;
                }
            }
            return value;
        }

        /**
         * Gets the csv course start date.
         *
         * @return Plain date
         */
        private PlainDate getCsvCourseStartDate() {
            OnsisExtractRecords matcher =
                    getGlobalData().getExtractHelper().getMatcherByExtractType(EXTRACT_TYPE_ENR_SSC);

            // Initialize members
            OnsisCsvDataRecord csvRecord = matcher == null ? null
                    : matcher.findRecord(
                            Arrays.asList(
                                    OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString(),
                                    OnsisExtractHelper.CsvField.OEN.toString(),
                                    OnsisExtractHelper.CsvField.CLASS_CODE.toString(),
                                    OnsisExtractHelper.CsvField.MINISTRY_DFND_CRS.toString(),
                                    OnsisExtractHelper.CsvField.STD_CLASS_LANGUAGE_TYPE.toString()),
                            Arrays.asList(
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.SCHOOL_NUMBER.toString()),
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.OEN.toString()),
                                    deepGetFieldValueByFieldName(OnsisExtractHelper.CsvField.CLASS_CODE.toString()),
                                    StringUtils.emptyIfNull(deepGetFieldValueByFieldName(
                                            OnsisExtractHelper.CsvField.MINISTRY_DFND_CRS.toString())),
                                    deepGetFieldValueByFieldName(
                                            OnsisExtractHelper.CsvField.STD_CLASS_LANGUAGE_TYPE.toString())));
            if (csvRecord == null) {
                return null;
            }

            return getCsvDate(csvRecord, OnsisExtractHelper.CsvField.COURSE_START_DATE);
        }

        private Range<Date> getDateRange() {
            Range<Date> dateRange = m_dateRange[getCurrentRow()];
            if (dateRange == null) {
                /*
                 * If Course Entry Date is in the CSV,
                 * it needs to publish the same here.
                 *
                 * If a changed Course Entry Date value needs to export,
                 * it should export in NEW_COURSE_START_DATE
                 */
                PlainDate courseStartDate = getCsvCourseStartDate();

                PlainDate enrStartDate = m_reportData.getParentEntity().getOnsisEnrollmentStartDate();
                PlainDate enrEndDate = m_reportData.getParentEntity().getOnsisEnrollmentEndDate();
                if (courseStartDate == null) {
                    courseStartDate = ((OnsisStudentScheduleSpan) getEntrySpan())
                            .calculateCourseStartDate(getGlobalData(), enrStartDate);
                    /*
                     * November 9th, 2020
                     *
                     * If ADD and start date is before submission start date,
                     * export submission start date as COURSE_START_DATE.
                     */
                    if (courseStartDate != null && courseStartDate.before(getGlobalData().getStartDate())) {
                        courseStartDate = getGlobalData().getStartDate();
                    }
                }

                /*
                 * 1. If there IS a Transcript Completion Date,
                 * adjust backward to Term End Date if necessary.
                 */
                PlainDate courseEndDate = getSpan().getTranscript() == null ? null
                        : ((OnTranscript) getSpan().getTranscript()).getDateCompleted();
                if (courseEndDate != null) {
                    OnSchool school = null;
                    if (school == null && getSpan().getSection() != null) {
                        school = (OnSchool) getSpan().getSection().getSchool(getBroker());
                    }
                    OnSection masterScheduleInfo = (OnSection) getSpan().getTranscript().getSection(getBroker());
                    if (masterScheduleInfo != null) {
                        PlainDate termEndDate =
                                masterScheduleInfo.getTermEndDate(getBroker(), getGlobalData().getDateRange());
                        if (termEndDate != null && termEndDate.before(courseEndDate)) {
                            courseEndDate = termEndDate;
                        }
                    }
                }

                /*
                 * 2. If there is NOT a Transcript Completion Date,
                 * get the span exit date which is earliest of:
                 * - Term End Date
                 * - Course Drop Date
                 */
                if (courseEndDate == null) {
                    courseEndDate = getSpan().getExitDate();
                    if (getSpan().getSection() != null) {
                        OnSection masterScheduleInfo = (OnSection) getSpan().getSection();
                        PlainDate termEndDate =
                                masterScheduleInfo.getTermEndDate(getBroker(), getGlobalData().getDateRange());
                        if (termEndDate != null && termEndDate.before(courseEndDate)) {
                            courseEndDate = termEndDate;
                        }
                    }
                }
                if (courseEndDate != null && enrEndDate != null && enrEndDate.before(courseEndDate)) {
                    courseEndDate = enrEndDate;
                }
                if (courseStartDate.equals(courseEndDate)) {
                    if (enrEndDate == null || courseEndDate.before(enrEndDate)) {
                        Calendar cal = Calendar.getInstance();
                        cal.setTime(courseEndDate);
                        cal.add(Calendar.DAY_OF_YEAR, 1);
                        courseEndDate = new PlainDate(cal.getTime());
                    } else {
                        Calendar cal = Calendar.getInstance();
                        cal.setTime(courseStartDate);
                        cal.add(Calendar.DAY_OF_YEAR, -1);
                        courseStartDate = new PlainDate(cal.getTime());
                    }
                }
                dateRange = Range.of(courseStartDate, courseEndDate);
                m_dateRange[getCurrentRow()] = dateRange;
            }
            return dateRange;
        }

        /**
         * Gets the field adjusted start date.
         *
         * @return Plain date
         */
        private PlainDate getFieldAdjustedStartDate() {
            String startDateString = getFieldValue(FIELD_ID_COURSE_START_DATE);
            PlainDate startDate =
                    OnsisConstants.parseDate(startDateString, OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES);
            return (startDate != null && startDate.after(getEntrySpan().getEntryDate())) ? startDate
                    : getEntrySpan().getEntryDate();
        }

        /**
         * Gets the period attendance num.
         *
         * @param attendanceValues Map<PlainDate,String>
         * @return int
         */
        private int getPeriodAttendanceNum(Map<PlainDate, List<ToolStudentPeriodAttendance>> attendanceValues) {
            int counter = 0;
            StudentScheduleSpanSet spanSet = getSpanSet();
            OnSection section = (OnSection) spanSet.getSubmissionSpan().getSection();
            if (section != null) {
                PlainDate periodStartDate = spanSet.getEntrySpan().getEntryDate();
                PlainDate periodEndDate = spanSet.getSubmissionSpan().getExitDate();
                TreeSet<ToolSchoolCalendarDate> calendarDates =
                        section.getCalendarDates(periodStartDate, periodEndDate, getBroker());
                if (calendarDates != null) {
                    Range<Date> range =
                            Range.of(getFieldAdjustedStartDate(), spanSet.getSubmissionSpan().getExitDate());
                    for (ToolSchoolCalendarDate calendarDate : calendarDates) {
                        if (calendarDate.getInSessionIndicator() && range.contains(calendarDate.getDate())
                                && spanSet.includesDate(calendarDate.getDate())) {
                            Set<ToolSchedulePeriod> periods = section.getSectionPeriods(getBroker(), calendarDate);
                            if (periods != null) {
                                for (ToolSchedulePeriod period : periods) {
                                    List<ToolStudentPeriodAttendance> attendances =
                                            attendanceValues.get(calendarDate.getDate());
                                    if (attendances != null && attendances.stream()
                                            .map(ToolStudentPeriodAttendance::getPeriodView)
                                            .anyMatch(periodView -> periodView != null
                                                    && periodView.equals(period.getId()))) {
                                        counter++;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return counter;
        }

        /**
         * Gets the withdrawal type.
         *
         * @param span StudentScheduleSpan
         * @return String
         */
        private String getWithdrawalType(StudentScheduleSpan span) {
            if (checkWithdrawalPopulation(span)) {
                OnTranscript transcript = (OnTranscript) span.getTranscript();
                if (transcript == null) {
                    return OnTranscript.WITHDRAWAL_TYPE_D;
                }

                String value = transcript.getCourseRepeated();

                if (OnTranscript.FLAG_REPEATED.equals(value)) {
                    return null;
                } else if (OnTranscript.FLAG_WITHDRAWN.equals(value)) {
                    return OnTranscript.WITHDRAWAL_TYPE_W;
                } else {
                    return OnTranscript.WITHDRAWAL_TYPE_D;
                }
            }

            return null;
        }

        /**
         * Checks if is self study.
         *
         * @return true, if is self study
         */
        private boolean isConed() {
            OnSection section = (OnSection) getSpan().getSection();
            if (section != null) {
                String conedProgType = section.getConedProgType();
                return OnSection.CONED_CREDIT_DAY.equals(conedProgType) ||
                        OnSection.CONED_CREDIT_NIGHT.equals(conedProgType) ||
                        OnSection.CONED_SELF_STUDY.equals(conedProgType) ||
                        OnSection.CONED_SUMMER_CREDIT.equals(conedProgType);
            }
            return false;
        }

        /**
         * Checks if is DCC course.
         *
         * @return true, if is DCC course
         */
        private boolean isDCCCourse() {
            String institutionType = getFieldValue(FIELD_ID_INSTITUTION_TYPE);
            return !StringUtils.isEmpty(institutionType);
        }

        /**
         * Checks if is failing mark.
         *
         * @return true, if is passing mark
         */
        private boolean isFailingMark() {
            boolean result = false;
            String finalMarkStr = getFieldValue(FIELD_ID_FINAL_MARK);
            if (!StringUtils.isEmpty(finalMarkStr)) {
                double finalMark = 0;
                if (StringUtils.isNumeric(finalMarkStr)) {
                    try {
                        finalMark = Double.valueOf(finalMarkStr);
                    } catch (Exception e) {
                        // ignore exception
                    }
                }
                result = finalMark < 50;
            }
            return result;
        }

        /**
         * Checks if is mce0089.
         *
         * @return true, if is mce0089
         */
        private boolean isMCE0089() {
            // Withdrawal type is null AND End date is not null
            return StringUtils.isEmpty(getFieldValue(FIELD_ID_WITHDRAWAL_TYPE))
                    && !StringUtils.isEmpty(getFieldValue(FIELD_ID_COURSE_END_DATE));
        }

        /**
         * Checks if is passing mark.
         *
         * @return true, if is passing mark
         */
        private boolean isPassingMark() {
            double finalMark = 0;
            String finalMarkStr = getFieldValue(FIELD_ID_FINAL_MARK);
            if (StringUtils.isNumeric(finalMarkStr)) {
                try {
                    finalMark = Double.valueOf(finalMarkStr);
                } catch (Exception e) {
                    // ignore exception
                }
            }
            return finalMark >= 50;
        }

    }


    private Set<String> m_closureReasonCodes;

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.follett.fsc.aspensif.framework.SifStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Gets the match on empty key value.
     *
     * @param keyField String
     * @return boolean
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getMatchOnEmptyKeyValue(java.lang.String)
     */
    @Override
    public boolean getMatchOnEmptyKeyValue(String keyField) {
        return !(CsvField.LOCAL_COURSE_CODE.toString().equals(keyField)
                || CsvField.MINISTRY_DFND_CRS.toString().equals(keyField));
    }

    /**
     * Gets the closure reason codes.
     *
     * @return Sets the
     */
    protected Set<String> getClosureReasonCodes() {
        if (m_closureReasonCodes == null) {
            DataDictionaryField field =
                    ToolStudentPeriodAttendance.FIELD_REASON_CODE.getField(getDictionaryExtractor());
            m_closureReasonCodes = getDictionaryExtractor().getRefCodesWithStateValue(field, Arrays.asList("NRL"))
                    .stream().map(ReferenceCode::getCode).collect(Collectors.toSet());
        }
        return m_closureReasonCodes;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisStudentClassEnrollmentEntity.class);
    }

}
