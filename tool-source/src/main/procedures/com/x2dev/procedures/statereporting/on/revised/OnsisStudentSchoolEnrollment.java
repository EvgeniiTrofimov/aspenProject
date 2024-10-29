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

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.PredefinedConverter;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolPersonAddress;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentContextAttributes;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.CsvField;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper.OnsisExtractRecords.OnsisCsvDataRecord;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisReportCardSubjectStrand.OnsisReportCardSubjectStrandEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolStudent.OnsisSchoolStudentEntity;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.math.NumberUtils;
import org.w3c.dom.Element;

/**
 * The Class OnsisStudentSchoolEnrollment.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisStudentSchoolEnrollment extends OnsisStateReportData {

    /**
     * The Class OnsisStudentEnrollmentEntity.
     */
    public static class OnsisStudentEnrollmentEntity extends OnsisStudentEntity {
        /**
         * The Interface Formula.
         */
        interface Formula {

            /**
             * Calculate.
             *
             * @param creditValue Float
             * @param twr Float
             * @param twc Float
             * @param hcf Float
             * @return Float
             */
            abstract Float calculate(Float creditValue, Float twr, Float twc, Float hcf);
        }

        public static final String VALUE_ADE_ZERO_STRING = "0.0";
        public static final String VALUE_FTE_ZERO_STRING = "0.00";
        public static final String VALUE_MAIN_SCHOOL_FLAG_TRUE = "T";

        private static final int AGE_FOR_EDP_TYPE_NO = 3;

        private static final String EDP_CODE_NO = "NO";

        private static final String PATTERN_GRADE_JK_K = "^(JK|K)$";

        private static final String VALUE_ENTRY_RETURNING_STUDENT = "RS";
        private static final String VALUE_GRADE_TYPE_P9 = "P9";
        private static final List<String> VALUE_STU_BRD_RES_STATS_HC_FTE =
                Arrays.asList(OnsisConstants.VALUE_STU_BRD_RES_STAT_PUPIL_OF_BOARD);
        private static final List<String> VALUE_STU_BRD_RES_STATS_NO_FTE =
                Arrays.asList(OnsisConstants.VALUE_STU_BRD_RES_STAT_E_LEARNING,
                        OnsisConstants.VALUE_STU_BRD_RES_STAT_SHARED);
        private static final List<String> VALUE_STU_BRD_RES_STATS_PART_TIME =
                Arrays.asList(OnsisConstants.VALUE_STU_BRD_RES_STAT_E_LEARNING,
                        OnsisConstants.VALUE_STU_BRD_RES_STAT_SHARED);

        private static final DecimalFormat s_attendanceFormat = new DecimalFormat("#0.00");

        private DecimalFormat m_decimalFormat = new DecimalFormat("####0.0###");
        private PlainDate m_endDate = null;
        private boolean m_endDateSet = false;
        private OnsisStudentSchoolEnrollment m_reportData;
        private OnsisCsvDataRecord m_record;
        private OnsisAnnualSpan m_span;
        private List<AnnualSpan> m_spans;
        private PlainDate m_startDate;
        private OnsisAnnualSpan m_terminatingSpan;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public OnsisStudentEnrollmentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Earned diploma not coming back.
         *
         * @return true, if successful
         */
        public boolean earnedDiplomaNotComingBack() {
            boolean isComingBack = getSpan().getSchool().getOid().equals(getStudent().getNextSchoolOid());
            PlainDate startDate = getSpan().getFirstActiveInSessionDate();
            PlainDate endDate = DateUtils.add(getGlobalData().getEndDate(), 1);
            String thisBSID = deepGetFieldValueByFieldName(OnsisSchoolData.FIELD_SCHOOL_NUMBER);
            boolean isUnderEnrolment = true;

            return !isComingBack && getStudent()
                    .getProgramStudies(getGlobalData().getDateRange(), thisBSID, isUnderEnrolment,
                            Range.of(startDate, endDate))
                    .size() > 0;
        }

        /**
         * Gets the absences PLUS count of "G" days.
         *
         * @return String
         */
        public String getAbsences() {
            /*
             * Only the June submission should include absences/lates
             * when the span doesn't have an end date in the submission period.
             */
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            if (!StringUtils.isEmpty(this.getFieldValue(FIELD_ENROLMENT_END_DATE))
                    || submissionType.isJuneSubmission()) {
                if (getSpan() == null) {
                    return s_attendanceFormat.format(0.0);
                }
                if (getGlobalData().getDebugDetail()) {
                    StringBuilder debugOutput = new StringBuilder();
                    debugOutput.append("OnsisStudentEnrollmentEntity.getAbsences - span from ["
                            + getSpan().getFirstActiveInSessionDate() + " to " + getSpan().getLastActiveInSessionDate()
                            + "]\n");
                    getStudent().getAttendance(getBroker()).forEach(att -> debugOutput.append(att.toString() + "\n"));
                    getReportData().log(debugOutput.toString());
                }
                List<OnStudentAttendance> absencesAM = getStudent().getAttendance(getBroker()).stream()
                        .map(att -> (OnStudentAttendance) att)
                        .filter(att -> att.getAbsentIndicator())
                        .collect(Collectors.toList());
                List<OnStudentAttendance> absencesPM = getStudent().getAttendance(getBroker()).stream()
                        .map(att -> (OnStudentAttendance) att)
                        .filter(att -> att.getAbsentIndicator02())
                        .collect(Collectors.toList());

                PlainDate absencesCountDate = getAbsencesCountDate();

                Double absencesSum =
                        absencesAM.stream()
                                .filter(absence -> !absence.getDate().after(absencesCountDate))
                                .filter(absence -> (!absence.getDate().before(getSpan().getFirstActiveInSessionDate()))
                                        && (getSpan().getLastActiveInSessionDate() == null
                                                || !absence.getDate().after(getSpan().getLastActiveInSessionDate())))
                                .mapToDouble(absence -> 0.5).sum() +
                                absencesPM.stream()
                                        .filter(absence -> !absence.getDate().after(absencesCountDate))
                                        .filter(absence -> (!absence.getDate()
                                                .before(getSpan().getFirstActiveInSessionDate()))
                                                && (getSpan().getLastActiveInSessionDate() == null
                                                        || !absence.getDate()
                                                                .after(getSpan().getLastActiveInSessionDate())))
                                        .mapToDouble(absence -> 0.5).sum();

                boolean includeGDays =
                        !Boolean.FALSE.equals(getReportData().getParameter(INPUT_PARAM_INCLUDE_G_DAYS));
                if (includeGDays) {
                    absencesSum += getAbsencesForClosureDouble();
                }

                return s_attendanceFormat.format(absencesSum);
            }
            return null;
        }

        /**
         * Gets the absences for closure.
         *
         * @return String
         */
        public String getAbsencesForClosure() {
            /*
             * Only the June submission should include absences/lates
             * when the span doesn't have an end date in the submission period.
             */
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            if (!StringUtils.isEmpty(getFieldValue(FIELD_ENROLMENT_END_DATE))
                    || submissionType.isJuneSubmission()) {
                if (getSpan() == null) {
                    return s_attendanceFormat.format(0.0);
                }
                return s_attendanceFormat.format(getAbsencesForClosureDouble());
            }
            return null;
        }

        /**
         * Gets the ade.
         *
         * @return double
         */
        public String getAde() {
            Double highCreditFactor = getHighCreditFactor();
            if (getGlobalData().getDebugDetail()) {
                getReportData()
                        .log("OnsisStudentEnrollmentEntity.getAde-getHighCreditFactor" + highCreditFactor + "\n");
            }
            return m_decimalFormat.format(getAdeRaw() * (1 - highCreditFactor));
        }

        /**
         * Gets the arrival date to Canada "all-psn-ArrivalDateCanada".
         *
         * @return String
         */
        public PlainDate getArrivalDateCanada() {
            return getStudent().getArrivalDateCanada();
        }

        /**
         * Gets the attendance type.
         *
         * @return String
         */
        public String getAttendanceType() {
            String type = null;
            if (getSpan() != null) {
                if (getSpan().isSecondary()) {
                    type = OnsisConstants.VALUE_ATTENDANCE_TYPE_PART_TIME;
                } else if (VALUE_STU_BRD_RES_STATS_PART_TIME.contains(getFieldValue(FIELD_BOARD_RESIDENCE_STATUS))) {
                    type = OnsisConstants.VALUE_ATTENDANCE_TYPE_PART_TIME; // MSE0078
                } else {
                    SubmissionSchoolType schoolType = getGlobalData().getSchoolType();
                    DistrictSchoolYearContext currentContext = getGlobalData().getCurrentContext();
                    if (SubmissionSchoolType.PUBLIC_SECONDARY.equals(schoolType)
                            && currentContext.getSchoolYear() > 2020) {
                        FteMonthly monthlyFteRecord =
                                getStudent().getFteMonthlyRecord(getBroker(), getGlobalData().getCurrentSchoolOids(),
                                        getGlobalData().getSubmissionType().getSubmissionPeriodCode());
                        if (monthlyFteRecord != null) {
                            type = monthlyFteRecord.getAttendanceType();
                        }
                        if (StringUtils.isBlank(type)) {
                            type = OnsisConstants.VALUE_ATTENDANCE_TYPE_PART_TIME;
                        }
                    } else {
                        OnEnrollment enrollment = (OnEnrollment) getSpan().getRecentEnrollmentES();
                        if (enrollment != null) {
                            type = getDictionaryExtractor().getStateValue(enrollment,
                                    OnEnrollment.FIELD_ENROLMENT_REGISTER);
                        }
                    }

                    if (StringUtils.isBlank(type)) {
                        return type;
                    }

                    // MSE0026 "FTE" must be within selected "Attendance Type" range.
                    BigDecimal fte = new BigDecimal(getFte());
                    BigDecimal highCreditFte = new BigDecimal(getFteHc());
                    BigDecimal totalFte = fte.add(highCreditFte);
                    if (totalFte.floatValue() < 0.7 && !OnsisConstants.VALUE_ATTENDANCE_TYPE_PART_TIME.equals(type)) {
                        type = OnsisConstants.VALUE_ATTENDANCE_TYPE_PART_TIME;
                    }
                }
            } else {
                type = OnsisConstants.VALUE_ATTENDANCE_TYPE_PART_TIME;
            }
            return type;
        }

        /**
         * Gets the birth country.
         *
         * @return String
         */
        public String getBirthCountry() {
            String country = getStudent().getBirthCountry();
            return OnsisConstants.VALUE_CANADA.equals(country) ? null : country;
        }

        /**
         * Gets the birth province.
         *
         * @return String
         */
        public String getBirthProvince() {
            return getStudent().getBirthProvince();
        }

        /**
         * Gets the board resident status.
         *
         * @return String
         */
        public String getBoardResidentStatus() {
            // check FTE for board resident status if found
            SubmissionSchoolType schoolType = getGlobalData().getSchoolType();
            DistrictSchoolYearContext currentContext = getGlobalData().getCurrentContext();
            if (SubmissionSchoolType.PUBLIC_SECONDARY.equals(schoolType) && currentContext.getSchoolYear() > 2021
                    && !StringUtils.isEmpty(FteMonthly.FIELD_SB_RESIDENT_STATUS.resolve(getDictionaryExtractor()))) {
                FteMonthly monthlyFteRecord =
                        getStudent().getFteMonthlyRecord(getBroker(), getGlobalData().getCurrentSchoolOids(),
                                getGlobalData().getSubmissionType().getSubmissionPeriodCode());
                if (monthlyFteRecord != null) {
                    String status = getDictionaryExtractor().getStateValue(monthlyFteRecord,
                            FteMonthly.FIELD_SB_RESIDENT_STATUS);
                    if (!StringUtils.isEmpty(status)) {
                        return status;
                    }
                }
            }

            if (getSpan() != null) {
                return getSpan().getBoardResidentStatus(getGlobalData());
            } else if (getTerminatingSpan() != null) {
                return getTerminatingSpan().getBoardResidentStatus(getGlobalData());
            } else {
                String csvValue = m_record.getSingleFieldValue(CsvField.STU_BRD_RES_STAT_TYPE);
                return StringUtils.isBlank(csvValue) ? null : csvValue;
            }
        }

        /**
         * Gets the canadian residence status.
         *
         * @return String
         */
        public String getCanadianResidenceStatus() {
            if (getSpan() != null) {
                return getSpan().getCanadianResidenceStatus(getGlobalData());
            }
            String csvValue = m_record.getSingleFieldValue(CsvField.RESIDENCE_STATUS_TYPE);
            return StringUtils.isBlank(csvValue) ? null : csvValue;
        }

        /**
         * Gets the class entity.
         *
         * @return Class wrapper
         */
        public String getClassCode() {
            return getStudent().getClassCode(getGlobalData().getBroker(), getGlobalData());
        }

        /**
         * Gets the country type exit.
         *
         * @return String
         */
        public String getCountryTypeExit() {
            String country = null;
            if (!StringUtils.isEmpty(this.getFieldValue(FIELD_ENROLMENT_END_DATE))) {
                if (getSpan() != null) {
                    if (getSpan().isSecondary()) {
                        return country;
                    }
                    country = getSpan().getEntryDemitCountryExit();
                } else if (getTerminatingSpan() != null) {
                    if (getTerminatingSpan().isSecondary()) {
                        return country;
                    }
                    country = getTerminatingSpan().getEntryDemitCountryExit();
                }
                if (StringUtils.isBlank(country)) {
                    SubmissionType submissionType = getGlobalData().getSubmissionType();
                    /*
                     * Student leaving board (all-std-EoyrDemitCode is not empty)
                     */
                    if (submissionType.isJuneSubmission()) {
                        PlainDate julyFirst = DateUtils.add(getGlobalData().getEndDate(), 1);
                        if (julyFirst.equals(getOnsisEnrollmentEndDate())) {
                            country = getDictionaryExtractor().getStateValue(getStudent(),
                                    OnsisStudent.FIELD_EOY_DEMIT_COUNTRY);
                        }
                    }
                }
            }
            return country;
        }

        /**
         * Gets the country type.
         *
         * @return String
         */
        public String getCountryType() {
            String country = null;
            if (isAddressRequired()) {
                OnsisCsvDataRecord record = getCsvDataRecord();
                if (record == null) {
                    country = getSpan().getEntryDemitCountryEntry();
                }
            }
            return country;
        }

        /**
         * Gets the current country.
         *
         * @return String
         */
        public String getCurrentCountry() {
            String country = null;
            OnAddress address = getAddress();
            if (address != null) {
                country = address.getCountry();
                if (OnsisConstants.VALUE_CANADA.equals(country)) {
                    country = null;
                }
            }
            return country;
        }

        /**
         * Gets the current province.
         *
         * @return String
         */
        public String getCurrentProvince() {
            String province = null;
            OnAddress address = getAddress();
            if (address != null) {
                province = address.getProvince();
            }
            return province;
        }

        /**
         * Gets the designated school.
         *
         * @return the designated school
         */
        public String getDesignatedSchool() {
            OnSchool school = getGlobalData().getCurrentSchool().get(0);
            String gradeLevel = getFieldValue(FIELD_GRADE_LEVEL);

            String value = null;
            if (OnsisConstants.VALUE_GRADES_ELEMENTARY_STATE_CODES.contains(gradeLevel)) {
                value = school.getDesignatedBsidElementary();
            } else if (OnsisConstants.VALUE_GRADES_SECONDARY_STATE_CODES.contains(gradeLevel)) {
                value = school.getDesignatedBsidSecondary();
            } else {
                throw new IllegalStateException("Designated school cannot be determined for grade level " + gradeLevel);
            }

            return value;
        }

        /**
         * Gets the extended program type.
         *
         * @return String
         */
        public String getExtendedProgramType() {
            int ageAsOfDecember31 = getStudent().getAgeAsOfDate(new PlainDate(getGlobalData().getDateDecember31()));
            if (ageAsOfDecember31 > AGE_FOR_EDP_TYPE_NO) {
                String gradeLevel = getFieldValue(FIELD_GRADE_LEVEL);
                boolean isGradeLevelJKK = !StringUtils.isEmpty(gradeLevel) && gradeLevel.matches(PATTERN_GRADE_JK_K);
                if (isGradeLevelJKK) {
                    OnStudentExtendedDay program = getExtendedProgram();
                    if (program != null) {
                        return program.getProgramType();
                    }
                    return EDP_CODE_NO;
                }
            }
            return null;
        }

        /**
         * Gets the french admission committee approval date.
         *
         * @return PlainDate
         */
        public PlainDate getFrenchAdmissionCommitteeApprovalDate() {
            return getStudent().getFrenchAdmissionCommitteeApprovalDate();
        }

        /**
         * Gets the fte.
         *
         * @return String
         */
        public String getFte() {
            if (getSpan() == null
                    || VALUE_STU_BRD_RES_STATS_NO_FTE.contains(getFieldValue(FIELD_BOARD_RESIDENCE_STATUS))) {
                return VALUE_FTE_ZERO_STRING;
            }

            SubmissionSchoolType schoolType = getGlobalData().getSchoolType();
            DistrictSchoolYearContext currentContext = getGlobalData().getCurrentContext();
            if (SubmissionSchoolType.PUBLIC_SECONDARY.equals(schoolType)
                    && currentContext.getSchoolYear() > 2020) {
                FteMonthly monthlyFteRecord =
                        getStudent().getFteMonthlyRecord(getBroker(), getGlobalData().getCurrentSchoolOids(),
                                getGlobalData().getSubmissionType().getSubmissionPeriodCode());
                BigDecimal fte = monthlyFteRecord == null ? BigDecimal.ZERO : monthlyFteRecord.getFte();
                return OnsisConstants.DECIMAL_FORMAT_FTE.format(fte.doubleValue());
            }
            return getSpan().getFte();
        }

        /**
         * Gets the high credit fte.
         *
         * @return String
         */
        public String getFteHc() {
            /*
             * Only report HIGH_CREDIT_FTE if Pupil of the Board (MSE0141)
             * and not 21 years old (MSE0140)
             *
             * But tag is mandatory for Enrolment Add in Public Secondary so return "0.00"
             */
            String boardResStatus = getFieldValue(FIELD_BOARD_RESIDENCE_STATUS);
            boolean isPupilOfTheBoard = boardResStatus != null
                    && VALUE_STU_BRD_RES_STATS_HC_FTE.contains(boardResStatus);
            if (getSpan() == null || !isPupilOfTheBoard || is21OrOlder()) {
                return VALUE_FTE_ZERO_STRING;
            }

            SubmissionSchoolType schoolType = getGlobalData().getSchoolType();
            DistrictSchoolYearContext currentContext = getGlobalData().getCurrentContext();
            if (SubmissionSchoolType.PUBLIC_SECONDARY.equals(schoolType)
                    && currentContext.getSchoolYear() > 2020) {
                FteMonthly monthlyFteRecord =
                        getStudent().getFteMonthlyRecord(getBroker(), getGlobalData().getCurrentSchoolOids(),
                                getGlobalData().getSubmissionType().getSubmissionPeriodCode());
                BigDecimal fte = monthlyFteRecord == null ? BigDecimal.ZERO : monthlyFteRecord.getFteHc();
                return OnsisConstants.DECIMAL_FORMAT_FTE.format(fte.doubleValue());
            }
            return getSpan().getFteHc();
        }

        /**
         * Gets the gender type.
         *
         * @return String
         */
        public String getGenderType() {
            return getDictionaryExtractor().getStateValue(getStudent(), OnsisStudent.FIELD_GENDER_CODE);
        }

        /**
         * Gets the grade designation.
         *
         * @return String
         */
        public String getGradeDesignation() {
            String gradeLevel = getFieldValue(FIELD_GRADE_LEVEL);

            return getReportData().getGradeDesignation(getStudent(), gradeLevel);
        }

        /**
         * Gets the grade type.
         *
         * @return String
         */
        public String getGradeType() {
            String gradeType = null;
            if (getSpan() != null) {
                gradeType = getSpan().getGradeType(getGlobalData());
            } else if (getTerminatingSpan() != null) {
                gradeType = getTerminatingSpan().getGradeType(getGlobalData());
            } else {
                String csvValue = m_record.getSingleFieldValue(CsvField.GRADE_TYPE);
                gradeType = StringUtils.isBlank(csvValue) ? null : csvValue;
            }
            PlainDate withdrawalDate = getOnsisEnrollmentEndDate();
            PlainDate evaluationDate =
                    withdrawalDate != null && getGlobalData().getDateRange().contains(withdrawalDate)
                            ? withdrawalDate
                            : getGlobalData().getEndDate();
            if (getStudent().getSpedPrograms(getBroker()).stream().anyMatch(pgm -> {
                return pgm.getProgramDetails(getBroker()).stream()
                        .anyMatch(pgd -> "Program".equals(pgd.getType())
                                && Range.of(pgd.getStartDate(), pgd.getEndDate())
                                        .contains(evaluationDate)
                                && VALUE_GRADE_TYPE_P9.equals(getGlobalData().getDictionaryExtractor()
                                        .getStateValue(pgd, OnStudentSpedDetail.FIELD_SPECED_PROGRAM_NAME)));
            })) {
                gradeType = VALUE_GRADE_TYPE_P9;
            }
            return gradeType;
        }

        /**
         * Gets the high credit ade.
         *
         * @return double
         */
        public String getHighCreditAde() {
            if (getSpan() != null) {
                // MSE0140
                if (is21OrOlder()) {
                    return VALUE_ADE_ZERO_STRING;
                }
                Double highCreditFactor = getHighCreditFactor();
                if (highCreditFactor > 0) {
                    return m_decimalFormat.format(getAdeRaw() * highCreditFactor);
                }
            }
            return VALUE_ADE_ZERO_STRING;
        }

        /**
         * Gets the indigenous self identification.
         *
         * @return String
         */
        public String getIndigenousSelfIdentification() {
            String indigenousSelfId = null;

            indigenousSelfId = getDictionaryExtractor().getStateValue(getStudent(), OnsisStudent.FIELD_INDIGENOUS);

            return indigenousSelfId;
        }

        /**
         * Gets the language prev school.
         *
         * @return String
         */
        public String getLanguagePrevSchool() {
            String language = null;
            OnsisCsvDataRecord record = getCsvDataRecord();
            if (record == null && !StringUtils.isEmpty(getFieldValue(FIELD_PROVINCE_STATE_TYPE))) {
                language = getSpan().getLanguagePrevSchool(getGlobalData());
            }
            return language;
        }

        /**
         * Gets the language type.
         *
         * @return String
         * @throws Exception exception
         */
        public String getLanguageType() throws Exception {
            String result = null;
            if (getSpan() != null) {
                result = getDictionaryExtractor().getStateValue(getSpan().getSchool(), OnSchool.FIELD_LANGUAGE_TYPE);
            } else if (getGlobalData().getCurrentSchool() != null && !getGlobalData().getCurrentSchool().isEmpty()) {

                String csvValue = m_record.getSingleFieldValue(CsvField.LANGUAGE_TYPE);
                result = StringUtils.isBlank(csvValue) ? null : csvValue;
            }
            if (StringUtils.isEmpty(result) && getReportData().getGlobalData() != null
                    && !getGlobalData().getCurrentSchool().isEmpty()) {
                result = getDictionaryExtractor().getStateValue(getGlobalData().getCurrentSchool().get(0),
                        OnSchool.FIELD_LANGUAGE_TYPE);
            }
            return result;
        }

        /**
         * Gets the lates.
         *
         * @return String
         */
        public String getLates() {
            /*
             * Only the June submission should include absences/lates
             * when the span doesn't have an end date in the submission period.
             */
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            if (!StringUtils.isEmpty(this.getFieldValue(FIELD_ENROLMENT_END_DATE))
                    || submissionType.isJuneSubmission()) {
                if (getSpan() == null) {
                    return String.valueOf(0);
                }
                PlainDate latesCountDate = getOnsisEnrollmentEndDate() == null
                        ? getGlobalData().getEndDate()
                        : (getOnsisEnrollmentEndDate().before(getGlobalData().getEndDate())
                                ? getOnsisEnrollmentEndDate()
                                : getGlobalData().getEndDate());

                List<OnStudentAttendance> latesAM = getStudent().getAttendance(getBroker()).stream()
                        .map(att -> (OnStudentAttendance) att)
                        .filter(att -> att.getTardyIndicator())
                        .collect(Collectors.toList());
                List<OnStudentAttendance> latesPM = getStudent().getAttendance(getBroker()).stream()
                        .map(att -> (OnStudentAttendance) att)
                        .filter(att -> att.getTardyIndicator02())
                        .collect(Collectors.toList());

                long count = latesAM.stream()
                        .filter(absence -> !absence.getDate().after(latesCountDate))
                        .count() +
                        latesPM.stream()
                                .filter(absence -> !absence.getDate().after(latesCountDate))
                                .count();

                return String.valueOf(count);
            }

            return null;
        }

        /**
         * Gets the mature student flag.
         *
         * @return Boolean
         */
        public Boolean getMatureStudentFlag() {
            return getStudent().isMatureStudent(getBroker(),
                    ToolBean.getPlainDateValue(getGlobalData().getCurrentYear(), Calendar.AUGUST, 31),
                    getGlobalData().getCurrentSchoolOids());
        }

        /**
         * Gets the mobility type.
         *
         * @return String
         */
        public String getMobilityTypeEntry() {
            String mobilityType = null;
            OnsisCsvDataRecord record = getCsvDataRecord();
            if (record == null) {
                if (!getSpan().isSecondary()) {
                    mobilityType = getSpan().getMobilityTypeEntry(getGlobalData());
                } else {
                    OnsisAnnualSpan primarySpan = (OnsisAnnualSpan) getSpan().getBestPrimarySpanFor(getBroker(), null);
                    if (primarySpan != null) {
                        mobilityType = primarySpan.getMobilityTypeFromPrimary(getGlobalData(), true);
                    }
                }
            }
            return mobilityType;
        }

        /**
         * Gets the mobility type.
         *
         * @return String
         */
        public String getMobilityTypeExit() {
            if (getSpan() != null) {
                if (StringUtils.isBlank(getFieldValue(FIELD_ENROLMENT_END_DATE))) {
                    getReportData().log("getMobilityTypeExit() - no end date");
                    return null;
                }

                /*
                 * 1. If secondary enrolment (shared student)
                 * + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT: 64
                 */
                if (getSpan().isSecondary()) {
                    getReportData().log("getMobilityTypeExit() - is secondary so shared");
                    return OnsisConstants.VALUE_MOBILITY_TYPE_SHARE_ENDED_STATE_CODE;
                }

                // TODO: Verify future W is excluded

                /*
                 * 2. Normal case of a span with W record in the submission period
                 */
                String mobilityType = getSpan().getMobilityTypeExit(getGlobalData());
                if (!StringUtils.isBlank(mobilityType)) {
                    getReportData().log("getMobilityTypeExit() - mobility type from span " + getSpan().toString()
                            + " is " + mobilityType);
                    return mobilityType;
                }

                /*
                 * 3. If it's not a JUN* submission -> return null
                 */
                SubmissionType submissionType = getGlobalData().getSubmissionType();
                if (!submissionType.isJuneSubmission()) {
                    getReportData().log("getMobilityTypeExit() - is not June submission so null");
                    return null;
                }

                /*
                 * Is student active on last in session date?
                 * Return firstInactiveInSessionDate if student not active on
                 * schoolLastInSessionDate.
                 */
                // EnrollmentSpanHelper spanHelper = getReportData().getSpanHelper();
                // boolean forward = false;
                // PlainDate schoolLastInSessionDate =
                // spanHelper.findFirstInSessionDate(getSpan().getSchoolCalendar(),
                // getSpan().getContext().getEndDate(), forward, getReportData().getBroker());
                // PlainDate lastActiveDate =
                // getSpan().getParentSpan().getLastActiveInSessionDate();
                // if (lastActiveDate != null && lastActiveDate.before(schoolLastInSessionDate)) {
                // return mobilityType;
                // }

                /*
                 * 4. Student leaving board (all-std-EoyrDemitCode is not empty)
                 */
                PlainDate julyFirst = DateUtils.add(getGlobalData().getEndDate(), 1);
                if (julyFirst.equals(getOnsisEnrollmentEndDate())) {
                    String eoyrDemitCode =
                            getDictionaryExtractor().getStateValue(getStudent(), OnsisStudent.FIELD_EOY_DEMIT_CODE);
                    if (!StringUtils.isBlank(eoyrDemitCode)) {
                        getReportData().log("getMobilityTypeExit() - use eoyrDemitCode " + eoyrDemitCode);
                        return eoyrDemitCode;
                    }
                }

                /*
                 * 5. Student graduating: Earned Diploma (for example OSSD)
                 * during submission period and Empty Next School
                 * + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT: 61
                 */
                if (earnedDiplomaNotComingBack()) {
                    getReportData().log("getMobilityTypeExit() - earned diploma is not coming back");
                    return OnsisConstants.VALUE_MOBILITY_TYPE_OSSD_ENDED_STATE_CODE;
                }

                /*
                 * Student who are active on the last in session date and
                 * not coming back, should export enrollment end date as July 1
                 * and mobility exit type as appropriate.
                 */
                /*
                 * 6. If existing W record on or before 7/1/YYYY
                 * + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT from W record
                 */
                PlainDate withdrawalMemberDate = getSpan().getWithdrawalMemberDate();
                if (withdrawalMemberDate != null && withdrawalMemberDate.before(julyFirst)) {
                    OnEnrollment terminatingEnrollment = (OnEnrollment) getSpan().getTerminatingEnrollment();
                    if (StudentEnrollment.WITHDRAWAL.equals(terminatingEnrollment.getEnrollmentType())) {
                        mobilityType = getDictionaryExtractor().getStateValue(terminatingEnrollment,
                                ToolEnrollment.FIELD_ENROLLMENT_CODE);
                    } else {
                        throw new RuntimeException("Expected withdrawal as terminating ENR for: " + getSpan());
                    }

                    getReportData().log("getMobilityTypeExit() - not active on last in session date and not returning "
                            + mobilityType);
                    return mobilityType;
                }

                /*
                 * 7. Student changing schools next year
                 * (progressing or moving to another school):
                 * Next school set on Student Record and not equal to current school.
                 * + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT: 64
                 */
                if (isChangingSchool()) {
                    getReportData().log("getMobilityTypeExit() - is changing school");
                    return OnsisConstants.VALUE_MOBILITY_TYPE_SHARE_ENDED_STATE_CODE;
                }

                // 8. Otherwise, send no ENROLMENT_END_DATE or STUDENT_MOBILITY_TYPE_EXIT for
                // student
                getReportData().log("getMobilityTypeExit() - no value found for current span");
                return null;
            }

            getReportData().log("getMobilityTypeExit() - NO current span");
            OnEnrollment terminatingEnrollment = null;
            if (getTerminatingSpan() != null) {
                getReportData().log("getMobilityTypeExit() - termination span wrapper span is "
                        + getTerminatingSpan().toString());
                terminatingEnrollment = (OnEnrollment) getTerminatingSpan().getTerminatingEnrollment();
            }

            String mobilityType = null;
            if (terminatingEnrollment != null) {
                mobilityType = getDictionaryExtractor().getStateValue(terminatingEnrollment,
                        ToolEnrollment.FIELD_ENROLLMENT_CODE);
                getReportData().log(
                        "getMobilityTypeExit() - from termination span wrapper terminating enrollment " + mobilityType);
            }

            if (StringUtils.isBlank(mobilityType)) {
                if (getGlobalData().getSubmissionType().isJuneSubmission()) {
                    mobilityType =
                            getDictionaryExtractor().getStateValue(getStudent(), OnsisStudent.FIELD_EOY_DEMIT_CODE);
                    getReportData()
                            .log("getMobilityTypeExit() - from termination span wrapper use EOY type " + mobilityType);
                }
            }

            if (StringUtils.isBlank(mobilityType)) {
                if (getTerminatingSpan() != null && getTerminatingSpan().isSecondary()) {
                    mobilityType = OnsisConstants.VALUE_MOBILITY_TYPE_SHARE_ENDED_STATE_CODE;
                    getReportData().log("getMobilityTypeExit() - from termination span wrapper use shared (secondary) "
                            + mobilityType);
                }
            }
            if (StringUtils.isBlank(mobilityType) && getOnsisEnrollmentEndDate() != null && !isMainSchool()) {
                mobilityType = OnsisConstants.VALUE_MOBILITY_TYPE_SHARE_ENDED_STATE_CODE;
                getReportData().log(
                        "getMobilityTypeExit() - from termination span wrapper use shared (not main) " + mobilityType);
            }
            return StringUtils.isBlank(mobilityType) ? null : mobilityType;
        }

        /**
         * Gets the onsis enrollment end date.
         *
         * @return Plain date
         */
        public PlainDate getOnsisEnrollmentEndDate() {
            if (m_endDateSet) {
                return m_endDate;
            }

            if (getSpan() != null) {
                m_endDate = computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate();
            } else {
                m_endDate = getGlobalData().getStartDate();
            }
            m_endDateSet = true;

            return m_endDate;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         */
        public PlainDate getOnsisEnrollmentStartDate() {
            if (m_startDate != null) {
                return m_startDate;
            }

            OnsisCsvDataRecord csvRecord = getCsvDataRecord();
            if (csvRecord != null) {
                m_startDate = getCsvDate(csvRecord, OnsisExtractHelper.CsvField.ENR_START_DATE);
            }
            if (m_startDate == null && getSpan() != null) {
                boolean isConEd = getGlobalData().getSubmissionType().isContinuingEducationSubmission();
                boolean isECPP = getGlobalData().getSubmissionType().isECPPSubmission();
                m_startDate = (isConEd || isECPP) && !getSpans().isEmpty()
                        ? getSpans().get(0).getFirstActiveInSessionDate()
                        : getSpan().getFirstActiveInSessionDate();
                if (VALUE_ENTRY_RETURNING_STUDENT.equals(getFieldValue(FIELD_MOBILITY_TYPE_ENTRY))) {
                    OnEnrollment firstEnrollment = null;
                    if (!getSpan().isSecondary()) {
                        firstEnrollment = (OnEnrollment) getSpan().getAllEnrollmentsDescend().iterator().next();
                    } else {
                        OnsisAnnualSpan primarySpan =
                                (OnsisAnnualSpan) getSpan().getBestPrimarySpanFor(getBroker(), null);
                        if (primarySpan != null) {
                            firstEnrollment = (OnEnrollment) primarySpan.getAllEnrollmentsDescend().iterator().next();
                        }
                    }
                    if (firstEnrollment != null) {
                        OnEnrollment testEnrollment = firstEnrollment;
                        ToolEnrollment previousEnrollment = getStudent().getEnrollments(getBroker()).stream()
                                .filter(enr -> StudentEnrollment.ENTRY.equals(enr.getEnrollmentType()) &&
                                        testEnrollment.getSchoolOid().equals(enr.getSchoolOid()) &&
                                        !VALUE_ENTRY_RETURNING_STUDENT
                                                .equals(getDictionaryExtractor().getStateValue(enr,
                                                        ToolEnrollment.FIELD_ENROLLMENT_CODE))
                                        &&
                                        enr.getEnrollmentDate() != null &&
                                        enr.getEnrollmentDate().before(testEnrollment.getEnrollmentDate()))
                                .findFirst().orElse(null);

                        if (previousEnrollment != null) {
                            m_startDate = previousEnrollment.getEnrollmentDate();
                        }
                    }
                }

                /*
                 * FTE records will need a span active after current Submission.
                 * Use Submission End Date.
                 */
                PlainDate periodEndDate = getGlobalData().getSubmissionType().getPeriodEndDate();
                if (m_startDate.after(periodEndDate)) {
                    ToolSchoolCalendar spanCalendar = ToolSchoolCalendar.findBestCalendar(getBroker(), getSpan());
                    boolean forward = false;
                    PlainDate firstInSessionDateBeforeCountDate =
                            spanCalendar.findFirstInSessionDate(getBroker(), periodEndDate, forward);

                    m_startDate = firstInSessionDateBeforeCountDate;
                }
            }
            return m_startDate;
        }

        /**
         * Gets the oyap flag.
         *
         * @return Boolean
         */
        public Boolean getOyapFlag() {
            Boolean oyapFlag = null;

            String mainSchoolFlag = getFieldValue(FIELD_MAIN_SCHOOL_FLAG);
            if (VALUE_MAIN_SCHOOL_FLAG_TRUE.equals(mainSchoolFlag)) {
                oyapFlag = Boolean.valueOf(isOyap());
            }

            return oyapFlag;
        }

        /**
         * Gets the province type.
         *
         * @return String
         */
        public String getPostalCode() {
            String postalCode = null;
            OnAddress address = getAddress();
            if (address != null) {
                String country = address.getCountry();
                if (OnsisConstants.VALUE_CANADA.equals(country)) {
                    String province = address.getProvince();
                    if (OnsisConstants.VALUE_ONTARIO.equals(province)) {
                        postalCode = address.getPostalCode();
                    }
                }
            }
            return postalCode;
        }

        /**
         * If [all-std-Retained] = True (1), set PROMOTION_FLAG to “F”.
         * Otherwise set flag to “T”
         *
         * @return Boolean
         */
        public Boolean getPromotionFlag() {
            boolean isRetained = false;
            if (getSpan() != null) {
                /*
                 * Get from Student record if span is in current context.
                 * Else attempt to get from Student Context Attributes for span context.
                 */
                ToolDistrictContext spanContext = getSpan().getContext();

                if (spanContext == null || spanContext.getOid().equals(getGlobalData().getCurrentContext().getOid())) {
                    // Get current value from Student record
                    isRetained = getStudent().getRetainedIndicator();
                } else {
                    // Get value from StudentContextAttributes
                    try {
                        ToolStudentContextAttributes attributes =
                                getStudent().getStudentContextAttributes(getBroker()).stream()
                                        .filter(attr -> spanContext.getOid().equals(attr.getContextOid())).findFirst()
                                        .orElse(null);
                        isRetained = (Boolean) PredefinedConverter.LOGICAL.convertedValue(attributes.getAttributeValue(
                                getBroker(), getDictionaryExtractor(), OnsisStudent.FIELD_RETAINED, false));
                    } catch (Exception e) {
                        //
                    }
                }
            }

            return Boolean.valueOf(!isRetained);
        }

        /**
         * Gets the province type.
         *
         * @return String
         */
        public String getProvinceType() {
            String province = null;
            if (isAddressRequired()) {
                OnsisCsvDataRecord record = getCsvDataRecord();
                if (record == null) {
                    province = getSpan().getEntryDemitProvinceEntry();
                }
            }
            return province;
        }

        /**
         * Gets the province type exit.
         *
         * @return String
         */
        public String getProvinceTypeExit() {
            String province = null;
            if (!StringUtils.isEmpty(this.getFieldValue(FIELD_ENROLMENT_END_DATE))) {
                if (getSpan() != null) {
                    province = getSpan().getEntryDemitProvinceExit();

                } else if (getTerminatingSpan() != null) {
                    province = getTerminatingSpan().getEntryDemitProvinceExit();
                }
                if (StringUtils.isBlank(province)) {
                    SubmissionType submissionType = getGlobalData().getSubmissionType();
                    /*
                     * Student leaving board (all-std-EoyrDemitCode is not empty)
                     */
                    if (submissionType.isJuneSubmission()) {
                        PlainDate julyFirst = DateUtils.add(getGlobalData().getEndDate(), 1);
                        if (julyFirst.equals(getOnsisEnrollmentEndDate())) {
                            province = getDictionaryExtractor().getStateValue(getStudent(),
                                    OnsisStudent.FIELD_EOY_DEMIT_PROVINCE);
                        }
                    }
                }
            }
            return province;
        }

        /**
         * Gets the reach ahead flag.
         *
         * @return Boolean
         */
        public Boolean getReachAheadFlag() {
            if (getSpan() != null) {
                return getSpan().getReachAheadFlag(getGlobalData());
            } else if (getTerminatingSpan() != null) {
                return getTerminatingSpan().getReachAheadFlag(getGlobalData());
            }
            return Boolean.FALSE;
        }

        /**
         * Gets the report data.
         *
         * @return Onsis state report data
         */
        @Override
        public OnsisStudentSchoolEnrollment getReportData() {
            return m_reportData;
        }

        /**
         * Gets the scholarship date.
         *
         * @return Plain date
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEntity#getScholarshipDate()
         */
        @Override
        public PlainDate getScholarshipDate() throws X2BaseException {
            // MDI0019 - Ontario Scholar can only be awarded by the student's Main School
            if (!isMainSchool()) {
                return null;
            }

            PlainDate date = null;
            PlainDate issuedDate = super.getScholarshipDate();
            if (issuedDate != null) {
                AnnualSpan matchingSpan = getStudent().getEnrollmentSpans(getBroker(), false, false).stream()
                        .filter(span -> !span.isSecondary()
                                && getGlobalData().getDateRange().isOverlap(span.getDateRange())
                                && span.getContext().getOid().equals(getGlobalData().getCurrentContext().getOid()))
                        .sorted(new Comparator<AnnualSpan>() {

                            @Override
                            public int compare(AnnualSpan span1, AnnualSpan span2) {
                                Range<Date> range1 = Range.of(span1.getSpanStartDate(), span1.getSpanEndDate());
                                Range<Date> range2 = Range.of(span2.getSpanStartDate(), span2.getSpanEndDate());
                                if (range1.contains(issuedDate)) {
                                    if (range2.contains(issuedDate)) {
                                        return span2.getSpanStartDate().compareTo(span1.getSpanStartDate());
                                    }
                                    return -1;
                                } else if (range2.contains(issuedDate)) {
                                    return 1;
                                }
                                return span2.getSpanStartDate().compareTo(span1.getSpanStartDate());
                            }
                        })
                        .findFirst()
                        .orElse(null);
                if (matchingSpan == null) {
                    // since there is no suitable span use this school
                    date = issuedDate;
                } else if (getGlobalData().getCurrentSchool().contains(matchingSpan.getSchool())) {
                    date = issuedDate;
                }
            }
            return date;
        }

        /**
         * Gets the student number limited to the last 9 characters.
         *
         * @return String
         */
        public String getSchoolStudentNumber() {
            String aspenValue = getStudent().getLocalId();
            if (aspenValue == null) {
                return null;
            }

            String regex = "^\\w*(\\w{9,9})$";
            String replacement = "$1";

            return aspenValue.replaceAll(regex, replacement);

        }

        /**
         * Gets the span.
         *
         * @return Onsis annual span
         */
        public OnsisAnnualSpan getSpan() {
            return m_span;
        }

        /**
         * Gets the span.
         *
         * @return Onsis annual span
         */
        public List<AnnualSpan> getSpans() {
            return m_spans;
        }

        /**
         * Gets the terminating span.
         *
         * @return Onsis annual span
         */
        public OnsisAnnualSpan getTerminatingSpan() {
            return m_terminatingSpan;
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

            m_reportData = (OnsisStudentSchoolEnrollment) data;
            m_startDate = null;
            m_endDate = null;
            m_span = null;
            m_terminatingSpan = null;

            OnsisSchoolStudentEntity parentEntity = ((OnsisSchoolStudentEntity) m_reportData.getParentEntity());
            m_record = parentEntity.getCsvDataRecord();

            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;

            m_spans = getStudent().getEnrollmentSpans(getBroker(), false, false).stream()
                    .filter(span -> getGlobalData().getCurrentSchoolOids().contains(span.getSchool().getOid())
                            && getGlobalData().getDateRange().isOverlap(span.getDateRange())
                            && span.getContext().getOid().equals(getGlobalData().getCurrentContext().getOid()))
                    .collect(Collectors.toList());


            if (m_spans.isEmpty()) {
                SubmissionType submissionType = getGlobalData().getSubmissionType();
                Collection<FteMonthly> fteStudentRecords =
                        getStudent().getFteMonthlyRecords(getBroker(), getGlobalData().getCurrentSchoolOids(),
                                getGlobalData().getSubmissionType().getSubmissionPeriodCode());
                if (!fteStudentRecords.isEmpty()) {
                    PlainDate queryAsOfDate = submissionType.getCountDates().stream().max(Date::compareTo).get();
                    m_spans = getStudent().getEnrollmentSpans(getBroker(), queryAsOfDate, true, false).stream()
                            .filter(span -> getGlobalData().getCurrentSchoolOids().contains(span.getSchool().getOid())
                                    && Range.of(getGlobalData().getStartDate(), queryAsOfDate)
                                            .isOverlap(span.getDateRange()))
                            .collect(Collectors.toList());
                }
            }

            m_span = (OnsisAnnualSpan) m_spans.stream().reduce((first, second) -> second).orElse(null);
            if (debugOutput != null) {
                debugOutput.append("annualSpans: " + m_spans + "\n");
                debugOutput.append("m_span: " + m_span + "\n");
            }

            if (m_span == null) {
                getReportData().log("enrollments = " + getStudent().getEnrollments(getBroker()));
                getReportData().log("aspen spans = " + getStudent().getAspenSpans(getBroker(), true, false));
                List<AnnualSpan> enrollmentSpans = getStudent().getEnrollmentSpans(getBroker(), true, false).stream()
                        .map(span -> {
                            getReportData().log("candidate span = " + span);
                            return span;
                        })
                        .filter(span -> getGlobalData().getCurrentSchoolOids().contains(span.getSchool().getOid())
                                && Range.of(null, getGlobalData().getStartDate())
                                        .isOverlap(span.getDateRange()))
                        .map(span -> {
                            getReportData().log("selected span = " + span);
                            return span;
                        })
                        .collect(Collectors.toList());

                AnnualSpan lastSpan =
                        enrollmentSpans.stream()
                                .filter(span -> !span.isSecondary())
                                .sorted(Comparator.comparing(AnnualSpan::getFirstInactiveInSessionDate,
                                        Comparator.nullsLast(Comparator.reverseOrder())))
                                .findFirst().orElse(null);

                getReportData().log("lastSpan = " + lastSpan);
                if (lastSpan != null && lastSpan.getTerminatingEnrollment() != null && StudentEnrollment.WITHDRAWAL
                        .equals(lastSpan.getTerminatingEnrollment().getEnrollmentType())) {
                    m_terminatingSpan = (OnsisAnnualSpan) lastSpan;
                }
            }

            if (debugOutput != null) {
                m_reportData.log(debugOutput.toString());
            }
            setRowCount(m_span == null && m_record == null ? 0 : 1);
        }

        /**
         * Checks if is main school.
         *
         * @return Boolean
         */
        public Boolean isMainSchool() {
            if (getSpan() != null) {
                if (getSpan().isSecondary()) {
                    OnStudentSchool ssk = (OnStudentSchool) getSpan().getSecondary();
                    return Boolean.valueOf(ssk.getMainSchoolOverride());
                }
                if (OnsisConstants.VALUE_STU_BRD_RES_STAT_E_LEARNING
                        .equals(getFieldValue(FIELD_BOARD_RESIDENCE_STATUS))) {
                    return Boolean.FALSE;
                }
                if (!getSpan().getDateRange().contains(getGlobalData().getEndDate())) {
                    // Test if student withdrawal with exit education code
                    if (!isEducationComplete() && !isDiploma() && !isOyap() && !isOntarioScholar()
                            && !getGlobalData().getSubmissionType().isECPPSubmission()) {
                        if (!getGlobalData().getSubmissionType().isJuneSubmission()) {
                            return Boolean.FALSE;
                        }
                        PlainDate lastInSessionDate =
                                getSpan().getSchoolCalendar().findFirstInSessionDate(getBroker(), null, false);
                        if (!getSpan().getDateRange().contains(lastInSessionDate)) {
                            return Boolean.FALSE;
                        }
                    }
                }
                return Boolean.TRUE;
            }
            String csvValue = m_record.getSingleFieldValue(CsvField.MAIN_SCHOOL_FLAG);
            return Boolean.valueOf("T".equals(csvValue));
        }

        /**
         * Checks if is special education.
         *
         * @return true, if is special education
         */
        public boolean isSpecialEducation() {
            return isSpecialEducation(getOnsisEnrollmentEndDate());
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

            List<Element> classAssignmentDeletes =
                    getElementsWithChildValue(ELEMENT_STUDENT_CLASS_ENROLMENT, entityElement,
                            OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_DELETE);
            for (Element classAssignmentDelete : classAssignmentDeletes) {
                m_reportData.log("remove deletes from parent add " + ELEMENT_STUDENT_CLASS_ENROLMENT + " with "
                        + ELEMENT_CLASS_CODE + " = "
                        + OnsisStateReportData.getChildText(ELEMENT_CLASS_CODE, classAssignmentDelete));
                entityElement.removeChild(classAssignmentDelete);
            }

            // rule MRCFRE001a
            if (getGlobalData().getSubmissionType().isMarchSubmission()
                    && getGlobalData().getSubmissionType().isElementarySubmission()) {
                List<Element> candidateSLPs = Stream.concat(Stream.concat(
                        getElementsWithChildValue(ELEMENT_SECOND_LANGUAGE_PROGRAM, entityElement, ELEMENT_TYPE,
                                OnStudentSLPProgram.TYPE_CORE).stream(),
                        getElementsWithChildValue(ELEMENT_SECOND_LANGUAGE_PROGRAM, entityElement, ELEMENT_TYPE,
                                OnStudentSLPProgram.TYPE_EXTENDED).stream()),
                        getElementsWithChildValue(ELEMENT_SECOND_LANGUAGE_PROGRAM, entityElement, ELEMENT_TYPE,
                                OnStudentSLPProgram.TYPE_EXTENDED).stream())
                        .collect(Collectors.toList());
                if (!candidateSLPs.isEmpty()) {
                    Element subjectStrandElement =
                            getElementsWithChildValue(OnsisReportCardTermEntity.ELEMENT_SUBJECT_STRAND,
                                    getElementsWithChildValue(OnsisReportCard.ELEMENT_TERM,
                                            getChildElement(ELEMENT_REPORT_CARD, entityElement), ELEMENT_TERM_CODE,
                                            OnsisReportCardTermEntity.ALT_REPORT_CARD_TERM_1)
                                                    .stream().findFirst().orElse(null),
                                    OnsisReportCardTermEntity.ELEMENT_SUBJECT_STRAND_CODE,
                                    OnsisReportCardSubjectStrandEntity.FRENCH_HEADER_SUBJECT)
                                            .stream().findFirst().orElse(null);
                    if (subjectStrandElement != null) {
                        Element naFlagElement = OnsisStateReportData
                                .getChildElement(OnsisReportCardTermEntity.ELEMENT_NA_FLAG, subjectStrandElement);
                        if (OnsisReportCardTermEntity.FLAG_T.equals(naFlagElement.getTextContent())) {
                            for (Element candidateSLP : candidateSLPs) {
                                m_reportData.log(
                                        "remove deletes from parent add " + ELEMENT_SECOND_LANGUAGE_PROGRAM + " with "
                                                + ELEMENT_SECOND_LANGUAGE_PROGRAM + " = "
                                                + OnsisStateReportData.getChildText(ELEMENT_SECOND_LANGUAGE_PROGRAM,
                                                        candidateSLP));
                                entityElement.removeChild(candidateSLP);
                            }

                        }
                    }
                }

            }
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
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.jdom.Element,
         *      org.jdom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            /*
             * m_spans includes spans that aren't active on the Submission End Date
             *
             * Those spans should only be exported if:
             * - The span starts before the reporting period or CSV record exists
             *
             * OR at least one of the following:
             * - Class Enrolment with earned credit > 0
             * - Diploma
             * - PLAR
             * - Other Credit
             * - SAL
             * - SHSM Program
             */
            boolean isInCsv = (m_record != null);
            if (isInCsv) {
                return false;
            }

            /*
             * If the span ends before it starts,
             * don't export it unless it was in the CSV.
             * If it's in the CSV, the start date will be replaced
             * with the original entry date from the CSV,
             * and an Update will export with the Enrollment End Date.
             */
            PlainDate onsisEnrollmentStartDate = getOnsisEnrollmentStartDate();
            PlainDate onsisEnrollmentEndDate = getOnsisEnrollmentEndDate();
            if (onsisEnrollmentStartDate == null
                    || (onsisEnrollmentEndDate != null && onsisEnrollmentEndDate.before(onsisEnrollmentStartDate))) {
                getReportData().log("OnsisStudentEnrollmentEntity.isRowCanceled(onsisEnrollmentStartDate="
                        + onsisEnrollmentStartDate + " to onsisEnrollmentEndDate=" + onsisEnrollmentEndDate + ".)");
                return true;
            }

            if (onsisEnrollmentEndDate != null) {
                List<Element> specialEducationDeletes =
                        getElementsWithChildValue(ELEMENT_SPECIAL_EDUCATION, entityElement,
                                OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_DELETE);
                for (Element specialEducationDelete : specialEducationDeletes) {
                    m_reportData.log("remove delete " + ELEMENT_SPECIAL_EDUCATION);
                    entityElement.removeChild(specialEducationDelete);
                }


                List<Element> secondLanguageProgramDeletes =
                        getElementsWithChildValue(ELEMENT_SECOND_LANGUAGE_PROGRAM, entityElement,
                                OnsisRetrieverAction.ELEMENT_NAME_ACTION, OnsisRetrieverAction.ACTION_DELETE);
                for (Element secondLanguageProgramDelete : secondLanguageProgramDeletes) {
                    m_reportData.log("remove delete " + ELEMENT_SECOND_LANGUAGE_PROGRAM);
                    entityElement.removeChild(secondLanguageProgramDelete);
                }
            }

            /*
             * Don't cancel the span if it overlaps report date range
             */
            Range<Date> enrollmentDateRange = Range.of(onsisEnrollmentStartDate, onsisEnrollmentEndDate);
            if (enrollmentDateRange.isOverlap(getGlobalData().getDateRange())) {
                return false;
            }

            /*
             * Don't cancel the span if it crosses the report start date.
             *
             * Look at original entry (could be prior year)
             * rather than span's first active for this year
             */
            // PlainDate originalStartDate =
            // getSpan().getParentSpan().getFirstActiveInSessionDate();
            // boolean crossesStartDate = originalStartDate != null
            // && originalStartDate.before(getReportData().getStartDate())
            // && (onsisEnrollmentEndDate == null
            // || !onsisEnrollmentEndDate.before(getReportData().getStartDate()));
            // if (crossesStartDate) {
            // return false;
            // }

            // Is there a ClassEnrolment with earned credit > 0?
            boolean hasReportableClassEnrollment = false;
            List<Element> classEnrolments =
                    OnsisStateReportData.getChildElements(ELEMENT_STUDENT_CLASS_ENROLMENT, entityElement);
            if (classEnrolments != null && !classEnrolments.isEmpty()) {
                for (Element classEnrolment : classEnrolments) {
                    if (OnsisStateReportData.getChildElement(ELEMENT_AVERAGE_DAILY_ENROLMENT, classEnrolment) != null) {
                        hasReportableClassEnrollment = true;
                        break;
                    }
                    try {
                        String creditText =
                                OnsisStateReportData.getChildText(ELEMENT_FINAL_MARK, classEnrolment);
                        if (Float.valueOf(creditText) > 0.0) {
                            hasReportableClassEnrollment = true;
                            break;
                        }
                    } catch (Exception e) {
                        //
                    }
                }
            }
            if (hasReportableClassEnrollment) {
                return false;
            }

            if (OnsisStateReportData.getChildElement(ELEMENT_SHSM_PROGRAM, entityElement) != null
                    || OnsisStateReportData.getChildElement(ELEMENT_DIPLOMA, entityElement) != null
                    || OnsisStateReportData.getChildElement(ELEMENT_PLAR, entityElement) != null
                    || OnsisStateReportData.getChildElement(ELEMENT_OTHER_CREDIT, entityElement) != null
                    || OnsisStateReportData.getChildElement(ELEMENT_SALEP, entityElement) != null
                    || OnsisStateReportData.getChildElement(ELEMENT_NON_CREDIT_AVERAGE_DAILY_ENROLMENT,
                            entityElement) != null) {
                return false;
            }

            if (OnsisStateReportData.getChildElement(ELEMENT_REPORT_CARD, entityElement) != null) {
                return false;
            }

            if (!getStudent().getFteMonthlyRecords(getBroker(), getGlobalData().getCurrentSchoolOids(),
                    getGlobalData().getSubmissionType().getSubmissionPeriodCode()).isEmpty()) {
                return false;
            }

            return true;
        }

        /**
         * Checks if is address required.
         *
         * @return true, if is address required
         */
        private boolean isAddressRequired() {
            String mobilityType = getFieldValue(FIELD_MOBILITY_TYPE_ENTRY);
            return !OnsisConstants.VALUE_MOBILITY_TYPES_ADDRESS_NOT_REQUIRED.contains(mobilityType);
        }

        /**
         * Always return span's firstInactiveInSession Date UNLESS:
         * - "Future"
         * - Condition: span firstInactiveInSession > Report End Date
         * - Return: null
         *
         * - "Summer Withdraw needs to report"
         * - Condition: Span's LastActiveInSessionDate is < Report Start; and W-1 >= Start Date.
         * - Return: W-1 date (assuming W exists and W-1 >= Start Date; else null)
         *
         * @return PlainDate
         */
        private PlainDate computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate() {
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            PlainDate spanEndDate = getSpan().getSpanEndDate();
            PlainDate submissionEndDate = getGlobalData().getEndDate();

            /*
             * Handle future firstInactiveInSessionDate (after Report End Date)
             */
            if (!submissionType.isJuneSubmission() && !submissionType.isECPPSubmission()) {
                if (spanEndDate != null) {
                    if (spanEndDate.after(submissionEndDate)
                            || getSpan().getDateAfterLastActiveInSessionDate().after(submissionEndDate)) {
                        spanEndDate = null;
                    } else {
                        if (getStudent().getFteMonthlyRecord(getBroker(), getGlobalData().getCurrentSchoolOids(),
                                getGlobalData().getSubmissionType().getSubmissionPeriodCode()) != null) {
                            spanEndDate = null;
                        }
                    }
                }
                getReportData().log(
                        "computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate MOY - spanEndDate " + spanEndDate
                                + "\n");
                return spanEndDate == null ? null
                        : OnsisHelpersContainer.maxDate(spanEndDate, submissionType.getPeriodStartDate());
            }

            /*
             * Is student active on last in session date?
             * Return firstInactiveInSessionDate if student not active on schoolLastInSessionDate.
             */
            boolean forward = false;
            PlainDate schoolLastInSessionDate = getSpan().getSchoolCalendar().findFirstInSessionDate(getBroker(),
                    getSpan().getContext().getEndDate(), forward);
            PlainDate lastActiveDate = getSpan().getParentSpan().getLastActiveInSessionDate();
            if (lastActiveDate != null && lastActiveDate.before(schoolLastInSessionDate)) {
                getReportData().log(
                        "computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate - spanEndDate " + spanEndDate + "\n");
                return spanEndDate;
            }

            /*
             * Applies to all students active on last in-session day for June Submission
             */
            PlainDate julyFirst = getGlobalData().getJuly1Date();

            /*
             * 1. If secondary enrolment (shared student)
             */
            if (getSpan().isSecondary()) {
                getReportData().log(
                        "computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate - isSecondary " + julyFirst + "\n");
                return julyFirst;
            }

            /*
             * 2. Student graduating: Earned Diploma (for example OSSD) during submission period and
             * Empty Next School + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT: 61
             */
            if (earnedDiplomaNotComingBack()) {
                getReportData()
                        .log("computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate - earnedDiplomaNotComingBack "
                                + julyFirst + "\n");
                return julyFirst;
            }

            /*
             * 3. Student leaving board (all-std-EoyrDemitCode is not empty)
             */
            String eoyrDemitCode =
                    getDictionaryExtractor().getStateValue(getStudent(), OnsisStudent.FIELD_EOY_DEMIT_CODE);
            if (!StringUtils.isBlank(eoyrDemitCode)) {
                getReportData().log("computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate - eoyrDemitCode "
                        + eoyrDemitCode + " Date " + julyFirst + "\n");
                return julyFirst;
            }

            /*
             * * 4. If existing W record on or before 7/1/YYYY
             * + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT from W record
             */
            PlainDate withdrawalMemberDate = getSpan().getWithdrawalMemberDate();
            if (withdrawalMemberDate != null && withdrawalMemberDate.before(julyFirst)) {
                getReportData().log("computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate - withdrawalMemberDate "
                        + julyFirst + "\n");
                return julyFirst;
            }

            /*
             * 5. Student changing schools next year (progressing or moving to another school):
             * Next school set on Student Record and not equal to current school.
             * + ENROLMENT_END_DATE: 7/1/YYYY, STUDENT_MOBILITY_TYPE_EXIT: 64
             */
            if (getStudent().isChangingSchool()) {
                getReportData().log("computeOnsisEnrollmentEndDate_BasedOnFirstInactiveDate - isChangingSchool "
                        + julyFirst + "\n");
                return julyFirst;
            }

            // 6. Otherwise, send no ENROLMENT_END_DATE or STUDENT_MOBILITY_TYPE_EXIT for student
            return null;
        }

        /**
         * Gets the absences count date.
         *
         * @return Plain date
         */
        private PlainDate getAbsencesCountDate() {
            return getOnsisEnrollmentEndDate() == null
                    ? getGlobalData().getEndDate()
                    : (getOnsisEnrollmentEndDate().before(getGlobalData().getEndDate())
                            ? getOnsisEnrollmentEndDate()
                            : getGlobalData().getEndDate());
        }

        /**
         * Gets the absences for closure double.
         *
         * @return double
         */
        private double getAbsencesForClosureDouble() {
            Double absencesSum = 0.0;
            PlainDate absencesCountDate = getAbsencesCountDate();

            List<OnStudentAttendance> gDaysAM = getStudent().getAttendance(getBroker()).stream()
                    .map(att -> (OnStudentAttendance) att)
                    .filter(att -> OnStudentAttendance.STD_ATT_G_DAY_CODE.equals(att.getOtherCode())
                            && getReportData().getClosureReasonCodes().contains(att.getReasonCode()))
                    .collect(Collectors.toList());
            List<OnStudentAttendance> gDaysPM = getStudent().getAttendance(getBroker()).stream()
                    .map(att -> (OnStudentAttendance) att)
                    .filter(att -> OnStudentAttendance.STD_ATT_G_DAY_CODE.equals(att.getOtherCode02())
                            && getReportData().getClosureReasonCodes().contains(att.getReasonCode02()))
                    .collect(Collectors.toList());

            absencesSum = gDaysAM.stream()
                    .filter(absence -> !absence.getDate().after(absencesCountDate))
                    .filter(absence -> (!absence.getDate().before(getSpan().getFirstActiveInSessionDate()))
                            && (getSpan().getLastActiveInSessionDate() == null
                                    || !absence.getDate().after(getSpan().getLastActiveInSessionDate())))
                    .mapToDouble(absence -> 0.5).sum() +
                    gDaysPM.stream()
                            .filter(absence -> !absence.getDate().after(absencesCountDate))
                            .filter(absence -> (!absence.getDate().before(getSpan().getFirstActiveInSessionDate()))
                                    && (getSpan().getLastActiveInSessionDate() == null
                                            || !absence.getDate().after(getSpan().getLastActiveInSessionDate())))
                            .mapToDouble(absence -> 0.5).sum();

            return absencesSum;
        }

        /**
         * Gets the address.
         *
         * @return onsis address
         */
        private OnAddress getAddress() {
            OnAddress address = null;
            if (getStudent().getShelteredStudentIndicator()) {
                address = (OnAddress) getSpan().getSchool().getAddress(getBroker());
            } else {
                ToolPersonAddress personAddress =
                        getStudent().getPersonAddresses(getBroker()).stream().findFirst().orElse(null);
                address = personAddress == null ? (OnAddress) getStudent().getPhysicalAddress(getBroker())
                        : (OnAddress) personAddress.getAddress(getBroker());
            }
            return address;
        }

        /**
         * Gets the ade.
         *
         * @return double
         */
        private Double getAdeRaw() {
            PlainDate summerStartDate = getGlobalData().getSummerStartDate();
            final StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;

            Double adeValue = getStudent().getStudentScheduleSpans(getBroker(), getGlobalData(), null).stream()
                    .map(span -> {
                        if (debugOutput != null) {
                            debugOutput.append("getAdeRaw filter test isIndependentStudy: " + span.isIndependentStudy()
                                    + " candidate: " + span + "\n");
                        }
                        return span;
                    })
                    .filter(span -> span.isIndependentStudy())
                    .map(span -> {
                        OnSection section = (OnSection) span.getSection();
                        Range<Date> range = getGlobalData().getDateRange().intersection(span.getDateRange());
                        PlainDate startDate = new PlainDate(range.getStart());
                        PlainDate endDate = new PlainDate(range.getEnd());
                        boolean isSummerDate = startDate != null && !startDate.before(summerStartDate);
                        Double ade = 0.0;
                        Boolean isSummer = false;
                        if (debugOutput != null) {
                            debugOutput.append("getAdeRaw included span: " + span + "\n");
                            debugOutput.append("section: " + section + " range: " + range + " isSummerDate: "
                                    + isSummerDate + "\n");
                        }
                        if ((isSummer && isSummerDate) || (!isSummer && !isSummerDate)) {
                            ade = section.getAde(getGlobalData().getDateRange(), getStudent(), startDate, endDate,
                                    debugOutput);
                        }
                        if (debugOutput != null) {
                            debugOutput.append("getAdeRaw ade: " + ade + "\n");
                        }
                        return ade;
                    })
                    .reduce(0.0, Double::sum);
            if (debugOutput != null) {
                debugOutput.append("getAdeRaw adeValue: " + adeValue + "\n");
                m_reportData.log(debugOutput.toString());
            }
            return adeValue;
        }

        /**
         * Gets the csv data record.
         *
         * @return Onsis csv data record
         */
        private OnsisCsvDataRecord getCsvDataRecord() {
            return m_record;
        }

        /**
         * Gets the extended program.
         *
         * @return Student program participation
         */
        private OnStudentExtendedDay getExtendedProgram() {
            return getStudent().getExtendedDayPrograms(getBroker()).stream()
                    .filter(pgm -> getSpan().getSchool().getOid().equals(pgm.getSchoolOid()))
                    .findFirst().orElse(null);
        }

        /**
         * Gets the high credit factor.
         *
         * @return Float
         */
        private Double getHighCreditFactor() {
            Double highCreditFactor = NumberUtils.DOUBLE_ZERO;
            FteMonthly monthlyFteRecord =
                    getStudent().getFteMonthlyRecord(getBroker(), getGlobalData().getCurrentSchoolOids(),
                            getGlobalData().getSubmissionType().getSubmissionPeriodCode());
            if (monthlyFteRecord != null) {
                double fte = monthlyFteRecord.getFte().doubleValue();
                double fteHc = monthlyFteRecord.getFteHc().doubleValue();
                highCreditFactor = fte + fteHc == 0 ? NumberUtils.DOUBLE_ZERO : fteHc / (fte + fteHc);
            }
            return highCreditFactor;
        }

        /**
         * Checks if is 21 or older.
         *
         * @return true, if is 21 or older
         */
        private boolean is21OrOlder() {
            Integer age = getStudent().getAgeAsOfDate(getGlobalData().getDateDecember31());
            return age != null && age >= 21 ? true : false;
        }

        /**
         * Return true if student current school is not the same as the next school.
         *
         * @return True when next school is not the same as the student school.
         */
        private boolean isChangingSchool() {
            String currentSchoolOid = getStudent().getSchoolOid();
            String nextSchoolOid = getStudent().getNextSchoolOid();

            if (!StringUtils.isEmpty(nextSchoolOid) && !nextSchoolOid.equals(currentSchoolOid)) {
                return true;
            }

            return false;
        }

        /**
         * Checks if is diploma.
         *
         * @return true, if is diploma
         */
        private boolean isDiploma() {
            String thisBSID = deepGetFieldValueByFieldName(OnsisSchoolData.FIELD_SCHOOL_NUMBER);
            PlainDate rangeStartDate = getOnsisEnrollmentStartDate();
            PlainDate rangeEndDate = getOnsisEnrollmentEndDate();
            if (rangeEndDate != null && rangeEndDate.before(getGlobalData().getEndDate())) {
                rangeEndDate = getGlobalData().getEndDate();
            }
            return !getStudent()
                    .getProgramStudies(getGlobalData().getDateRange(), thisBSID, true,
                            Range.of(rangeStartDate, rangeEndDate))
                    .isEmpty();
        }

        /**
         * Checks if is graduated.
         *
         * @return boolean
         */
        private boolean isEducationComplete() {
            OnEnrollment finalEnrollment = (OnEnrollment) getSpan().getTerminatingEnrollment();

            if (finalEnrollment != null && StudentEnrollment.WITHDRAWAL.equals(finalEnrollment.getEnrollmentType())) {
                String withdrawalCode =
                        getDictionaryExtractor().getStateValue(finalEnrollment, ToolEnrollment.FIELD_ENROLLMENT_CODE);
                if (EDUCATION_COMPLETED_CODES.contains(withdrawalCode)) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Checks if is ontario scholar.
         *
         * @return true, if is ontario scholar
         */
        private boolean isOntarioScholar() {
            PlainDate issuedDate = getStudent().getOntarioScholarAwardDate();
            return issuedDate != null && issuedDate.before(getGlobalData().getEndDate())
                    && !getGlobalData().getStartDate().after(issuedDate);
        }

        /**
         * Checks if is oyap.
         *
         * @return true, if is oyap
         */
        private boolean isOyap() {
            return !getStudent().getOyapPrograms(getBroker()).isEmpty();
        }

    }


    /**
     * The Class OnsisStudentEntity.
     */
    public static class OnsisStudentEntity extends OnsisStateReportEntity {
        private static final String VALUE_DIPLOMA_CERTIFICATE_TYPE_OSSD2 = "2";
        private static final String VALUE_GRADUATION_REQUIREMENT_LITERACY = "LITERACY";
        private static final String VALUE_LITERACY_STATUS_TYPE_AJUDICATION = "4";
        private static final String VALUE_LITERACY_STATUS_TYPE_NOT_APPLICABLE = "3";
        private static final String VALUE_LITERACY_STATUS_TYPE_OLC = "6";
        private static final List<String> VALUE_OSSLT_COURSES = Arrays.asList("OLC3O", "OLC4O", "CCL3O", "CCL4O");
        private static final String VALUE_OSSLT_OUTCOME_SUCCESSFUL = "1";

        private OnsisStudent m_student;

        /**
         * Gets the community involvement hours.
         *
         * @return Big decimal
         */
        public BigDecimal getCommunityInvolvementHours() {
            return getStudent().getCommunityInvolvementAssessments(getBroker()).stream()
                    .map(asm -> asm.getHours())
                    .filter(Objects::nonNull)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnsisStudent student = (OnsisStudent) getBean();
            String name = student.getNameView();

            return name;
        }

        /**
         * Gets the student literacy status.
         *
         * @return String
         */
        public String getLiteracyStatus() {
            final List<String> VALID_DIPLOMA_TYPES_FOR_LITERACY =
                    Arrays.asList(OnGraduationStudentProgram.DIPLOMA_TYPE_COLLEGE_PREP,
                            OnGraduationStudentProgram.DIPLOMA_TYPE_OSSD_SHSM);
            GlobalData globalData = getGlobalData();
            OnsisStudent student = getStudent();
            String gradeLevel = getFieldValue(FIELD_GRADE_LEVEL);

            String result = null;
            if (isGradeLevel10to12(gradeLevel)) {
                List<OnGraduationStudentProgram> gsrList = student.getProgramStudies(globalData.getBroker());

                if (gsrList.stream().filter(gsr -> {
                    if (OnGraduationStudentProgram.DIPLOMA_TYPE_COLLEGE_PREP
                            .equals(globalData.getDictionaryExtractor().getStateValue(gsr,
                                    OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE))) {
                        return gsr.getWaivers(globalData.getBroker()).stream()
                                .filter(gsw -> VALUE_GRADUATION_REQUIREMENT_LITERACY
                                        .equals(globalData.getDictionaryExtractor().getStateValue(gsw,
                                                OnGraduationStudentWaiver.FIELD_REQUIREMENT_CODE)))
                                .findAny().isPresent();
                    }
                    return false;
                }).findAny().isPresent()) {
                    /*
                     * Diploma type is VALUE_DIPLOMA_CERTIFICATE_TYPE_OSSD and associated
                     * GraduationStudentWaiver has an ALIAS_GSW_WAIVER_REASON#state value of
                     * VALUE_LITERACY_STATUS_TYPE_NOT_APPLICABLE
                     */
                    result = VALUE_LITERACY_STATUS_TYPE_NOT_APPLICABLE;
                } else if (gsrList.stream()
                        .filter(gsr -> VALID_DIPLOMA_TYPES_FOR_LITERACY.contains(globalData.getDictionaryExtractor()
                                .getStateValue(gsr, OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE)))
                        .findAny().isPresent()) {
                    /*
                     * Diploma type is either VALUE_DIPLOMA_CERTIFICATE_TYPE_OSSD or
                     * VALUE_DIPLOMA_CERTIFICATE_TYPE_OSSD_SHSM
                     */
                    OssltAssessment recentAssessment =
                            student.getOssltAssessments(globalData.getBroker()).stream().findFirst().orElse(null);

                    boolean hasSuccessfulAssessment = false;
                    String assessmentLanguageCode = null;
                    if (recentAssessment != null) {
                        if (VALUE_OSSLT_OUTCOME_SUCCESSFUL.equals(recentAssessment.getOutcome())) {
                            hasSuccessfulAssessment = true;
                            assessmentLanguageCode = recentAssessment.getLanguage();
                        }
                    }

                    if (hasSuccessfulAssessment) {
                        result = assessmentLanguageCode;
                    } else if (isCompletedOLCCourse() && !isCompletedAdjudication()) {
                        result = VALUE_LITERACY_STATUS_TYPE_OLC;
                    } else if (isCompletedAdjudication()) {
                        result = VALUE_LITERACY_STATUS_TYPE_AJUDICATION;
                    }
                } else if (gsrList.stream()
                        .anyMatch(gsr -> VALUE_DIPLOMA_CERTIFICATE_TYPE_OSSD2.equals(globalData.getDictionaryExtractor()
                                .getStateValue(gsr, OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE)))) {
                    result = VALUE_LITERACY_STATUS_TYPE_NOT_APPLICABLE;
                }
            }
            return result;
        }

        /**
         * Gets the online graduation requirement.
         *
         * @return the online graduation requirement
         */
        public String getOnlineGraduationRequirement() {
            String value = null;
            if (getStudent().getGrade9Cohort() >= 2020) {
                BigDecimal totalOnlineCredit = getStudent().getStudentTranscripts(getBroker()).stream()
                        .map(trn -> (OnTranscript) trn)
                        .filter(trn -> trn.getOnlineLearningCreditIndicator() && trn.getDateCompleted() != null
                                && !trn.getDateCompleted().after(getGlobalData().getEndDate())
                                && trn.getTotalCredit() != null && trn.getTotalCredit().compareTo(BigDecimal.ZERO) > 0)
                        .map(trn -> trn.getTotalCredit())
                        .reduce(BigDecimal.ZERO, BigDecimal::add);
                if (totalOnlineCredit.compareTo(BigDecimal.valueOf(2)) >= 0) {
                    value = OnGraduationStudentProgram.ONLINE_GRADUATION_REQUIREMENT_COMPLETED;
                }
                if (value == null) {
                    List<OnGraduationStudentProgram> gsrList = getStudent().getProgramStudies(getBroker());
                    if (gsrList.stream().filter(gsr -> {
                        if (OnGraduationStudentProgram.DIPLOMA_TYPE_COLLEGE_PREP
                                .equals(getDictionaryExtractor().getStateValue(gsr,
                                        OnGraduationStudentProgram.FIELD_DIPLOMA_TYPE))) {
                            return gsr.getWaivers(getBroker()).stream()
                                    .filter(gsw -> OnGraduationStudentProgram.ONLINE_GRADUATION_REQUIREMENT_NOT_APPLICABLE
                                            .equals(gsw.getWaiverReason()))
                                    .findAny().isPresent();
                        }
                        return false;
                    }).findAny().isPresent()) {
                        value = OnGraduationStudentProgram.ONLINE_GRADUATION_REQUIREMENT_NOT_APPLICABLE;
                    }
                }
            } else {
                value = OnGraduationStudentProgram.ONLINE_GRADUATION_REQUIREMENT_NOT_APPLICABLE;
            }
            return value;
        }

        /**
         * Gets the scholarship date.
         *
         * @return Plain date
         * @throws X2BaseException exception
         */
        public PlainDate getScholarshipDate() throws X2BaseException {
            PlainDate date = null;

            PlainDate issuedDate = getStudent().getScholarAwardedDate();

            if (issuedDate != null && getGlobalData().getDateRange().contains(issuedDate)) {
                date = issuedDate;
            }
            return date;
        }

        /**
         * Gets the scholarship flag.
         *
         * @return Boolean
         */
        public String getScholarshipFlag() {
            // only send this element when the date is being sent
            return StringUtils.isEmpty(this.getFieldValue(FIELD_ONTARIO_SCHOLARSHIP_DATE)) ? null : "T";
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public OnsisStudent getStudent() {
            return m_student;
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
            super.intitialize(data, bean);

            m_student = (OnsisStudent) bean;
        }

        /**
         * Checks if is special education.
         *
         * @param withdrawalDate PlainDate
         * @return true, if is special education
         */
        public boolean isSpecialEducation(PlainDate withdrawalDate) {
            if (getGlobalData().getSubmissionType().isOctoberSubmission()
                    && this instanceof OnsisStudentEnrollmentEntity
                    && ((OnsisStudentEnrollmentEntity) this).getSpan() == null) {
                return false;
            }
            PlainDate evaluationDate = getGlobalData().getEndDate();
            if (withdrawalDate != null && getGlobalData().getDateRange().contains(withdrawalDate)) {
                evaluationDate = withdrawalDate;
            }
            PlainDate testDate = evaluationDate;
            return getStudent().getSpedPrograms(getBroker()).stream()
                    .filter(pgm -> pgm.getDateRange().contains(testDate) && pgm.getSpedReportIndicator())
                    .findAny().isPresent();
        }

        /**
         * Checks if is completed adjudication.
         *
         * @return true, if is completed adjudication
         */
        private boolean isCompletedAdjudication() {
            Object value = getStudent().getOssltAdjudication();
            if (value instanceof Boolean) {
                return ((Boolean) value).booleanValue();
            } else if (value instanceof PlainDate) {
                return !((PlainDate) value).after(getGlobalData().getEndDate());
            }
            return false;
        }

        /**
         * Checks if is completed OLC course.
         *
         * @return true, if is completed OLC course
         */
        private boolean isCompletedOLCCourse() {
            return getStudent().getStudentTranscripts(getGlobalData().getBroker()).stream()
                    .anyMatch(trn -> OnsisHelpersContainer.greaterThan(trn.getTotalCredit(), BigDecimal.ZERO)
                            && VALUE_OSSLT_COURSES.contains(((OnTranscript) trn).getMinistryCourseCode()));
        }


    }

    public static final String ALIAS_ORG_GRADE_DESIGNATION = "all-org-GradeDesignation";

    public static final String DEFAULT_GRADE_DESIGNATION = "1";

    public static final List<String> EDUCATION_COMPLETED_CODES = Arrays.asList("18", "61", "62", "63");

    public static final String ELEMENT_AVERAGE_DAILY_ENROLMENT = "AVERAGE_DAILY_ENROLMENT";
    public static final String ELEMENT_CERTIFICATE_ISSUED = "CERTIFICATE_ISSUED";
    public static final String ELEMENT_CLASS_CODE = "CLASS_CODE";
    public static final String ELEMENT_DIPLOMA = "DIPLOMA";
    public static final String ELEMENT_EARNED_CREDIT_VALUE = "EARNED_CREDIT_VALUE";
    public static final String ELEMENT_ENROLMENT_END_DATE = "ENROLMENT_END_DATE";
    public static final String ELEMENT_ENROLMENT_START_DATE = "ENROLMENT_START_DATE";
    public static final String ELEMENT_FINAL_MARK = "FINAL_MARK";
    public static final String ELEMENT_OTHER_CREDIT = "OTHER_CREDIT";
    public static final String ELEMENT_NAME_OEN = "OEN";
    public static final String ELEMENT_NON_CREDIT_AVERAGE_DAILY_ENROLMENT = "NON_CREDIT_AVERAGE_DAILY_ENROLMENT";
    public static final String ELEMENT_PLAR = "PLAR";
    public static final String ELEMENT_REPORT_CARD = "REPORT_CARD";
    public static final String ELEMENT_SALEP = "SALEP";
    public static final String ELEMENT_SECOND_LANGUAGE_PROGRAM = "SECOND_LANGUAGE_PROGRAM";
    public static final String ELEMENT_SHSM_PROGRAM = "SHSM_PROGRAM";
    public static final String ELEMENT_SPCE = "SPCE";
    public static final String ELEMENT_SPCE_TYPE = "SPCE_TYPE";
    public static final String ELEMENT_STUDENT_CLASS_ENROLMENT = "STUDENT_CLASS_ENROLMENT";
    public static final String ELEMENT_SPECIAL_EDUCATION = "SPECIAL_EDUCATION";
    public static final String ELEMENT_STUDENT_SCHOOL_ENROLMENT = "STUDENT_SCHOOL_ENROLMENT";
    public static final String ELEMENT_TERM_CODE = "TERM_CODE";
    public static final String ELEMENT_TYPE = "TYPE";

    public static final String FIELD_BOARD_RESIDENCE_STATUS = "StuBrdResStatType";
    public static final String FIELD_ENROLMENT_END_DATE = "EnrolmentEndDate";
    public static final String FIELD_GRADE_LEVEL = "Grade Level";
    public static final String FIELD_MAIN_SCHOOL_FLAG = "MainSchoolFlag";
    public static final String FIELD_MATURE_STUDENT = "MatureStudentFlag";
    public static final String FIELD_MOBILITY_TYPE_ENTRY = "StudentMobilityType";
    public static final String FIELD_ONTARIO_SCHOLARSHIP_DATE = "OntrScholarshipDate";
    public static final String FIELD_PROVINCE_STATE_TYPE = "ProvinceStateType";
    public static final String FIELD_ACTION = "Action";


    public static final String PATTERN_GRADE_10_12 = "^1[0-2]$";

    /**
     * Checks if is grade level 10 to 12.
     *
     * @param gradeLevel String
     *
     * @return true, if is grade level 10 to 12
     */
    public static boolean isGradeLevel10to12(String gradeLevel) {
        return !StringUtils.isEmpty(gradeLevel) && gradeLevel.matches(PATTERN_GRADE_10_12);
    }

    private Set<String> m_closureReasonCodes;

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans(com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity)
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

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
     * Gets the closure reason codes.
     *
     * @return Sets the
     */
    public Set<String> getClosureReasonCodes() {
        if (m_closureReasonCodes == null) {
            DataDictionaryField field = OnStudentAttendance.FIELD_REASON_CODE.getField(getDictionaryExtractor());
            m_closureReasonCodes = getDictionaryExtractor().getRefCodesWithStateValue(field, Arrays.asList("NRL"))
                    .stream().map(ReferenceCode::getCode).collect(Collectors.toSet());
        }
        return m_closureReasonCodes;
    }

    /**
     * Gets the grade designation.
     *
     * @param student SisStudent
     * @param gradeLevel String
     * @return String
     */
    public String getGradeDesignation(OnsisStudent student, String gradeLevel) {
        String result = EMPTY_STRING;
        if (isGradeLevel10to12(gradeLevel)) {
            result = getDictionaryExtractor().getStateValue(student, OnsisStudent.FIELD_GRADE_DESIGNATION);
            if (StringUtils.isEmpty(result)) {
                result = getDictionaryExtractor().getStateValueByAlias(getOrganization(), ALIAS_ORG_GRADE_DESIGNATION);
            }
            if (StringUtils.isEmpty(result)) {
                result = DEFAULT_GRADE_DESIGNATION;
            }
        }

        return result;
    }

    /**
     * Initialize entity class.
     *
     * @see com.follett.fsc.aspensif.framework.PluginStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisStudentEnrollmentEntity.class);
    }

}
