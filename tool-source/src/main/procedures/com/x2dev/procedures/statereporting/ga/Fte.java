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
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Georgia state report for GTID FTE Student export.
 * This class implements the data export for GA GTID export.
 *
 * @author X2 Development Corporation
 */
public class Fte extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the GA FTE Student export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class FteEntity extends StateReportEntity {

        private IepData m_iep = null;
        private SisSchool m_school = null;
        private String m_status = null;
        private StudentEnrollment m_withdrawalEnr = null;
        private StudentEnrollment m_entryEnr = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public FteEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Filter out students that should not be reported.
         * Use the m_school as an indicator. Student must have this from the initialize step.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            if (m_status == null) {
                error = new StateReportValidationError(this, getData().getFieldDefinition(FTE_0410_STUDENT_STATUS),
                        "Student not active or withdrawn", "Student not active or withdrawn");
            }

            return error;
        }

        /**
         * Initialize.
         * 1. Find student status on report date.
         * 2. Find withdrawal record for withdrawn student in cycle 1.
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

            /*
             * For cycle 1, lookup enrollment record for withdrawal date and reason.
             */
            Fte fteData = (Fte) data;
            SisStudent student = (SisStudent) bean;
            m_school = student.getSchool();

            StudentEnrollment entryEnrToOperate = null;
            StudentEnrollment exitEnrToOperate = null;
            StudentEnrollmentSpan exitSpanToOperate = null;
            PlainDate entryDate = null;
            PlainDate exitDate = null;
            Collection<StudentEnrollmentSpan> enrSpans = fteData.m_stdHelper.getStudentEnrollmentSpans(student, false);

            if (CYCLE_MARCH.equals(fteData.m_cycle)) {
                for (StudentEnrollmentSpan enrSpan : enrSpans) {
                    if (enrSpan.getFirstActiveDate() != null && !enrSpan.getFirstActiveDate().after(fteData.m_fteDate)
                            && (enrSpan.getFirstInactiveEnrollment() == null
                                    || enrSpan.getFirstInactiveEnrollment().getEnrollmentDate()
                                            .after(fteData.m_fteDate))) {
                        if (entryDate == null) {
                            entryDate = enrSpan.getFirstActiveDate();
                            entryEnrToOperate = enrSpan.getFirstActiveEnrollment();
                        } else if (entryDate.before(enrSpan.getFirstActiveDate())) {
                            entryDate = enrSpan.getFirstActiveDate();
                            entryEnrToOperate = enrSpan.getFirstActiveEnrollment();
                        }
                    }
                }
                if (entryEnrToOperate == null) {
                    setRowCount(0);
                } else {
                    m_entryEnr = entryEnrToOperate;
                    m_status = fteData.m_activeCode.equals(entryEnrToOperate.getStatusCode()) ? STATUS_NORMAL : null;
                    m_school = entryEnrToOperate.getSchool();
                }
            } else {
                for (StudentEnrollmentSpan enrSpan : enrSpans) {
                    if (enrSpan.getFirstActiveDate() != null && !enrSpan.getFirstActiveDate().after(fteData.m_fteDate)
                            && (enrSpan.getFirstInactiveEnrollment() == null
                                    || enrSpan.getFirstInactiveEnrollment().getEnrollmentDate()
                                            .after(fteData.m_fteDate))) {
                        if (entryDate == null) {
                            entryDate = enrSpan.getFirstActiveDate();
                            entryEnrToOperate = enrSpan.getFirstActiveEnrollment();
                        } else if (entryDate.before(enrSpan.getFirstActiveDate())) {
                            entryDate = enrSpan.getFirstActiveDate();
                            entryEnrToOperate = enrSpan.getFirstActiveEnrollment();
                        }
                    }
                    if (enrSpan.getFirstInactiveEnrollment() != null
                            && !enrSpan.getFirstInactiveEnrollment().getEnrollmentDate().after(fteData.m_fteDate)
                            && !enrSpan.getFirstInactiveEnrollment().getEnrollmentDate()
                                    .before(fteData.m_withdrawnAfterDate)) {
                        if (exitDate == null) {
                            exitDate = enrSpan.getLastActiveDate();
                            exitEnrToOperate = enrSpan.getFirstInactiveEnrollment();
                            exitSpanToOperate = enrSpan;
                        } else if (exitDate.before(enrSpan.getLastActiveDate())) {
                            exitDate = enrSpan.getLastActiveDate();
                            exitEnrToOperate = enrSpan.getFirstInactiveEnrollment();
                            exitSpanToOperate = enrSpan;
                        }
                    }
                }

                if (entryEnrToOperate != null) {
                    m_entryEnr = entryEnrToOperate;
                    m_status = fteData.m_activeCode.equals(entryEnrToOperate.getStatusCode()) ? STATUS_NORMAL : null;
                    m_school = entryEnrToOperate.getSchool();
                } else if (exitEnrToOperate != null && exitSpanToOperate.getFirstActiveEnrollment() != null) {
                    m_status = STATUS_WITHDRAWN;
                    m_school = exitEnrToOperate.getSchool();
                    m_withdrawalEnr = exitEnrToOperate;
                    m_entryEnr = exitSpanToOperate.getFirstActiveEnrollment();
                } else {
                    setRowCount(0);
                }
            }

            /*
             * Find an IEP for the student.
             */
            m_iep = fteData.m_iepMap.get(student.getOid());
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
         * Returns the students entry enrollment record if there is one.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEntryEnr() {
            return m_entryEnr;
        }

        /**
         * Returns the current active IEP for the student.
         *
         * @return IepData
         */
        public IepData getIep() {
            return m_iep;
        }

        /**
         * Returns student school on report date.
         *
         * @return School
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Return the student status value.
         *
         * @return String
         */
        public String getStatus() {
            return m_status;
        }

        /**
         * Returns the students withdrawal enrollment record if there is one.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getWithdrawal() {
            return m_withdrawalEnr;
        }
    }

    /*
     * FTE field alias constants.
     */
    protected static final String FTE_0010_DISTRICT = "System";
    protected static final String FTE_0020_SCHOOL_YEAR = "Fiscal Year";
    protected static final String FTE_0030_REPORT_CYCLE = "Report Period";
    protected static final String FTE_0050_SCHOOL_CODE = "School Code";
    protected static final String FTE_0060_GRADE_LEVEL = "Grade Level";
    protected static final String FTE_0070_PRIOR_TEN_DAYS = "Prior Ten Days";
    protected static final String FTE_0080_RESIDENT_STATUS_CODE = "Resident Status Code";
    protected static final String FTE_0090_SYSTEM_OF_RESIDENCY = "System of Residency";
    protected static final String FTE_0100_PROGRAM_CODE_1 = "Program Code 1";
    protected static final String FTE_0110_PROGRAM_CODE_2 = "Program Code 2";
    protected static final String FTE_0120_PROGRAM_CODE_3 = "Program Code 3";
    protected static final String FTE_0130_PROGRAM_CODE_4 = "Program Code 4";
    protected static final String FTE_0140_PROGRAM_CODE_5 = "Program Code 5";
    protected static final String FTE_0150_PROGRAM_CODE_6 = "Program Code 6";
    protected static final String FTE_0160_FILLER = "Filler3";
    protected static final String FTE_0170_WITHDRAWAL_DATE = "Withdrawal Date";
    protected static final String FTE_0180_LOCATION_1 = "Loc of Enroll 1";
    protected static final String FTE_0190_LOCATION_2 = "Loc of Enroll 2";
    protected static final String FTE_0200_LOCATION_3 = "Loc of Enroll 3";
    protected static final String FTE_0210_LOCATION_4 = "Loc of Enroll 4";
    protected static final String FTE_0220_LOCATION_5 = "Loc of Enroll 5";
    protected static final String FTE_0230_LOCATION_6 = "Loc of Enroll 6";
    protected static final String FTE_0240_FISCAL_AGENT_1 = "FTE024";
    protected static final String FTE_0250_FISCAL_AGENT_2 = "FTE025";
    protected static final String FTE_0260_FISCAL_AGENT_3 = "FTE026";
    protected static final String FTE_0270_FISCAL_AGENT_4 = "FTE027";
    protected static final String FTE_0280_FISCAL_AGENT_5 = "FTE028";
    protected static final String FTE_0290_FISCAL_AGENT_6 = "FTE029";
    protected static final String FTE_0300_ESOL_ITENERANT = "ESOL-Itinerant";
    protected static final String FTE_0310_ESOL_NON_ITENERANT = "FTE030 ESOL-Non-Itinerant";
    protected static final String FTE_0320_GENDER = "Gender";
    protected static final String FTE_0330_HISPANIC = "Hispanic";
    protected static final String FTE_0340_RACE_INDIAN = "Indian";
    protected static final String FTE_0350_RACE_ASIAN = "Asian";
    protected static final String FTE_0360_RACE_BLACK = "Black";
    protected static final String FTE_0370_RACE_PACIFIC = "Pacific";
    protected static final String FTE_0380_RACE_WHITE = "White";
    protected static final String FTE_0390_WITHDRAWAL_REASON = "Withdrawal Reason";
    protected static final String FTE_0400_DEPLOMA_TYPE = "Diploma Type";
    protected static final String FTE_0410_STUDENT_STATUS = "Student Status";
    protected static final String FTE_0420_SCHOOL_ENTRY_CODE = "School Entry Code";
    protected static final String FTE_0425_DATE_OF_ELP_SCREENER = "Date of ELP Screener";
    protected static final String FTE_0426_DATE_OF_ENTRY_TO_US_SCOOL = "Date of Entry to U.S. School";
    protected static final String FTE_0427_PLACE_OF_BIRTH = "Place of Birth";
    protected static final String FTE_0428_PRIMARY_LANGUAGE = "Primary Language";
    protected static final String FTE_0430_ENVIRONMENT = "Res Environment";
    protected static final String FTE_0440_REPORT_TYPE = "Report Type";
    protected static final String FTE_0450_DATE_OF_BIRTH = "Date of Birth";
    protected static final String FTE_0460_TRANSPORT_1 = "Transport Seg 1";
    protected static final String FTE_0470_TRANSPORT_2 = "Transport Seg 2";
    protected static final String FTE_0480_TRANSPORT_3 = "Transport Seg 3";
    protected static final String FTE_0490_TRANSPORT_4 = "Transport Seg 4";
    protected static final String FTE_0500_TRANSPORT_5 = "Transport Seg 5";
    protected static final String FTE_0510_TRANSPORT_6 = "Transport Seg 6";
    protected static final String FTE_0520_SPEECH_1 = "Supp Speech Seg 1";
    protected static final String FTE_0530_SPEECH_2 = "Supp Speech Seg 2";
    protected static final String FTE_0540_SPEECH_3 = "Supp Speech Seg 3";
    protected static final String FTE_0550_SPEECH_4 = "Supp Speech Seg 4";
    protected static final String FTE_0560_SPEECH_5 = "Supp Speech Seg 5";
    protected static final String FTE_0570_SPEECH_6 = "Supp Speech Seg 6";
    protected static final String FTE_0580_ITENERANT_1 = "Integrant Seg 1";
    protected static final String FTE_0590_ITENERANT_2 = "Integrant Seg 2";
    protected static final String FTE_0600_ITENERANT_3 = "Integrant Seg 3";
    protected static final String FTE_0610_ITENERANT_4 = "Integrant Seg 4";
    protected static final String FTE_0620_ITENERANT_5 = "Integrant Seg 5";
    protected static final String FTE_0630_ITENERANT_6 = "Integrant Seg 6";
    protected static final String FTE_0640_INCLUSION_1 = "Inclusion Seg 1";
    protected static final String FTE_0650_INCLUSION_2 = "Inclusion Seg 2";
    protected static final String FTE_0660_INCLUSION_3 = "Inclusion Seg 3";
    protected static final String FTE_0670_INCLUSION_4 = "Inclusion Seg 4";
    protected static final String FTE_0680_INCLUSION_5 = "Inclusion Seg 5";
    protected static final String FTE_0690_INCLUSION_6 = "Inclusion Seg 6";
    protected static final String FTE_0700_HOURS_P = "SE Service P";
    protected static final String FTE_0710_HOURS_Q = "SE Service Q";
    protected static final String FTE_0720_HOURS_R = "SE Service R";
    protected static final String FTE_0730_HOURS_S = "SE Service S";
    protected static final String FTE_0740_HOURS_T = "SE Service T";
    protected static final String FTE_0750_HOURS_U = "SE Service U";
    protected static final String FTE_0760_HOURS_V = "SE Service V";
    protected static final String FTE_0770_HOURS_W = "SE Service W";
    protected static final String FTE_0780_HOURS_X = "SE Service X";
    protected static final String FTE_0790_HOURS_Y = "SE Service Y";
    protected static final String FTE_0800_HOURS_Z = "SE Service Z";
    protected static final String FTE_0810_HOURS_1 = "SE Service 1";
    protected static final String FTE_0820_HOURS_2 = "SE Service 2";
    protected static final String FTE_0830_HOURS_3 = "SE Service 3";
    protected static final String FTE_0840_HOURS_6 = "SE Service 6";
    protected static final String FTE_0850_HOURS_7 = "SE Service 7";
    protected static final String FTE_0860_HOURS_8 = "SE Service 8";
    protected static final String FTE_0865_NON_ESOL = "Non-ESOL";
    protected static final String FTE_0870_PRIMARY_DISABILITY = "Primary Area";
    protected static final String FTE_0880_HOURS_A = "Related Ser A";
    protected static final String FTE_0890_HOURS_B = "Related Ser B";
    protected static final String FTE_0900_HOURS_C = "Related Ser C";
    protected static final String FTE_0910_HOURS_D = "Related Ser D";
    protected static final String FTE_0920_HOURS_E = "Related Ser E";
    protected static final String FTE_0930_HOURS_F = "Related Ser F";
    protected static final String FTE_0940_HOURS_G = "Related Ser G";
    protected static final String FTE_0950_HOURS_H = "Related Ser H";
    protected static final String FTE_0960_HOURS_I = "Related Ser I";
    protected static final String FTE_0970_HOURS_J = "Related Ser J";
    protected static final String FTE_0980_HOURS_K = "Related Ser K";
    protected static final String FTE_0990_HOURS_L = "Related Ser L";
    protected static final String FTE_1000_ALL_IEP = "All IEP";
    protected static final String FTE_1010_ELL = "EL";
    protected static final String FTE_1020_SPED_ENV = "SE Environment";
    protected static final String FTE_1030_GTID = "GTID";
    protected static final String FTE_1040_GIFTED_1 = "Gifted Del 1";
    protected static final String FTE_1050_GIFTED_2 = "Gifted Del 2";
    protected static final String FTE_1060_GIFTED_3 = "Gifted Del 3";
    protected static final String FTE_1070_GIFTED_4 = "Gifted Del 4";
    protected static final String FTE_1080_GIFTED_5 = "Gifted Del 5";
    protected static final String FTE_1090_GIFTED_6 = "Gifted Del 6";
    protected static final String FTE_1100_SPED_EXIT_CODE = "SE Event Code";
    protected static final String FTE_1110_SPED_EXIT_DATE = "SE Event Date";
    protected static final String FTE_1150_LAST_NAME = "Last Name";
    protected static final String FTE_1160_FIRST_NAME = "First Name";
    protected static final String FTE_1170_MIDDLE_NAME = "Middle Name";

    /*
     * Report values and aliases.
     */
    private static final String ALIAS_DOE_DATE_ENTRY_US = "DOE Date Entry US";
    private static final String ALIAS_DOE_ELL = "DOE ELL";
    private static final String ALIAS_DOE_EL_YEAR = "DOE YEAR EL MONITORING BEGAN";
    private static final String ALIAS_DOE_EXCLUDE_STD = "DOE EXCLUDE STD";
    private static final String ALIAS_DOE_RETAINED = "DOE Retained";
    private static final String ALIAS_DOE_PRIMARY_LANG = "DOE Primary Lang";
    private static final String ALIAS_DOE_PARENT_COMM_LANG = "DOE Parent Communication Lang";
    private static final String ALIAD_PLACE_OF_BIRTH = "Birth Place";
    private static final String ALIAS_STD_FEDERAL_CHILD_FIND = "all-std-FederalChildFind";
    private static final String ALIAS_STD_GAA_FLAG = "DOE GAA Flag";
    private static final String ALIAS_STD_SPED_ENV = "DOE SPED Environment";
    private static final String ALIAS_STD_SPED_TYPE = "DOE SPED Type";
    private static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    private static final String DOE_SCHOOL = "DOE School";
    private static final String DOE_SPED_EXIT_REASON = "DOE SPED EXIT REASON";

    /*
     * Miscellaneous Constants
     */
    private static final String CYCLE_MARCH = "3";
    private static final String CYCLE_OCTOBER = "1";
    private static final String DIPLOMA_TYPE_SPECIAL_EDUCATION = "S";
    private static final String PGM_OTHER_CODE = "O";
    private static final String REPORT_TYPE_REGULAR = "R";
    private static final String STATUS_NORMAL = "N";
    private static final String STATUS_RETAINED = "R";
    private static final String STATUS_WITHDRAWN = "W";
    private static final String STATUS_FEDERAL = "C";

    /*
     * Parameters
     */
    private static final String PARAM_CYCLE = "cycle";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_WITHDRAWN_AFTER_DATE = "withdrawnAfterDate";

    private static final String PARAM_WITHDRAWAL_DATE = "DATE";
    private static final String PARAM_WITHDRAWAL_REASON = "REASON";

    /*
     * Other internal constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

    /*
     * Instance variables.
     */
    protected PlainDate m_10DayCutoff;
    protected Map<String, Float> m_absences;
    protected String m_activeCode;
    protected String m_cycle;
    protected PlainDate m_currentYearStartDate;
    protected String m_doeElYearBeganField;
    protected String m_doeEllField;
    protected String m_doeExcludeField;
    protected PlainDate m_fteDate;
    protected Map<String, IepData> m_iepMap;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected String m_overrideSchoolCodeField;
    protected String m_parentCommLangField;
    protected String m_placeOfBirthField;
    protected String m_primaryLangField;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected String m_retainedField;
    protected Date m_startOfYear;
    protected Map<String, SchoolCalendarDate> m_schoolCalendarDatesMap;
    protected String m_schoolCodeField;
    protected String m_spedExitReasonField;
    protected String m_stdFieldFedChildFind;
    protected String m_stdFieldGAAFlag;
    protected String m_stdFieldSpedEnv;
    protected String m_stdFieldSpedType;
    protected GAFTEStudentHistoryHelper m_stdHelper;
    protected String m_studentDateEntryUSField;
    protected Map<String, ReferenceCode> m_withdrawalCodes;
    protected PlainDate m_withdrawnAfterDate;

    /**
     * The Class TNStudentHistoryHelper.
     */
    protected class GAFTEStudentHistoryHelper extends StudentHistoryHelper {

        /**
         * Instantiates a new GAFTE student history helper.
         *
         * @param data StateReportData
         */
        public GAFTEStudentHistoryHelper(StateReportData data) {
            super(data);
        }

        /**
         * Returns a criteria that finds enrollment records based on the student selection mode.
         *
         * @param applySchool - if true, the criteira will be limited to those in the current
         *        school; if
         *        false,
         *        students in active, non-archive schools will be included
         * @param beginDate PlainDate
         * @param endDate PlainDate
         * @param waDate PlainDate
         * @return X2Criteria
         */
        @Override
        protected X2Criteria buildStudentCriteriaForEnrollments(Boolean applySchool,
                                                                PlainDate beginDate,
                                                                PlainDate endDate,
                                                                PlainDate waDate) {
            // Build the subquery for student enrollments within the current year.
            X2Criteria enrollmentCriteria = new X2Criteria();
            // Select enrollment dates based on date, depending on mode.
            if (MODE_STUDENT_ACTIVE_ANY_TIME.equals(getStudentSelectionMode())) {
                if (waDate != null) {
                    enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, waDate);
                } else {
                    enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, beginDate);
                }
            }
            if (MODE_STUDENT_ACTIVE_SNAPSHOT.equals(getStudentSelectionMode())) {
                if (waDate != null) {
                    enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, waDate);
                } else {
                    enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
                }
            }
            // Apply school selection criteria.
            if (applySchool.booleanValue() && getData().isSchoolContext()) {
                enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getData().getSchool().getOid());
            } else {
                enrollmentCriteria.addNotEqualTo(
                        StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }
            return enrollmentCriteria;
        }
    }

    /**
     * Return the ALL IEP value for the student. This value is blank if the Student Status is "W"
     * (withdrawn)
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAllIep implements FieldRetriever {

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
            String studentStatus = ((FteEntity) entity).getStatus();

            if (STATUS_WITHDRAWN.equals(studentStatus)) {
                value = null;
            }
            return value;
        }
    }

    /**
     * Retrieve the specified field value.
     * If the student is reporting as withdrawn, return nothing.
     * Valid values Y, N or "Blank".Return blank when Student Status (Export position 410) = W
     * Return Y or N when Report Type = S (Export position 440)
     * Return ""Blank"" when Report Type = N"
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveClearIfWithdrawn implements FieldRetriever {
        private static final String PARAM_GAA_FLAG = "GAAFLAG";

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
            Student student = (Student) entity.getBean();
            if (StringUtils.isEqual((String) student.getFieldValueByBeanPath(m_stdFieldFedChildFind),
                    BooleanAsStringConverter.TRUE)) {
                if (PARAM_GAA_FLAG.equals(field.getParameter())) {
                    return "";
                }
                return "N";
            }

            String value = (String) getProperty(student, field.getBeanPath());
            if ((STATUS_WITHDRAWN.equals(entity.getFieldValue(FTE_0410_STUDENT_STATUS)))
                    || (REPORT_TYPE_REGULAR.equals(entity.getFieldValue(FTE_0440_REPORT_TYPE)))) {
                value = "";
            } else {
                if (StringUtils.isEmpty(value)) {
                    value = "0";
                }
            }
            return value;
        }
    }

    /**
     * Return the student ELL status on report date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELL implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String ellCode = (String) student.getFieldValueByBeanPath(m_doeEllField);
            if (!StringUtils.isEmpty(ellCode)) {
                String ellCodeState = lookupStateValue(SisStudent.class, m_doeEllField, ellCode);
                if (!StringUtils.isEmpty(ellCodeState)) {
                    Integer ellYearBegan = null;
                    Integer currYear = Integer.valueOf(getCurrentContext().getSchoolYear());
                    String doeElYearBegan = (String) student.getFieldValueByBeanPath(m_doeElYearBeganField);
                    if (doeElYearBegan != null && doeElYearBegan.length() > 1) {
                        try {
                            ellYearBegan =
                                    Integer.valueOf((String) student.getFieldValueByBeanPath(m_doeElYearBeganField));
                        } catch (NumberFormatException e) {
                            // nothing on formatting error;
                        }
                    }
                    if (!"M".equals(ellCodeState)) {
                        value = ellCodeState;
                    } else if (ellYearBegan != null) {
                        int diff = currYear.intValue() - ellYearBegan.intValue();
                        switch (diff) {
                            case 0:
                                value = "1";
                                break;
                            case 1:
                                value = "2";
                                break;
                            case 2:
                                value = "3";
                                break;
                            case 3:
                                value = "4";
                                break;
                            default:
                                break;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveGAA.
     */
    protected class RetrieveGAA implements FieldRetriever {

        private static final String CALC_ID = "FTE-GAA";

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String stdStatus = entity.getFieldValue(FTE_0410_STUDENT_STATUS);
            String reportType = entity.getFieldValue(FTE_0440_REPORT_TYPE);
            if (!StringUtils.isEmpty(stdStatus)
                    && (STATUS_NORMAL.equals(stdStatus) || STATUS_RETAINED.equals(stdStatus))
                    && !StringUtils.isEmpty(reportType)
                    && DIPLOMA_TYPE_SPECIAL_EDUCATION.equals(reportType)) {
                value = lookupStateValue(SisStudent.class, m_stdFieldGAAFlag,
                        (String) student.getFieldValueByBeanPath(m_stdFieldGAAFlag));
            }
            return value;
        }
    }

    /**
     * Retrieve the specified field value.
     * If the student is reporting as withdrawn, return grade on withdrawal date.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveGrade implements FieldRetriever {
        private Collection<DistrictSchoolYearContext> m_contexts;
        private Map<String, ReferenceCode> m_referenceGradeCodeMap;

        /**
         * Instantiates a new retrieve grade.
         */
        public RetrieveGrade() {
            m_referenceGradeCodeMap = loadRefCodeMapByField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String gradeCode = (String) getProperty(entity.getBean(), field.getBeanPath());
            String value = m_referenceGradeCodeMap.get(gradeCode) != null
                    ? m_referenceGradeCodeMap.get(gradeCode).getStateCode()
                    : null;

            FteEntity fteEntity = (FteEntity) entity;

            if (STATUS_WITHDRAWN.equals(((FteEntity) entity).getStatus())) {
                StudentEnrollment enrollment = fteEntity.getWithdrawal();
                PlainDate date = enrollment.getEnrollmentDate();
                Calendar cal = Calendar.getInstance();
                cal.setTime(date);
                if (Calendar.JUNE == cal.get(Calendar.MONTH) && 16 == cal.get(Calendar.DAY_OF_MONTH)) {
                    value = gradeCode;
                } else {
                    int yog = enrollment.getYog();
                    if (yog == 0) {
                        yog = ((SisStudent) entity.getBean()).getYog();
                    }
                    value = getGradeLevel(yog, getSchoolYear(date)).getStateCode();
                }
            }
            return value;
        }

        /**
         * Returns grade level based on yog and schoolYear.
         *
         * @param yog int
         * @param schoolYear int
         * @return ReferenceCode
         */
        private ReferenceCode getGradeLevel(int yog, int schoolYear) {
            ReferenceCode gradeCode = null;

            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels =
                    StudentManager.getMatchingGradeLevels(maxGradeLevel, yog, schoolYear, sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
            return gradeCode;
        }

        /**
         * Returns school year based on date.
         *
         * @param date PlainDate
         * @return int
         */
        private int getSchoolYear(PlainDate date) {
            int schoolYear = 0;
            if (m_contexts == null) {
                m_contexts = loadContexts();
            }
            for (DistrictSchoolYearContext context : m_contexts) {
                if (!date.before(context.getStartDate()) && !date.after(context.getEndDate())) {
                    schoolYear = context.getSchoolYear();
                    break;
                }
            }
            return schoolYear;
        }

        /**
         * Load reference code map by field name.
         *
         * @param beanClass Class
         * @param fieldName String
         * @return Map<String, ReferenceCode>
         */
        private Map<String, ReferenceCode> loadRefCodeMapByField(Class beanClass, String fieldName) {
            Map<String, ReferenceCode> refCodeMap = null;
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            ModelProperty prop = new ModelProperty(beanClass, fieldName, getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
            if (field != null) {
                ReferenceTable referenceTable = field.getReferenceTable();
                refCodeMap = referenceTable.getCodeMap();
            }
            return refCodeMap;
        }

        /**
         * Load contexts to determine school year for grade calculation.
         * Contexts are restricted by selected date range.
         *
         * @return Collection
         */
        private Collection<DistrictSchoolYearContext> loadContexts() {
            X2Criteria ctxsCriteria = new X2Criteria();
            ctxsCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_fteDate);
            if (m_withdrawnAfterDate != null) {
                ctxsCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_withdrawnAfterDate);
            }
            QueryByCriteria ctxsQuery = new QueryByCriteria(DistrictSchoolYearContext.class, ctxsCriteria);

            return getBroker().getCollectionByQuery(ctxsQuery);
        }
    }

    /**
     * Return the students district of residence.
     * The field may be populated for any student.
     * The report only wants this value if it is not the same as FTE001 reporting district.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveNotFTE001 implements FieldRetriever {

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
            Student student = (Student) entity.getBean();
            if (StringUtils.isEqual((String) student.getFieldValueByBeanPath(m_stdFieldFedChildFind),
                    BooleanAsStringConverter.TRUE)) {
                return student.getOrganization1().getFieldValueByAlias("DOE District");
            }

            String value = (String) getProperty(student, field.getBeanPath());
            String fte0010 = entity.getFieldValue(FTE_0010_DISTRICT);
            if (fte0010.equals(value)) {
                value = null;
            }
            return value;
        }
    }

    /**
     * Retrieve the specified field value.
     * If populated, return state code. If blank/null, reference Student.DOE Primary Lang and return
     * state code..
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveParentCommLang implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            String primaryLanguage = (String) student.getFieldValueByBeanPath(m_primaryLangField);
            if (!StringUtils.isEmpty(primaryLanguage)) {
                primaryLanguage = lookupStateValue(SisStudent.class, m_primaryLangField, primaryLanguage);
            }
            if (StringUtils.isEmpty(value)) {
                value = primaryLanguage;
            } else {
                value = lookupStateValue(SisStudent.class, m_parentCommLangField, value);
            }
            if (!StringUtils.isEmpty(value)) {
                value = StringUtils.padLeft(value, 3, '0');
            }
            return StringUtils.isEmpty(value) ? "008" : value;
        }
    }

    /**
     * Retrieve the requested value or an alternate value if the first one is empty.
     * Use the bean path in the field definition to retrieve a value.
     * If the value is empty, use the calculation parameter as an alias and
     * look up an alternate field and its value.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePgmCode implements FieldRetriever {

        private static final String ALIAS_PGM_ORIG = "DOE Orig Prog Code";
        private static final String ALIAS_PGM_FTE = "DOE Program Code";

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
            String codeNumber = (String) field.getParameter();
            SisStudent std = (SisStudent) entity.getBean();
            String value = null;

            if (StringUtils.isEqual((String) std.getFieldValueByBeanPath(m_stdFieldFedChildFind),
                    BooleanAsStringConverter.TRUE)) {
                return PGM_OTHER_CODE;
            }

            String beanPathDoePgmCode = data.translateAliasToJavaName(ALIAS_PGM_FTE + " " + codeNumber, false);
            if (!StringUtils.isEmpty(beanPathDoePgmCode)) {
                value = data.lookupStateValue(SisStudent.class, beanPathDoePgmCode,
                        (String) std.getFieldValueByBeanPath(beanPathDoePgmCode));
            }
            if (StringUtils.isEmpty(value)) {
                String beanPathOrigPgmCode = data.translateAliasToJavaName(ALIAS_PGM_ORIG + " " + codeNumber, false);
                if (!StringUtils.isEmpty(beanPathOrigPgmCode)) {
                    value = data.lookupStateValue(SisStudent.class, beanPathOrigPgmCode,
                            (String) std.getFieldValueByBeanPath(beanPathOrigPgmCode));
                }

            }
            if (StringUtils.isEmpty(value) && (STATUS_NORMAL.equals(((FteEntity) entity).getStatus())
                    || STATUS_RETAINED.equals(((FteEntity) entity).getStatus()))) {
                value = PGM_OTHER_CODE;
            }
            return value;
        }
    }

    /**
     * Retrieve the specified field value.
     * Pad with leading zeros to create a 3 digit value.
     * When Place of Birth (Export position 428) = 2310 OR 1790 AND DOE Primary Lang = '08' (state
     * code value in reference table) primary Language shall be blank.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrievePrimaryLang implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            Person person = student.getPerson();
            String primaryLanguage = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (!StringUtils.isEmpty(primaryLanguage)) {
                primaryLanguage = lookupStateValue(SisStudent.class, m_primaryLangField, primaryLanguage);
            }
            String placeOfBirth = (String) person.getFieldValueByBeanPath(m_placeOfBirthField);
            if (!StringUtils.isEmpty(placeOfBirth)) {
                placeOfBirth = lookupStateValue(Person.class, m_placeOfBirthField, placeOfBirth);
            }
            if (!StringUtils.isEmpty(primaryLanguage)) {
                primaryLanguage = StringUtils.padLeft(primaryLanguage, 3, '0');
            }
            return StringUtils.isEmpty(primaryLanguage) ? "008" : primaryLanguage;
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
     * Return the students Resident Status Code.
     * The field may be populated for any student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveResidencyCode implements FieldRetriever {

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
            Student student = (Student) entity.getBean();
            if (StringUtils.isEqual((String) student.getFieldValueByBeanPath(m_stdFieldFedChildFind),
                    BooleanAsStringConverter.TRUE)) {
                return "1";
            }

            return getProperty(student, field.getBeanPath());
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
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            String overrideSchoolCodeField = ((Fte) data).m_overrideSchoolCodeField;
            String schoolCodeField = ((Fte) data).m_schoolCodeField;
            value = (String) getProperty(student, overrideSchoolCodeField);
            if (StringUtils.isEmpty(value)) {
                SisSchool school = ((FteEntity) entity).getSchool();
                if (school != null && schoolCodeField != null) {
                    value = (String) getProperty(school, schoolCodeField);
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveSEEnvironment.
     */
    protected class RetrieveSEEnvironment implements FieldRetriever {

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
            Student student = (Student) entity.getBean();
            String value = null;
            String code = (String) student.getFieldValueByBeanPath(m_stdFieldSpedEnv);
            if (!StringUtils.isEmpty(code)) {
                value = data.lookupStateValue(SisStudent.class, m_stdFieldSpedEnv, code);
            }
            if ((STATUS_WITHDRAWN.equals(entity.getFieldValue(FTE_0410_STUDENT_STATUS)))
                    || (REPORT_TYPE_REGULAR.equals(entity.getFieldValue(FTE_0440_REPORT_TYPE)))) {
                value = "";
            }
            return value;
        }
    }

    /**
     * Retrieve the Sped Exit date and exit reason code.
     * Only display either when:
     * A. The exit date is within the prior school year or in this
     * school year prior to report date.
     * and
     * B. Exit reason state code is "09" or "10" or "13".
     *
     * Otherwise hide both.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSpedExit implements FieldRetriever {
        private static final String PARAM_CODE = "CODE";
        private static final String PARAM_DATE = "DATE";

        private static final String WITDRAWAL_DATE_PATTERN = "MMddyy";

        private SimpleDateFormat m_format = new SimpleDateFormat(WITDRAWAL_DATE_PATTERN);
        private Set<String> m_exitReasonCodes;

        /**
         * Instantiates a new retrieve sped exit.
         */
        protected RetrieveSpedExit() {
            m_exitReasonCodes = new HashSet<String>();
            m_exitReasonCodes.add("09");
            m_exitReasonCodes.add("10");
            m_exitReasonCodes.add("13");
        }

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            PlainDate exitDate = student.getSpedExitDate();
            String withdrawalDateString = entity.getFieldValue(FTE_0170_WITHDRAWAL_DATE);
            PlainDate withdrawalDate = null;
            if (!StringUtils.isEmpty(withdrawalDateString)) {
                try {
                    withdrawalDate = new PlainDate(m_format.parse(withdrawalDateString));
                } catch (ParseException e) {
                    // withdrawal date remains null
                }
            }
            String exitReason = (String) student.getFieldValueByBeanPath(m_spedExitReasonField);
            String exitReasonCode = lookupStateValue(SisStudent.class, m_spedExitReasonField, exitReason);
            if (BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_stdFieldFedChildFind))) {
                if (PARAM_CODE.equals(param)) {
                    value = exitReason;
                } else if (PARAM_DATE.equals(param)) {
                    value = exitDate;
                }
            } else {
                if (!CYCLE_MARCH.equals(m_cycle)) {
                    if (m_exitReasonCodes.contains(exitReasonCode) && exitDate != null &&
                            !exitDate.after(m_fteDate) &&
                            !exitDate.before(m_currentYearStartDate)
                            && (withdrawalDate == null || exitDate.before(withdrawalDate))) {
                        if (PARAM_CODE.equals(param)) {
                            value = exitReason;
                        } else if (PARAM_DATE.equals(param)) {
                            value = exitDate;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * The Class RetrieveSpedPrimaryArea.
     */
    protected class RetrieveSpedPrimaryArea implements FieldRetriever {
        private static final String ALIAS_PRIMARY_AREA = "DOE Primary Exceptionality";

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
            Object value = null;
            SisStudent student = (SisStudent) entity.getBean();
            if (BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_stdFieldFedChildFind))) {
                value = getPrimaryArea(student);
            } else {
                if (REPORT_TYPE_REGULAR.equals(entity.getFieldValue(FTE_0440_REPORT_TYPE))) {
                    if (!StringUtils.isEmpty(entity.getFieldValue(FTE_1100_SPED_EXIT_CODE))
                            || DIPLOMA_TYPE_SPECIAL_EDUCATION.equals(entity.getFieldValue(FTE_0400_DEPLOMA_TYPE))) {
                        value = getPrimaryArea(student);
                    } else if (CYCLE_MARCH.equals(m_cycle)) {
                        if (student.getSpedExitDate() != null && m_startOfYear != null
                                && !student.getSpedExitDate().before(m_startOfYear)) {
                            String exitReason = (String) student.getFieldValueByBeanPath(m_spedExitReasonField);
                            String exitReasonCode =
                                    lookupStateValue(SisStudent.class, m_spedExitReasonField, exitReason);
                            if ("09".equals(exitReasonCode) || "10".equals(exitReasonCode)
                                    || "13".equals(exitReasonCode)) {
                                value = getPrimaryArea(student);
                            }
                        }
                    }
                } else {
                    value = getPrimaryArea(student);
                }
            }
            return value;
        }

        /**
         * Gets the primary area.
         *
         * @param student SisStudent
         * @return String
         */
        private String getPrimaryArea(SisStudent student) {
            String value = null;
            String code = (String) student.getFieldValueByAlias(ALIAS_PRIMARY_AREA);
            if (!StringUtils.isEmpty(code)) {
                value = lookupReferenceCodeByAlias(ALIAS_PRIMARY_AREA, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * Retrieve the Sped type code.
     * By default, use student sped status with reference code.
     *
     * Normally, the reference code for state value of student
     * special ed status Exited is 'S' for special ed.
     * However, if special ed. exit reason state code is 09 or 10, then
     * override and make sped type = empty so it will produce an 'R'.
     *
     * The Primary Exceptionality field also has reliance on the sped type code for validations.
     * This also returns the Primary Exceptionality for the student. This value is blank during
     * the October reporting period if a student is Exited from sped during the date range of
     * the last fiscal year unless they are a withdrawn student with a sped type of 'S'.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSpedType implements FieldRetriever {
        private static final String ALIAS_DIPLOMA_TYPE = "DOE Diploma Type";

        private String m_diplomaBeanPath;
        private Set<String> m_exitReasonCodes;

        /**
         * Instantiates a new retrieve sped type.
         */
        public RetrieveSpedType() {
            m_exitReasonCodes = new HashSet<String>();
            m_exitReasonCodes.add("09");
            m_exitReasonCodes.add("10");
            m_diplomaBeanPath = translateAliasToJavaName(ALIAS_DIPLOMA_TYPE, true);

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = REPORT_TYPE_REGULAR;
            if (StringUtils.isEmpty(entity.getFieldValue(FTE_1110_SPED_EXIT_DATE))) {
                SisStudent std = (SisStudent) entity.getBean();
                String spedActiveCode = (String) std.getFieldValueByBeanPath(m_stdFieldSpedType);
                String spedActiveCodeState = null;
                if (!StringUtils.isEmpty(spedActiveCode)) {
                    spedActiveCodeState = lookupStateValue(SisStudent.class, m_stdFieldSpedType, spedActiveCode);
                }
                FteEntity fteEntity = (FteEntity) entity;
                IepData iep = fteEntity.getIep();
                SisStudent student = (SisStudent) entity.getBean();
                // Sped status should be dependent on the existence of an IEP on the report date.
                // The sped status field is not sufficiently date sensitive.
                if (iep != null) {
                    value = spedActiveCodeState; // An IEP indicates the student was active at
                                                 // the time.
                } else {
                    String exitReason = (String) student.getFieldValueByBeanPath(m_spedExitReasonField);
                    String exitReasonCode = lookupStateValue(SisStudent.class, m_spedExitReasonField, exitReason);
                    SpedStatusCode spedStatus = student.getSpedStatusCodeEnum();
                    PlainDate exitDate = student.getSpedExitDate();
                    String diploma = (String) student.getFieldValueByBeanPath(m_diplomaBeanPath);
                    diploma = lookupStateValue(SisStudent.class, m_diplomaBeanPath, diploma);
                    // Graduated sped students
                    if (exitDate != null &&
                            !exitDate.after(m_fteDate) &&
                            (m_cycle.equals(CYCLE_MARCH) || !exitDate.before(m_withdrawnAfterDate)) &&
                            SpedStatusCode.EXITED.equals(spedStatus)) {
                        if (StringUtils.isEmpty(exitReasonCode)) {
                            value = DIPLOMA_TYPE_SPECIAL_EDUCATION;
                        } else if (m_exitReasonCodes.contains(exitReasonCode)) {
                            value = spedActiveCodeState;
                        }
                        // Active without IEP
                    } else if (exitDate == null
                            && !SpedStatusCode.INELIGIBLE.equals(spedStatus)
                            && !SpedStatusCode.NOT_SPED.equals(spedStatus)) {
                        value = spedActiveCodeState; // Eligible status attained before
                                                     // report date.
                    }
                }
                if (DIPLOMA_TYPE_SPECIAL_EDUCATION.equals(entity.getFieldValue(FTE_0400_DEPLOMA_TYPE))
                        || BooleanAsStringConverter.TRUE
                                .equals(std.getFieldValueByBeanPath(m_stdFieldFedChildFind))) {
                    value = DIPLOMA_TYPE_SPECIAL_EDUCATION;
                }
            }
            return value;
        }
    }

    /**
     * Return the student status on report date.
     * This is calculated in the Entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStatus implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            Object value = null;

            Student student = (Student) entity.getBean();
            if (StringUtils.isEqual((String) student.getFieldValueByBeanPath(m_stdFieldFedChildFind),
                    BooleanAsStringConverter.TRUE)) {
                return STATUS_FEDERAL;
            }

            if (CYCLE_MARCH.equals(m_cycle)) {
                value = STATUS_NORMAL;
            } else if (CYCLE_OCTOBER.equals(m_cycle)) {
                String retained = null;
                retained = (String) student.getFieldValueByBeanPath(m_retainedField);
                value = ((FteEntity) entity).getStatus();
                if (BooleanAsStringConverter.TRUE.equals(retained) && STATUS_NORMAL.equals(value)) {
                    value = STATUS_RETAINED;
                }
            }
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
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
     * Retrieve the specified field value.
     * Calculate based on input parameter "Report Date" and student.DOE Date Entry US. If < 3
     * years,
     * return "Y", otherwise, return "N".
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveStudentUSSchoolDate implements FieldRetriever {

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
            String DATE_ENTRY_SCHOOL_DATE_PATTERN = "yyyy-MM-dd";
            SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_ENTRY_SCHOOL_DATE_PATTERN);

            String value;
            SisStudent student = (SisStudent) entity.getBean();
            value = (String) student.getFieldValueByBeanPath(m_studentDateEntryUSField);

            if (!StringUtils.isEmpty(value)) {
                try {
                    Calendar calendar = Calendar.getInstance();
                    PlainDate entryDate = new PlainDate(dateFormat.parse(value.toString()));

                    calendar.setTime(entryDate);
                    calendar.add(Calendar.YEAR, 3);

                    if (calendar.getTime().after(m_fteDate)) {
                        value = "1";
                    } else {
                        value = "0";
                    }

                    return value;

                } catch (ParseException e) {
                    // Invalid Date format
                    e.printStackTrace();
                }
            }

            return null;
        }
    }

    /**
     * Return an indicator of whether the student has been in attendance
     * within the last ten days before the report date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTenDays implements FieldRetriever {

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
            Object value = Boolean.TRUE;

            Student student = (Student) entity.getBean();
            if (StringUtils.isEqual((String) student.getFieldValueByBeanPath(m_stdFieldFedChildFind),
                    BooleanAsStringConverter.TRUE)) {
                return Boolean.FALSE;
            }

            StudentEnrollment enrollment = ((FteEntity) entity).getWithdrawal();
            if (enrollment != null) {
                PlainDate withdrawalDate = enrollment.getEnrollmentDate();
                if (m_10DayCutoff.after(withdrawalDate)) {
                    value = Boolean.FALSE;
                }
            }

            if (STATUS_NORMAL.equals(((FteEntity) entity).getStatus())) {
                Float absences = ((Fte) data).m_absences.get(student.getOid());
                if (absences != null && absences.floatValue() >= 10.0) {
                    value = Boolean.FALSE;
                }
            }

            return value;
        }
    }

    /**
     * Retrieve the student withdrawal record and return values from it.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveWithdrawal implements FieldRetriever {

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
            Object value = null;
            Fte fteData = (Fte) data;
            StudentEnrollment enrollment = ((FteEntity) entity).getWithdrawal();
            if (enrollment != null) {
                String param = (String) field.getParameter();
                if (PARAM_WITHDRAWAL_DATE.equals(param)) {
                    value = enrollment.getEnrollmentDate();
                } else if (PARAM_WITHDRAWAL_REASON.equals(param)) {
                    String reason = enrollment.getEnrollmentCode();
                    if (fteData.m_withdrawalCodes != null) {
                        LinkedHashMap<String, ReferenceCode> withdrawalCodes =
                                (LinkedHashMap<String, ReferenceCode>) fteData.m_withdrawalCodes;
                        if (withdrawalCodes.containsKey(reason)) {
                            ReferenceCode refCode = withdrawalCodes.get(reason);
                            if (refCode != null) {
                                value = refCode.getStateCode();
                            }
                        }
                    }
                }
            }

            return value;
        }
    }

    /**
     * Retrieve the school.
     * Return only the last two digits: For 2010-2011 return 11.
     *
     * The bean path for the school year comes from the field bean path
     * which should point to the appropriate district school year context school year.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveYear implements FieldRetriever {

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
            String value = null;
            int year = ((Integer) getProperty(entity.getBean(), field.getBeanPath())).intValue();
            year = year % 100;
            value = Integer.toString(year);
            return value;
        }
    }

    /**
     * The Class ValidateHours.
     */
    protected class ValidateHours implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String fte0440 = entity.getFieldValue(FTE_0440_REPORT_TYPE);
            String fte1000 = entity.getFieldValue(FTE_1000_ALL_IEP);
            String fte1020 = entity.getFieldValue(FTE_1020_SPED_ENV);
            if (REPORT_TYPE_REGULAR.equals(fte0440) && !StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If report type FTE044 is " + STYLE_BOLD + REPORT_TYPE_REGULAR + STYLE_END
                                + " then hours must be blank",
                        "FTE044=" + STYLE_BOLD + fte0440 + STYLE_END + ", " + field.getFieldId() + "=" + STYLE_BOLD
                                + value + STYLE_END));
            }
            if ("N".equals(fte1000) && "0".equals(fte1020) && !StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If All IEP FTE100 is " + STYLE_BOLD + "N" + STYLE_END + " and Sped Environment FTE102 is "
                                + STYLE_BOLD + "0" + STYLE_END + " then hours must be blank",
                        "FTE100=" + STYLE_BOLD + fte1000 + STYLE_END + ", FTE102=" + STYLE_BOLD + fte1020
                                + STYLE_END
                                + ", " + field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END));
            }
            if (!"0".equals(fte1020) && (StringUtils.isEmpty(value) || "N".equals(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Sped Environment FTE102 is not " + STYLE_BOLD + "0" + STYLE_END
                                + " then hours must not be blank or " + STYLE_BOLD + "N" + STYLE_END,
                        "FTE102=" + STYLE_BOLD + fte1020 + STYLE_END + ", " + field.getFieldId() + "=" + STYLE_BOLD
                                + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate program codes and combinations of program codes.
     *
     * @author X2 Development Corporation
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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String FTE044 = entity.getFieldValue(FTE_0440_REPORT_TYPE);
            String FTE006 = entity.getFieldValue(FTE_0060_GRADE_LEVEL);
            if ("PQRSTUVWXYZ1234".contains(value) && !"S".equals(FTE044)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "P-Z,1-4" + STYLE_END
                                + " requires report type FTE044 to be "
                                + STYLE_BOLD + "S" + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE044=" + STYLE_BOLD
                                + FTE044
                                + STYLE_END));
            }

            // Validate grade levels.
            if ("BCDFGHJKMN59".contains(value)
                    && ("PK".equals(FTE006) || "UK".equals(FTE006) || "KK".equals(FTE006))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "B,C,D,F,G,H,J,K,M,N,9,5" + STYLE_END
                                + " cannot be used for grade level FTE006 of" + STYLE_BOLD + "PK,UK,KK" + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }
            if ("AE".contains(value) && ("PK".equals(FTE006) || "UK".equals(FTE006))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "A,E" + STYLE_END
                                + " cannot be used for grade level FTE006 of"
                                + STYLE_BOLD + "PK,UK" + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }
            if ("EFG".contains(value) && "U1".equals(FTE006)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "E,F,G" + STYLE_END
                                + " cannot be used for grade level FTE006 of" + STYLE_BOLD + "U1" + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }
            if ("AC9DEGHJKMN5".contains(value)
                    && ("U1".equals(FTE006) || "01".equals(FTE006) || "02".equals(FTE006) || "03".equals(FTE006))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "A,C,D,E,G,H,J,K,M,N,9,5" + STYLE_END
                                + " cannot be used for grade level FTE006 of" + STYLE_BOLD + "U1,01,02,03"
                                + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }
            if ("ABDEFHJKMN95".contains(value) && ("04".equals(FTE006) || "05".equals(FTE006))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "A,B,D,E,F,H,J,K,M,N,9,5" + STYLE_END
                                + " cannot be used for grade level FTE006 of" + STYLE_BOLD + "04,05" + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }
            if ("ABCDEFGKMN".contains(value)
                    && ("06".equals(FTE006) || "07".equals(FTE006) || "08".equals(FTE006))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "A,B,C,D,E,F,G,K,M,N" + STYLE_END
                                + " cannot be used for grade level FTE006 of" + STYLE_BOLD + "06,07,08" + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }
            if ("ABCEFGH9".contains(value)
                    && ("09".equals(FTE006) || "10".equals(FTE006) || "11".equals(FTE006) || "12".equals(FTE006))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Program codes " + STYLE_BOLD + "A,B,C,E,F,G,H,9" + STYLE_END
                                + " cannot be used for grade level FTE006 of" + STYLE_BOLD + "09,10,11,12"
                                + STYLE_END,
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", FTE006=" + STYLE_BOLD
                                + FTE006
                                + STYLE_END));
            }

            // Validate combinations. Only do these when we are on the first program code.
            if (FTE_0100_PROGRAM_CODE_1.equals(field.getFieldId())) {
                String AllCodes = value + entity.getFieldValue(FTE_0110_PROGRAM_CODE_2)
                        + entity.getFieldValue(FTE_0120_PROGRAM_CODE_3)
                        + entity.getFieldValue(FTE_0130_PROGRAM_CODE_4)
                        + entity.getFieldValue(FTE_0140_PROGRAM_CODE_5)
                        + entity.getFieldValue(FTE_0150_PROGRAM_CODE_6);
                if (AllCodes.contains("J")
                        && (AllCodes.contains("R") || AllCodes.contains("S") || AllCodes.contains("2"))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Program code " + STYLE_BOLD + "J" + STYLE_END + " is not usually used with codes "
                                    + STYLE_BOLD + "R,S or 2" + STYLE_END,
                            "All Codes = " + STYLE_BOLD + AllCodes + STYLE_END));
                }
                if (AllCodes.contains("W") && AllCodes.contains("X")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Program code " + STYLE_BOLD + "W" + STYLE_END + " must not be used with " + STYLE_BOLD
                                    + "X" + STYLE_END,
                            "All Codes = " + STYLE_BOLD + AllCodes + STYLE_END));
                }
                if (AllCodes.contains("Z") && AllCodes.contains("1")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Program code " + STYLE_BOLD + "Z" + STYLE_END + " must not be used with " + STYLE_BOLD
                                    + "1" + STYLE_END,
                            "All Codes = " + STYLE_BOLD + AllCodes + STYLE_END));
                }
                if (AllCodes.contains("U") && (AllCodes.contains("Z") || AllCodes.contains("W")
                        || AllCodes.contains("X") || AllCodes.contains("1"))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Program code " + STYLE_BOLD + "U" + STYLE_END + " should not be used with codes "
                                    + STYLE_BOLD + "Z,W,X,1" + STYLE_END,
                            "All Codes = " + STYLE_BOLD + AllCodes + STYLE_END));
                }

            }

            return errors;
        }
    }

    /**
     * Validate residency status and system.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateResidentStatus implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String FTE001 = entity.getFieldValue(FTE_0010_DISTRICT);
            String FTE009 = entity.getFieldValue(FTE_0090_SYSTEM_OF_RESIDENCY);
            String FTE041 = entity.getFieldValue(FTE_0410_STUDENT_STATUS);
            String FTE042 = entity.getFieldValue(FTE_0420_SCHOOL_ENTRY_CODE);
            if ("1".equals(value) || "7".equals(value)) {
                if (!StringUtils.isEmpty(FTE009) && !FTE001.equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Residency Status FTE008 of " + STYLE_BOLD + "1,7" + STYLE_END
                                    + " requires system of residency FTE009 empty or local.",
                            "FTE008=" + STYLE_BOLD + value + STYLE_END + ", FTE009=" + STYLE_BOLD + FTE009
                                    + STYLE_END));
                }
            }
            if ("2".equals(value) || "3".equals(value) || "4".equals(value) || "6".equals(value)) {
                if (StringUtils.isEmpty(FTE009) || FTE001.equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Residency Status FTE008 of " + STYLE_BOLD + "2,3,4,6" + STYLE_END
                                    + " requires system of residency FTE009 different than local.",
                            "FTE008=" + STYLE_BOLD + value + STYLE_END + ", FTE009=" + STYLE_BOLD + FTE009
                                    + STYLE_END
                                    + ", FTE001=" + STYLE_BOLD + FTE001 + STYLE_END));
                }
            }
            if ("5".equals(value)) {
                if (!"800".equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Residency Status FTE008 of " + STYLE_BOLD + "5" + STYLE_END
                                    + " requires system of residency FTE009 to be " + STYLE_BOLD + "800"
                                    + STYLE_END,
                            "FTE008=" + STYLE_BOLD + value + STYLE_END + ", FTE009=" + STYLE_BOLD + FTE009
                                    + STYLE_END));
                }
            }
            if ("7".equals(value) && "R".equals(FTE041)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Residency Status of " + STYLE_BOLD + "7" + STYLE_END
                                + " cannot have a student status FTE041 of " + STYLE_BOLD + "R" + STYLE_END,
                        "FTE008=" + STYLE_BOLD + value + STYLE_END + ", FTE041=" + STYLE_BOLD + FTE041
                                + STYLE_END));
            }
            if ("7".equals(value) && !"O".equals(FTE042)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Residency Status of " + STYLE_BOLD + "7" + STYLE_END
                                + " must have a school entry code FTE042 of " + STYLE_BOLD + "O" + STYLE_END,
                        "FTE008=" + STYLE_BOLD + value + STYLE_END + ", FTE042=" + STYLE_BOLD + FTE042
                                + STYLE_END));
            }
            return errors;
        }

    }

    /**
     * The Class ValidateStudentStatus.
     */
    protected class ValidateStudentStatus implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String FTE017 = entity.getFieldValue(FTE_0170_WITHDRAWAL_DATE);
            String FTE039 = entity.getFieldValue(FTE_0390_WITHDRAWAL_REASON);
            String FTE003 = entity.getFieldValue(FTE_0030_REPORT_CYCLE);
            if (STATUS_WITHDRAWN.equals(value) && (StringUtils.isEmpty(FTE017) || StringUtils.isEmpty(FTE039))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Student status " + STYLE_BOLD + STATUS_WITHDRAWN + STYLE_END
                                + " requires withdrawal date FTE017 and reason FTE039",
                        "FTE017=" + STYLE_BOLD + FTE017 + STYLE_END + ", FTE039=" + STYLE_BOLD + FTE039
                                + STYLE_END));
            }
            if (!STATUS_WITHDRAWN.equals(value) && (!StringUtils.isEmpty(FTE017) || !StringUtils.isEmpty(FTE039))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Student status requires withdrawal date FTE017 and reason FTE039 to be empty",
                        "FTE041=" + STYLE_BOLD + value + STYLE_END + ", FTE017=" + STYLE_BOLD + FTE017 + STYLE_END
                                + ", FTE039=" + STYLE_BOLD + FTE039 + STYLE_END));
            }
            if (CYCLE_MARCH.equals(FTE003) && !STATUS_NORMAL.equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Student status must be " + STYLE_BOLD + "N" + STYLE_END + " in report cycle 3",
                        "FTE017=" + STYLE_BOLD + FTE017 + STYLE_END + ", FTE003=" + STYLE_BOLD + FTE003
                                + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        /*
         * Job parameters.
         */
        m_fteDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_withdrawnAfterDate = (PlainDate) getParameter(PARAM_WITHDRAWN_AFTER_DATE);
        m_cycle = (String) getParameter(PARAM_CYCLE);


        SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        String startYear = String.valueOf(getCurrentContext().getSchoolYear() - 1);
        try {
            m_startOfYear = dateFormat.parse("07/01/" + startYear);
        } catch (ParseException e) {
            // Invalid Date format
            e.printStackTrace();
        }

        initializeFields();

        if (getSetupErrors().size() == 0) {
            /*
             * Build the criteria/query for the staff to include in this export based on user
             * input.
             */
            m_stdHelper = new GAFTEStudentHistoryHelper(this);
            m_stdHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_fteDate);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.FALSE);
            m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            if (m_cycle.equals(CYCLE_OCTOBER) && m_withdrawnAfterDate != null) {
                m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_WITHDRAWN_AFTER_DATE,
                        m_withdrawnAfterDate);
            }

            X2Criteria studentCriteria = m_stdHelper.getStudentCriteria();

            X2Criteria excludeCriteria = new X2Criteria();
            excludeCriteria.addNotEqualTo(m_doeExcludeField, BooleanAsStringConverter.TRUE);

            if (m_cycle.equals(CYCLE_OCTOBER)) {
                X2Criteria fedChildFindCriteria = new X2Criteria();
                fedChildFindCriteria.addEqualTo(m_doeExcludeField, BooleanAsStringConverter.TRUE);
                fedChildFindCriteria.addEqualTo(m_stdFieldFedChildFind, BooleanAsStringConverter.TRUE);

                X2Criteria orCriteria = new X2Criteria();
                orCriteria.addOrCriteria(excludeCriteria);
                orCriteria.addOrCriteria(fedChildFindCriteria);

                studentCriteria.addAndCriteria(orCriteria);
            } else {
                studentCriteria.addAndCriteria(excludeCriteria);
            }

            setQuery(m_stdHelper.getStudentQuery(false));
            setEntityClass(FteEntity.class);

            loadMaps();

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("FTE-CLEAN-NAME", new RetrieveStripNameChar());
            calcs.put("FTE-NOT-FTE001", new RetrieveNotFTE001());
            calcs.put("FTE-RES-ID-CODE", new RetrieveResidencyCode());
            calcs.put("FTE-RACE", new RetrieveRace());
            calcs.put("FTE-SCHOOL", new RetrieveSchool());
            calcs.put("FTE-STATUS", new RetrieveStatus());
            calcs.put("FTE-TENDAYS", new RetrieveTenDays());
            calcs.put("FTE-WITHDRAWAL", new RetrieveWithdrawal());
            calcs.put("FTE-YEAR", new RetrieveYear());
            calcs.put("FTE-PGM-CODE", new RetrievePgmCode());
            calcs.put("FTE-SPED-PRIM-AREA", new RetrieveSpedPrimaryArea());
            calcs.put("FTE-SPED-EXIT", new RetrieveSpedExit());
            calcs.put("FTE-SPED-TYPE", new RetrieveSpedType());
            calcs.put("FTE-NOT-WITHDRAWN", new RetrieveClearIfWithdrawn());
            calcs.put("FTE-SE-ENVIRONMENT", new RetrieveSEEnvironment());
            calcs.put("FTE-STD-SCHOOL-DATE", new RetrieveStudentUSSchoolDate());
            calcs.put("FTE-PAR-COMM-LANG", new RetrieveParentCommLang());
            calcs.put("FTE-PRM-LANGUAGE", new RetrievePrimaryLang());
            calcs.put("FTE-ALL-IEP", new RetrieveAllIep());
            calcs.put("FTE-GRADE", new RetrieveGrade());
            calcs.put("FTE-ELL", new RetrieveELL());
            calcs.put(RetrieveGAA.CALC_ID, new RetrieveGAA());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put("FTE-RESIDENT", new ValidateResidentStatus());
            validators.put("FTE-PROGRAM", new ValidateProgramCode());
            validators.put("FTE-STATUS", new ValidateStudentStatus());
            validators.put("FTE-HOURS", new ValidateHours());
            super.addValidators(validators);
        }
    }

    /**
     * The method which build map of all school calendar dates keyed on school oid.
     *
     * @return Map<String, Collection<SchoolCalendarDate>>
     */
    private Map<String, SchoolCalendarDate> getCSDMap() {
        X2Criteria csdCriteria = new X2Criteria();
        csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, getMostCommonCalendars());
        csdCriteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);

        QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, true);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_DATE, false);

        return getBroker().getMapByQuery(csdQuery,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR
                        + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                2056);
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @return Collection of SchoolCalendars oids
     */
    private Collection getMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        HashMap<String, String> schoolCalendars = new HashMap();

        X2Criteria criteria = new X2Criteria();

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!schoolCalendars.containsKey(schoolOid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                    if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                        SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                        schoolCalendars.put(schoolOid, schoolCalendar.getOid());
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (String oid : mapSchoolCalendars.keySet()) {
            if (!schoolCalendars.containsKey(oid)) {
                Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                schoolCalendars.put(oid, schoolCalendar.getOid());
            }
        }

        return schoolCalendars.values();
    }

    /**
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
        X2Criteria casCriteria = new X2Criteria();

        casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        // Filter to eliminate unused schools.
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR,
                Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

        return getBroker().getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
    }

    /**
     * Initialize instance variables, aliases, lookups.
     */
    private void initializeFields() {
        /*
         * Initialize local values
         */
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        String withdrawalCodesRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, withdrawalCodesRefTableOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_withdrawalCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 30);

        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not
         * found.
         */
        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_spedExitReasonField = translateAliasToJavaName(DOE_SPED_EXIT_REASON, true);
        m_retainedField = translateAliasToJavaName(ALIAS_DOE_RETAINED, true);
        m_studentDateEntryUSField = translateAliasToJavaName(ALIAS_DOE_DATE_ENTRY_US, true);
        m_placeOfBirthField = translateAliasToJavaName(ALIAD_PLACE_OF_BIRTH, true);
        m_primaryLangField = translateAliasToJavaName(ALIAS_DOE_PRIMARY_LANG, true);
        m_parentCommLangField = translateAliasToJavaName(ALIAS_DOE_PARENT_COMM_LANG, true);
        m_doeEllField = translateAliasToJavaName(ALIAS_DOE_ELL, true);
        m_doeExcludeField = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_STD, true);
        m_doeElYearBeganField = translateAliasToJavaName(ALIAS_DOE_EL_YEAR, true);
        m_stdFieldFedChildFind = translateAliasToJavaName(ALIAS_STD_FEDERAL_CHILD_FIND, true);
        m_stdFieldSpedEnv = translateAliasToJavaName(ALIAS_STD_SPED_ENV, true);
        m_stdFieldSpedType = translateAliasToJavaName(ALIAS_STD_SPED_TYPE, true);
        m_stdFieldGAAFlag = translateAliasToJavaName(ALIAS_STD_GAA_FLAG, true);
        /*
         * Calculate the membership day that represents the beginning of ten-days-attendance
         * period.
         */
        criteria = new Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
        criteria.addLessThan(DistrictCalendar.COL_DATE, m_fteDate);
        query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderBy(DistrictCalendar.COL_DATE, false);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);

        try {
            int counter = 0;
            DistrictCalendar cal = null;
            while (counter < 10 && iterator.hasNext()) {
                cal = (DistrictCalendar) iterator.next();
                counter++;
            }
            m_10DayCutoff = cal != null ? cal.getDate() : null;
        } finally {
            iterator.close();
        }

        /*
         * Find the start date of the current year.
         */
        m_currentYearStartDate = getOrganization().getCurrentContext().getStartDate();
    }

    /**
     * Load maps of supporting data.
     */
    private void loadMaps() {
        // Get race code reference codes for use in the race retriever.
        X2Criteria studentCriteria = m_stdHelper.getStudentCriteria();
        Criteria raceCodeCriteria = new Criteria();
        raceCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCodeCriteria);
        m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

        // Load the race codes for all students included in the export.
        SubQuery studentQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, studentQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

        // Load attendance record counts for student in the 10-day pre-report period.
        // Main report query - students and Absence.
        Criteria attendanceCriteria = new Criteria();
        attendanceCriteria.addIn(StudentAttendance.COL_STUDENT_OID, studentQuery);
        attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_10DayCutoff);
        attendanceCriteria.addLessThan(StudentAttendance.COL_DATE, m_fteDate);
        attendanceCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                new String[] {StudentAttendance.COL_STUDENT_OID, "SUM(ATT_PORTION_ABSENT)"}, attendanceCriteria);
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

        // Load a map of IEPs by student.
        List<Integer> statusTypes = Arrays.asList(Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal()),
                Integer.valueOf(IepData.StatusCode.AMENDED.ordinal()));
        Criteria iepCriteria = new Criteria();
        iepCriteria.addIn(IepData.COL_STUDENT_OID, studentQuery);
        iepCriteria.addIn(IepData.COL_STATUS_CODE, statusTypes);
        // Only IEPs created before report date
        iepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_fteDate);
        // Only IEPs that did not end before report date
        Criteria criteriaE1 = new Criteria();
        Criteria criteriaE2 = new Criteria();
        criteriaE1.addGreaterOrEqualThan(IepData.COL_END_DATE, m_fteDate);
        criteriaE2.addIsNull(IepData.COL_END_DATE);
        criteriaE1.addOrCriteria(criteriaE2);
        iepCriteria.addAndCriteria(criteriaE1);
        // Only IEPs where the student has not exited before report date
        criteriaE1 = new Criteria();
        criteriaE2 = new Criteria();
        criteriaE1.addGreaterOrEqualThan(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SPED_EXIT_DATE,
                m_fteDate);
        criteriaE2.addIsNull(IepData.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SPED_EXIT_DATE);
        criteriaE1.addOrCriteria(criteriaE2);
        iepCriteria.addAndCriteria(criteriaE1);
        query = new QueryByCriteria(IepData.class, iepCriteria);
        query.addOrderBy(IepData.COL_STUDENT_OID, true);
        query.addOrderBy(IepData.COL_START_DATE, true);
        m_iepMap = getBroker().getMapByQuery(query, IepData.COL_STUDENT_OID, 256);

        m_schoolCalendarDatesMap = getCSDMap();
    }
}
