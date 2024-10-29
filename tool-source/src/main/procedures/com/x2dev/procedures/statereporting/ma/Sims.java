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

package com.x2dev.procedures.statereporting.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.ACTIVE;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.EXITED;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.INELIGIBLE;
import static com.x2dev.sis.model.beans.SisStudent.SpedStatusCode.REFERRED;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_EDUCATIONAL_ENVIRONMENT;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_EDUCATIONAL_ENVIRONMENT_EC;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_LEVEL_OF_NEED;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPA_SPECIAL_INSTRUCTION_CONTENT;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPA_SPECIAL_INSTRUCTION_METHODOLOGY;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPA_SPECIAL_INSTRUCTION_PERFORMANCE;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPB_SPECIAL_INSTRUCTION_CONTENT;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPB_SPECIAL_INSTRUCTION_METHODOLOGY;
import static com.x2dev.sis.model.business.sped.MassachusettsAliases.IEP_PLEPB_SPECIAL_INSTRUCTION_PERFORMANCE;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.beans.SisStudent.Section504StatusCode;
import com.x2dev.sis.model.beans.SisStudent.SpedStatusCode;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.model.business.sped.IepLookup;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.Normalizer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Massachusetts state report for SIMS export.
 * This class implements the data export for Mass SIMS export.
 *
 * @author X2 Development Corporation
 */
public class Sims extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SIMS export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SIMSEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        Date m_birthDate = null;
        Boolean m_requireMemberDay = null;
        Sims m_simsData = null;
        EnrollmentSnapshot m_snapshot = null;

        /*
         * Place holders for calculated unmapped fields. These can be written
         * back to the database in postProcess if update flag is set.
         * Also, holds some calculated values that have been overridden with
         * default or related values.
         *
         * Map key should be field alias constant.
         */
        Map<String, Object> m_updateValues = null;

        /**
         * Instantiates a new SIMS entity.
         */
           /*
            * Public no argument constructor for dynamic instantiation.
            */
        public SIMSEntity() {
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
            // If the student has a status of "NotEnroll SPED Only" then ignore requireMemberDay
            // check
            SisStudent student = (SisStudent) getBean();

            if (!"NotEnroll SPED Only".equals(student.getEnrollmentStatus())) {
                if (m_requireMemberDay == null) {
                    m_requireMemberDay = (Boolean) getData().getParameter(REQUIRE_MEMBER_DAY_PARAM);
                }

                if (m_requireMemberDay != null && !m_simsData.m_summerWithdrawals.contains(getBean().getOid())) {
                    // Get membership days parameter.
                    int membershipCountAsInt = 0;

                    // Get enrollment count (DOE 18). (Force calculation of membership days and
                    // storage of count).
                    String membershipCount = getFieldValue(DOE_18_MEMBERSHIP_DAYS);

                    // Get saved value before 555 change up.
                    membershipCount = (String) getUpdateValue(DOE_18_MEMBERSHIP_DAYS);

                    if (membershipCount != null) {
                        try {
                            membershipCountAsInt = Integer.parseInt(membershipCount);
                        } catch (NumberFormatException nfe) {
                            // invalid format, will be reported elsewhere.
                        }
                    }

                    // check enrollment count and membership days parameter.
                    if ((m_requireMemberDay.booleanValue() && membershipCountAsInt > 0)
                            || !m_requireMemberDay.booleanValue()) {
                        // No filtering.
                    } else {
                        // Student filtered.
                        error = new StateReportValidationError(this,
                                getData().getFieldDefinition(DOE_18_MEMBERSHIP_DAYS),
                                "0 member days - excluded from export", "");
                    }
                }
            }
            /*
             * October report check for summer withdrawals:
             *
             * If the October Report option is checked, all summer withdrawals are reported.
             *
             * If the October Report option is NOT checked only codes 20-36 are reported for the
             * summer withdrawals.
             */
            String enrollmentStatus = getFieldValue(DOE_12_ENROLLMENT_STATUS);

            if (m_simsData.m_summerWithdrawals.contains(getBean().getOid()) &&
                    !m_simsData.m_octoberReport &&
                    m_simsData.m_octoberReportOnlyEnrollmentCodes.contains(enrollmentStatus) &&
                    error == null) {
                error = new StateReportValidationError(this, getData().getFieldDefinition(DOE_12_ENROLLMENT_STATUS), "",
                        "Enrollment Status Code: " + enrollmentStatus + " - Non-October exclusion");
            }

            return error;
        }

        /**
         * Returns the birth date as a date.
         *
         * @return date
         */
        public Date getBirthDate() {
            if (m_birthDate == null) {
                String DOE06 = getFieldValue(DOE_06_BIRTH_DATE);
                try {
                    m_birthDate = ((Sims) getData()).m_dateFormat.parse(DOE06);
                } catch (ParseException e) {
                    // Fails to parse. Ignore, will be reported elsewhere.
                }
            }
            return m_birthDate;
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
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";

            return name;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot(PlainDate reportDate, FieldDefinition field) {
            if (m_snapshot == null) {
                m_snapshot = getSnapshot((SisStudent) getBean(), reportDate, field);
            }
            return m_snapshot;
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

            m_simsData = (Sims) data;
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
             * accumulate membership and attendance totals for
             * this student into school/grade subtotals.
             */
            String DOE11 = getFieldValue(DOE_11_REASON);
            String DOE12 = getFieldValue(DOE_12_ENROLLMENT_STATUS);
            String DOE17 = getFieldValue(DOE_17_ATTENDANCE_DAYS);
            String DOE18 = getFieldValue(DOE_18_MEMBERSHIP_DAYS);
            if (!ZERO_DAYS_OVERRIDE.equals(DOE18) && DOE11 != null && DOE11.matches("0[1|3]")) {
                try {
                    int intDOE17 = Integer.parseInt(DOE17);
                    int intDOE18 = Integer.parseInt(DOE18);
                    Map<String, MembershipAttendance> countsMap = ((Sims) getData()).getMembershipStats();
                    SisStudent student = ((SisStudent) getBean());
                    String key = student.getSchool().getName() + "\t" + student.getGradeLevel();
                    MembershipAttendance memAtt = countsMap.get(key);
                    if (memAtt == null) {
                        memAtt = new MembershipAttendance(key, intDOE18, intDOE17);
                        countsMap.put(key, memAtt);
                    } else {
                        memAtt.addMemebAtted(intDOE18, intDOE17);
                    }
                } catch (NumberFormatException nfe) {
                    // Do nothing, can not accumulate statistics for this student.
                    // The bad values will be reported elsewhere.
                }
            }

            /*
             * Check DOE 12 cannot have both 10 and 11 in the same district.
             */
            if ("10".equals(DOE12)) {
                ((Sims) getData()).setDOE12Cert(true);
            } else if ("11".equals(DOE12)) {
                ((Sims) getData()).setDOE12Grad(true);
            }

            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (updateRecords != null && updateRecords.booleanValue()) {
                try {
                    Converter integerConverter = ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER,
                            Locale.getDefault(), true);

                    FieldDefinition field = getData().getFieldDefinition(DOE_10_RACE);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(DOE_10_RACE));

                    field = getData().getFieldDefinition(DOE_17_ATTENDANCE_DAYS);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(DOE_17_ATTENDANCE_DAYS)));

                    field = getData().getFieldDefinition(DOE_18_MEMBERSHIP_DAYS);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(DOE_18_MEMBERSHIP_DAYS)));

                    field = getData().getFieldDefinition(DOE_52_TRUANCIES);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            integerConverter.stringToJava(getFieldValue(DOE_52_TRUANCIES)));

                    field = getData().getFieldDefinition(DOE_32_SPED_PLACEMENT);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getUpdateValue(DOE_32_SPED_PLACEMENT));
                    field = getData().getFieldDefinition(DOE_34_SPED_PLACEMENT);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getUpdateValue(DOE_34_SPED_PLACEMENT));
                    field = getData().getFieldDefinition(DOE_36_SPED_DISABILITY);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getUpdateValue(DOE_36_SPED_DISABILITY));
                    field = getData().getFieldDefinition(DOE_38_SPED_LEVEL);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getUpdateValue(DOE_38_SPED_LEVEL));
                    field = getData().getFieldDefinition(DOE_39_IN_504);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getUpdateValue(DOE_39_IN_504));
                    field = getData().getFieldDefinition(DOE_40_SPED_EVALUATION_RESULTS);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(),
                            getUpdateValue(DOE_40_SPED_EVALUATION_RESULTS));

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
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @param field FieldDefinition
         * @return EnrollmentSnapshot
         */
        @SuppressWarnings("all")
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate, FieldDefinition field) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            if (!SUPPRESS_ENROLLMENT_WARNINGS && !snapshot.isPrecise()) {
                addRetrievalError(DOE_12_ENROLLMENT_STATUS, new StateReportValidationError(this, field,
                        "WARNING: Enrollment information (enrollment status, school, and/or YOG) is not precise", ""));
            }

            return snapshot;
        }
    }

    /**
     * A class for storing membership and attendance statistics for averages checking.
     *
     * @author X2 Development Corporation
     */
    protected static class MembershipAttendance {
        /**
         * Instance for membership count and attendance count.
         */
        long m_attendance;
        String m_key;
        long m_membership;
        long m_students;

        /**
         * constructor, set initial attendance and membership counts.
         *
         * @param key String
         * @param member long
         * @param attend long
         */
        protected MembershipAttendance(String key, long member, long attend) {
            m_key = key;
            m_membership = member;
            m_attendance = attend;
            m_students = 1;
        }

        /**
         * Add new values to the attendance and membership counts.
         *
         * @param member long
         * @param attend long
         */
        protected void addMemebAtted(long member, long attend) {
            m_membership += member;
            m_attendance += attend;
            m_students++;
        }

        /**
         * return the accumulated attendance count.
         *
         * @return long
         */
        protected long getAttendance() {
            return m_attendance;
        }

        /**
         * Returns the key for this record (school/grade).
         *
         * @return String
         */
        protected String getKey() {
            return m_key;
        }

        /**
         * Return the accumulated membership count.
         *
         * @return long
         */
        protected long getMembership() {
            return m_membership;
        }

        /**
         * Return the count of students that added membership totals.
         *
         * @return long
         */
        protected long getStudents() {
            return m_students;
        }
    }

    /*
     * DOE field alias constants. These field aliases are all for the STUDENT table except for DOE
     * field 15 - that is an alias for the SCHOOL table.
     *
     * It is assumed that the "count" fields (for In-school Suspensions, Out-of-school Suspensions,
     * Truancies, Days in Attendance, and Days in Membership) are stored in a field with a
     * user-defined type of Integer. This assumption is used both when retrieving values from the
     * Student bean and when updating the Student bean with calculated totals.
     *
     * Fields without aliases must have a bean path specified in the Field Definition.
     */
    private static final String DOE_01_LOCAL_ID = "DOE 01";
    private static final String DOE_02_STATE_ID = "DOE 02";
    private static final String DOE_03_FIRST_NAME = "DOE 03";
    private static final String DOE_04_MIDDLE_NAME = "DOE 04";
    private static final String DOE_05_LAST_NAME = "DOE 05";
    private static final String DOE_06_BIRTH_DATE = "DOE 06";
    private static final String DOE_07_BIRTH_DATE_FORMAT = "DOE 07";
    private static final String DOE_08_BIRTH_CITY = "DOE 08";
    private static final String DOE_09_GENDER = "DOE 09";
    private static final String DOE_10_RACE = "DOE 10";
    private static final String DOE_11_REASON = "DOE 11";
    private static final String DOE_12_ENROLLMENT_STATUS = "DOE 12";
    private static final String DOE_13_ENROLLMENT_REASON = "DOE 13";
    private static final String DOE_14_TOWN_OF_RESIDENCE = "DOE 14";
    private static final String DOE_15_SCHOOL = "DOE 15";
    private static final String DOE_16_GRADE = "DOE 16";
    private static final String DOE_17_ATTENDANCE_DAYS = "DOE 17";
    private static final String DOE_18_MEMBERSHIP_DAYS = "DOE 18";
    private static final String DOE_19_LOW_INCOME_STATUS = "DOE 19";
    private static final String DOE_20_TITLE_1 = "DOE 20";
    private static final String DOE_21_LEP_FIRST_YEAR = "DOE 21";
    private static final String DOE_22_IMMIGRATION_STATUS = "DOE 22";
    private static final String DOE_23_COUTRY_OF_ORIGIN = "DOE 23";
    private static final String DOE_24_NATIVE_LANGUAGE = "DOE 24";
    private static final String DOE_25_ENGLISH_PROFICIENCY = "DOE 25";
    private static final String DOE_26_ENGLISH_LEARNERS_PROGRAM = "DOE 26";
    private static final String DOE_27_ALT_EDUCATION_PROGRAM = "DOE 27";
    private static final String DOE_28_TITLE1_SCHOOL_CHOICE = "DOE 28";
    private static final String DOE_29_FAMILY_MILITAR_STATUS = "DOE 29";
    private static final String DOE_30_TITLE_TARGETED = "DOE 30";
    private static final String DOE_31_CTE_COMPETENCY = "DOE 31";
    private static final String DOE_32_SPED_PLACEMENT = "DOE 32";
    private static final String DOE_33_FUTURE_PLANS = "DOE 33";
    private static final String DOE_34_SPED_PLACEMENT = "DOE 34";
    private static final String DOE_35_CTE_PROGRAM = "DOE 35";
    private static final String DOE_36_SPED_DISABILITY = "DOE 36";
    private static final String DOE_37_GRADUATE_CORE_CURRICULUM = "DOE 37";
    private static final String DOE_38_SPED_LEVEL = "DOE 38";
    private static final String DOE_39_IN_504 = "DOE 39";
    private static final String DOE_40_SPED_EVALUATION_RESULTS = "DOE 40";
    private static final String DOE_41_SLIFE = "DOE 41";
    private static final String DOE_42_CTE_POPULATION = "DOE 42";
    private static final String DOE_43_CTE_74 = "DOE 43";
    private static final String DOE_44_CTE_NON_74 = "DOE 44";
    private static final String DOE_45_SUSPENSIONS_IN = "DOE 45";
    private static final String DOE_46_SUSPENSIONS_OUT = "DOE 46";
    private static final String DOE_47_AP_COURSE_1 = "DOE 47";
    private static final String DOE_48_AP_COURSE_2 = "DOE 48";
    private static final String DOE_49_AP_COURSE_3 = "DOE 49";
    private static final String DOE_50_AP_COURSE_4 = "DOE 50";
    private static final String DOE_51_AP_COURSE_5 = "DOE 51";
    private static final String DOE_52_TRUANCIES = "DOE 52";

    private static final String DOE_NO_MIDDLE_NAME = "DOE NMN";

    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values).
     *
     * Note: The alias for the adjusted status is optional (is does not have to be defined in the
     * Data Dictionary).
     */
    private static final String ADJUSTED_ATTENDANCE_COUNT_FIELD = "DOE Adjusted Attendance";
    private static final String ADJUSTED_MEMBERSHIP_COUNT_FIELD = "DOE Adjusted Membership";
    private static final String ADJUSTED_SCHOOL_CODE_FIELD = "DOE Adjusted School";
    private static final String ADJUSTED_STATUS_FIELD = "DOE Adjusted Status";

    /*
     * Field alias for the AP course field on the COURSE table.
     */
    private static final String EXIT_DATE_504 = "DOE 504 End Date";

    /*
     * Field alias for the AP course field on the COURSE table.
     */
    // private static final String AP_COURSE_CODE_FIELD = "DOE AP Course";

    /*
     * Field alias for the adjusted district code on the SCHOOL table. This alias is optional.
     */
    private static final String ADJUSTED_DISTRICT_CODE_FIELD = "DOE ADJUSTED DISTRICT";

    /*
     * Field alias for Haverhill's Adjusted City of Residence field on the Student table.
     */
    private static final String ADJUSTED_RESIDENCE_CITY_FIELD = "DOE Adjusted City";

    /*
     * display label for discontinued fields.
     */
    private static final String DISCONTINUED = "label.state.report.discontinued";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_STATUS_FIELD = "DOE Status";
    private static final String DOE_STATUS_FIELD_REPORT_CODE = "Report";

    /*
     * State code for enrollment status of Graduate
     */
    // private static final String GRADUATE_DOE_12_STATE_CODES = "04,10,11";

    /*
     * Constants for determining out-of-state towns of residence. If the state in the student's
     * physical address does not match MA_STATE_VALUE then OUT_OF_STATE_CODE will be exported. This
     * is done to help distinguish towns like Salem, MA from Salem, NH.
     */
    private static final String MA_STATE_VALUE = "MA";
    private static final String OUT_OF_STATE_CODE = "888";

    /*
     * October report DOE 12 codes
     */
    private static final String OCTOBER_REPORT_DOE_12_STATE_CODES = "04,05,06,09,10,11,40,41";

    /*
     * The DOE uses an arbitrary code for its combinations of races/ethnicities. X2 instead uses
     * a bitmap with the following values:
     *
     * - WHITE: 1
     * - BLACK: 2
     * - ASIAN: 4
     * - AMERICAN INDIAN: 8
     * - PACIFIC ISLANDER: 16
     * - HISPANIC: 32 (this value is actually used as a bitmask by the DOE)
     *
     * The X2 bitmap is translated to a state code using the following array.
     *
     * (DOE values from http://www.doe.mass.edu/infoservices/data/guides/masscodes.html)
     */
    protected static final int[] STATE_RACE_CODES = new int[]
    // X2 Values: { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    // 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }
    {0, 1, 2, 6, 3, 7, 10, 16, 4, 8, 11, 17, 13, 19, 23, 26, 5, 9, 12, 18, 14, 29, 22, 26, 15, 21, 24, 27, 25, 28, 30,
            31};

    /*
     * Conduct codes
     */
    private static final String SUSPENSION_IN_CODE = "1";
    private static final String SUSPENSION_OUT_CODE = "2";
    private static final String TRUANCY_CODE = "XTR";

    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- X2-specific code below
    // ---------------------------------------------------------------------------------------------

    /*
     * TODO: Calculation optimizations: perform one query for all students and store results in a
     * map for conduct, attendance, and membership lookups (possibly AP courses as well but that
     * might be more difficult to do in a single query). Also, put the term OID lookup in a map
     * keyed on schedule OID.
     */

    /**
     * Name for the "calculate AP courses" parameter. The value is a Boolean.
     */
    public static final String CALCULATE_AP_COURSES_PARAM = "calculateApCourses";

    /**
     * Name for the "calculate totals" parameter. The value is a Boolean.
     */
    public static final String CALCULATE_TOTALS_PARAM = "calculateTotals";

    /**
     * Name for the "calculate DOE 39" parameter. The value is a Boolean.
     */
    public static final String CALCULATE_DOE_39_PARAM = "calculateDOE39";

    /**
     * Name for the "include Family Military Status" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_FAMILY_MILITARY_STATUS_PARAM = "includeFamilyMilitaryStatus";

    /**
     * Name for the "October report" parameter. The value is a Boolean.
     */
    public static final String OCTOBER_REPORT_PARAM = "octoberReport";

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
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";

    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";

    /**
     * Name for the "retrieve special ed values" parameter. The value is a Boolean.
     */
    public static final String RETRIEVE_SPED_VALUES = "retrieveSpedValues";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "SPED only" parameter. The value is a Boolean.
     */
    public static final String SPED_ONLY_PARAM = "spedOnly";

    /**
     * Name for the "summer end date" parameter. The corresponding values is a java.sql.Date object.
     */
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";

    /**
     * Name for the "summer start date" parameter. The corresponding values is a java.sql.Date
     * object.
     */
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";

    /**
     * Name for the "calculate totals" parameter. The value is a Boolean.
     */
    public static final String TRUANCY_CALCULATION_TYPE_PARAM = "truancyType";

    /**
     * Name for the "update student records" parameter. The value is a Boolean.
     */
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";

    public static final String PARAM_INCLUDE_SIF_SCHOOL = "includeSifSchoolId";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_BIRTH_CITY_CHARACTERS = "[_\\W&&[^\\s]]";
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String DATE_FORMAT_CODE = "D10";
    private static final String DEFAULT_AP_COURSE_CODE = "500";
    private static final String REGEX_ALPHANUMERIC = "[A-Za-z0123456789]*";
    private static final String REGEX_ALPHANUMERIC_SPACE = "[A-Za-z0123456789 ]*";
    private static final String REGEX_NUMERIC = "[0123456789]*";
    private static final String REGEX_AP_COURSE = "[012][0123456789][0123456789]|[35]00";
    private static final boolean SUPPRESS_ENROLLMENT_WARNINGS = true;
    private static final String ZERO_DAYS_OVERRIDE = "555";
    private static final String ALIAS_SKL_SIF_DISTRICT_ID = "skl-sif-district-id";
    private static final String SEPARATOR_COMMA = ",";

    // private static final String TRUANCT_TYPE_ATTENDANCE = "attendance";
    private static final String TRUANCT_TYPE_INCIDENT = "incident";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_adjustedAttendanceCount;
    protected String m_adjustedMembershipCount;
    protected String m_adjustedSchoolCode;
    // protected String m_apCourseCode;
    protected Map<String, List<String>> m_apCourses;
    protected Map<String, Integer> m_absences;
    protected Map<String, Integer> m_absencesUnex;
    // protected boolean m_calculateApCourses;
    protected boolean m_calculateTotals;
    protected boolean m_calculateDOE39;
    protected SimpleDateFormat m_dateFormat;
    protected String m_doeNoMiddleName;
    protected String m_doeStatusField;
    protected boolean m_doe12_cert;
    protected boolean m_doe12_grad;
    protected String m_doe15Alias;
    protected EnrollmentManager m_enrollmentManager;
    protected Collection<String>[] m_enrollmentStatusMatches;
    protected Collection<String>[] m_enrollmentReasonMatches;
    protected String m_exitDate504Field;
    protected PlainDate m_firstDayDate;
    protected Set m_firstDayMembers;
    protected TreeMap m_gradeLevelMap;
    protected DataDictionary m_iepDictionary;
    protected Map<String, Collection<IepDisability>> m_iepDisabilityLookup;
    protected IepLookup m_iepLookup;
    protected String m_iepEdEnviroment3_5FieldName;
    protected String m_iepEdEnviroment6_21FieldName;
    protected Map<String, Collection<IepData>> m_iepDataTransferStudents;
    protected boolean m_includeMilitaryStatus;
    protected Converter m_integerConverter;
    protected Pattern m_illegalBirthCityCharacters;
    protected Pattern m_illegalNameCharacters;
    protected Pattern m_diacriticalMarks;
    protected Map<String, MembershipAttendance> m_membershipStats;
    protected boolean m_octoberReport;
    protected List<String> m_octoberReportOnlyEnrollmentCodes;
    protected Date m_preDateMax;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, String> m_raceRefMap;
    protected PlainDate m_reportDate;
    protected Date m_reportDateMax;
    protected Date m_reportDateMin;
    protected boolean m_retrieveSpedValues;
    protected ScheduleManager m_scheduleManager;
    protected HashMap m_schoolsToCalendars;
    protected Date m_spedDateMax;
    protected Date m_sped2DateMax;
    protected Collection<String> m_summerWithdrawals;
    protected Collection m_suspensionInCodes;
    protected Collection m_suspensionOutCodes;
    protected Map<String, Integer> m_suspensionsIn;
    protected Map<String, Integer> m_suspensionsOut;
    protected Map<String, Integer> m_truancies;
    protected String m_truancyType;
    protected Collection m_truancyCodes;
    protected StudentHistoryHelper m_helper;
    protected List<String> m_includeSifSchoolIds = null;
    protected String m_sklDstrIdField = null;

    /**
     * Returns the AP courses for the given student.
     * This retriever is used by several fields (DOE 47 - DOE 51).
     *
     * The calculation of all courses works once and stores the
     * results into the entity object for repeated retrieval.
     *
     * When defining the FieldDefinition, place a Integer(1) - Integer(5)
     * into the FieldDefinition.other parameter to indicate which AP course
     * to retrieve.
     */
    protected class RetrieveApCourse implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String courseCode = DEFAULT_AP_COURSE_CODE;

            /*
             * SisStudent student = (SisStudent) entity.getBean();
             * Integer indexI = (Integer) field.getParameter();
             * int index = 1;
             * if (indexI != null)
             * {
             * index = indexI.intValue();
             * }
             *
             * if (m_calculateApCourses)
             * {
             * // Get ap course from loaded map for student at index position.
             * List<String> courses = m_apCourses.get(student.getOid());
             * if (courses != null && index >= 1 && index <= courses.size())
             * {
             * courseCode = courses.get(index - 1);
             * }
             * }
             * else
             * {
             * // get ap course from student fields.
             * String fieldId = (index == 1 ? DOE_47_AP_COURSE_1 :
             * (index == 2 ? DOE_48_AP_COURSE_2 :
             * (index == 3 ? DOE_49_AP_COURSE_3 :
             * (index == 4 ? DOE_50_AP_COURSE_4 :
             * DOE_51_AP_COURSE_5))));
             * String property = getFieldDefinition(fieldId).getBeanPath();
             * String code = (String) WebUtils.getProperty(student, property);
             * if (!StringUtils.isEmpty(code))
             * {
             * courseCode = code;
             * }
             * }
             */

            return courseCode;
        }
    }

    /**
     * Returns the attendance days for the student.
     * If membership days is zero days override (555) then attendance is also.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAttendanceDays implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            int membershipCountAsInt = 0;
            int absenceCountAsInt = 0;
            int attendanceCountAsInt = 0;
            String attendanceCount = null;
            String membershipCount = entity.getFieldValue(DOE_18_MEMBERSHIP_DAYS); // Get membership
                                                                                   // days.
            if (m_calculateTotals) {
                if (ZERO_DAYS_OVERRIDE.equals(membershipCount)) {
                    attendanceCount = ZERO_DAYS_OVERRIDE;
                } else if (membershipCount != null) {
                    String adjustedCount = (String) WebUtils.getProperty(student, m_adjustedAttendanceCount);
                    if (!StringUtils.isEmpty(adjustedCount)) {
                        attendanceCount = adjustedCount;
                    } else {
                        try {
                            membershipCountAsInt = Integer.parseInt(membershipCount);
                        } catch (NumberFormatException nfe) {
                            // Do nothing, this error has already been logged.
                        }
                        Integer absenceCount = m_absences.get(student.getOid());
                        if (absenceCount != null) {
                            absenceCountAsInt = absenceCount.intValue();
                        }

                        attendanceCountAsInt = membershipCountAsInt - absenceCountAsInt;

                        if (attendanceCountAsInt < 0) {
                            entity.addRetrievalError(field.getFieldId(),
                                    new StateReportValidationError(entity, field,
                                            "Attendance less than zero",
                                            "Membership=" + STYLE_BOLD + Integer.toString(membershipCountAsInt)
                                                    + STYLE_END +
                                                    ", Absense=" + STYLE_BOLD + Integer.toString(absenceCountAsInt)
                                                    + STYLE_END));
                        }

                        if (membershipCountAsInt == 0) {
                            attendanceCount = ZERO_DAYS_OVERRIDE;
                        } else {
                            attendanceCount = Integer.toString(attendanceCountAsInt);
                        }
                    }
                }
            } else {
                attendanceCount = m_integerConverter.javaToString(WebUtils.getProperty(student, field.getBeanPath()));
            }
            return attendanceCount;
        }
    }

    /**
     * Retriever for birth city. This filters illegal characters from the birth city.
     * [Currently empty, placeholder implementation in case some show up]
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveBirthCity implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String city = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (city != null) {
                Matcher matcher = m_illegalBirthCityCharacters.matcher(city);
                city = matcher.replaceAll("");
            }

            return city;
        }
    }

    /**
     * Returns the state equivalent for the enrollment status of the student based on the snapshot.
     * The student may have an adjusted status that will override the value in the snapshot.
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
            EnrollmentSnapshot snapshot = ((SIMSEntity) entity).getSnapshot(m_reportDate, field);
            SisStudent student = (SisStudent) entity.getBean();
            String baseCode = (String) student.getFieldValueByAlias(ADJUSTED_STATUS_FIELD);
            if (StringUtils.isEmpty(baseCode)) {
                baseCode = snapshot.getEnrollmentStatus();
            }

            return baseCode;
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
            EnrollmentSnapshot snapshot = ((SIMSEntity) entity).getSnapshot(m_reportDate, field);
            SisStudent student = (SisStudent) entity.getBean();
            SisOrganization district =
                    (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, student.getOrganization1Oid());
            int schoolYear = district.getCurrentContext().getSchoolYear();

            String gradeLevel = student.getGradeLevel();
            if (snapshot.getYog() != student.getYog()) {
                int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
                List gradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, snapshot.getYog(), schoolYear,
                        m_gradeLevelMap);

                gradeLevel = (String) gradeLevels.get(0);

                if (gradeLevels.size() > 1) {
                    entity.addRetrievalError(DOE_16_GRADE, new StateReportValidationError(entity, field,
                            "WARNING: Calculated grade level is not precise",
                            "DOE16=" + STYLE_BOLD + gradeLevels.toString() + STYLE_END));
                }
            }

            return gradeLevel;
        }
    }

    /**
     * Returns the in 504 status. This value is taken from the student 504 status and dates if the
     * "retrieve sped values" input parameter was selected. Otherwise the value currently on the
     * student record is returned.
     * <p>
     * The 504 status indicates the student is in 504 or was in 504 during the school year.
     *
     */
    protected class RetrieveIn504 implements FieldRetriever {
        private SimpleDateFormat exitDateFormat = new SimpleDateFormat("yyyy-MM-dd");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String in504 = data.lookupUserValue(SisStudent.class, field.getBeanPath(), "00");

            if (m_calculateDOE39) {
                boolean active = false;
                // If the district has the 504 exit date set, use that to determine
                // exited this year.
                // If the exit date is not set, use 504 status == EXITED to indicate exited this
                // year.
                if (StringUtils.isEmpty(m_exitDate504Field)) {
                    active = (Section504StatusCode.ACTIVE.equals(student.getSection504StatusCodeEnum()) ||
                            Section504StatusCode.EXITED.equals(student.getSection504StatusCodeEnum()));
                } else {
                    Object exitDate504 = student.getFieldValueByBeanPath(m_exitDate504Field);
                    // If this is a String (custom field) convert to Plain date.
                    // If it is already a plain date, leave it.
                    if (exitDate504 instanceof String) {
                        try {
                            exitDate504 = exitDateFormat.parse((String) exitDate504);
                        } catch (ParseException dfe) {
                            exitDate504 = null;
                        }
                    }
                    if (exitDate504 instanceof java.util.Date) {
                        active = (Section504StatusCode.ACTIVE.equals(student.getSection504StatusCodeEnum())) ||
                                (Section504StatusCode.EXITED.equals(student.getSection504StatusCodeEnum()) &&
                                        !data.getOrganization().getCurrentContext().getStartDate()
                                                .after((java.util.Date) exitDate504));
                    } else {
                        active = Section504StatusCode.ACTIVE.equals(student.getSection504StatusCodeEnum());
                    }
                }
                if (active) {
                    in504 = data.lookupUserValue(SisStudent.class, field.getBeanPath(), "01");
                }
            } else {
                in504 = (String) WebUtils.getProperty(student, field.getBeanPath());
            }

            /*
             * Save the unmapped value in the entity so it can be written
             * back to the student record in postProcess.
             */
            ((SIMSEntity) entity).setUpdateValue(field.getFieldId(), in504);

            return in504;
        }
    }

    /**
     * Retrieve membership days for a student.
     * If the count is zero, use the 555 placeholder.
     *
     * @author X2 Development Corporation
     *         TODO
     */
    protected class RetrieveMembershipDays implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            EnrollmentSnapshot snapshot = ((SIMSEntity) entity).getSnapshot(m_reportDate, field);
            String membershipCount = getMembershipDays(entity, snapshot, field);
            int membershipCountAsInt = 0;
            if (membershipCount != null) {
                try {
                    membershipCountAsInt = Integer.parseInt(membershipCount);
                } catch (NumberFormatException nfe) {
                    // Do nothing, this error has already been logged.
                }
            }
            /*
             * Save the unmapped value in the entity so it can be written
             * back to the student record in postProcess.
             */
            if (membershipCount == null) {
                membershipCount = "0";
            }

            ((SIMSEntity) entity).setUpdateValue(field.getFieldId(), membershipCount);

            if (membershipCountAsInt == 0) {
                membershipCount = ZERO_DAYS_OVERRIDE;
            }

            return membershipCount;
        }

        /**
         * Returns the number of days the student has been a member from the start of school to the
         * report date.
         *
         * @param entity StateReportEntity
         * @param snapshot EnrollmentSnapshot
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         */
        private String getMembershipDays(StateReportEntity entity, EnrollmentSnapshot snapshot, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String count = null;

            if (m_calculateTotals) {
                String adjustedCount = (String) WebUtils.getProperty(student, m_adjustedMembershipCount);
                if (!StringUtils.isEmpty(adjustedCount)) {
                    count = adjustedCount;
                } else {
                    // Check the active schedule for the school.
                    SisSchool school = snapshot.getSchool();
                    Schedule sched = null;
                    if (school != null) {
                        sched = school.getActiveSchedule();
                        if (sched == null) {
                            entity.addRetrievalError(field.getFieldId(),
                                    new StateReportValidationError(entity, field,
                                            "No active schedule: " + school.getName(), school.getName()));
                        } else {
                            try {
                                count = String.valueOf(
                                        getMembershipTotal(
                                                student,
                                                getCalendarDays(snapshot.getSchool(), student.getCalendarCode()),
                                                m_firstDayMembers.contains(student.getOid()),
                                                m_firstDayDate,
                                                m_reportDate,
                                                null));
                            } catch (Exception e) {
                                entity.addRetrievalError(field.getFieldId(),
                                        new StateReportValidationError(entity, field,
                                                "Could not calculate membership: exception", e.getMessage()));
                            }
                        }
                    }
                }
            } else {
                count = m_integerConverter.javaToString(WebUtils.getProperty(student, field.getBeanPath()));
            }

            return count;
        }

        /**
         * *****************************************************************
         *
         * CODE IS TAKEN FROM ENROLLMENT MANAGER
         * SMALL CHANGE TO EXCLUDE PREREG
         *
         * *****************************************************************.
         *
         * @param student SisStudent
         * @param sessionDays Set
         * @param isInitiallyMember boolean
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param school SisSchool
         * @return int
         */

        /**
         * Returns the total number of membership days for the given student from the start date to
         * the
         * end date, inclusive. Entry and withdrawal days are counted towards the total according to
         * the
         * values of the EntryIsMemberDay and WithdrawalIsMemberDay properties.
         *
         * @param student the student for which to calculate membership
         * @param sessionDays the set of PlainDate objects that defines the in-session days
         *        for the current student's school and calendar
         * @param isInitiallyMember if true then the student is initially considered a member
         * @param startDate the start of the date range, inclusive
         * @param endDate the end of the date range, inclusive
         * @param school if not null then membership will be considered for just the given
         *        school, otherwise membership will be considered for the entire
         *        district
         *
         * @return int
         */
        public int getMembershipTotal(SisStudent student,
                                      Set sessionDays,
                                      boolean isInitiallyMember,
                                      PlainDate startDate,
                                      PlainDate endDate,
                                      SisSchool school) {
            int totalMembership = 0;

            if (student != null && sessionDays != null && startDate != null && endDate != null) {
                List<StudentEnrollment> enrollments =
                        getFilteredStudentEnrollments(student, startDate, endDate, school, true);// getOrderedEnrollment(student,
                                                                                                 // startDate,
                                                                                                 // endDate,
                                                                                                 // school,
                                                                                                 // true);

                Calendar calendar = Calendar.getInstance(Locale.getDefault());
                calendar.setTime(startDate);
                boolean isMemberToday = isInitiallyMember;
                while (!calendar.getTime().after(endDate)) {
                    boolean isMemberTomorrow = isMemberToday;

                    if (!enrollments.isEmpty()) {
                        StudentEnrollment topEnrollmentRecord = enrollments.get(0);
                        while (topEnrollmentRecord.getEnrollmentDate().equals(calendar.getTime())) {
                            if (topEnrollmentRecord.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                                isMemberToday = m_enrollmentManager.getWithdrawalIsMemberDay();
                                isMemberTomorrow = false;
                            } else {
                                isMemberToday = m_enrollmentManager.getEntryIsMemberDay();
                                isMemberTomorrow = true;
                            }

                            enrollments.remove(0);

                            if (enrollments.isEmpty()) {
                                break;
                            }

                            topEnrollmentRecord = enrollments.get(0);
                        }
                    }

                    if (sessionDays.contains(calendar.getTime())) {
                        if (isMemberToday) {
                            totalMembership++;
                        }
                    }

                    calendar.add(Calendar.DATE, 1);
                    isMemberToday = isMemberTomorrow;
                }
            }

            return totalMembership;
        }

        /**
         * Gets the filtered student enrollments.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @param school SisSchool
         * @param ascending boolean
         * @return List
         */
        private List<StudentEnrollment> getFilteredStudentEnrollments(SisStudent student,
                                                                      PlainDate startDate,
                                                                      PlainDate endDate,
                                                                      SisSchool school,
                                                                      boolean ascending) {
            ArrayList typeCodes = new ArrayList(2);
            typeCodes.add(StudentEnrollment.ENTRY);
            typeCodes.add(StudentEnrollment.WITHDRAWAL);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(StudentEnrollment.COL_STUDENT_OID, student.getOid());
            criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);

            if (startDate != null) {
                criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
            }

            if (endDate != null) {
                criteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
            }

            if (school != null) {
                criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, school.getOid());
            }

            criteria.addNotEqualTo(StudentEnrollment.COL_STATUS_CODE, "PreReg");

            QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
            query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, ascending);
            query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, ascending);

            ArrayList enrollments = new ArrayList();
            enrollments.addAll(getBroker().getCollectionByQuery(query));

            return enrollments;
        }

        /**
         * Returns the days-in-session for the given school and calendar combination.
         *
         * @param school SisSchool
         * @param calendar String
         * @return Set of PlainDate objects
         */
        private Set getCalendarDays(SisSchool school, String calendar) {
            if (!m_schoolsToCalendars.containsKey(school.getOid())) {
                PlainDate startDate = school.getActiveSchedule().getStartDate();
                Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, m_reportDate);
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            }

            return (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);
        }
    }

    /**
     * Retriever class for looking up student middle name.
     * This lookup checks the NMN (no middle name) indicator
     * and substitutes "NMN" if necessary.
     *
     * The NMN field bean path must be stored in "Other" in the field definition.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMiddleName implements FieldRetriever {

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
            String middleName = null;
            // No Middle Name field def stored in "Other" by field definition.
            String doeNoMiddleName = (String) field.getParameter();

            if (doeNoMiddleName != null) {
                String noMiddleName = (String) WebUtils.getProperty(entity.getBean(), doeNoMiddleName);

                if (BooleanAsStringConverter.TRUE.equals(noMiddleName)) {
                    middleName = "NMN";
                } else {
                    middleName = (String) getProperty(entity.getBean(), field.getBeanPath());
                    if (StringUtils.isEmpty(middleName)) {
                        middleName = "NMN";
                    }
                }
            } else {
                middleName = (String) getProperty(entity.getBean(), field.getBeanPath());
                if (StringUtils.isEmpty(middleName)) {
                    middleName = "NMN";
                }
            }

            return middleName;
        }
    }

    /**
     * Returns the DOE race code for a student.
     * This implementation looks up all race codes for a student and constructs the
     * bitmap race code used by the state.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String raceCode = "";
            SisStudent student = (SisStudent) entity.getBean();

            if (m_calculateTotals) {
                /*
                 * Calculate the local (X2) bitmap value and then translate it to the corresponding
                 * DOE
                 * code.
                 */
                int stateRaceCode = 0;
                Collection<Race> raceBeans = m_raceCodeMap.get(student.getPersonOid());
                if (!CollectionUtils.isEmpty(raceBeans)) {
                    int localRaceCode = 0;

                    for (Race raceBean : raceBeans) {
                        String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceBean.getRaceCode());
                        if (StringUtils.isInteger(stateCode)) {
                            localRaceCode += Integer.parseInt(stateCode);
                        }
                    }

                    if (localRaceCode >= 0 && localRaceCode <= STATE_RACE_CODES.length) {
                        stateRaceCode = STATE_RACE_CODES[localRaceCode];
                    }

                    if (student.getPerson().getHispanicLatinoIndicator()) {
                        stateRaceCode += 32;
                    }

                    raceCode = StringUtils.padLeft(String.valueOf(stateRaceCode), 2, '0');
                }
            } else {
                raceCode = (String) WebUtils.getProperty(student, field.getBeanPath());
            }

            return raceCode;
        }
    }

    /**
     * Returns the school code for the given student. This method considers the adjusted school code
     * value.
     * <p>
     * For non-archived students we use the snapshot. For archived students (i.e., summer
     * withdrawals) we use the school from the most recent enrollment record during summer vacation
     * (if we didn't do this then the export would show the history school as the student's school).
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
            EnrollmentSnapshot snapshot = ((SIMSEntity) entity).getSnapshot(m_reportDate, field);
            SisStudent student = (SisStudent) entity.getBean();
            String schoolCode = null;

            String adjustedSchoolCode = (String) WebUtils.getProperty(student, m_adjustedSchoolCode);
            if (StringUtils.isEmpty(adjustedSchoolCode)) {
                if (student.getSchool().getArchiveIndicator()) {
                    List enrollments = m_enrollmentManager.getOrderedEnrollment(student,
                            (PlainDate) getParameter(SUMMER_START_DATE_PARAM),
                            (PlainDate) getParameter(SUMMER_END_DATE_PARAM),
                            null,
                            false);
                    if (!enrollments.isEmpty()) {
                        StudentEnrollment withdrawal = (StudentEnrollment) enrollments.get(0);
                        schoolCode = (String) WebUtils.getProperty(withdrawal.getSchool(), m_doe15Alias);
                    }
                } else {
                    String schoolBeanPath = field.getBeanPath().substring(field.getBeanPath().indexOf('.') + 1);
                    schoolCode = (String) WebUtils.getProperty(snapshot.getSchool(), schoolBeanPath);
                }
            } else {
                schoolCode = adjustedSchoolCode;
            }

            return schoolCode;
        }
    }

    /**
     * Returns the sped primary disability value. This value is taken from the passed IEP record if
     * the "retrieve sped values" input parameter was selected. Otherwise the value currently on the
     * student record is returned.
     * <p>
     * The student's disability is retrieved from the active IEP if they are an active special
     * education student, or if they exited within the past year. If the student has a primary
     * disability, it is used. Otherwise, the first disability among the student's disabilities
     * is taken.
     */
    protected class RetrieveSpedDisability implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String disability = data.lookupUserValue(SisStudent.class, field.getBeanPath(), "500");

            if (m_retrieveSpedValues) {
                String iepOid = null;
                if (SpedStatusCode.ACTIVE.equals(student.getSpedStatusCodeEnum())) {
                    iepOid = (String) m_iepLookup.getIepValue(student.getOid(), IepData.StatusCode.ACTIVE,
                            X2BaseBean.COL_OID);
                } else if (isSpedExitedThisYear(student)) {
                    iepOid = (String) m_iepLookup.getIepValue(student.getOid(), IepData.StatusCode.PREVIOUS,
                            X2BaseBean.COL_OID);
                }

                if (iepOid != null) {
                    Collection<IepDisability> disabilities = m_iepDisabilityLookup.get(iepOid);

                    if (!CollectionUtils.isEmpty(disabilities)) {
                        disability = disabilities.iterator().next().getDisabilityCode();
                    }
                }
            } else {
                disability = (String) WebUtils.getProperty(student, field.getBeanPath());
            }

            /*
             * Save the unmapped value in the entity so it can be written
             * back to the student record in postProcess.
             */
            ((SIMSEntity) entity).setUpdateValue(field.getFieldId(), disability);

            return disability;
        }
    }

    /**
     * Returns the sped evaluation results value. If the "retrieve sped values" input parameter
     * was selected, the following logic is used to derive the return value:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Returned state code equivalent</th>
     * <th>Circumstance</th>
     * </tr>
     * <tr>
     * <td>00</td>
     * <td>Special education status is empty</td>
     * </tr>
     * <tr>
     * <td>01</td>
     * <td>Special education status is active, active IEP is a review</td>
     * </tr>
     * <tr>
     * <td>02</td>
     * <td>Special education status is ineligible, newest IEP is an initial, exit
     * date is within the current school year</td>
     * </tr>
     * <tr>
     * <td>03</td>
     * <td>Special education status is ineligible, newest IEP is a re-evaluation,
     * exit date is within the current school year</td>
     * </tr>
     * <tr>
     * <td>04</td>
     * <td>Special education status is active, active IEP is an initial, IEP
     * designates student for specially designed instruction</td>
     * </tr>
     * <tr>
     * <td>05</td>
     * <td>Special education status is active, active IEP is an initial, IEP does not
     * designate student for specially designed instruction</td>
     * </tr>
     * <tr>
     * <td>06</td>
     * <td>Special education status is active, active IEP is a re-evaluation, IEP
     * designates student for specially designed instruction</td>
     * </tr>
     * <tr>
     * <td>07</td>
     * <td>Special education status is active, active IEP is a re-evaluation, IEP
     * does not designate student for specially designed instruction</td>
     * </tr>
     * <tr>
     * <td>08</td>
     * <td>Special education status is referred</td>
     * </tr>
     * </table>
     * </p>
     * Note that codes 01, 04, 05, 06, and 07 also apply to students with an "Exited" special
     * education status. The exited status indicates that the student withdrew from the district.
     * The student, therefore, is still active on an IEP, but in another district. In this case we
     * examine the IEP at the time of the exit to determine the evaluation results.
     * <p>
     * If the "retrieve sped values" is not selected, the value currently on the student record is
     * returned.
     *
     * @return String
     */
    protected class RetrieveSpedEvaluation implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String evaluationResults = null;

            if (m_retrieveSpedValues) {
                String stateCode = "00";

                IepData.StatusCode iepStatusCode = null;
                if (student.getSpedStatusCodeEnum() == ACTIVE) {
                    iepStatusCode = IepData.StatusCode.ACTIVE;
                } else if (student.getSpedStatusCodeEnum() == EXITED && isSpedExitedThisYear(student)) {
                    iepStatusCode = IepData.StatusCode.PREVIOUS;
                }

                if (iepStatusCode != null) {
                    if (m_iepLookup.hasIep(student.getOid(), iepStatusCode)) {
                        /*
                         * Check the signed date of the IEP. If it happened in a previous school
                         * year, the stateCode should be reported as '01' because the special status
                         * would have been reported in a previous send. If the signed date happened
                         * this school year, use the correct special code depended upon the student
                         * IEP's last meeting type.
                         */
                        Calendar calendar = Calendar.getInstance();

                        int schoolYear = student.getOrganization1().getCurrentContext().getSchoolYear() - 1;

                        calendar.set(Calendar.YEAR, schoolYear);
                        calendar.set(Calendar.MONTH, Calendar.JULY);
                        calendar.set(Calendar.DAY_OF_MONTH, 1);

                        PlainDate cutoffDate = new PlainDate(calendar.getTime());
                        Object date = m_iepLookup.getIepValue(student.getOid(), iepStatusCode, IepData.COL_SIGNED_DATE);

                        if (date == null || new PlainDate((Date) date).after(cutoffDate)) {
                            Object meetingTypeCodeObj = m_iepLookup.getIepValue(student.getOid(), iepStatusCode,
                                    IepData.COL_MEETING_TYPE_CODE);
                            if (meetingTypeCodeObj != null) {
                                int meetingTypeCode = Integer.parseInt(meetingTypeCodeObj.toString());
                                switch (IepMeeting.TypeCode.values()[meetingTypeCode]) {
                                    case REEVAL:
                                        if (isSpedSpecialInstruction(student.getOid())) {
                                            stateCode = "06";
                                        } else {
                                            stateCode = "07";
                                        }
                                        break;

                                    case INITIAL:
                                        if (isSpedSpecialInstruction(student.getOid())) {
                                            stateCode = "04";
                                        } else {
                                            stateCode = "05";
                                        }
                                        break;

                                    case AMENDMENT:
                                    case OTHER:
                                    case REVIEW:
                                        stateCode = "01";
                                        break;

                                    default:
                                        break;
                                }
                            }
                        } else {
                            stateCode = "01";
                        }
                    }
                } else if (student.getSpedStatusCodeEnum() == INELIGIBLE && isSpedExitedThisYear(student)) {
                    if (m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.DISCARDED)) {
                        int meetingTypeCode = Integer.parseInt(m_iepLookup.getIepValue(student.getOid(),
                                IepData.StatusCode.DISCARDED, IepData.COL_MEETING_TYPE_CODE).toString());
                        switch (IepMeeting.TypeCode.values()[meetingTypeCode]) {
                            case REEVAL:
                                stateCode = "03";
                                break;

                            case INITIAL:
                                stateCode = "02";
                                break;

                            default:
                                entity.addRetrievalError(field.getFieldId(),
                                        new StateReportValidationError(entity, field,
                                                "Meeting type must be Initial Referral or Re-evaluation for ineligible students.",
                                                IepMeeting.TypeCode.values()[meetingTypeCode].toString()));
                                break;
                        }
                    } else {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field,
                                        "Student is ineligible and exited program this year but does not have a discarded IEP",
                                        ""));
                    }
                } else if (student.getSpedStatusCodeEnum() == REFERRED) {
                    stateCode = "08";

                    /*
                     * If the student's status is referred and their most recent IEP is REJECTED,
                     * then
                     * use code 09 -
                     * "Student evaluated and found eligible for services but parent/guardian declined."
                     */
                    if (m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.REJECTED) &&
                            !m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.DRAFT)) {
                        stateCode = "09";
                    }
                }

                evaluationResults = data.lookupUserValue(SisStudent.class, field.getBeanPath(), stateCode);
            } else {
                evaluationResults = (String) WebUtils.getProperty(student, field.getBeanPath());
            }

            /*
             * Save the unmapped value in the entity so it can be written
             * back to the student record in postProcess.
             */
            ((SIMSEntity) entity).setUpdateValue(field.getFieldId(), evaluationResults);

            return evaluationResults;
        }

        /**
         * Returns true if the passed IEP designates the student for specially designed instruction.
         *
         * @param studentOid String
         * @return boolean
         */
        protected boolean isSpedSpecialInstruction(String studentOid) {
            String plepASpecialContent =
                    (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                            IEP_PLEPA_SPECIAL_INSTRUCTION_CONTENT);
            String plepASpecialMethodology =
                    (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                            IEP_PLEPA_SPECIAL_INSTRUCTION_METHODOLOGY);
            String plepASpecialPerformance =
                    (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                            IEP_PLEPA_SPECIAL_INSTRUCTION_PERFORMANCE);
            String plepBSpecialContent =
                    (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                            IEP_PLEPB_SPECIAL_INSTRUCTION_CONTENT);
            String plepBSpecialMethodology =
                    (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                            IEP_PLEPB_SPECIAL_INSTRUCTION_METHODOLOGY);
            String plepBSpecialPerformance =
                    (String) m_iepLookup.getIepValueByAlias(studentOid, IepData.StatusCode.ACTIVE,
                            IEP_PLEPB_SPECIAL_INSTRUCTION_PERFORMANCE);

            return !StringUtils.isEmpty(plepASpecialContent) ||
                    !StringUtils.isEmpty(plepASpecialMethodology) ||
                    !StringUtils.isEmpty(plepASpecialPerformance) ||
                    !StringUtils.isEmpty(plepBSpecialContent) ||
                    !StringUtils.isEmpty(plepBSpecialMethodology) ||
                    !StringUtils.isEmpty(plepBSpecialPerformance);
        }
    }

    /**
     * Returns the sped level of need value. This value is taken from the passed IEP record if the
     * "retrieve sped values" input parameter was selected. Otherwise the value currently on the
     * student record is returned.
     * <p>
     * The level of need is retrieved from the active IEP if they are an active special education
     * student, or if they exited within the past year.
     *
     */
    protected class RetrieveSpedLevel implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String level = data.lookupUserValue(SisStudent.class, field.getBeanPath(), "500");

            if (m_retrieveSpedValues) {
                boolean active = SpedStatusCode.ACTIVE.equals(student.getSpedStatusCodeEnum());

                if (active) {
                    level = (String) m_iepLookup.getIepValueByAlias(student.getOid(), IepData.StatusCode.ACTIVE,
                            IEP_LEVEL_OF_NEED);
                } else if (isSpedExitedThisYear(student)) {
                    level = (String) m_iepLookup.getIepValueByAlias(student.getOid(), IepData.StatusCode.PREVIOUS,
                            IEP_LEVEL_OF_NEED);
                }
            } else {
                level = (String) WebUtils.getProperty(student, field.getBeanPath());
            }

            /*
             * Save the unmapped value in the entity so it can be written
             * back to the student record in postProcess.
             */
            ((SIMSEntity) entity).setUpdateValue(field.getFieldId(), level);

            return level;
        }
    }

    /**
     * Returns the sped placement info value. If the "retrieve sped values" input parameter
     * was selected, the following logic is used to derive the return value:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Returned state code equivalent</th>
     * <th>Circumstance</th>
     * </tr>
     * <tr>
     * <td>00</td>
     * <td>Special education status is not active and student did not exit special education during
     * the current school year</td>
     * </tr>
     * <tr>
     * <td>00</td>
     * <td>The field being processed is DOE32 and the student is more than 5 years old</td>
     * </tr>
     * <tr>
     * <td>00</td>
     * <td>The field being processed is DOE34 and the student is 5 years old or younger</td>
     * </tr>
     * <tr>
     * <td>00</td>
     * <td>Special education status is not active and student did not exit special education during
     * the current school year</td>
     * </tr>
     * <tr>
     * <td>01</td>
     * <td>Special education status is ineligible; student was found ineligible during the current
     * year; student DOE32 value is NOT currently set to role model (state code 05)</td>
     * </tr>
     * <tr>
     * <td>05</td>
     * <td>Special education status is not active and student did not exit special education during
     * the current school year; student DOE32 value is currently set to role model (state code 05)
     * </td>
     * </tr>
     * <tr>
     * <td>(Value from IEP)</td>
     * <td>Special education status is active</td>
     * </tr>
     * <tr>
     * <td>(Value from IEP)</td>
     * <td>Special education status is exited; student exited during the current year</td>
     * </tr>
     * </table>
     *
     */
    protected class RetrieveSpedPlacement implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String placementInfo = null;

            String iepAlias;
            String studentJavaName = field.getBeanPath();
            if (DOE_34_SPED_PLACEMENT.equals(field.getFieldId())) {
                iepAlias = IEP_EDUCATIONAL_ENVIRONMENT;
            } else {
                iepAlias = IEP_EDUCATIONAL_ENVIRONMENT_EC;
            }

            /*
             * If the district uses the SPED Module.
             * This is set in the Input Definition, defaulted to false and hidden.
             */
            if (m_retrieveSpedValues) {
                boolean notSpecialEd = false;

                int ageAsOf = student.getPerson().getAgeAsOfDate(m_reportDate);

                if (ageAsOf == 0) {
                    entity.addRetrievalError(field.getFieldId(),
                            new StateReportValidationError(entity, field,
                                    "Unable to calculate sped placement (DOE32/DOE34).",
                                    "The student does not have a valid date of birth."));
                    notSpecialEd = true;
                } else if (ageAsOf < 6 && DOE_34_SPED_PLACEMENT.equals(field.getFieldId()) ||
                        ageAsOf >= 6 && DOE_32_SPED_PLACEMENT.equals(field.getFieldId())) {
                    /*
                     * DOE34 is for students age 6+; when processing DOE34, students younger than 6
                     * are considered not special ed for that field.
                     *
                     * Similarly, DOE32 is for students 5 and under. When processing DOE32, students
                     * older than 5 are considered not special ed for that field.
                     */
                    placementInfo = data.lookupUserValue(SisStudent.class, studentJavaName, "00");
                    notSpecialEd = true;
                } else {
                    switch (student.getSpedStatusCodeEnum()) {
                        case ACTIVE:
                            placementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(),
                                    IepData.StatusCode.ACTIVE, iepAlias);
                            break;

                        case EXITED:
                            if (isSpedExitedThisYear(student)) {
                                if (student.getSpedExitDate().after(m_reportDate)) {
                                    // If the student exited AFTER report date, display what would
                                    // have been their DOE32/34 status prior to exiting.
                                    placementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(),
                                            IepData.StatusCode.PREVIOUS, iepAlias);
                                } else {
                                    // If the student exited on or before report date, report the
                                    // "exited this year" code "01".
                                    placementInfo = data.lookupUserValue(SisStudent.class, studentJavaName, "01");
                                }
                            } else {
                                notSpecialEd = true;
                            }
                            break;

                        case INELIGIBLE:
                            placementInfo = data.lookupUserValue(SisStudent.class, studentJavaName, "00");
                            notSpecialEd = true;

                            if (isSpedExitedThisYear(student)) {
                                String iepPlacementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(),
                                        IepData.StatusCode.PREVIOUS, iepAlias);
                                if (!StringUtils.isEmpty(iepPlacementInfo)) {
                                    if (student.getSpedExitDate().after(m_reportDate)) {
                                        // The student was deemed in-eligible this year after report
                                        // date.
                                        // Report what the Previous IEP had for placement.
                                        placementInfo = iepPlacementInfo;
                                    } else {
                                        // The student was deemed in-eligible this year before
                                        // report date.
                                        // If they were not sped before, they are still not sped.
                                        // If they were placed in sped in any way before, they are
                                        // now Exited.
                                        String currentPlacementCode = data.lookupStateValue(SisStudent.class,
                                                studentJavaName, iepPlacementInfo);
                                        if (!"00".equals(currentPlacementCode)) {
                                            placementInfo =
                                                    data.lookupUserValue(SisStudent.class, studentJavaName, "01");
                                            notSpecialEd = false;
                                        }
                                    }
                                }
                            }
                            break;

                        default:
                            placementInfo = data.lookupUserValue(SisStudent.class, studentJavaName, "00");
                            notSpecialEd = true;
                    }
                }

                /*
                 * The special case below handles the "role model" code, which is a value in DOE32
                 * for
                 * regular education students. If the student is not a special education student,
                 * and we
                 * are processing DOE32, maintain code values of 05 for role models.
                 */
                if (notSpecialEd && DOE_32_SPED_PLACEMENT.equals(field.getFieldId())) {
                    String roleModelCode = data.lookupUserValue(SisStudent.class, studentJavaName, "05");
                    if (roleModelCode != null) {
                        String currentCode = (String) WebUtils.getProperty(student, studentJavaName);
                        if (roleModelCode.equals(currentCode)) {
                            placementInfo = roleModelCode;
                        }
                    }
                }

                /* If nothing else is caught then pull it form the student */
                if (placementInfo == null) {
                    placementInfo = (String) WebUtils.getProperty(student, studentJavaName);
                }
            }
            /*
             * If the student has withdrawn from the district in the last school year
             * then set the DOE 32 Sped Placement to their previous IEP status else set it to "10"
             */
            else if (EXITED.equals(student.getSpedStatusCodeEnum())
                    && isSpedExitedThisYear(student)
                    && m_iepDataTransferStudents != null
                    && m_iepDataTransferStudents.containsKey(student.getOid())) {
                if (DOE_32_SPED_PLACEMENT.equals(field.getFieldId())
                        || DOE_34_SPED_PLACEMENT.equals(field.getFieldId())) {
                    // Get student's last Iep, is sorted descending
                    LinkedList<IepData> iepDatas =
                            (LinkedList<IepData>) m_iepDataTransferStudents.get(student.getOid());
                    IepData lastIepData = iepDatas.get(0);

                    String iepFieldJaveName = null;
                    if (DOE_32_SPED_PLACEMENT.equals(field.getFieldId())) {
                        iepFieldJaveName = m_iepEdEnviroment3_5FieldName;
                    } else {
                        iepFieldJaveName = m_iepEdEnviroment6_21FieldName;
                    }

                    String iepPlacementInfo = (String) lastIepData.getFieldValueByBeanPath(iepFieldJaveName);

                    placementInfo = iepPlacementInfo;
                } else {
                    placementInfo = (String) WebUtils.getProperty(student, studentJavaName);
                }
            } else if (DOE_32_SPED_PLACEMENT.equals(field.getFieldId())) {
                if (EXITED.equals(student.getSpedStatusCodeEnum())
                        && isSpedExitedThisYear(student)) {
                    // Pull from the Student DOE32 or DOE34 field.
                    placementInfo = (String) WebUtils.getProperty(student, studentJavaName);

                    // If the student has a IEP record then use the previous.
                    if (m_iepLookup != null && m_iepLookup.hasIep(student.getOid(), IepData.StatusCode.PREVIOUS)) {
                        String iepPlacementInfo = (String) m_iepLookup.getIepValueByAlias(student.getOid(),
                                IepData.StatusCode.PREVIOUS, iepAlias);
                        if (!StringUtils.isEmpty(iepPlacementInfo)) {
                            placementInfo = iepPlacementInfo;
                        }
                    }
                } else {
                    // Pull from the Student DOE32 or DOE34 field.
                    placementInfo = (String) WebUtils.getProperty(student, studentJavaName);
                }
            } else {
                // Pull from the Student DOE32 or DOE34 field.
                placementInfo = (String) WebUtils.getProperty(student, studentJavaName);
            }

            /*
             * Save the unmapped value in the entity so it can be written
             * back to the student record in postProcess.
             */
            ((SIMSEntity) entity).setUpdateValue(field.getFieldId(), placementInfo);

            return placementInfo;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
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
            value = Normalizer.normalize(value, Normalizer.Form.NFD);
            value = m_diacriticalMarks.matcher(value).replaceAll("");
            Matcher matcher = m_illegalNameCharacters.matcher(value);
            return matcher.replaceAll("");
        }
    }

    /**
     * Returns the number of in-school suspensions for the given student from the start of school to
     * the report date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSuspensionsIn implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String count = null;

            if (m_calculateTotals) {
                Integer trcCount = m_suspensionsIn.get(student.getOid());
                if (trcCount != null) {
                    count = trcCount.toString();
                } else {
                    count = "0";
                }
            } else {
                count = m_integerConverter.javaToString(WebUtils.getProperty(student, field.getBeanPath()));
            }

            return count;
        }
    }

    /**
     * Returns the DOE 30 state code for a student.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTitleTargetedAssist implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String code = "";

            String ttaCode = (String) getProperty(entity.getBean(), field.getBeanPath());

            if (!StringUtils.isEmpty(ttaCode)) {
                code = lookupStateValue(SisStudent.class, field.getBeanPath(), ttaCode);
            }

            return code;
        }
    }

    /**
     * Returns the number of out-of-school suspensions for the given student from the start of
     * school to the report date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSuspensionsOut implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String count = null;

            if (m_calculateTotals) {
                Integer trcCount = m_suspensionsOut.get(student.getOid());
                if (trcCount != null) {
                    count = trcCount.toString();
                } else {
                    count = "0";
                }
            } else {
                count = m_integerConverter.javaToString(WebUtils.getProperty(student, field.getBeanPath()));
            }

            return count;
        }
    }

    /**
     * Returns the town of residence code for the given student. This method checks if the student
     * lives out of state in which case a special code is returned.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTownOfResidence implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String townOfResidence = null;

            String adjustedCity = (String) student.getFieldValueByAlias(ADJUSTED_RESIDENCE_CITY_FIELD);
            if (!StringUtils.isEmpty(adjustedCity)) {
                townOfResidence = lookupStateValue(SisAddress.class, SisAddress.COL_CITY, adjustedCity);
            } else {
                String state = (String) WebUtils.getProperty(student,
                        SisStudent.REL_PERSON + "." + SisPerson.REL_PHYSICAL_ADDRESS + "." + SisAddress.COL_STATE);
                if (ObjectUtils.match(state, MA_STATE_VALUE)) {
                    townOfResidence = lookupStateValue(SisAddress.class, SisAddress.COL_CITY,
                            (String) getProperty(student, field.getBeanPath()));
                } else {
                    townOfResidence = OUT_OF_STATE_CODE;
                }
            }

            return townOfResidence;
        }
    }

    /**
     * Returns the number of truancies for the given student from the start of school to the report
     * date.
     */
    protected class RetrieveTruancies implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String count = null;

            if (m_calculateTotals) {
                if (TRUANCT_TYPE_INCIDENT.equals(m_truancyType)) {
                    // Old way, count truancy tpye conduct incidents.
                    Integer trcCount = m_truancies.get(student.getOid());
                    if (trcCount != null) {
                        count = trcCount.toString();
                    } else {
                        count = "0";
                    }
                } else {
                    // New way, count unexcused absences.
                    Integer absenceCount = m_absencesUnex.get(student.getOid());
                    if (absenceCount != null) {
                        count = absenceCount.toString();
                    } else {
                        count = "0";
                    }
                }
            } else {
                count = m_integerConverter.javaToString(WebUtils.getProperty(student, field.getBeanPath()));
            }

            return count;
        }
    }

    /**
     * Validate alternate education courses are in grade 6-12 and SP.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateAlternateEducation implements FieldValidator {

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
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);
            if (DOE16 != null && !DOE16.matches("0[5-9]|1[012]|SP") && !"00000000".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Grade DOE16 invalid for Alt Ed course DOE27. Requires " + STYLE_BOLD + "5-12, SP" + STYLE_END,
                        "DOE16=" + STYLE_BOLD + DOE16 + STYLE_END + ", DOE27=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate AP courses are only in students in grades 9-12.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateApCourse implements FieldValidator {

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
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);
            if (DOE16 != null && !DOE16.matches("09|1[012]") && !DEFAULT_AP_COURSE_CODE.equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Grade DOE16 invalid for AP course " + field.getFieldId() + ". Requires " + STYLE_BOLD + "9-12"
                                + STYLE_END + ".",
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END + ", DOE16=" + STYLE_BOLD + DOE16
                                + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate student attendance days, compare to membership days and truancy days.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateAttendanceDays implements FieldValidator {

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
            String DOE18 = entity.getFieldValue(DOE_18_MEMBERSHIP_DAYS);
            String DOE52 = entity.getFieldValue(DOE_52_TRUANCIES);
            if (ZERO_DAYS_OVERRIDE.equals(DOE18)) {
                if (!ZERO_DAYS_OVERRIDE.equals(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Membership/Attendance days mismatch, must be both or neither " + STYLE_BOLD + "555"
                                    + STYLE_END,
                            "DOE18=" + STYLE_BOLD + ", DOE17=" + STYLE_BOLD + value + STYLE_END));
                }
            } else {
                int DOE17int = 0;
                int DOE18int = 0;
                int DOE52int = 0;
                try {
                    DOE17int = (value == null ? 0 : Integer.parseInt(value));
                    DOE18int = (DOE18 == null ? 0 : Integer.parseInt(DOE18));
                    DOE52int = (DOE52 == null ? 0 : Integer.parseInt(DOE52));
                } catch (NumberFormatException nfe) {
                    // invalid format, will be reported elsewhere.
                }
                if (DOE17int + DOE52int > DOE18int) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Membership days DOE18 mismatch",
                            "Attendance DOE17 " + STYLE_BOLD + value + STYLE_END + " plus truant DOE52 " +
                                    STYLE_BOLD + DOE52 + STYLE_END + " geater than membership DOE18 " +
                                    STYLE_BOLD + DOE18 + STYLE_END));
                }
                if (DOE17int < 0 || DOE17int > 261) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Attendance DOE17 must be between " + STYLE_BOLD + "0-261" + STYLE_END,
                            "DOE17=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validates birthdate of student.
     * Student must be between the age of 3 and 21 if enrolled.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateBirthDate implements FieldValidator {

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
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            Date bday = null;
            try {
                bday = m_dateFormat.parse(value);
            } catch (ParseException e) {
                // Fails to parse. Error.
                errors.add(new StateReportValidationError(entity, field,
                        "Date of birth DOE06 format error", "DOE06=" + STYLE_BOLD + value + STYLE_END));
            }
            if ("01".equals(DOE12) || "40".equals(DOE12)) {
                if (bday != null && (bday.after(m_reportDateMax) || bday.before(m_reportDateMin))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Date of birth DOE06 out of range (" + STYLE_BOLD + "3-21" + STYLE_END + " years)",
                            "DOE06=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate country of origin.
     * Must have immigration status to have country of origin.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCountryOfOrigin implements FieldValidator {

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
            String DOE22 = entity.getFieldValue(DOE_22_IMMIGRATION_STATUS);
            if (("00".equals(DOE22) && !"500".equals(value)) ||
                    (!"00".equals(DOE22) && "500".equals(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Country of origin DOE23 invalid for Immigration status DOE22",
                        "DOE23=" + STYLE_BOLD + value + STYLE_END + ", DOE22=" + STYLE_BOLD + DOE22 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate C/TE student population type.
     * compares to C/TE program.
     */
    protected class ValidateCtePopulation implements FieldValidator {

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
            String DOE35 = entity.getFieldValue(DOE_35_CTE_PROGRAM);
            if ("00".equals(DOE35) && !"500".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "CT/E population DOE42 invalid for CT/E Program DOE35",
                        "DOE42=" + STYLE_BOLD + value + STYLE_END + ", DOE35=" + STYLE_BOLD + DOE35 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate C/TE program.
     * Compare with:
     * enrollment reason,
     * grade level (9-12 only)
     * C/TE 74 program
     * C/TE non-74 program
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCteProgram implements FieldValidator {

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
            String DOE11 = entity.getFieldValue(DOE_11_REASON);
            String DOE13 = entity.getFieldValue(DOE_13_ENROLLMENT_REASON);
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);
            String DOE43 = entity.getFieldValue(DOE_43_CTE_74);
            String DOE44 = entity.getFieldValue(DOE_44_CTE_NON_74);
            if ("02".equals(DOE11) && !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "CT/E Program DOE35 invalid for Reporting reason DOE11",
                        "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE11=" + STYLE_BOLD + DOE11 + STYLE_END));
            }
            if (("13".equals(value) || "14".equals(value)) && "500".equals(DOE44)) {
                errors.add(new StateReportValidationError(entity, field,
                        "CT/E Program DOE35 requires non-chapter 74 program DOE44",
                        "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE44=" + STYLE_BOLD + DOE44 + STYLE_END));
            } else if (!("13".equals(value) || "14".equals(value)) && !"500".equals(DOE44)) {
                errors.add(new StateReportValidationError(entity, field,
                        "CT/E Program DOE35 disallows non-chapter 74 program DOE44",
                        "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE44=" + STYLE_BOLD + DOE44 + STYLE_END));
            }
            if (value != null && value.matches("0[2345]")) {
                if ("500".equals(DOE43)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "CT/E Program DOE35 requires Chapter 74 DOE43",
                            "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE43=" + STYLE_BOLD + DOE43 + STYLE_END));
                }
            } else {
                if ("05".equals(DOE13)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "CT/E Program DOE35 invalid when DOE13 is " + STYLE_BOLD + "05" + STYLE_END,
                            "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE13=" + STYLE_BOLD + DOE13 + STYLE_END));
                }
                if (!"500".equals(DOE43)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "CT/E Program DOE35 disallows Chapter 74 DOE43",
                            "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE43=" + STYLE_BOLD + DOE43 + STYLE_END));
                }
            }
            if (!"00".equals(value)) {
                if (DOE16 != null && !DOE16.matches("09|1[012]")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "CT/E Program DOE35 available only in Grades DOE16 " + STYLE_BOLD + "9-12" + STYLE_END,
                            "DOE35=" + STYLE_BOLD + value + STYLE_END + ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validates english proficiency code
     * Compares with native language.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEnglishProficiency implements FieldValidator {

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
            String DOE21 = entity.getFieldValue(DOE_21_LEP_FIRST_YEAR);
            String DOE24 = entity.getFieldValue(DOE_24_NATIVE_LANGUAGE);

            // check language.
            if ("267".equals(DOE24) && "01".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "English Proficiency DOE25 invalid for Language DOE24 " + STYLE_BOLD + "267" + STYLE_END
                                + " (English)",
                        "DOE25=" + STYLE_BOLD + value + STYLE_END + ", DOE24=" + STYLE_BOLD + DOE24 + STYLE_END));
            }
            // Check years
            if (("00".equals(value) && !"00".equals(DOE21)) ||
                    (!"00".equals(value) && "00".equals(DOE21))) {
                errors.add(new StateReportValidationError(entity, field,
                        "English Proficiency DOE25, LEP DOE21 must both be " + STYLE_BOLD + "00" + STYLE_END
                                + " or non-" +
                                STYLE_BOLD + "00" + STYLE_END + " together.",
                        "DOE25=" + STYLE_BOLD + value + STYLE_END + ", DOE21=" + STYLE_BOLD + DOE21 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate english learners program enrollment. Must have LEP code.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEnglishLearners implements FieldValidator {

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
            String DOE25 = entity.getFieldValue(DOE_25_ENGLISH_PROFICIENCY);
            if (!"01".equals(DOE25) && !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "English Learners DOE26 invalid for Proficiency DOE25",
                        "DOE26=" + STYLE_BOLD + value + STYLE_END + ", DOE25=" + STYLE_BOLD + DOE25 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Class validates the enrollment reason
     * compares to enrollment reason for reporting (DOE 11).
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEnrollmentReason implements FieldValidator {

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
            String DOE11 = entity.getFieldValue(DOE_11_REASON);
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            String DOE14 = entity.getFieldValue(DOE_14_TOWN_OF_RESIDENCE);
            String DOE15 = entity.getFieldValue(DOE_15_SCHOOL);
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);
            String DOE22 = entity.getFieldValue(DOE_22_IMMIGRATION_STATUS);
            int DOE11val = 0;
            try {
                DOE11val = Integer.parseInt(DOE11);
            } catch (NumberFormatException nfe) {
                // invalid format, will be reported elsewhere.
            }

            if (DOE11val > 0 && DOE11val < 4) {
                if (!m_enrollmentReasonMatches[DOE11val].contains(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment reason DOE13 invalid for Reporting reason DOE11",
                            "DOE13=" + STYLE_BOLD + value + STYLE_END + ", DOE11=" + STYLE_BOLD + DOE11 + STYLE_END));
                }
            }
            if (DOE12 != null && DOE12.matches("4[01]") && !"01".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment reason DOE13 invalid for Enrollment status DOE12",
                        "DOE13=" + STYLE_BOLD + value + STYLE_END + ", DOE12=" + STYLE_BOLD + DOE12 + STYLE_END));
            }
            if ("11".equals(value) && "01".equals(DOE22)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment reason DOE13 Foreign exchange should not report as Immigrant status DOE22.",
                        "DOE13=" + STYLE_BOLD + value + STYLE_END + ",DOE22=" + STYLE_BOLD + DOE22 + STYLE_END));
            }
            if ("04".equals(value) && DOE14 != null && !DOE14.matches("035|281")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment reason DOE13 not METCO city DOE14",
                        "DOE13=" + STYLE_BOLD + value + STYLE_END + ", DOE14=" + STYLE_BOLD + DOE14 + STYLE_END));
            }
            if ("01".equals(DOE11) && "08".equals(value) && DOE16 != null && !DOE16.matches("PK|KT")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Reporting reason DOE11, Enrollment reason DOE13 not valid for Grade DOE16 (" + STYLE_BOLD
                                + "PK,KT" + STYLE_END + " only)",
                        "DOE11=" + STYLE_BOLD + DOE11 + STYLE_END + ", DOE13=" + STYLE_BOLD + value + STYLE_END
                                + ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END));
            }
            if ("02".equals(DOE11) && "06".equals(value) && DOE15 != null && !DOE15.matches("0950[0-9]*")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Reporting reason DOE11, Enrollment reason DOE13 requires out of state School code DOE15",
                        "DOE11=" + STYLE_BOLD + DOE11 + STYLE_END + ", DOE13=" + STYLE_BOLD + value + STYLE_END
                                + ", DOE15=" + STYLE_BOLD + DOE15 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validates the student enrollment value,
     * compares to enrollment reason and grade.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEnrollmentStatus implements FieldValidator {

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
            String DOE06 = entity.getFieldValue(DOE_06_BIRTH_DATE);
            String DOE11 = entity.getFieldValue(DOE_11_REASON);
            String DOE15 = entity.getFieldValue(DOE_15_SCHOOL);
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);
            String DOE32 = entity.getFieldValue(DOE_32_SPED_PLACEMENT);
            String DOE34 = entity.getFieldValue(DOE_34_SPED_PLACEMENT);
            String DOE38 = entity.getFieldValue(DOE_38_SPED_LEVEL);
            int DOE11val = 0;
            try {
                DOE11val = Integer.parseInt(DOE11);
            } catch (NumberFormatException nfe) {
                // invalid value. Will be reported elsewhere.
            }
            Date bday = ((SIMSEntity) entity).getBirthDate();
            if (DOE11val > 0 && DOE11val < 4) {
                if (!m_enrollmentStatusMatches[DOE11val].contains(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment status DOE12 invalid for Reporting reason DOE11",
                            "DOE11=" + STYLE_BOLD + DOE11 + STYLE_END + ", DOE12=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // check graduation or attainment of certificate grade.
            if (("04".equals(value) || "10".equals(value)) && DOE16 != null && !DOE16.matches("11|12|SP")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment status DOE12 Attainment only valid for Grades " + STYLE_BOLD + "11, 12, SP"
                                + STYLE_END,
                        "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END));
            }
            // check graduation grade.
            if ("11".equals(value) && DOE16 != null && !DOE16.matches("12")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment status DOE12 Graduation only valid for Grade " + STYLE_BOLD + "12" + STYLE_END,
                        "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END));
            }
            // check dropout grade.
            if (value != null && value.matches("3[0123456]") && DOE16 != null && !DOE16.matches("0[6789]|1[012]")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment status DOE12 dropouts only valid for Grades " + STYLE_BOLD + "6-12" + STYLE_END,
                        "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END));
            }
            // Check aged out age.
            if ("09".equals(value)) {
                if (bday != null && bday.after(m_reportDateMin)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment status DOE12 age out must be over 21",
                            "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE06=" + STYLE_BOLD + DOE06 + STYLE_END));
                }
            }
            // check sped enrollment by age.
            if ("40".equals(value)) {
                if (bday != null && bday.after(m_preDateMax) && bday.before(m_reportDateMin)) {
                    if (DOE32 != null && !DOE32.matches("3[0123456789]|4[012345678]")) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Enrollment status DOE12 40 requires SPED Placement DOE32 " + STYLE_BOLD + "30-48"
                                        + STYLE_END,
                                "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE32=" + STYLE_BOLD + DOE32
                                        + STYLE_END));
                    }
                }
                if (bday != null && bday.after(m_reportDateMax) && bday.before(m_preDateMax)) {
                    if (DOE34 != null && !DOE32.matches("[123456789][0123456789]")) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Enrollment status DOE12 " + STYLE_BOLD + "40" + STYLE_END
                                        + " requires SPED Placement DOE34 " + STYLE_BOLD + "10-90" + STYLE_END,
                                "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE34=" + STYLE_BOLD + DOE34
                                        + STYLE_END));
                    }
                }
                // Check school typecode, no DYS, DSS, correctional schools.
                if (DOE15 != null && DOE15.matches("0920[0123456789]*")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment status DOE12 " + STYLE_BOLD + "40" + STYLE_END
                                    + " not valid for DOE15 school type DYS or DSS",
                            "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE15=" + STYLE_BOLD + DOE15 + STYLE_END));
                }
            }
            if ("41".equals(value)) {
                if (bday != null && bday.after(m_preDateMax) && bday.before(m_reportDateMax)) {
                    if (!"01".equals(DOE32)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Enrollment status DOE12 " + STYLE_BOLD + "41" + STYLE_END
                                        + " requires SPED Placement DOE32 " + STYLE_BOLD + "01" + STYLE_END,
                                "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE32=" + STYLE_BOLD + DOE32
                                        + STYLE_END));
                    }
                }
                if (bday != null && bday.after(m_reportDateMin) && bday.before(m_preDateMax)) {
                    if (!"01".equals(DOE34)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Enrollment status DOE12 " + STYLE_BOLD + "41" + STYLE_END
                                        + " requires SPED Placement DOE34 " + STYLE_BOLD + "01" + STYLE_END,
                                "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE34=" + STYLE_BOLD + DOE34
                                        + STYLE_END));
                    }
                }
            }
            // Validate sped level need.
            if ("40".equals(value) || "41".equals(value)) {
                if ("500".equals(DOE38)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment status DOE12 " + STYLE_BOLD + "40,41" + STYLE_END + " requires SPED need DOE38",
                            "DOE12=" + STYLE_BOLD + value + STYLE_END + ", DOE38=" + STYLE_BOLD + DOE38 + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate future plans code.
     * Compares to enrollment status (DOE 12).
     * DOE 12 = 04 requires a future plan.
     * A future plan requires DOE 12 = 04, 10 or 11.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateFuturePlans implements FieldValidator {

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
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            if (("500".equals(value) && "04".equals(DOE12)) ||
                    (!"500".equals(value) && DOE12 != null && !DOE12.matches("04|10|11"))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Future plan DOE33 invalid for Enrollment status DOE12",
                        "DOE33=" + STYLE_BOLD + value + STYLE_END + ", DOE12=" + STYLE_BOLD + DOE12 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate grade level SP with age.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateGradeLevel implements FieldValidator {

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
            String DOE06 = entity.getFieldValue(DOE_06_BIRTH_DATE);
            String DOE34 = entity.getFieldValue(DOE_34_SPED_PLACEMENT);
            Date bday = ((SIMSEntity) entity).getBirthDate();
            if ("SP".equals(value) && bday != null && (bday.before(m_reportDateMin) || bday.after(m_sped2DateMax))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Grade level DOE16 " + STYLE_BOLD + "SP" + STYLE_END + " valid only for age " + STYLE_BOLD
                                + "16-21" + STYLE_END,
                        "DOE16=" + STYLE_BOLD + value + STYLE_END + ", DOE06=" + STYLE_BOLD + DOE06 + STYLE_END));
            }
            if ("SP".equals(value) && "00".equals(DOE34)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Grade level DOE16 " + STYLE_BOLD + "SP" + STYLE_END + " requires SPED Placement DOE34",
                        "DOE16=" + STYLE_BOLD + value + STYLE_END + ", DOE34=" + STYLE_BOLD + DOE34 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Check graduation curriculum. Must be present if DOE12 is 04.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateGraduationCurriculum implements FieldValidator {

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
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            if (("04".equals(DOE12) && "00".equals(value)) ||
                    (!"04".equals(DOE12) && !"00".equals(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Graduation curriculum DOE37 required only for Enrollment status DOE12 " + STYLE_BOLD + "04"
                                + STYLE_END,
                        "DOE37=" + STYLE_BOLD + value + STYLE_END + ", DOE12 " + STYLE_BOLD + DOE12 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate membership days
     * 555 only valid in October report.
     * 1-261 for regular membership.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateMembershipDays implements FieldValidator {

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
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            Calendar cal = Calendar.getInstance();
            cal.setTime(m_reportDate);

            // 555 for summer exit in october report.
            if (DOE12 != null &&
                    DOE12.matches("04|10|11") &&
                    !ZERO_DAYS_OVERRIDE.equals(value) &&
                    cal.get(Calendar.MONTH) == Calendar.OCTOBER) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment status DOE12 Summer exit requires Membership days DOE18 " + STYLE_BOLD + "555"
                                + STYLE_END,
                        "DOE18=" + STYLE_BOLD + value + STYLE_END + ", DOE12=" + STYLE_BOLD + DOE12 + STYLE_END));
            }
            // 555 only for summer exit and dropouts.
            if (ZERO_DAYS_OVERRIDE.equals(value)) {
                if (DOE12 != null &&
                        ((!DOE12.matches("04|10|11|2[01234]|3[0123456]") &&
                                cal.get(Calendar.MONTH) == Calendar.OCTOBER))
                        ||
                        ((!DOE12.matches("2[01234]|3[0123456]") &&
                                cal.get(Calendar.MONTH) != Calendar.OCTOBER))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Only Enrollment status DOE12 Summer exit/dropouts can have Membership days DOE18 "
                                    + STYLE_BOLD + "555" + STYLE_END,
                            "DOE18=" + STYLE_BOLD + value + STYLE_END + ", DOE12=" + STYLE_BOLD + DOE12 + STYLE_END));
                }
            } else {
                int DOE18int = 0;
                if (value != null) {
                    try {
                        DOE18int = Integer.parseInt(value);
                    } catch (NumberFormatException nfe) {
                        // invalid format, will be reported elsewhere.
                    }
                }
                if ((DOE18int < 1 || DOE18int > 65) && cal.get(Calendar.MONTH) == Calendar.OCTOBER) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Membership days DOE18 must be between " + STYLE_BOLD + "1-65" + STYLE_END
                                    + " for October reporting",
                            "DOE18=" + STYLE_BOLD + value + STYLE_END));
                } else if ((DOE18int < 1 || DOE18int > 171) && cal.get(Calendar.MONTH) == Calendar.MARCH) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Membership days DOE18 must be between " + STYLE_BOLD + "1-171" + STYLE_END
                                    + " for March reporting",
                            "DOE18=" + STYLE_BOLD + value + STYLE_END));
                } else if (DOE18int < 1 || DOE18int > 261) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Membership days DOE18 must be between " + STYLE_BOLD + "1-261" + STYLE_END
                                    + " for Final reporting",
                            "DOE18=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate school.
     * School
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSchoolCode implements FieldValidator {

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
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);

            if ("00000001".equals(value) && !"PK".equals(DOE16)) {
                errors.add(new StateReportValidationError(entity, field,
                        "School code DOE15 pre-school only valid in Grade level DOE16 " + STYLE_BOLD + "PK" + STYLE_END,
                        "DOE15=" + STYLE_BOLD + value + STYLE_END + ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Check evaluation against age and placement.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSpedEvaluation implements FieldValidator {

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
            String DOE32 = entity.getFieldValue(DOE_32_SPED_PLACEMENT);
            String DOE34 = entity.getFieldValue(DOE_34_SPED_PLACEMENT);
            Date bday = ((SIMSEntity) entity).getBirthDate();

            // check age for placement type, (3-5).
            if (bday != null && bday.after(m_preDateMax) && bday.before(m_reportDateMax)) {
                if (value != null && value.matches("0[0289]") && DOE32 != null && !DOE32.matches("00|05")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Evaluation DOE40 only valid in SPED Placement DOE32 " + STYLE_BOLD + "00, 05"
                                    + STYLE_END,
                            "DOE40=" + STYLE_BOLD + value + STYLE_END + ", DOE32=" + STYLE_BOLD + DOE32 + STYLE_END));
                }
            }
            // check age for placement type, (6-21)
            else if (bday != null && bday.after(m_reportDateMin) && bday.before(m_preDateMax)) {
                if (value != null && value.matches("0[0289]") && DOE34 != null && !DOE34.matches("00")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Evaluation DOE40 only valid in SPED Placement DOE34 " + STYLE_BOLD + "00" + STYLE_END,
                            "DOE40=" + STYLE_BOLD + value + STYLE_END + ", DOE34=" + STYLE_BOLD + DOE34 + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate eligibility age for sped disability.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSpedDisability implements FieldValidator {

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
            String DOE06 = entity.getFieldValue(DOE_06_BIRTH_DATE);

            if ("13".equals(value)) {
                Date bday = ((SIMSEntity) entity).getBirthDate();
                if (bday != null && (bday.after(m_reportDateMax) || bday.before(m_spedDateMax))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Disability DOE36 only for age (" + STYLE_BOLD + "3-9" + STYLE_END + " years)",
                            "DOE36=" + STYLE_BOLD + value + STYLE_END + ", DOE06=" + STYLE_BOLD + DOE06 + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validate sped placement in institutional schools for ages 6-21.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSpedPlacement implements FieldValidator {

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
            String DOE06 = entity.getFieldValue(DOE_06_BIRTH_DATE);
            String DOE11 = entity.getFieldValue(DOE_11_REASON);
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            String DOE13 = entity.getFieldValue(DOE_13_ENROLLMENT_REASON);
            String DOE15 = entity.getFieldValue(DOE_15_SCHOOL);
            String DOE16 = entity.getFieldValue(DOE_16_GRADE);
            String DOE32 = entity.getFieldValue(DOE_32_SPED_PLACEMENT);
            String DOE34 = entity.getFieldValue(DOE_34_SPED_PLACEMENT);
            String DOE36 = entity.getFieldValue(DOE_36_SPED_DISABILITY);
            String DOE38 = entity.getFieldValue(DOE_38_SPED_LEVEL);
            String DOE40 = entity.getFieldValue(DOE_40_SPED_EVALUATION_RESULTS);
            Date bday = ((SIMSEntity) entity).getBirthDate();
            SisStudent student = (SisStudent) entity.getBean();

            // check age for placement.
            if (bday != null) {
                if ((bday.after(m_preDateMax) || bday.before(m_reportDateMin))) {
                    // Check when out of age range
                    if (!"00".equals(value) &&
                            (DOE12 != null && DOE12.matches("01|40"))) {
                        errors.add(new StateReportValidationError(entity, field,
                                "SPED Placement DOE34 valid only for age (" + STYLE_BOLD + "6-21" + STYLE_END + ")",
                                "DOE34=" + STYLE_BOLD + value + STYLE_END + ", DOE06=" + STYLE_BOLD + DOE06
                                        + STYLE_END));
                    }
                } else if ((bday.before(m_preDateMax) && bday.after(m_reportDateMin))) {
                    // Check when in age range.
                    // validate placement for evaluation.
                    if ("03".equals(DOE40) && !"01".equals(value)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "SPED Placement DOE34 must be " + STYLE_BOLD + "01" + STYLE_END
                                        + " for SPED Evaluation DOE40 " +
                                        STYLE_BOLD + "03" + STYLE_END,
                                "DOE34=" + STYLE_BOLD + value + STYLE_END + ", DOE40=" + STYLE_BOLD + DOE40
                                        + STYLE_END));
                    }

                    // Validate placement with school.
                    if (DOE15 != null && DOE15.matches("037000[0123456789]*") && !"90".equals(value)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "SPED Placement DOE34 invalid for school code DOE15",
                                "DOE34=" + STYLE_BOLD + value + STYLE_END + ", DOE15=" + STYLE_BOLD + DOE15
                                        + STYLE_END));
                    }
                }
            }
            // validate other sped fields for non-placement
            if ("00".equals(value) && ("00".equals(DOE32) || "05".equals(DOE32))) {
                if (!"500".equals(DOE36) || !"500".equals(DOE38) || (DOE40 != null && !DOE40.matches("0[0289]"))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED fields (DOE36,38,40) must be empty for non-placement DOE32/DOE34",
                            "DOE32=" + STYLE_BOLD + DOE32 + STYLE_END + ",DOE34=" + STYLE_BOLD + value + STYLE_END
                                    + ", DOE36=" + STYLE_BOLD + DOE36 + STYLE_END +
                                    ",\nDOE38=" + STYLE_BOLD + DOE38 + STYLE_END + ", DOE40=" + STYLE_BOLD + DOE40
                                    + STYLE_END));
                }
            }
            if (!"00".equals(value) || (!"00".equals(DOE32) && !"05".equals(DOE32))) {
                if ("500".equals(DOE36) || "500".equals(DOE38) || (DOE40 != null && DOE40.matches("0[0289]"))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED fields (DOE36,38,40) must be present for placement DOE32/DOE34",
                            "DOE32=" + STYLE_BOLD + DOE32 + STYLE_END + ",DOE34=" + STYLE_BOLD + value + STYLE_END
                                    + ", DOE36=" + STYLE_BOLD + DOE36 + STYLE_END +
                                    ",\nDOE38=" + STYLE_BOLD + DOE38 + STYLE_END + ", DOE40=" + STYLE_BOLD + DOE40
                                    + STYLE_END));
                }
            }
            // Validate placement with school.
            if (DOE15 != null && !DOE15.matches("037000[0123456789]*|09200300|09200500") && "90".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "School code DOE15 invalid for SPED Placement DOE34 ",
                        "DOE34=" + STYLE_BOLD + value + STYLE_END + ", DOE15=" + STYLE_BOLD + DOE15 + STYLE_END));
            }
            // Check for only one placement.
            if (!"00".matches(value) && !"00".matches(DOE32)) {
                errors.add(new StateReportValidationError(entity, field,
                        "SPED Placement DOE32 and DOE34 cannot both have placements",
                        "DOE32=" + STYLE_BOLD + DOE32 + STYLE_END + ", DOE34=" + STYLE_BOLD + value + STYLE_END));
            }
            // validate placement for enrollment reason and reporting reason.
            if (value != null && value.matches("50|60")) {
                if (DOE13 != null && !DOE13.matches("0[2679]")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Placement DOE34 " + STYLE_BOLD + "50,60" + STYLE_END
                                    + " require Enrollment reason DOE13 " +
                                    STYLE_BOLD + "02,04,07 or 09" + STYLE_END,
                            "DOE34=" + STYLE_BOLD + value + STYLE_END + ", DOE13=" + STYLE_BOLD + DOE13 + STYLE_END));
                }
                if (!"02".equals(DOE11)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Placement DOE34 " + STYLE_BOLD + "50,60" + STYLE_END
                                    + " require Reporting reason DOE11 " +
                                    STYLE_BOLD + "02" + STYLE_END,
                            "DOE34=" + STYLE_BOLD + value + STYLE_END + ", DOE11=" + STYLE_BOLD + DOE11 + STYLE_END));
                }
            }
            if (Arrays.asList("00", "05").contains(DOE32) && "00".equals(DOE34)
                    && (!StringUtils.isBlank(DOE36) || !StringUtils.isBlank(DOE38) || !"00".equals(DOE40))) {
                errors.add(new StateReportValidationError(entity, field,
                        "If student DOE32 = " + STYLE_BOLD + "00 or 05" + STYLE_END + "AND DOE34 = " + STYLE_BOLD + "00"
                                + STYLE_END
                                + "then DOE36 should be blank, DOE38 should be blank and DOE40 should be 00.",
                        "DOE34=" + STYLE_BOLD + value + STYLE_END +
                                ", DOE32=" + STYLE_BOLD + DOE32 + STYLE_END +
                                ", DOE36=" + STYLE_BOLD + DOE36 + STYLE_END +
                                ", DOE38=" + STYLE_BOLD + DOE38 + STYLE_END +
                                ", DOE40=" + STYLE_BOLD + DOE40 + STYLE_END));
            }
            int ageAsOf = student.getPerson().getAgeAsOfDate(m_reportDate);
            if (5 == ageAsOf && Arrays.asList("KP", "KF", "FT").contains(DOE16) && "00".equals(DOE34)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If student is grade K and age 5 then DOE34 on PL2 (6-21) form alias= educational-environment cannot = 00.",
                        "DOE34=" + STYLE_BOLD + value + STYLE_END +
                                ", DOE16=" + STYLE_BOLD + DOE16 + STYLE_END +
                                ", Age=" + STYLE_BOLD + String.valueOf(ageAsOf) + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate sped placement in institutional schools for ages 3-5.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSpedPlacement35 implements FieldValidator {

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
            String DOE06 = entity.getFieldValue(DOE_06_BIRTH_DATE);
            String DOE11 = entity.getFieldValue(DOE_11_REASON);
            String DOE12 = entity.getFieldValue(DOE_12_ENROLLMENT_STATUS);
            String DOE13 = entity.getFieldValue(DOE_13_ENROLLMENT_REASON);
            String DOE15 = entity.getFieldValue(DOE_15_SCHOOL);
            String DOE40 = entity.getFieldValue(DOE_40_SPED_EVALUATION_RESULTS);
            Date bday = ((SIMSEntity) entity).getBirthDate();

            // check age for placement.
            if (bday != null) {
                if (bday.after(m_reportDateMax) || bday.before(m_preDateMax)) {
                    // Check if out of age range 3-5
                    if (!"00".equals(value) &&
                            (DOE12 != null && DOE12.matches("01|40"))) {
                        errors.add(new StateReportValidationError(entity, field,
                                "SPED Placement DOE32 valid only for age (" + STYLE_BOLD + "3-5" + STYLE_END + ")",
                                "DOE32=" + STYLE_BOLD + value + STYLE_END + ", DOE06=" + STYLE_BOLD + DOE06
                                        + STYLE_END));
                    }
                } else if (bday.before(m_reportDateMax) && bday.after(m_preDateMax)) {
                    // Check when in age range 3-5
                    // validate placement for evaluation.
                    if ("03".equals(DOE40) && !"01".equals(value)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "SPED Placement DOE32 must be " + STYLE_BOLD + "01" + STYLE_END
                                        + " for SPED Evaluation DOE40 " + STYLE_BOLD + "03" + STYLE_END,
                                "DOE32=" + STYLE_BOLD + value + STYLE_END + ", DOE40=" + STYLE_BOLD + DOE40
                                        + STYLE_END));
                    }
                    // Check school type
                    if (DOE15 != null && DOE15.matches("037000[0-9]*") && !"45".equals(value)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "SPED Placement DOE32 invalid for School code DOE15",
                                "DOE32=" + STYLE_BOLD + value + STYLE_END + ", DOE15=" + STYLE_BOLD + DOE15
                                        + STYLE_END));
                    }
                }
            }
            // Check school type
            if (DOE15 != null && !DOE15.matches("037000[0123456789]*|09200300|09200500") && "45".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "School code DOE15 invalid for SPED Placement DOE32",
                        "DOE32=" + STYLE_BOLD + value + STYLE_END + ", DOE15=" + STYLE_BOLD + DOE15 + STYLE_END));
            }
            // validate placement for enrollment reason and reporting reason.
            if (value != null && value.matches("42|44")) {
                if (DOE13 != null && !DOE13.matches("0[2679]")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Placement DOE32 " + STYLE_BOLD + "42,44" + STYLE_END
                                    + " require Enrollment reason DOE13 " + STYLE_BOLD + "02,04,07 or 09" + STYLE_END,
                            "DOE32=" + STYLE_BOLD + value + STYLE_END + ", DOE13=" + STYLE_BOLD + DOE13 + STYLE_END));
                }
                if (!"02".equals(DOE11)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "SPED Placement DOE32 " + STYLE_BOLD + "42,44" + STYLE_END
                                    + " require Reporting reason DOE11 " + STYLE_BOLD + "02" + STYLE_END,
                            "DOE32=" + STYLE_BOLD + value + STYLE_END + ", DOE11=" + STYLE_BOLD + DOE11 + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validates title 1 reporting.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateTitle1School implements FieldValidator {

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
            String DOE11 = entity.getFieldValue(DOE_11_REASON);
            String DOE13 = entity.getFieldValue(DOE_13_ENROLLMENT_REASON);

            if ("02".equals(DOE11) && "01".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Reporting reason DOE11 disallows Title 1 DOE28",
                        "DOE28=" + STYLE_BOLD + value + STYLE_END + ", DOE11=" + STYLE_BOLD + DOE11 + STYLE_END));
            }
            if (DOE13 != null && DOE13.matches("0[4567]|11") && "01".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Enrollment reason DOE13 disallows Title 1 DOE28",
                        "DOE28=" + STYLE_BOLD + value + STYLE_END + ", DOE13=" + STYLE_BOLD + DOE13 + STYLE_END));
            }

            return errors;
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
        return "DOE SIMS";
    }

    /**
     * Return summarization errors, membership totals and statistics.
     *
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
        long ttlMembership = 0;
        long ttlStudents = 0;

        // calculate statistics for membership and attendance. (only if not reporting one student.)
        if (m_membershipStats != null && ((Integer) getParameter(QUERY_BY_PARAM)).intValue() == 0) {
            for (MembershipAttendance ma : m_membershipStats.values()) {
                ttlMembership += ma.getMembership();
                ttlStudents += ma.getStudents();
                if (ma.getMembership() == ma.getAttendance()) {
                    int pos = ma.getKey().indexOf('\t');
                    String school = ma.getKey().substring(0, pos);
                    String grade = ma.getKey().substring(pos + 1);
                    FieldDefinition field = getFieldDefinition(DOE_17_ATTENDANCE_DAYS);

                    String fieldUserName = field.getUserName(SisStudent.class, getBroker().getPersistenceKey());

                    StateReportValidationError error = new StateReportValidationError(school + ", grade " + grade,
                            "(" + field.getFieldId() + ") " + fieldUserName,
                            "Perfect attendance not likely",
                            Long.toString(ma.getAttendance()) + " of " + Long.toString(ma.getMembership()));
                    errors.add(error);
                }
            }
        }

        // check membership average.
        if (ttlStudents > 0) {
            ttlMembership /= ttlStudents;
            if (ttlMembership < 150) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(m_reportDate);
                if (cal.get(Calendar.MONTH) != Calendar.OCTOBER && cal.get(Calendar.MONTH) != Calendar.MARCH) {
                    FieldDefinition field = getFieldDefinition(DOE_18_MEMBERSHIP_DAYS);
                    String fieldUserName = field.getUserName(SisStudent.class, getBroker().getPersistenceKey());
                    errors.add(new StateReportValidationError("Total", "(" + field.getFieldId() + ") " + fieldUserName,
                            "Average Membership too low (<150)", Long.toString(ttlMembership) + " per student"));
                }
            }
        }

        // Check DOE12 status 10/11, cannot have both.
        if (getDOE12Cert() && getDOE12Grad()) {
            FieldDefinition field = getFieldDefinition(DOE_12_ENROLLMENT_STATUS);
            String fieldUserName = field.getUserName(SisStudent.class, getBroker().getPersistenceKey());
            errors.add(new StateReportValidationError("District", "(" + field.getFieldId() + ") " + fieldUserName,
                    "Cannot have both 10 and 11 in same district.", ""));
        }

        return errors;
    }

    // /**
    // * @see
    // com.follett.fsc.core.k12.tools.stateexports.StateReportData#getProperty(com.follett.fsc.core.k12.beans.X2BaseBean,
    // java.lang.String)
    // */
    // @Override
    // public Object getProperty(X2BaseBean bean, String beanPath) throws X2BaseException
    // {
    // Object value = null;
    // if (beanPath.charAt(0) != LABEL_PREFIX_CHAR)
    // {
    // value = WebUtils.getProperty(bean, beanPath);
    //
    // /*
    // * HACK: There is a bug where suffix isn't included. I am including it here.
    // */
    // if ("person.lastName".equals(beanPath))
    // {
    // Object suffixCode = WebUtils.getProperty(bean, "person.nameSuffixCode");
    //
    // if (suffixCode != null)
    // {
    // value = value.toString() + " " + suffixCode.toString();
    // }
    // }
    // }
    //
    // return value;
    // }

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

        /*
         * Load initialization data
         */
        initializeFields();

        m_scheduleManager = new ScheduleManager(getBroker());

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        // m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_START_DATE, property);
        // m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        /*
         * Calculate min and max enrollment birth date based on report date.
         * Calculate max sped eligibility (age 9).
         */
        Calendar cal = Calendar.getInstance();
        cal.setTime(m_reportDate);
        cal.add(Calendar.YEAR, -3);
        m_reportDateMax = cal.getTime(); // 3 years old (> 3), min for enrollment.
        cal.add(Calendar.YEAR, -3);
        m_preDateMax = cal.getTime(); // 5 years old (< 6 years), max for early sped.
        cal.add(Calendar.YEAR, -4);
        m_spedDateMax = cal.getTime(); // 9 years old (< 10 years), max for some sped.
        cal.add(Calendar.YEAR, -6);
        m_sped2DateMax = cal.getTime(); // 16 years old (> 16), min for grade SP.
        cal.add(Calendar.YEAR, -6);
        m_reportDateMin = cal.getTime(); // 21 years old (< 22), max for enrollment.

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_gradeLevelMap = StudentManager.buildGradeLevelMap(getBroker());
        m_illegalBirthCityCharacters = Pattern.compile(ILLEGAL_BIRTH_CITY_CHARACTERS);
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        m_diacriticalMarks = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");
        m_integerConverter =
                ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);

        // Load codes and support data from database.
        loadConductCodes();

        loadEnrollmentData();

        m_octoberReportOnlyEnrollmentCodes =
                StringUtils.convertDelimitedStringToList(OCTOBER_REPORT_DOE_12_STATE_CODES, ',');

        // Set the field definition array.
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(52);
        fieldDefinitions.add(getDOE01_lasid());
        fieldDefinitions.add(getDOE02_sasid());
        fieldDefinitions.add(getDOE03_firstName());
        fieldDefinitions.add(getDOE04_middleName());
        fieldDefinitions.add(getDOE05_lastName());
        fieldDefinitions.add(getDOE06_birthDate());
        fieldDefinitions.add(getDOE07_birthDateFormat());
        fieldDefinitions.add(getDOE08_birthCity());
        fieldDefinitions.add(getDOE09_gender());
        fieldDefinitions.add(getDOE10_race());
        fieldDefinitions.add(getDOE11_enrollmentReport());
        fieldDefinitions.add(getDOE12_enrollmentStatus());
        fieldDefinitions.add(getDOE13_enrollmentReason());
        fieldDefinitions.add(getDOE14_townOfResidence());
        fieldDefinitions.add(getDOE15_school());
        fieldDefinitions.add(getDOE16_grade());
        fieldDefinitions.add(getDOE17_attendance());
        fieldDefinitions.add(getDOE18_membership());
        fieldDefinitions.add(getDOE19_incomeStatus());
        fieldDefinitions.add(getDOE20_title1Status());
        fieldDefinitions.add(getDOE21_lepFirstYear());
        fieldDefinitions.add(getDOE22_immigrationStatus());
        fieldDefinitions.add(getDOE23_countryOfOrigin());
        fieldDefinitions.add(getDOE24_nativeLanguage());
        fieldDefinitions.add(getDOE25_englishProficiency());
        fieldDefinitions.add(getDOE26_englishLearnersProgram());
        fieldDefinitions.add(getDOE27_alternateEducation());
        fieldDefinitions.add(getDOE28_title1School());
        fieldDefinitions.add(getDOE29_familyMilitaryStatus());
        fieldDefinitions.add(getDOE30_titleTargeted());
        fieldDefinitions.add(getDOE31_cteCompetency());
        fieldDefinitions.add(getDOE32_spedPlacement35());
        fieldDefinitions.add(getDOE33_futurePlans());
        fieldDefinitions.add(getDOE34_spedPlacement());
        fieldDefinitions.add(getDOE35_cteProgram());
        fieldDefinitions.add(getDOE36_speddisability());
        fieldDefinitions.add(getDOE37_graduationCurriculum());
        fieldDefinitions.add(getDOE38_spedLevel());
        fieldDefinitions.add(getDOE39_in504());
        fieldDefinitions.add(getDOE40_spedEvaluation());
        fieldDefinitions.add(getDOE41_SLIFE());
        fieldDefinitions.add(getDOE42_ctePopulation());
        fieldDefinitions.add(getDOE43_cte74());
        fieldDefinitions.add(getDOE44_cteNon74());
        fieldDefinitions.add(getDOE45_suspensionsIn());
        fieldDefinitions.add(getDOE46_suspensionsOut());
        fieldDefinitions.add(getDOE47_apCourse1());
        fieldDefinitions.add(getDOE48_apCourse2());
        fieldDefinitions.add(getDOE49_apCourse3());
        fieldDefinitions.add(getDOE50_apCourse4());
        fieldDefinitions.add(getDOE51_apCourse5());
        fieldDefinitions.add(getDOE52_truancies());
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

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            translateAliasToJavaName(DOE_15_SCHOOL, true));
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(SIMSEntity.class);

            int count = getBroker().getCount(studentQuery);

            /*
             * Load the race codes for all students included in the export.
             */
            SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
            Criteria raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, count);

            ExtendedDataDictionary extendedDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
            m_iepDictionary = DataDictionary.getDistrictDictionary(extendedDictionary, getBroker().getPersistenceKey());
            /*
             * Initialize the sped data lookups, which allow us to efficiently retrieve IEP
             * values without querying for the entire IepData object for each student
             */
            if (m_retrieveSpedValues) {
                loadSpedData(studentCriteria);
            }

            DataDictionaryField iepField3_5 =
                    m_iepDictionary.findDataDictionaryFieldByAlias(IEP_EDUCATIONAL_ENVIRONMENT_EC);
            m_iepEdEnviroment3_5FieldName = iepField3_5.getJavaName();
            DataDictionaryField iepField6_21 =
                    m_iepDictionary.findDataDictionaryFieldByAlias(IEP_EDUCATIONAL_ENVIRONMENT);
            m_iepEdEnviroment6_21FieldName = iepField6_21.getJavaName();

            // Load IepData Map for Students that transfered out of state or district.
            loadIepDataTransferStudents();

            /*
             * Load AP courses for all students.
             */
            /*
             * if (m_calculateApCourses)
             * {
             * loadApCourseMaps(studentCriteria);
             * }
             */

            // Load attendance maps.
            if (m_calculateTotals) {
                loadAbsenceDaysMaps(studentCriteria);
                if (TRUANCT_TYPE_INCIDENT.equals(m_truancyType)) {
                    m_truancies = loadConductIncidentMap(studentCriteria, m_truancyCodes);
                }
                // m_suspensionsIn = loadConductActionMap(studentCriteria, m_suspensionInCodes);
                // m_suspensionsOut = loadConductActionMap(studentCriteria, m_suspensionOutCodes);
            }
        }
    }

    /**
     * Build Field definition for DOE 01, student local identifier (LASID).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE01_lasid() {
        FieldDefinition field = new FieldDefinition(DOE_01_LOCAL_ID,
                SisStudent.COL_LOCAL_ID,
                null, false, 1, 32, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 02, student state identifier (SASID).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE02_sasid() {
        FieldDefinition field = new FieldDefinition(DOE_02_STATE_ID,
                SisStudent.COL_STATE_ID,
                null, false, 10, 10, REGEX_ALPHANUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 03, student first name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE03_firstName() {
        FieldDefinition field = new FieldDefinition(DOE_03_FIRST_NAME,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, false, 1, 32, null, null,
                new RetrieveStripNameChar(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 04, student middle name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE04_middleName() {
        FieldDefinition field = new FieldDefinition(DOE_04_MIDDLE_NAME,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME,
                null, false, 1, 32, null, null,
                new RetrieveMiddleName(), null, m_doeNoMiddleName);
        return field;
    }

    /**
     * Build Field definition for DOE 05, student last name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE05_lastName() {
        FieldDefinition field = new FieldDefinition(DOE_05_LAST_NAME,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, false, 1, 50, null, null,
                new RetrieveStripNameChar(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 06, student birth date.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE06_birthDate() {
        FieldDefinition field = new FieldDefinition(DOE_06_BIRTH_DATE,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_DOB,
                null, false, 8, 10, null, m_dateFormat, null,
                new ValidateBirthDate(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 07, student birth date format.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE07_birthDateFormat() {
        FieldDefinition field = new FieldDefinition(DOE_07_BIRTH_DATE_FORMAT,
                LABEL_PREFIX_CHAR + "Date of Birth Format",
                DATE_FORMAT_CODE, false, 3, 3, null, null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 08, student birth city.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE08_birthCity() {
        FieldDefinition field =
                new FieldDefinition(DOE_08_BIRTH_CITY, translateAliasToJavaName(DOE_08_BIRTH_CITY, true),
                        null, false, 1, 50, REGEX_ALPHANUMERIC_SPACE, null,
                        new RetrieveBirthCity(),
                        null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 09, student gender.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE09_gender() {
        FieldDefinition field = new FieldDefinition(DOE_09_GENDER,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_GENDER_CODE,
                null, false, 1, 1, "M|F|N",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 10, student race code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE10_race() {
        FieldDefinition field =
                new FieldDefinition(DOE_10_RACE, translateAliasToJavaName(DOE_10_RACE, true), null, false,
                        2, 2, REGEX_ALPHANUMERIC, null,
                        new RetrieveRace(),
                        null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 11, student enrollment reason for reporting.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE11_enrollmentReport() {
        FieldDefinition field = new FieldDefinition(DOE_11_REASON,
                translateAliasToJavaName(DOE_11_REASON, true), null, true, 2, 2, "0[123]",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 12, student enrollment status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE12_enrollmentStatus() {
        FieldDefinition field =
                new FieldDefinition(DOE_12_ENROLLMENT_STATUS, translateAliasToJavaName(DOE_12_ENROLLMENT_STATUS, true),
                        null, true, 2, 2, "0[14569]|1[01]|2[01234]|3[0123456]|4[01]", null,
                        new RetrieveEnrollmentStatus(),
                        new ValidateEnrollmentStatus(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 13, student enrollment reason.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE13_enrollmentReason() {
        FieldDefinition field = new FieldDefinition(DOE_13_ENROLLMENT_REASON,
                translateAliasToJavaName(DOE_13_ENROLLMENT_REASON, true), null, true, 2, 2, "0[123456789]|1[01]", null,
                null,
                new ValidateEnrollmentReason(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 14, student town of residence.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE14_townOfResidence() {
        FieldDefinition field = new FieldDefinition(DOE_14_TOWN_OF_RESIDENCE,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.REL_PHYSICAL_ADDRESS + ModelProperty.PATH_DELIMITER
                        + SisAddress.COL_CITY,
                null, false, 3, 3, REGEX_NUMERIC, null,
                new RetrieveTownOfResidence(),
                null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 15, school.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE15_school() {
        m_doe15Alias = translateAliasToJavaName(DOE_15_SCHOOL, true);
        FieldDefinition field = new FieldDefinition(DOE_15_SCHOOL,
                SisStudent.REL_SCHOOL + PATH_DELIMITER + m_doe15Alias,
                null, false, 8, 8, REGEX_ALPHANUMERIC, null,
                new RetrieveSchoolCode(),
                new ValidateSchoolCode(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 16, student grade.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE16_grade() {
        FieldDefinition field = new FieldDefinition(DOE_16_GRADE, translateAliasToJavaName(DOE_16_GRADE, true),
                null, true, 2, 2, "0[123456789]|1[012]|PK|K[PFT]|KF|SP", null,
                new RetrieveGradeLevel(),
                new ValidateGradeLevel(), null);
        return field;
    }

    /**
     * Build Field definition for DOE 17, student attendance days.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE17_attendance() {
        FieldDefinition field =
                new FieldDefinition(DOE_17_ATTENDANCE_DAYS, translateAliasToJavaName(DOE_17_ATTENDANCE_DAYS, true),
                        null, false, 1, 3, REGEX_NUMERIC, null,
                        new RetrieveAttendanceDays(),
                        new ValidateAttendanceDays(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 18, student membership days.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE18_membership() {
        FieldDefinition field =
                new FieldDefinition(DOE_18_MEMBERSHIP_DAYS, translateAliasToJavaName(DOE_18_MEMBERSHIP_DAYS, true),
                        null, false, 1, 3, REGEX_NUMERIC, null,
                        new RetrieveMembershipDays(),
                        new ValidateMembershipDays(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 19, student family income status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE19_incomeStatus() {
        FieldDefinition field =
                new FieldDefinition(DOE_19_LOW_INCOME_STATUS, translateAliasToJavaName(DOE_19_LOW_INCOME_STATUS, true),
                        "00", true, 2, 2, "0[012]",
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 20, student title 1 status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE20_title1Status() {
        FieldDefinition field = new FieldDefinition(DOE_20_TITLE_1, translateAliasToJavaName(DOE_20_TITLE_1, true),
                "00", true, 2, 2, "[01][0123456789]|2[01234567]",
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 21, student english proficiency.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE21_lepFirstYear() {
        FieldDefinition field =
                new FieldDefinition(DOE_21_LEP_FIRST_YEAR, translateAliasToJavaName(DOE_21_LEP_FIRST_YEAR, true),
                        "00", true, 2, 2, "0[012]",
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 22, student immigration status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE22_immigrationStatus() {
        FieldDefinition field = new FieldDefinition(DOE_22_IMMIGRATION_STATUS,
                translateAliasToJavaName(DOE_22_IMMIGRATION_STATUS, true),
                "00", true, 2, 2, "0[01]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 23, student country of origin.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE23_countryOfOrigin() {
        FieldDefinition field =
                new FieldDefinition(DOE_23_COUTRY_OF_ORIGIN, translateAliasToJavaName(DOE_23_COUTRY_OF_ORIGIN, true),
                        "500", true, 2, 3, "[A-Z][A-Z]|500", null, null,
                        new ValidateCountryOfOrigin(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 24, student native language.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE24_nativeLanguage() {
        FieldDefinition field =
                new FieldDefinition(DOE_24_NATIVE_LANGUAGE, translateAliasToJavaName(DOE_24_NATIVE_LANGUAGE, true),
                        "267", true, 3, 3, REGEX_NUMERIC,
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 25, student english proficiency.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE25_englishProficiency() {
        FieldDefinition field = new FieldDefinition(DOE_25_ENGLISH_PROFICIENCY,
                translateAliasToJavaName(DOE_25_ENGLISH_PROFICIENCY, true),
                "00", true, 2, 2, "0[01]", null, null,
                new ValidateEnglishProficiency(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 26, english learners program.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE26_englishLearnersProgram() {
        FieldDefinition field = new FieldDefinition(DOE_26_ENGLISH_LEARNERS_PROGRAM,
                translateAliasToJavaName(DOE_26_ENGLISH_LEARNERS_PROGRAM, true),
                "00", true, 2, 2, "0[01234]", null,
                null, new ValidateEnglishLearners(), null);
        return field;
    }


    /**
     * Build Field definition for DOE 27, alternate education.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE27_alternateEducation() {
        FieldDefinition field = new FieldDefinition(DOE_27_ALT_EDUCATION_PROGRAM,
                translateAliasToJavaName(DOE_27_ALT_EDUCATION_PROGRAM, true),
                "00000000", true, 8, 8, REGEX_NUMERIC,
                null, null,
                new ValidateAlternateEducation(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 28, title 1 school choice.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE28_title1School() {
        FieldDefinition field = new FieldDefinition(DOE_28_TITLE1_SCHOOL_CHOICE,
                translateAliasToJavaName(DOE_28_TITLE1_SCHOOL_CHOICE, true),
                "00", true, 2, 2, "0[01]", null,
                null, new ValidateTitle1School(), null);
        return field;
    }

    /**
     * Build Field definition for DOE 29, Family Military Status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE29_familyMilitaryStatus() {
        String beanPath = null;
        if (m_includeMilitaryStatus) {
            beanPath = translateAliasToJavaName(DOE_29_FAMILY_MILITAR_STATUS, true);
        } else {
            // If not selected then display "500"
            String discontinued =
                    LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
            beanPath = LABEL_PREFIX_CHAR + discontinued;
        }

        FieldDefinition field = new FieldDefinition(DOE_29_FAMILY_MILITAR_STATUS, beanPath,
                "00", true, 2, 2, "0[01]",
                null, null, null, null);

        return field;
    }

    /**
     * Build Field definition for DOE 30, discontinued.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE30_titleTargeted() {

        FieldDefinition field = new FieldDefinition(DOE_30_TITLE_TARGETED,
                translateAliasToJavaName(DOE_30_TITLE_TARGETED, true),
                "500", false, 3, 3, "0[01234567]|500", null,
                new RetrieveTitleTargetedAssist(),
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 31, C/TE competency.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE31_cteCompetency() {
        FieldDefinition field =
                new FieldDefinition(DOE_31_CTE_COMPETENCY, translateAliasToJavaName(DOE_31_CTE_COMPETENCY, true),
                        "500", true, 2, 3, "0[1234567]|1[1234]|500",
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 32, SPED Placement ages 3-5.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE32_spedPlacement35() {
        FieldDefinition field =
                new FieldDefinition(DOE_32_SPED_PLACEMENT, translateAliasToJavaName(DOE_32_SPED_PLACEMENT, true),
                        "00", true, 2, 2, "0[015]|3[012468]|4[24568]", null,
                        new RetrieveSpedPlacement(),
                        new ValidateSpedPlacement35(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 33, Future plans.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE33_futurePlans() {
        FieldDefinition field =
                new FieldDefinition(DOE_33_FUTURE_PLANS, translateAliasToJavaName(DOE_33_FUTURE_PLANS, true),
                        "500", true, 2, 3, "0[123456789]|500", null, null,
                        new ValidateFuturePlans(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 34, SPED Placement ages 6-21.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE34_spedPlacement() {
        FieldDefinition field =
                new FieldDefinition(DOE_34_SPED_PLACEMENT, translateAliasToJavaName(DOE_34_SPED_PLACEMENT, true),
                        "00", true, 2, 2, "[01245679]0|[04]1", null,
                        new RetrieveSpedPlacement(),
                        new ValidateSpedPlacement(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 35, C/TE program.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE35_cteProgram() {
        FieldDefinition field =
                new FieldDefinition(DOE_35_CTE_PROGRAM, translateAliasToJavaName(DOE_35_CTE_PROGRAM, true),
                        "00", true, 2, 2, "0[012345]|1[34]", null, null,
                        new ValidateCteProgram(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 36, SPED Disability.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE36_speddisability() {
        FieldDefinition field =
                new FieldDefinition(DOE_36_SPED_DISABILITY, translateAliasToJavaName(DOE_36_SPED_DISABILITY, true),
                        "500", true, 2, 3,
                        "0[123456789]|1[0123]|500", null,
                        new RetrieveSpedDisability(),
                        new ValidateSpedDisability(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 37, graduation core curriculum.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE37_graduationCurriculum() {
        FieldDefinition field = new FieldDefinition(DOE_37_GRADUATE_CORE_CURRICULUM,
                translateAliasToJavaName(DOE_37_GRADUATE_CORE_CURRICULUM, true),
                "00", true, 2, 2, "0[012]",
                null, null,
                new ValidateGraduationCurriculum(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 38, SPED level.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE38_spedLevel() {
        FieldDefinition field =
                new FieldDefinition(DOE_38_SPED_LEVEL, translateAliasToJavaName(DOE_38_SPED_LEVEL, true),
                        "500", true, 2, 3, "0[1234]|500", null,
                        new RetrieveSpedLevel(),
                        null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 39, Student in 504.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE39_in504() {
        FieldDefinition field = new FieldDefinition(DOE_39_IN_504, translateAliasToJavaName(DOE_39_IN_504, true),
                "00", true, 2, 2, "0[01]",
                null, new RetrieveIn504(), null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 40, SPED evaluation.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE40_spedEvaluation() {
        FieldDefinition field = new FieldDefinition(DOE_40_SPED_EVALUATION_RESULTS,
                translateAliasToJavaName(DOE_40_SPED_EVALUATION_RESULTS, true),
                "00", true, 2, 2, "0[0123456789]", null,
                new RetrieveSpedEvaluation(),
                new ValidateSpedEvaluation(),
                null);
        return field;
    }

    /**
     * Build Field definition for DOE 41, discontinued.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE41_SLIFE() {
        FieldDefinition field = new FieldDefinition(DOE_41_SLIFE,
                translateAliasToJavaName(DOE_41_SLIFE, true),
                "00", false, 2, 2, "0[01]",
                new ExportFormatManager.X2LogicalFormat("01,00"), null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 42, C/TE population.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE42_ctePopulation() {
        FieldDefinition field =
                new FieldDefinition(DOE_42_CTE_POPULATION, translateAliasToJavaName(DOE_42_CTE_POPULATION, true),
                        "500", true, 2, 3, "0[123]|500",
                        null, null,
                        new ValidateCtePopulation(),
                        null);
        return field;
    }

    /**
     * Build Field definition for DOE 43, chapter 74 C/TE.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE43_cte74() {
        FieldDefinition field = new FieldDefinition(DOE_43_CTE_74, translateAliasToJavaName(DOE_43_CTE_74, true),
                "500", true, 3, 6, REGEX_ALPHANUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 44, non-chapter 74 C/TE.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE44_cteNon74() {
        FieldDefinition field =
                new FieldDefinition(DOE_44_CTE_NON_74, translateAliasToJavaName(DOE_44_CTE_NON_74, true),
                        "500", true, 3, 4, REGEX_ALPHANUMERIC,
                        null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 45, suspensions in school
     * NOTE: 2012-2013 Discontinued. Report 0
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE45_suspensionsIn() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_45_SUSPENSIONS_IN, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_45_SUSPENSIONS_IN,
                                                                                                             // true),
                "0", false, 1, 2, null, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 46, suspensions out of school
     * NOTE: 2012-2013 Discontinued. Report 0
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE46_suspensionsOut() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_46_SUSPENSIONS_OUT, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_46_SUSPENSIONS_OUT,
                                                                                                              // true),
                "0", false, 1, 2, null, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 47, AP course 1
     * NOTE: 2012-2013 Discontinued. Report 500
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE47_apCourse1() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_47_AP_COURSE_1, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_47_AP_COURSE_1,
                                                                                                          // true),
                "500", false, 3, 3, REGEX_AP_COURSE, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 48, AP course 2
     * NOTE: 2012-2013 Discontinued. Report 500
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE48_apCourse2() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_48_AP_COURSE_2, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_48_AP_COURSE_2,
                                                                                                          // true),
                "500", false, 3, 3, REGEX_AP_COURSE, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 49, AP course 3
     * NOTE: 2012-2013 Discontinued. Report 500
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE49_apCourse3() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_49_AP_COURSE_3, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_49_AP_COURSE_3,
                                                                                                          // true),
                "500", false, 3, 3, REGEX_AP_COURSE, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 50, AP course 4
     * NOTE: 2012-2013 Discontinued. Report 500
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE50_apCourse4() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_50_AP_COURSE_4, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_50_AP_COURSE_4,
                                                                                                          // true),
                "500", false, 3, 3, REGEX_AP_COURSE, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 51, AP course 5
     * NOTE: 2012-2013 Discontinued. Report 500
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE51_apCourse5() {
        String discontinued =
                LocalizationCache.getMessages(getDataDictionary().getPersistenceKey()).getMessage(DISCONTINUED);
        FieldDefinition field = new FieldDefinition(DOE_51_AP_COURSE_5, LABEL_PREFIX_CHAR + discontinued, // translateAliasToJavaName(DOE_51_AP_COURSE_5,
                                                                                                          // true),
                "500", false, 3, 3, REGEX_AP_COURSE, null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for DOE 52, truancies.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getDOE52_truancies() {
        FieldDefinition field =
                new FieldDefinition(DOE_52_TRUANCIES, translateAliasToJavaName(DOE_52_TRUANCIES, true), null,
                        false, 1, 3, REGEX_NUMERIC, null,
                        new RetrieveTruancies(),
                        null, null);
        return field;
    }

    /**
     * Returns a flag indicating someone in the district
     * has DOE12 status 10 (certificate of attainment).
     *
     *
     * @return boolean
     */
    protected boolean getDOE12Cert() {
        return m_doe12_cert;
    }

    /**
     * Returns a flag indicating someone in the district
     * has DOE12 status 11 (graduate grade 12).
     *
     *
     * @return boolean
     */
    protected boolean getDOE12Grad() {
        return m_doe12_grad;
    }

    /**
     * Return the membership stats map for the entity to accumulate stats in post process.
     *
     * @return Map
     */
    protected Map<String, MembershipAttendance> getMembershipStats() {
        if (m_membershipStats == null) {
            m_membershipStats = new HashMap<String, MembershipAttendance>();
        }
        return m_membershipStats;
    }

    /**
     * Returns true if the passed student exited special education this year. A student is
     * considered exited if their status is either EXITED or INELIGIBLE, and the exit date on the
     * student record is within the current school year.
     *
     * @param student SisStudent
     * @return boolean
     */
    protected boolean isSpedExitedThisYear(SisStudent student) {
        return (SpedStatusCode.EXITED.equals(student.getSpedStatusCodeEnum()) ||
                SpedStatusCode.INELIGIBLE.equals(student.getSpedStatusCodeEnum())) &&
                student.getSpedExitDate() != null &&
                student.getSpedExitDate().after(getOrganization().getCurrentContext().getStartDate());
    }

    /**
     * Set the flag indicating someone in the district has received
     * DOE12 status 10 (certificate of attainment).
     *
     * @param cert void
     */
    protected void setDOE12Cert(boolean cert) {
        m_doe12_cert = cert;
    }

    /**
     * Set the flag indicating someone in the district has received
     * DOE12 status 11 (graduate grade 12).
     *
     * @param grad void
     */
    protected void setDOE12Grad(boolean grad) {
        m_doe12_grad = grad;
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
         * Who should be included? Primary students and, optionally, summer withdrawals.
         *
         * The students that belong to each group depend upon how this export is being run:
         *
         * Case 1: The export is being run for either (A) the entire district or (B) a single school
         * without "nested districts" (99% of all cases)
         *
         * Case 2: The export is being run for a single school with "nested districts"
         *
         * ----------------------------------------------------------------------------------------
         *
         * Q: What are "nested districts" anyway?
         *
         * A: A single X2 district could really represent multiple districts as far as the DOE is
         * concerned. For example, Nauset is a single X2 district but only the middle and high
         * schools are in the Nauset Regional School Organization. All the elementary schools belong
         * to their own districts according to the DOE. These "nested districts" can be
         * represented in X2 by setting different Adjusted Organization Code values on the school
         * records. If "nested districts" are used then ALL schools should have an Adjusted
         * Organization Code (even the archive school).
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case 1:
         *
         * Students in an active, non-archived school in the district
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case 2:
         *
         * Students in the select school as well as students who have withdrawn from the selected
         * school between the start of the school year and the report date (both dates inclusive)
         * and are now in a school with a different adjusted district code
         *
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals, case 1:
         *
         * Students who withdrew during the summer (start/end dates inclusive) and are now in the
         * archive school
         *
         * ----------------------------------------------------------------------------------------
         *
         * Summer withdrawals, case 2:
         *
         * Students who withdrew from the selected school during the summer (start/end dates
         * inclusive) and are now either in the archive school or a school with a different
         * adjusted district code
         *
         * ----------------------------------------------------------------------------------------
         *
         * TODO:
         *
         * There are two shortcomings with "nested districts" for students who transfer from one X2
         * school to another:
         *
         * 1. The school code will not be correct for summer transfers (since this procedure
         * uses an enrollment snapshot based on the reporting date)
         *
         * 2. The enrollment status will not be correct for any students (since the student stays
         * active during a transfer)
         *
         * Item 1 could be fixed by changing how this procedure works. Item 2 would require
         * additional information to be stored in an enrollment record. Right now users will have to
         * manually adjust these two fields after the export has finished.
         */
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ADJUSTED_DISTRICT_CODE_FIELD);

        String adjustedDistrictCode = null;
        if (isSchoolContext() && field != null) {
            adjustedDistrictCode = (String) getSchool().getFieldValueByBeanPath(field.getJavaName());
        }

        boolean useNestedDistricts = !StringUtils.isEmpty(adjustedDistrictCode);

        /*
         * Primary students
         */
        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            if (useNestedDistricts) {
                PlainDate startDate = ((SisSchool) getSchool()).getActiveSchedule().getStartDate();

                Criteria withdrawalsCriteria = new Criteria();
                withdrawalsCriteria.addIn(X2BaseBean.COL_OID, getStudentWithdrawalQuery(startDate, m_reportDate));
                withdrawalsCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);

                primaryCriteria.addOrCriteria(withdrawalsCriteria);
            }
        } else {
            primaryCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
            if (m_includeSifSchoolIds != null) {
                primaryCriteria.addIn(SisStudent.REL_SCHOOL
                        + ModelProperty.PATH_DELIMITER
                        + m_sklDstrIdField, m_includeSifSchoolIds);
            }
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addAndCriteria(primaryCriteria);

        /*
         * Summer withdrawals
         */
        PlainDate summerEndDate = (PlainDate) getParameter(SUMMER_END_DATE_PARAM);
        PlainDate summerStartDate = (PlainDate) getParameter(SUMMER_START_DATE_PARAM);

        // Load a collection of OIDs of students that withdrew during the summer
        m_summerWithdrawals =
                getBroker().getSubQueryCollectionByQuery(getStudentWithdrawalQuery(summerStartDate, summerEndDate));

        if (!CollectionUtils.isEmpty(m_summerWithdrawals)) {
            X2Criteria schoolCriteria = new X2Criteria();

            if (useNestedDistricts) {
                schoolCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + field.getJavaName(),
                        adjustedDistrictCode);
            }

            schoolCriteria.addOrEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);

            X2Criteria summerCriteria = new X2Criteria();
            summerCriteria.addEqualTo(SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
            summerCriteria.addAndCriteria(schoolCriteria);

            summerCriteria.addIn(X2BaseBean.COL_OID, m_summerWithdrawals);

            reportingCriteria.addOrCriteria(summerCriteria);
        }

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        FieldDefinition fieldDef32 = getFieldDefinition(DOE_32_SPED_PLACEMENT);
        FieldDefinition fieldDef34 = getFieldDefinition(DOE_34_SPED_PLACEMENT);
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            userCriteria.addEqualTo(m_doeStatusField, DOE_STATUS_FIELD_REPORT_CODE);
        }

        Boolean spedOnly = (Boolean) getParameter(SPED_ONLY_PARAM);
        if (spedOnly.booleanValue()) {
            // Retrieve the default code for doe32&34 state code '00'
            DataDictionary districtDictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
            String oid32 =
                    districtDictionary.findDataDictionaryFieldByAlias(DOE_32_SPED_PLACEMENT).getReferenceTableOid();
            String oid34 =
                    districtDictionary.findDataDictionaryFieldByAlias(DOE_34_SPED_PLACEMENT).getReferenceTableOid();

            Criteria criteria32 = new Criteria();
            criteria32.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID, oid32);
            criteria32.addEqualTo(ReferenceCode.COL_STATE_CODE, fieldDef32.getDefaultValue());
            criteria32.addEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.FALSE);
            ReferenceCode code32 =
                    (ReferenceCode) getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria32));

            Criteria criteria34 = new Criteria();
            criteria34.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + "." + X2BaseBean.COL_OID, oid34);
            criteria34.addEqualTo(ReferenceCode.COL_STATE_CODE, fieldDef34.getDefaultValue());
            criteria34.addEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.FALSE);
            ReferenceCode code34 =
                    (ReferenceCode) getBroker().getBeanByQuery(new QueryByCriteria(ReferenceCode.class, criteria34));

            /*
             * and ( (doe32 != null && doe32 != default code) or
             * (doe34 != null && doe34 != default code) )
             */
            X2Criteria doe32subCriteria = new X2Criteria();
            doe32subCriteria.addNotEmpty(fieldDef32.getBeanPath(), getBroker().getPersistenceKey());
            doe32subCriteria.addNotEqualTo("upper(" + fieldDef32.getBeanPath() + ")", code32.getCode().toUpperCase());

            X2Criteria doe34subCriteria = new X2Criteria();
            doe34subCriteria.addNotEmpty(fieldDef34.getBeanPath(), getBroker().getPersistenceKey());
            doe34subCriteria.addNotEqualTo("upper(" + fieldDef34.getBeanPath() + ")", code34.getCode().toUpperCase());

            doe32subCriteria.addOrCriteria(doe34subCriteria);

            userCriteria.addAndCriteria(doe32subCriteria);

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
     * Returns a query that finds the students who withdrew during the given date range (filtered
     * by school as appropriate) and never re-entered the district again.
     *
     * @param startDate PlainDate
     * @param endDate PlainDate
     * @return A SubQuery that returns the student OID from StudentEnrollment records
     */
    private SubQuery getStudentWithdrawalQuery(PlainDate startDate, PlainDate endDate) {
        /*
         * Retrieve all the Entry records of the Summer withdrawals after the Summer end date.
         */
        Criteria entryCriteria = new Criteria();
        Criteria entryCriteria1 = new Criteria();
        Criteria entryCriteria2 = new Criteria();
        entryCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        entryCriteria.addEqualToField(StudentEnrollment.COL_STUDENT_OID,
                Criteria.PARENT_QUERY_PREFIX + StudentEnrollment.COL_STUDENT_OID);

        entryCriteria1.addGreaterThanField(StudentEnrollment.COL_ENROLLMENT_DATE,
                Criteria.PARENT_QUERY_PREFIX + StudentEnrollment.COL_ENROLLMENT_DATE);

        entryCriteria2.addEqualToField(StudentEnrollment.COL_ENROLLMENT_DATE,
                Criteria.PARENT_QUERY_PREFIX + StudentEnrollment.COL_ENROLLMENT_DATE);
        entryCriteria2.addGreaterThanField(StudentEnrollment.COL_TIMESTAMP,
                Criteria.PARENT_QUERY_PREFIX + StudentEnrollment.COL_TIMESTAMP);

        entryCriteria1.addOrCriteria(entryCriteria2);
        entryCriteria.addAndCriteria(entryCriteria1);

        if (isSchoolContext()) {
            entryCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        /*
         * Retrieve all the Summer withdrawal records that never re-entered the district again.
         */
        Criteria withdrawalCriteria = new Criteria();
        withdrawalCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        withdrawalCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        withdrawalCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
        withdrawalCriteria.addNotExists(
                new SubQuery(StudentEnrollment.class, X2BaseBean.COL_OID, entryCriteria));

        if (isSchoolContext()) {
            withdrawalCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, withdrawalCriteria);
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_sklDstrIdField = translateAliasToJavaName(ALIAS_SKL_SIF_DISTRICT_ID, true);
        // Load Input Definition Parameters
        String includeIds = (String) getParameter(PARAM_INCLUDE_SIF_SCHOOL);
        if (!StringUtils.isEmpty(includeIds)) {
            List<String> rcdOids = new ArrayList<String>(Arrays.asList(includeIds.split(SEPARATOR_COMMA)));
            X2Criteria sifDistrIdCriteria = new X2Criteria();
            sifDistrIdCriteria.addIn(X2BaseBean.COL_OID, rcdOids);
            QueryByCriteria byCriteria = new QueryByCriteria(ReferenceCode.class, sifDistrIdCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(byCriteria);
            m_includeSifSchoolIds = new ArrayList();
            for (ReferenceCode code : refCodes) {
                m_includeSifSchoolIds.add(code.getCode());
            }
        }
        /*
         * Get core parameters
         */
        // m_calculateApCourses = ((Boolean)
        // getParameter(CALCULATE_AP_COURSES_PARAM)).booleanValue();
        m_calculateTotals = ((Boolean) getParameter(CALCULATE_TOTALS_PARAM)).booleanValue();
        m_calculateDOE39 = ((Boolean) getParameter(CALCULATE_DOE_39_PARAM)).booleanValue();
        m_octoberReport = ((Boolean) getParameter(OCTOBER_REPORT_PARAM)).booleanValue();
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        m_retrieveSpedValues = ((Boolean) getParameter(RETRIEVE_SPED_VALUES)).booleanValue();
        m_truancyType = (String) getParameter(TRUANCY_CALCULATION_TYPE_PARAM);
        m_includeMilitaryStatus = ((Boolean) getParameter(INCLUDE_FAMILY_MILITARY_STATUS_PARAM)).booleanValue();

        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_adjustedAttendanceCount = translateAliasToJavaName(ADJUSTED_ATTENDANCE_COUNT_FIELD, true);
        m_adjustedMembershipCount = translateAliasToJavaName(ADJUSTED_MEMBERSHIP_COUNT_FIELD, true);
        m_adjustedSchoolCode = translateAliasToJavaName(ADJUSTED_SCHOOL_CODE_FIELD, true);
        // m_apCourseCode = translateAliasToJavaName(AP_COURSE_CODE_FIELD, false);
        m_exitDate504Field = translateAliasToJavaName(EXIT_DATE_504, false);
        m_doeNoMiddleName = translateAliasToJavaName(DOE_NO_MIDDLE_NAME, false);
        m_doeStatusField = translateAliasToJavaName(DOE_STATUS_FIELD, true);
    }

    /**
     * Loads a map by student of AP courses for that student. The AP courses are loaded from the
     * Transcript records for graduated seniors and from Student Schedule records for everyone else.
     *
     * @param studentCriteria Criteria
     */
    /*
     * private void loadApCourseMaps(Criteria studentCriteria)
     * {
     * // SubQuery 1 - Students.
     * SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID,
     * studentCriteria);
     *
     * // SubQuery 2 - Graduated Seniors.
     * DataDictionary dictionary =
     * DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());
     * DataDictionaryField field =
     * dictionary.findDataDictionaryFieldByAlias(DOE_12_ENROLLMENT_STATUS);
     *
     * List<String> graduateCodes = new ArrayList<String>();
     * List<String> graduateStateCodes =
     * StringUtils.convertDelimitedStringToList(GRADUATE_DOE_12_STATE_CODES, ',');
     *
     * ReferenceTable referenceTable = field.getReferenceTable();
     * if (referenceTable != null)
     * {
     * Collection<ReferenceCode> referenceCodes = referenceTable.getReferenceCodes();
     * for (ReferenceCode referenceCode : referenceCodes)
     * {
     * if (graduateStateCodes.contains(referenceCode.getStateCode()))
     * {
     * graduateCodes.add(referenceCode.getCode());
     * }
     * }
     * }
     *
     * X2Criteria graduatedSeniorsCriteria = new X2Criteria();
     * graduatedSeniorsCriteria.addEqualTo(SisStudent.COL_YOG, new
     * Integer(getOrganization().getCurrentContext().getSchoolYear()));
     * graduatedSeniorsCriteria.addIn(SisStudent.COL_ENROLLMENT_STATUS, graduateCodes);
     * graduatedSeniorsCriteria.addIn(X2BaseBean.COL_OID, studentsSubQuery);
     *
     * SubQuery graduatedSeniorsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID,
     * graduatedSeniorsCriteria);
     *
     * // Subquery 3 - Active school schedules.
     * X2Criteria schedCriteria = new X2Criteria();
     * schedCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
     * getOrganization().getCurrentContext().getOid());
     * SubQuery schedSubQuery = new SubQuery(SchoolScheduleContext.class,
     * SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, schedCriteria);
     *
     * // Subquery 4 - Student Schedule term.
     * X2Criteria studentScheduleTermsCriteria = new X2Criteria();
     * studentScheduleTermsCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE,
     * m_reportDate);
     * studentScheduleTermsCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE,
     * m_reportDate);
     * studentScheduleTermsCriteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM +
     * PATH_DELIMITER + ScheduleTerm.COL_SCHEDULE_OID,
     * Criteria.PARENT_QUERY_PREFIX + StudentSchedule.REL_SECTION + PATH_DELIMITER +
     * MasterSchedule.COL_SCHEDULE_OID);
     * SubQuery studentScheduleTermsSubQuery = new SubQuery(ScheduleTermDate.class,
     * ScheduleTermDate.COL_SCHEDULE_TERM_OID, studentScheduleTermsCriteria);
     *
     * // Student Schedule query - students and AP courses.
     * X2Criteria studentSchedulecriteria = new X2Criteria();
     * studentSchedulecriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentsSubQuery);
     * studentSchedulecriteria.addNotIn(StudentSchedule.COL_STUDENT_OID, graduatedSeniorsSubQuery);
     * studentSchedulecriteria.addIn(StudentSchedule.COL_SCHEDULE_OID, schedSubQuery);
     * studentSchedulecriteria.addIn(StudentSchedule.REL_SECTION + PATH_DELIMITER +
     * MasterSchedule.COL_SCHEDULE_TERM_OID, studentScheduleTermsSubQuery);
     * studentSchedulecriteria.addNotNull(StudentSchedule.REL_SECTION + PATH_DELIMITER +
     * MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
     * SchoolCourse.REL_COURSE + PATH_DELIMITER +
     * m_apCourseCode);
     * studentSchedulecriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
     * MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
     * SchoolCourse.REL_COURSE + PATH_DELIMITER +
     * m_apCourseCode, DEFAULT_AP_COURSE_CODE);
     *
     * ReportQueryByCriteria studentScheduleQuery = new ReportQueryByCriteria(StudentSchedule.class,
     * new String[] {StudentSchedule.COL_STUDENT_OID,
     * StudentSchedule.REL_SECTION + PATH_DELIMITER +
     * MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
     * SchoolCourse.REL_COURSE + PATH_DELIMITER +
     * m_apCourseCode}, studentSchedulecriteria);
     *
     * // Build the map of student to courses from Student Schedule records.
     * m_apCourses = new HashMap<String, List<String>>();
     * ReportQueryIterator studentScheduleIterator =
     * getBroker().getReportQueryIteratorByQuery(studentScheduleQuery);
     * try
     * {
     * while (studentScheduleIterator.hasNext())
     * {
     * Object[] row = (Object[]) studentScheduleIterator.next();
     * String studentOid = (String) row[0];
     * String apCode = (String) row[1];
     *
     * if (!StringUtils.isEmpty(apCode))
     * {
     * List<String> courses = m_apCourses.get(studentOid);
     * if (courses == null)
     * {
     * courses = new ArrayList<String>();
     * m_apCourses.put(studentOid, courses);
     * }
     *
     * courses.add(apCode);
     * }
     * }
     * }
     * finally
     * {
     * studentScheduleIterator.close();
     * }
     *
     * // Subquery 5 - Transcript Schedule term.
     * X2Criteria transcriptTermsCriteria = new X2Criteria();
     * transcriptTermsCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_reportDate);
     * transcriptTermsCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, m_reportDate);
     * transcriptTermsCriteria.addEqualToField(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER +
     * ScheduleTerm.COL_SCHEDULE_OID,
     * Criteria.PARENT_QUERY_PREFIX + Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
     * MasterSchedule.COL_SCHEDULE_OID);
     * SubQuery transcriptTermsSubQuery = new SubQuery(ScheduleTermDate.class,
     * ScheduleTermDate.COL_SCHEDULE_TERM_OID, transcriptTermsCriteria);
     *
     *
     * // Transcript query - students and AP courses.
     * X2Criteria transcriptCriteria = new X2Criteria();
     * transcriptCriteria.addNotEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
     * transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, graduatedSeniorsSubQuery);
     * transcriptCriteria.addIn(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
     * MasterSchedule.COL_SCHEDULE_OID, schedSubQuery);
     * transcriptCriteria.addIn(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
     * MasterSchedule.COL_SCHEDULE_TERM_OID, transcriptTermsSubQuery);
     * transcriptCriteria.addNotNull(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
     * MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
     * SchoolCourse.REL_COURSE + PATH_DELIMITER + m_apCourseCode);
     * transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
     * MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
     * SchoolCourse.REL_COURSE + PATH_DELIMITER + m_apCourseCode, DEFAULT_AP_COURSE_CODE);
     *
     * ReportQueryByCriteria transcriptQuery = new ReportQueryByCriteria(Transcript.class,
     * new String[] {Transcript.COL_STUDENT_OID,
     * Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
     * MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
     * SchoolCourse.REL_COURSE + PATH_DELIMITER +
     * m_apCourseCode}, transcriptCriteria);
     *
     * // Add to the map courses from Transcript records.
     * ReportQueryIterator transcriptIterator =
     * getBroker().getReportQueryIteratorByQuery(transcriptQuery);
     * try
     * {
     * while (transcriptIterator.hasNext())
     * {
     * Object[] row = (Object[]) transcriptIterator.next();
     * String studentOid = (String) row[0];
     * String apCode = (String) row[1];
     *
     * if (!StringUtils.isEmpty(apCode))
     * {
     * List<String> courses = m_apCourses.get(studentOid);
     * if (courses == null)
     * {
     * courses = new ArrayList<String>();
     * m_apCourses.put(studentOid, courses);
     * }
     *
     * courses.add(apCode);
     * }
     * }
     * }
     * finally
     * {
     * transcriptIterator.close();
     * }
     * }
     */

    /**
     * Loads a map by student of absence days for that student.
     *
     * @param Query of students that will be loaded.
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentsSubQuery);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                new String[] {StudentAttendance.COL_STUDENT_OID, "COUNT(*)"}, criteria);
        reportQuery.addGroupBy(StudentAttendance.COL_STUDENT_OID);

        // Build the map of student to absences.
        m_absences = new HashMap<String, Integer>();
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String studentOid = (String) row[0];
                Integer absencesCount = Integer.valueOf(row[1].toString());
                m_absences.put(studentOid, absencesCount);
            }
        } finally {
            iterator.close();
        }

        // Now count unexcused absences.
        if (!TRUANCT_TYPE_INCIDENT.equals(m_truancyType)) {
            m_absencesUnex = new HashMap<String, Integer>();
            // continue using the same criteria, add the unexcused criteria.
            criteria.addNotEqualTo(StudentAttendance.COL_EXCUSED_INDICATOR, Boolean.TRUE);

            reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                    new String[] {StudentAttendance.COL_STUDENT_OID, "COUNT(*)"}, criteria);
            reportQuery.addGroupBy(StudentAttendance.COL_STUDENT_OID);

            iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String studentOid = (String) row[0];
                    Integer absencesCount = Integer.valueOf(row[1].toString());
                    m_absencesUnex.put(studentOid, absencesCount);
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Build a student by count map from the conduct action table for a given set
     * of students (subquery) and conduct codes (collection of codes).
     *
     * @param studentCriteria Criteria
     * @param codes Collection<String>
     * @return Map
     */
    /*
     * private Map<String, Integer> loadConductActionMap(Criteria studentCriteria,
     * Collection<String> codes)
     * {
     * Map<String, Integer> resultMap = new HashMap<String, Integer>();
     * if (codes != null && codes.size() > 0)
     * {
     * // subQuery - Students.
     * SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID,
     * studentCriteria);
     *
     * // Query - Find contexts for the report date.
     * Criteria criteria = new Criteria();
     * criteria.addIn(ConductAction.COL_STUDENT_OID, studentsSubQuery);
     * criteria.addIn(ConductAction.COL_ACTION_CODE, codes);
     * criteria.addGreaterOrEqualThanField(ConductAction.COL_ACTION_START_DATE,
     * ConductAction.REL_SCHOOL + PATH_DELIMITER +
     * SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
     * SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
     * Schedule.COL_START_DATE);
     * criteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_reportDate);
     *
     * ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(ConductAction.class,
     * new String[] {ConductAction.COL_STUDENT_OID, "COUNT(*)"}, criteria);
     * reportQuery.addGroupBy(ConductAction.COL_STUDENT_OID);
     *
     * // Build the map of student to courses.
     * ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
     * try
     * {
     * while (iterator.hasNext())
     * {
     * Object[] row = (Object[]) iterator.next();
     * String studentOid = (String) row[0];
     * Integer absencesCount = Integer.valueOf(row[1].toString());
     * resultMap.put(studentOid, absencesCount);
     * }
     * }
     * finally
     * {
     * iterator.close();
     * }
     * }
     *
     * return resultMap;
     * }
     */

    /**
     * Build a student by count map from the conduct incident table for a given set
     * of students (subquery) and conduct codes (collection of codes)
     *
     * @param studentSubQuery
     * @param codes
     * @return
     */
    private Map<String, Integer> loadConductIncidentMap(Criteria studentCriteria, Collection<String> codes) {
        Map<String, Integer> resultMap = new HashMap<String, Integer>();
        if (codes != null && codes.size() > 0) {
            // subQuery - Students.
            SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            Criteria criteria = new Criteria();
            criteria.addIn(ConductIncident.COL_STUDENT_OID, studentsSubQuery);
            criteria.addIn(ConductIncident.COL_INCIDENT_CODE, codes);
            criteria.addGreaterOrEqualThanField(ConductIncident.COL_INCIDENT_DATE,
                    ConductIncident.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                            SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                            Schedule.COL_START_DATE);
            criteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_reportDate);

            ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(ConductIncident.class,
                    new String[] {ConductIncident.COL_STUDENT_OID, "COUNT(*)"}, criteria);
            reportQuery.addGroupBy(ConductIncident.COL_STUDENT_OID);

            // Build the map of student to courses.
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String studentOid = (String) row[0];
                    Integer absencesCount = Integer.valueOf(row[1].toString());
                    resultMap.put(studentOid, absencesCount);
                }
            } finally {
                iterator.close();
            }
        }

        return resultMap;
    }

    /**
     * Loads all the basic codes that correspond to the state codes for in-school suspensions,
     * out-of-school suspensions, and truancies. If there are no basic codes for the state code then
     * the collection(s) will be empty.
     */
    private void loadConductCodes() {
        m_suspensionInCodes = new LinkedList();
        m_suspensionOutCodes = new LinkedList();
        m_truancyCodes = new LinkedList();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        /*
         * Suspensions are ConductAction codes
         */
        DataDictionaryField actionCode = dictionary.findDataDictionaryField(
                ConductAction.class.getName(), ConductAction.COL_ACTION_CODE);
        String actionReferenceTable = actionCode.getDataFieldConfig().getReferenceTableOid();

        // In-school
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, SUSPENSION_IN_CODE);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator referenceCodes = getBroker().getIteratorByQuery(query);
        try {
            while (referenceCodes.hasNext()) {
                ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
                m_suspensionInCodes.add(referenceCode.getCode());
            }
        } finally {
            referenceCodes.close();
        }

        // Out-of-school
        criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, actionReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, SUSPENSION_OUT_CODE);

        query = new QueryByCriteria(ReferenceCode.class, criteria);
        referenceCodes = getBroker().getIteratorByQuery(query);
        try {
            while (referenceCodes.hasNext()) {
                ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
                m_suspensionOutCodes.add(referenceCode.getCode());
            }
        } finally {
            referenceCodes.close();
        }

        /*
         * Truancies are ConductIncident codes
         */
        DataDictionaryField incidentCode = dictionary.findDataDictionaryField(
                ConductIncident.class.getName(), ConductIncident.COL_INCIDENT_CODE);
        String incidentReferenceTable = incidentCode.getDataFieldConfig().getReferenceTableOid();

        criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, incidentReferenceTable);
        criteria.addEqualTo(ReferenceCode.COL_STATE_CODE, TRUANCY_CODE);

        query = new QueryByCriteria(ReferenceCode.class, criteria);
        referenceCodes = getBroker().getIteratorByQuery(query);
        try {
            while (referenceCodes.hasNext()) {
                ReferenceCode referenceCode = (ReferenceCode) referenceCodes.next();
                m_truancyCodes.add(referenceCode.getCode());
            }
        } finally {
            referenceCodes.close();
        }
    }

    /**
     * Loads the enrollment data required by this export.
     */
    private void loadEnrollmentData() {
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_schoolsToCalendars = new HashMap();
        m_firstDayDate = getOrganization().getCurrentContext().getStartDate();
        m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_firstDayDate, getOrganization(), true);

        // Validation maps for DOE11, 12, 13
        m_enrollmentStatusMatches = new Collection[4];
        m_enrollmentReasonMatches = new Collection[4];
        m_enrollmentStatusMatches[1] = Arrays.asList("01", "04", "05", "06", "09", "10", "11", "20",
                "21", "22", "23", "24", "30", "31", "32", "33",
                "34", "35", "36", "40", "41");
        m_enrollmentStatusMatches[2] = Arrays.asList("01", "04", "05", "06", "09", "10", "11", "20",
                "21", "22", "23", "24", "30", "31", "32", "33",
                "34", "35", "36");
        m_enrollmentStatusMatches[3] = Arrays.asList("01", "04", "05", "06", "09", "10", "11", "20",
                "21", "22", "23", "24", "30", "31", "32", "33",
                "34", "35", "36", "40", "41");
        m_enrollmentReasonMatches[1] = Arrays.asList("01", "08", "11");
        m_enrollmentReasonMatches[2] = Arrays.asList("06", "07");
        m_enrollmentReasonMatches[3] = Arrays.asList("02", "03", "04", "05", "06", "08", "09", "10",
                "11");
    }

    /**
     * Loads the IepData For Students that have Transfer out of state or district.
     */
    private void loadIepDataTransferStudents() {
        Criteria transferCriteria = new Criteria();
        ArrayList transferredStatuses = new ArrayList();
        transferredStatuses.add("Transferred out of district");
        transferredStatuses.add("Transferred out of state");
        transferCriteria.addIn(IepData.COL_EXIT_REASON, transferredStatuses);
        // transferCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, startDate);
        // transferCriteria.addLessOrEqualThan(IepData.COL_END_DATE, endDate);
        if (isSchoolContext()) {
            transferCriteria.addEqualTo(IepData.REL_STUDENT + "." + Student.COL_SCHOOL_OID, getSchool().getOid());
        } else if (m_includeSifSchoolIds != null) {
            transferCriteria.addIn(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER + Student.REL_SCHOOL
                    + ModelProperty.PATH_DELIMITER
                    + m_sklDstrIdField, m_includeSifSchoolIds);
        }

        QueryByCriteria transferQuery = new QueryByCriteria(IepData.class, transferCriteria);
        transferQuery.addOrderByAscending(IepData.COL_STUDENT_OID);
        transferQuery.addOrderByDescending(IepData.COL_END_DATE);

        m_iepDataTransferStudents =
                getBroker().getGroupedCollectionByQuery(transferQuery, IepData.COL_STUDENT_OID, 1024);
    }

    /**
     * Loads the IEP and IEP disability lookup objects for students included in the passed criteria.
     *
     * @param studentCriteria Criteria
     */
    private void loadSpedData(Criteria studentCriteria) {
        String[] columns = new String[] {X2BaseBean.COL_OID,
                IepData.COL_MEETING_TYPE_CODE};

        String[] aliases = new String[] {IEP_EDUCATIONAL_ENVIRONMENT,
                IEP_EDUCATIONAL_ENVIRONMENT_EC,
                IEP_LEVEL_OF_NEED,
                IEP_PLEPA_SPECIAL_INSTRUCTION_CONTENT,
                IEP_PLEPA_SPECIAL_INSTRUCTION_METHODOLOGY,
                IEP_PLEPA_SPECIAL_INSTRUCTION_PERFORMANCE,
                IEP_PLEPB_SPECIAL_INSTRUCTION_CONTENT,
                IEP_PLEPB_SPECIAL_INSTRUCTION_METHODOLOGY,
                IEP_PLEPB_SPECIAL_INSTRUCTION_PERFORMANCE};

        m_iepLookup = new IepLookup(studentCriteria, columns, aliases, m_iepDictionary, getBroker());

        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria disabilityCriteria = new Criteria();
        disabilityCriteria.addIn(IepDisability.REL_IEP_DATA + "." + IepData.COL_STUDENT_OID, subQuery);

        QueryByCriteria disabilityQuery = new QueryByCriteria(IepDisability.class, disabilityCriteria);
        disabilityQuery.addOrderByAscending(IepDisability.COL_IEP_DATA_OID);
        disabilityQuery.addOrderByDescending(IepDisability.COL_PRIMARY_INDICATOR);

        m_iepDisabilityLookup =
                getBroker().getGroupedCollectionByQuery(disabilityQuery, IepDisability.COL_IEP_DATA_OID, 1024);
    }
}
