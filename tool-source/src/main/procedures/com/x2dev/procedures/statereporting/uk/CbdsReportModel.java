/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StaffCalendar;
import com.follett.fsc.core.k12.beans.StaffCalendarDate;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.BeanRetriever;
import com.follett.fsc.core.k12.tools.stateexports.BeanValidator;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportModel;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.tools.stateexports.XMLStateReportData;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.collections.Bag;
import org.apache.commons.collections.bag.HashBag;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The UK's Common Basic Data Set (CBDS) Model.
 * <p>
 * This contains retrievers, validators, and field definitions
 * pertaining to all UK-related exports.
 *
 * @author Follett Software Company
 */
public class CbdsReportModel extends StateReportModel {
    /**
     * Alias on the StudentProgramParticipation table for the Alternative Provision type
     */
    private static final String ALIAS_PGM_AP_TYPE = "DFE AP TYPE";

    /**
     * Alias on the StaffPosition table for the support category
     */
    public static final String ALIAS_SFP_SUPPORT_CATEGORY = "DFE SUPPORT CATEGORY";

    /**
     * Alias for the Authorized and Statistical.
     */
    private static final String ALIAS_AUTHORIZED = "rat-att-std-authorized";

    /**
     * Code for the autumn term
     */
    public static final String CODE_TERM_AUTUMN = "AUT";

    /**
     * Code for the summer term
     */
    public static final String CODE_TERM_SUMMER = "SUM";

    /**
     * Code for the spring term
     */
    public static final String CODE_TERM_SPRING = "SPR";

    /**
     * Code for Pupil Referral Unit (PRU)
     */
    protected static final String CODE_TERM_PRU = "PRU";

    /**
     * Variable for program's end date
     */
    public static final String VAR_PROGRAM_END_DATE = "programEndDate";

    /**
     * Variable for program's start date
     */
    public static final String VAR_PROGRAM_START_DATE = "programStartDate";

    /**
     * Special School. A school that caters to special needs students.
     */
    public static final String CODE_SCHOOL_SPECIAL = "SP";

    /**
     * State code value for <strong>DFE STAFF CATEGORY</strong>,
     * meaning the staff is a qualified teacher.
     */
    public static final String CODE_QUALIFIED_TEACHER = "QT";

    /**
     * This field returns the state code for <strong>DFE SCHOOL PHASE</strong>
     */
    public static final String FIELD_SKL_SCHOOL_PHASE = "200006";

    /**
     * Field ID for the StaffPosition table.
     * <p>
     * Returns <strong>DFE SUPPORT CATEGORY</strong>'s state value
     */
    public static final String FIELD_SFP_SUPPORTCATEGORY = "SUPPORTCATEGORY";

    /**
     * Variable for age at date
     */
    private static final String VAR_AGE_AT_DATE = "ageAtDate";

    /**
     * Variable for absence end date to query absences
     */
    public static final String VAR_ABSENCE_END_DATE = "absenceEndDate";

    /**
     * Variable for absence start date to query absences
     */
    public static final String VAR_ABSENCE_START_DATE = "absenceStartDate";

    /**
     * Variable for annual attendance end date
     */
    private static final String VAR_ANNUAL_ATTENDANCE_END_DATE = "annualAttendanceEndDate";

    /**
     * Variable for the census date
     */
    public static final String VAR_CENSUS_DATE = "censusDate";

    /**
     * Variable for Contract's end date
     */
    public static final String VAR_CONTRACT_END_DATE = "contractEndDate";

    /**
     * Variable for Contracts's start date
     */
    public static final String VAR_CONTRACT_START_DATE = "contractStartDate";

    /**
     * Variable for a yyyy-MM-dd date formatter
     */
    public static final String VAR_DATE_FORMAT = "dateFormat";

    /**
     * Variable for fsm end date to query free school meals
     */
    public static final String VAR_FSM_END_DATE = "fsmEndDate";

    /**
     * Variable for fsm start date to query free school meals
     */
    public static final String VAR_FSM_START_DATE = "fsmStartDate";

    /**
     * Variable for what report the model should be based on
     */
    public static final String VAR_REPORT = "report";

    /**
     * Variable for the school year
     */
    public static final String VAR_SCHOOL_YEAR = "schoolYear";

    /**
     * Variable for what term the model should be based on
     */
    public static final String VAR_TERM = "term";

    /**
     * Variable for the start date for query termly attendance
     */
    public static final String VAR_TERMLY_ATTENDANCE_START_DATE = "termlyAttendanceStartDate";

    /**
     * Variable for the end date to query termly attendance
     */
    public static final String VAR_TERMLY_ATTENDANCE_END_DATE = "termlyAttendanceEndDate";

    /**
     * Variable for the start date for query summer attendance
     */
    public static final String VAR_SUMMER_ATTENDANCE_START_DATE = "summerAttendanceStartDate";

    /**
     * Variable for the end date to query summer attendance
     */
    public static final String VAR_SUMMER_ATTENDANCE_END_DATE = "summerAttendanceEndDate";

    /**
     * Variable for the start date to query termly exclusions
     */
    public static final String VAR_TERMLY_EXCLUSION_START_DATE = "termlyExclusionStartDate";

    /**
     * Variable for the end date to query termly exclusions
     */
    public static final String VAR_TERMLY_EXCLUSION_END_DATE = "termlyExclusionEndDate";

    /**
     * Variable for term's end date
     */
    public static final String VAR_TERM_END_DATE = "termEndDate";

    /**
     * Variable for term's start date
     */
    public static final String VAR_TERM_START_DATE = "termStartDate";

    /**
     * Variable for the previous term's start date
     */
    public static final String VAR_PREVIOUS_TERM__START_DATE = "previousTermStartDate";

    /**
     * Input parameter for the Census Date
     */
    private static final String PARAM_CENSUS_DATE = VAR_CENSUS_DATE;

    /**
     * Input parameter for the Term's Start Date
     */
    private static final String PARAM_TERM_START_DATE = VAR_TERM_START_DATE;

    /**
     * Input parameter for the Term's End Date
     */
    private static final String PARAM_TERM_END_DATE = VAR_TERM_END_DATE;

    /**
     * Input parameter for the FSM's Start Date
     */
    private static final String PARAM_FSM_START_DATE = VAR_FSM_START_DATE;

    /**
     * Input parameter for the FSM's End Date
     */
    // private static final String PARAM_FSM_END_DATE = VAR_FSM_END_DATE;

    /**
     * Input parameter for the Termly Exclusion's Start Date
     */
    private static final String PARAM_TERMLY_EXCLUSION_START_DATE = VAR_TERMLY_EXCLUSION_START_DATE;

    /**
     * Input parameter for the Termly Exclusion's End Date
     */
    private static final String PARAM_TERMLY_EXCLUSION_END_DATE = VAR_TERMLY_EXCLUSION_END_DATE;

    /**
     * Input parameter for the Termly Attendance's Start Date
     */
    private static final String PARAM_TERMLY_ATTENDANCE_START_DATE = VAR_TERMLY_ATTENDANCE_START_DATE;

    /**
     * Input parameter for the Termly Attendance's End Date
     */
    private static final String PARAM_TERMLY_ATTENDANCE_END_DATE = VAR_TERMLY_ATTENDANCE_END_DATE;

    /**
     * Input parameter for the Summer Attendance's Start Date
     */
    private static final String PARAM_SUMMER_ATTENDANCE_START_DATE = VAR_SUMMER_ATTENDANCE_START_DATE;

    /**
     * Input parameter for the Summer Attendance's End Date
     */
    private static final String PARAM_SUMMER_ATTENDANCE_END_DATE = VAR_SUMMER_ATTENDANCE_END_DATE;

    /**
     * Input parameter for the Annual Attendance's End Date
     */
    private static final String PARAM_ANNUAL_ATTENDANCE_END_DATE = VAR_ANNUAL_ATTENDANCE_END_DATE;

    /**
     * Input parameter for the Contract's Start Date
     */
    private static final String PARAM_CONTRACT_START_DATE = VAR_CONTRACT_START_DATE;

    /**
     * Input parameter for the Contract's End Date
     */
    private static final String PARAM_CONTRACT_END_DATE = VAR_CONTRACT_END_DATE;

    /**
     * Input parameter for the Absence's Start Date
     */
    private static final String PARAM_ABSENCE_START_DATE = VAR_ABSENCE_START_DATE;

    /**
     * Input parameter for the Absence's End Date
     */
    private static final String PARAM_ABSENCE_END_DATE = VAR_ABSENCE_END_DATE;

    /**
     * Input parameter for the Program Start Date
     */
    private static final String PARAM_PROGRAM_START_DATE = VAR_PROGRAM_START_DATE;

    /**
     * Input parameter for the Program End Date
     */
    private static final String PARAM_PROGRAM_END_DATE = VAR_PROGRAM_END_DATE;

    /**
     * Variable for the previous term's start date
     */
    public static final String PARAM_PREVIOUS_TERM__START_DATE = VAR_PREVIOUS_TERM__START_DATE;

    /**
     * Variable for the age at date
     */
    private static final String PARAM_AGE_AT_DATE = VAR_AGE_AT_DATE;

    /**
     * Standard date formatter (yyyy-MM-dd)
     * <p>
     * This is so we don't get confused with MM/dd/yyyy and dd/MM/yyyy.
     */
    private static final SimpleDateFormat VAL_FORMATTER = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * Constructor for UK Model.
     */
    public CbdsReportModel() {
        set(VAR_DATE_FORMAT, getFormatter());
    }

    /**
     * 'ALTPROVISION'
     *
     * Retrieve Alternative Provision program values.
     *
     * @author Follett Software Company
     */
    public class RetrieveAlternativeProvision extends BeanRetriever {
        /**
         * Map of students and their alternative provision programs
         */
        private Map<String, StudentProgramParticipation> m_apMap = new HashMap<String, StudentProgramParticipation>();

        /**
         * Constructor.
         *
         * Needs report-able student criteria to initialize stdOid, collection of pgm map
         *
         * @param params Object[]
         */
        public RetrieveAlternativeProvision(Object[] params) {
            X2Criteria studentCriteria = (X2Criteria) params[0];
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            String apTypeJavaName = getData().translateAliasToJavaName(ALIAS_PGM_AP_TYPE, true);
            X2Criteria pgmCriteria = new X2Criteria();
            pgmCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, stdSubQuery);
            pgmCriteria.addNotEmpty(apTypeJavaName, getBroker().getPersistenceKey());

            BeanQuery apQuery = new BeanQuery(StudentProgramParticipation.class, pgmCriteria);
            m_apMap = getBroker().getMapByQuery(apQuery, StudentProgramParticipation.COL_STUDENT_OID, 64);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object result = null;
            SisStudent student = (SisStudent) bean;
            String stdOid = student.getOid();
            StudentProgramParticipation pgm = m_apMap.get(stdOid);

            if (pgm != null) {
                /* Check not null to avoid NullPointerException */
                result = data.getFieldValue(pgm, "APTYPE");
            } else {
                result = null;
            }

            return result;
        }
    }

    /**
     * The Class RetrieveSessionDetail.
     */
    public class RetrieveSessionDetail extends BeanRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            return null;
        }

    }

    /**
     * 'ATTENDANCE'
     *
     * Retrieve attendance related stuff.
     *
     * @author Follett Software Company
     */
    public class RetrieveAttendance extends BeanRetriever {
        /**
         * Regular expression string to match field parameter
         */
        public static final String REGEX_PATTERN_CALC_PARAM =
                "(termly|annual|summer),(authorised|unauthorised|possible|codeCSessions|codeESessions|codeFSessions|codeGSessions|codeHSessions|codeISessions|codeMSessions|codeNSessions|codeOSessions|codeRSessions|codeSSessions|codeTSessions|codeUSessions)";

        /**
         * <tt><strong>(termly|annual|summer)</strong>,<strong>(authorised|unauthorised|possible)</strong></tt>
         */
        private final Pattern m_calcPattern = Pattern.compile(REGEX_PATTERN_CALC_PARAM);

        /**
         * Map of students and their termly attendance information
         */
        private Map<String, AttendanceInfo> m_termlyAttendanceMap = new HashMap<String, AttendanceInfo>();

        /**
         * Map of students and their annual attendance information
         */
        private Map<String, AttendanceInfo> m_annualAttendanceMap = new HashMap<String, AttendanceInfo>();

        /**
         * Map of students and their summer attendance information
         */
        private Map<String, AttendanceInfo> m_summerAttendanceMap = new HashMap<String, AttendanceInfo>();

        private Collection<SisStudent> m_students;

        private StudentHistoryHelper m_helper;

        private Map<String, RefAttendanceStudent> m_refAttMap;

        private DataDictionary m_refAttStdDictionary;

        /**
         * Constructor.
         *
         * @param params Object[]
         */
        public RetrieveAttendance(Object[] params) {
            m_students = (Collection<SisStudent>) params[0];
            m_helper = (StudentHistoryHelper) params[1];

            // Get the extended data dictionary (for Authorized, Statistical, and Explained)
            ReferenceTable refAttStdTabl = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                    ReferenceTable.REF_TABLE_OID_STUDENT_ATTENDANCE);
            ExtendedDataDictionary refAttStdDdx = refAttStdTabl.getExtendedDataDictionary();
            m_refAttStdDictionary = DataDictionary.getDistrictDictionary(refAttStdDdx, getBroker().getPersistenceKey());

            // Load the refAttMap (Reference Attendance Student Code)
            Criteria criteria = new Criteria();
            criteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE,
                    Integer.valueOf(RefAttendanceStudent.ATTENDANCE_TYPE_DAILY));
            criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE,
                    Integer.valueOf(RefAttendanceStudent.TYPE_OTHER_CODE));
            QueryByCriteria query = new QueryByCriteria(RefAttendanceStudent.class, criteria);
            m_refAttMap = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 30);

            countAnnualAttendances();
            if ("SchoolCensus".equals(get(VAR_REPORT)) && !CODE_TERM_PRU.equals(get(VAR_TERM))) {
                countAttendancesByTerm(VAR_TERMLY_ATTENDANCE_START_DATE, VAR_TERMLY_ATTENDANCE_END_DATE, false);
                if (CODE_TERM_AUTUMN.equals(get(VAR_TERM))) {
                    countAttendancesByTerm(VAR_SUMMER_ATTENDANCE_START_DATE, VAR_SUMMER_ATTENDANCE_END_DATE, true);
                }
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, X2BaseBean bean, FieldDefinition field) {
            Object value = null;

            SisStudent student = (SisStudent) bean;
            String studentOid = student.getOid();

            String calcParam = (String) field.getParameter();
            Matcher m = m_calcPattern.matcher(calcParam);
            String group1 = null;
            String group2 = null;
            if (m.find()) {
                group1 = m.group(1);
                group2 = m.group(2);
            }

            AttendanceInfo attendanceInfo = null;
            if (!StringUtils.isEmpty(group1)) {
                if ("termly".equals(group1)
                        && ("SchoolCensus".equals(get(VAR_REPORT)) && !CODE_TERM_PRU.equals(get(VAR_TERM)))) {
                    attendanceInfo = m_termlyAttendanceMap.get(studentOid);
                } else if ("annual".equals(group1)) {
                    attendanceInfo = m_annualAttendanceMap.get(studentOid);
                } else if ("summer".equals(group1)) {
                    attendanceInfo = m_summerAttendanceMap.get(studentOid);
                }
            }

            if (!StringUtils.isEmpty(group2) && attendanceInfo != null) {
                if ("possible".equals(group2)) {
                    value = Integer.valueOf(attendanceInfo.m_sessionsPossible);
                }
                Integer absences = null; // attendanceInfo.m_sessionDetail.get(reasonCode);

                if ("codeISessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("I");
                    value = absences;
                } else if ("codeMSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("M");
                    value = absences;
                } else if ("codeRSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("R");
                    value = absences;
                } else if ("codeSSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("S");
                    value = absences;
                } else if ("codeTSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("T");
                    value = absences;
                } else if ("codeHSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("H");
                    value = absences;
                } else if ("codeFSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("F");
                    value = absences;
                } else if ("codeESessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("E");
                    value = absences;
                } else if ("codeCSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("C");
                    value = absences;
                } else if ("codeGSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("G");
                    value = absences;
                } else if ("codeNSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("N");
                    value = absences;
                } else if ("codeOSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("O");
                    value = absences;
                } else if ("codeUSessions".equals(group2)) {
                    absences = attendanceInfo.m_sessionDetail.get("U");
                    value = absences;
                } else if ("authorised".equals(group2)) {
                    value = Integer.valueOf(attendanceInfo.m_sessionsAuthorised);
                } else if ("unauthorised".equals(group2)) {
                    value = Integer.valueOf(attendanceInfo.m_sessionsUnauthorised);
                }
            } else {
                value = Integer.valueOf(0);
            }

            return value;
        }

        /**
         * Count up student's annual attendances (from last year).
         */
        private void countAnnualAttendances() {
            // get last year's context
            int currentSchoolYear = getData().getCurrentContext().getSchoolYear();
            X2Criteria ctxCriteria = new X2Criteria();
            ctxCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(currentSchoolYear - 1));
            BeanQuery contextQuery = new BeanQuery(DistrictSchoolYearContext.class, ctxCriteria);
            DistrictSchoolYearContext lastYearContext =
                    (DistrictSchoolYearContext) getBroker().getBeanByQuery(contextQuery);

            // set the start and end date based off of last year's context
            if (lastYearContext != null) {
                PlainDate lastYearStartDate = lastYearContext.getStartDate();
                PlainDate lastYearEndDate = (PlainDate) get(VAR_ANNUAL_ATTENDANCE_END_DATE);
                if (lastYearEndDate == null) {
                    lastYearEndDate = lastYearContext.getEndDate();
                }

                StudentHistoryHelper helper = new StudentHistoryHelper(getData());
                helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, lastYearStartDate);
                helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, lastYearEndDate);

                QueryByCriteria studentQuery = helper.getStudentQuery(false);
                Collection<SisStudent> students = getBroker().getCollectionByQuery(studentQuery);

                for (SisStudent student : students) {
                    AttendanceInfo info = new AttendanceInfo();
                    List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(student, false);
                    StudentEnrollment entryEnrollment =
                            m_helper.getEnrollmentForDate(student.getOid(), lastYearEndDate, "E");
                    PlainDate entryDate = null;
                    int studentAgeAsOfAug31 = getStudentAgeAsOfAug31(student);
                    if (4 <= studentAgeAsOfAug31 && studentAgeAsOfAug31 <= 15) {
                        if (entryEnrollment != null && entryEnrollment.getEnrollmentDate().before(lastYearEndDate)) {
                            entryDate = entryEnrollment.getEnrollmentDate();
                            for (StudentEnrollmentSpan span : spans) {
                                Collection<PlainDate> inSessionDates =
                                        CalendarManager.getInSessionDates(lastYearStartDate,
                                                lastYearEndDate,
                                                student,
                                                getBroker());

                                PlainDate lastActiveDate = span.getLastActiveDate();
                                if (lastActiveDate == null) {
                                    lastActiveDate = lastYearEndDate;
                                }
                                for (PlainDate date : inSessionDates) {
                                    if (!date.before(entryDate) && !date.after(lastActiveDate)) {
                                        info.m_sessionsPossible += 2;
                                    }
                                }

                                List<StudentAttendance> attendances = null;
                                try {
                                    attendances = span.getStudentAttendance();
                                } catch (Exception e) {
                                    attendances = new ArrayList<StudentAttendance>();
                                }

                                for (StudentAttendance attendance : attendances) {
                                    String otherCodeAM = attendance.getOtherCode();
                                    String otherCodePM = attendance.getOtherCode02();
                                    boolean areOtherCodesEqual = false;
                                    if (StringUtils.isEqual(otherCodeAM, otherCodePM)) {
                                        areOtherCodesEqual = true;
                                    }
                                    int absenceSessionsAM;
                                    int absenceSessionsPM;
                                    if (info.m_sessionDetail.containsKey(otherCodeAM)) {
                                        absenceSessionsAM = info.m_sessionDetail.get(otherCodeAM).intValue();
                                    } else {
                                        absenceSessionsAM = 0;
                                    }

                                    if (info.m_sessionDetail.containsKey(otherCodePM)) {
                                        absenceSessionsPM = info.m_sessionDetail.get(otherCodePM).intValue();
                                    } else {
                                        absenceSessionsPM = 0;
                                    }

                                    if (isAuthorizedAbsent(otherCodeAM) || isUnAuthorizedAbsent(otherCodeAM)) {
                                        absenceSessionsAM++;
                                        info.m_sessionDetail.put(otherCodeAM, Integer.valueOf(absenceSessionsAM));
                                    }

                                    if (isAuthorizedAbsent(otherCodePM) || isUnAuthorizedAbsent(otherCodePM)) {
                                        if (areOtherCodesEqual) {
                                            absenceSessionsAM++;
                                            info.m_sessionDetail.put(otherCodeAM, Integer.valueOf(absenceSessionsAM));
                                        } else {
                                            absenceSessionsPM++;
                                            info.m_sessionDetail.put(otherCodePM, Integer.valueOf(absenceSessionsPM));
                                        }
                                    }

                                    /*
                                     * ------------------------------------------
                                     * AM
                                     */
                                    if (isAuthorizedAbsent(otherCodeAM)) {
                                        info.m_sessionsAuthorised++;
                                    } else if (isUnAuthorizedAbsent(otherCodeAM)) {
                                        info.m_sessionsUnauthorised++;
                                    }

                                    /*
                                     * -------------------------------------------
                                     * PM
                                     */
                                    if (isAuthorizedAbsent(otherCodePM)) {
                                        info.m_sessionsAuthorised++;
                                    } else if (isUnAuthorizedAbsent(otherCodePM)) {
                                        info.m_sessionsUnauthorised++;
                                    }
                                }
                            }
                            m_annualAttendanceMap.put(student.getOid(), info);
                        }
                    }
                }

            }
        }

        /**
         * Returns an excused indicator based on the other code provided.
         *
         * Note: these fields (authorized, statistical, and explained) must be defined as "logical"
         * in extended data
         * dictionary, otherwise you will get exception.
         *
         * @param attendanceCode String
         * @return boolean
         */
        private boolean isAuthorizedAbsent(String attendanceCode) {
            boolean isAuthorized = false;
            if (m_refAttMap != null) {
                RefAttendanceStudent refAttStd = m_refAttMap.get(attendanceCode);
                if (refAttStd != null && refAttStd.getAbsentIndicator()) {
                    Boolean tempAuthorizedCode =
                            (Boolean) refAttStd.getFieldValueByAliasExtended(ALIAS_AUTHORIZED, m_refAttStdDictionary);
                    if (tempAuthorizedCode != null && tempAuthorizedCode.booleanValue()) {
                        isAuthorized = true;
                    }
                }
            }

            return isAuthorized;
        }

        /**
         * Returns an excused indicator based on the other code provided.
         *
         *
         * Note: these fields (authorized, statistical, and explained) must be defined as "logical"
         * in extended data
         * dictionary, otherwise you will get exception.
         *
         * @param attendanceCode String
         * @return boolean
         */
        private boolean isUnAuthorizedAbsent(String attendanceCode) {
            boolean isUnAuthorized = false;
            if (m_refAttMap != null) {
                RefAttendanceStudent refAttStd = m_refAttMap.get(attendanceCode);
                if (refAttStd != null && refAttStd.getAbsentIndicator()) {
                    Boolean tempAuthorizedCode =
                            (Boolean) refAttStd.getFieldValueByAliasExtended(ALIAS_AUTHORIZED, m_refAttStdDictionary);
                    if (tempAuthorizedCode != null && !tempAuthorizedCode.booleanValue()) {
                        isUnAuthorized = true;
                    }
                }
            }

            return isUnAuthorized;
        }


        /**
         * Count up students' attendances by term.
         *
         * @param startDate String
         * @param endDate String
         * @param isSummer boolean
         */
        private void countAttendancesByTerm(String startDate, String endDate, boolean isSummer) {
            PlainDate attendanceByTermStartDate = (PlainDate) get(startDate);
            PlainDate attendanceByTermEndDate = (PlainDate) get(endDate);
            PlainDate termEndDate = (PlainDate) get(VAR_TERM_END_DATE);

            X2Criteria attendanceCriteria = new X2Criteria();
            attendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, attendanceByTermStartDate);
            attendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, attendanceByTermEndDate);
            attendanceCriteria.addEqualTo(StudentAttendance.REL_SCHOOL, getData().getSchool().getOid());
            BeanQuery attendanceQuery = new BeanQuery(StudentAttendance.class, attendanceCriteria);
            Map<String, Collection<StudentAttendance>> stdAttsMap =
                    getBroker().getGroupedCollectionByQuery(attendanceQuery, StudentAttendance.COL_STUDENT_OID, 128);

            for (SisStudent student : m_students) {
                AttendanceInfo info = new AttendanceInfo();
                List<StudentEnrollmentSpan> spans = m_helper.getStudentEnrollmentSpans(student, false);
                StudentEnrollment entryEnrollment = m_helper.getEnrollmentForDate(student.getOid(), termEndDate, "E");
                PlainDate entryDate = null;

                int studentAgeAsOfAug31 = getStudentAgeAsOfAug31(student);

                if (4 <= studentAgeAsOfAug31 && studentAgeAsOfAug31 <= 15) {
                    if (entryEnrollment != null
                            && entryEnrollment.getEnrollmentDate().before(attendanceByTermEndDate)) {
                        entryDate = entryEnrollment.getEnrollmentDate();
                        for (StudentEnrollmentSpan span : spans) {
                            Collection<PlainDate> inSessionDates =
                                    CalendarManager.getInSessionDates(attendanceByTermStartDate,
                                            attendanceByTermEndDate,
                                            student,
                                            getBroker());

                            PlainDate lastActiveDate = span.getLastActiveDate();
                            if (lastActiveDate == null) {
                                lastActiveDate = attendanceByTermEndDate;
                            }
                            for (PlainDate date : inSessionDates) {
                                if (!date.before(entryDate) && !date.after(lastActiveDate)) {
                                    info.m_sessionsPossible += 2;
                                }
                            }

                            Collection<StudentAttendance> attendances = stdAttsMap.get(student.getOid());
                            if (attendances != null) {
                                for (StudentAttendance attendance : attendances) {
                                    String otherCodeAM = attendance.getOtherCode();
                                    String otherCodePM = attendance.getOtherCode02();
                                    boolean areOtherCodesEqual = false;

                                    if (StringUtils.isEqual(otherCodeAM, otherCodePM)) {
                                        areOtherCodesEqual = true;
                                    }
                                    int absenceSessionsAM;
                                    int absenceSessionsPM;
                                    if (info.m_sessionDetail.containsKey(otherCodeAM)) {
                                        absenceSessionsAM = info.m_sessionDetail.get(otherCodeAM).intValue();
                                    } else {
                                        absenceSessionsAM = 0;
                                    }

                                    if (info.m_sessionDetail.containsKey(otherCodePM)) {
                                        absenceSessionsPM = info.m_sessionDetail.get(otherCodePM).intValue();
                                    } else {
                                        absenceSessionsPM = 0;
                                    }

                                    PlainDate date = attendance.getDate();
                                    if (!date.before(attendanceByTermStartDate)
                                            && !date.after(attendanceByTermEndDate)) {
                                        if (isAuthorizedAbsent(otherCodeAM) || isUnAuthorizedAbsent(otherCodeAM)) {
                                            absenceSessionsAM++;
                                            info.m_sessionDetail.put(otherCodeAM, Integer.valueOf(absenceSessionsAM));
                                        }

                                        if (isAuthorizedAbsent(otherCodePM) || isUnAuthorizedAbsent(otherCodePM)) {
                                            if (areOtherCodesEqual) {
                                                absenceSessionsAM++;
                                                info.m_sessionDetail.put(otherCodeAM, Integer.valueOf(absenceSessionsAM));
                                            } else {
                                                absenceSessionsPM++;
                                                info.m_sessionDetail.put(otherCodePM, Integer.valueOf(absenceSessionsPM));
                                            }
                                        }

                                        /*
                                         * -------------------------------------------
                                         * AM
                                         */
                                        if (isAuthorizedAbsent(otherCodeAM)) {
                                            info.m_sessionsAuthorised++;
                                        } else if (isUnAuthorizedAbsent(otherCodeAM)) {
                                            info.m_sessionsUnauthorised++;
                                        }

                                        /*
                                         * --------------------------------------------
                                         * PM
                                         */

                                        if (isAuthorizedAbsent(otherCodePM)) {
                                            info.m_sessionsAuthorised++;
                                        } else if (isUnAuthorizedAbsent(otherCodePM)) {
                                            info.m_sessionsUnauthorised++;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if (isSummer) {
                        m_summerAttendanceMap.put(student.getOid(), info);
                    } else {
                        m_termlyAttendanceMap.put(student.getOid(), info);
                    }
                }
            }
        }
    }

    /**
     * 'CLASS'
     *
     * Retrieve class (MasterSchedule) information.
     */
    public class RetrieveClass extends BeanRetriever {
        /**
         * Field parameter to return the number of guest pupils in this section
         */
        public static final String PARAM_GUEST_PUPILS = "GuestPupils";

        /**
         * Field parameter to return the number of home pupils in this section
         */
        public static final String PARAM_HOME_PUPILS = "HomePupils";

        /**
         * Field parameter to return the number of non teachers in this section
         */
        public static final String PARAM_NON_TEACHERS = "NonTeachers";

        /**
         * Field parameter to return the number of teachers in this section
         */
        public static final String PARAM_TEACHERS_IN_CLASS = "TeachersInClass";

        /**
         * Bag of guest pupils. Used for mstOid counting where std is not enrolled in mst's school
         */
        private Bag m_guestPupils = new HashBag();

        /**
         * Bag of home pupils Used for mstOid counting where std is enrolled in mst's school
         */
        private Bag m_homePupils = new HashBag();

        /**
         * Bag of non-teachers. Used for mstOid counting where mtc is not a teacher
         */
        private Bag m_nonTeachersBag = new HashBag();

        /**
         * Bag of teachers. Used for mstOid counting where mtc is a teacher type
         */
        private Bag m_teachersBag = new HashBag();


        /**
         * Instantiates a new retrieve class.
         *
         * @param params Object[]
         */
        public RetrieveClass(Object[] params) {
            Collection<SisStudent> students = (Collection<SisStudent>) params[0];
            StudentHistoryHelper helper = (StudentHistoryHelper) params[1];
            Map<String, List<MasterSchedule>> studentSchedules = (Map<String, List<MasterSchedule>>) params[2];
            countPupils(students, helper, studentSchedules);
            countStaff();
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object result = null;
            MasterSchedule mst = (MasterSchedule) bean;
            String mstOid = mst.getOid();
            String fieldId = field.getFieldId();
            String calcParam = (String) field.getParameter();

            /*
             * Count up the number of teachers for this section
             */
            if ("200190".equals(fieldId) || PARAM_TEACHERS_IN_CLASS.equalsIgnoreCase(calcParam)) {
                result = Integer.valueOf(m_teachersBag.getCount(mstOid));
            }

            /*
             * Count up the number of non-teachers for this section
             */
            else if ("200191".equals(fieldId) || PARAM_NON_TEACHERS.equalsIgnoreCase(calcParam)) // Non-teachers
            {
                result = Integer.valueOf(m_nonTeachersBag.getCount(mstOid));
            }

            /*
             * Count up the home pupils (pupils who actually go
             * to the school where the class is in)
             */
            else if ("200201".equals(fieldId) || PARAM_HOME_PUPILS.equalsIgnoreCase(calcParam)) {
                result = Integer.valueOf(m_homePupils.getCount(mstOid));
            }

            /*
             * Count up the guest pupils (pupils who go to this class, but
             * don't actually go to the school where the class is at)
             */
            else if ("200202".equals(fieldId) || PARAM_GUEST_PUPILS.equalsIgnoreCase(calcParam)) {
                result = Integer.valueOf(m_guestPupils.getCount(mstOid));
            }

            return result;
        }

        /**
         * Count up all the pupils and see if they're home (they're enrolled in the same school the
         * class
         * takes place in) or guest (they're not enrolled in the same school the class takes place
         * in).
         *
         * @param students Collection<SisStudent>
         * @param helper StudentHistoryHelper
         * @param studentSchedules Map<String,List<MasterSchedule>>
         */
        private void countPupils(Collection<SisStudent> students,
                                 StudentHistoryHelper helper,
                                 Map<String, List<MasterSchedule>> studentSchedules) {
            PlainDate startDate = (PlainDate) get(VAR_TERM_START_DATE);
            PlainDate endDate = (PlainDate) get(VAR_TERM_END_DATE);

            for (SisStudent student : students) {
                StudentEnrollment recentEnrollment = helper.getEnrollmentForDate(student.getOid(),
                        endDate,
                        "EWS");

                List<MasterSchedule> schedules = studentSchedules.get(student.getOid());
                List<String> sectionsAdded = new ArrayList<String>();
                if (recentEnrollment != null) {
                    String studentEnrolledSchoolOid = recentEnrollment.getSchoolOid();
                    List<StudentScheduleSpan> spans = helper.getStudentScheduleSpans(student);
                    for (StudentScheduleSpan span : spans) {
                        PlainDate entryDate = span.getEntryDate();
                        PlainDate exitDate = span.getExitDate();
                        if ((entryDate != null && entryDate.before(endDate))
                                || (exitDate != null && exitDate.after(startDate))) {
                            MasterSchedule section = span.getSection();
                            if (section != null) {
                                String mstOid = section.getOid();
                                String mstCskSklOid = section.getSchoolCourse().getSchoolOid();

                                if (schedules != null) {
                                    boolean hasSection = false;
                                    for (MasterSchedule sched : schedules) {
                                        if (mstOid.equals(sched.getOid())) {
                                            hasSection = true;
                                            break;
                                        }
                                    }

                                    if (hasSection) {
                                        if (mstCskSklOid.equals(studentEnrolledSchoolOid)) {
                                            if (!sectionsAdded.contains(mstOid)) {
                                                sectionsAdded.add(mstOid);
                                                m_homePupils.add(mstOid);
                                            }
                                        } else {
                                            m_guestPupils.add(mstOid);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

            }
        }

        /**
         * Count up the staff for each class and see if they're teachers or non-teachers.
         */
        private void countStaff() {
            SisSchool school = (SisSchool) getData().getSchool();

            /*
             * Find all the master schedules that are within the school and school's active schedule
             */
            X2Criteria classesCriteria = new X2Criteria();
            classesCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.COL_SCHOOL_OID,
                    school.getOid());
            classesCriteria.addEqualTo(MasterSchedule.COL_SCHEDULE_OID, school.getActiveScheduleOid());
            SubQuery mstSubQuery = new SubQuery(MasterSchedule.class, X2BaseBean.COL_OID, classesCriteria);

            /*
             * Then find all the teacher types (by checking the local code if they equal or not
             * equal "Teacher")
             * and add them to the 'teacherTypes' and 'nonTeacherTypes' Set.
             */
            Set<String> teacherTypes = new HashSet<String>();
            DataDictionaryField staffTypeField =
                    getData().getDataDictionaryField(SisStaff.class, SisStaff.COL_STAFF_TYPE);
            if (staffTypeField != null) {
                ReferenceTable staffTypeRefTable = staffTypeField.getReferenceTable();
                if (staffTypeRefTable != null) {
                    Map<String, ReferenceCode> refCodeMap = staffTypeRefTable.getCodeMap();
                    Collection<ReferenceCode> referenceCodes = refCodeMap.values();
                    for (ReferenceCode referenceCode : referenceCodes) {
                        String code = referenceCode.getCode();
                        if ("Teacher".equals(referenceCode.getLocalCode())) {
                            teacherTypes.add(code);
                        }
                    }
                } else {
                    teacherTypes.add("Teacher");
                    getData().addSetupError("Missing reference table",
                            "Staff Type field is missing a reference table!");
                }
            }

            /*
             * Load the teachers and non-teachers map.
             *
             * Iterate through the schedule teachers and see if that attached staff's type
             * is equal to one of the types in "teacherTypes" (see above code). If it is,
             * add it to "m_teachersMap", if not add it to "m_nonTeachersMap"
             */
            X2Criteria teacherCriteria = new X2Criteria();
            teacherCriteria.addIn(ScheduleTeacher.COL_SECTION_OID, mstSubQuery);
            BeanQuery teacherQuery = new BeanQuery(ScheduleTeacher.class, teacherCriteria);

            Collection<ScheduleTeacher> mtcs = getBroker().getCollectionByQuery(teacherQuery);
            if (mtcs != null) {
                for (ScheduleTeacher mtc : mtcs) {
                    String mstOid = mtc.getSectionOid();
                    SisStaff stf = mtc.getStaff();
                    if (stf != null) {
                        String staffType = stf.getStaffType();
                        if (!StringUtils.isEmpty(staffType)) {
                            if (teacherTypes.contains(staffType)) {
                                m_teachersBag.add(mstOid);
                            } else {
                                m_nonTeachersBag.add(mstOid);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * 'EXCLUSION'
     *
     * Retrieve exclusion fields from students.
     *
     * @author Follett Software Company
     */
    public class RetrieveExclusion extends BeanRetriever {
        /**
         * Alias on the ConductAction table to indicate if the student was still in-care
         */
        public static final String ALIAS_DFE_IN_CARE_INDICATOR = "DFE IN CARE INDICATOR";

        /**
         * Field ID for the ConductAction table.
         * <p>
         * Returns <strong>DFE EXCLUSION CATEGORY</strong>'s state value.
         */
        public static final String FIELD_ACT_EXCLUSIONCATEGORY = "EXCLUSIONCATEGORY";

        /**
         * Retriever parameter to return a conduct incident's category
         */
        public static final String PARAM_CATEGORY = "Category";

        /**
         * Retriever parameter to return a conduct incident's sessions
         */
        public static final String PARAM_SESSIONS = "Sessions";

        /**
         * Retriever parameter to return a conduct incident's student's in-care indictaor
         */
        public static final String PARAM_IN_CARE = "InCare";

        /**
         * Map of conduct incident's Collection of actions
         */
        private Map<String, Collection<ConductAction>> m_actionMap;

        /**
         * Instantiates a new retrieve exclusion.
         *
         * @param params Object[]
         */
        public RetrieveExclusion(Object[] params) {
            m_actionMap = (Map<String, Collection<ConductAction>>) params[0];
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object value = null;
            ConductIncident incident = (ConductIncident) bean;
            String calcParam = (String) field.getParameter();

            /*
             * Get the incident's conduct action and retrieve the value from DFE EXCLUSION CATEGORY
             */
            if (PARAM_CATEGORY.equalsIgnoreCase(calcParam)) {
                Collection<ConductAction> actions = m_actionMap.get(incident.getOid());
                if (actions != null && !actions.isEmpty()) {
                    for (ConductAction action : actions) {
                        value = data.getFieldValue(action, FIELD_ACT_EXCLUSIONCATEGORY);
                        if (!StringUtils.isEmpty((String) value)) {
                            break;
                        }
                    }
                }
            }

            /*
             * Return the sum of penalty time from the incident's conduct actions
             */
            else if (PARAM_SESSIONS.equalsIgnoreCase(calcParam)) {
                Collection<ConductAction> actions = m_actionMap.get(incident.getOid());
                BigDecimal sum = BigDecimal.ZERO;
                if (actions != null && !actions.isEmpty()) {
                    for (ConductAction action : actions) {
                        BigDecimal actionPenaltyTime = action.getActionPenaltyTime();
                        if (actionPenaltyTime != null) {
                            sum = sum.add(actionPenaltyTime);
                        } else {
                            // by default, just add 1
                            sum = sum.add(BigDecimal.ONE);
                        }
                    }
                }
                value = sum;
            }

            /*
             * Check if the child is 'looked after' during his/her exclusion ('DFE IN CARE
             * INDICATOR')
             */
            else if (PARAM_IN_CARE.equalsIgnoreCase(calcParam)) {
                value = Boolean.FALSE;
                Collection<ConductAction> actions = m_actionMap.get(incident.getOid());
                if (actions != null && !actions.isEmpty()) {
                    for (ConductAction action : actions) {
                        String inCareAsString = (String) action.getFieldValueByAlias(ALIAS_DFE_IN_CARE_INDICATOR);
                        if (BooleanAsStringConverter.TRUE.equals(inCareAsString)) {
                            value = Boolean.TRUE;
                        }
                    }
                }
            }

            return value;
        }
    }

    /**
     * 'FSMELIGIBLE'
     *
     * Retrieve if a student has free meals.
     */
    public class RetrieveFSMEligibility extends BeanRetriever {

        /**
         * Map of students' Collection of Free School Meal programs
         */
        private Map<String, Collection<StudentProgramParticipation>> m_fsmMap;

        /**
         * Instantiates a new retrieve FSM eligibility.
         *
         * @param params Object[]
         */
        public RetrieveFSMEligibility(Object[] params) {
            m_fsmMap = (Map<String, Collection<StudentProgramParticipation>>) params[0];
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            SisStudent student = (SisStudent) bean;
            String studentOid = student.getOid();

            Collection<StudentProgramParticipation> pgms = m_fsmMap.get(studentOid);
            Boolean result = null;
            if (pgms == null || pgms.isEmpty()) {
                result = Boolean.FALSE;
            } else {
                result = Boolean.TRUE;
            }

            return result;
        }
    }

    /**
     * The Class RetrieveHoursAtSetting.
     */
    public class RetrieveHoursAtSetting extends BeanRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Number result = null;

            SisStudent student = (SisStudent) bean;
            int studentAgeAsOfAug31 = getStudentAgeAsOfAug31(student);
            if (studentAgeAsOfAug31 < 5) {
                String hoursSettingAsString = (String) student.getFieldValueByAlias("DFE HOURS SETTING");
                if (StringUtils.isEmpty(hoursSettingAsString)) {
                    hoursSettingAsString = "0";
                }
                result = new BigDecimal(hoursSettingAsString);
            }

            return result;
        }
    }

    /**
     * 'LEARNINGAIM'
     *
     * Retrieve Learning Aim values.
     *
     * @author Follett Software Company
     */
    public class RetrieveLearningAim extends BeanRetriever {
        private Map<String, Collection<StudentScheduleChange>> m_studentScheduleChangeMap;
        private Collection<MasterSchedule> m_masterScheduleList;
        private HashMap m_sectionTermMap = new HashMap();
        private Map<String, Collection<ScheduleTermDate>> m_scheduleTermDatesMap;

        /**
         * Constructor.
         *
         * @param params params[0] = x2criteria studentCriteria
         */
        public RetrieveLearningAim(Object[] params) {
            X2Criteria stdCriteria = (X2Criteria) params[0];
            HashSet activeScheduleOids = (HashSet<String>) params[1];

            // Create a list of StudentOid
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

            // Get All sectionOids used in the QAN coded StudentSchedule
            X2Criteria studentScheduleCriteria = new X2Criteria();
            studentScheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, stdSubQuery);
            studentScheduleCriteria.addIn(StudentSchedule.COL_SCHEDULE_OID, activeScheduleOids);
            studentScheduleCriteria.addNotNull(StudentSchedule.COL_QAN_CODE_OID);
            SubQuery sectionSubQuery =
                    new SubQuery(StudentSchedule.class, StudentSchedule.COL_SECTION_OID, studentScheduleCriteria);
            Collection<String> sectionOids = getBroker().getSubQueryCollectionByQuery(sectionSubQuery);
            HashSet uniqueSectionOids = new HashSet(sectionOids);

            if (uniqueSectionOids != null && uniqueSectionOids.size() > 0) {
                X2Criteria masterScheduleCriteria = new X2Criteria();
                masterScheduleCriteria.addIn(X2BaseBean.COL_OID, uniqueSectionOids);
                QueryByCriteria masterScheduleQuery = new QueryByCriteria(MasterSchedule.class, masterScheduleCriteria);
                m_masterScheduleList = getBroker().getCollectionByQuery(masterScheduleQuery);
                for (MasterSchedule masterSchedule : m_masterScheduleList) {
                    m_sectionTermMap.put(masterSchedule.getOid(), masterSchedule.getScheduleTermOid());
                }

                Collection scheduleTerms = m_sectionTermMap.values();
                X2Criteria scheduleTermDatesCriteria = new X2Criteria();
                scheduleTermDatesCriteria.addIn(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTerms);
                QueryByCriteria scheduleTermDatesQuery =
                        new QueryByCriteria(ScheduleTermDate.class, scheduleTermDatesCriteria);
                m_scheduleTermDatesMap = getBroker().getGroupedCollectionByQuery(scheduleTermDatesQuery,
                        ScheduleTermDate.COL_SCHEDULE_TERM_OID, 128);

                // Get student StudentScheduleChange sections
                X2Criteria studentScheduleChangeCriteria = new X2Criteria();
                studentScheduleChangeCriteria.addIn(StudentScheduleChange.COL_STUDENT_OID, stdSubQuery);
                studentScheduleChangeCriteria.addIn(StudentScheduleChange.COL_SCHEDULE_OID, activeScheduleOids);
                studentScheduleChangeCriteria.addIn(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, uniqueSectionOids);
                QueryByCriteria studentScheduleChangeQuery =
                        new QueryByCriteria(StudentScheduleChange.class, studentScheduleChangeCriteria);
                studentScheduleChangeQuery.addOrderByAscending(StudentScheduleChange.COL_STUDENT_OID);
                studentScheduleChangeQuery.addOrderByAscending(StudentScheduleChange.COL_MASTER_SCHEDULE_OID);
                studentScheduleChangeQuery.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
                m_studentScheduleChangeMap = getBroker().getGroupedCollectionByQuery(studentScheduleChangeQuery,
                        StudentScheduleChange.COL_STUDENT_OID, 128);
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            String calcParam = (String) field.getParameter();

            StudentSchedule studentSchedule = (StudentSchedule) bean;
            String studentOid = studentSchedule.getStudentOid();
            String sectionOid = studentSchedule.getSectionOid();

            String scheduleTermOid = null;
            if (sectionOid != null) {
                scheduleTermOid = (String) m_sectionTermMap.get(sectionOid);
            }
            ScheduleTermDate sectionScheduleTermDate = null;
            if (scheduleTermOid != null) {
                Collection<ScheduleTermDate> scheduleTermDates = m_scheduleTermDatesMap.get(scheduleTermOid);
                for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                    sectionScheduleTermDate = scheduleTermDate;
                }
            }

            Object result = null;

            if ("startDate".equals(calcParam)) {
                // Take the first Start Date for the Schedule Term
                PlainDate scheduleTermStartDate = null;
                if (sectionScheduleTermDate != null) {
                    scheduleTermStartDate = sectionScheduleTermDate.getStartDate();
                }

                Collection<StudentScheduleChange> studentScheduleChanges = m_studentScheduleChangeMap.get(studentOid);
                PlainDate actualStartDate = null;
                if (studentScheduleChanges != null) {
                    for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                        if (sectionOid.equals(studentScheduleChange.getMasterScheduleOid())) {
                            // Selected the latest Start Date
                            if (StudentScheduleChange.CODE_ADD.equals(studentScheduleChange.getChangeTypeCode())) {
                                actualStartDate = studentScheduleChange.getEffectiveDate();
                            }
                        }
                    }
                }

                if (actualStartDate == null) {
                    result = scheduleTermStartDate;
                } else if (scheduleTermStartDate != null) {
                    if (actualStartDate.after(scheduleTermStartDate)) {
                        result = actualStartDate;
                    } else {
                        result = scheduleTermStartDate;
                    }
                }
            } else {
                PlainDate today = new PlainDate();
                // Take the last End Date for the Schedule Term
                PlainDate scheduleTermEndDate = null;
                if (sectionScheduleTermDate != null) {
                    scheduleTermEndDate = sectionScheduleTermDate.getEndDate();
                }

                if ("plannedEndDate".equals(calcParam)) {
                    result = scheduleTermEndDate;
                } else {
                    PlainDate actualEndDate = null;
                    boolean droppedPreviously = false;
                    Collection<StudentScheduleChange> studentScheduleChanges =
                            m_studentScheduleChangeMap.get(studentOid);
                    if (studentScheduleChanges != null) {
                        for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                            if (sectionOid.equals(studentScheduleChange.getMasterScheduleOid())) {
                                // Selected the latest Drop Date
                                if (StudentScheduleChange.CODE_DROP.equals(studentScheduleChange.getChangeTypeCode())) {
                                    actualEndDate = studentScheduleChange.getEffectiveDate();
                                    droppedPreviously = true;
                                } else if (StudentScheduleChange.CODE_ADD.equals(
                                        studentScheduleChange.getChangeTypeCode()) && droppedPreviously == true) {
                                    // If dropped but then re-added then reset the actualEndDate
                                    actualEndDate = null;
                                }
                            }
                        }
                    }

                    if ("actualEndDate".equals(calcParam)) {
                        // If Completed then set the actualEndDate to the scheduleTermEndDate
                        if (actualEndDate == null && today.after(scheduleTermEndDate)) {
                            result = scheduleTermEndDate;
                        } else {
                            result = actualEndDate;
                        }
                    } else {
                        if ("aimStatus".equals(calcParam)) {
                            if (scheduleTermEndDate == null) {
                                result = "0";
                            } else if (actualEndDate != null && scheduleTermEndDate.after(actualEndDate)) {
                                // Dropped
                                result = "3";
                            } else if (today.after(scheduleTermEndDate)) {
                                // Completed
                                result = "2";
                            } else {
                                // In Progress
                                result = "1";
                            }
                        }
                    }
                }
            }

            return result;
        }
    }

    /**
     * 'PUPILSTATUS'
     *
     * Retrieve &lt;PupilStatus&gt; calculations.
     *
     * @author Follett Software Company
     */
    public class RetrievePupilStatus extends BeanRetriever {
        /**
         * Alias in the StudentEnrollment table to indicate if a student is a
         * part-timer at the school he/she is in.
         */
        public static final String ALIAS_ENR_PART_TIME = "DFE PART TIME";

        /**
         * Field ID For the MasterSchedule table.
         * <p>
         * Returns <strong>DFE CLASS TYPE</strong>'s state value. Indicates if
         * a class is a Nursery type (teaches little kids).
         * <p>
         * <strong>N</strong> being a nursery class, <strong>O</strong> being NOT a nursery class
         */
        public static final String FIELD_MST_CLASS_TYPE = "200552";

        /**
         * Field ID for the MasterSchedule table.
         * <p>
         * Return <strong>DFE CLASS YEAR GROUP</strong>'s state value, default X
         */
        public static final String FIELD_MST_CLASS_YEAR_GROUP = "200560";

        /**
         * A state value for MST's <strong>DFE CLASS TYPE</strong> meaning a class
         * is of a Nursery type.
         */
        public static final String NURSERY = "N";

        /**
         * Field parameter to retrieve if a student is part-time or not
         */
        public static final String PARAM_PART_TIME = "PartTime";

        /**
         * Field parameter to retrieve a student's last day of school
         */
        public static final String PARAM_LEAVING_DATE = "LeavingDate";

        /**
         * Field parameter to retrieve a student's date of entry to the school
         */
        public static final String PARAM_ENTRY_DATE = "EntryDate";

        /**
         * Field parameter to retrieve if a student is in a Nursery class or not
         */
        public static final String PARAM_TYPE_OF_CLASS = "TypeOfClass";

        /**
         * Cached helper class
         */
        private StudentHistoryHelper m_helper;

        /**
         * The end date the retriever should check for
         */
        private PlainDate m_termEndDate;

        /**
         * Instantiates a new retrieve pupil status.
         *
         * @param params Object[]
         */
        public RetrievePupilStatus(Object[] params) {
            m_helper = (StudentHistoryHelper) params[0];
            m_termEndDate = (PlainDate) get(VAR_TERM_END_DATE);

            if (m_termEndDate == null) {
                m_termEndDate = (PlainDate) get(VAR_CENSUS_DATE);
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object value = null;
            SisStudent student = (SisStudent) bean;
            String calcParam = (String) field.getParameter();

            if (PARAM_TYPE_OF_CLASS.equalsIgnoreCase(calcParam)) {
                List<StudentScheduleSpan> studentScheduleSpans = m_helper.getStudentScheduleSpans(student);
                for (StudentScheduleSpan span : studentScheduleSpans) {
                    MasterSchedule section = span.getSection();
                    if (section != null) {
                        String fieldValue = data.getFieldValue(section, FIELD_MST_CLASS_TYPE);
                        if (NURSERY.equals(fieldValue)) {
                            value = NURSERY;
                            break;
                        }
                    }
                }
            }

            else if (PARAM_ENTRY_DATE.equalsIgnoreCase(calcParam)) {
                StudentEnrollment recentEnrollment =
                        m_helper.getEnrollmentForDate(student.getOid(), m_termEndDate, "E");
                if (recentEnrollment != null) {
                    value = recentEnrollment.getEnrollmentDate();
                }
            }

            else if (PARAM_LEAVING_DATE.equalsIgnoreCase(calcParam)) {
                StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(),
                        m_termEndDate,
                        "W");
                if (enrollment != null) {
                    value = enrollment.getEnrollmentDate();
                }
            }

            else if (PARAM_PART_TIME.equalsIgnoreCase(calcParam)) {
                StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(),
                        m_termEndDate,
                        "EWS");
                if (enrollment != null) {
                    String partTimeIndAsString = (String) enrollment.getFieldValueByAlias(ALIAS_ENR_PART_TIME);
                    value = Boolean.valueOf(BooleanAsStringConverter.TRUE.equals(partTimeIndAsString));
                }
            }

            return value;
        }
    }

    /**
     * 'RESOURCEDPROVISIONINDICATOR'.
     *
     * @author Follett Software Company
     */
    public class RetrieveResourcedProvisionIndicator extends BeanRetriever {
        /**
         * Map of students' Collection special educational needs programs
         */
        private Map<String, Collection<StudentProgramParticipation>> m_senMap;

        /**
         * Instantiates a new retrieve resourced provision indicator.
         *
         * @param params Object[]
         */
        public RetrieveResourcedProvisionIndicator(Object[] params) {
            m_senMap = (Map<String, Collection<StudentProgramParticipation>>) params[0];
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            SisStudent student = (SisStudent) bean;

            Collection<StudentProgramParticipation> progs = m_senMap.get(student.getOid());
            String value = "false";

            if (progs != null) {
                for (StudentProgramParticipation prog : progs) {
                    String retrieveResourcedProvisionIdicator =
                            (String) prog.getFieldValueByAlias("DFE SEN RESOURCED PROVISION");

                    if (!StringUtils.isEmpty(retrieveResourcedProvisionIdicator)
                            && retrieveResourcedProvisionIdicator.equals("1")) {
                        value = "true";
                        break;
                    }
                }
            }

            return value;
        }
    }

    /**
     * 'FUNDEDHOURS'.
     *
     * @author Follet Software Company
     */
    public class RetrieveFundedHours extends BeanRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Number result = null;
            SisStudent student = (SisStudent) bean;

            int studentAgeAsOfAug31 = getStudentAgeAsOfAug31(student);
            if (studentAgeAsOfAug31 < 5) {
                String fundedHoursAsString = (String) student.getFieldValueByAlias("DFE FUNDED HOURS");
                if (StringUtils.isEmpty(fundedHoursAsString)) {
                    fundedHoursAsString = "0";
                }
                result = new BigDecimal(fundedHoursAsString);
            }

            return result;
        }
    }

    /**
     * 'SENPROVISION'
     *
     * Retrieve a student's SEN provision.
     */
    public class RetrieveSenProvision extends BeanRetriever {
        /**
         * Field ID for the StudentProgramParticipation table
         * <p>
         * Returns <strong>DFE SEN PROVISION</strong>'s state value
         */
        public static final String FIELD_PGM_SENPROVISION = "SENPROVISION";

        /**
         * State code for <strong>DFE SEN PROVISION</strong>, meaning no sped needed
         */
        public static final String NO_SPECIAL_EDUCATION_NEED = "N";

        /**
         * Map of students' Collection special educational needs programs
         */
        private Map<String, Collection<StudentProgramParticipation>> m_senMap;

        /**
         * Instantiates a new retrieve sen provision.
         *
         * @param params Object[]
         */
        public RetrieveSenProvision(Object[] params) {
            m_senMap = (Map<String, Collection<StudentProgramParticipation>>) params[0];
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object value = null;
            SisStudent student = (SisStudent) bean;

            Collection<StudentProgramParticipation> sens = m_senMap.get(student.getOid());
            if (sens != null && !sens.isEmpty()) {
                for (StudentProgramParticipation sen : sens) {
                    String senProvisionCode = data.getFieldValue(sen, FIELD_PGM_SENPROVISION);
                    if (!StringUtils.isEmpty(senProvisionCode) && !senProvisionCode.equals(NO_SPECIAL_EDUCATION_NEED)) {
                        value = senProvisionCode;
                    }
                }
            }

            return value;
        }

    }

    /**
     * 'SENUNITMEMBER'
     *
     * Check if the student is enrolled in a Special Education school.
     */
    public class RetrieveSenUnitMember extends BeanRetriever {

        private StudentHistoryHelper m_helper;
        private PlainDate m_termEndDate;

        /**
         * Instantiates a new retrieve sen unit member.
         *
         * @param params Object[]
         */
        public RetrieveSenUnitMember(Object[] params) {
            m_helper = (StudentHistoryHelper) params[0];
            m_termEndDate = (PlainDate) get(VAR_TERM_END_DATE);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object result = null;
            SisStudent student = (SisStudent) bean;

            StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_termEndDate, "EWS");
            if (enrollment != null) {
                SisSchool school = enrollment.getSchool();
                String schoolPhase = data.getFieldValue(school, FIELD_SKL_SCHOOL_PHASE);
                if (CODE_SCHOOL_SPECIAL.equals(schoolPhase)) {
                    result = Boolean.TRUE;
                } else {
                    result = Boolean.FALSE;
                }
            }

            return result;
        }
    }

    /**
     * 'STAFFABSENCES'
     *
     * Retrieve Staff Attendance and Staff Leaves.
     *
     * @author Follett Software Company
     */
    public class RetrieveStaffAbsences extends BeanRetriever {
        /**
         * Field parameter to retrieve Staff Absence information
         */
        public static final String PARAM_FIRST_DAY_OF_ABSENCE = "FirstDayOfAbsence";
        public static final String PARAM_LAST_DAY_OF_ABSENCE = "LastDayOfAbsence";
        public static final String PARAM_WORKING_DAYS_LOST = "WorkingDaysLost";
        public static final String PARAM_ABSENCE_CATEGORY = "AbsenceCategory";

        public static final String REF_CODE_STAFF_ATTENDANCE_REASONS = "rtbSfaReason";
        public static final String DEFAULT_STAFF_ATTENDANCE_REASON = "OTH";

        /**
         * SubQuery for filtering StaffOids
         */
        private SubQuery m_staffSubQuery;

        /**
         * Map of Staff Attendance Reason State Codes. Map by Code
         */
        private Map<String, String> m_staffAttendanceReasonsCodesMap = new HashMap<String, String>();

        /**
         * Map of StaffAttendance and StaffLeave records
         */
        private Map<String, StaffAttendanceInfo> m_staffAttendanceInfoMap = new HashMap<String, StaffAttendanceInfo>();

        /**
         * Map of staffCalendarId to staffCalendarOid
         */
        private Map<String, String> m_calendarIdMap = new HashMap<String, String>();

        /**
         * Map of StaffCalendarDate map by staffCalendarOid
         */
        private Map<String, Collection<StaffCalendarDate>> m_calendarDateMap =
                new HashMap<String, Collection<StaffCalendarDate>>();

        /**
         * Constructor.
         *
         * @param params Object[]
         */
        public RetrieveStaffAbsences(Object[] params) {
            m_staffSubQuery = (SubQuery) params[0];

            PlainDate absenceStartDate = (PlainDate) get(VAR_ABSENCE_START_DATE);
            PlainDate absenceEndDate = (PlainDate) get(VAR_ABSENCE_END_DATE);

            loadStaffCalendarDates(absenceStartDate, absenceEndDate);

            loadStaffAttendanceReasonsCodes();

            loadStaffAttendances(absenceStartDate, absenceEndDate);
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, X2BaseBean bean, FieldDefinition field) {
            Object value = null;
            String staffAttendanceInfoOid = bean.getOid();
            String calcParam = (String) field.getParameter();

            StaffAttendanceInfo staffAttendanceInfo = m_staffAttendanceInfoMap.get(staffAttendanceInfoOid);

            if (PARAM_FIRST_DAY_OF_ABSENCE.equals(calcParam)) {
                value = new PlainDate(staffAttendanceInfo.m_firstDayOfAbsence);
            } else if (PARAM_LAST_DAY_OF_ABSENCE.equals(calcParam)) {
                value = new PlainDate(staffAttendanceInfo.m_lastDayOfAbsence);
            } else if (PARAM_WORKING_DAYS_LOST.equals(calcParam)) {
                value = Integer.valueOf(staffAttendanceInfo.m_workingDaysLost);
            } else if (PARAM_ABSENCE_CATEGORY.equals(calcParam)) {
                if (staffAttendanceInfo.m_absenceCategory != null) {
                    value = staffAttendanceInfo.m_absenceCategory;
                } else {
                    value = "";
                }
            }

            return value;
        }

        /**
         * Get Staff Attendance Key by staffOid and reasonCode.
         *
         * @param staffAttendance StaffAttendance
         * @return String
         */
        private String getAttendanceKey(StaffAttendance staffAttendance) {
            String staffOid = staffAttendance.getStaffOid();
            String reasonCode = staffAttendance.getReason();

            StringBuffer attendanceKey = new StringBuffer();
            attendanceKey.append(staffOid);
            if (reasonCode != null) {
                attendanceKey.append(reasonCode);
            }

            return attendanceKey.toString();
        }

        /**
         * Get the number of Working days between two dates for a specific calendar Id.
         *
         * @param staffCalendarId String
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return workingDaysCount
         */
        private int getWorkingDaysCount(String staffCalendarId, PlainDate startDate, PlainDate endDate) {
            int workingDaysCount = 0;

            if (StringUtils.isEmpty(staffCalendarId) || !m_calendarIdMap.containsKey(staffCalendarId)) {
                staffCalendarId = "Standard";
            }
            String staffCalendarOid = m_calendarIdMap.get(staffCalendarId);

            Collection<StaffCalendarDate> calendarDates = m_calendarDateMap.get(staffCalendarOid);

            if (calendarDates != null) {
                for (StaffCalendarDate staffCalendarDate : calendarDates) {
                    PlainDate date = staffCalendarDate.getDate();

                    if (date.equals(startDate) || date.equals(endDate)) {
                        workingDaysCount++;
                    } else if (date.after(startDate) && date.before(endDate)) {
                        workingDaysCount++;
                    }
                }
            }

            return workingDaysCount;
        }

        /**
         * Check if two dates are consecutive.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return boolean
         */
        private boolean areConsecutiveDays(PlainDate startDate, PlainDate endDate) {
            boolean areConsecutive = false;
            if (startDate != null && endDate != null) {
                PlainDate newDate = DateUtils.add(startDate, +1);
                if (newDate.equals(endDate)) {
                    areConsecutive = true;
                }
            }
            return areConsecutive;
        }

        /**
         * Load Staff Attendance Reason State Codes.
         */
        private void loadStaffAttendanceReasonsCodes() {
            X2Criteria codesCriteria = new X2Criteria();
            codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_STAFF_ATTENDANCE_REASONS);
            BeanQuery codesQuery = new BeanQuery(ReferenceCode.class, codesCriteria);
            codesQuery.addOrderByAscending(ReferenceCode.COL_CODE);
            Collection<ReferenceCode> codesList = getBroker().getCollectionByQuery(codesQuery);

            for (ReferenceCode referenceCode : codesList) {
                String stateCode = referenceCode.getStateCode();
                if (stateCode == null) {
                    stateCode = DEFAULT_STAFF_ATTENDANCE_REASON;
                }
                m_staffAttendanceReasonsCodesMap.put(referenceCode.getCode(), stateCode);
            }
        }

        /**
         * Load Staff Attendance and Staff Leaves.
         *
         * @param absenceStartDate PlainDate
         * @param absenceEndDate PlainDate
         */
        private void loadStaffAttendances(PlainDate absenceStartDate, PlainDate absenceEndDate) {
            // Get Staff Attendance
            X2Criteria attendanceCriteria = new X2Criteria();
            attendanceCriteria.addIn(StaffAttendance.COL_STAFF_OID, m_staffSubQuery);
            attendanceCriteria.addGreaterOrEqualThan(StaffAttendance.COL_DATE, absenceStartDate);
            attendanceCriteria.addLessOrEqualThan(StaffAttendance.COL_DATE, absenceEndDate);
            attendanceCriteria.addEqualTo(StaffAttendance.COL_CODE, "A");

            BeanQuery attendanceQuery = new BeanQuery(StaffAttendance.class, attendanceCriteria);
            attendanceQuery.addOrderByAscending(StaffAttendance.COL_STAFF_OID);
            attendanceQuery.addOrderByAscending(StaffAttendance.COL_REASON);
            attendanceQuery.addOrderByAscending(StaffAttendance.COL_DATE);

            Collection<StaffAttendance> attendanceList = getBroker().getCollectionByQuery(attendanceQuery);

            // Load Staff Attendance as StaffAttendanceInfo objects
            PlainDate currDate = null;
            PlainDate prevDate = null;
            String currAttendanceKey = null;
            String prevAttendanceKey = null;
            StaffAttendance prevStaffAttendance = null;
            PlainDate firstDayOfAbsenceDate = null;

            for (StaffAttendance staffAttendance : attendanceList) {
                currDate = staffAttendance.getDate();
                currAttendanceKey = getAttendanceKey(staffAttendance);

                if (prevStaffAttendance == null) {
                    firstDayOfAbsenceDate = currDate;
                } else {
                    boolean areConsecutiveDays = areConsecutiveDays(prevDate, currDate);

                    if (!(currAttendanceKey.equals(prevAttendanceKey) && areConsecutiveDays)) {
                        StaffAttendanceInfo staffAttendanceInfo = new StaffAttendanceInfo();
                        staffAttendanceInfo.m_firstDayOfAbsence = firstDayOfAbsenceDate;
                        staffAttendanceInfo.m_lastDayOfAbsence = prevDate;
                        staffAttendanceInfo.m_absenceCategory = DEFAULT_STAFF_ATTENDANCE_REASON;
                        if (prevStaffAttendance.getReason() != null) {
                            staffAttendanceInfo.m_absenceCategory =
                                    lookUpStaffAttendanceReasonStateCode(prevStaffAttendance.getReason());
                        }

                        String staffCalendarId = prevStaffAttendance.getStaff().getCalendarId();
                        int calendarLeaveDays = getWorkingDaysCount(staffCalendarId, firstDayOfAbsenceDate, prevDate);
                        staffAttendanceInfo.m_workingDaysLost = calendarLeaveDays;

                        m_staffAttendanceInfoMap.put(prevStaffAttendance.getOid(), staffAttendanceInfo);

                        firstDayOfAbsenceDate = currDate;
                    }
                }

                prevDate = currDate;
                prevAttendanceKey = currAttendanceKey;
                prevStaffAttendance = staffAttendance;
            }

            // Add the info for the last staffAttendance
            if (prevStaffAttendance != null) {
                StaffAttendanceInfo staffAttendanceInfo = new StaffAttendanceInfo();
                staffAttendanceInfo.m_firstDayOfAbsence = firstDayOfAbsenceDate;
                staffAttendanceInfo.m_lastDayOfAbsence = prevDate;
                staffAttendanceInfo.m_absenceCategory = DEFAULT_STAFF_ATTENDANCE_REASON;
                if (prevStaffAttendance.getReason() != null) {
                    staffAttendanceInfo.m_absenceCategory =
                            lookUpStaffAttendanceReasonStateCode(prevStaffAttendance.getReason());
                }

                String staffCalendarId = prevStaffAttendance.getStaff().getCalendarId();
                int calendarLeaveDays = getWorkingDaysCount(staffCalendarId, firstDayOfAbsenceDate, prevDate);
                staffAttendanceInfo.m_workingDaysLost = calendarLeaveDays;

                m_staffAttendanceInfoMap.put(prevStaffAttendance.getOid(), staffAttendanceInfo);
            }


            // Get Staff Leaves
            X2Criteria leaveCriteria = new X2Criteria();
            leaveCriteria.addGreaterOrEqualThan(StaffLeave.COL_START_DATE, absenceStartDate);
            leaveCriteria.addLessOrEqualThan(StaffLeave.COL_END_DATE, absenceEndDate);
            leaveCriteria.addIn(StaffLeave.COL_STAFF_OID, m_staffSubQuery);

            BeanQuery leaveQuery = new BeanQuery(StaffLeave.class, leaveCriteria);
            leaveQuery.addOrderByAscending(StaffLeave.COL_STAFF_OID);
            leaveQuery.addOrderByAscending(StaffLeave.COL_REASON_CODE);
            leaveQuery.addOrderByAscending(StaffLeave.COL_START_DATE);

            Collection<StaffLeave> leaveList = getBroker().getCollectionByQuery(leaveQuery);

            // Load Staff Leaves as StaffAttendanceInfo objects
            for (StaffLeave staffLeave : leaveList) {
                StaffAttendanceInfo staffAttendanceInfo = new StaffAttendanceInfo();
                staffAttendanceInfo.m_firstDayOfAbsence = staffLeave.getStartDate();
                staffAttendanceInfo.m_lastDayOfAbsence = staffLeave.getEndDate();
                staffAttendanceInfo.m_absenceCategory = DEFAULT_STAFF_ATTENDANCE_REASON;
                if (prevStaffAttendance != null && prevStaffAttendance.getReason() != null) {
                    staffAttendanceInfo.m_absenceCategory =
                            lookUpStaffAttendanceReasonStateCode(prevStaffAttendance.getReason());
                }

                int calendarLeaveDays = 1;
                if (staffLeave.getEndDate() != null) {
                    String staffCalendarId = staffLeave.getStaff().getCalendarId();
                    calendarLeaveDays =
                            getWorkingDaysCount(staffCalendarId, staffLeave.getStartDate(), staffLeave.getEndDate());
                } else {
                    staffAttendanceInfo.m_lastDayOfAbsence = staffLeave.getStartDate();
                }
                staffAttendanceInfo.m_workingDaysLost = calendarLeaveDays;

                m_staffAttendanceInfoMap.put(staffLeave.getOid(), staffAttendanceInfo);
            }
        }

        /**
         * Load the Staff Calendar's and Staff Calendar Dates collections.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        private void loadStaffCalendarDates(PlainDate startDate, PlainDate endDate) {
            Criteria calendarCriteria = new Criteria();
            calendarCriteria.addGreaterOrEqualThan(
                    StaffCalendar.REL_STAFF_CALENDAR_DATES + "." + StaffCalendarDate.COL_DATE, startDate);
            calendarCriteria.addLessOrEqualThan(
                    StaffCalendar.REL_STAFF_CALENDAR_DATES + "." + StaffCalendarDate.COL_DATE, endDate);

            QueryByCriteria calendarQuery = new QueryByCriteria(StaffCalendar.class, calendarCriteria);

            Collection<StaffCalendar> calendars = getBroker().getCollectionByQuery(calendarQuery);
            for (StaffCalendar staffCalendar : calendars) {
                m_calendarIdMap.put(staffCalendar.getCalendarId(), staffCalendar.getOid());
            }

            Criteria calendarDateCriteria = new Criteria();
            calendarDateCriteria.addEqualTo(StaffCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));
            calendarDateCriteria.addGreaterOrEqualThan(StaffCalendarDate.COL_DATE, startDate);
            calendarDateCriteria.addLessOrEqualThan(StaffCalendarDate.COL_DATE, endDate);

            QueryByCriteria calendarDateQuery = new QueryByCriteria(StaffCalendarDate.class, calendarDateCriteria);
            calendarDateQuery.addOrderByAscending(StaffCalendarDate.COL_STAFF_CALENDAR_OID);
            calendarDateQuery.addOrderByAscending(StaffCalendarDate.COL_DATE);

            m_calendarDateMap = getBroker().getGroupedCollectionByQuery(calendarDateQuery,
                    StaffCalendarDate.COL_STAFF_CALENDAR_OID, 64);
        }

        /**
         * Get Staff Attendance Reason State Code.
         *
         * @param reasonCode String
         * @return String
         */
        private String lookUpStaffAttendanceReasonStateCode(String reasonCode) {
            String reasonStateCode = DEFAULT_STAFF_ATTENDANCE_REASON;
            if (reasonCode != null && m_staffAttendanceReasonsCodesMap != null) {
                reasonStateCode = m_staffAttendanceReasonsCodesMap.get(reasonCode);
                if (reasonStateCode == null) {
                    reasonStateCode = DEFAULT_STAFF_ATTENDANCE_REASON;
                }
            }

            return reasonStateCode;
        }

    }


    /**
     * 'STAFFCONTRACT'
     *
     * Retrieve Staff's Person Category (Workforce Contract Category).
     */
    public class RetrieveStaffContract extends BeanRetriever {
        private static final String PARAM_PERSON_CATEGORY = "PERSONCATEGORY";

        private static final String ALIAS_DFE_CONTRACT_TYPE = "DFE CONTRACT TYPE";
        private static final String ALIAS_DFE_CONTRACT_START_DATE = "DFE CONTRACT START";
        private static final String ALIAS_DFE_CONTRACT_END_DATE = "DFE CONTRACT END";
        private static final String ALIAS_DFE_CONTRACT_POST = "DFE POST";

        // Constant values
        private static final String DFE_CONTRACT_TYPE_PRM = "PRM";
        private static final String DFE_CONTRACT_TYPE_FXT = "FXT";
        private static final String DFE_CONTRACT_TYPE_TMP = "TMP";
        private static final String DFE_CONTRACT_POST_SUP = "SUP";
        private static final String DFE_CONTRACT_POST_TAS = "TAS";
        private static final String DFE_CONTRACT_POST_AVT = "AVT";

        private final Integer DFE_DEFAULT_PERSON_CATEGORY = Integer.valueOf(5);

        private final SimpleDateFormat m_dateFormatter = new SimpleDateFormat("yyyy-MM-dd");

        private PlainDate m_censusDate;

        /**
         * Map of a Collection of StaffPosition. Keyed by StaffOid
         */
        Map<String, Collection<StaffPosition>> m_contractsMap = new HashMap<String, Collection<StaffPosition>>();

        /**
         * Map of Staff Person Categories. Keyed by StaffOid
         */
        Map<String, Integer> m_personCategoryMap = new HashMap<String, Integer>();

        /**
         * Default constructor.
         *
         * @param params Object[]
         */
        public RetrieveStaffContract(Object[] params) {
            m_censusDate = (PlainDate) get(VAR_CENSUS_DATE);

            m_contractsMap = (Map<String, Collection<StaffPosition>>) params[0];

            loadWorkforceMemberPersonCategory();
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object result = null;
            String calcParam = (String) field.getParameter();

            if (PARAM_PERSON_CATEGORY.equals(calcParam)) {
                String staffOid = bean.getOid();
                Integer personCategory = m_personCategoryMap.get(staffOid);

                if (personCategory != null) {
                    result = personCategory;
                } else {
                    result = DFE_DEFAULT_PERSON_CATEGORY;
                }
            }

            return result;
        }

        /**
         * Load the Workforce Member's Person Category.
         */
        private void loadWorkforceMemberPersonCategory() {
            int personCategory = 0;

            Collection<String> staffOids = m_contractsMap.keySet();
            for (String staffOid : staffOids) {
                // Set default
                m_personCategoryMap.put(staffOid, DFE_DEFAULT_PERSON_CATEGORY);

                Collection<StaffPosition> contractCol = m_contractsMap.get(staffOid);
                for (StaffPosition staffPosition : contractCol) {
                    String contractType = (String) staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_TYPE);
                    String startDate = convertObjectToStringDate(
                            staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_START_DATE));
                    String endDate =
                            convertObjectToStringDate(staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_END_DATE));
                    String post = (String) staffPosition.getFieldValueByAlias(ALIAS_DFE_CONTRACT_POST);

                    PlainDate staffPositionStartDate = null;
                    PlainDate staffPositionEndDate = null;
                    boolean isEndDateGreaterThan27Days = false;
                    boolean isReferenceDateGreaterThan27Days = false;
                    if (!StringUtils.isEmpty(startDate)) {
                        staffPositionStartDate = DateUtils.getDate(startDate);
                        PlainDate staffPositionStartPlus27DaysDate = DateUtils.add(staffPositionStartDate, 27);

                        if (m_censusDate != null && !StringUtils.isEmpty(m_censusDate.toString())) {
                            if (m_censusDate.equals(staffPositionStartPlus27DaysDate)
                                    || m_censusDate.after(staffPositionStartPlus27DaysDate)) {
                                isReferenceDateGreaterThan27Days = true;
                            }
                        } else {
                            isReferenceDateGreaterThan27Days = true;
                        }

                        if (!StringUtils.isEmpty(endDate)) {
                            staffPositionEndDate = DateUtils.getDate(endDate);

                            if (staffPositionEndDate.equals(staffPositionStartPlus27DaysDate)
                                    || staffPositionEndDate.after(staffPositionStartPlus27DaysDate)) {
                                isEndDateGreaterThan27Days = true;
                            }
                        } else {
                            isEndDateGreaterThan27Days = true;
                        }
                    }

                    // Teachers in regular service (Category 1)
                    if (DFE_CONTRACT_TYPE_PRM.equals(contractType)
                            && !(DFE_CONTRACT_POST_SUP.equals(post) || DFE_CONTRACT_POST_TAS.equals(post)
                                    || DFE_CONTRACT_POST_AVT.equals(post))) {
                        personCategory = 1;
                    } else if (DFE_CONTRACT_TYPE_FXT.equals(contractType)
                            && isEndDateGreaterThan27Days
                            && !(DFE_CONTRACT_POST_SUP.equals(post) || DFE_CONTRACT_POST_TAS.equals(post)
                                    || DFE_CONTRACT_POST_AVT.equals(post))) {
                        personCategory = 1;
                    } else if (DFE_CONTRACT_TYPE_TMP.equals(contractType)
                            && (staffPositionEndDate != null)
                            && isEndDateGreaterThan27Days
                            && !(DFE_CONTRACT_POST_SUP.equals(post) || DFE_CONTRACT_POST_TAS.equals(post)
                                    || DFE_CONTRACT_POST_AVT.equals(post))) {
                        personCategory = 1;
                    } else if (DFE_CONTRACT_TYPE_TMP.equals(contractType)
                            && (staffPositionEndDate == null)
                            && isReferenceDateGreaterThan27Days
                            && !(DFE_CONTRACT_POST_SUP.equals(post) || DFE_CONTRACT_POST_TAS.equals(post)
                                    || DFE_CONTRACT_POST_AVT.equals(post))) {
                        personCategory = 1;
                    }
                    // Agency/Service Agreement (Category 2)
                    else if ((!DFE_CONTRACT_TYPE_PRM.equals(contractType) && !DFE_CONTRACT_TYPE_FXT.equals(contractType)
                            && !DFE_CONTRACT_TYPE_TMP.equals(contractType))
                            && (staffPositionEndDate != null)
                            && isEndDateGreaterThan27Days
                            && !(DFE_CONTRACT_POST_SUP.equals(post) || DFE_CONTRACT_POST_TAS.equals(post)
                                    || DFE_CONTRACT_POST_AVT.equals(post))) {
                        personCategory = 2;
                    } else if ((!DFE_CONTRACT_TYPE_PRM.equals(contractType)
                            && !DFE_CONTRACT_TYPE_FXT.equals(contractType)
                            && !DFE_CONTRACT_TYPE_TMP.equals(contractType))
                            && (staffPositionEndDate == null)
                            && isReferenceDateGreaterThan27Days
                            && !(DFE_CONTRACT_POST_SUP.equals(post) || DFE_CONTRACT_POST_TAS.equals(post)
                                    || DFE_CONTRACT_POST_AVT.equals(post))) {
                        personCategory = 2;
                    }
                    // Teachers Assistant (Category 3)
                    else if (DFE_CONTRACT_POST_TAS.equals(post)
                            && DFE_CONTRACT_TYPE_PRM.equals(contractType)) {
                        personCategory = 3;
                    } else if (DFE_CONTRACT_POST_TAS.equals(post)
                            && (staffPositionEndDate != null)
                            && isEndDateGreaterThan27Days) {
                        personCategory = 3;
                    } else if (DFE_CONTRACT_POST_TAS.equals(post)
                            && (staffPositionEndDate == null)
                            && isReferenceDateGreaterThan27Days) {
                        personCategory = 3;
                    }
                    // Other Support Staff (Category 4)
                    else if (DFE_CONTRACT_POST_SUP.equals(post)
                            && DFE_CONTRACT_TYPE_PRM.equals(contractType)) {
                        personCategory = 4;
                    } else if (DFE_CONTRACT_POST_SUP.equals(post)
                            && (staffPositionEndDate != null)
                            && isEndDateGreaterThan27Days) {
                        personCategory = 4;
                    } else if (DFE_CONTRACT_POST_SUP.equals(post)
                            && (staffPositionEndDate == null)
                            && isReferenceDateGreaterThan27Days) {
                        personCategory = 4;
                    } else if (DFE_CONTRACT_POST_AVT.equals(post)
                            && DFE_CONTRACT_TYPE_PRM.equals(contractType)) {
                        personCategory = 4;
                    } else if (DFE_CONTRACT_POST_AVT.equals(post)
                            && (staffPositionEndDate != null)
                            && isEndDateGreaterThan27Days) {
                        personCategory = 4;
                    } else if (DFE_CONTRACT_POST_AVT.equals(post)
                            && (staffPositionEndDate == null)
                            && isReferenceDateGreaterThan27Days) {
                        personCategory = 4;
                    } else
                    // No valid contract (Category 5)
                    {
                        personCategory = 5;
                    }

                    Integer personCategoryNew = Integer.valueOf(personCategory);
                    // If multiple contracts take the lowest personCategory
                    if (m_personCategoryMap.containsKey(staffOid)) {
                        Integer personCategoryOld = m_personCategoryMap.get(staffOid);
                        if ((personCategory != 0) && (personCategory < personCategoryOld.intValue())) {
                            m_personCategoryMap.remove(staffOid);
                            m_personCategoryMap.put(staffOid, personCategoryNew);
                        }
                    } else {
                        m_personCategoryMap.put(staffOid, personCategoryNew);
                    }

                }

            }

        }

        /**
         * Convert a Object date to a String Date.
         *
         * @param dateObj Object
         * @return dateStr
         */
        private String convertObjectToStringDate(Object dateObj) {
            String dateStr = null;

            if (dateObj instanceof String) {
                dateStr = (String) dateObj;
            } else if (dateObj instanceof PlainDate) {
                dateStr = m_dateFormatter.format((PlainDate) dateObj);
            }

            return dateStr;
        }

    }

    /**
     * 'STATUS'
     *
     * Retrieve staffs' teaching statuses.
     */
    public class RetrieveStatus extends BeanRetriever {

        /**
         * Field ID on the Staff table
         * <p>
         * Returns <strong>DFE STAFF CATEGORY</strong>'s state value
         */
        public static final String FIELD_STF_CATEGORY = "CATEGORY";

        /**
         * State code value for <strong>DFE STAFF CATEGORY</strong>,
         * meaning the staff is a higher level teaching assistant.
         */
        public static final String HIGHER_LEVEL_TEACHING_ASSISTANT = "HL";

        /**
         * Field parameter to return whether a staff is a qualified teacher
         */
        public static final String PARAM_QT_STATUS = "QTStatus";

        /**
         * Default constructor.
         */
        public RetrieveStatus() {
            // empty
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {
            Object result = null;
            String calcParam = (String) field.getParameter();
            SisStaff staff = (SisStaff) bean;

            if (PARAM_QT_STATUS.equals(calcParam)) // QTStatus
            {
                String category = data.getFieldValue(staff, FIELD_STF_CATEGORY);
                if (CODE_QUALIFIED_TEACHER.equals(category)) {
                    result = Boolean.TRUE;
                } else {
                    result = Boolean.FALSE;
                }
            }

            return result;
        }
    }


    /**
     * 'SYSTEM'
     *
     * Retrieve system values.
     *
     * @author Follett Software Company
     */
    public class RetrieveSystem extends BeanRetriever {

        /**
         * Field parameter to retrieve the DCSF XSLT Version, if used by software supplier.
         * Which we don't?
         */
        private static final String PARAM_XVERSION = "Xversion";

        /**
         * Field parameter to retrieve the data and time of the organization's time
         */
        public static final String PARAM_DATE_TIME = "DateTime";

        /**
         * Field parameter to retrieve the system's name. Always "Aspen".
         */
        public static final String PARAM_NAME = "Name";

        /**
         * Field parameter to retrieve the system's current version.
         */
        public static final String PARAM_RELEASE = "Release";

        /**
         * Field parameter to retrieve the serial number. Always "001".
         */
        public static final String PARAM_SERIAL_NO = "SerialNo";

        /**
         * Constructor.
         */
        public RetrieveSystem() {
            // empty
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, X2BaseBean bean, FieldDefinition field) {
            Object result = null;
            String calcParam = (String) field.getParameter();

            if (PARAM_RELEASE.equalsIgnoreCase(calcParam)) // Release
            {
                result = AppGlobals.getVersion();
            } else if (PARAM_SERIAL_NO.equalsIgnoreCase(calcParam)) // SerialNo
            {
                result = "001";
            } else if (PARAM_NAME.equals(calcParam)) {
                result = "Aspen";
            } else if (PARAM_DATE_TIME.equalsIgnoreCase(calcParam)) // DateTime
            {
                PlainDate currentDate = new PlainDate(OrganizationManager.getTimeZone(getData().getOrganization()));
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
                result = dateFormat.format(currentDate);
            } else if (PARAM_XVERSION.equals(calcParam)) {
                result = "N/A";
            }

            return result;
        }
    }

    /**
     * 'UNITCONTACTTIME'
     *
     * Retrieve exclusion field from school table for students.
     *
     * @author Follett Software Company
     */
    public class RetrieveUnitContactTime extends BeanRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    X2BaseBean bean,
                                    FieldDefinition field) {

            SisStudent student = (SisStudent) bean;

            SisSchool school = student.getSchool();

            String unitContactTimeString = (String) school.getFieldValueByAlias("DFE UNITCONTACTTIMEPUPILL");
            Integer unitContactTimeInt = Integer.valueOf(0);
            String unitContactTime = "";

            if (StringUtils.isNumeric(unitContactTimeString)) {
                unitContactTimeInt = Integer.valueOf(unitContactTimeString);
                unitContactTime = unitContactTimeInt.toString();
            }

            if (StringUtils.isEmpty(unitContactTimeString)) {
                unitContactTime = "";
            }

            return unitContactTime;
        }
    }

    /**
     * ACCOMMODATION
     *
     * Validate &lt;Accommodation&gt;.
     *
     * @author Follett Software Company
     */
    public class ValidateAccommodation extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String accommodation = value;
            SisSchool school = (SisSchool) bean;
            String schoolPhase = data.getFieldValue(school, "200006");
            String term = (String) get("term");
            if ("SP".equals(schoolPhase) && "SPR".equals(term)) {
                if (accommodation.isEmpty()) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "School organisation (day, boarding or hospital) is missing or invalid",
                            ""));
                }
            }

            return errors;
        }
    }

    /**
     * Validate Exclusion category.
     *
     * @author Follett Software Company
     */
    public class ValidateExclusionCategory extends BeanValidator {

        Set<String> stdOidsWithPermanentExclusions = new HashSet<String>();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String category = value;

            if ("PERM".equals(category)) {
                /*
                 * A pupil cannot have more than one permanent exclusion
                 */
                if (stdOidsWithPermanentExclusions.contains(bean.getOid())) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Pupil has more than one permanent exclusion record",
                            ""));
                }
                stdOidsWithPermanentExclusions.add(bean.getOid());
            }

            return null;
        }
    }

    /**
     * The Class ValidateExclusionStartDate.
     */
    public class ValidateExclusionStartDate extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {

            // all schools except nursery
            String schoolPhase = getSchoolPhase();
            if (!"NS".equals(schoolPhase)) {
                String term = (String) get(VAR_TERM);
                if (CODE_TERM_SPRING.equals(term)) {
                    // TODO
                }
            }

            return null;
        }
    }

    /**
     * CONNEXIONS
     *
     * Validate &lt;Connexions&gt; tag.
     *
     * @author Follett Software Company
     */
    public class ValidateConnexions extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String connexions = value;
            SisStudent student = (SisStudent) bean;
            PlainDate aug31 = (PlainDate) get(VAR_AGE_AT_DATE);
            int ageAsOfAug31 = student.getPerson().getAgeAsOfDate(aug31);
            String senProvision = data.getFieldValue(bean, "100472");

            String schoolPhase = data.getFieldValue(data.getSchool(), FIELD_SKL_SCHOOL_PHASE);

            // middle-deemed primary, secondary, and special schools
            if (schoolPhase.matches("MP|SS|SP")) {
                /*
                 * Where <SENprovision> (100472) = 'S' and age is between 12 and 25 at
                 * previous 31st August then <Connexions> (100036) must be Yes, No, UNS or SNR
                 */
                if (senProvision.equals("S") &&
                        (12 <= ageAsOfAug31 && ageAsOfAug31 <= 25) &&
                        !connexions.matches("[Yy](es?)|[Nn]o?|UNS|SNR")) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Connexions agreement is missing or invalid",
                            ""));
                }

                /*
                 * Where <SENprovision> (100472) is not = 'S', and age is between 12 and 20 at
                 * previous 31st August if <DOB> (100007)) then <Connexions> (100036) must
                 * be Yes, No, UNS or SNR
                 */
                else if (!senProvision.equals("S") &&
                        (12 <= ageAsOfAug31 && ageAsOfAug31 <= 20) &&
                        !connexions.matches("[Yy](es?)|[Nn]o?|UNS|SNR")) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Connexions agreement is missing or invalid",
                            ""));
                }
            }

            return errors;
        }
    }

    /**
     * DOB
     *
     * Validate a student's date of birth.
     */
    public class ValidateDob extends BeanValidator {

        /**
         * The Class AgeRange.
         */
        private class AgeRange {

            public int m_age = -1;
            public int m_min;
            public int m_max;

            /**
             * Instantiates a new age range.
             *
             * @param min int
             * @param max int
             */
            public AgeRange(int min, int max) {
                m_min = min;
                m_max = max;
            }

            /**
             * Sets the dob.
             *
             * @param age void
             */
            public void setDob(int age) {
                m_age = age;
            }

            /**
             * Checks if an age is VALID.
             * min < age < max
             *
             * @return Boolean
             */
            public Boolean check() {
                if (m_age == -1) {
                    throw new X2RuntimeException();
                }
                return Boolean.valueOf(m_min < m_age && m_age < m_max);
            }
        }

        Map<String, AgeRange> m_validAgeMap = new HashMap<String, CbdsReportModel.ValidateDob.AgeRange>();

        /**
         * Instantiates a new validate dob.
         */
        public ValidateDob() {
            m_validAgeMap.put("50", new AgeRange(2, 7));
            m_validAgeMap.put("51", new AgeRange(2, 7));
            m_validAgeMap.put("01", new AgeRange(2, 9));
            m_validAgeMap.put("16", new AgeRange(2, 9));
            m_validAgeMap.put("02", new AgeRange(2, 10));
            m_validAgeMap.put("03", new AgeRange(2, 11));
            m_validAgeMap.put("04", new AgeRange(2, 13));
            m_validAgeMap.put("17", new AgeRange(7, 12));
            m_validAgeMap.put("18", new AgeRange(2, 11));
            m_validAgeMap.put("42", new AgeRange(2, 8));
            m_validAgeMap.put("43", new AgeRange(7, 11));
            m_validAgeMap.put("05", new AgeRange(8, 13));
            m_validAgeMap.put("06", new AgeRange(9, 14));
            m_validAgeMap.put("45", new AgeRange(9, 13));
            m_validAgeMap.put("07", new AgeRange(9, 14));
            m_validAgeMap.put("08", new AgeRange(10, 14));
            m_validAgeMap.put("09", new AgeRange(12, 17));
            m_validAgeMap.put("10", new AgeRange(12, 19));
            m_validAgeMap.put("11", new AgeRange(13, 17));
            m_validAgeMap.put("29", new AgeRange(13, 17));
            m_validAgeMap.put("12", new AgeRange(13, 20));
            m_validAgeMap.put("30", new AgeRange(13, 20));
            m_validAgeMap.put("31", new AgeRange(13, 20));
            m_validAgeMap.put("21", new AgeRange(11, 17));
            m_validAgeMap.put("27", new AgeRange(11, 17));
            m_validAgeMap.put("28", new AgeRange(11, 17));
            m_validAgeMap.put("48", new AgeRange(11, 17));
            m_validAgeMap.put("22", new AgeRange(11, 20));
            m_validAgeMap.put("36", new AgeRange(11, 20));
            m_validAgeMap.put("37", new AgeRange(11, 20));
            m_validAgeMap.put("38", new AgeRange(11, 20));
            m_validAgeMap.put("39", new AgeRange(11, 20));
            m_validAgeMap.put("47", new AgeRange(11, 20));
            m_validAgeMap.put("25", new AgeRange(11, 14));
            m_validAgeMap.put("26", new AgeRange(11, 15));
            m_validAgeMap.put("32", new AgeRange(14, 20));
            m_validAgeMap.put("33", new AgeRange(14, 20));
            m_validAgeMap.put("44", new AgeRange(14, 20));
            m_validAgeMap.put("41", new AgeRange(10, 15));
            m_validAgeMap.put("46", new AgeRange(10, 17));
            m_validAgeMap.put("49", new AgeRange(3, 19));
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String schoolType = data.getFieldValue(data.getSchool(), "200632").trim();
            SisStudent student = (SisStudent) bean;
            PlainDate aug31 = (PlainDate) get(VAR_AGE_AT_DATE);
            int ageAsOfAug31 = student.getPerson().getAgeAsOfDate(aug31);
            boolean validAge = false;

            AgeRange ageRange = m_validAgeMap.get(schoolType);
            if (ageRange != null) {
                ageRange.setDob(ageAsOfAug31);
                try {
                    validAge = ageRange.check().booleanValue();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }

            if (!validAge) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Please Check: Pupil's age is out of range for School Type",
                        String.format("Pupil's age = %s, school type = %s", String.valueOf(ageAsOfAug31), schoolType)));
            }

            return errors;
        }
    }

    /**
     * ENTRYDATE
     *
     * Validate a student's entry date.
     *
     * @author Follett Software Company
     */
    public class ValidateEntryDate extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String entryDateAsString = value;
            PlainDate entryDate = null;
            try {
                entryDate = new PlainDate(getFormatter().parse(entryDateAsString));
            } catch (ParseException e) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Pupil's entry date is formatted incorrectly or empty",
                        ""));
            }

            if (entryDate != null) {
                PlainDate censusDate = (PlainDate) get(VAR_CENSUS_DATE);
                if (entryDate.after(censusDate)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Pupil's entry date to school missing or after Census date",
                            String.format("Entry date = %s, Census date = %s", entryDate.toString(),
                                    censusDate.toString())));
                }
            }

            return errors;
        }
    }

    /**
     * FSMELIGIBLE
     *
     * Validate a student's free school meal eligibility.
     *
     * @author Follett Software Company
     */
    public class ValidateFsmEligible extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String fsmEligible = value;

            // where <ServiceChild> (100330) = Y, <FSMeligible> must be 0 or false
            String serviceChild = data.getFieldValue(bean, "100330");
            if (serviceChild.matches("Y|1") && !fsmEligible.matches("0|[Ff](alse)?")) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Based on Ministry of Defence criteria, Service Children (i.e. parents " +
                                "designated as personnel category 1 or 2) are not eligible for Free School Meals",
                        String.format("<ServiceChild> = %s", serviceChild)));
            }

            return errors;
        }
    }

    /**
     * FUNDEDHOURS
     *
     * Validate a student's funded hours.
     *
     * @author Follett Software Company
     */
    public class ValidateFundedHours extends BeanValidator {
        private static final String FIELD_STD_HOURS_AT_SETTING = "100291";
        BigDecimal m_half = new BigDecimal(0.5);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String governance = data.getFieldValue(data.getSchool(), "200015");
            String schoolPhase = getSchoolPhase();
            boolean ctcSchool = governance.equals("CTC");
            boolean nmss = governance.equals("NM") && schoolPhase.equals("SS");
            SisStudent student = (SisStudent) bean;

            // for all schools except CTCs and NMSS
            if (!(ctcSchool || nmss)) {
                String fundedHours = value;
                int pupilAge = getStudentAgeAsOfAug31(student);
                String hoursAtSetting = data.getFieldValue(student, FIELD_STD_HOURS_AT_SETTING);

                // for pupils aged => 5, <FundedHours> must NOT be provided
                if (pupilAge >= 5 && !StringUtils.isEmpty(fundedHours)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Child's Funded Hours must not be provided for pupils aged 5 and over",
                            String.format("Pupil's age = %s", String.valueOf(pupilAge))));
                }

                // for pupils aged 2 and 3, <FundedHours> must be provided and in the range 0 to 15
                // to the nearest 0.5
                else if (pupilAge == 2 || pupilAge == 3) {
                    if (StringUtils.isNumeric(fundedHours)) {
                        BigDecimal hours = new BigDecimal(fundedHours);
                        BigDecimal remainder = hours.remainder(m_half);
                        if (remainder.compareTo(BigDecimal.ZERO) != 0) {
                            errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                                    "Child's Funded Hours not provided or out of the range 0 to 15 to the nearest 0.5 for pupils aged 2 and 3",
                                    String.format("Pupil's age = %s, <FundedHours> = %s", String.valueOf(pupilAge),
                                            fundedHours)));
                        }
                    }
                }

                // for pupils aged => 5, <HoursAtSetting> must NOT be provided
                else if (pupilAge >= 5) {
                    if (!StringUtils.isEmpty(hoursAtSetting)) {
                        errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                                "Child's Hours at Setting must not be provided for pupil aged 5 and over",
                                String.format("Pupil's age = %s, <HoursAtSetting> = %s", String.valueOf(pupilAge),
                                        hoursAtSetting)));
                    }
                }

                else if (2 <= pupilAge && pupilAge <= 4) {
                    if (StringUtils.isNumeric(hoursAtSetting)) {
                        BigDecimal hours = new BigDecimal(hoursAtSetting);
                        int compareTo = hours.compareTo(BigDecimal.ZERO);
                        if (compareTo < 0) {
                            errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                                    "Child's Hours at Setting is missing or invalid",
                                    String.format("Pupil's age = %s, <HoursAtSetting> = %s", Integer.valueOf(pupilAge),
                                            hoursAtSetting)));
                        }
                    }
                }
            }

            return errors;
        }
    }

    /**
     * GOVERNANCE
     *
     * Validate a school's governance.
     *
     * @author Follett Software Company
     */
    public class ValidateGovernance extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String governance = value;

            String schoolType = data.getFieldValue(bean, "200632");
            if ("CA".equals(governance) && !"49".equals(schoolType)) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "If School Governance is recorded as an Academy (CA) then School Type must also be recorded as an Academy with code 49",
                        String.format("Governance = %s, School Type = %s", governance, schoolType)));
            } else if ("CT".equals(governance) && !"47".equals(schoolType)) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "If Governance is recorded as a CTC (CT) then School Type must also be recorded as a CTC with code 47",
                        String.format("Governance = %s, School Type = %s", governance, schoolType)));
            }

            return errors;
        }
    }

    /**
     * SENPROVISION
     *
     * Validate &lt;SENPROVISION&gt;.
     *
     * @author Follett Software Company
     */
    public class ValidateSenProvision extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            // special schools only
            String schoolPhase = getSchoolPhase();
            if ("SP".equals(schoolPhase)) {
                // where <intake> (200014) = SPEC...
                String intake = data.getFieldValue(bean, "200014");
                String senProvision = value;
                if ("SPEC".equals(intake)) {
                    // <senprovision> (100472) must be A, P, or S
                    if (!senProvision.matches("A|P|S")) {
                        errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                                "Pupil in special school with no SEN",
                                String.format("<Intake> = %s, <SENprovision> = %s", intake, senProvision)));
                    }
                }
            }

            return errors;
        }
    }

    /**
     * LANGUAGE
     *
     * Validate a student's language.
     *
     * @author Follett Software Company
     */
    public class ValidateLanguage extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String schoolPhase = getSchoolPhase();

            // all schools except nurseries
            if (!schoolPhase.matches("NS")) {
                SisStudent student = (SisStudent) bean;
                PlainDate aug31 = (PlainDate) get(VAR_AGE_AT_DATE);
                int ageAsOfAug31 = student.getPerson().getAgeAsOfDate(aug31);
                String language = value;

                if (ageAsOfAug31 >= 5 && StringUtils.isEmpty(language)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Pupils aged 5 and over Language missing or invalid",
                            String.format("Pupil's age = %s, Language = %s", String.valueOf(ageAsOfAug31), language)));
                }
            }

            return errors;
        }
    }

    /**
     * LEAVINGDATE
     *
     * Validate a student's leaving date.
     *
     * @author Follett Software Company
     */
    public class ValidateLeavingDate extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            PlainDate leavingDate = null;
            try {
                leavingDate = new PlainDate(getFormatter().parse(value));
            } catch (ParseException e) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Pupil's leaving date is formatted incorrectly or empty",
                        ""));
            }
            PlainDate censusDate = (PlainDate) get(VAR_CENSUS_DATE);

            if (leavingDate != null && leavingDate.after(censusDate)) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Pupil's leaving date to school missing or after Census date",
                        String.format("Leaving date = %s, Census date = %s", leavingDate.toString(),
                                censusDate.toString())));
            }

            return errors;
        }
    }

    /**
     * LOWESTNCYEAR
     *
     * Validate a school's lowest NC year group.
     *
     * @author Follett Software Company
     */
    public class ValidateLowestNcYear extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String lowestNcYearAsString = value;
            String highestNcYearAsString = data.getFieldValue(bean, "200013");

            String intake = data.getFieldValue(bean, "200014");
            if ("SPEC".equals(intake)) {
                if (!"X".equals(lowestNcYearAsString) || !"X".equals(highestNcYearAsString)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "School's lowest National Curriculum Year Group cannot be greater than School's National Curriculum Year Group",
                            String.format("Low NC Year Group: %s, Highest Nc Year Group: %s", lowestNcYearAsString,
                                    highestNcYearAsString)));
                }
            } else {
                int lowestNcYearAsInt = convertNcYearToInt(lowestNcYearAsString);
                int highestNcYearAsInt = convertNcYearToInt(highestNcYearAsString);

                if (lowestNcYearAsInt > highestNcYearAsInt) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "School's lowest National Curriculum Year Group cannot be greater than School's National Curriculum Year Group",
                            String.format("Low NC Year Group: %s, Highest Nc Year Group: %s", lowestNcYearAsString,
                                    highestNcYearAsString)));
                }
            }

            return errors;
        }

        /**
         * N1 = -2
         * N2 = -1
         * R = 0
         * 1-14 = 1-14.
         *
         * @param ncYear String
         * @return int
         */
        private int convertNcYearToInt(String ncYear) {
            int result;
            if (StringUtils.isNumeric(ncYear)) {
                result = Integer.valueOf(ncYear).intValue();
            } else {
                if ("N1".equals(ncYear)) {
                    result = -2;
                } else if ("N2".equals(ncYear)) {
                    result = -1;
                } else {
                    result = 0;
                }
            }

            return result;
        }
    }

    /**
     * PARTTIME
     *
     * Validate &lt;PartTime&gt;.
     *
     * @author Follett Software Company
     */
    public class ValidatePartTime extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String partTimeAsString = value;
            SisStudent student = (SisStudent) bean;
            int ageAsOfAug31 = getStudentAgeAsOfAug31(student);

            if (BooleanAsStringConverter.TRUE.equals(partTimeAsString)) {
                if (5 <= ageAsOfAug31 && ageAsOfAug31 <= 15) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Pupils aged 5-15 cannot be shown as having part-time status",
                            String.format("Pupil's age = %s, <PartTime> = %s", String.valueOf(ageAsOfAug31),
                                    partTimeAsString)));
                }

                String boarder = data.getFieldValue(bean, "100067");
                if (!"N".equals(boarder)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "Part-time pupil shown as boarder",
                            String.format("<PartTime> = %s, <Boarder> = %s", partTimeAsString, boarder)));
                }
            }

            return errors;
        }
    }

    /**
     * PHASE
     *
     * Validate &lt;Phase&gt; (200006).
     *
     * @author Follett Software Company
     */
    public class ValidatePhase extends BeanValidator {

        /**
         * Map of phases and the valid estab codes
         */
        Map<String, String> m_validRegexMap = new HashMap<String, String>();

        /**
         * Instantiates a new validate phase.
         */
        public ValidatePhase() {
            // Nursery (<Phase>(200006)=NS): 1000-1099, 1800-1899 and 6000-6899
            m_validRegexMap.put("NS", "10[0-9]{2}|18[0-9]{2}|6[0-8][0-9]{2}");
            // Primary (<Phase>(200006)=PS): 2000-3999, 5200-5299, 5940-5949 and 6000-6899
            m_validRegexMap.put("PS", "[2-3][0-9]{3}|52[0-9]{2}|594[0-9]|6[0-8][0-9]{2}");
            // Middle-deemed Primary (<Phase>(200006)=MP): 2000-3999, 5200-5299, 5940-5949 and
            // 6000-6899
            m_validRegexMap.put("MP", "[2-3][0-9]{3}|52[0-9]{2}|594[0-9]|6[0-8][0-9]{2}");
            // Middle-deemed Secondary (<Phase>(200006)=MS): 4000-4999, 5400-5499, 5900-5939 and
            // 6000-6899
            m_validRegexMap.put("MS", "[2-3][0-9]{3}|52[0-9]{2}|594[0-9]|6[0-8][0-9]{2}");
            // Secondary (<Phase>(200006)=SS): 4000-4999, 5400-5499, 5900-5939, 6000-6899 and
            // 6900-6999
            m_validRegexMap.put("SS", "4[0-9]{3}|54[0-9]{2}|59[0-3][0-9]|6[0-8][0-9]{2}|69[0-9]{2}");
            // Special Schools (<Phase>(200006)=SP): 5950-5999, 6000-6899 and 7000-7999
            m_validRegexMap.put("SP", "59[5-9][0-9]|6[0-8][0-9]{2}|7[0-9]{3}");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String phase = value;

            String estabAsString = data.getFieldValue(bean, "200002");
            if (m_validRegexMap.containsKey(phase)) {
                String regexPattern = m_validRegexMap.get(phase);
                if (!estabAsString.matches(regexPattern)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "DfE Number and Phase are not consistent",
                            String.format("Dfe Number = %s, Phase = %s", estabAsString, phase)));
                }
            } else {
                // invalid
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Phase is missing or invalid",
                        String.format("Phase = %s", phase)));
            }

            return errors;
        }

    }

    /**
     * SCHOOLTYPE
     *
     * Validate a school's type.
     *
     * @author Follett Software Company
     */
    public class ValidateSchoolType extends BeanValidator {
        /**
         * Map of phases and their valid school type regex pattern
         */
        private Map<String, String> m_validRegexMap = new HashMap<String, String>();

        /**
         * Instantiates a new validate school type.
         */
        public ValidateSchoolType() {
            /*
             * <SchoolType> (200632) must be within the ranges for <Phase> (200006)
             *
             * NS 49, 50, 51
             * PS 01-04, 16-18, 42,43 and 49
             * MP 05-06, 45 and 49
             * MS 07, 08, 41 and 49
             * SS 09-12, 21, 22, 25-33, 36-39, 44, 46-49
             * SP 49, 52 and 53
             */
            m_validRegexMap.put("NS", "49|50|51"); // 49, 50, 51
            m_validRegexMap.put("PS", "0?[1-4]|1[6-8]|42|43|49"); // 01-04, 16-18, 42,43 and 49
            m_validRegexMap.put("MP", "0?[56]|45|49"); // 05-06, 45 and 49
            m_validRegexMap.put("MS", "0?[78]|41|49"); // 07, 08, 41 and 49
            m_validRegexMap.put("SS", "09|1[0-2]|21|22|2[5-9]|3[0-3]|3[6-9]|44|4[6-9]"); // 09-12,
                                                                                         // 21, 22,
                                                                                         // 25-33,
                                                                                         // 36-39,
                                                                                         // 44,
                                                                                         // 46-49
            m_validRegexMap.put("SP", "49|52|53"); // 49, 52 and 53
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String schoolType = value;

            // <SchoolType> (200632) must be within the ranges for <Phase> (200006)
            String phase = data.getFieldValue(bean, "200006");
            if (m_validRegexMap.containsKey(phase)) {
                String regexPattern = m_validRegexMap.get(phase);
                if (!schoolType.matches(regexPattern)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "School Type and Phase not consistent",
                            String.format("School Type = %s, Phase = %s", schoolType, phase)));
                }
            } else {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "School Type is missing",
                        ""));
            }

            // If <SchoolType> = 49, then <Governance> must be CA
            String governance = data.getFieldValue(bean, "200015");
            if ("49".equals(schoolType) && !"CA".equals(governance)) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "If School Type is recorded as an Academy (49) then the School Governance must also be recorded as an Academy with code CA",
                        String.format("School Type = %s, Governance = %s", schoolType, governance)));
            } else if ("47".equals(schoolType) && "CT".equals(governance)) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "If School Type is recorded as a CTC (47) then Governance must also be recorded as a CTC with code CT",
                        String.format("School Type = %s, Governance = %s", schoolType, governance)));
            }

            return errors;
        }
    }

    /**
     * TYPEOFCLASS
     *
     * Validate &lt;TypeOfClass&gt;.
     */
    public class ValidateTypeOfClass extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            String typeOfClass = value;
            SisStudent student = (SisStudent) bean;
            int ageAsOfAug31 = getStudentAgeAsOfAug31(student);

            if ("O".equals(typeOfClass) && ageAsOfAug31 < 6) {
                errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                        "Pupil aged over 6 or over is shown as being in a nursery class",
                        String.format("Pupil age = %s, <TypeOfClass> = %s", String.valueOf(ageAsOfAug31),
                                typeOfClass)));
            }

            return errors;
        }
    }

    /**
     * ULN
     *
     * Validate a student's unique learners number (ULN).
     *
     * @author Follett Software Company
     */
    public class ValidateUln extends BeanValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String uln = value.trim();

            if (uln.length() == 10) {
                int sum = 0;
                for (int i = 0; i < uln.length(); i++) {
                    int digit = Character.getNumericValue(uln.charAt(i));
                    sum += (digit * (10 - i));
                }

                int remainder = sum % 11;
                int checkDigit = Character.getNumericValue(uln.charAt(0));

                if (remainder != checkDigit) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "ULN Check Digit is incorrect - please check",
                            String.format("ULN = %s", uln)));
                }
            }

            return errors;
        }
    }

    /**
     * UPN
     *
     * Validate a student's unique pupil number (UPN).
     *
     * @author Follett Software Company
     */
    public class ValidateUpn extends BeanValidator {
        /**
         * Check letters available (yes, the letter I, O, S is missing)
         */
        char[] m_checkLetters = new char[] {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K',
                'L', 'M', 'N', 'P', 'Q', 'R', 'T', 'U', 'V', 'W',
                'X', 'Y', 'Z'};

        Set<String> m_validPostApr1999Codes = new HashSet<String>();

        /**
         * Instantiates a new validate upn.
         */
        public ValidateUpn() {
            populateValidPostApr1999Codes();
        }

        /**
         * Populate valid post apr 1999 codes.
         */
        private void populateValidPostApr1999Codes() {
            m_validPostApr1999Codes.addAll(getRange("001-005"));
            m_validPostApr1999Codes.addAll(getRange("201-213"));
            m_validPostApr1999Codes.addAll(getRange("330-336"));
            m_validPostApr1999Codes.addAll(getRange("350-359"));
            m_validPostApr1999Codes.addAll(getRange("380-384"));
            m_validPostApr1999Codes.addAll(getRange("420-420"));
            m_validPostApr1999Codes.addAll(getRange("701-708"));
            m_validPostApr1999Codes.addAll(getRange("805-808"));
            m_validPostApr1999Codes.addAll(getRange("815-815"));
            m_validPostApr1999Codes.addAll(getRange("820-823"));
            m_validPostApr1999Codes.addAll(getRange("826-826"));
            m_validPostApr1999Codes.addAll(getRange("831-831"));
            m_validPostApr1999Codes.addAll(getRange("840-840"));
            m_validPostApr1999Codes.addAll(getRange("845-845"));
            m_validPostApr1999Codes.addAll(getRange("850-852"));
            m_validPostApr1999Codes.addAll(getRange("860-860"));
            m_validPostApr1999Codes.addAll(getRange("865-896"));
            m_validPostApr1999Codes.addAll(getRange("909-909"));
            m_validPostApr1999Codes.addAll(getRange("919-919"));
            m_validPostApr1999Codes.addAll(getRange("925-925"));
            m_validPostApr1999Codes.addAll(getRange("928-928"));
            m_validPostApr1999Codes.addAll(getRange("931-931"));
            m_validPostApr1999Codes.addAll(getRange("935-938"));
            m_validPostApr1999Codes.addAll(getRange("301-320"));
            m_validPostApr1999Codes.addAll(getRange("340-344"));
            m_validPostApr1999Codes.addAll(getRange("370-373"));
            m_validPostApr1999Codes.addAll(getRange("390-394"));
            m_validPostApr1999Codes.addAll(getRange("660-681"));
            m_validPostApr1999Codes.addAll(getRange("800-803"));
            m_validPostApr1999Codes.addAll(getRange("810-813"));
            m_validPostApr1999Codes.addAll(getRange("816-816"));
            m_validPostApr1999Codes.addAll(getRange("825-825"));
            m_validPostApr1999Codes.addAll(getRange("830-830"));
            m_validPostApr1999Codes.addAll(getRange("835-837"));
            m_validPostApr1999Codes.addAll(getRange("841-841"));
            m_validPostApr1999Codes.addAll(getRange("846-846"));
            m_validPostApr1999Codes.addAll(getRange("855-857"));
            m_validPostApr1999Codes.addAll(getRange("861-861"));
            m_validPostApr1999Codes.addAll(getRange("908-908"));
            m_validPostApr1999Codes.addAll(getRange("916-916"));
            m_validPostApr1999Codes.addAll(getRange("921-921"));
            m_validPostApr1999Codes.addAll(getRange("926-926"));
            m_validPostApr1999Codes.addAll(getRange("929-929"));
            m_validPostApr1999Codes.addAll(getRange("933-933"));
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.BeanValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             X2BaseBean bean,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String upn = value.trim();

            /*
             * Valid UPN
             */
            if (upn.length() >= 13) {
                int sum = 0;
                for (int i = 1; i < 13; i++) {
                    /*
                     * 1. Multiply the individual digits by their weights as follows:
                     * digit 2 by weight 2; digit 3 by weight 3; digit 4 by weight 4;
                     * digit 5 by weight 5; digit 6 by weight 6; digit 7 by weight 7;
                     * digit 8 by weight 8; digit 9 by weight 9; digit 10 by weight 10;
                     * digit 11 by weight 11; digit 12 by weight 12; digit 13 by weight 13.
                     */
                    int digit = Character.getNumericValue(upn.charAt(i));
                    sum += (digit * (i + 1));
                }

                /*
                 * 2. Sum the individual results, divide the total by 23, and take the remainder.
                 */
                int remainder = sum % 23;

                /*
                 * 3. Calculate the check letter from the result as follows:
                 * 0 = A; 1 = B; 2 = C; 3 = D; 4 = E; 5 = F; 6 = G; 7 = H;
                 * 8 = J; 9 = K; 10 = L; 11 = M; 12 = N; 13 = P; 14 = Q; 15 = R;
                 * 16 = T; 17 = U; 18 = V; 19 = W; 20 = X; 21 = Y; 22 = Z
                 */
                char checkLetter = upn.charAt(0);
                if (checkLetter != m_checkLetters[remainder]) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "UPN invalid (wrong check letter at character 1)",
                            String.format("UPN = %s", upn)));
                }
            }

            /*
             * Characters 2-4 must be a valid post April 1999 LA code or a recognised "pseudo LA"
             * code
             */
            if (upn.length() > 4) {
                String laCode = upn.substring(1, 4);
                if (!m_validPostApr1999Codes.contains(laCode)) {
                    errors.add(new StateReportValidationError(getDescription(bean), field.getFieldId(),
                            "UPN invalid (characters 2-4 not a recognised LA code)",
                            String.format("UPN = %s", upn)));
                }
            }

            return errors;
        }
    }

    /**
     * Get a bean's identifying values
     * <p>
     * Customers can change this by going to the data dictionary's table's detail page.
     *
     * @param bean the bean
     * @return the bean's identifying values
     */
    public String getDescription(X2BaseBean bean) {
        String identifyingValues = bean.getIdentifyingValues(Locale.getDefault());
        return identifyingValues;
    }

    /**
     * Some UK exports' validations depend on a student's age
     * on 2012-08-31. This is just a convenience method.
     * <p>
     * The Student must have a Person object.
     *
     * @param student the student object
     * @return int
     */
    public int getStudentAgeAsOfAug31(Student student) {
        PlainDate aug31 = (PlainDate) get(VAR_AGE_AT_DATE);
        int ageAsOfAug31 = 0;
        if (student.getPerson() != null && student.getPerson().getDob() != null) {
            ageAsOfAug31 = student.getPerson().getAgeAsOfDate(aug31);
        }

        return ageAsOfAug31;
    }

    /**
     * Return a set of numbers based on a string.
     * <p>
     * <tt>s</tt> needs to be in this format: two integers separated by a hyphen (-).
     * For example, passing in <tt>"12-17"</tt> will return a Set
     * containing <tt>"12", "13", "14", "15", "16", "17"</tt>.
     * <p>
     * Range is <strong>inclusive</strong>, i.e. min <= number <= max.
     *
     * @param s representation of range
     * @return set of numbers from min to max
     */
    public Set<String> getRange(String s) {
        Set<String> set = new HashSet<String>();
        if (s != null && s.matches("^\\d+-\\d+$")) {
            int indexOf = s.indexOf('-');
            int min = Integer.parseInt(s.substring(0, indexOf));
            int max = Integer.parseInt(s.substring(indexOf + 1));
            set.addAll(getRange(min, max));
        }
        return set;
    }

    /**
     * Return a set of numbers from <tt>min</tt> to <tt>max</tt>.
     * <p>
     * Range is <strong>inclusive</strong>, i.e. min <= number <= max.
     *
     * @param min min number
     * @param max max number
     * @return Set of numbers from min to max
     */
    public Set<String> getRange(int min, int max) {
        Set<String> set = new HashSet<String>();
        for (int i = min; i <= max; i++) {
            set.add(String.valueOf(i));
        }
        return set;
    }

    /**
     * Check if a number is within a range
     * <p>
     * For example, <tt>isInRange(5, "1-10")</tt> will return <strong>true</strong>,
     * whereas <tt>isInRange(5, "1-4")</tt> will return <strong>false</strong>.
     * <p>
     * Range is <strong>inclusive</strong>, i.e. min <= number <= max.
     *
     * @param number int
     * @param s String
     * @return true, if is in range
     */
    public boolean isInRange(int number, String s) {
        boolean result = false;

        if (s != null && s.matches("^\\d+-\\d+$")) {
            int indexOf = s.indexOf('-');
            int min = Integer.parseInt(s.substring(0, indexOf));
            int max = Integer.parseInt(s.substring(indexOf + 1));
            result = (min <= number && number <= max);
        }

        return result;
    }

    /**
     * Gets the school phase.
     *
     * @return String
     */
    public String getSchoolPhase() {
        String schoolPhase = getData().getFieldValue(getData().getSchool(), FIELD_SKL_SCHOOL_PHASE);
        return schoolPhase;
    }

    /**
     * Attendance class.
     */
    private class AttendanceInfo {
        protected int m_sessionsPossible;
        protected int m_sessionsAuthorised;
        protected int m_sessionsUnauthorised;
        protected Map<String, Integer> m_sessionDetail;

        /**
         * Instantiates a new attendance info.
         */
        public AttendanceInfo() {
            m_sessionsPossible = 0;
            m_sessionsAuthorised = 0;
            m_sessionsUnauthorised = 0;
            m_sessionDetail = new HashMap<String, Integer>();
        }
    }

    /**
     * Staff Attendance class.
     */
    private class StaffAttendanceInfo {
        protected PlainDate m_firstDayOfAbsence;
        protected PlainDate m_lastDayOfAbsence;
        protected int m_workingDaysLost = 0;
        protected String m_absenceCategory;

        /**
         * Instantiates a new staff attendance info.
         */
        public StaffAttendanceInfo() {
            // Empty block
        }
    }

    /**
     * Exception for a method name not found.
     *
     * @author Follett Software Company
     */
    private class MethodNotFoundException extends RuntimeException {
        /**
         * Method name
         */
        private String m_methodName;

        /**
         * Constructor.
         *
         * @param methodName the method name
         */
        public MethodNotFoundException(String methodName) {
            m_methodName = methodName;
        }

        /**
         * @see java.lang.Throwable#toString()
         */
        @Override
        public String toString() {
            return String.format("%s: Unable to find method '%s'", getClass().getName(), m_methodName);
        }
    }

    /**
     * Exception for a retriever not found.
     *
     * @author Follett Software Company
     */
    private class RetrieverNotFoundException extends RuntimeException {
        /**
         * Retriever name
         */
        private String m_retrieverName;

        /**
         * Constructor.
         *
         * @param retrieverName the retriever name
         */
        public RetrieverNotFoundException(String retrieverName) {
            m_retrieverName = retrieverName;
        }

        /**
         * @see java.lang.Throwable#toString()
         */
        @Override
        public String toString() {
            return String.format("%s: Unable to find retriever '%s'", getClass().getName(), m_retrieverName);
        }
    }

    /**
     * Exception for a validator not found.
     *
     * @author Follett Software Company
     */
    private class ValidatorNotFoundException extends RuntimeException {
        /**
         * Validator name
         */
        private String m_validatorName;

        /**
         * Constructor.
         *
         * @param validatorName the validator name
         */
        public ValidatorNotFoundException(String validatorName) {
            m_validatorName = validatorName;
        }

        /**
         * @see java.lang.Throwable#toString()
         */
        @Override
        public String toString() {
            return String.format("%s: Unable to find validator '%s'", getClass().getName(), m_validatorName);
        }
    }

    /**
     * The retrievers available for this model.
     *
     * @author Follett Software Company
     */
    public enum Retriever {
        ALTPROVISION, ATTENDANCE, CLASS, STAFFABSENCES, STAFFCONTRACT, EXCLUSION, FSMELIGIBLE, LEARNINGAIM, PUPILSTATUS, SENPROVISION, SENUNITMEMBER, STATUS, SYSTEM, FUNDEDHOURS, HOURSATSETTING, RESOURCEDPROVISIONID, UNITCONTACTTIME
    }

    /**
     * The Enum Validator.
     */
    public enum Validator {
        ACCOMMODATION, CONNEXIONS, DOB, ENTRYDATE, EXCLUSIONCATEGORY, EXCLUSIONSTARTDATE, FSMELIGIBLE, FUNDEDHOURS, GOVERNANCE, LANGUAGE, LEAVINGDATE, LOWESTNCYEAR, PARTTIME, PHASE, SCHOOLTYPE, SENPROVISION, TYPEOFCLASS, ULN, UPN
    }

    /**
     * Execute a method.
     *
     * <pre>
     * <p>
     * <style>
     *   table { border: 0; font-size: 10pt; margin-bottom: 15px; }
     *
     *   th { text-align: left; padding: 5px; background: #333; color: #fff; }
     *   tr { background: #ddd; }
     *   tr:nth-child(odd) { background: #eee; }
     *   td { vertical-align: top; padding: 5px; }
     *
     *   ul { padding: 0 30px; margin: 0; }
     *   .col1 { font-weight: bold; }
     * </style>
     * <table id="methods">
     *   <tr>
     *     <th>Method</th>
     *     <th>Description</th>
     *     <th>Returns</th>
     *   </tr>
     *   <tr>
     *     <td class="col1">loadDates()</td>
     *     <td class="col2">
     *      Load and set the dates, depending on the report that is being run.
     *      <p>
     *      For the <strong>School Census</strong> report, the model requires the following
     *      variables to be set:
     *      <ul>
     *        <li><tt>report</tt> = <tt>SchoolCensus</tt></li>
     *        <li><tt>schoolYear</tt> = <tt>Integer</tt></li>
     *        <li><tt>term</tt> = <tt>AUT</tt>, <tt>SPR</tt>, or <tt>SUM</tt></li>
     *      </ul>
     *      <p>
     *      So for example, setting <tt>report</tt> = <tt>SchoolCensus</tt>, <tt>schoolYear</tt> =
     *      <tt>Integer.valueOf(2012)</tt>, and <tt>term</tt> = <tt>SPR</tt> will set the
     *      dates for the 2012 School Census report for the Spring term.
     *      <p>
     *      For the <strong>School Workforce Census</strong> report, the model requires the
     *      following variable to be set:
     *      <ul>
     *        <li><tt>report</tt> = <tt>SchoolWorkforce</tt></li>
     *      </ul>
     *     </td>
     *     <td class="col3">
     *       void
     *     </td>
     *   </tr>
     *   <tr>
     *     <td class=
    "col1">getAgencyTpSupportMap(Map&lt;String, Collection&lt;StaffPosition&gt;&gt; rolesMap)</td>
     *     <td class="col2">
     *       Given the Map of staff positions, count up and group by <strong>DFE SUPPORT CATEGORY</strong>.
     *     </td>
     *     <td class="col3">
     *       <strong>Map&lt;String, Integer&gt;</strong> - the key being the support category code, the value
     *       being how many staff there are in that category
     *     </td>
     *   </tr>
     * </table>
     * </pre>
     *
     * @param id String
     * @param params Object[]
     * @return Object
     */
    @Override
    public Object execute(String id, Object... params) {
        Object result = null;

        /*
         * loadDates
         *
         * Method name for loading dates for UK exports
         * School Census and Pupil Referral Unit requires to set "term" and "schoolYear"
         */
        if ("loadDates".equals(id)) {
            try {
                loadDates();
            } catch (ParseException e) {
                getData().addSetupError("ParseException", "Error parsing dates!");
            }
        }

        /*
         * getAgencyTpSupportMap
         *
         * Get the Map of <AgencyTPsupportCount> elements.
         * This is for School Workforce for the <School> module.
         */
        else if ("getAgencyTpSupportMap".equals(id)) {
            result = getAgencyTpSupportMap((Map<String, Collection<StaffPosition>>) params[0]);
        }

        else {
            throw new MethodNotFoundException(id);
        }

        return result;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportModel#getData()
     */
    @Override
    public XMLStateReportData getData() {
        return (XMLStateReportData) super.getData();
    }

    /**
     * Get the universal date formatter.
     *
     * @return yyyy-MM-dd SimpleDateFormat
     */
    public SimpleDateFormat getFormatter() {
        return VAL_FORMATTER;
    }

    /**
     * Return the agency tp support count map.
     *
     * @param rolesMap staff roles map
     * @return map of agency categories and their counts
     */
    public Map<String, Integer> getAgencyTpSupportMap(Map<String, Collection<StaffPosition>> rolesMap) {
        // get all the available support categories and put into supportCategorySet
        String supportCategoryJavaName = getData().translateAliasToJavaName(ALIAS_SFP_SUPPORT_CATEGORY, true);
        DataDictionaryField supportCategoryField =
                getData().getDataDictionaryField(StaffPosition.class, supportCategoryJavaName);
        ReferenceTable supportCategoryRefTable = supportCategoryField.getReferenceTable();
        Set<String> supportCategorySet = new HashSet<String>();
        if (supportCategoryRefTable != null) {
            Map<String, ReferenceCode> supportCategoryMap = supportCategoryRefTable.getCodeMap();
            supportCategorySet.addAll(supportCategoryMap.keySet());
        }

        Bag bag = new HashBag();
        Set<Entry<String, Collection<StaffPosition>>> roleEntrySet = rolesMap.entrySet();
        for (Entry<String, Collection<StaffPosition>> roleEntry : roleEntrySet) {
            Collection<StaffPosition> roles = roleEntry.getValue();
            for (StaffPosition role : roles) {
                String supportCategory = getData().getFieldValue(role, FIELD_SFP_SUPPORTCATEGORY);
                if (supportCategorySet.contains(supportCategory)) {
                    bag.add(supportCategory);
                }
            }
        }

        Map<String, Integer> agencyMap = new HashMap<String, Integer>();
        for (Object categoryAsObj : bag.uniqueSet()) {
            String category = (String) categoryAsObj;
            agencyMap.put(category, Integer.valueOf(bag.getCount(category)));
        }

        return agencyMap;
    }

    /**
     * Initialize a UK-related FieldRetriever
     * <p>
     * <style>
     * table { border: 0; font-family: consolas, courier-new; font-size: 10pt; margin-bottom: 15px;
     * }
     *
     * th { text-align: left; padding: 5px; background: #333; color: #fff; }
     * tr { background: #ddd; }
     * tr:nth-child(odd) { background: #eee; }
     * td { vertical-align: top; padding: 5px; }
     *
     * #retrievers th { background: #9c1d14; }
     * #retrievers tr { background: #fce7e6; }
     * #retrievers tr:nth-child(odd) { background: #f8cbc8; }
     *
     * ul { padding: 0 30px; margin: 0; }
     * .col1 { font-weight: bold; }
     * </style>
     * <table id="retrievers">
     * <tr>
     * <th colspan="3">
     * Retrievers
     * </th>
     * </tr>
     * <tr>
     * <th>ID</th>
     * <th>Description</th>
     * <th>Parameters</th>
     * </tr>
     * <tr>
     * <td class="col1">ATTENDANCE</td>
     * <td class="col2">Termly and annual attendance</td>
     * <td class="col3">
     * <ol>
     * <li>Collection&lt;SisStudent&gt;</li>
     * <li>StudentHistoryHelper</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">CLASS</td>
     * <td class="col2">Retrieve class (MasterSchedule) information</td>
     * <td class="col3">
     * <ol>
     * <li>Collection&lt;SisStudent&gt;</li>
     * <li>StudentHistoryHelper</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">CONTRACT</td>
     * <td class="col2">Retrieve staff contract information</td>
     * <td class="col3">
     * <ol>
     * <li>Map&lt;String, Collection&lt;StaffPosition&gt;&gt;</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">EXCLUSION</td>
     * <td class="col2">Retrieve exclusion fields from students</td>
     * <td class="col3">
     * <ol>
     * <li>Map&lt;String, Collection&lt;ConductAction&gt;&gt;</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">FSMELIGIBLE</td>
     * <td class="col2">Retrieve if a student has free meals</td>
     * <td class="col3">
     * <ol>
     * <li>Map&lt;String, Collection&lt;StudentProgramParticipation&gt;&gt;</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">STAFFCONTRACT</td>
     * <td class="col2">Retrieve a Staff's Person Category</td>
     * <td class="col3">
     * <ol>
     * <li>Map&lt;String, Collection&lt;StaffPosition&gt;&gt;() m_contractsMap</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">PUPILSTATUS</td>
     * <td class="col2">Retrieve &lt;PupilStatus&gt; calculations</td>
     * <td class="col3">
     * <ol>
     * <li>Map&lt;String, Collection&lt;StudentProgramParticipation&gt;&gt;</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">SENPROVISION</td>
     * <td class="col2">Retrieve a student's SEN provision</td>
     * <td class="col3">
     * <ol>
     * <li>Map&lt;String, Collection&lt;StudentProgramParticipation&gt;&gt;</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">SENUNITMEMBER</td>
     * <td class="col2">Check if the student is enrolled in a Special Education school</td>
     * <td class="col3">
     * <ol>
     * <li>StudentHistoryHelper</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">STAFFABSENCES</td>
     * <td class="col2">Retrieve a Staff's Absences</td>
     * <td class="col3">
     * <ol>
     * <li>SubQuery staffSubQuery</li>
     * </ol>
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">STATUS</td>
     * <td class="col2">Retrieve staffs' teaching statuses</td>
     * <td class="col3">
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">SYSTEM</td>
     * <td class="col2">Retrieve system values</td>
     * <td class="col3">
     * </td>
     * </tr>
     * <tr>
     * <td class="col1">ALTPROVISION</td>
     * <td class="col2">Retrieve Alternative Provision program values</td>
     * <td class="col3">
     * <ol>
     * <li>X2Criteria studentCriteria</li>
     * </ol>
     * </td>
     * </tr>
     * </table>
     *
     * @param id the retriever id
     * @param params parameters required to initialize retriever
     * @return the retriever associated to the id
     */
    @Override
    public FieldRetriever initializeRetriever(String id, Object... params) {
        FieldRetriever retriever = getRetriever(id);
        if (retriever == null) {
            Retriever re = null;

            try {
                re = Retriever.valueOf(id);
            } catch (IllegalArgumentException iae) {
                // wasn't able to find id, must have used misspelled retriever name
                throw new RetrieverNotFoundException(id);
            }

            switch (re) {
                case STATUS:
                    retriever = new RetrieveStatus();
                    break;
                case SYSTEM:
                    retriever = new RetrieveSystem();
                    break;
                case ATTENDANCE:
                    retriever = new RetrieveAttendance(params);
                    break;
                case CLASS:
                    retriever = new RetrieveClass(params);
                    break;
                case EXCLUSION:
                    retriever = new RetrieveExclusion(params);
                    break;
                case FSMELIGIBLE:
                    retriever = new RetrieveFSMEligibility(params);
                    break;
                case STAFFCONTRACT:
                    retriever = new RetrieveStaffContract(params);
                    break;
                case PUPILSTATUS:
                    retriever = new RetrievePupilStatus(params);
                    break;
                case SENPROVISION:
                    retriever = new RetrieveSenProvision(params);
                    break;
                case SENUNITMEMBER:
                    retriever = new RetrieveSenUnitMember(params);
                    break;
                case STAFFABSENCES:
                    retriever = new RetrieveStaffAbsences(params);
                    break;
                case ALTPROVISION:
                    retriever = new RetrieveAlternativeProvision(params);
                    break;
                case LEARNINGAIM:
                    retriever = new RetrieveLearningAim(params);
                    break;
                case FUNDEDHOURS:
                    retriever = new RetrieveFundedHours();
                    break;
                case HOURSATSETTING:
                    retriever = new RetrieveHoursAtSetting();
                    break;
                case RESOURCEDPROVISIONID:
                    retriever = new RetrieveResourcedProvisionIndicator(params);
                    break;
                case UNITCONTACTTIME:
                    retriever = new RetrieveUnitContactTime();
                    break;
            }

            addRetriever(id, retriever);
        }

        return retriever;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportModel#initializeValidator(java.lang.String,
     *      java.lang.Object[])
     */
    @Override
    public FieldValidator initializeValidator(String id, Object... params) {
        FieldValidator validator = getValidator(id);
        if (validator == null) {
            Validator v = null;

            try {
                v = Validator.valueOf(id);
            } catch (IllegalArgumentException iae) {
                throw new ValidatorNotFoundException(id);
            }

            switch (v) {
                case PHASE:
                    validator = new ValidatePhase();
                    break;
                case SCHOOLTYPE:
                    validator = new ValidateSchoolType();
                    break;
                case LOWESTNCYEAR:
                    validator = new ValidateLowestNcYear();
                    break;
                case GOVERNANCE:
                    validator = new ValidateGovernance();
                    break;
                case ACCOMMODATION:
                    validator = new ValidateAccommodation();
                    break;
                case UPN:
                    validator = new ValidateUpn();
                    break;
                case ULN:
                    validator = new ValidateUln();
                    break;
                case DOB:
                    validator = new ValidateDob();
                    break;
                case FSMELIGIBLE:
                    validator = new ValidateFsmEligible();
                    break;
                case CONNEXIONS:
                    validator = new ValidateConnexions();
                    break;
                case LANGUAGE:
                    validator = new ValidateLanguage();
                    break;
                case FUNDEDHOURS:
                    validator = new ValidateFundedHours();
                    break;
                case ENTRYDATE:
                    validator = new ValidateEntryDate();
                    break;
                case LEAVINGDATE:
                    validator = new ValidateLeavingDate();
                    break;
                case PARTTIME:
                    validator = new ValidatePartTime();
                    break;
                case TYPEOFCLASS:
                    validator = new ValidateTypeOfClass();
                    break;
                case SENPROVISION:
                    validator = new ValidateSenProvision();
                    break;
                case EXCLUSIONCATEGORY:
                    validator = new ValidateExclusionCategory();
                    break;
                case EXCLUSIONSTARTDATE:
                    validator = new ValidateExclusionStartDate();
                    break;
            }

            addValidator(id, validator);

        }
        return validator;
    }

    /**
     * Load all dates for a report
     * <p>
     * User must set "term" and "schoolYear". By default, this method will use
     * SPR and 2013 as the term and schoolYear, respectively.
     * <p>
     * Refer to Annex C in the School Census / PRU documentation
     *
     * @throws ParseException exception
     */
    public void loadDates() throws ParseException {
        String term = (String) get(VAR_TERM);
        if (StringUtils.isEmpty(term)) {
            term = CODE_TERM_SPRING;
        }

        Integer schoolYear = (Integer) get(VAR_SCHOOL_YEAR);
        if (schoolYear == null) {
            schoolYear = Integer.valueOf(2013);
        }

        // School Census and Pupil Referral Unit
        set(VAR_CENSUS_DATE, getData().getParameter(PARAM_CENSUS_DATE));
        set(VAR_TERM_START_DATE, getData().getParameter(PARAM_TERM_START_DATE));
        set(VAR_TERM_END_DATE, getData().getParameter(PARAM_TERM_END_DATE));
        set(VAR_FSM_START_DATE, getData().getParameter(PARAM_FSM_START_DATE));
        set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE));
        set(VAR_TERMLY_EXCLUSION_START_DATE, getData().getParameter(PARAM_TERMLY_EXCLUSION_START_DATE));
        set(VAR_TERMLY_EXCLUSION_END_DATE, getData().getParameter(PARAM_TERMLY_EXCLUSION_END_DATE));
        set(VAR_TERMLY_ATTENDANCE_START_DATE, getData().getParameter(PARAM_TERMLY_ATTENDANCE_START_DATE));
        set(VAR_TERMLY_ATTENDANCE_END_DATE, getData().getParameter(PARAM_TERMLY_ATTENDANCE_END_DATE));
        set(VAR_SUMMER_ATTENDANCE_START_DATE, getData().getParameter(PARAM_SUMMER_ATTENDANCE_START_DATE));
        set(VAR_SUMMER_ATTENDANCE_END_DATE, getData().getParameter(PARAM_SUMMER_ATTENDANCE_END_DATE));
        set(VAR_ANNUAL_ATTENDANCE_END_DATE, getData().getParameter(PARAM_ANNUAL_ATTENDANCE_END_DATE));
        set(VAR_PREVIOUS_TERM__START_DATE, getData().getParameter(PARAM_PREVIOUS_TERM__START_DATE));

        String ageAtDateString = (String) getData().getParameter(PARAM_AGE_AT_DATE);

        try {
            if (!StringUtils.isEmpty(ageAtDateString)) {
                set(VAR_AGE_AT_DATE, new PlainDate(getFormatter().parse(ageAtDateString)));
            } else {
                set(VAR_AGE_AT_DATE, new PlainDate(getFormatter().parse("2013-08-31")));
            }
        } catch (ParseException e) {
            e.printStackTrace();
        }

        // School Workforce Dates
        set(VAR_CONTRACT_START_DATE, getData().getParameter(PARAM_CONTRACT_START_DATE));
        set(VAR_CONTRACT_END_DATE, getData().getParameter(PARAM_CONTRACT_END_DATE));
        set(VAR_ABSENCE_START_DATE, getData().getParameter(PARAM_ABSENCE_START_DATE));
        set(VAR_ABSENCE_END_DATE, getData().getParameter(PARAM_ABSENCE_END_DATE));

        // Child In Need Dates
        set(VAR_PROGRAM_START_DATE, getData().getParameter(PARAM_PROGRAM_START_DATE));
        set(VAR_PROGRAM_END_DATE, getData().getParameter(PARAM_PROGRAM_END_DATE));


        // if ("SchoolWorkforce".equals(report))
        // {
        // Report Date
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2012-11-06")));
        // Staff Contracts and Roles Selection dates
        // set(VAR_CONTRACT_START_DATE, new PlainDate(getFormatter().parse("2011-09-01")));
        // set(VAR_CONTRACT_END_DATE, new PlainDate(getFormatter().parse("2012-08-31")));
        // //Staff Curriculum Term Selection Dates
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2012-09-01")));
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2012-11-06")));
        // //Test Dates
        // //set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2010-09-01")));
        // //set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2011-08-31")));
        // //Staff Absence Selection Dates
        // set(VAR_ABSENCE_START_DATE, new PlainDate(getFormatter().parse("2011-09-01")));
        // set(VAR_ABSENCE_END_DATE, new PlainDate(getFormatter().parse("2012-08-31")));
        // }
        // else if ("AlternativeProvision".equals(report))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2013-01-17")));
        // set(VAR_FSM_END_DATE, new PlainDate(getFormatter().parse("2012-01-20")));
        // }
        // else
        // {
        // if (schoolYear.intValue() == 2012)
        // {
        // if (CODE_TERM_SPRING.equals(term))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2012-01-19")));
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2012-01-01")));
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2012-04-08")));
        // set(VAR_FSM_START_DATE, new PlainDate(getFormatter().parse("2012-10-05")));
        // set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE));
        // set(VAR_TERMLY_EXCLUSION_START_DATE, new PlainDate(getFormatter().parse("2012-04-25")));
        // set(VAR_TERMLY_EXCLUSION_END_DATE, new PlainDate(getFormatter().parse("2012-08-31")));
        // set(VAR_TERMLY_ATTENDANCE_START_DATE, new PlainDate(getFormatter().parse("2011-09-01")));
        // set(VAR_TERMLY_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2011-12-31")));
        // }
        // else if (CODE_TERM_SUMMER.equals(term))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2012-05-17")));
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2012-04-09")));
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2012-07-31")));
        // set(VAR_FSM_START_DATE, new PlainDate(getFormatter().parse("2012-04-09")));
        // set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE));
        // set(VAR_TERMLY_EXCLUSION_START_DATE, new PlainDate(getFormatter().parse("2011-09-01")));
        // set(VAR_TERMLY_EXCLUSION_END_DATE, new PlainDate(getFormatter().parse("2011-12-31")));
        // set(VAR_TERMLY_ATTENDANCE_START_DATE, new PlainDate(getFormatter().parse("2012-01-01")));
        // set(VAR_TERMLY_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2012-04-08")));
        // }
        // else if (CODE_TERM_AUTUMN.equals(term))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2012-10-04")));
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2012-09-01")));
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2012-12-31")));
        // set(VAR_FSM_START_DATE, new PlainDate(getFormatter().parse("2012-09-01")));
        // set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE));
        // set(VAR_TERMLY_EXCLUSION_START_DATE, new PlainDate(getFormatter().parse("2012-01-01")));
        // set(VAR_TERMLY_EXCLUSION_END_DATE, new PlainDate(getFormatter().parse("2012-04-08")));
        // set(VAR_TERMLY_ATTENDANCE_START_DATE, new PlainDate(getFormatter().parse("2012-04-09")));
        // set(VAR_TERMLY_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2012-06-01")));
        // }
        //
        // set(VAR_ANNUAL_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2012-06-01")));
        // }
        // else
        // {
        // /*
        // * 2013 stuff
        // */
        // if (CODE_TERM_SPRING.equals(term))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2013-01-17"))); //input
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2013-01-01"))); // first of
        // the year
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2013-03-31"))); // input
        // (easter sunday)
        // set(VAR_FSM_START_DATE, new PlainDate(getFormatter().parse("2012-10-05"))); // input
        // set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE)); //same as current cencus date
        // set(VAR_TERMLY_EXCLUSION_START_DATE, new PlainDate(getFormatter().parse("2012-04-09")));
        // // input
        // set(VAR_TERMLY_EXCLUSION_END_DATE, new PlainDate(getFormatter().parse("2012-08-31"))); //
        // input
        // set(VAR_TERMLY_ATTENDANCE_START_DATE, new PlainDate(getFormatter().parse("2012-08-01")));
        // // input
        // set(VAR_TERMLY_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2012-12-31")));
        // // input
        // }
        // else if (CODE_TERM_SUMMER.equals(term))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2013-05-16")));
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2013-04-01"))); //found from
        // easter date
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2013-07-31")));
        // set(VAR_FSM_START_DATE, new PlainDate(getFormatter().parse("2013-01-18")));
        // set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE));
        // set(VAR_TERMLY_EXCLUSION_START_DATE, new PlainDate(getFormatter().parse("2012-09-01")));
        // set(VAR_TERMLY_EXCLUSION_END_DATE, new PlainDate(getFormatter().parse("2012-12-31")));
        // set(VAR_TERMLY_ATTENDANCE_START_DATE, new PlainDate(getFormatter().parse("2013-01-01")));
        // set(VAR_TERMLY_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2013-03-31")));
        // }
        // else if (CODE_TERM_AUTUMN.equals(term))
        // {
        // set(VAR_CENSUS_DATE, new PlainDate(getFormatter().parse("2013-10-03")));
        // set(VAR_TERM_START_DATE, new PlainDate(getFormatter().parse("2013-08-01")));
        // set(VAR_TERM_END_DATE, new PlainDate(getFormatter().parse("2013-12-31")));
        // set(VAR_FSM_START_DATE, new PlainDate(getFormatter().parse("2013-05-17")));
        // set(VAR_FSM_END_DATE, get(VAR_CENSUS_DATE));
        // set(VAR_TERMLY_EXCLUSION_START_DATE, new PlainDate(getFormatter().parse("2013-01-01")));
        // set(VAR_TERMLY_EXCLUSION_END_DATE, new PlainDate(getFormatter().parse("2013-03-31")));
        // set(VAR_TERMLY_ATTENDANCE_START_DATE, new PlainDate(getFormatter().parse("2013-04-01")));
        // set(VAR_TERMLY_ATTENDANCE_END_DATE, new PlainDate(getFormatter().parse("2013-05-26")));
        // }
        // }
        // }
    }
}
