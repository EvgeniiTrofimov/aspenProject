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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.Field;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.PersistenceBroker;
import org.apache.ojb.broker.query.Query;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student attendance export.
 */
public class TNStudentAttendanceData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for student attendance export.
     */
    public static class TNStudentAttendanceEntity extends TNStateReportEntity
            implements TNStateReportData.HasStudentRecordHelper {
        protected List<StudentRecordHelper> m_allStdHelper;
        protected TNStudentAttendanceData m_currentStateReportData = null;
        protected List<StudentSessionDayAttendanceHelper> m_list;
        protected List<StudentRecordHelper> m_stdRecordHelperList;

        /**
         * Instantiates a new TN student attendance entity.
         */
        public TNStudentAttendanceEntity() {

        }

        /**
         * Get the record helper for the current row.
         *
         * @return Student record helper
         */
        @Override
        public StudentRecordHelper getCurrentRecord() {
            return m_stdRecordHelperList.get(getCurrentRow());
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
            return student.getNameView()
                    + " [LASID: " + student.getLocalId()
                    + ", SASID: " + student.getStateId()
                    + "] ";
        }

        /**
         * Gets the student attendance data.
         *
         * @return Student session day attendance helper
         */
        public StudentSessionDayAttendanceHelper getStudentAttendanceData() {
            return m_list.get(getCurrentRow());
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

            m_currentStateReportData = (TNStudentAttendanceData) data;
            SisStudent student = (SisStudent) getBean();
            m_list = m_currentStateReportData.m_studentAttendanceHelperMap.get(student.getOid());
            if (m_list == null) {
                setRowCount(0);
            } else {
                m_allStdHelper = m_currentStateReportData.getStudentHelperMap().get(student.getOid());
                calculateRecords();
                setRowCount(m_list.size());
            }

            m_currentStateReportData.addEntityRowsCount(getRowCount());
        }

        /**
         * Calculate records.
         */
        private void calculateRecords() {
            Iterator<StudentSessionDayAttendanceHelper> iterator = m_list.iterator();
            m_stdRecordHelperList = new ArrayList<TNStateReportData.StudentRecordHelper>();
            while (iterator.hasNext()) {
                StudentSessionDayAttendanceHelper attendanceHelper = iterator.next();
                StudentRecordHelper studentReordHelper = findAppropriateStdRecordHelper(attendanceHelper);
                if (studentReordHelper == null) {
                    iterator.remove();
                } else {
                    m_stdRecordHelperList.add(studentReordHelper);
                }
            }
        }

        /**
         * Find appropriate std record helper.
         *
         * @param attendanceHelper StudentSessionDayAttendanceHelper
         * @return StudentRecordHelper
         */
        private StudentRecordHelper findAppropriateStdRecordHelper(StudentSessionDayAttendanceHelper attendanceHelper) {
            StudentRecordHelper returnValue = null;
            String stringAttendanceDate = attendanceHelper.getAttendanceDate();

            PlainDate attendanceDate = formatDate(stringAttendanceDate);

            for (StudentRecordHelper enrollHelper : m_allStdHelper) {
                PlainDate enrollDate = enrollHelper.getEnrollDate();
                PlainDate exitDate = enrollHelper.getExitDate();
                if (enrollDate != null && attendanceDate != null) {
                    if (isBetween(enrollDate, exitDate, attendanceDate)) {
                        returnValue = enrollHelper;
                        break;
                    }
                }
            }
            return returnValue;
        }



        /**
         * Format date.
         *
         * @param stringDate String
         * @return PlainDate
         */
        private PlainDate formatDate(String stringDate) {
            PlainDate returnDate = null;
            if (!StringUtils.isEmpty(stringDate)) {
                SimpleDateFormat dateFormat = new SimpleDateFormat("yyyyMMdd");
                try {
                    Date date = dateFormat.parse(stringDate);
                    if (date != null) {
                        returnDate = new PlainDate(date);
                    }
                } catch (ParseException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            return returnDate;
        }

        /**
         * Checks if is between.
         *
         * @param startPeriod PlainDate
         * @param endPeriod PlainDate
         * @param findingDate PlainDate
         * @return true, if is between
         */
        private boolean isBetween(PlainDate startPeriod, PlainDate endPeriod, PlainDate findingDate) {

            boolean isBetween = false;
            if (!findingDate.before(startPeriod) && (endPeriod == null || !findingDate.after(endPeriod))) {
                isBetween = true;
            }
            return isBetween;
        }
    }

    /**
     * Field retriever for Student Attendance fields.
     */
    protected class FieldRetrieverAttendance implements FieldRetriever {
        protected static final String ATT_CALC_ID = "ATT_CALC_ATTEND";

        protected static final String CALC_PARAM_ATTENDDATE = "ATT_ATTENDDATE";
        protected static final String CALC_PARAM_ATTENDTYPE = "ATT_ATTENDTYPE";
        protected static final String CALC_PARAM_SCHOOL = "ATT_SCHOOL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentAttendanceEntity seEntity = (TNStudentAttendanceEntity) entity;
            StudentSessionDayAttendanceHelper attendData = seEntity.getStudentAttendanceData();

            String param = (String) field.getParameter();

            if (param.equalsIgnoreCase(CALC_PARAM_ATTENDDATE)) {
                return attendData.getAttendanceDate();
            }

            if (param.equalsIgnoreCase(CALC_PARAM_ATTENDTYPE)) {
                return attendData.getAttendanceStatus();
            }

            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOL)) {
                return attendData.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId);
            }

            return "##Invalid-Param: " + param;
        }
    }

    /**
     * Field retriever for Instr program and SchoolDate fields.
     */
    protected class FieldRetrieverOther implements FieldRetriever {
        protected static final String ATT_CALC_ID = "ATT_CALC_OTHER";

        protected static final String CALC_PARAM_SCHOOLYEAR = "ATT_SCHOOLYEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentAttendanceData seData = (TNStudentAttendanceData) data;

            String param = (String) field.getParameter();

            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                return seData.m_schoolYear;
            }

            return null;
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String ATT_CALC_ID = "ATT_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentAttendanceEntity seEntity = (TNStudentAttendanceEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
            Person psn = student.getPerson();

            if (psn == null) {
                return "";
            }

            String ssn = psn.getPersonId();
            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * Field validator for PIN or LOCAL STUDENT KEY.
     * Validates SSN against non-numeric or missing value
     *
     */
    protected class FieldValidatorNonNumericOrEmpty implements FieldValidator {
        protected static final String ATT_VAL_ID = "ATT_VAL";
        private static final String patternSSN = "^[0-9]+$|^$";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field, "Missing Value",
                        field.getFieldId() + " is missing."));
            } else if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        field.getFieldId() + " value must be numeric. Incorrect value: " + value));
            }

            return errors;
        }

    }

    /**
     * Validate Student.[DOE PIN]
     */
    protected class FieldValidatorPIN implements FieldValidator {
        protected static final String ATT_VAL_ID = "ATT_VAL_PIN";

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
            Collection errors = new ArrayList<StateReportValidationError>();
            String ssn = entity.getFieldValue("STUDENT SSN");
            if (StringUtils.isEmpty(value) && StringUtils.isEmpty(ssn)) {
                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "Student SSN and DOE PIN are missing.", "At least one of values must represent.");
                errors.add(error);
            } else if (!StringUtils.isEmpty(value) && !StringUtils.isEmpty(ssn)) {
                StringBuilder errorMessage = new StringBuilder("PIN should exist only if SSN is empty. ");
                errorMessage.append("SSN: ");
                errorMessage.append(ssn);
                errorMessage.append(". DOE PIN: ");
                errorMessage.append(value);

                StateReportValidationError error = new StateReportValidationError(entity, field,
                        "Student SSN and DOE PIN both exist.", errorMessage.toString());
                errors.add(error);
            }
            return errors;
        }
    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FieldValidatorSSN implements FieldValidator {
        protected static final String ATT_VAL_ID = "ATT_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long. Incorrect value: " + value));
            }

            return errors;
        }
    }

    /**
     * Helper class for storing attendance data for a student.
     */
    protected class StudentSessionDayAttendanceHelper {
        private String m_attendanceDateString;
        private String m_attendanceStatus;
        private SisSchool m_school;

        /**
         * Instantiates a new student session day attendance helper.
         *
         * @param school SisSchool
         * @param sessionDay PlainDate
         * @param attendanceStatus String
         */
        public StudentSessionDayAttendanceHelper(SisSchool school, PlainDate sessionDay, String attendanceStatus) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
            m_attendanceStatus = attendanceStatus;
            m_attendanceDateString = sdf.format(sessionDay);
            m_school = school;
        }

        /**
         * Gets the attendance date.
         *
         * @return String
         */
        public String getAttendanceDate() {
            return m_attendanceDateString;
        }

        /**
         * Gets the attendance status.
         *
         * @return String
         */
        public String getAttendanceStatus() {
            return m_attendanceStatus;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }
    }

    /**
     * @author Follett Software Company
     * @copyright 2020
     */
    protected class CachingQueryIterator<T> extends QueryIterator<T> {
        private Collection<T> m_beans;
        private Iterator<T> m_iterator;
        private PersistenceKey m_persistenceKey;

        public CachingQueryIterator(Collection<T> beans) {
            m_beans = beans;
            m_iterator = beans.iterator();
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#close()
         */
        @Override
        public void close() {
            m_beans = null;
            m_iterator = null;
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#getPersistenceKey()
         */
        @Override
        public PersistenceKey getPersistenceKey() {
            return m_persistenceKey;
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#hasNext()
         */
        @Override
        public boolean hasNext() {
            return m_iterator.hasNext();
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#next()
         */
        @Override
        public T next() {
            return m_iterator.next();
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#remove()
         */
        @Override
        public void remove() {
            m_iterator.remove();
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#finalize()
         */
        @Override
        protected void finalize() {
            // super.finalize();
        }

        /**
         * @see com.follett.fsc.core.k12.beans.QueryIterator#getIterator(org.apache.ojb.broker.PersistenceBroker,
         *      org.apache.ojb.broker.query.Query)
         */
        @Override
        protected Iterator getIterator(PersistenceBroker broker, Query query) {
            return null;
        }
    }


    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_ATTENDANCE_REPORTED = "DOE ATTENDANCE REPORTED";

    protected static final String INPUT_REPORT_END_DATE = "reportEndDate";
    protected static final String INPUT_REPORT_START_DATE = "reportStartDate";
    protected static final String INPUT_USE_REPORT_DATE_RANGE = "useReportDateRange";
    protected static final String INPUT_PARAM_PROGRAM_CODE = "programCode";

    protected static final String FILLER_SPACE = " ";
    /*
     * Instance variables.
     */
    protected PlainDate m_contextEndDate;
    protected PlainDate m_contextStartDate;

    protected String m_fieldExcludeStudent;
    protected String m_fieldAttendanceReported;

    protected PlainDate m_reportEndDate;
    protected PlainDate m_reportStartDate;
    protected Boolean m_useReportDateRange;
    protected String m_schoolYear;

    /**
     * Helper map for storing student attendance data. key = studentOid, value attendanceData
     */
    protected Map<String, List<StudentSessionDayAttendanceHelper>> m_studentAttendanceHelperMap;

    protected TNStudentHistoryHelper m_studentHelper;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeQuery();
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Checks if is between.
     *
     * @param startPeriod PlainDate
     * @param endPeriod PlainDate
     * @param findingDate PlainDate
     * @return true, if is between
     */
    public boolean isBetween(PlainDate startPeriod, PlainDate endPeriod, PlainDate findingDate) {

        boolean isBetween = false;
        if (!findingDate.before(startPeriod) && (endPeriod == null || !findingDate.after(endPeriod))) {
            isBetween = true;
        }
        return isBetween;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#open()
     */
    @Override
    public boolean open() {
        // A null m_query means nothing to return
        if (getQuery() == null) {
            return true;
        }

        Field iteratorField = getPrivateField(StateReportData.class, "m_iterator");
        if (iteratorField == null) {
            throw new RuntimeException("Unable to access m_iterator.");
        }

        boolean openResult = false;
        try {
            Collection<X2BaseBean> beans = getBroker().getCollectionByQuery(getQuery());
            CachingQueryIterator<X2BaseBean> sifIterator = new CachingQueryIterator<X2BaseBean>(beans);

            iteratorField.set(this, sifIterator);
            openResult = true;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }

        return openResult;
    }

    /**
     *
     * @param origClazz
     * @param fieldName
     * @return
     */
    protected Field getPrivateField(Class origClazz, String fieldName) {
        Field foundField = null;
        Class clazz = origClazz;

        while (true) {
            try {
                Field field = clazz.getDeclaredField(fieldName);
                if (field != null) {
                    foundField = field;
                    foundField.setAccessible(true);
                    break;
                }
            } catch (NoSuchFieldException ex) {
                // Not found on class. Try superclass.
                clazz = clazz.getSuperclass();
                if (clazz == null) {
                    break;
                }
            } catch (Exception ex) {
                String message =
                        "Unable to locate field [" + fieldName + "] on class [" + origClazz.getSimpleName() + "] \n"
                                + LoggerUtils.convertThrowableToString(ex);
                throw new RuntimeException(message);
            }
        }

        return foundField;
    }

    /**
     * This method gets attendance data
     * for each inSessionDay for each student in a query.
     *
     * @param query QueryByCriteria
     * @return void
     */
    private void getAttendanceDataForStudents(QueryByCriteria query) {
        QueryIterator iter = getBroker().getIteratorByQuery(query);
        PlainDate startDate = null;
        PlainDate endDate = null;
        if (m_useReportDateRange.booleanValue()) {
            startDate = m_reportStartDate;
            endDate = m_reportEndDate;
        } else {
            startDate = m_contextStartDate;
            endDate = m_contextEndDate;
        }

        try {
            while (iter.hasNext()) {
                SisStudent student = (SisStudent) iter.next();
                String studentOid = student.getOid();

                if (!m_studentAttendanceHelperMap.containsKey(studentOid)) {
                    m_studentAttendanceHelperMap.put(studentOid, new ArrayList<StudentSessionDayAttendanceHelper>());
                }
                List<StudentAttendance> attendancesList = m_studentHelper.getStudentAttendances(studentOid);
                List<TNStudentEnrollmentSpan> spans =
                        m_studentHelper.getTNStudentEnrollmentSpans(student, true);
                if (attendancesList != null) {
                    for (StudentAttendance attendance : attendancesList) {
                        PlainDate attendanceDate = attendance.getDate();

                        if (!startDate.after(attendanceDate) && !endDate.before(attendanceDate)) {
                            String status = (String) attendance.getFieldValueByBeanPath(m_fieldAttendanceReported);
                            String reportedStatus = lookupStateValue(attendance.getClass(), m_fieldAttendanceReported,
                                    status);
                            if (!"P".equals(reportedStatus) && !StringUtils.isEmpty(reportedStatus)) {
                                StudentSessionDayAttendanceHelper sah = new StudentSessionDayAttendanceHelper(
                                        getSchoolForAttendance(attendance, spans), attendanceDate, reportedStatus);
                                m_studentAttendanceHelperMap.get(studentOid).add(sah);
                            }
                        }
                    }
                }
            }
        } finally {
            iter.close();
        }
    }

    /**
     * Method for implementing business rule for schoolYear.
     *
     * @return string representation of school year = (CTX_SCHOOL_YEAR - 1)
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Look at enrollment spans to capture the primary school and always report that school as
     * Attendance School for the given attendance.
     *
     * @param att
     * @return
     */
    private SisSchool getSchoolForAttendance(StudentAttendance att, List<TNStudentEnrollmentSpan> spans) {
        SisSchool skl = null;
        if (spans != null) {
            for (TNStudentEnrollmentSpan span : spans) {
                if (isBetween(span.getFirstActiveDate(), span.getLastActiveDate(), att.getDate())) {
                    skl = span.getSchool();
                    break;
                }
            }
        }
        if (skl == null) {
            skl = att.getSchool();
        }
        return skl;
    }

    /**
     * Method for building custom Student criteria.
     *
     * @return criteria for query for list of active students
     *         limited by active and not excluded students, reportDate range and school
     */
    private X2Criteria getStudentCriteria() {
        if (m_useReportDateRange.booleanValue()) {
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_reportStartDate);
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportEndDate);
        } else {
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_contextStartDate);
            m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_contextEndDate);
        }
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL,
                isSchoolContext() ? Boolean.TRUE : Boolean.FALSE);

        X2Criteria criteria = m_studentHelper.getStudentCriteria();

        applyInputCriteria(criteria, false, null);

        return criteria;
    }

    /**
     * Lookup field aliases and paths.
     * Get data from input definition
     */
    private void initializeFields() {
        m_fieldExcludeStudent = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, true);
        m_fieldAttendanceReported = translateAliasToJavaName(ALIAS_ATTENDANCE_REPORTED, true);

        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        m_studentHelper = helper.getStudentHistoryHelper();
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);

        m_contextStartDate = getCurrentContext().getStartDate();
        m_contextEndDate = getCurrentContext().getEndDate();

        m_reportStartDate = (PlainDate) getParameter(INPUT_REPORT_START_DATE);
        m_reportEndDate = (PlainDate) getParameter(INPUT_REPORT_END_DATE);
        m_useReportDateRange = (Boolean) getParameter(INPUT_USE_REPORT_DATE_RANGE);

        m_schoolYear = getCurentSchoolYear();

        m_studentAttendanceHelperMap = new HashMap<String, List<StudentSessionDayAttendanceHelper>>();
    }

    /**
     * Initialize query.
     */
    private void initializeQuery() {
        X2Criteria criteria = getStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        initStudentHelperMap(m_studentHelper, query);
        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(TNStudentAttendanceEntity.class);

        getAttendanceDataForStudents(query);
    }

    /**
     * Register custom field retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(FieldRetrieverAttendance.ATT_CALC_ID, new FieldRetrieverAttendance());
        calcs.put(FieldRetrieverOther.ATT_CALC_ID, new FieldRetrieverOther());
        calcs.put(FieldRetrieverSSN.ATT_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FieldValidatorNonNumericOrEmpty.ATT_VAL_ID, new FieldValidatorNonNumericOrEmpty());
        validators.put(FieldValidatorSSN.ATT_VAL_ID, new FieldValidatorSSN());
        validators.put(FieldValidatorPIN.ATT_VAL_ID, new FieldValidatorPIN());
        super.addValidators(validators);
    }
}
