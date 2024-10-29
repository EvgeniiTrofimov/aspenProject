/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.SchedulePeriod;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Exports all students active in the school at the time. For each student it will list their daily
 * attendnace and
 * their period attendance for the selected date.
 *
 * @author Follett Software Company
 */
public class AutoDialerExport extends ExportJavaSource {
    /**
     * StudentAssistance class for gathering data, using StudentHistoryHelper, calculating
     * enrollment history.
     * This export should report a row for each active on report date student/school combination.
     *
     * @author Follett Development Corporation
     */
    class StudentAssistance extends StateReportData {
        /*
         * Instance variables.
         */
        protected PlainDate m_attendanceDate;
        protected StudentHistoryHelper m_helper;
        protected ArrayList<String> m_students;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_attendanceDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_attendanceDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            setQuery(m_helper.getStudentQuery(false));
        }

        /**
         * check student status (active/non active) on attendanceDate.
         *
         * @param student SisStudent
         * @return boolean
         */
        public boolean isStudentActive(SisStudent student) {
            boolean isActive = false;

            List<StudentEnrollmentSpan> enrollmentSpans = m_helper.getStudentEnrollmentSpans(student, true);
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                if (m_attendanceDate.after(span.getFirstActiveDate())
                        && (span.getLastActiveDate() == null || m_attendanceDate.before(span.getLastActiveDate()))) {
                    Set<PlainDate> inSessionDays =
                            m_helper.getCalendarDays(span.getSchool(), student.getCalendarCode());
                    if (inSessionDays == null || inSessionDays.contains(m_attendanceDate)) {
                        isActive = true;
                    }
                }
            }

            return isActive;
        }

        /**
         * Set End Date.
         *
         * @param date void
         */
        public void setAttendanceDate(PlainDate date) {
            m_attendanceDate = date;
        }

        /**
         * Get Active Students.
         *
         * @return QueryByCriteria
         */
        protected QueryByCriteria getStudentsQuery() {
            return getQuery();
        }

        /**
         * Return the current student criteria.
         * 
         * @return Criteria
         */
        protected Criteria getStudentCriteria() {
            return m_helper.getStudentCriteria();
        }
    }

    /*
     * Input paraters
     */
    private static final String PARAM_ATTENDANCE_DATE = "attendanceDate";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";

    /*
     * General constants
     */
    private static final String ACTIVE_STUDENT_VALUE = "A";
    private static final String ALIAS_AUTODIAL = "ctj-prevent-autodial";
    private static final String AUTODIAL_Y_VALUE = "Y";
    private static final String AUTODIAL_N_VALUE = "N";
    private static final char CHAR_VALUE_DELIMITER = '\t';
    private static final String SIS_ID_VALUE = "COQ25";

    /*
     * Set up attendance code fields
     */
    private static final int ATTENDANCE_CODE_FIELDS_QUANTITY = 32;
    private static final List<String> PERIOD_ATTENDANCE_CODE_FIELDS;

    /**
     * static block that generates PERIOD_ATTENDANCE_CODE_FIELDS values
     */
    static {
        PERIOD_ATTENDANCE_CODE_FIELDS = new ArrayList<String>(ATTENDANCE_CODE_FIELDS_QUANTITY);
        for (int i = 0; i < ATTENDANCE_CODE_FIELDS_QUANTITY; i++) {
            PERIOD_ATTENDANCE_CODE_FIELDS.add(i + 1 + " Period Attendance Code");
        }
    }

    /**
     * The Enum REQUIRED_FIELDS.
     */
    private enum REQUIRED_FIELDS {
        FIELD_SIS_ID("SIS Identifier", "SISID"), FIELD_STUD_ID("Student Number", "STUDID"), FIELD_STUD_LAST_NAME(
                "Student Last Name",
                "STUDLNAME"), FIELD_STUD_FIRST_NAME("Student First Name", "STUDFNAME"), FIELD_STUD_STATUS(
                        "Student Status",
                        "STUDSTATUS"), FIELD_PHONE_NUMBER("Telephone Number", "PHONE"), FIELD_PREV_AUTODIALING(
                                "Prevent Autodialing", "PREVAUTODIALING"), FIELD_GRADE_LEVEL("Student Grade Level",
                                        "GRADELAVEL"), FIELD_HOMEROOM_NUMBER("Student Homeroom Number",
                                                "HOMEROOMNUM"), FIELD_ATTENDANCE_CODE("Daily Attendance Code", "ACODE");

        private String m_fieldId;
        private String m_fieldName;

        /**
         * Instantiates a new required fields.
         *
         * @param fieldName String
         * @param id String
         */
        private REQUIRED_FIELDS(String fieldName, String id) {
            m_fieldName = fieldName;
            m_fieldId = id;
        }

        /**
         * Gets the field id.
         *
         * @return String
         */
        public String getFieldId() {
            return m_fieldId;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldName;
        }
    }

    private PlainDate m_date;
    private Map<String, StudentAttendance> m_attendanceMap;
    private Collection<String> m_studentAutodialAllowed;
    private Collection<String> m_schoolOids;
    private StudentAssistance m_studHelper;
    private Map<String, Collection<StudentPeriodAttendance>> m_periodAttendancesMap;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid();

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, buildCriteria());
        query.addOrderByAscending(SisStudent.COL_SCHOOL_OID);
        query.addOrderByAscending(SisStudent.COL_LOCAL_ID);

        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();

                if (m_studHelper.isStudentActive(student)) {
                    grid.append();
                    grid.set(REQUIRED_FIELDS.FIELD_SIS_ID.getFieldId(), SIS_ID_VALUE);
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_ID.getFieldId(), student.getLocalId());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_LAST_NAME.getFieldId(), student.getPerson().getLastName());
                    grid.set(REQUIRED_FIELDS.FIELD_STUD_FIRST_NAME.getFieldId(), student.getPerson().getFirstName());
                    grid.set(REQUIRED_FIELDS.FIELD_GRADE_LEVEL.getFieldId(), student.getGradeLevel());
                    grid.set(REQUIRED_FIELDS.FIELD_HOMEROOM_NUMBER.getFieldId(), student.getHomeroom());
                    grid.set(REQUIRED_FIELDS.FIELD_PHONE_NUMBER.getFieldId(),
                            convertPhoneNumberFormat(student.getPerson().getPhone01()));

                    if (StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus())) {
                        grid.set(REQUIRED_FIELDS.FIELD_STUD_STATUS.getFieldId(), ACTIVE_STUDENT_VALUE);
                    }

                    if (m_studentAutodialAllowed != null && m_studentAutodialAllowed.contains(student.getOid())) {
                        grid.set(REQUIRED_FIELDS.FIELD_PREV_AUTODIALING.getFieldId(), AUTODIAL_Y_VALUE);
                    } else {
                        grid.set(REQUIRED_FIELDS.FIELD_PREV_AUTODIALING.getFieldId(), AUTODIAL_N_VALUE);
                    }

                    /*
                     * Set daily attendance
                     */
                    StudentAttendance attendance = m_attendanceMap.get(student.getOid());
                    if (attendance != null) {
                        grid.set(REQUIRED_FIELDS.FIELD_ATTENDANCE_CODE.getFieldId(), attendance.getCodeView());
                    }

                    /*
                     * Set period attendance
                     */
                    Collection<StudentPeriodAttendance> studentPeriodAttendances =
                            m_periodAttendancesMap.get(student.getOid());
                    if (studentPeriodAttendances != null) {
                        for (StudentPeriodAttendance studentAttendance : studentPeriodAttendances) {
                            String[] periodIds = studentAttendance.getPeriodView().split(",");
                            Collection<SchedulePeriod> periods =
                                    studentAttendance.getMasterSchedule().getSchedule().getSchedulePeriods();
                            for (String periodId : periodIds) {
                                for (SchedulePeriod period : periods) {
                                    if (period.getId().compareTo(periodId) == 0) {
                                        grid.set(PERIOD_ATTENDANCE_CODE_FIELDS.get(period.getNumber() - 1),
                                                validateCode(studentAttendance.getCodeView()));
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } finally {
            students.close();
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columnNames = new ArrayList(REQUIRED_FIELDS.values().length);

        for (REQUIRED_FIELDS field : REQUIRED_FIELDS.values()) {
            columnNames.add(field.getFieldId());
        }

        columnNames.addAll(PERIOD_ATTENDANCE_CODE_FIELDS);

        return columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columnNames = new ArrayList(REQUIRED_FIELDS.values().length);

        for (REQUIRED_FIELDS field : REQUIRED_FIELDS.values()) {
            columnNames.add(field.getFieldName());
        }

        columnNames.addAll(PERIOD_ATTENDANCE_CODE_FIELDS);

        return columnNames;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        StringBuilder headerBuilder = new StringBuilder();
        SimpleDateFormat formatter = new SimpleDateFormat("yyyyMMdd");

        headerBuilder.append(SIS_ID_VALUE)
                .append("\t")
                .append(formatter.format(m_date))
                .append(FORMAT_EOL_WINDOWS);

        return headerBuilder.toString();
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        setIncludeHeaderRow(false);
        setValueDelimiter(Character.valueOf(CHAR_VALUE_DELIMITER));
        setUseValueWrappers(false);
        setLineSeparator(FORMAT_EOL_WINDOWS);

        m_date = (PlainDate) getParameter(PARAM_ATTENDANCE_DATE);

        m_studHelper = new StudentAssistance();
        m_studHelper.setBroker(getBroker());
        m_studHelper.setCurrentContext(getCurrentContext());
        m_studHelper.setPrivilegeSet(getPrivilegeSet());
        m_studHelper.setOrganization(OrganizationManager.getRootOrganization(getOrganization()));
        m_studHelper.setAttendanceDate(m_date);
        m_studHelper.setSchoolContext(false);
        m_studHelper.setParameters(getParameters());
        m_studHelper.initializeExport();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        loadStudentAttendence();
        loadStudentPeriodAttendance();
        loadStudentAutodialAllowed(dictionary);
    }

    /**
     * Returns the criteria for students included in the export
     *
     * TODO: This returns students currently in the selected schools. Since this is run for historic
     * dates we need to
     * get the students who were in the school on the selected date.
     *
     * @return Criteria
     */
    private Criteria buildCriteria() {
        Criteria criteria = m_studHelper.getStudentCriteria();
        criteria.addIn(Student.COL_SCHOOL_OID, getSchoolOids());

        return criteria;
    }

    /**
     * Formats the phone number.
     *
     * @param phoneNumber String
     * @return String
     */
    private String convertPhoneNumberFormat(String phoneNumber) {
        String value = null;

        if (phoneNumber != null) {
            String result = phoneNumber.replaceAll("[^0-9]", "");

            if (result.length() == 7 || result.length() == 10 || (result.length() == 11 && result.startsWith("1"))) {
                value = result;
            }
        }

        return value;
    }

    /**
     * Returns collection with export school oids.
     *
     * @return Collection<String>
     */
    private Collection<String> getSchoolOids() {
        if (m_schoolOids == null) {
            X2Criteria criteria = new X2Criteria();

            if (getSchool() != null) {
                criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
            } else {
                String oids = (String) getParameter(PARAM_SCHOOL_OIDS);
                if (!StringUtils.isEmpty(oids)) {
                    Collection<String> oidList = StringUtils.convertDelimitedStringToList(oids, ',', true);
                    criteria.addIn(X2BaseBean.COL_OID, oidList);
                } else {
                    criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                    criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                    criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
                }
            }

            m_schoolOids = getBroker()
                    .getSubQueryCollectionByQuery(new SubQuery(SisSchool.class, X2BaseBean.COL_OID, criteria));
        }

        return m_schoolOids;
    }

    /**
     * Load daily attendance keyed to the student OID.
     */
    private void loadStudentAttendence() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentAttendance.COL_DATE, m_date);
        criteria.addIn(StudentAttendance.COL_SCHOOL_OID, getSchoolOids());

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
        query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);
        query.addOrderByAscending(StudentAttendance.COL_DATE);

        m_attendanceMap = getBroker().getMapByQuery(query, StudentAttendance.COL_STUDENT_OID, 512);
    }

    /**
     * Load class attendance keyed to the student OID.
     */
    private void loadStudentPeriodAttendance() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentPeriodAttendance.COL_DATE, m_date);
        criteria.addIn(StudentPeriodAttendance.COL_SCHOOL_OID, getSchoolOids());

        QueryByCriteria query = new QueryByCriteria(StudentPeriodAttendance.class, criteria);
        query.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);

        m_periodAttendancesMap = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID, 512);
    }

    /**
     * Pre-load student oids for which autodial allowed.
     *
     * @param dictionary DataDictionary
     */
    private void loadStudentAutodialAllowed(DataDictionary dictionary) {
        DataDictionaryField fieldAutodial = dictionary.findDataDictionaryFieldByAlias(ALIAS_AUTODIAL);

        Criteria criteria = m_studHelper.getStudentCriteria().copy(true, true, true);

        if (fieldAutodial != null) {
            criteria.addIn(Student.COL_SCHOOL_OID, getSchoolOids());

            Criteria autodialCriteria = new Criteria();
            Criteria autodialIsNullCriteria = new Criteria();
            Criteria autodialEqualsCriteria = new Criteria();

            autodialIsNullCriteria.addIsNull(Student.REL_CONTACTS + PATH_DELIMITER + fieldAutodial.getDatabaseName());
            autodialEqualsCriteria.addEqualTo(Student.REL_CONTACTS + PATH_DELIMITER + fieldAutodial.getDatabaseName(),
                    AUTODIAL_Y_VALUE);
            autodialCriteria.addAndCriteria(autodialIsNullCriteria);
            autodialCriteria.addOrCriteria(autodialEqualsCriteria);
            criteria.addAndCriteria(autodialCriteria);
        } else {
            addNoMatchCriteria(criteria);
        }

        m_studentAutodialAllowed =
                getBroker().getSubQueryCollectionByQuery(new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria));
    }

    /**
     * Check if Period Attendance Code contains not allowed character.
     *
     * @param code String
     * @return String
     */
    private String validateCode(String code) {
        String value = null;

        if (code != null && !code.contains("*")) {
            value = code;
        }

        return value;
    }
}
