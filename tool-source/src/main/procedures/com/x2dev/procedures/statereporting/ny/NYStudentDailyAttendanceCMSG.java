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
package com.x2dev.procedures.statereporting.ny;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This is the Comsewogue's version of the for Student Daily Attendance export java code.
 *
 * @author X2 Development Corporation
 */

public class NYStudentDailyAttendanceCMSG extends StateReportData {
    /**
     * Entity class for Student Daily Attendance export.
     *
     * @author X2 Development Corporation
     */

    public static class StudentDailyAttendanceEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        protected StudentAttendance attendance;
        protected ArrayList<String> otherCodes = new ArrayList<String>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentDailyAttendanceEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            attendance = (StudentAttendance) bean;

            if (!StringUtils.isEmpty(attendance.getOtherCode())) {
                addCodeIfReportable(attendance.getOtherCode());
            }
            if (!StringUtils.isEmpty(attendance.getOtherCode02())) {
                addCodeIfReportable(attendance.getOtherCode02());
            }
            setRowCount(otherCodes.size());
        }

        /**
         * If the absent indicator is set, export the record, or if the code is .
         *
         * @param code String
         */
        public void addCodeIfReportable(String code) {
            if (code.matches(REGEX_ATTENDANCE_CODES)) {
                otherCodes.add(code);
            }
        }

        /**
         * Returns attendance code.
         *
         * @return Student attendance
         */
        public StudentAttendance getAttendance() {
            return attendance;
        }

        /**
         * Gets the other code.
         *
         * @return String
         */
        public String getOtherCode() {
            return otherCodes.get(getCurrentRow());
        }
    }

    /**
     * Fields
     */
    protected static final String FIELD_ATTENDANCE_CODE_LONG = "Attendance Code Long";

    /**
     * Aliases
     */
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_ATTENDANCE_STATE_CODE = "NY Attendance State Code";
    protected static final String ALIAS_SCHOOL_LOCATION_CODE = "LOCATION CODE";

    /**
     * Constants for reporting information.
     */
    protected static final String REGEX_ATTENDANCE_CODES = "ABS|ALC|COU|CRT|CUT|ETY|OSS|SIC|TDY|UNX|VAC";
    protected static final String ATTENDANCE_CODE_ALC = "ALC";
    protected static final String ATTENDANCE_CODE_EXCUSED = "E";
    protected static final String ATTENDANCE_CODE_EXCUSED_TARDY = "ETY";
    protected static final String ATTENDANCE_CODE_OSS = "OSS";
    protected static final String ATTENDANCE_CODE_TARDY = "TDY";
    protected static final String ATTENDANCE_CODE_TARDY_REPORTABLE = "T";
    protected static final String ATTENDANCE_CODE_UNEXCUSED = "U";

    /**
     * Params
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Local variables for reporting information.
     */
    protected String m_excludeStdField;
    protected String m_attendanceStateCode;
    protected String m_schoolLocationCode;
    protected PlainDate m_reportDate;
    protected String m_activeCode;
    protected StudentHistoryHelper m_helper;
    protected Map<String, String> m_attendanceStateCodes = new HashMap<String, String>();

    /**
     * Retrieves the location code for the attendance record.
     */
    protected class RetrieveLocationCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentDailyAttendanceEntity sdaEntity = (StudentDailyAttendanceEntity) entity;
            SisSchool school = sdaEntity.getAttendance().getSchool();
            if (school != null) {
                value = school.getFieldValueByBeanPath(m_schoolLocationCode);
            }
            return value;
        }

    }

    /**
     * Retrieves the Attendance Code field.
     */
    protected class RetrieveAttendanceCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            StudentDailyAttendanceEntity sdaEntity = (StudentDailyAttendanceEntity) entity;
            String otherCode = sdaEntity.getOtherCode();
            String stateValue = m_attendanceStateCodes.get(otherCode);
            if (stateValue != null) {
                value = stateValue;
            } else {
                value = ATTENDANCE_CODE_UNEXCUSED;
            }
            return value;
        }
    }

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        initializeFields();
        initializeAttendanceCodeMap();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        Criteria studentCriteria = m_helper.getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria attendanceCriteria = getAttendanceCriteria();
        attendanceCriteria.addIn(StudentAttendance.COL_STUDENT_OID, studentSubQuery);

        // … create Query …
        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, attendanceCriteria);
        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(StudentDailyAttendanceEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("STDATTEND-CODE", new RetrieveAttendanceCode());
        calcs.put("STDATTEND-LOCATION", new RetrieveLocationCode());

        HashMap validators = new HashMap<String, FieldRetriever>();

        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Gets the attendance criteria.
     *
     * @return Criteria
     */
    private Criteria getAttendanceCriteria() {
        PlainDate beginDate = getCurrentContext().getStartDate();

        X2Criteria studentAttendanceCriteria = new X2Criteria();
        studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, beginDate);
        studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);

        applyInputCriteria(studentAttendanceCriteria, false, StudentEnrollment.REL_STUDENT);
        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            studentAttendanceCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            studentAttendanceCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT
                    + PATH_DELIMITER + m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }
        return studentAttendanceCriteria;
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_attendanceStateCode = translateAliasToJavaName(ALIAS_ATTENDANCE_STATE_CODE, true);
        m_schoolLocationCode = translateAliasToJavaName(ALIAS_SCHOOL_LOCATION_CODE, true);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
    }

    /**
     * The attendance reference map is somewhat strange and isn't able to be used with lookupValue
     * from StateReportData.java
     * This method pulls all attendance codes and stores them with their state value for translation
     * later.
     */
    private void initializeAttendanceCodeMap() {
        Map<String, RefAttendanceStudent> refTableMap = null;
        Criteria dailyRefCodeCriteria = new Criteria();
        dailyRefCodeCriteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE,
                Integer.valueOf(RefAttendanceStudent.ATTENDANCE_TYPE_DAILY)); // Student Daily
                                                                              // Attendance Code
        dailyRefCodeCriteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE,
                Integer.valueOf(RefAttendanceStudent.TYPE_OTHER_CODE));
        QueryByCriteria dailyRefCodeQuery = new QueryByCriteria(RefAttendanceStudent.class, dailyRefCodeCriteria);

        refTableMap = getBroker().getMapByQuery(dailyRefCodeQuery, RefAttendanceStudent.COL_ATTENDANCE_CODE, 30);
        if (refTableMap != null) {
            for (String code : refTableMap.keySet()) {
                RefAttendanceStudent refValue = refTableMap.get(code);
                m_attendanceStateCodes.put(refValue.getAttendanceCode(),
                        (String) refValue.getFieldValueByBeanPath(m_attendanceStateCode));
            }
        }
    }
}
