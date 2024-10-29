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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * Export procedure for Student Absence File.
 *
 * @author X2 Development Corporation
 */
public class WAStudentAbsence extends StateReportData {
    /**
     * Entity class for Student Absence export.
     *
     */
    public static class WAStudentAbsenceEntity extends StateReportEntity {
        List<StudentAttendance> m_attendance;

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
        public StudentAttendance getAbsence() {
            return m_attendance.get(getCurrentRow());
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

            StudentAttendance att = getAbsence();
            if (att != null && att.getSchool() != null) {
                name += att.getSchool().getName();
            }

            return name;
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
            WAStudentAbsence sdData = (WAStudentAbsence) data;

            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, sdData);

            // Get attendance from enrollment spans rather than all form helper.
            // This has the effect of skipping attendance records that were entered after the
            // withdrawal date by mistake.
            m_attendance = new ArrayList<StudentAttendance>();
            if (districtSpans.size() > 0) {
                List<StudentEnrollmentSpan> spans = sdData.m_helper.getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    List<StudentAttendance> studentAttendance = span.getStudentAttendance();
                    if (enrollment != null) {
                        String withdrawalCode = enrollment.getEnrollmentCode();
                        withdrawalCode = data.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode,
                                withdrawalCode, ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        if (!CODE_NO_SHOW.equals(withdrawalCode)) {
                            addStudentAttendanceWithSchoolCheck(sdData, studentAttendance);
                        }
                    } else {
                        addStudentAttendanceWithSchoolCheck(sdData, studentAttendance);
                    }
                }
            }
            setRowCount(m_attendance.size());
        }

        /**
         * Added checks for each attendance record to verify each one is at a school that is to be
         * included in state reports.
         *
         * @param data WAStudentAbsence
         * @param attendance List<StudentAttendance>
         */
        private void addStudentAttendanceWithSchoolCheck(WAStudentAbsence data, List<StudentAttendance> attendance) {
            for (StudentAttendance studentAttendance : attendance) {
                if (data.includeSchool(studentAttendance.getSchoolOid())) {
                    m_attendance.add(studentAttendance);
                }
            }
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
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. in-district
         * transfer.
         *
         * @param student Student
         * @param sdData WAStudentAbsence
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentAbsence sdData) {
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
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode, withdrawalCode,
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
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(stateCode) || "2".equals(stateCode)) {
                            if (currentSpan != null &&
                                    (currentSpan.m_exitDate == null ||
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
            if (currentSpan != null &&
                    (currentSpan.m_exitDate == null ||
                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
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
            WAStudentAbsenceEntity myEntity = (WAStudentAbsenceEntity) entity;
            StudentAttendance absence = myEntity.getAbsence();
            WAStudentAbsence sdData = (WAStudentAbsence) data;
            X2BaseBean bean = myEntity.getBean();
            SisStudent student = (SisStudent) bean;

            if (param.equals(WAStudentAbsence.PARAM_LOCATION_ID)) {
                StudentEnrollment enrollment = null;
                StudentAttendance attendanceRecord = myEntity.m_attendance.get(entity.getCurrentRow());
                enrollment = sdData.m_helper.getEnrollmentForDate(student.getOid(), attendanceRecord.getDate(), "ESY");

                if (enrollment != null) {
                    value = enrollment.getFieldValueByBeanPath(m_fieldLocationOverride);
                }
                if (value == null || (value instanceof String && StringUtils.isEmpty((String) value))) {
                    value = absence.getSchool() == null ? null : absence.getSchool().getSchoolId();
                }
            } else if (param.equals(WAStudentAbsence.PARAM_ABSENCE_DATE)) {
                value = absence.getDate();
            } else if (param.equals(WAStudentAbsence.PARAM_ABSENCE_CODE)) {
                if (absence.getExcusedIndicator()) {
                    if (absence.getPortionAbsent().floatValue() > 0.5) {
                        value = CODE_EXCUSED_FULL;
                    } else {
                        value = CODE_EXCUSED_HALF;
                    }
                } else {
                    if (absence.getPortionAbsent().floatValue() > 0.5) {
                        value = CODE_UNEXCUSED_FULL;
                    } else {
                        value = CODE_UNEXCUSED_HALF;
                    }
                }
            }
            return value;
        }
    }

    /*
     * Constants: parameters, Ids
     */
    protected static final String CALCULATION_ID = "EXPDATA-WA-ATT";

    protected static final String CODE_EXCUSED_FULL = "EF";
    protected static final String CODE_EXCUSED_HALF = "EP";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String CODE_UNEXCUSED_FULL = "UF";
    protected static final String CODE_UNEXCUSED_HALF = "UP";

    protected static final String PARAM_ABSENCE_CODE = "ABSENCE-CODE";
    protected static final String PARAM_ABSENCE_DATE = "ABSENCE-DATE";
    protected static final String PARAM_LOCATION_ID = "LOCATION-ID";

    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";


    /*
     * Instance variables
     */
    protected Map m_excludeSchool;
    protected StudentHistoryHelper m_helper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected String m_fieldLocationOverride;

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
        m_fieldLocationOverride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        PlainDate currentDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        if (getCurrentContext().getEndDate().before(currentDate)) {
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());
        }

        loadSchoolExcludeMap();

        // Setup OK
        if (getSetupErrors().size() == 0) {
            // create query - use the appropriate class
            setQuery(m_helper.getStudentQuery(false));

            // Set Custom Entity
            setEntityClass(WAStudentAbsenceEntity.class);


            // Build and attach retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALCULATION_ID, new RetrieveReportValue());
            super.addCalcs(calcs);
        }
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchool = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }


    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolOid);
    }
}
