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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for SIDManagement export.
 *
 * @author X2 Development Corporation
 */

public class SIDManagement extends StateReportData {
    /**
     * Entity class for SID management export.
     *
     * @author X2 Development Corporation
     */

    public static class SIDManagementEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        private SidInfo m_sidInfo;
        private SIDManagement m_data;
        private BigDecimal m_remoteDaysMembership;
        private BigDecimal m_remoteDaysPresent;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SIDManagementEntity() {
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
            m_data = (SIDManagement) data;
            SisStudent student = (SisStudent) bean;

            // Selecting only the enrollment spans that are after school's start date
            List<StudentEnrollmentSpan> enrollmentSpans =
                    m_data.m_helper.getStudentEnrollmentSpans(student, true);

            // Get the latest enrollment span
            StudentEnrollmentSpan latestSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                if (latestSpan == null || span.getLastActiveDate() == null ||
                        (latestSpan.getLastActiveDate() != null
                                && span.getLastActiveDate().after(latestSpan.getLastActiveDate()))) {
                    latestSpan = span;
                }
            }
            // If the student has a latest enrollment span and the reportable districts are known,
            // continue.
            if (latestSpan != null) {
                Collection<StudentEnrollmentSpan> spanToAdd = JUNE.equals(m_data.m_snapshot) ? enrollmentSpans : null;
                m_sidInfo = m_data.new SidInfo((SisStudent) bean, latestSpan, spanToAdd);
            }

            // if there is no enrollment to consider, skip the student
            if (getSidInfo() == null || getSidInfo().getActiveEnrollment() == null
                    || !m_data.includeSchool(getSidInfo().getActiveEnrollment().getSchoolOid())) {
                setRowCount(0);
            }

            // fetch all student attendance for the month
            X2Criteria attCriteria = new X2Criteria();
            attCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
            attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_data.m_startDate);
            attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_data.m_endDate);
            BeanQuery query = new BeanQuery(StudentAttendance.class, attCriteria);

            // load rest of membership and attendance counts
            Collection<StudentAttendance> attendance = m_data.getBroker().getCollectionByQuery(query);
            int remoteDaysMembership = 0;
            int remoteDaysPresent = 0;
            for (StudentAttendance att : attendance) {
                if (isVirtual(att)) {
                    remoteDaysMembership++;
                }
                if (isVirtualPresent(att)) {
                    remoteDaysPresent++;
                }
            }

            if (m_sidInfo != null
                    && m_sidInfo.getAttendingEnrollment().getFieldValueByBeanPath(m_data.m_fieldEnrollmentType) != null
                    && "S".equals(m_data.lookupStateValue(StudentEnrollment.class, m_data.m_fieldEnrollmentType,
                            (String) m_sidInfo.getAttendingEnrollment()
                                    .getFieldValueByBeanPath(m_data.m_fieldEnrollmentType)))) {
                BigDecimal remoteDaysMembershipDouble = BigDecimal.valueOf(remoteDaysMembership);
                BigDecimal remoteDaysPresentDouble = BigDecimal.valueOf(remoteDaysPresent);
                m_remoteDaysMembership = BigDecimal.valueOf(remoteDaysMembershipDouble.doubleValue() / 2);
                m_remoteDaysPresent = BigDecimal.valueOf(remoteDaysPresentDouble.doubleValue() / 2);
            } else {
                m_remoteDaysMembership = BigDecimal.valueOf(remoteDaysMembership);
                m_remoteDaysPresent = BigDecimal.valueOf(remoteDaysPresent);
            }

        }

        /**
         * Determine if attendance record has a virtual code.
         *
         * @param attendance StudentAttendance
         * @return True is either other code 1 or 2 are flagged as isVirtual
         */
        private boolean isVirtual(StudentAttendance attendance) {
            String other1 = attendance.getOtherCode();
            String other2 = attendance.getOtherCode02();
            String isVirtual = null;

            if (!StringUtils.isEmpty(other1)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.getIsVirtualField());
                }
            }

            if (!StringUtils.isEmpty(other2) && StringUtils.isEmpty(isVirtual)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other2);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.getIsVirtualField());
                }
            }

            return BooleanAsStringConverter.TRUE.equals(isVirtual);
        }

        /**
         * Determine if attendance record has the other code 'VP'.
         *
         * @param attendance StudentAttendance
         * @return True is either other code 1 or 2 are flagged as isVirtual
         */
        private boolean isVirtualPresent(StudentAttendance attendance) {
            String other1 = attendance.getOtherCode();
            String other2 = attendance.getOtherCode02();
            boolean isVirtualPresent = false;

            if (!StringUtils.isEmpty(other1)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtualPresent = "VP".equals(refCode.getAttendanceCode());
                }
            }

            if (!StringUtils.isEmpty(other2) && isVirtualPresent == false) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other2);
                if (refCode != null) {
                    isVirtualPresent = "VP".equals(refCode.getAttendanceCode());
                }
            }

            return isVirtualPresent;
        }

        /**
         * @return the m_remoteDaysMembership
         */
        protected BigDecimal getRemoteDaysMembership() {
            return m_remoteDaysMembership;
        }

        /**
         * @return the m_remoteDaysPresent
         */
        protected BigDecimal getRemoteDaysPresent() {
            return m_remoteDaysPresent;
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
            StudentEnrollmentSpan span = getSidInfo().getEnrollmentSpan();
            if (span != null) {
                name += span.getSchool().getName();
            }

            return name;
        }

        /**
         * Gets the sid info.
         *
         * @return Sid info
         */
        public SidInfo getSidInfo() {
            return m_sidInfo;
        }

        /**
         * Returns the current student.
         *
         * @return SisStudent
         */
        public SisStudent getStudent() {
            return (SisStudent) getBean();
        }
    }

    /**
     * Retrieves the bridge year.
     */
    protected class RetrieveBridgeYear implements FieldRetriever {

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
            Student student = (Student) entity.getBean();
            if (m_gradeLevels.contains(student.getGradeLevel())) {
                String value = (String) student.getFieldValueByBeanPath(m_fieldBridgeYear);
                return lookupStateValue(Student.class, m_fieldBridgeYear, value);
            }
            return "";
        }
    }

    /**
     * Retrieves remote membership days
     */
    protected class RetrieveRemoteDaysMembership implements FieldRetriever {
        protected static final String CALC_ID = "REMOTE_MEMBERSHIP";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SIDManagementEntity sidMgmtEntity = ((SIDManagementEntity) entity);
            SIDManagement sidData = (SIDManagement) data;

            if (sidData.isUpdateStudent()) {
                sidMgmtEntity.getStudent().setFieldValueByBeanPath(sidData.m_fieldRemoteDaysMembership,
                        sidMgmtEntity.getRemoteDaysMembership().toString());
            } else {
                return sidMgmtEntity.getStudent().getFieldValueByBeanPath(sidData.m_fieldRemoteDaysMembership);
            }

            return sidMgmtEntity.getRemoteDaysMembership();
        }
    }

    /**
     * Retrieves remote present days
     */
    protected class RetrieveRemoteDaysPresent implements FieldRetriever {
        protected static final String CALC_ID = "REMOTE_PRESENT";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SIDManagementEntity sidMgmtEntity = ((SIDManagementEntity) entity);
            SIDManagement sidData = (SIDManagement) data;

            if (sidData.isUpdateStudent()) {
                sidMgmtEntity.getStudent().setFieldValueByBeanPath(sidData.m_fieldRemoteDaysPresent,
                        sidMgmtEntity.getRemoteDaysPresent().toString());
            } else {
                return sidMgmtEntity.getStudent().getFieldValueByBeanPath(sidData.m_fieldRemoteDaysPresent);
            }

            return sidMgmtEntity.getRemoteDaysPresent();
        }
    }

    /**
     * Retrieves the Enrollment data fields and calculates enrollment type, district
     * entry date yog, membership days, truant days and present days.
     *
     */
    protected class RetrieveEnrollment implements FieldRetriever {

        protected static final String PARAM_DISTR_PLACE = "DISTR_PLACE";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            Object value = null;
            SIDManagementEntity sidmEntity = ((SIDManagementEntity) entity);
            SidInfo sidInfo = sidmEntity.getSidInfo();
            StudentEnrollmentSpan span = sidmEntity.getSidInfo().getEnrollmentSpan();
            SisStudent student = sidmEntity.getStudent();

            if (PARAM_DAYS_MEMBERSHIP.equals(param)) {
                if (!OCTOBER.equals(m_snapshot)
                        || (OCTOBER.equals(m_snapshot) && !STUDENT_STATUS_ACTIVE.equals(sidInfo.getSidStatus()))) {
                    value = sidInfo.getMembershipDays();
                }
            } else if (PARAM_DAYS_TRUANT.equals(param)) {
                if (!OCTOBER.equals(m_snapshot)
                        || (OCTOBER.equals(m_snapshot) && !STUDENT_STATUS_ACTIVE.equals(sidInfo.getSidStatus()))) {
                    value = sidInfo.getTruantDays();
                }
            } else if (PARAM_DAYS_PRESENT.equals(param)) {
                if (!OCTOBER.equals(m_snapshot)
                        || (OCTOBER.equals(m_snapshot) && !STUDENT_STATUS_ACTIVE.equals(sidInfo.getSidStatus()))) {
                    value = sidInfo.getPresentDays();
                }
            } else if (PARAM_YOG.equals(param)) {
                StudentEnrollment activeEnrollment = sidmEntity.getSidInfo().getActiveEnrollment();
                if (activeEnrollment != null) {
                    value = Integer.toString(activeEnrollment.getYog());
                }
            } else if (PARAM_ENROLL_TYPE.equals(param)) {
                // if there is a status change enrollment report the enrollment type from that
                // enrollment,
                // otherwise use the active enrollment
                StudentEnrollment enrollment = sidInfo.getAttendingEnrollment();
                if (enrollment != null) {
                    String enrollmentType = (String) enrollment.getFieldValueByBeanPath(m_fieldEnrollmentType);
                    if (!StringUtils.isEmpty(enrollmentType)) {
                        enrollmentType = lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldEnrollmentType,
                                enrollmentType, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                    value = enrollmentType;
                }
            } else if (PARAM_DISTRICT_ENTRY_DATE.equals(param)) {
                PlainDate districtEntryDate = null;
                if (BooleanAsStringConverter.TRUE
                        .equals(student.getSchool().getFieldValueByBeanPath(m_fieldSklEntryDate))) {
                    value = data.getPropertyAsJavaType(student, m_fieldStdDistrEntryDate);
                } else {
                    List<StudentEnrollment> enrollments = m_helper.getStudentEnrollments(student);
                    for (StudentEnrollment enrollment : enrollments) {
                        String code = enrollment.getEnrollmentCode();
                        String enrollmentType = enrollment.getEnrollmentType();
                        if (getActiveStatuses().contains(enrollment.getStatusCode()) &&
                                (StudentEnrollment.ENTRY.equals(enrollmentType)
                                        || StudentEnrollment.STATUS_CHANGE.equals(enrollmentType))) {
                            districtEntryDate = enrollment.getEnrollmentDate();
                        }
                        if (StudentEnrollment.ENTRY.equals(enrollmentType) && DISTRICT_ENTRY_CODES.contains(code)) {
                            break;
                        }
                    }
                    value = districtEntryDate;
                }
            } else if (PARAM_DISTR_PLACE.equals(param)) {
                StudentEnrollment otherEnr = m_helper.getEnrollmentForDate(student.getOid(), m_endDate, "ESY");
                if (otherEnr != null) {
                    value = otherEnr.getFieldValueByBeanPath(m_fieldEnrInDistrPlacement);
                }
                if (value == null) {
                    StudentEnrollment entryEnr =
                            m_helper.getEnrollmentForDate(student.getOid(), m_endDate, StudentEnrollment.ENTRY);
                    value = entryEnr.getFieldValueByBeanPath(m_fieldEnrInDistrPlacement);
                }
                if (value != null) {
                    value = data.lookupStateValue(StudentEnrollment.class, m_fieldEnrInDistrPlacement, (String) value);
                }
            }
            if (PARAM_DAYS_MEMBERSHIP.equals(param) || PARAM_DAYS_PRESENT.equals(param)
                    || PARAM_DAYS_TRUANT.equals(param)) {
                // If Status is (Inactive) and SchoolExitWithdrawalCode is D10, indicate a 0
                // for this data element.
                if (!sidInfo.isActive()) {
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        String withDrawalCode = enrollment.getEnrollmentCode();
                        if (WITHDRAWAL_CODE_D10.equalsIgnoreCase(withDrawalCode)) {
                            value = Integer.valueOf(0);
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Retrieves the Attending/Receiving/Residing
     * County/District/School data fields.
     */
    protected class RetrieveCDSCode implements FieldRetriever {

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

            String param = (String) field.getParameter();
            SidInfo sidInfo = ((SIDManagementEntity) entity).getSidInfo();

            StudentEnrollment enrollment = sidInfo.getAttendingEnrollment();
            if (param != null) {
                String[] splitParam = param.split(STRING_COMMA);
                String type = splitParam[0];
                String cdsFlag = splitParam[1];

                String countySchoolName = null;
                if (PARAM_TYPE_ATTEND.equalsIgnoreCase(type)) {
                    countySchoolName = (String) enrollment.getFieldValueByAlias(ALIAS_ATTENDING_SCHOOL);
                } else if (PARAM_TYPE_RECEIVE.equalsIgnoreCase(type)) {
                    countySchoolName = (String) enrollment.getFieldValueByAlias(ALIAS_RECEIVING_SCHOOL);
                } else if (PARAM_TYPE_RESIDE.equalsIgnoreCase(type)) {
                    countySchoolName = (String) enrollment.getFieldValueByAlias(ALIAS_RESIDENT_SCHOOL);
                }
                String countyDistrictSchoolCode = getCountryDistrictStateCodeByCountySchoolName(countySchoolName);

                value = null;
                if (PARAM_COUNTY.equals(cdsFlag)) {
                    String countryCode = null;
                    if (countyDistrictSchoolCode != null) {
                        countryCode = getCountyCodeFromCountyDistrictSchoolCode(countyDistrictSchoolCode);
                    } else if (!sidInfo.isAttendingQualified(enrollment)) {
                        Organization organization = getOrganization();
                        if (organization != null) {
                            countryCode = (String) organization.getFieldValueByAlias(ALIAS_COUNTY_CODE);
                        }
                    }
                    value = countryCode;
                } else if (PARAM_DISTRICT.equals(cdsFlag)) {
                    String districtCode = null;
                    if (countyDistrictSchoolCode != null) {
                        districtCode = getDistrictCodeFromCountyDistrictSchoolCode(countyDistrictSchoolCode);
                    } else if (!sidInfo.isAttendingQualified(enrollment) && enrollment.getSchool() != null) {
                        String districtCodeName =
                                (String) enrollment.getSchool().getFieldValueByAlias(ALIAS_DISTRICT_CODE);
                        districtCode = getCodeFromCodeName(districtCodeName);
                    }
                    value = districtCode;
                } else if (PARAM_SCHOOL.equals(cdsFlag)) {
                    String schoolCode = null;
                    if (countyDistrictSchoolCode != null) {
                        schoolCode = getSchoolCodeFromCountyDistrictSchoolCode(countyDistrictSchoolCode);
                    } else if (!sidInfo.isAttendingQualified(enrollment) && enrollment.getSchool() != null) {
                        String schoolCodeName = (String) enrollment.getSchool().getFieldValueByAlias(ALIAS_SCHOOL_CODE);
                        schoolCode = getCodeFromCodeName(schoolCodeName);
                    }
                    value = schoolCode;
                }
            }

            return value;
        }
    }

    /**
     * Retrives WL Assessment and WL Assessed codes.
     * Return values only for 12 Grade STDs.
     *
     */
    protected class RetrieveLanguageCodes implements FieldRetriever {
        protected static final String ALIAS_STD_BILITRATE = "all-std-Biliterate";
        protected static final String CALC_ID = "LANG";
        protected static final String CALC_PARAM_BILITRATE = "BILITRATE";

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SIDManagementEntity sidMgmtEntity = ((SIDManagementEntity) entity);
            SisStudent student = sidMgmtEntity.getStudent();
            String param = (String) field.getParameter();
            if ("12".equals(entity.getFieldValue(EXPORT_FIELD_GRADE))) {
                if (CALC_PARAM_BILITRATE.equals(param)) {
                    value = student.getFieldValueByAlias(ALIAS_STD_BILITRATE) != null
                            && BooleanAsStringConverter.TRUE.equals(student.getFieldValueByAlias(ALIAS_STD_BILITRATE))
                                    ? "Y"
                                    : "N";
                } else if (student.getFieldValueByAlias(param) != null) {
                    value = data.lookupReferenceCodeByAlias(param, (String) student.getFieldValueByAlias(param),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return value;
        }
    }

    /**
     * Retrieves the Enrollment data fields and returns the entry date, exit code
     * and exit date.
     *
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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            Object value = null;
            SIDManagementEntity sidmEntity = (SIDManagementEntity) entity;
            SidInfo sidInfo = sidmEntity.getSidInfo();
            StudentEnrollmentSpan span = sidmEntity.getSidInfo().getEnrollmentSpan();
            StudentEnrollment inactiveEnrollment = span.getFirstInactiveEnrollment();
            SisStudent student = sidmEntity.getStudent();

            if (PARAM_ENTRY_DATE.equals(param)) {
                if (BooleanAsStringConverter.TRUE
                        .equals(student.getSchool().getFieldValueByBeanPath(m_fieldSklEntryDate))) {
                    value = data.getPropertyAsJavaType(student, m_fieldStdSklEntryDate);
                } else {
                    value = sidInfo.getSchoolEnrollmentDate();
                }
            } else if (PARAM_EXIT_CODE.equals(param)) {
                value = sidInfo.getWithdrawalCode();
            } else if (PARAM_EXIT_DATE.equals(param) && inactiveEnrollment != null) {
                value = inactiveEnrollment.getEnrollmentDate();
            }
            return value;
        }
    }

    /**
     * Retrieves the student related information like municipal code, tuition code,
     * sped classification and lep start/end date.
     *
     */
    protected class RetrieveStudent implements FieldRetriever {

        private final SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd");

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
        // DS-STDINFO
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            Object value = null;
            SIDManagementEntity sidMgmtEntity = ((SIDManagementEntity) entity);
            SisStudent student = sidMgmtEntity.getStudent();
            SidInfo sidInfo = sidMgmtEntity.getSidInfo();
            if (PARAM_TUITION_CODE.equalsIgnoreCase(param)) {
                StudentEnrollment enrollment = sidInfo.getAttendingEnrollment();

                if (enrollment != null) {
                    String tuitionCode = (String) enrollment.getFieldValueByBeanPath(m_tuitionCode);
                    value = lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_tuitionCode, tuitionCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (PARAM_LEP_START_DATE.equalsIgnoreCase(param)) {
                StudentProgramParticipation program = sidInfo.getLepProgram();
                if (program != null) {
                    value = dateFormatter.format(program.getStartDate());
                }
            } else if (PARAM_LEP_END_DATE.equalsIgnoreCase(param)) {
                StudentProgramParticipation program = sidInfo.getLepProgram();
                if (program != null) {
                    boolean isRefused = BooleanAsStringConverter.TRUE.equals(
                            program.getFieldValueByAlias(ALIAS_PARENT_REFUSED_SERV));
                    if (isRefused) {
                        value = REFUSED;
                    } else if (program.getEndDate() != null) {
                        value = dateFormatter.format(program.getEndDate());
                    }
                }
            } else if (PARAM_MUNICIPAL_CODE.equalsIgnoreCase(param)) {
                StudentEnrollment enrollment = sidInfo.getAttendingEnrollment();

                // Get municipal code from the student enrollment record.
                if (enrollment != null) {
                    String municipalCode = (String) enrollment.getFieldValueByBeanPath(m_municipalCode);
                    if (null == municipalCode) {
                        municipalCode = (String) student.getFieldValueByBeanPath(m_resMunicipalCode);
                    }
                    value = lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_municipalCode, municipalCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (PARAM_MILITARY_CODE.equalsIgnoreCase(param)) {
                String militaryCode = (String) student.getFieldValueByBeanPath(m_fieldMilitaryConnected);
                if (!StringUtils.isEmpty(militaryCode) && m_militaryCodesMap.containsKey(militaryCode)) {
                    value = m_militaryCodesMap.get(militaryCode).getStateCode();
                }
            } else if (PARAM_ELA_GRAD_IND.equalsIgnoreCase(param)) {
                String gradeLevel = entity.getFieldValue(EXPORT_FIELD_GRADE);
                if ("11".equals(gradeLevel) || "12".equals(gradeLevel)) {
                    String code = (String) student.getFieldValueByBeanPath(m_fieldElaGradInd);
                    if (!StringUtils.isEmpty(code)) {
                        value = data.lookupStateValue(SisStudent.class, m_fieldElaGradInd, code);
                    }
                }
            } else if (PARAM_MATH_GRAD_IND.equalsIgnoreCase(param)) {
                String gradeLevel = entity.getFieldValue(EXPORT_FIELD_GRADE);
                if ("11".equals(gradeLevel) || "12".equals(gradeLevel)) {
                    String code = (String) student.getFieldValueByBeanPath(m_fieldMathGradInd);
                    if (!StringUtils.isEmpty(code)) {
                        value = data.lookupStateValue(SisStudent.class, m_fieldMathGradInd, code);
                    }
                }
            } else if (PARAM_LIEP.equalsIgnoreCase(param)) {
                StudentProgramParticipation program = sidInfo.getLepProgram();
                if (program != null) {
                    String code = (String) program.getFieldValueByBeanPath(m_fieldPgmLIEP);
                    if (!StringUtils.isEmpty(code)) {
                        value = data.lookupStateValue(StudentProgramParticipation.class, m_fieldPgmLIEP, code);
                    }
                }
            } else if (PARAM_LIEP_LANG.equalsIgnoreCase(param)) {
                StudentProgramParticipation program = sidInfo.getLepProgram();
                if (program != null && (program.getEndDate() == null ||
                        (!program.getEndDate().before(data.getCurrentContext().getStartDate())
                                && !program.getEndDate().after(data.getCurrentContext().getEndDate())))) {
                    String code = (String) program.getFieldValueByBeanPath(m_fieldPgmLIEPLang);
                    if (!StringUtils.isEmpty(code)) {
                        value = data.lookupStateValue(StudentProgramParticipation.class, m_fieldPgmLIEPLang, code);
                    }
                }
            } else if (PARAM_GIFTED_TALENTED.equalsIgnoreCase(param)) {
                value = "N";

                String gifted = (String) student.getFieldValueByBeanPath(m_fieldGiftedAndTalented);
                if (!StringUtils.isEmpty(gifted) && StringUtils.isEqual(gifted, BooleanAsStringConverter.TRUE)) {
                    value = "Y";
                }
            }
            return value;
        }
    }

    /**
     * Retrieves the Student status from the SIDManagementEntity.
     */
    protected class RetrieveStudentStatus implements FieldRetriever {

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
            SidInfo sidInfo = ((SIDManagementEntity) entity).getSidInfo();
            return sidInfo.getSidStatus();
        }
    }

    /**
     * validate "First Ent Date US Sc" field
     * rule:
     * If Immigrant Status = Y and DOE FIRST ENT DATE US SC is blank/null - appear validation error.
     *
     * @author Follett Software Company
     */
    public class ValidateFirstEDUS implements FieldValidator {
        private static final String ERROR_MESSAGE =
                "First Entry Date into US school required for country of birth is not US or Puerto Rico.";
        private static final String REQUIRED_VALUE = "Required value";
        private static final String VAL_ID = "validateFirstEDUS";

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
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();
            if (!StringUtils.isEmpty(value) && !Arrays.asList("2330", "1790").contains(value)
                    && student.getFieldValueByBeanPath(m_fieldStdFirstEDUS) == null) {
                errors.add(new StateReportValidationError(entity, field, REQUIRED_VALUE, ERROR_MESSAGE));
            }
            return errors;
        }
    }

    /**
     * The Class SidInfo.
     */
    public class SidInfo {
        private StudentEnrollment m_activeEnrollment;
        private StudentEnrollment m_attendingEnrollment;
        private PlainDate m_dateSchoolEnrollment;
        private boolean m_isActive;
        private BigDecimal m_membershipDays;
        private BigDecimal m_presentDays;
        private String m_sidStatus;
        private StudentEnrollmentSpan m_span;
        private Collection<StudentEnrollmentSpan> m_spans;
        private SisStudent m_student;
        private BigDecimal m_truantDays;
        private String m_withdrawalCode;

        /**
         * Instantiates a new sid info.
         *
         * @param student SisStudent
         * @param span StudentEnrollmentSpan
         * @param spans
         */
        public SidInfo(SisStudent student, StudentEnrollmentSpan span, Collection<StudentEnrollmentSpan> spans) {
            m_student = student;
            m_span = span;
            m_activeEnrollment = getLastActiveEnrollment();
            if (spans != null && !spans.isEmpty()) {
                m_spans = spans;
            }
            StudentEnrollment lastEnrollment = m_helper.getEnrollmentForDate(student.getOid(), m_endDate, "EW");
            if (lastEnrollment != null
                    && StudentEnrollment.ENTRY.equalsIgnoreCase(lastEnrollment.getEnrollmentType())) {
                m_isActive = true;
            } else {
                m_isActive = false;
            }

        }

        /**
         * Gets the active enrollment.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getActiveEnrollment() {
            return m_activeEnrollment;
        }

        /**
         * Gets the attending enrollment.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getAttendingEnrollment() {
            if (m_attendingEnrollment == null) {

                List<StudentEnrollment> enrollments = m_helper.getStudentEnrollments(m_student);
                if (enrollments != null) {
                    for (StudentEnrollment enrollment : enrollments) {
                        if (enrollment.getEnrollmentDate() != null
                                && !enrollment.getEnrollmentDate().after(m_endDate)
                                && "ESY".contains(enrollment.getEnrollmentType())) {
                            if (isAttendingQualified(enrollment)) {
                                m_attendingEnrollment = enrollment;
                                break;
                            }
                            if ("E".equals(enrollment.getEnrollmentType())) {
                                m_attendingEnrollment = enrollment;
                                break;
                            }
                        }
                    }
                }
                if (m_attendingEnrollment == null) {
                    m_attendingEnrollment = m_activeEnrollment;
                }
            }
            return m_attendingEnrollment;
        }

        /**
         * Gets the enrollment span.
         *
         * @return Student enrollment span
         */
        public StudentEnrollmentSpan getEnrollmentSpan() {
            return m_span;
        }

        /**
         * Gets the lep program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getLepProgram() {
            return SIDManagement.this.getLepProgram(m_student);
        }

        /**
         * Gets the membership days.
         *
         * @return Big decimal
         */
        public BigDecimal getMembershipDays() {
            if (m_membershipDays == null) {
                m_membershipDays = new BigDecimal(0);
                Collection<StudentEnrollmentSpan> spansToOperate = new ArrayList<>();
                if (m_spans != null && !m_spans.isEmpty()) {
                    spansToOperate.addAll(m_spans);
                } else {
                    spansToOperate.add(m_span);
                }
                for (StudentEnrollmentSpan span : spansToOperate) {
                    List<StudentAttendance> attendances = m_helper.getStudentAttendances(m_student.getOid());
                    BigDecimal membershipDays = new BigDecimal(getSpanMembershipDays(span));
                    BigDecimal count = BigDecimal.ZERO;
                    BigDecimal threshold = new BigDecimal(3);
                    if (attendances != null) {
                        for (StudentAttendance attendance : attendances) {
                            if (!attendance.getDate().before(span.getFirstActiveDate()) &&
                                    (span.getLastActiveDate() == null
                                            || !attendance.getDate().after(span.getLastActiveDate()))) {
                                // Hunterdon doesn't follow the general rule for calculating the
                                // membership days. Hunterdon checks the reaon for absent.
                                // If it is one of the below reasons, then that day is not counted
                                // towards actual membership days.
                                // State Code Description
                                // 3 Excused Absence - Religious Holiday or Bring Your Child to Work
                                // Day
                                // 7 Home Instruction
                                String reason = attendance.getReasonCode();
                                reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                        StudentAttendance.COL_REASON_CODE, reason,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if (!StringUtils.isEmpty(reason)
                                        && (reason.contains(ABSENT_REASON_CLGNONMEM)
                                                || reason.contains(ABSENT_REASON_NONMEM))) {
                                    membershipDays = membershipDays.subtract(attendance.getPortionAbsent());

                                    if (reason.contains(ABSENT_REASON_CLGNONMEM)) {
                                        count = count
                                                .add(attendance.getPortionAbsent() != null
                                                        ? attendance.getPortionAbsent()
                                                        : BigDecimal.ONE);
                                    }
                                    if (count.compareTo(threshold) > 0) {
                                        membershipDays = membershipDays.add(BigDecimal.ONE);
                                        count = BigDecimal.ZERO;
                                    }
                                }
                            }
                        }
                    }
                    if (getAttendingEnrollment().getFieldValueByBeanPath(m_fieldEnrollmentType) != null
                            && "S".equals(lookupStateValue(StudentEnrollment.class, m_fieldEnrollmentType,
                                    (String) getAttendingEnrollment()
                                            .getFieldValueByBeanPath(m_fieldEnrollmentType)))) {
                        double daysDouble = membershipDays.doubleValue() / 2;
                        String daysString = String.valueOf(daysDouble);
                        if (daysString.contains(".")) {
                            daysString = daysString.substring(daysString.indexOf('.') + 1);
                            if (!StringUtils.isEmpty(daysString) && daysString.length() > 0) {
                                String firstDecimal = daysString.substring(0, 1);
                                if (!StringUtils.isEmpty(firstDecimal)) {
                                    int firstDecimalInt = Integer.valueOf(firstDecimal).intValue();
                                    if (firstDecimalInt >= 0 && firstDecimalInt <= 4) {
                                        membershipDays = membershipDays.divide(new BigDecimal(2), 0, RoundingMode.DOWN);
                                    } else if (firstDecimalInt > 5 && firstDecimalInt <= 9) {
                                        membershipDays = membershipDays.divide(new BigDecimal(2), 0, RoundingMode.UP);
                                    } else if (firstDecimalInt == 5) {
                                        membershipDays = BigDecimal.valueOf(daysDouble);
                                    }
                                }
                            }
                        }
                    }
                    m_membershipDays = m_membershipDays.add(membershipDays);
                }
            }
            return m_membershipDays;
        }

        /**
         * Gets the present days.
         *
         * @return Big decimal
         */
        public BigDecimal getPresentDays() {
            if (m_presentDays == null) {
                m_presentDays = new BigDecimal(0);
                Collection<StudentEnrollmentSpan> spansToOperate = new ArrayList<>();
                if (m_spans != null && !m_spans.isEmpty()) {
                    spansToOperate.addAll(m_spans);
                } else {
                    spansToOperate.add(m_span);
                }
                for (StudentEnrollmentSpan span : spansToOperate) {
                    List<StudentAttendance> attendances = m_helper.getStudentAttendances(m_student.getOid());
                    BigDecimal membershipDays = new BigDecimal(getSpanMembershipDays(span));
                    BigDecimal count = BigDecimal.ZERO;
                    BigDecimal threshold = new BigDecimal(3);
                    if (attendances != null) {
                        for (StudentAttendance attendance : attendances) {
                            if (!attendance.getDate().before(span.getFirstActiveDate()) &&
                                    (span.getLastActiveDate() == null
                                            || !attendance.getDate().after(span.getLastActiveDate()))) {
                                // Hunterdon doesn't follow the general rule for calculating the
                                // membership days. Hunterdon checks the reaon for absent.
                                // If it is one of the below reasons, then that day is not counted
                                // towards actual membership days.
                                // State Code Description
                                // 3 Excused Absence - Religious Holiday or Bring Your Child to Work
                                // Day
                                // 7 Home Instruction
                                String reason = attendance.getReasonCode();
                                reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                        StudentAttendance.COL_REASON_CODE, reason,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                        StudentAttendance.COL_REASON_CODE, reason,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if (!StringUtils.isEmpty(reason)
                                        && (reason.contains(ABSENT_REASON_CLGNONMEM)
                                                || reason.contains(ABSENT_REASON_NONMEM))) {
                                    membershipDays = membershipDays.subtract(attendance.getPortionAbsent());
                                    if (reason.contains(ABSENT_REASON_CLGNONMEM)) {
                                        count = count
                                                .add(attendance.getPortionAbsent() != null
                                                        ? attendance.getPortionAbsent()
                                                        : BigDecimal.ONE);
                                    }
                                    if (count.compareTo(threshold) > 0) {
                                        membershipDays = membershipDays.add(BigDecimal.ONE);
                                        count = BigDecimal.ZERO;
                                    }
                                }
                                if (attendance.getAbsentIndicator()) {
                                    membershipDays = membershipDays.subtract(attendance.getPortionAbsent());
                                }
                            }
                        }
                    }
                    if (getAttendingEnrollment().getFieldValueByBeanPath(m_fieldEnrollmentType) != null
                            && "S".equals(lookupStateValue(StudentEnrollment.class, m_fieldEnrollmentType,
                                    (String) getAttendingEnrollment()
                                            .getFieldValueByBeanPath(m_fieldEnrollmentType)))) {
                        double daysDouble = membershipDays.doubleValue() / 2;
                        String daysString = String.valueOf(daysDouble);
                        if (daysString.contains(".")) {
                            daysString = daysString.substring(daysString.indexOf('.') + 1);
                            if (!StringUtils.isEmpty(daysString) && daysString.length() > 0) {
                                String firstDecimal = daysString.substring(0, 1);
                                if (!StringUtils.isEmpty(firstDecimal)) {
                                    int firstDecimalInt = Integer.valueOf(firstDecimal).intValue();
                                    if (firstDecimalInt >= 0 && firstDecimalInt <= 4) {
                                        membershipDays = membershipDays.divide(new BigDecimal(2), 0, RoundingMode.DOWN);
                                    } else if (firstDecimalInt > 5 && firstDecimalInt <= 9) {
                                        membershipDays = membershipDays.divide(new BigDecimal(2), 0, RoundingMode.UP);
                                    } else if (firstDecimalInt == 5) {
                                        membershipDays = BigDecimal.valueOf(daysDouble);
                                    }
                                }
                            }
                        }
                    }
                    m_presentDays = m_presentDays.add(membershipDays);
                }
            }
            return m_presentDays;
        }

        /**
         * Gets the school enrollment date.
         *
         * @return Plain date
         */
        public PlainDate getSchoolEnrollmentDate() {
            if (m_dateSchoolEnrollment == null) {
                // Set to current active date
                m_dateSchoolEnrollment = m_activeEnrollment.getEnrollmentDate();

                // Find matching enrollment span
                List<StudentEnrollmentSpan> spansToCalculateEntry =
                        m_helper.getStudentEnrollmentSpans(m_student, false);
                int i = -1;
                for (StudentEnrollmentSpan span : spansToCalculateEntry) {
                    if (span.getFirstActiveEnrollment() != null && m_span.getFirstActiveEnrollment() != null
                            && span.getFirstActiveEnrollment().getEnrollmentDate()
                                    .equals(m_span.getFirstActiveEnrollment().getEnrollmentDate())) {
                        i = spansToCalculateEntry.indexOf(span) - 1;
                        break;
                    }
                }

                // Search prior spans fo rschool entry date
                while (i >= 0) {
                    StudentEnrollmentSpan span = spansToCalculateEntry.get(i);
                    if (m_span.getSchool() != null
                            && span.getSchool() != null
                            && m_span.getSchool().getOid().equals(span.getSchool().getOid())
                            && span.getFirstInactiveEnrollment() != null
                            && "T1".equals(span.getFirstInactiveEnrollment().getEnrollmentCode())) {
                        if (span.getFirstActiveEnrollment() != null
                                && span.getFirstActiveEnrollment().getEnrollmentDate() != null) {
                            m_dateSchoolEnrollment = span.getFirstActiveEnrollment().getEnrollmentDate();
                        }
                        i--;
                    } else {
                        break;
                    }
                }
            }
            return m_dateSchoolEnrollment;
        }

        /**
         * Gets the sid status.
         *
         * @return String
         */
        public String getSidStatus() {
            if (m_sidStatus == null) {
                PlainDate exitDate = null;
                StudentEnrollment inactiveEnrollment = m_span.getFirstInactiveEnrollment();
                if (inactiveEnrollment != null) {
                    exitDate = inactiveEnrollment.getEnrollmentDate();
                }

                // If we are looking at a previous enrollment, where there was a withdrawal,
                // indicated by an exit date, then even if the student is currently active, this
                // should
                // not
                // be reported, since the status for this record would be inactive.
                if (isActive() && exitDate == null) {
                    m_sidStatus = STUDENT_STATUS_ACTIVE;
                } else {
                    m_sidStatus = STUDENT_STATUS_INACTIVE;
                }
            }
            return m_sidStatus;
        }

        /**
         * Gets the truant days.
         *
         * @return Integer
         */
        public BigDecimal getTruantDays() {
            if (m_truantDays == null) {
                m_truantDays = new BigDecimal(0);
                Collection<StudentEnrollmentSpan> spansToOperate = new ArrayList<>();
                if (m_spans != null && !m_spans.isEmpty()) {
                    spansToOperate.addAll(m_spans);
                } else {
                    spansToOperate.add(m_span);
                }
                for (StudentEnrollmentSpan span : spansToOperate) {
                    int truantDaysToAdd = 0;
                    List<StudentAttendance> attendances = m_helper.getStudentAttendances(m_student.getOid());
                    if (attendances != null) {
                        for (StudentAttendance attendance : attendances) {
                            if (!attendance.getDate().before(span.getFirstActiveDate()) &&
                                    (span.getLastActiveDate() == null
                                            || !attendance.getDate().after(span.getLastActiveDate()))) {
                                String reason = attendance.getReasonCode();
                                reason = lookupReferenceCodeByBeanPath(StudentAttendance.class,
                                        StudentAttendance.COL_REASON_CODE, reason,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if (attendance.getAbsentIndicator()
                                        && ABSENT_REASON_TRUANT_STATE_CODE.equalsIgnoreCase(reason)) {
                                    truantDaysToAdd++;
                                }

                            }
                        }
                    }
                    BigDecimal daysDouble = null;
                    if (getAttendingEnrollment().getFieldValueByBeanPath(m_fieldEnrollmentType) != null
                            && "S".equals(lookupStateValue(StudentEnrollment.class, m_fieldEnrollmentType,
                                    (String) getAttendingEnrollment()
                                            .getFieldValueByBeanPath(m_fieldEnrollmentType)))) {
                        daysDouble = new BigDecimal(truantDaysToAdd / 2d);
                        String daysString = String.valueOf(daysDouble);
                        if (daysString.contains(".")) {
                            daysString = daysString.substring(daysString.indexOf('.') + 1);
                            if (!StringUtils.isEmpty(daysString) && daysString.length() > 0) {
                                String firstDecimal = daysString.substring(0, 1);
                                if (!StringUtils.isEmpty(firstDecimal)) {
                                    int firstDecimalInt = Integer.valueOf(firstDecimal).intValue();
                                    if (firstDecimalInt >= 0 && firstDecimalInt <= 4) {
                                        daysDouble = daysDouble.divide(new BigDecimal(2), 0, RoundingMode.DOWN);
                                    } else if (firstDecimalInt > 5 && firstDecimalInt <= 9) {
                                        daysDouble = daysDouble.divide(new BigDecimal(2), 0, RoundingMode.UP);
                                    }
                                }
                            }
                        }
                    }
                    m_truantDays = m_truantDays.add(daysDouble == null ? BigDecimal.valueOf(truantDaysToAdd)
                            : BigDecimal.valueOf(daysDouble.doubleValue()));
                }
            }
            return m_truantDays;
        }

        /**
         * Gets the withdrawal code.
         *
         * @return String
         */
        public String getWithdrawalCode() {
            if (m_withdrawalCode == null) {
                StudentEnrollment inactiveEnrollment = m_span.getFirstInactiveEnrollment();
                if (inactiveEnrollment != null) {
                    String code = inactiveEnrollment.getEnrollmentCode();
                    ReferenceCode associatedRef = null;
                    if (!StringUtils.isEmpty(code) && (associatedRef = m_withdrawalCodes.get(code)) != null) {
                        m_withdrawalCode = associatedRef.getStateCode();
                    }
                }
                if (m_withdrawalCode == null) {
                    m_withdrawalCode = "";
                }
            }
            return m_withdrawalCode;
        }

        /**
         * Checks if is active.
         *
         * @return true, if is active
         */
        public boolean isActive() {
            return m_isActive;
        }

        /**
         * Checks if is attending qualified.
         *
         * @param enrollment StudentEnrollment
         * @return true, if is attending qualified
         */
        public boolean isAttendingQualified(StudentEnrollment enrollment) {
            return !StringUtils.isEmpty((String) enrollment.getFieldValueByBeanPath(m_fieldAttendingSchool))
                    || !StringUtils.isEmpty((String) enrollment.getFieldValueByBeanPath(m_fieldReceivingSchool))
                    || !StringUtils.isEmpty((String) enrollment.getFieldValueByBeanPath(m_fieldResidingSchool))
                    || !StringUtils.isEmpty((String) enrollment.getFieldValueByBeanPath(m_tuitionCode))
                    || !StringUtils.isEmpty((String) enrollment.getFieldValueByBeanPath(m_municipalCode))
                    || !StringUtils.isEmpty((String) enrollment.getFieldValueByBeanPath(m_fieldEnrollmentType));
        }

        /**
         * Gets the last enrollment of type.
         *
         * @param enrollmentType String
         * @return Student enrollment
         */
        private StudentEnrollment getLastActiveEnrollment() {
            StudentEnrollment lastEnrollment = null;

            Collection<StudentEnrollment> studentEnrollments = m_span.getEnrollments();
            for (StudentEnrollment enrollment : studentEnrollments) {
                if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                    if (lastEnrollment == null ||
                            lastEnrollment.getEnrollmentDate() == null ||
                            (enrollment.getEnrollmentDate() != null
                                    && enrollment.getEnrollmentDate().after(lastEnrollment.getEnrollmentDate()))) {
                        lastEnrollment = enrollment;
                    }
                }
            }

            if (lastEnrollment != null
                    && !StudentManager.isActiveStudent(getOrganization(), lastEnrollment.getStatusCode())) {
                boolean isNext = false;
                for (StudentEnrollment enrollment : studentEnrollments) {
                    if (enrollment == lastEnrollment) {
                        isNext = true;
                    } else if (isNext) {
                        if (StudentEnrollment.STATUS_CHANGE.equals(enrollment.getEnrollmentType())) {
                            if (getActiveStatuses().contains(enrollment.getStatusCode())) {
                                lastEnrollment = enrollment;
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                }
            }

            return lastEnrollment;
        }

        /**
         * Gets the span membership days.
         *
         * @param span StudentEnrollmentSpan
         * @return int
         */
        private int getSpanMembershipDays(StudentEnrollmentSpan span) {
            Set<PlainDate> insessionDates = m_helper.getCalendarDays(span.getSchool(), m_student.getCalendarCode());
            if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(m_student.getCalendarCode())) {
                insessionDates = m_helper.getCalendarDays(span.getSchool(), DEFAULT_CALENDAR_ID);
            }
            if (insessionDates == null) {
                insessionDates = m_helper.getCalendarDays(span.getSchool(), StudentHistoryHelper.CALENDAR_ANY);
            }

            // Count in session days between (and including) first and last active dates.
            int count = 0;

            if (m_preferenceMemberOnWithdrawal || span.getFirstInactiveEnrollment() == null ||
            // Case when last active date is first in session date - not in membership
                    (span.getLastActiveDate() != null && span.getFirstActiveEnrollment() != null
                            && span.getFirstActiveEnrollment().getEnrollmentDate() != null
                            && span.getLastActiveDate()
                                    .before(span.getFirstInactiveEnrollment().getEnrollmentDate()))) {
                if (insessionDates != null) {
                    PlainDate endDate = span.getLastActiveDate();
                    if (endDate == null) {
                        endDate = m_endDate;
                    }
                    PlainDate m_firstActiveDate = span.getFirstActiveDate();
                    for (PlainDate date : insessionDates) {
                        if (m_firstActiveDate != null && !date.before(m_firstActiveDate) && !date.after(endDate)) {
                            count++;
                        }
                    }
                }
            }
            return count;
        }
    }

    /**
     * Reference Codes
     */
    protected static final String REF_TABLE_COUNTRY_DISTRICT_SCHOOL_CODES = "County District School Codes";
    protected static final String REF_TABLE_DOE_DISTRICT_CODES = "DOE District Codes";
    protected static final String REF_TABLE_DOE_TUITION_CODES = "DOE Tuition Code";
    protected static final String SPED_STATUS_REFERRED = "Referred";
    protected static final String SPED_STATUS_REFERRED_NOT_IDENT = "Referred Not Ident";
    protected static final String SPED_STATUS_REFERRED_WAIVED = "Referred Waived Serv";
    protected static final String SPED_STATUS_ACTIVE = "Active";

    /**
     * Aliases
     */
    protected static final String ALIAS_ATTENDING_SCHOOL = "DOE ATTENDING SCHOOL";
    protected static final String ALIAS_BRIDGE_YEAR = "all-std-BridgeYear";
    protected static final String ALIAS_COUNTY_CODE = "DOE COUNTY CODE";
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT CODE";
    protected static final String ALIAS_ELA_GRAD_IND = "DOE ELA GRAD PATHWAY IND";
    protected static final String ALIAS_ENR_IN_DISTR_PLACE = "DOE IN DISTRICT PLACEMENT";
    protected static final String ALIAS_ENROLLMENT_CLASSIFICATION = "DOE ENROLL CLASSIFIER";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_FIRST_ENT_US = "DOE FIRST ENT DATE US SC";
    protected static final String ALIAS_GIFTED_AND_TALENTED = "all-std-GiftedandTalented";
    protected static final String ALIAS_HOME_SCHOOL = "DOE HOME SCHOOL";
    protected static final String ALIAS_MATH_GRAD_IND = "DOE MATH GRAD PATHWAY IND";
    protected static final String ALIAS_MILITARY_CONNECTED = "DOE MILITARY CONNECTED STUDENT";
    protected static final String ALIAS_MUNICIPAL_CODE = "DOE MUNICIPAL CODE";
    protected static final String ALIAS_NON_PUBLIC = "DOE NON PUBLIC";
    protected static final String ALIAS_PARENT_REFUSED_SERV = "DOE PARENT REFUSED SERVICES";
    protected static final String ALIAS_RECEIVING_SCHOOL = "DOE RECEIVING SCHOOL";
    protected static final String ALIAS_RESIDENT_SCHOOL = "DOE RESIDING SCHOOL";
    protected static final String ALIAS_RES_MUNICIPAL_CODE = "DOE RES MUNICIPAL CODE";
    protected static final String ALIAS_SAME_DISTRICT_CODES_LIST = "RELATED RESIDENT DIST CODES";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL CODE";
    protected static final String ALIAS_SKL_ENTRY_DATE = "DOE PULL DISTRICT/SCHOOL ENTRY DATE";
    protected static final String ALIAS_STD_DISTR_ENTRY_DATE = "DOE DISTRICT ENTRY DATE";
    protected static final String ALIAS_PGM_LIEP = "all-pgm-LanguageInstructionEdProgram";
    protected static final String ALIAS_PGM_LIEP_LANG = "all-pgm-LanguageofInstruction";
    protected static final String ALIAS_STD_SKL_ENTRY_DATE = "DOE SCHOOL ENTRY DATE";
    protected static final String ALIAS_TUITION_CODE = "DOE TUITION CODE";
    protected static final String ALIAS_YOG_ORIGINAL = "DOE YOG ORIGINAL";
    protected static final String ALIAS_REMOTE_DAYS_MEMBERSHIP = "all-std-RemoteDaysMembship";
    protected static final String ALIAS_REMOTE_DAYS_PRESENT = "all-std-RemoteDaysPresent";
    protected static final String ALIAS_RAT_VIRTUAL_FLAG = "all-rat-IsVirtual";

    /**
     * Export Fields
     */
    protected static final String EXPORT_FIELD_GRADE = "Grade Level";

    /**
     * Parameter
     */
    protected static final String PARAM_COUNTY = "COUNTY";
    protected static final String PARAM_DAYS_MEMBERSHIP = "MEMBERSHIP";
    protected static final String PARAM_DAYS_PRESENT = "PRESENT";
    protected static final String PARAM_DAYS_TRUANT = "TRUANT";
    protected static final String PARAM_DISTRICT = "DISTRICT";
    protected static final String PARAM_DISTRICT_ENTRY_DATE = "DISTENTRYDATE";
    protected static final String PARAM_ELA_GRAD_IND = "ELA GRAD IND";
    protected static final String PARAM_ENROLL_TYPE = "TYPE";
    protected static final String PARAM_ENTRY_DATE = "ENTRYDATE";
    protected static final String PARAM_EXIT_CODE = "WDCODE";
    protected static final String PARAM_EXIT_DATE = "EXITDATE";
    protected static final String PARAM_GIFTED_TALENTED = "GIFTEDTALENTED";
    protected static final String PARAM_LEP_END_DATE = "LEP-END";
    protected static final String PARAM_LEP_START_DATE = "LEP-START";
    protected static final String PARAM_LIEP = "LIEP";
    protected static final String PARAM_LIEP_LANG = "LIEP_LANG";
    protected static final String PARAM_MATH_GRAD_IND = "MATH GRAD IND";
    protected static final String PARAM_MUNICIPAL_CODE = "MUNICIPAL";
    protected static final String PARAM_MILITARY_CODE = "MILITARY";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SAVE_RESULTS = "saveResults";
    protected static final String PARAM_SCHOOL = "SCHOOL";
    protected static final String PARAM_SNAPSHOT = "snapshot";
    protected static final String PARAM_TUITION_CODE = "TUITION";
    protected static final String PARAM_TYPE_ATTEND = "ATTEND";
    protected static final String PARAM_TYPE_RECEIVE = "RECEIVE";
    protected static final String PARAM_TYPE_RESIDE = "RESIDE";
    protected static final String PARAM_YOG = "YOG";

    protected static final String INPUT_PARAM_START_DATE = "startDate";
    protected static final String INPUT_PARAM_END_DATE = "endDate";
    protected static final String INPUT_PARAM_UPDATE_STUDENT = "updateStudent";

    /**
     * Other Constants
     */
    protected static final String ABSENT_REASON_CLGNONMEM = "CLGNONMEM";
    protected static final String ABSENT_REASON_NONMEM = "NONMEM";
    protected static final String ABSENT_REASON_TRUANT_STATE_CODE = "TT";

    protected static final String NON_PUBLIC_ELIGIBLE_AND_RECEIVING = "Yes, std is eligible and is receiving services";
    protected static final String NON_PUBLIC_ELIGIBLE_AND_NOT_RECEIVING =
            "Yes, std is eligible, is not receiving services";

    protected static final String CHOICE_TUITION_CODE = "06";

    protected static final List<String> DISTRICT_ENTRY_CODES = Arrays.asList(new String[] {"E1", "R3", "R4",
            "R6", "R7", "R9", "R10", "R12", "R13", "R14"});
    protected static final String DEFAULT_CALENDAR_ID = "Standard";
    protected static final String ENROLLMENT_STATUS_CODE = "Active";
    protected static final String EXITED_CODE = "Exited";
    protected static final String EXPORT_SEASON_FALL_CODE = "fall";
    protected static final String FULL_TIME = "F";
    protected static final String INELIGIBLE_CODE = "Ineligible";
    protected static final String JUNE = "JUNE";
    protected static final String KEY_COUNTY_CODE = "countyCode";
    protected static final String KEY_DISTRICT_CODE = "districtCode";
    protected static final String KEY_SCHOOL_CODE = "schoolCode";
    protected static final String MOVED_KNOWN_TO_CONTINUE = "Moved, Known to be continuing";
    protected static final String OCTOBER = "OCTOBER";
    protected static final String PROGRAM_CODE_LEP = "LEP";
    protected static final String REFUSED = "REFUSED";
    protected static final String RETURNED_TO_GENERAL_ED = "Returned to General Ed";
    protected static final String STATUS_CHANGE_ENROLLMENT_CODE = StudentEnrollment.STATUS_CHANGE;
    protected static final String STRING_00 = "00";
    protected static final String STRING_99 = "99";
    protected static final String STRING_A = "A";
    protected static final String STRING_COMMA = ",";
    protected static final String STRING_HYPHEN = "-";
    protected static final String STRING_R = "R";
    protected static final String STUDENT_STATUS_ACTIVE = "A";
    protected static final String STUDENT_STATUS_INACTIVE = "I";
    protected static final String WITHDRAWAL_CODE_D10 = "D10";


    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<StudentEnrollment>> m_enrollmentMap;
    protected Map<String, RefAttendanceStudent> m_portionAbsentByCode;
    protected PlainDate m_endDate;
    protected String m_excludeSchool;
    protected Map m_excludeSchoolMap;
    protected PlainDate m_spedDate;
    protected PlainDate m_startDate;
    protected Collection<String> m_activeStatuses;
    protected String m_attendingCountyCode;
    protected String m_attendingDistrictCode;
    protected String m_attendingSchoolCode;
    protected String m_exportSeason;
    protected String m_fieldAttendingSchool;
    protected String m_fieldBridgeYear;
    protected String m_fieldCountyCode;
    protected String m_fieldDistrictCode;
    protected String m_fieldElaGradInd;
    protected String m_fieldEnrInDistrPlacement;
    protected String m_fieldEnrollmentType;
    protected String m_fieldGiftedAndTalented;
    protected String m_fieldHomeSchool;
    protected String m_fieldMathGradInd;
    protected String m_fieldMilitaryConnected;
    protected String m_fieldPgmLIEP;
    protected String m_fieldPgmLIEPLang;
    protected String m_fieldReceivingSchool;
    protected String m_fieldResidingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldSklEntryDate;
    protected String m_fieldStdSklEntryDate;
    protected String m_fieldStdDistrEntryDate;
    protected String m_fieldStdFirstEDUS;
    protected String m_fieldYogOriginal;
    protected String m_fieldRemoteDaysMembership;
    protected String m_fieldRemoteDaysPresent;
    protected String m_fieldRatIsVirtual;

    protected Map<String, ReferenceCode> m_militaryCodesMap;
    protected String m_municipalCode;
    protected boolean m_preferenceMemberOnWithdrawal;
    protected String m_resMunicipalCode;
    protected String m_sameDistrictCodes;
    protected String m_snapshot;
    protected String m_tuitionCode;
    protected StudentHistoryHelper m_helper;
    private Map<String, ReferenceCode> m_withdrawalCodes;
    private Map<String, StudentProgramParticipation> m_lepProgramsMap;

    protected static Map<String, String> m_countryDistrictSchoolCodesMap = new HashMap<String, String>();
    protected static Collection<ReferenceCode> m_districtSchoolCodesList = new ArrayList<ReferenceCode>();
    protected static Map<String, String> m_tuitionCodesMap = new HashMap<String, String>();
    protected Map<String, RefAttendanceStudent> m_refAttendanceMap = null;
    protected List<String> m_gradeLevels = null;

    private boolean m_updateStudentIndicator;

    /**
     * Checks if schoolOid given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();
        loadGradeLevelList();
        loadMilitaryConnectedCodesMap();
        loadSchoolExcludeMap();
        loadWithdrawalCodes();
        loadRefAttendanceMap();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

        // Load 'DOE District Codes'
        X2Criteria districtCodesCriteria = new X2Criteria();
        districtCodesCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                REF_TABLE_DOE_DISTRICT_CODES);
        BeanQuery districtCodesQuery = new BeanQuery(ReferenceCode.class, districtCodesCriteria);
        districtCodesQuery.addOrderByAscending(ReferenceCode.COL_CODE);
        m_districtSchoolCodesList = getBroker().getCollectionByQuery(districtCodesQuery);

        // Load County 'District School Codes'
        // The fields: Code is CountrySchoolName, StateCode is CountyDistrictSchoolCode
        X2Criteria countryDistrictSchoolCodesCriteria = new X2Criteria();
        countryDistrictSchoolCodesCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                REF_TABLE_COUNTRY_DISTRICT_SCHOOL_CODES);
        BeanQuery countryDistrictSchoolCodesQuery =
                new BeanQuery(ReferenceCode.class, countryDistrictSchoolCodesCriteria);
        countryDistrictSchoolCodesQuery.addOrderByAscending(ReferenceCode.COL_CODE);
        Collection<ReferenceCode> countryDistrictSchoolCodesList =
                getBroker().getCollectionByQuery(countryDistrictSchoolCodesQuery);

        for (ReferenceCode referenceCode : countryDistrictSchoolCodesList) {
            String districtSchoolCountryCode = referenceCode.getStateCode();
            String countrySchoolName = referenceCode.getCode();
            if (countrySchoolName != null && districtSchoolCountryCode != null) {
                m_countryDistrictSchoolCodesMap.put(countrySchoolName, districtSchoolCountryCode);
            }
        }
        // Load 'Tuition Codes'
        X2Criteria tuitionCodesCriteria = new X2Criteria();
        tuitionCodesCriteria.addEqualTo(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                REF_TABLE_DOE_TUITION_CODES);
        BeanQuery tuitionCodesQuery = new BeanQuery(ReferenceCode.class, tuitionCodesCriteria);
        tuitionCodesQuery.addOrderByAscending(ReferenceCode.COL_CODE);
        Collection<ReferenceCode> tuitionCodesList = getBroker().getCollectionByQuery(tuitionCodesQuery);

        for (ReferenceCode referenceCode : tuitionCodesList) {
            String stateCode = referenceCode.getStateCode();
            if (stateCode != null) {
                String refCode = referenceCode.getCode();

                m_tuitionCodesMap.put(refCode, stateCode);
            }
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Build the criteria and query. Exclude any home schooled students
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(SIDManagementEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SID-ENROLL", new RetrieveEnrollment());
            calcs.put("SID-CDSCODE", new RetrieveCDSCode());
            calcs.put("SID-STATUS", new RetrieveStudentStatus());
            calcs.put("SID-SCHOOL", new RetrieveSchool());
            calcs.put("SID-STUDENT", new RetrieveStudent());
            calcs.put("SID-BRIDGE-YEAR", new RetrieveBridgeYear());
            calcs.put(RetrieveRemoteDaysMembership.CALC_ID, new RetrieveRemoteDaysMembership());
            calcs.put(RetrieveRemoteDaysPresent.CALC_ID, new RetrieveRemoteDaysPresent());
            calcs.put(RetrieveLanguageCodes.CALC_ID, new RetrieveLanguageCodes());

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateFirstEDUS.VAL_ID, new ValidateFirstEDUS());

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    protected Collection<String> getActiveStatuses() {
        if (m_activeStatuses == null) {
            m_activeStatuses = StudentManager.getActiveStudentCodeList(getOrganization());
        }
        return m_activeStatuses;
    }

    /**
     * Gets the latest enrollment.
     *
     * @param student SisStudent
     * @return Student enrollment
     */
    protected StudentEnrollment getLatestEnrollment(SisStudent student) {
        return m_helper.getEnrollmentForDate(student.getOid(), m_endDate, "EY");
    }

    /**
     * Return Code from CodeName.
     *
     * @param codeName String
     * @return String
     */
    protected static String getCodeFromCodeName(String codeName) {
        String code = null;
        if (!StringUtils.isEmpty(codeName) && codeName.indexOf(STRING_HYPHEN) > 0) {
            code = codeName.split(STRING_HYPHEN)[0].trim();
        }
        return code;
    }

    /**
     * Return CountyCode from countryDistrictSchoolName.
     *
     * @param countryDistrictSchoolName String
     * @return String
     */
    protected static String getCountyCodeFromCountyDistrictSchoolCode(String countryDistrictSchoolName) {
        String countyCode = null;

        if (!StringUtils.isEmpty(countryDistrictSchoolName) && countryDistrictSchoolName.length() == 9) {
            countyCode = countryDistrictSchoolName.substring(0, 2);
        }

        return countyCode;
    }

    /**
     * This method returns the county, district code by splitting the
     * countyDistrictSchoolCode code.
     *
     * @param countyDistrictSchoolCode String
     * @return String
     */
    protected String getCountyDistrictCode(String countyDistrictSchoolCode) {
        String countyDistrictCode = null;
        if (!StringUtils.isEmpty(countyDistrictSchoolCode) && countyDistrictSchoolCode.length() > 6) {
            countyDistrictCode = countyDistrictSchoolCode.substring(0, 6);
        }
        return countyDistrictCode;
    }

    /**
     * Lookup CountryDistrictStateCode By CountySchoolName.
     *
     * @param countrySchoolName String
     * @return String
     */
    protected static String getCountryDistrictStateCodeByCountySchoolName(String countrySchoolName) {
        String countryDistrictStateCode = null;

        if (!StringUtils.isEmpty(countrySchoolName) && m_countryDistrictSchoolCodesMap.containsKey(countrySchoolName)) {
            countryDistrictStateCode = m_countryDistrictSchoolCodesMap.get(countrySchoolName);
        }

        return countryDistrictStateCode;
    }

    /**
     * Lookup District Code by Country School Name.
     *
     * @param countrySchoolName String
     * @return String
     */
    protected static String getDistrictCodeByCountySchoolName(String countrySchoolName) {
        String districtCode = null;

        String districtSchoolCountry = getCountryDistrictStateCodeByCountySchoolName(countrySchoolName);

        if (!StringUtils.isEmpty(districtSchoolCountry) && districtSchoolCountry.length() == 9) {
            // DOE countryDistrictSchoolCode
            // CountyCode is pos 1, len 2
            // DistrictCode is pos 3, len 4
            // SchoolCode is pos 5, len 3
            districtCode = districtSchoolCountry.substring(2, 6);
        }

        return districtCode;
    }

    /**
     * Return DistrictCode from countryDistrictSchoolName.
     *
     * @param countryDistrictSchoolName String
     * @return String
     */
    protected static String getDistrictCodeFromCountyDistrictSchoolCode(String countryDistrictSchoolName) {
        String districtCode = null;

        if (!StringUtils.isEmpty(countryDistrictSchoolName) && countryDistrictSchoolName.length() == 9) {
            districtCode = countryDistrictSchoolName.substring(2, 6);
        }

        return districtCode;
    }

    /**
     * Gets the lep program.
     *
     * @param m_student SisStudent
     * @return Student program participation
     */
    protected StudentProgramParticipation getLepProgram(SisStudent m_student) {
        if (m_lepProgramsMap == null) {
            ModelProperty prop = new ModelProperty(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_PROGRAM_CODE, getBroker().getPersistenceKey());
            DataDictionaryField field = getDataDictionary().findDataDictionaryField(prop.getFieldId());
            if (StringUtils.isEmpty(field.getReferenceTableOid())) {
                throw new IllegalStateException(
                        "The student program participation code column must contain a reference table");
            }
            X2Criteria refCodeCriteria = new X2Criteria();
            refCodeCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, PROGRAM_CODE_LEP);
            refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID,
                    new ColumnQuery(SisStudent.class, new String[] {X2BaseBean.COL_OID},
                            m_helper.getStudentCriteria()));
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE,
                    new ColumnQuery(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE},
                            refCodeCriteria));
            criteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

            BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
            query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, true);
            m_lepProgramsMap = getBroker().getMapByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
        }
        return m_lepProgramsMap.get(m_student.getOid());
    }

    /**
     * Return SchoolCode from countryDistrictSchoolName.
     *
     * @param countryDistrictSchoolName String
     * @return String
     */
    protected static String getSchoolCodeFromCountyDistrictSchoolCode(String countryDistrictSchoolName) {
        String schoolCode = null;

        if (!StringUtils.isEmpty(countryDistrictSchoolName) && countryDistrictSchoolName.length() == 9) {
            schoolCode = countryDistrictSchoolName.substring(6, 9);
        }

        return schoolCode;
    }

    /**
     * Checks if is update student.
     *
     * @return true, if is update student
     */
    protected boolean isUpdateStudent() {
        return m_updateStudentIndicator;
    }

    /**
     * This method returns the school start date.
     *
     * @return Plain date
     */
    protected PlainDate getStartDate() {
        return m_startDate;
    }

    /**
     * This method returns the studentHistoryHelper.
     *
     * @return Student history helper
     */
    protected StudentHistoryHelper getStudentHistoryHelper() {
        return m_helper;
    }

    /**
     * This populates a map of reference attendance codes by attendance code.
     *
     * @return void
     */
    private void getRefAttendanceCodeByAttendanceCode() {
        m_portionAbsentByCode = new HashMap<String, RefAttendanceStudent>();

        Criteria refTableCriteria = new Criteria();
        refTableCriteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE, "0");

        QueryByCriteria query = new QueryByCriteria(RefAttendanceStudent.class, refTableCriteria);
        m_portionAbsentByCode = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 10);
    }

    /**
     * Gets the checks if is virtual field.
     *
     * @return String
     */
    public String getIsVirtualField() {
        if (m_fieldRatIsVirtual == null) {
            m_fieldRatIsVirtual = translateAliasToJavaName(ALIAS_RAT_VIRTUAL_FLAG, true);
        }

        return m_fieldRatIsVirtual;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        getRefAttendanceCodeByAttendanceCode();
        m_preferenceMemberOnWithdrawal = Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        Object parameter = getParameter(INPUT_PARAM_UPDATE_STUDENT);
        m_updateStudentIndicator =
                parameter != null && parameter instanceof Boolean && ((Boolean) parameter).booleanValue() ? true
                        : false;

        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);

        m_fieldRemoteDaysMembership = translateAliasToJavaName(ALIAS_REMOTE_DAYS_MEMBERSHIP, true);
        m_fieldRemoteDaysPresent = translateAliasToJavaName(ALIAS_REMOTE_DAYS_PRESENT, true);

        m_fieldEnrollmentType = translateAliasToJavaName(ALIAS_ENROLLMENT_CLASSIFICATION, true);
        m_fieldAttendingSchool = translateAliasToJavaName(ALIAS_ATTENDING_SCHOOL, true);
        m_fieldBridgeYear = translateAliasToJavaName(ALIAS_BRIDGE_YEAR, true);
        m_fieldReceivingSchool = translateAliasToJavaName(ALIAS_RECEIVING_SCHOOL, true);
        m_fieldResidingSchool = translateAliasToJavaName(ALIAS_RESIDENT_SCHOOL, true);
        m_fieldCountyCode = translateAliasToJavaName(ALIAS_COUNTY_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);
        m_fieldYogOriginal = translateAliasToJavaName(ALIAS_YOG_ORIGINAL, true);
        m_fieldElaGradInd = translateAliasToJavaName(ALIAS_ELA_GRAD_IND, true);
        m_fieldMathGradInd = translateAliasToJavaName(ALIAS_MATH_GRAD_IND, true);
        m_tuitionCode = translateAliasToJavaName(ALIAS_TUITION_CODE, true);
        m_snapshot = (String) getParameter(PARAM_SNAPSHOT);
        m_sameDistrictCodes = translateAliasToJavaName(ALIAS_SAME_DISTRICT_CODES_LIST, false);
        m_municipalCode = translateAliasToJavaName(ALIAS_MUNICIPAL_CODE, true);
        m_resMunicipalCode = translateAliasToJavaName(ALIAS_RES_MUNICIPAL_CODE, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldMilitaryConnected = translateAliasToJavaName(ALIAS_MILITARY_CONNECTED, true);
        m_fieldSklEntryDate = translateAliasToJavaName(ALIAS_SKL_ENTRY_DATE, true);
        m_fieldStdSklEntryDate = translateAliasToJavaName(ALIAS_STD_SKL_ENTRY_DATE, true);
        m_fieldStdDistrEntryDate = translateAliasToJavaName(ALIAS_STD_DISTR_ENTRY_DATE, true);
        m_fieldStdFirstEDUS = translateAliasToJavaName(ALIAS_FIRST_ENT_US, true);
        m_fieldEnrInDistrPlacement = translateAliasToJavaName(ALIAS_ENR_IN_DISTR_PLACE, true);
        m_fieldPgmLIEP = translateAliasToJavaName(ALIAS_PGM_LIEP, true);
        m_fieldPgmLIEPLang = translateAliasToJavaName(ALIAS_PGM_LIEP_LANG, true);
        m_fieldGiftedAndTalented = translateAliasToJavaName(ALIAS_GIFTED_AND_TALENTED, true);
        Calendar boyBackOneYearAndDay = Calendar.getInstance();

        boyBackOneYearAndDay.setTime(m_startDate);
        boyBackOneYearAndDay.add(Calendar.YEAR, -1);
        boyBackOneYearAndDay.add(Calendar.DATE, -1);

        m_spedDate = new PlainDate(boyBackOneYearAndDay.getTime());
    }

    /**
     * Load grade level ref list.
     */
    private void loadGradeLevelList() {
        ReferenceTable gradeLevelRefTable = (ReferenceTable) getBroker().getBeanByOid(ReferenceTable.class,
                ReferenceTable.REF_TABLE_OID_GRADE_LEVEL);
        Map<String, ReferenceCode> gradeLevelMap = gradeLevelRefTable.getCodeMap();

        ExtendedDataDictionary gradeLevelDdx = gradeLevelRefTable.getExtendedDataDictionary();
        DataDictionary gradeLevelDdxDictionary =
                DataDictionary.getDistrictDictionary(gradeLevelDdx, getBroker().getPersistenceKey());
        DataDictionaryField field = gradeLevelDdxDictionary.findDataDictionaryFieldByAlias("NumericGradeLevel");

        List<String> gradeLevels = new ArrayList<String>();
        for (ReferenceCode code : gradeLevelMap.values()) {
            String numericLevel = (String) code.getFieldValueByBeanPath(field.getJavaName());
            Integer grade = Integer.parseInt(numericLevel);
            if (grade == 12) {
                gradeLevels.add(code.getCode());
            }
        }
        m_gradeLevels = gradeLevels;
    }

    /**
     * Load map of the all (including disabled) military connected codes.
     */
    private void loadMilitaryConnectedCodesMap() {

        DataDictionaryField ddfMilitary = getDataDictionaryField(SisStudent.class, m_fieldMilitaryConnected);
        if (ddfMilitary != null && ddfMilitary.getReferenceTable() != null) {
            X2Criteria rcdCriteria = new X2Criteria();
            rcdCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ddfMilitary.getReferenceTableOid());

            m_militaryCodesMap = getBroker().getMapByQuery(
                    new QueryByCriteria(ReferenceCode.class, rcdCriteria), ReferenceCode.COL_CODE, 1024);
        } else {
            m_militaryCodesMap = new HashMap<>();
        }
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Initialize a map of Daily Attendance Reference Codes.
     */
    private void loadRefAttendanceMap() {
        DataDictionaryField isVirtualField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_RAT_VIRTUAL_FLAG);
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE, RefAttendanceStudent.ATTENDANCE_TYPE_DAILY);
        criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE, RefAttendanceStudent.TYPE_OTHER_CODE);
        if (isVirtualField != null) {
            criteria.addEqualTo(isVirtualField.getJavaName(), BooleanAsStringConverter.TRUE);
        }

        BeanQuery query = new BeanQuery(RefAttendanceStudent.class, criteria);

        m_refAttendanceMap = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 8);
    }

    /**
     * Loads map of withdrawal codes from preference.
     */
    private void loadWithdrawalCodes() {
        String withdrawalCodesRefTbl = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        Criteria withdrawalCodesCriteria = new Criteria();
        withdrawalCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, withdrawalCodesRefTbl);
        QueryByCriteria withdrawalCodesQuery = new QueryByCriteria(ReferenceCode.class, withdrawalCodesCriteria);

        m_withdrawalCodes = getBroker().getMapByQuery(withdrawalCodesQuery, ReferenceCode.COL_CODE, 256);

    }
}