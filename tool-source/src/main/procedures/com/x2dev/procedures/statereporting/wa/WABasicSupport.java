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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for WA P223 Basic Support.
 *
 * @author X2 Development Corporation
 */

public class WABasicSupport extends StateReportData {
    /**
     * Entity class for WA P223 Basic Support export.
     *
     */
    public static class WABasicSupportEntity extends StateReportEntity {
        private static final String CAL_CODE_STANDARD = "Standard";
        private static final String CODE_BILINGUAL_PROGRAM = "1";
        private static final String CODE_NO_SHOW = "NS";
        private static final String CODE_NO_SHOW_2 = "U2";
        private static final String STRING_SPLITTER = "\\(";

        /**
         * Entity instance variables.
         */
        // Needed for FTE Audit Report
        HashMap<Section, HashMap<String, Object>> m_auditInfo = new HashMap<Section, HashMap<String, Object>>();
        WABasicSupport m_data;
        StudentEnrollmentSpan m_enrollmentSpan = null;
        BigDecimal[] m_fte = new BigDecimal[NUM_FTE];
        Collection<StudentProgramParticipation> m_programParticipations;
        boolean m_skipHeadcount = false;
        boolean m_isOpenDoorsMember = false;
        boolean kTwelveOverrideValue = false;

        /**
         * Return map for Audit Report .
         *
         * @return Hash map
         */
        public HashMap<Section, HashMap<String, Object>> getAuditInfo() {
            return m_auditInfo;
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentEnrollmentSpan getEnrollmentSpan() {
            return m_enrollmentSpan;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name;
            if (getBean() instanceof SisStudent) {
                SisStudent student = (SisStudent) getBean();
                name = student.getNameView() +
                        " [LASID: " + student.getLocalId() +
                        ", SASID: " + student.getStateId() +
                        "] ";
            } else {
                name = getBean().getClass().toString();
            }
            return name;
        }

        /**
         * Returns the FTE Equivalent array for the current index.
         *
         * @return BigDecimal[]
         */
        public BigDecimal[] getFte() {
            return m_fte;
        }

        /**
         * Returns the Collection of StudentProgramParticipations record for the current Student.
         *
         * @return Collection <StudentProgramParticipation>
         */
        public Collection<StudentProgramParticipation> getProgramParticipations() {
            return m_programParticipations;
        }

        /**
         * Return flag for skipping headcount.
         *
         * @return boolean
         */
        public boolean getSkipHeadcount() {
            return m_skipHeadcount;
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
            m_data = (WABasicSupport) data;

            // If we use export result, where nothing to initialize, bean
            // already have all needed data.
            if (!m_data.isUseExportResults()) {
                List<StudentEnrollmentSpan> enrollmentSpans =
                        m_data.m_helper.getStudentEnrollmentSpans((SisStudent) bean,
                                true);
                Iterator<StudentEnrollmentSpan> i_enrollmentSpans = enrollmentSpans.iterator();

                // find enrollment records
                while (i_enrollmentSpans.hasNext()) {
                    StudentEnrollmentSpan enrollmentSpan = i_enrollmentSpans.next();
                    String excludeSchoolFlag =
                            (String) enrollmentSpan.getSchool().getFieldValueByAlias(ALIAS_EXC_SKL_IN_P223);
                    boolean excludeSchool = BooleanAsStringConverter.TRUE.equals(excludeSchoolFlag);
                    if (excludeSchool) {
                        continue;
                    }

                    // Exclude spans where withdrawal code = Local Code 'NS' or State Code 'U2'
                    StudentEnrollment withdrawal = enrollmentSpan.getFirstInactiveEnrollment();
                    if (withdrawal != null && !StringUtils.isEmpty(withdrawal.getEnrollmentCode())) {
                        String stateCode = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                StudentEnrollment.COL_ENROLLMENT_CODE,
                                withdrawal.getEnrollmentCode(),
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        String localCode = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                StudentEnrollment.COL_ENROLLMENT_CODE,
                                withdrawal.getEnrollmentCode(),
                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());

                        if (CODE_NO_SHOW.equals(localCode) || CODE_NO_SHOW_2.equals(stateCode)) {
                            continue;
                        }
                    }

                    // Skip enrollment records for students receiving service at
                    // another district.
                    String districtServe = (String) enrollmentSpan.getFirstActiveEnrollment()
                            .getFieldValueByBeanPath(m_data.m_fieldDistrictServe);
                    if (!StringUtils.isEmpty(districtServe)) {
                        districtServe = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                m_data.m_fieldDistrictServe,
                                districtServe,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                    if (!StringUtils.isEmpty(districtServe) && !districtServe.equals(m_data.m_districtId)) {
                        continue;
                    }

                    // Check this enrollment with report date
                    // Logic is to include enrollment that starts on report date
                    // and ends after report date
                    if (enrollmentSpan.getFirstActiveDate().after(m_data.m_reportDate) ||
                            ((enrollmentSpan.getLastActiveDate() != null) &&
                                    enrollmentSpan.getLastActiveDate().before(m_data.m_reportDate))) {
                        continue;
                    }

                    m_enrollmentSpan = enrollmentSpan;
                    break;
                }

                // Check attendance requirement
                if ((m_enrollmentSpan == null) || !isAttendanceOK((SisStudent) bean, m_enrollmentSpan)) {
                    setRowCount(0);
                } else {
                    if (m_data.getStudentOpenDoorsPrograms((SisStudent) bean) != null) {
                        m_isOpenDoorsMember = true;
                    }
                    initializeFTE(bean);
                    lookupOverrideFte(bean);
                    reduceFTE();
                    String skipHeadcountlFlag = (String) m_enrollmentSpan.getFirstActiveEnrollment()
                            .getFieldValueByAlias(ALIAS_SKIP_P223_HEADCOUNT);
                    boolean skipHeadcount = BooleanAsStringConverter.TRUE.equals(skipHeadcountlFlag);

                    if (skipHeadcount) {
                        m_skipHeadcount = skipHeadcount;
                    }
                    m_programParticipations = m_data.getStudentPrograms((SisStudent) bean);
                }
            }
        }

        /**
         * Check if student enrolled in bilungual program.
         * Must be public for be called under reflection.
         *
         * @return Boolean
         */
        public Boolean isBilingual() {
            Boolean value = Boolean.FALSE;
            Collection<StudentProgramParticipation> studentProgramParticipations = getProgramParticipations();
            if (studentProgramParticipations != null) {
                Iterator<StudentProgramParticipation> i_studentProgramParticipations =
                        studentProgramParticipations.iterator();

                while (i_studentProgramParticipations.hasNext()) {
                    StudentProgramParticipation studentProgramParticipation = i_studentProgramParticipations.next();
                    String programValue = (String) studentProgramParticipation
                            .getFieldValueByBeanPath(m_data.m_fieldLepProgramDesignation);
                    String code = m_data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                            m_data.m_fieldLepProgramDesignation,
                            programValue,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (CODE_BILINGUAL_PROGRAM.equals(code)) {
                        value = Boolean.TRUE;
                        break;
                    }
                }
            }

            return value;
        }

        /**
         * Return flag about membership in Open Doors program.
         *
         * @return true, if is open doors member
         */
        public boolean isOpenDoorsMember() {
            return m_isOpenDoorsMember;
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
         * Retrieve a string value from a field. Interpret it as a BigDecimal if it is not empty.
         *
         * @param bean X2BaseBean
         * @param field String
         * @return BigDecimal
         * @throws X2BaseException exception
         */
        private BigDecimal getBigDecimalFromField(X2BaseBean bean, String field) throws X2BaseException {
            String sFte = (String) m_data.getProperty(bean, field);
            BigDecimal fte = null;

            if (!StringUtils.isEmpty(sFte)) {
                try {
                    fte = new BigDecimal(sFte);
                } catch (NumberFormatException nfe) {
                    // ignore this value.
                }
            }
            return fte;
        }

        /**
         * Populates the fte array with totals based on the schedule.
         *
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        private void initializeFTE(X2BaseBean bean) throws X2BaseException {
            for (int i = 0; i < m_fte.length; ++i) {
                m_fte[i] = BigDecimal.ZERO;
            }
            String grade = m_data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    ((SisStudent) bean).getGradeLevel(),
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            // Get data based on StudentScheduleSpan.MasterSchedule
            List<StudentScheduleSpan> scheduleSpans = null;
            try {
                // here is the problem, its using the school context
                boolean origSchoolContext = m_data.isSchoolContext();
                m_data.setSchoolContext(false);
                scheduleSpans = m_data.m_helper.getStudentScheduleSpans((SisStudent) bean);
                m_data.setSchoolContext(origSchoolContext);
            } catch (NullPointerException npe) {
                // Catch bug in schedule spans. continue.
            }
            if (scheduleSpans != null) {
                Iterator<StudentScheduleSpan> i_scheduleSpans = scheduleSpans.iterator();

                while (i_scheduleSpans.hasNext()) {
                    StudentScheduleSpan scheduleSpan = i_scheduleSpans.next();

                    // Check if the span is on report date.
                    // Logic is to include span that starts before or on report date and
                    // ends after or on report date
                    if (((scheduleSpan.getEntryDate() != null)
                            && !m_data.m_reportDate.before(scheduleSpan.getEntryDate())) &&
                            ((scheduleSpan.getExitDate() == null)
                                    || !m_data.m_reportDate.after(scheduleSpan.getExitDate()))) {
                        MasterSchedule section = scheduleSpan.getSection();
                        String period = section.getScheduleDisplay();
                        Course course = section.getSchoolCourse().getCourse();
                        BigDecimal fte = getBigDecimalFromField(course, m_data.m_fieldCourseFte);
                        BigDecimal fteOver = getBigDecimalFromField(section, m_data.m_fieldSectionFte);
                        String fteTypeOver = (String) section.getFieldValueByBeanPath(m_data.m_fieldSectionFteType);
                        String fteType = (String) course.getFieldValueByBeanPath(m_data.m_fieldCourseFteType);
                        String aleCat = (String) course.getFieldValueByBeanPath(m_data.m_fieldCourseAleCat);
                        SisSchool school = section.getSchedule().getSchool();
                        String isRemote = (String) school.getFieldValueByBeanPath(m_data.m_fieldSklIsRemote);
                        String isHighPoverty = BooleanAsStringConverter.FALSE;

                        HashMap<String, Object> auditInfo = m_auditInfo.get(section);
                        if (auditInfo == null) {
                            auditInfo = new HashMap<String, Object>();
                            auditInfo.put(AUDIT_COURSE, course);
                            auditInfo.put(AUDIT_SCHOOL, school);
                            auditInfo.put(AUDIT_DISTRICT_ID, school.getOrganization1().getId());
                            if (period != null) {
                                period = period.split(STRING_SPLITTER)[0];
                                auditInfo.put(AUDIT_PERIOD, period);
                            }
                        }
                        if (fte != null) {
                            auditInfo.put(AUDIT_FTE_CRS, fte);
                        }
                        // set defaults
                        if (fteOver != null) {
                            fte = fteOver;
                            auditInfo.put(AUDIT_FTE_OVER, fteOver);
                        }
                        if (fteTypeOver != null) {
                            fteType = fteTypeOver;
                            if (fteTypeOver.matches(MATCH_FTE_TYPE_NON_VOC_RS)) {
                                auditInfo.put(AUDIT_NON_VOC_RS, Boolean.TRUE);
                            } else if (fteTypeOver.matches(MATCH_FTE_TYPE_SKILLS_CENTER)) {
                                auditInfo.put(AUDIT_SKILLS_CENTER, Boolean.TRUE);
                            } else if (fteTypeOver.matches(MATCH_FTE_TYPE_VOC_PROGRAM)) {
                                auditInfo.put(AUDIT_VOC, Boolean.TRUE);
                            } else if (fteTypeOver.matches(MATCH_FTE_TYPE_VOC_RS)) {
                                auditInfo.put(AUDIT_VOC, Boolean.TRUE);
                                auditInfo.put(AUDIT_NON_VOC_RS, Boolean.TRUE);
                            }
                        }
                        if (fteType == null) {
                            fteType = DEFAULT_FTE_TYPE;
                        }
                        if (aleCat == null) {
                            aleCat = DEFAULT_ALE_CAT;
                        }
                        // High poverty only applies to grade K-3. For
                        // elementary marked as poverty, turn it off for
                        // other grades.
                        if ((grade != null) && grade.matches(MATCH_GRADE_K3)) {
                            String schoolValue =
                                    (String) school.getFieldValueByBeanPath(m_data.m_fieldSklIsHighPoverty);
                            if ((schoolValue != null) && BooleanAsStringConverter.TRUE.equals(schoolValue)) {
                                isHighPoverty = BooleanAsStringConverter.TRUE;
                            }
                        }
                        if ((isRemote == null) || !BooleanAsStringConverter.TRUE.equals(isRemote)) {
                            isRemote = BooleanAsStringConverter.FALSE;
                        }

                        if (fte != null) {
                            if (fteType.matches(MATCH_FTE_TYPE_VOC_RS)) {
                                m_fte[INDEX_FTE_VOC_RS] = m_fte[INDEX_FTE_VOC_RS].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_NON_VOC_RS)) {
                                m_fte[INDEX_FTE_NON_VOC_RS] = m_fte[INDEX_FTE_NON_VOC_RS].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM) && (grade != null) &&
                                    grade.matches(MATCH_GRADE_78) && aleCat.matches(MATCH_ALE_CAT_NOT_ALE)) {
                                m_fte[INDEX_FTE_VOC_78] = m_fte[INDEX_FTE_VOC_78].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM) && (grade != null) &&
                                    grade.matches(MATCH_GRADE_78) && aleCat.matches(MATCH_ALE_CAT_QUALIFIED)) {
                                m_fte[INDEX_FTE_VOC_78_ALE_Q] = m_fte[INDEX_FTE_VOC_78_ALE_Q].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM) && (grade != null) &&
                                    grade.matches(MATCH_GRADE_78) && aleCat.matches(MATCH_ALE_CAT_NOT_QUALIFIED)) {
                                m_fte[INDEX_FTE_VOC_78_ALE_NQ] = m_fte[INDEX_FTE_VOC_78_ALE_NQ].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM) && (grade != null) &&
                                    grade.matches(MATCH_GRADE_912) && aleCat.matches(MATCH_ALE_CAT_NOT_ALE)) {
                                m_fte[INDEX_FTE_VOC_912] = m_fte[INDEX_FTE_VOC_912].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM) && (grade != null) &&
                                    grade.matches(MATCH_GRADE_912) && aleCat.matches(MATCH_ALE_CAT_QUALIFIED)) {
                                m_fte[INDEX_FTE_VOC_912_ALE_Q] = m_fte[INDEX_FTE_VOC_912_ALE_Q].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM) && (grade != null) &&
                                    grade.matches(MATCH_GRADE_912) && aleCat.matches(MATCH_ALE_CAT_NOT_QUALIFIED)) {
                                m_fte[INDEX_FTE_VOC_912_ALE_NQ] = m_fte[INDEX_FTE_VOC_912_ALE_NQ].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_SKILLS_CENTER)
                                    && aleCat.matches(MATCH_ALE_CAT_NOT_ALE)) {
                                m_fte[INDEX_FTE_SKILLS_CENTER] = m_fte[INDEX_FTE_SKILLS_CENTER].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_SKILLS_CENTER)
                                    && aleCat.matches(MATCH_ALE_CAT_QUALIFIED)) {
                                m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q] = m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_SKILLS_CENTER)
                                    && aleCat.matches(MATCH_ALE_CAT_NOT_QUALIFIED)) {
                                m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ] = m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM)) {
                                m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_REG_SCHOOL) && aleCat.matches(MATCH_ALE_CAT_NOT_ALE) &&
                                    !BooleanAsStringConverter.TRUE.equals(isRemote)) {
                                m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_1) && aleCat.matches(MATCH_ALE_CAT_NOT_ALE) &&
                                    BooleanAsStringConverter.TRUE.equals(isHighPoverty)) {
                                // If on the Schedule Master table, field
                                // Section FTE Type=Regular and there is an
                                // amount in the Schedule Master Section FTE
                                // field, use that amount for the K-12 FTE
                                if (!StringUtils.isEmpty(fteTypeOver) && (fteOver != null) &&
                                        fteTypeOver.matches(MATCH_FTE_TYPE_REG_SCHOOL)) {
                                    m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].add(fteOver);
                                    kTwelveOverrideValue = true;
                                } else {
                                    m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].add(fte);
                                }
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_1) && aleCat.matches(MATCH_ALE_CAT_NOT_ALE) &&
                                    BooleanAsStringConverter.TRUE.equals(isRemote)) {
                                m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_1) && aleCat.matches(MATCH_ALE_CAT_QUALIFIED)) {
                                m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].add(fte);
                            }

                            if (fteType.matches(MATCH_FTE_TYPE_1) && aleCat.matches(MATCH_ALE_CAT_NOT_QUALIFIED)) {
                                m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].add(fte);
                            }

                            if (m_isOpenDoorsMember && fteType.matches(MATCH_FTE_TYPE_VOC_PROGRAM)) {
                                m_fte[INDEX_FTE_OPEN_DOORS_VOC] = m_fte[INDEX_FTE_OPEN_DOORS_VOC].add(fte);
                            } else if (m_isOpenDoorsMember) {
                                m_fte[INDEX_FTE_OPEN_DOORS] = m_fte[INDEX_FTE_OPEN_DOORS].add(fte);
                            }
                        }
                        m_auditInfo.put(section, auditInfo);
                    }
                }
                m_fte[INDEX_FTE_K12_AND_SC] =
                        (m_fte[INDEX_FTE_K12_AND_SC].add(m_fte[INDEX_FTE_K12])).add(m_fte[INDEX_FTE_SKILLS_CENTER]);

                if (m_fte[INDEX_FTE_K12_AND_SC].compareTo(BigDecimal.ZERO) > 0) {
                    m_fte[INDEX_FTE_HEADCOUNT] = BigDecimal.ONE;
                }
            }
        }

        /**
         * Enforces attendance rule by testing for attendance within the last 20 days.
         *
         * @param student SisStudent
         * @param span StudentEnrollmentSpan
         * @return true, if is attendance OK
         */
        private boolean isAttendanceOK(SisStudent student, StudentEnrollmentSpan span) {
            // Check bypass
            if (!m_data.m_checkAttendance.booleanValue()) {
                return true;
            }

            Set<PlainDate> absences = new HashSet<PlainDate>();
            List<StudentAttendance> attendenceRecords = m_data.m_helper.getStudentAttendances(student.getOid());
            if (attendenceRecords != null) {
                Iterator attendenceRecord = attendenceRecords.iterator();
                while (attendenceRecord.hasNext()) {
                    StudentAttendance sa = (StudentAttendance) attendenceRecord.next();

                    if (!sa.getDate().before(span.getFirstActiveDate()) &&
                            ((span.getLastActiveDate() == null) || !sa.getDate().after(span.getLastActiveDate())) &&
                            (sa.getPortionAbsent().intValue() >= 1)) {
                        absences.add(sa.getDate());
                    }
                }
            }

            Set<PlainDate> insessionDates =
                    m_data.m_helper.getCalendarDays(span.getSchool(), student.getCalendarCode());
            if ((insessionDates == null) && !CAL_CODE_STANDARD.equals(student.getCalendarCode())) {
                insessionDates = m_data.m_helper.getCalendarDays(span.getSchool(), CAL_CODE_STANDARD);
            }
            PlainDate[] lastDays = new PlainDate[NUM_DAYS];

            // Count in session days between (and including) first and last
            // active dates.
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (!date.before(span.getFirstActiveDate()) &&
                            ((span.getLastActiveDate() == null) || !date.after(span.getLastActiveDate()))) {
                        if (date.after(m_data.m_reportDate)) {
                            continue;
                        }
                        for (int i = 0; i < NUM_DAYS; ++i) {
                            if (lastDays[i] == null) {
                                lastDays[i] = date;
                                break;
                            }
                            if (!lastDays[i].after(date)) {
                                for (int j = NUM_DAYS - 2; j >= i; --j) {
                                    lastDays[j + 1] = lastDays[j];
                                }
                                lastDays[i] = date;
                                break;
                            }
                        }
                    }
                }
            }
            for (int i = 0; i < NUM_DAYS; ++i) {
                if ((lastDays[i] != null) && !absences.contains(lastDays[i])) {
                    return true;
                }
            }

            // Check student extension data for basic support
            PlainDate extDate = (PlainDate) m_enrollmentSpan.getFirstActiveEnrollment()
                    .getFieldValueByBeanPath(m_data.m_fieldBasicSupExtDate);
            if ((extDate != null) && !extDate.before(m_data.m_reportDate)) {
                return true;
            }
            return false;
        }

        /**
         * Lookup student programs for FTE override. If an FTE value has an override, apply it to
         * the fte array.
         *
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        private void lookupOverrideFte(X2BaseBean bean) throws X2BaseException {
            Collection<StudentProgramParticipation> studentFTEPrograms =
                    m_data.m_studentFTEProgramMap.get(bean.getOid());
            PlainDate reportDate = m_data.m_reportDate;
            StudentProgramParticipation fteProgram = null;
            if (studentFTEPrograms != null) {
                for (StudentProgramParticipation program : studentFTEPrograms) {
                    if ((program.getStartDate() != null) && !program.getStartDate().after(reportDate)) {
                        if ((program.getEndDate() == null) || !program.getEndDate().before(reportDate)) {
                            fteProgram = program;
                            break;
                        }
                    }
                }

                if (fteProgram != null) {
                    BigDecimal fteK12 = getBigDecimalFromField(fteProgram, m_data.m_fieldFTEK12);
                    BigDecimal fteRS = getBigDecimalFromField(fteProgram, m_data.m_fieldFTERS);
                    BigDecimal fteRSV = getBigDecimalFromField(fteProgram, m_data.m_fieldFTERSV);
                    BigDecimal fteSC = getBigDecimalFromField(fteProgram, m_data.m_fieldFTESC);
                    BigDecimal fteVOC = getBigDecimalFromField(fteProgram, m_data.m_fieldFTEVOC);

                    // Update Regular K12 value to K12 fields.
                    if (fteK12 != null) {
                        if (!kTwelveOverrideValue) {
                            if (m_fte[INDEX_FTE_K12].doubleValue() > 0) {
                                BigDecimal factor = fteK12.divide(m_fte[INDEX_FTE_K12], 4, RoundingMode.HALF_EVEN);
                                m_fte[INDEX_FTE_K12] = fteK12;
                                m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].multiply(factor);
                                m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].multiply(factor);
                                m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].multiply(factor);
                                m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].multiply(factor);

                            } else {
                                m_fte[INDEX_FTE_K12] = fteK12;
                            }
                        }
                    }

                    // Update vocational to vocational values.
                    if (fteVOC != null) {
                        if (m_fte[INDEX_FTE_VOC_78].doubleValue() > 0) {
                            BigDecimal factor = fteVOC.divide(m_fte[INDEX_FTE_VOC_78], 4, RoundingMode.HALF_EVEN);
                            m_fte[INDEX_FTE_VOC_78] = fteVOC;
                            m_fte[INDEX_FTE_VOC_78_ALE_Q] = m_fte[INDEX_FTE_VOC_78_ALE_Q].multiply(factor);
                            m_fte[INDEX_FTE_VOC_78_ALE_NQ] = m_fte[INDEX_FTE_VOC_78_ALE_NQ].multiply(factor);
                        } else if (m_fte[INDEX_FTE_VOC_912].doubleValue() > 0) {
                            BigDecimal factor = fteVOC.divide(m_fte[INDEX_FTE_VOC_912], 4, RoundingMode.HALF_EVEN);
                            m_fte[INDEX_FTE_VOC_912] = fteVOC;
                            m_fte[INDEX_FTE_VOC_912_ALE_Q] = m_fte[INDEX_FTE_VOC_912_ALE_Q].multiply(factor);
                            m_fte[INDEX_FTE_VOC_912_ALE_NQ] = m_fte[INDEX_FTE_VOC_912_ALE_NQ].multiply(factor);
                        } else {
                            m_fte[INDEX_FTE_VOC_78] = fteVOC;
                        }
                    }

                    // Update Running Start values.
                    if (fteRSV != null) {
                        m_fte[INDEX_FTE_VOC_RS] = fteRSV;
                    }
                    if (fteRS != null) {
                        m_fte[INDEX_FTE_NON_VOC_RS] = fteRS;
                    }

                    // Skills Center
                    if (fteSC != null) {
                        if (m_fte[INDEX_FTE_SKILLS_CENTER].doubleValue() > 0) {
                            BigDecimal factor = fteSC.divide(m_fte[INDEX_FTE_SKILLS_CENTER], 4, RoundingMode.HALF_EVEN);
                            m_fte[INDEX_FTE_SKILLS_CENTER] = fteSC;
                            m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q] =
                                    m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q].multiply(factor);
                            m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ] =
                                    m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ].multiply(factor);

                        } else {
                            m_fte[INDEX_FTE_SKILLS_CENTER] = fteSC;
                        }
                    }
                }
            }
        }

        /**
         * Implements rules to reduce FTE based on reporting limits.
         */
        private void reduceFTE() {
            // Check if a student has Running Start FTE
            BigDecimal sumRS = m_fte[INDEX_FTE_VOC_RS].add(m_fte[INDEX_FTE_NON_VOC_RS]);
            if (sumRS.compareTo(BigDecimal.ZERO) > 0) {
                // reduce sum of Running Start FTE to 1
                if (sumRS.compareTo(BigDecimal.ONE) > 0) {
                    m_fte[INDEX_FTE_VOC_RS] = m_fte[INDEX_FTE_VOC_RS].divide(sumRS);
                    m_fte[INDEX_FTE_NON_VOC_RS] = m_fte[INDEX_FTE_NON_VOC_RS].divide(sumRS);
                    sumRS = BigDecimal.ONE;
                }

                // reduce sum of Remaining FTE to (1.2 - Running Start FTE)
                BigDecimal lim = BigDecimal.valueOf(1.2).subtract(sumRS);
                BigDecimal sumHS = m_fte[INDEX_FTE_K12].add(m_fte[INDEX_FTE_HIGH_POVERTY].add(m_fte[INDEX_FTE_REMOTE]
                        .add(m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].add(m_fte[INDEX_FTE_ALE_NQ]))));
                if (sumHS.compareTo(lim) > 0) {
                    m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].divide(sumHS).multiply(lim);
                    sumHS = lim;
                }

                // reduce sum of Remaining FTE to 1
                if (sumHS.compareTo(BigDecimal.ONE) > 0) {
                    m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].divide(sumHS);
                    m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].divide(sumHS);
                    m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].divide(sumHS);
                    m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].divide(sumHS);
                    m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].divide(sumHS);
                }
            }

            // Check if a student has Skills Center FTE
            BigDecimal sumSC = m_fte[INDEX_FTE_SKILLS_CENTER]
                    .add(m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q].add(m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ]));
            if (sumSC.compareTo(BigDecimal.ZERO) > 0) {
                // reduce sum of Skills Center FTE to 1
                if (sumSC.compareTo(BigDecimal.ONE) > 0) {
                    m_fte[INDEX_FTE_SKILLS_CENTER] = m_fte[INDEX_FTE_SKILLS_CENTER].divide(sumSC);
                    m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q] = m_fte[INDEX_FTE_SKILLS_CENTER_ALE_Q].divide(sumSC);
                    m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ] = m_fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ].divide(sumSC);
                    sumSC = BigDecimal.ONE;
                }

                // reduce sum of Remaining FTE to (1.6 - Skills Center FTE)
                BigDecimal lim = BigDecimal.valueOf(1.6).subtract(sumSC);
                BigDecimal sumHS = m_fte[INDEX_FTE_K12].add(m_fte[INDEX_FTE_HIGH_POVERTY].add(m_fte[INDEX_FTE_REMOTE]
                        .add(m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].add(m_fte[INDEX_FTE_ALE_NQ]))));
                if (sumHS.compareTo(lim) > 0) {
                    m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].divide(sumHS).multiply(lim);
                    m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].divide(sumHS).multiply(lim);
                    sumHS = lim;
                }

                // reduce sum of Remaining FTE to 1
                if (sumHS.compareTo(BigDecimal.ONE) > 0) {
                    m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].divide(sumHS);
                    m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].divide(sumHS);
                    m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].divide(sumHS);
                    m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].divide(sumHS);
                    m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].divide(sumHS);
                }
            }

            // Check if a student has Regular FTE > 1.0
            BigDecimal sumK12 = m_fte[INDEX_FTE_K12];
            if ((sumK12.compareTo(BigDecimal.ONE) > 0) &&
                    (sumSC.compareTo(BigDecimal.ZERO) <= 0) &&
                    (sumRS.compareTo(BigDecimal.ZERO) <= 0)) {
                // reduce sum of Regular FTE to 1
                m_fte[INDEX_FTE_K12] = m_fte[INDEX_FTE_K12].divide(sumK12);
                m_fte[INDEX_FTE_HIGH_POVERTY] = m_fte[INDEX_FTE_HIGH_POVERTY].divide(sumK12);
                m_fte[INDEX_FTE_REMOTE] = m_fte[INDEX_FTE_REMOTE].divide(sumK12);
                m_fte[INDEX_FTE_ALE_Q] = m_fte[INDEX_FTE_ALE_Q].divide(sumK12);
                m_fte[INDEX_FTE_ALE_NQ] = m_fte[INDEX_FTE_ALE_NQ].divide(sumK12);
            }
        }
    }

    /**
     * Field retriever for enrollment related information.
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private final DateFormat m_dateFormatter = DateFormat.getDateTimeInstance();
        private final String PARAM_REPORT_SCHOOL = "REPORT_SCHOOL";
        private final String PARAM_COUNTY_HOME = "COUNTY_HOME";
        private final String PARAM_COUNTY_SERVE = "COUNTY_SERVE";
        private final String PARAM_DATE_GENERATION = "GENERATION_DATE";
        private final String PARAM_DATE_REPORT = "REPORT_DATE";
        private final String PARAM_DIST_HOME = "DIST_HOME";
        private final String PARAM_DIST_HOME_CODE = "DIST_HOME_CODE";
        private final String PARAM_DIST_SERVE = "DIST_SERVE";
        private final String PARAM_DIST_SERVE_CODE = "DIST_SERVE_CODE";
        private final String PARAM_DIST_SERVE_ESD = "DIST_SERVE_ESD";
        private final String PARAM_SCHOOL_NAME = "SCHOOL_NAME";
        private final String VALUE_SCHOOL_CHOICE_1 = "1";
        private final String VALUE_SCHOOL_CHOICE_2 = "2";
        private final String VALUE_SCHOOL_CHOICE_3 = "3";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            StudentEnrollmentSpan span = ((WABasicSupportEntity) entity).getEnrollmentSpan();
            Object value = null;

            // Lookup school choice option.
            boolean schoolChoice = false;
            String schoolChoiceStr =
                    (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldSchoolChoice);
            schoolChoiceStr = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldSchoolChoice,
                    schoolChoiceStr,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (VALUE_SCHOOL_CHOICE_1.equals(schoolChoiceStr) ||
                    VALUE_SCHOOL_CHOICE_2.equals(schoolChoiceStr) ||
                    VALUE_SCHOOL_CHOICE_3.equals(schoolChoiceStr)) {
                schoolChoice = true;
            }

            if (PARAM_COUNTY_HOME.equals(parameter)) {
                String countyHome =
                        (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldDistrictHome);
                if (!StringUtils.isEmpty(countyHome)) {
                    countyHome = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldDistrictHome,
                            countyHome,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                }
                if (StringUtils.isEmpty(countyHome) || schoolChoice) {
                    countyHome = (String) getOrganization().getFieldValueByBeanPath(m_fieldCounty);
                }
                value = countyHome;
            } else if (PARAM_SCHOOL_NAME.equals(parameter)) {
                StudentEnrollment studentEnrollment =
                        span.getEnrollmentForDate(new PlainDate(m_reportDate), StudentEnrollment.ENTRY);
                if (studentEnrollment != null) {
                    value = studentEnrollment.getSchool().getName();
                }
            } else if (PARAM_DIST_HOME.equals(parameter)) {
                String districtHome =
                        (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldDistrictHome);
                if (StringUtils.isEmpty(districtHome) || schoolChoice) {
                    districtHome = getOrganization().getName();
                }
                value = districtHome;
            } else if (PARAM_DIST_HOME_CODE.equals(parameter)) {
                String districtHome =
                        (String) span.getFirstActiveEnrollment().getFieldValueByBeanPath(m_fieldDistrictHome);
                if (!StringUtils.isEmpty(districtHome)) {
                    districtHome = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldDistrictHome,
                            districtHome,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (StringUtils.isEmpty(districtHome) || schoolChoice) {
                    districtHome = m_districtId;
                }
                value = districtHome;
            } else if (PARAM_COUNTY_SERVE.equals(parameter)) {
                value = getOrganization().getFieldValueByBeanPath(m_fieldCounty);
            } else if (PARAM_DIST_SERVE.equals(parameter)) {
                value = getOrganization().getName();
            } else if (PARAM_DIST_SERVE_CODE.equals(parameter)) {
                value = m_districtId;
            } else if (PARAM_DIST_SERVE_ESD.equals(parameter)) {
                value = getOrganization().getFieldValueByAlias(ALIAS_DISTRICT_DOE_ESD);
            } else if (PARAM_DATE_REPORT.equals(parameter)) {
                value = m_dateFormatter.format(m_reportDate);
            } else if (PARAM_DATE_GENERATION.equals(parameter)) {
                value = m_dateFormatter.format(m_currentDate);
            } else if (PARAM_REPORT_SCHOOL.equals(parameter)) {
                if (isSchoolContext()) {
                    value = getSchool().getName();
                }
            }

            return value;
        }
    }

    /**
     * Field retriever to report FTE accumulators for the student.
     */
    protected class RetrieveFTE implements FieldRetriever {
        private final DecimalFormat m_formatter = new DecimalFormat(FORMAT_PATTERN);
        private final String PARAM_ALE_NQ = "ALE_NQ";
        private final String PARAM_ALE_Q = "ALE_Q";
        private final String PARAM_HEADCOUNT = "HEADCOUNT";
        private final String PARAM_HIGH_POVERTY = "HIGH_POVERTY";
        private final String PARAM_K12 = "K12";
        private final String PARAM_NON_VOC_RS = "NON_VOC_RS";
        private final String PARAM_OPEN_DOORS = "OPEN_DOORS";
        private final String PARAM_OPEN_DOORS_VOC = "OPEN_DOORS_VOC";
        private final String PARAM_REMOTE = "REMOTE";
        private final String PARAM_SKILLS_CENTER = "SKILLS_CENTER";
        private final String PARAM_SKILLS_CENTER_ALE_NQ = "SKILLS_CENTER_ALE_NQ";
        private final String PARAM_SKILLS_CENTER_ALE_Q = "SKILLS_CENTER_ALE_Q";
        private final String PARAM_VOC_78 = "VOC_78";
        private final String PARAM_VOC_78_ALE_NQ = "VOC_78_ALE_NQ";
        private final String PARAM_VOC_78_ALE_Q = "VOC_78_ALE_Q";
        private final String PARAM_VOC_912 = "VOC_912";
        private final String PARAM_VOC_912_ALE_NQ = "VOC_912_ALE_NQ";
        private final String PARAM_VOC_912_ALE_Q = "VOC_912_ALE_Q";
        private final String PARAM_VOC_RS = "VOC_RS";
        private final String PARAM_K12_AND_SC = "K12+SC";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            Object value = null;
            BigDecimal[] fte = ((WABasicSupportEntity) entity).getFte();

            if (PARAM_VOC_RS.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_RS];
            } else if (PARAM_NON_VOC_RS.equals(parameter)) {
                value = fte[INDEX_FTE_NON_VOC_RS];
            } else if (PARAM_VOC_78.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_78];
            } else if (PARAM_VOC_78_ALE_Q.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_78_ALE_Q];
            } else if (PARAM_VOC_78_ALE_NQ.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_78_ALE_NQ];
            } else if (PARAM_VOC_912.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_912];
            } else if (PARAM_VOC_912_ALE_Q.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_912_ALE_Q];
            } else if (PARAM_VOC_912_ALE_NQ.equals(parameter)) {
                value = fte[INDEX_FTE_VOC_912_ALE_NQ];
            } else if (PARAM_SKILLS_CENTER.equals(parameter)) {
                value = fte[INDEX_FTE_SKILLS_CENTER];
            } else if (PARAM_SKILLS_CENTER_ALE_Q.equals(parameter)) {
                value = fte[INDEX_FTE_SKILLS_CENTER_ALE_Q];
            } else if (PARAM_SKILLS_CENTER_ALE_NQ.equals(parameter)) {
                value = fte[INDEX_FTE_SKILLS_CENTER_ALE_NQ];
            } else if (PARAM_K12.equals(parameter)) {
                value = fte[INDEX_FTE_K12];
            } else if (PARAM_HIGH_POVERTY.equals(parameter)) {
                value = fte[INDEX_FTE_HIGH_POVERTY];
            } else if (PARAM_REMOTE.equals(parameter)) {
                value = fte[INDEX_FTE_REMOTE];
            } else if (PARAM_ALE_Q.equals(parameter)) {
                value = fte[INDEX_FTE_ALE_Q];
            } else if (PARAM_ALE_NQ.equals(parameter)) {
                value = fte[INDEX_FTE_ALE_NQ];
            } else if (PARAM_HEADCOUNT.equals(parameter)) {
                value = fte[INDEX_FTE_HEADCOUNT];
            } else if (PARAM_K12_AND_SC.equals(parameter)) {
                value = fte[INDEX_FTE_K12_AND_SC];
            } else if (PARAM_OPEN_DOORS.equals(parameter)) {
                value = fte[INDEX_FTE_OPEN_DOORS];
            } else if (PARAM_OPEN_DOORS_VOC.equals(parameter)) {
                value = fte[INDEX_FTE_OPEN_DOORS_VOC];
            }
            if (value != null) {
                return m_formatter.format(value);
            }
            return value;
        }
    }

    /**
     *
     * Field retriever to test for Poverty School inclusion.
     */
    protected class RetrievePoverty implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WABasicSupport bsData = (WABasicSupport) data;
            String grade = data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL,
                    ((SisStudent) entity.getBean()).getGradeLevel(),
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if ((grade != null) && grade.matches(MATCH_GRADE_K3)) {
                String schoolValue = null;
                StudentEnrollmentSpan span = ((WABasicSupportEntity) entity).getEnrollmentSpan();
                StudentEnrollment studentEnrollment =
                        span.getEnrollmentForDate(new PlainDate(m_reportDate), StudentEnrollment.ENTRY);
                if (studentEnrollment != null && studentEnrollment.getSchool() != null) {
                    schoolValue = (String) studentEnrollment.getSchool()
                            .getFieldValueByBeanPath(bsData.m_fieldSklIsHighPoverty);
                }
                if ((schoolValue != null) && BooleanAsStringConverter.TRUE.equals(schoolValue)) {
                    return BooleanAsStringConverter.TRUE;
                }
            }
            return BooleanAsStringConverter.FALSE;
        }
    }

    /**
     *
     * Field retriever to gather program indicators.
     */
    protected class RetrieveProgram implements FieldRetriever {
        private static final String CODE_STATE_BILINGUAL = "1";
        private final String PARAM_BILINGUAL = "BILINGUAL";
        private final String PARAM_COLLEGE_RS = "COLLEGE_RS";
        private final String PARAM_ELL_EXIT = "ELL_EXIT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = BooleanAsStringConverter.FALSE;
            String param = (String) field.getParameter();
            Collection<StudentProgramParticipation> studentProgramParticipations = null;
            int exitedPrograms = 0;

            if (PARAM_BILINGUAL.equals(param) || PARAM_ELL_EXIT.equals(param)) {
                studentProgramParticipations = ((WABasicSupportEntity) entity).getProgramParticipations();
            }

            if (PARAM_COLLEGE_RS.equals(param)) {
                studentProgramParticipations = m_studentCollegeRSProgramMap.get(entity.getBean().getOid());
                value = BooleanAsStringConverter.FALSE;
            }

            if (studentProgramParticipations != null) {
                Iterator<StudentProgramParticipation> i_studentProgramParticipations =
                        studentProgramParticipations.iterator();

                while (i_studentProgramParticipations.hasNext()) {
                    StudentProgramParticipation studentProgramParticipation = i_studentProgramParticipations.next();
                    if (PARAM_BILINGUAL.equals(param)) {
                        String programValue = (String) studentProgramParticipation
                                .getFieldValueByBeanPath(m_fieldLepProgramDesignation);
                        String code = data.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                                m_fieldLepProgramDesignation,
                                programValue,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if (CODE_STATE_BILINGUAL.equals(code)) {
                            value = BooleanAsStringConverter.TRUE;
                        }
                    }
                    if (PARAM_COLLEGE_RS.equals(param)) {
                        String indicator =
                                (String) studentProgramParticipation.getFieldValueByBeanPath(m_fieldCollegeRS);
                        if (BooleanAsStringConverter.TRUE.equals(indicator)) {
                            value = BooleanAsStringConverter.TRUE;
                            break;
                        }
                    }
                    if (PARAM_ELL_EXIT.equals(param)) {
                        if ((studentProgramParticipation.getEndDate() != null) &&
                                studentProgramParticipation.getEndDate().before(m_reportDate)) {
                            exitedPrograms++;
                        }
                    }
                }
            }

            if (PARAM_ELL_EXIT.equals(param)) {
                value = Integer.valueOf(exitedPrograms);
            }

            return value;
        }
    }

    /**
     * Field retriever need for taking values from ExportFormatRow,
     * in case when procedure run on export result data.
     */
    protected class RetrieveResultField implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = EMPTY_STRING;
            String beanPath = field.getSaveBeanPath();
            if (beanPath != null && !beanPath.isEmpty()) {
                value = entity.getBean().getFieldValueByBeanPath(beanPath);
            }
            return value;
        }
    }

    /**
     * Field retriver for getting fields from student bean.
     */
    protected class RetrieveStudentField implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = EMPTY_STRING;
            String beanPath = field.getBeanPath();
            value = entity.getBean().getFieldValueByBeanPath(beanPath);
            return value;
        }
    }

    /**
     * Validate the student has an FTE of 1.0 or greater, except for half day kindergarten grades.
     */
    protected class ValidateFTE implements FieldValidator {
        private static final String GRADE_K2 = "K2";
        private static final String STRING_BRACKET = "]";
        private static final String STRING_EMPTY = "";
        private static final String STRING_FTE_LESS_THAN_1_0 = " FTE less than 1.0";
        private static final String STRING_GL = " GL: ";
        private static final String STRING_INVALID_FTE_VALUE = "Invalid FTE value";

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
            List<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            String entityString = entity.toString();
            if ((entityString != null) && !entityString.isEmpty()) {
                entityString =
                        entityString.substring(0, entityString.length() - 2) + STRING_GL + student.getGradeLevel() +
                                STRING_BRACKET;
            }
            if (StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entityString, field.getFieldId(),
                        param + STRING_FTE_LESS_THAN_1_0, STRING_EMPTY));
            } else {
                float fte = 0;
                String gradeLevel = student.getGradeLevel();
                gradeLevel = lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                try {
                    fte = Float.parseFloat(value);
                } catch (NumberFormatException nfe) {
                    errors.add(new StateReportValidationError(entity, field, STRING_INVALID_FTE_VALUE, value));
                }
                if ((fte < 1) && !GRADE_K2.equals(gradeLevel)) {
                    errors.add(new StateReportValidationError(entityString, field.getFieldId(),
                            param + STRING_FTE_LESS_THAN_1_0, value));
                }
            }
            return errors;
        }
    }

    /*
     * Constants: Aliases, Codes, Parameters
     */
    protected static final String ALIAS_BASIC_SUP_EXT_DATE = "DOE BASIC SUP EXT DATE";
    protected static final String ALIAS_COLLEGE_RS = "DOE COLLEGE RS";
    protected static final String ALIAS_COUNTY = "DOE COUNTY";
    protected static final String ALIAS_COURSE_ALE_CAT = "DOE COURSE ALE CAT";
    protected static final String ALIAS_COURSE_FTE = "DOE COURSE FTE";
    protected static final String ALIAS_COURSE_FTE_TYPE = "DOE COURSE FTE TYPE";
    protected static final String ALIAS_DISTRICT_HOME = "DOE DISTRICT HOME";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DISTRICT_SERVE = "DOE DISTRICT SERVE";
    protected static final String ALIAS_DISTRICT_DOE_ESD = "DOE ESD";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXC_SKL_IN_P223 = "DOE EXCLUDE SCHOOL IN P223";
    protected static final String ALIAS_FTE_OVR_K12 = "DOE FTE OVR K12";
    protected static final String ALIAS_FTE_OVR_RS = "DOE FTE OVR RS";
    protected static final String ALIAS_FTE_OVR_RS_VOC = "DOE FTE OVR RS VOC";
    protected static final String ALIAS_FTE_OVR_SC = "DOE FTE OVR SC";
    protected static final String ALIAS_FTE_OVR_VOC = "DOE FTE OVR VOC";
    protected static final String ALIAS_IS_STATE_LEP = "DOE IS STATE LEP";
    protected static final String ALIAS_LEP_PROGRAM_DESIGNATION = "DOE LEP PRG DESIGNATION";
    protected static final String ALIAS_SCHOOL_CHOICE = "DOE SCHOOL CHOICE";
    protected static final String ALIAS_SECTION_FTE = "DOE SECTION FTE";
    protected static final String ALIAS_SECTION_FTE_TYPE = "DOE SECTION FTE TYPE";
    protected static final String ALIAS_SKIP_P223_HEADCOUNT = "DOE SKIP HEADCOUNT IN P223";
    protected static final String ALIAS_SKL_IS_HIGH_POVERTY = "DOE SKL IS HIGH POVERTY";
    protected static final String ALIAS_SKL_IS_REMOTE = "DOE SKL IS REMOTE";

    protected static final String AUDIT_COURSE = "course";
    protected static final String AUDIT_DISTRICT_ID = "districtId";
    protected static final String AUDIT_FTE_CRS = "fteCrs";
    protected static final String AUDIT_FTE_MAX = "fteMax";
    protected static final String AUDIT_FTE_OVER = "fteOver";
    protected static final String AUDIT_NON_VOC_RS = "nonVocRs";
    protected static final String AUDIT_PERIOD = "period";
    protected static final String AUDIT_SCHOOL = "school";
    protected static final String AUDIT_SKILLS_CENTER = "skillsCenter";
    protected static final String AUDIT_VOC = "vocational";

    protected static final String CALC_P223_ENROLLMENT = "P223-ENROLLMENT";
    protected static final String CALC_P223_FTE = "P223-FTE";
    protected static final String CALC_P223_PGM = "P223-PGM";
    protected static final String CALC_P223_POVERTY = "P223-POVERTY";
    protected static final String CALC_P223_STUDENT = "P223-STUDENT";

    protected static final String CODE_ELL = "ELL";
    protected static final String CODE_FTE = "FTE";
    protected static final String CODE_OPEN_DOORS = "1418";
    protected static final String CODE_COLLEGE_RS = "RSF";
    protected static final String DEFAULT_ALE_CAT = "Not ALE";
    protected static final String DEFAULT_FTE_TYPE = "Regular School";
    protected static final char DELIMITER = '|';
    protected static final String FORMAT_PATTERN = "####0.00";

    protected static final int INDEX_FTE_ALE_NQ = 0;
    protected static final int INDEX_FTE_ALE_Q = 1;
    protected static final int INDEX_FTE_HIGH_POVERTY = 2;
    protected static final int INDEX_FTE_K12 = 3;
    protected static final int INDEX_FTE_NON_VOC_RS = 4;
    protected static final int INDEX_FTE_REMOTE = 5;
    protected static final int INDEX_FTE_SKILLS_CENTER = 6;
    protected static final int INDEX_FTE_SKILLS_CENTER_ALE_Q = 7;
    protected static final int INDEX_FTE_SKILLS_CENTER_ALE_NQ = 8;
    protected static final int INDEX_FTE_VOC_78 = 9;
    protected static final int INDEX_FTE_VOC_78_ALE_Q = 10;
    protected static final int INDEX_FTE_VOC_78_ALE_NQ = 11;
    protected static final int INDEX_FTE_VOC_912 = 12;
    protected static final int INDEX_FTE_VOC_912_ALE_Q = 13;
    protected static final int INDEX_FTE_VOC_912_ALE_NQ = 14;
    protected static final int INDEX_FTE_VOC_RS = 15;
    protected static final int INDEX_FTE_OPEN_DOORS = 16;
    protected static final int INDEX_FTE_OPEN_DOORS_VOC = 17;
    protected static final int INDEX_FTE_HEADCOUNT = 18;
    protected static final int INDEX_FTE_K12_AND_SC = 19;

    protected static final String MATCH_ALE_CAT_NOT_ALE = "^Not ALE$|^$";
    protected static final String MATCH_ALE_CAT_NOT_QUALIFIED = "^ALE not Qualified$";
    protected static final String MATCH_ALE_CAT_QUALIFIED = "^ALE Qualified$";

    protected static final String MATCH_FTE_TYPE_NON_VOC_RS = "^Non-vocational RS$";
    protected static final String MATCH_FTE_TYPE_REG_SCHOOL = "^Regular School$|^$";
    protected static final String MATCH_FTE_TYPE_SKILLS_CENTER = "^Skills Center$";
    protected static final String MATCH_FTE_TYPE_VOC_PROGRAM = "^Vocational Program$";
    protected static final String MATCH_FTE_TYPE_VOC_RS = "^Vocational RS$";
    protected static final String MATCH_FTE_TYPE_1 = MATCH_FTE_TYPE_REG_SCHOOL +
            "|" +
            MATCH_FTE_TYPE_VOC_PROGRAM +
            "|" +
            MATCH_FTE_TYPE_SKILLS_CENTER;

    // protected static final String MATCH_GRADE_K3 =
    // "^K2$|^1$|^01$|^2$|^02$|^3$|^03$";
    protected static final String MATCH_GRADE_K3 = "K1|K2|KF|KP|KA|01|1|02|2|03|3";
    protected static final String MATCH_GRADE_78 = "^7$|^07$|^8$|^08$";
    protected static final String MATCH_GRADE_912 = "^9$|^09$|^10$|^11$|^12$";
    // Some state code grades are currently set to single digit
    protected static final String INCLUDED_GRADES = "K1|K2|1|2|3|4|5|6|7|8|9|01|02|03|04|05|06|07|08|09|10|11|12";

    protected static final int NUM_CNT = 8;
    protected static final int NUM_DAYS = 21;
    protected static final int NUM_FTE = 20;

    protected static final String PARAM_AUDIT_PROCEDURE_ID = "auditProcedureId";
    protected static final String PARAM_CHECK_ATTENDANCE = "checkAttendance";
    protected static final String PARAM_CURRENT_DATE = "currentDate";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_FROM_EXPORT_RESULT = "fromExportResult";
    protected static final String PARAM_INCLUDE_ZERO_VALUES = "omitZeroes";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_RESULT_OID = "efrOid";

    protected static final String STRING_ALL = "##all";

    /*
     * Instance variables.
     */
    protected Boolean m_checkAttendance;
    protected Date m_currentDate;
    protected String m_districtId;
    protected String m_fieldBasicSupExtDate;
    protected String m_fieldCollegeRS;
    protected String m_fieldCounty;
    protected String m_fieldCourseAleCat;
    protected String m_fieldCourseFte;
    protected String m_fieldCourseFteType;
    protected String m_fieldDistrictHome;
    protected String m_fieldDistrictId;
    protected String m_fieldDistrictServe;
    protected String m_fieldFTEK12;
    protected String m_fieldFTERS;
    protected String m_fieldFTERSV;
    protected String m_fieldFTESC;
    protected String m_fieldFTEVOC;
    protected String m_fieldLepProgramDesignation;
    protected String m_fieldSchoolChoice;
    protected String m_fieldSectionFte;
    protected String m_fieldSectionFteType;
    protected String m_fieldSklIsHighPoverty;
    protected String m_fieldSklIsRemote;
    protected String m_excludeSchool;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;

    protected Map<String, Collection<StudentProgramParticipation>> m_studentELLProgramMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentFTEProgramMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentOpenDoorsProgramMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentCollegeRSProgramMap;
    protected Boolean m_useExportResult = Boolean.FALSE;
    protected Boolean m_includeZeroKTwelveAndSkills = Boolean.FALSE;

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
        initializeFields();
        if (getParameter(PARAM_INCLUDE_ZERO_VALUES) != null) {
            m_includeZeroKTwelveAndSkills = (Boolean) getParameter(PARAM_INCLUDE_ZERO_VALUES);
        }
        Boolean useExportResult = (Boolean) getParameter(PARAM_FROM_EXPORT_RESULT);
        setUseExportResults(useExportResult);

        if (getSetupErrors().size() == 0) {
            setEntityClass(WABasicSupportEntity.class);
            if (isUseExportResults()) {
                String efrOid = (String) getParameter(PARAM_RESULT_OID);
                String queryBy = (String) getParameter(PARAM_QUERY_BY_FIELD + Integer.toString(1));
                String queryString = (String) getParameter(PARAM_QUERY_BY_CRITERIA + Integer.toString(1));
                if (queryBy != null && !STRING_ALL.equals(queryBy)) {
                    setQuery(getQueryForExportResultRows(efrOid, queryBy, queryString));
                } else {
                    setQuery(getQueryForExportResultRows(efrOid));
                }

                // Build a map of calculations/retrievers
                // in case then module work with export results data, we need
                // special retriever.
                RetrieveResultField retriever = new RetrieveResultField();
                Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
                calcs.put(CALC_P223_ENROLLMENT, retriever);
                calcs.put(CALC_P223_FTE, retriever);
                calcs.put(CALC_P223_PGM, retriever);
                calcs.put(CALC_P223_POVERTY, retriever);
                calcs.put(CALC_P223_STUDENT, retriever);
                super.addCalcs(calcs);
            }
            // If report not use export results
            else {
                /*
                 * Build helper object.
                 */
                m_helper = new StudentHistoryHelper(this);
                m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

                // Set the query to be used for student selection.
                if ((getParameter(PARAM_EXCLUDE_SCHOOL) != null) &&
                        ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
                    m_helper.getStudentCriteria().addNotEqualTo(Student.REL_SCHOOL
                            + PATH_DELIMITER + m_excludeSchool,
                            BooleanAsStringConverter.TRUE);
                }
                List<String> gradesIncluded = getIncludedGradeLevels();
                m_helper.getStudentCriteria().addIn(Student.COL_GRADE_LEVEL, gradesIncluded);

                setQuery(m_helper.getStudentQuery(false));

                m_studentELLProgramMap = loadProgramsParticipations(ReferenceCode.COL_LOCAL_CODE, CODE_ELL);
                m_studentFTEProgramMap = loadProgramsParticipations(ReferenceCode.COL_LOCAL_CODE, CODE_FTE);
                m_studentOpenDoorsProgramMap =
                        loadProgramsParticipations(ReferenceCode.COL_STATE_CODE, CODE_OPEN_DOORS);
                m_studentCollegeRSProgramMap =
                        loadProgramsParticipations(ReferenceCode.COL_LOCAL_CODE, CODE_COLLEGE_RS);
                // Build a map of calculations/retrievers
                Map<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
                calcs.put(CALC_P223_ENROLLMENT, new RetrieveEnrollment());
                calcs.put(CALC_P223_FTE, new RetrieveFTE());
                calcs.put(CALC_P223_PGM, new RetrieveProgram());
                calcs.put(CALC_P223_POVERTY, new RetrievePoverty());
                calcs.put(CALC_P223_STUDENT, new RetrieveStudentField());
                super.addCalcs(calcs);
            }
            Map<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(CALC_P223_FTE, new ValidateFTE());
            super.addValidators(validators);
        }
    }

    /**
     * Build query for Export Result Rows.
     * If no last Result, return empty query.
     *
     * @param efrOid String
     * @return Query by criteria
     */
    protected QueryByCriteria getQueryForExportResultRows(String efrOid) {
        if (efrOid != null) {
            Criteria expResultCriteria = new Criteria();
            expResultCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, efrOid);
            return new QueryByCriteria(ExportFormatRow.class, expResultCriteria);
        }
        // Build empty query, that's need to prevent creation of
        // m_query in parent class.
        Criteria emptyCriteria = new Criteria();
        emptyCriteria.addIsNull(X2BaseBean.COL_OID);
        return new QueryByCriteria(ExportFormatRow.class, emptyCriteria);
    }

    /**
     * Build query for Export Result Rows if queryBy1 different from ##all.
     * If no last Result, return empty query.
     *
     * @param efrOid String
     * @param queryBy String
     * @param queryString String
     * @return Query by criteria
     */
    protected QueryByCriteria getQueryForExportResultRows(String efrOid, String queryBy, String queryString) {
        HashMap<String, ExportFormatField> m_auditMapping = new HashMap<String, ExportFormatField>();
        String auditProcedureId = (String) getParameter(PARAM_AUDIT_PROCEDURE_ID);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExportFormatField.REL_DEFINITION + PATH_DELIMITER + ExportFormatDefinition.COL_PROCEDURE_ID,
                auditProcedureId);
        QueryByCriteria query = new QueryByCriteria(ExportFormatField.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                ExportFormatField field = (ExportFormatField) iterator.next();
                m_auditMapping.put(field.getName(), field);
            }
        } finally {
            iterator.close();
        }
        if (efrOid != null) {
            Criteria expResultCriteria = new Criteria();
            expResultCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, efrOid);
            ExportFormatField field = m_auditMapping.get(queryBy);
            if (field != null) {
                expResultCriteria.addEqualTo(field.getFieldPath(), queryString);
            }
            return new QueryByCriteria(ExportFormatRow.class, expResultCriteria);
        }
        // Build empty query, that's need to prevent creation of
        // m_query in parent class.
        Criteria emptyCriteria = new Criteria();
        emptyCriteria.addIsNull(X2BaseBean.COL_OID);
        return new QueryByCriteria(ExportFormatRow.class, emptyCriteria);
    }

    /**
     * Returns the StudentProgramParticipation Open Doors records for the student.
     *
     * @param student SisStudent
     * @return List containing the records for the student
     */
    protected Collection<StudentProgramParticipation> getStudentOpenDoorsPrograms(SisStudent student) {
        return m_studentOpenDoorsProgramMap.get(student.getOid());
    }

    /**
     * Returns the StudentProgramParticipation records for the student.
     *
     * @param student SisStudent
     * @return List containing the records for the student
     */
    protected Collection<StudentProgramParticipation> getStudentPrograms(SisStudent student) {
        return m_studentELLProgramMap.get(student.getOid());
    }

    /**
     * True if procedure was configured to include zero k12 and skills.
     *
     * @return true, if is include zero K twelve and skills
     */
    protected boolean isIncludeZeroKTwelveAndSkills() {
        return m_includeZeroKTwelveAndSkills.booleanValue();
    }

    /**
     * True if procedure was configured to use export result data.
     *
     * @return true, if is use export results
     */
    protected boolean isUseExportResults() {
        return m_useExportResult.booleanValue();
    }

    /**
     * Setter for m_useExportResult.
     *
     * @param useExportResults void
     */
    protected void setUseExportResults(Boolean useExportResults) {
        if (useExportResults != null) {
            m_useExportResult = useExportResults;
        }
    }

    /**
     * Gets the included grade levels.
     *
     * @return List
     */
    private List<String> getIncludedGradeLevels() {
        List<String> listGrades = new LinkedList<String>();
        List<String> includedGrades = StringUtils.convertDelimitedStringToList(INCLUDED_GRADES, DELIMITER);
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        if ((field != null) && field.hasReferenceTable()) {
            ReferenceTable referenceTable = field.getReferenceTable();
            if (referenceTable != null) {
                for (Entry<String, ReferenceCode> entry : referenceTable.getCodeMap().entrySet()) {
                    if (includedGrades.contains((entry.getValue().getStateCode()))) {
                        listGrades.add(entry.getKey());
                    }
                }
            }
        }
        return listGrades;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_currentDate = new Date(System.currentTimeMillis());
        m_checkAttendance = (Boolean) getParameter(PARAM_CHECK_ATTENDANCE);
        if (m_checkAttendance == null) {
            m_checkAttendance = Boolean.TRUE;
        }

        m_fieldBasicSupExtDate = translateAliasToJavaName(ALIAS_BASIC_SUP_EXT_DATE, true);
        m_fieldCounty = translateAliasToJavaName(ALIAS_COUNTY, true);
        m_fieldCourseFte = translateAliasToJavaName(ALIAS_COURSE_FTE, true);
        m_fieldCourseFteType = translateAliasToJavaName(ALIAS_COURSE_FTE_TYPE, true);
        m_fieldCourseAleCat = translateAliasToJavaName(ALIAS_COURSE_ALE_CAT, true);
        m_fieldCollegeRS = translateAliasToJavaName(ALIAS_COLLEGE_RS, true);
        m_fieldDistrictHome = translateAliasToJavaName(ALIAS_DISTRICT_HOME, true);
        m_fieldDistrictId = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldDistrictServe = translateAliasToJavaName(ALIAS_DISTRICT_SERVE, true);
        m_fieldSectionFte = translateAliasToJavaName(ALIAS_SECTION_FTE, true);
        m_fieldSectionFteType = translateAliasToJavaName(ALIAS_SECTION_FTE_TYPE, true);
        m_fieldSchoolChoice = translateAliasToJavaName(ALIAS_SCHOOL_CHOICE, true);
        m_fieldSklIsHighPoverty = translateAliasToJavaName(ALIAS_SKL_IS_HIGH_POVERTY, true);
        m_fieldSklIsRemote = translateAliasToJavaName(ALIAS_SKL_IS_REMOTE, true);
        m_fieldLepProgramDesignation = translateAliasToJavaName(ALIAS_LEP_PROGRAM_DESIGNATION, true);

        m_fieldFTEK12 = translateAliasToJavaName(ALIAS_FTE_OVR_K12, true);
        m_fieldFTERS = translateAliasToJavaName(ALIAS_FTE_OVR_RS, true);
        m_fieldFTERSV = translateAliasToJavaName(ALIAS_FTE_OVR_RS_VOC, true);
        m_fieldFTESC = translateAliasToJavaName(ALIAS_FTE_OVR_SC, true);
        m_fieldFTEVOC = translateAliasToJavaName(ALIAS_FTE_OVR_VOC, true);

        m_districtId = (String) getOrganization().getFieldValueByBeanPath(m_fieldDistrictId);

        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }

    /**
     * Load a map of student program participation records.
     *
     * @param referenceCode String
     * @param programCode String
     * @return HashMap
     */
    private HashMap<String, Collection<StudentProgramParticipation>> loadProgramsParticipations(String referenceCode,
                                                                                                String programCode) {
        X2Criteria codesCriteria = null;
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            codesCriteria = new X2Criteria();
            codesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            codesCriteria.addEqualTo(referenceCode, programCode);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, codesCriteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }
        if ((reportableCodes != null) && (reportableCodes.size() > 0)) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

            X2Criteria endDateCriteria = new X2Criteria();
            endDateCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

            X2Criteria endDateOrCriteria = new X2Criteria();
            endDateOrCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getOrganization().getCurrentContext().getStartDate());
            endDateCriteria.addOrCriteria(endDateOrCriteria);
            programCriteria.addAndCriteria(endDateCriteria);
            if (isSchoolContext()) {
                programCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                programCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }
            applyInputCriteria(programCriteria, false, StudentProgramParticipation.REL_STUDENT);

            QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
            programQuery.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
            return (HashMap<String, Collection<StudentProgramParticipation>>) getBroker().getGroupedCollectionByQuery(
                    programQuery,
                    StudentProgramParticipation.COL_STUDENT_OID,
                    500);
        }
        return new HashMap<String, Collection<StudentProgramParticipation>>();
    }
}
