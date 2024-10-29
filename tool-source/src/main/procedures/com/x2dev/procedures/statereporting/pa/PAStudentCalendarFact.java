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
package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.pa.PAStudentCalendarFact.PAStudentCalendarFactEntity.EntityRow;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.PAChildCommonData;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.PAChildWorker;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpanDefaultBehavior;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpanPACF;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.WorkerSubSpanFabric;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.joda.time.LocalDate;
import org.joda.time.Years;

/**
 * The Class PAStudentCalendarFact.
 */
public class PAStudentCalendarFact extends StateReportData {
    private static final String STDC_CALC_HOMEB = "STDC_CALC_HOMEB";

    /**
     * This helper class is used to store spans with matching grade
     * level and residence status, since this is the criteria that
     * distinguishes between the rows in the report.
     */
    public static class PAStudentCalendarFactEntity extends StateReportEntity {

        /**
         * The Class EntityRow.
         */
        class EntityRow implements Comparable<EntityRow> {


            private String m_calendarId;
            private String m_districtResidenceCode;
            private String m_fundingDistrictCode;
            private String m_gradeLevel;
            private String m_percentage;
            private String m_residenceStatus;
            private SisSchool m_school;
            private String m_specialEdIndicator;
            private List<SubSpan<PAChildWorker>> m_spans = new ArrayList<SubSpan<PAChildWorker>>();
            private List<StateReportValidationError> m_validationErrors;

            /**
             * Instantiates a new entity row.
             *
             * @param data PAStudentCalendarFact
             * @param worker PAChildWorker
             */
            public EntityRow(PAStudentCalendarFact data, PAChildWorker worker) {
                m_calendarId = worker.getCalendarId();
                m_districtResidenceCode = getStudentOrganizationOverride(worker.getStudent(), data.getOrganization(),
                        data.m_fieldDistrictCodeOfResidance);
                m_fundingDistrictCode = getStudentOrganizationOverride(worker.getStudent(), data.getOrganization(),
                        data.m_fieldFundingDistrictCode);
                m_gradeLevel = worker.getGradeLevel();
                m_percentage = worker.getPercentageForCalendar();
                m_residenceStatus = worker.getResidentStatus();
                if (StringUtils.isEmpty(m_residenceStatus)) {
                    m_residenceStatus = "INVALID";
                }
                m_school = worker.getSchool();
                m_specialEdIndicator = worker.getSpecialEdIndicator();
            }

            /**
             * Compare to.
             *
             * @param other EntityRow
             * @return int
             * @see java.lang.Comparable#compareTo(java.lang.Object)
             */
            @Override
            public int compareTo(EntityRow other) {
                return m_calendarId.compareTo(other.m_calendarId) == 0
                        ? m_districtResidenceCode.compareTo(other.m_districtResidenceCode) == 0
                                ? m_fundingDistrictCode.compareTo(other.m_fundingDistrictCode) == 0
                                        ? m_residenceStatus.compareTo(other.m_residenceStatus) == 0
                                                ? m_gradeLevel.compareTo(other.m_gradeLevel) == 0
                                                        ? m_specialEdIndicator
                                                                .compareTo(other.m_specialEdIndicator)
                                                        : m_gradeLevel.compareTo(other.m_gradeLevel)
                                                : m_residenceStatus.compareTo(other.m_residenceStatus)
                                        : m_fundingDistrictCode.compareTo(other.m_fundingDistrictCode)
                                : m_districtResidenceCode.compareTo(other.m_districtResidenceCode)
                        : m_calendarId.compareTo(other.m_calendarId);
            }

            /**
             * Equals.
             *
             * @param obj Object
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                EntityRow other = (EntityRow) obj;
                return other.m_calendarId.equals(m_calendarId) &&
                        other.m_districtResidenceCode.equals(m_districtResidenceCode) &&
                        other.m_fundingDistrictCode.equals(m_fundingDistrictCode) &&
                        other.m_residenceStatus.equals(m_residenceStatus) &&
                        other.m_gradeLevel.equals(m_gradeLevel) &&
                        other.m_specialEdIndicator.equals(m_specialEdIndicator);
            }

            /**
             * Gets the calendar code.
             *
             * @return String
             */
            public String getCalendarCode() {
                SchoolCalendar lastCalendar = null;
                PlainDate date = null;

                for (SubSpan<PAChildWorker> span : getSpans()) {
                    SchoolCalendar calendar = span.getWorker().getCalendar();
                    if (calendar != null) {
                        if (lastCalendar == null || date == null
                                || span.getFirstInSpanEnr().getEnrollmentDate().after(date)) {
                            lastCalendar = calendar;
                            date = span.getFirstInSpanEnr().getEnrollmentDate();
                        }

                    }
                }
                String value = null;
                if (lastCalendar != null) {
                    StringBuilder result = new StringBuilder();
                    result.append(lastCalendar.getSchool().getSchoolId())
                            .append("-")
                            .append(lastCalendar.getCalendarId());
                    value = result.toString();
                }
                return value;
            }

            /**
             * Gets the key.
             *
             * @return String
             */
            public String getKey() {
                StringBuilder key = new StringBuilder();
                key.append(m_calendarId);
                key.append(",");
                key.append(m_districtResidenceCode);
                key.append(",");
                key.append(m_fundingDistrictCode);
                key.append(",");
                key.append(m_residenceStatus);
                key.append(",");
                key.append(m_gradeLevel);
                key.append(",");
                key.append(m_specialEdIndicator);
                return null;
            }

            /**
             * Gets the validation errors.
             *
             * @return List
             */
            public List<StateReportValidationError> getValidationErrors() {
                if (m_validationErrors == null) {
                    m_validationErrors = new LinkedList();
                }
                return m_validationErrors;
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return m_calendarId.hashCode() + m_districtResidenceCode.hashCode() + m_fundingDistrictCode.hashCode()
                        + m_residenceStatus.hashCode() + m_gradeLevel.hashCode() + m_specialEdIndicator.hashCode();
            }

            /**
             * Checks for validation errors.
             *
             * @return true, if successful
             */
            public boolean hasValidationErrors() {
                if (m_spans.size() > 1) {
                    for (SubSpan<PAChildWorker> span : m_spans) {
                        if (!m_percentage.equals(span.getWorker().getPercentageForCalendar())) {
                            getValidationErrors().add(new StateReportValidationError(PAStudentCalendarFactEntity.this,
                                    PAStudentCalendarFactEntity.this.getData().getFieldDefinition(0),
                                    "Inconsistent Percentage Time Enrolled",
                                    "Percentages found are " + m_percentage + " and "
                                            + span.getWorker().getPercentageForCalendar()));
                            break;
                        }
                    }
                }
                return m_validationErrors != null && !m_validationErrors.isEmpty();
            }

            /**
             * Adds the span.
             *
             * @param span SubSpan<PAChildWorker>
             */
            void addSpan(SubSpan<PAChildWorker> span) {
                if (!m_percentage.equals(span.getWorker().getPercentageForCalendar())) {
                    // use percentage from last span
                    m_percentage = span.getWorker().getPercentageForCalendar();
                }
                m_spans.add(span);
            }

            /**
             * Gets the district residence code.
             *
             * @return Object
             */
            Object getDistrictResidenceCode() {
                return m_districtResidenceCode;
            }

            /**
             * Gets the funding district code.
             *
             * @return Object
             */
            Object getFundingDistrictCode() {
                return m_fundingDistrictCode;
            }

            /**
             * Gets the grade level.
             *
             * @return Object
             */
            Object getGradeLevel() {
                return m_gradeLevel;
            }

            /**
             * Gets the num days absent.
             *
             * @return double
             */
            double getNumDaysAbsent() {
                double absent = 0.0;
                for (SubSpan<PAChildWorker> span : getSpans()) {
                    absent += span.getWorker().getAbsentDays();
                }
                return absent;
            }

            /**
             * Gets the num days enrolled.
             *
             * @return int
             */
            int getNumDaysEnrolled() {
                int enrolled = 0;
                for (SubSpan<PAChildWorker> span : getSpans()) {
                    enrolled += span.getWorker().getMemberShipDaysInPeriod().size();
                }
                return enrolled;
            }

            /**
             * Gets the percentage.
             *
             * @return String
             */
            String getPercentage() {
                return m_percentage;
            }

            /**
             * Gets the residence status.
             *
             * @return String
             */
            String getResidenceStatus() {
                return m_residenceStatus;
            }

            /**
             * Gets the school.
             *
             * @return Sis school
             */
            SisSchool getSchool() {
                return m_school;
            }

            /**
             * Gets the spans.
             *
             * @return List
             */
            List<SubSpan<PAChildWorker>> getSpans() {
                return m_spans;
            }

            /**
             * Gets the special ed indicator.
             *
             * @return String
             */
            String getSpecialEdIndicator() {
                return m_specialEdIndicator;
            }

            /**
             * Gets the student organization override.
             *
             * @param student SisStudent
             * @param organization Organization
             * @param beanPath String
             * @return String
             */
            private String getStudentOrganizationOverride(SisStudent student,
                                                          Organization organization,
                                                          String beanPath) {
                String value = (String) student.getFieldValueByBeanPath(beanPath);
                if (StringUtils.isEmpty(value)) {
                    value = organization.getId();
                }
                return value;
            }

        }

        protected PAStudentCalendarFact m_sData;
        protected SisStudent m_student;

        private List<EntityRow> m_rows;

        /**
         * Instantiates a new PA student calendar fact entity.
         */
        public PAStudentCalendarFactEntity() {

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
                    "] " + getEntityRow().getKey();

            return name;
        }

        /**
         * Gets the entity row.
         *
         * @return Entity row
         */
        public EntityRow getEntityRow() {
            return m_rows.get(getCurrentRow());
        }

        /**
         * Gets the field validations.
         *
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValidations()
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidations() {
            Collection<StateReportValidationError> errors = super.getFieldValidations();
            if (getEntityRow().hasValidationErrors()) {
                errors.addAll(getEntityRow().getValidationErrors());
            }
            return errors;
        }

        /**
         * Processes the enrollment spans for the current year to determine the row count and
         * populate
         * the entity rows member.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_sData = (PAStudentCalendarFact) data;
            m_student = (SisStudent) getBean();

            Set<EntityRow> rows = new HashSet<EntityRow>();
            if (m_sData.m_splitSpan != null) {
                for (SubSpan<PAChildWorker> span : m_sData.m_splitSpan
                        .splitSpan(m_sData.m_helper.getStudentEnrollments(m_student), false)) {
                    if (span != null && !span.isSkipRecord()) {
                        PAChildWorker spanWorker = span.getWorker();
                        SisSchool curSchool = spanWorker.getSchool();
                        if (curSchool != null) {
                            EntityRow row = new EntityRow(m_sData, spanWorker);
                            if (!rows.add(row)) {
                                for (EntityRow test : rows) {
                                    if (test.equals(row)) {
                                        row = test;
                                        break;
                                    }
                                }
                            }
                            row.addSpan(span);
                        }
                    }
                }
            }
            m_rows = new ArrayList<EntityRow>();
            m_rows.addAll(rows);
            setRowCount(m_rows.size());
        }
    }

    /**
     * Fabric for creating PAChildWorker and SubSpanPACF.
     *
     * @author Follett Software Company
     */
    public class PACFWorkerFabric implements WorkerSubSpanFabric<PAChildWorker, SubSpanPACF> {
        PAChildCommonData m_commonData = null;
        SubSpanHelper m_helperWF;
        PAChildWorker m_worker = null;

        /**
         * Instantiates a new PACF worker fabric.
         *
         * @param commonData PAChildCommonData
         * @param helper SubSpanHelper
         */
        public PACFWorkerFabric(PAChildCommonData commonData, SubSpanHelper helper) {
            m_helperWF = helper;
            m_commonData = commonData;
        }

        /**
         * Creates the sub span.
         *
         * @param previousSpan SubSpanPACF
         * @return SubSpanPACF
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.WorkerSubSpanFabric#createSubSpan(com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan)
         */
        @Override
        public SubSpanPACF createSubSpan(SubSpanPACF previousSpan) {

            return m_helperWF.getSubSpanPACF(previousSpan, createWorker());
        }

        /**
         * Creates the worker.
         *
         * @return PAChildWorker
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.WorkerSubSpanFabric#createWorker()
         */
        @Override
        public PAChildWorker createWorker() {
            m_worker = m_helperWF.getPaChildWorker();
            m_worker.setCommonData(m_commonData);
            return m_worker;
        }
    }

    /**
     * The Class RetrieveAbsentUnexcused.
     */
    public class RetrieveAbsentUnexcused implements FieldRetriever {

        private static final String CALC_ID = "STDC_CALC_ABS_UNEXC";

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
            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentAttendance> absences = m_helper.getStudentAttendances(student.getOid());
            PlainDate dob = student.getPerson().getDob();
            double countDays = 0.0;
            if (absences != null) {
                for (StudentAttendance abs : absences) {
                    if (!abs.getExcusedIndicator()) {
                        if (abs != null && dob != null) {
                            LocalDate birthday = new LocalDate(dob);
                            LocalDate absDate = new LocalDate(abs.getDate());
                            int age = Years.yearsBetween(birthday, absDate).getYears();
                            if (age >= 5 && age <= 16) {
                                countDays = countDays + abs.getPortionAbsent().doubleValue();
                            }
                        }
                    }
                }
            }
            return String.valueOf(countDays);
        }
    }

    /**
     * The Class RetrieveCalendarId.
     */
    public class RetrieveCalendarId implements FieldRetriever {

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
            PAStudentCalendarFactEntity scfEntity = (PAStudentCalendarFactEntity) entity;
            EntityRow row = scfEntity.getEntityRow();
            return row.getCalendarCode();
        }
    }

    /**
     * The Class RetrieveDaysEnrolled.
     */
    public class RetrieveDaysEnrolled implements FieldRetriever {

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
            EntityRow row = ((PAStudentCalendarFactEntity) entity).getEntityRow();
            return Integer.valueOf(row.getNumDaysEnrolled());
        }
    }

    /**
     * The Class RetrieveDaysPresent.
     */
    public class RetrieveDaysPresent implements FieldRetriever {

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
            EntityRow row = ((PAStudentCalendarFactEntity) entity).getEntityRow();
            double enrolled = row.getNumDaysEnrolled();
            double absent = row.getNumDaysAbsent();
            return new BigDecimal(enrolled - absent);
        }
    }

    /**
     * The Class RetrieveFunding.
     */
    public class RetrieveFunding implements FieldRetriever {

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
            PAStudentCalendarFactEntity scfEntity = (PAStudentCalendarFactEntity) entity;
            return scfEntity.getEntityRow().getFundingDistrictCode();
        }
    }

    /**
     * The Class RetrieveGradeLevel.
     */
    public class RetrieveGradeLevel implements FieldRetriever {

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
            EntityRow row = ((PAStudentCalendarFactEntity) entity).getEntityRow();
            return row.getGradeLevel();
        }
    }

    /**
     * The Class RetrieveHomeboundInstr.
     */
    public class RetrieveHomeboundInstr implements FieldRetriever {
        private static final String PROGRAM_CODE_013 = "013";

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

            PAStudentCalendarFact scfData = (PAStudentCalendarFact) data;
            SisStudent student = (SisStudent) entity.getBean();

            String intensity = (String) student.getFieldValueByAlias(ALIAS_HOMEINSTRUCTION);

            Collection<StudentProgramParticipation> programs = student.getProgramParticipation();

            for (StudentProgramParticipation program : programs) {
                if (program.getProgramCode() != null && program.getProgramCode().equals(PROGRAM_CODE_013) &&
                        !program.getStartDate().after(scfData.m_reportDate) &&
                        (program.getEndDate() == null || !program.getEndDate().before(scfData.m_startDate))) {
                    intensity = (String) program.getFieldValueByAlias(ALIAS_INTENSITY);
                }
            }

            if (intensity != null) {
                value = new BigDecimal(intensity);
            }

            return value;
        }
    }

    /**
     * The Class RetrievePercentage.
     */
    public class RetrievePercentage implements FieldRetriever {

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
            PAStudentCalendarFactEntity scfEntity = (PAStudentCalendarFactEntity) entity;
            return scfEntity.getEntityRow().getPercentage();
        }
    }

    /**
     * The Class RetrieveResCode.
     */
    public class RetrieveResCode implements FieldRetriever {

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
            EntityRow row = ((PAStudentCalendarFactEntity) entity).getEntityRow();
            return row.getDistrictResidenceCode();
        }
    }

    /**
     * The Class RetrieveResidenceStatusCode.
     */
    public class RetrieveResidenceStatusCode implements FieldRetriever {

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
            EntityRow row = ((PAStudentCalendarFactEntity) entity).getEntityRow();
            return row.getResidenceStatus();
        }
    }

    /**
     * The Class RetrieveSchoolData.
     */
    public class RetrieveSchoolData implements FieldRetriever {
        private static final String PARAM_INSTR_CODE = "INSTR_CODE";
        private static final String PARAM_YEAR_DATE = "YEAR_DATE";

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

            PAStudentCalendarFactEntity scfEntity = (PAStudentCalendarFactEntity) entity;

            if (PARAM_INSTR_CODE.equals(field.getParameter())) {
                value = scfEntity.getEntityRow().getSchool().getOrganization1().getId();
            } else if (PARAM_YEAR_DATE.equals(field.getParameter())) {
                value = scfEntity.getEntityRow().getSchool().getOrganization1().getCurrentContext().getEndDate();
            }

            return value;
        }

    }

    /**
     * The Class RetrieveSEIndicator.
     */
    public class RetrieveSEIndicator implements FieldRetriever {

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
            EntityRow row = ((PAStudentCalendarFactEntity) entity).getEntityRow();
            return row.getSpecialEdIndicator();
        }
    }

    /**
     * The Class ValidateSendingCharter.
     */
    public class ValidateSendingCharter implements FieldValidator {

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

            String schoolCodeOfSendingCharter = entity.getFieldValue(12);
            String distCodeOfSendingCharter = entity.getFieldValue(13);
            if (StringUtils.isEmpty(schoolCodeOfSendingCharter) ^ StringUtils.isEmpty(distCodeOfSendingCharter)) {
                errors.add(new StateReportValidationError(entity, field, "This field must be populated",
                        "Both sending charter fields must be populated if either is populated"));
            }
            return errors;
        }
    }

    private static final String ALIAS_FUNDING = "DOE FUNDING";
    private static final String ALIAS_HOMEINSTRUCTION = "DOE HOME INSTRUCTION";
    private static final String ALIAS_INTENSITY = "DOE INTENSITY";
    private static final String ALIAS_RESIDENCE = "DOE DISTRICT RESIDENCE";
    private static final String ALIAS_RESSTATUS = "DOE RESIDENCE STATUS";
    private static final String ALIAS_SENDSCHOOL = "DOE SENDSCHOOL";

    private static final String CALC_ID_CALENDAR_ID = "STDC_CALC_CALENDR_ID";
    private static final String CALC_ID_CURGRADE = "STDC_CALC_CURGRADE";
    private static final String CALC_ID_DAYSENR = "STDC_CALC_DAYSENR";
    private static final String CALC_ID_DAYSPRES = "STDC_CALC_DAYSPRES";
    private static final String CALC_ID_DISTRESCODE = "STDC_CALC_DISTRESCOD";
    private static final String CALC_ID_FUNDING = "STDC_CALC_FUNDING";
    private static final String CALC_ID_PERC = "STDC_CALC_PERC";
    private static final String CALC_ID_RESSTATUS = "STDC_CALC_RESSTATUS";
    private static final String CALC_ID_SCHOOL = "STDC_CALC_SCHOOL";
    private static final String CALC_ID_SE_IND = "STDC_CALC_SE_IND";

    private static final String CYCLE_ONE = "01";

    private static final String PARAM_REPORT_DATE = "reportDate";

    private static final String VAL_ID_SENDCHART = "STDC_VAL_SENDCHART";

    protected List<DistrictCalendar> m_distCalendars;
    protected String m_fieldDistrictCodeOfResidance;
    protected String m_fieldFundingDistrictCode;
    protected String m_fieldHomeInstruction;
    protected String m_fieldResidence;
    protected String m_fieldResStatus;
    protected String m_fieldSendSchool;
    protected StudentHistoryHelper m_helper;
    protected Map<String, String> m_mostCommonCalendars;
    Map<String, Map<String, Collection<SchoolCalendar>>> m_mapSchoolCalendars;
    protected PlainDate m_reportDate;
    protected PlainDate m_startDate;
    protected SplitSpanDefaultBehavior<PAChildWorker, SubSpanPACF> m_splitSpan;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        initializeFields();
        if (getSetupErrors().size() == 0) {
            loadDistrictCals();

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);


            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(PAStudentCalendarFactEntity.class);
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_CALENDAR_ID, new RetrieveCalendarId());
            calcs.put(CALC_ID_CURGRADE, new RetrieveGradeLevel());
            calcs.put(CALC_ID_DAYSENR, new RetrieveDaysEnrolled());
            calcs.put(CALC_ID_DAYSPRES, new RetrieveDaysPresent());
            calcs.put(CALC_ID_FUNDING, new RetrieveFunding());
            calcs.put(CALC_ID_RESSTATUS, new RetrieveResidenceStatusCode());
            calcs.put(CALC_ID_SE_IND, new RetrieveSEIndicator());
            calcs.put(CALC_ID_DISTRESCODE, new RetrieveResCode());
            calcs.put(CALC_ID_PERC, new RetrievePercentage());
            calcs.put(CALC_ID_SCHOOL, new RetrieveSchoolData());
            calcs.put(STDC_CALC_HOMEB, new RetrieveHomeboundInstr());
            calcs.put(RetrieveAbsentUnexcused.CALC_ID, new RetrieveAbsentUnexcused());

            super.addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(VAL_ID_SENDCHART, new ValidateSendingCharter());
            super.addValidators(validators);
            SubSpanHelper subSpanHelper = new SubSpanHelper(m_helper, getOrganization(), getBroker());
            List<School> schools = new ArrayList<School>();

            if (isSchoolContext()) {
                schools.add(getSchool());
            }

            PAChildCommonData commonData = subSpanHelper.getPAChildCommonData(schools, Boolean.valueOf(true), null);
            PACFWorkerFabric fabric = new PACFWorkerFabric(commonData, subSpanHelper);
            m_splitSpan = subSpanHelper.getSplitSPanDefaultBehavior(fabric,
                    subSpanHelper.getRules(SubSpanHelper.SPLIT_RULES_PA_CHILD));

        }
    }


    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_fieldDistrictCodeOfResidance = translateAliasToJavaName(ALIAS_RESIDENCE, true);
        m_fieldFundingDistrictCode = translateAliasToJavaName(ALIAS_FUNDING, true);
        m_fieldHomeInstruction = translateAliasToJavaName(ALIAS_HOMEINSTRUCTION, true);
        m_fieldResidence = translateAliasToJavaName(ALIAS_RESIDENCE, true);
        m_fieldResStatus = translateAliasToJavaName(ALIAS_RESSTATUS, true);
        m_fieldSendSchool = translateAliasToJavaName(ALIAS_SENDSCHOOL, true);
    }

    /**
     * Get first day of school.
     */
    private void loadDistrictCals() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        criteria.addEqualTo(DistrictCalendar.COL_CYCLE, CYCLE_ONE);
        criteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, BooleanAsStringConverter.TRUE);
        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderBy(DistrictCalendar.COL_DATE, true);

        m_distCalendars = (List<DistrictCalendar>) getBroker().getCollectionByQuery(query);
        if (!m_distCalendars.isEmpty()) {
            m_startDate = m_distCalendars.get(0).getDate();
        }
    }
}
