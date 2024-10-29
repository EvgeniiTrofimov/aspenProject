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

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStaffMultiYearHelper;
import com.x2dev.procedures.statereporting.tn.TNStaffMemberClassAssignmentData.TNStaffMemberClassAssignmentEntity.TeacherStatus;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for staff member class assignment export.
 */
public class TNStaffMemberClassAssignmentData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for staff member class assignment export.
     */
    public static class TNStaffMemberClassAssignmentEntity extends TNStateReportEntity {
        /**
         * Helper class to store information about ScheduleTeacher.
         */
        class TeacherStatus implements Comparable<TeacherStatus> {
            private boolean m_additionalTor2;
            private PlainDate m_endDate;
            private SisSchool m_school;
            private PlainDate m_startDate;
            private ScheduleTeacher m_teacher;
            private ScheduleTerm m_term;

            /**
             * Constructor.
             *
             * @param school SisSchool
             * @param teacher ScheduleTeacher
             * @param term ScheduleTerm
             * @param startDate PlainDate
             * @param endDate PlainDate
             */
            public TeacherStatus(SisSchool school, ScheduleTeacher teacher, ScheduleTerm term, PlainDate startDate,
                    PlainDate endDate) {
                m_school = school;
                m_teacher = teacher;
                m_term = term;
                m_startDate = startDate;
                m_endDate = endDate;
                m_additionalTor2 = false;
            }

            /**
             * Implementing interface's method.
             *
             * @param o TeacherStatus
             * @return int
             */
            @Override
            public int compareTo(TeacherStatus o) {
                int result = m_teacher.getOid().compareTo(o.getTeacher().getOid());
                if (result == 0) {
                    result = m_school.getOid().compareTo(o.getSchool().getOid());
                }
                if (result == 0) {
                    result = m_term.getOid().compareTo(o.getTerm().getOid());
                }
                if (result == 0) {
                    result = m_startDate.compareTo(o.getStartDate());
                }

                return result;
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

                return compareTo((TeacherStatus) obj) == 0 ? true : false;
            }

            /**
             * Gets the end date.
             *
             * @return the m_endDate
             */
            public PlainDate getEndDate() {
                return m_endDate;
            }

            /**
             * Gets the start date.
             *
             * @return the m_startDate
             */
            public PlainDate getStartDate() {
                return m_startDate;
            }

            /**
             * Gets the school.
             *
             * @return the m_school
             */
            public SisSchool getSchool() {
                return m_school;
            }

            /**
             * Gets the teacher.
             *
             * @return the m_teacher
             */
            public ScheduleTeacher getTeacher() {
                return m_teacher;
            }

            /**
             * Gets the term.
             *
             * @return the m_term
             */
            public ScheduleTerm getTerm() {
                return m_term;
            }

            /**
             * Hash code.
             *
             * @return int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return m_teacher.hashCode() + m_term.hashCode() + m_startDate.hashCode() + m_endDate.hashCode();
            }

            /**
             * Checks if is additional tor 2.
             *
             * @return the m_additionalTor2
             */
            public boolean isAdditionalTor2() {
                return m_additionalTor2;
            }

            /**
             * Sets the additional tor 2.
             *
             * @param additionalTor2 void
             */
            public void setAdditionalTor2(boolean additionalTor2) {
                this.m_additionalTor2 = additionalTor2;
            }

            /**
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder value = new StringBuilder();
                if (m_teacher != null) {
                    value.append(m_teacher.getStaff() == null ? "No Staff Found" : m_teacher.getStaff().getNameView());
                    value.append(" Course[");
                    value.append(m_teacher.getSection() == null ? "No Section Found"
                            : m_teacher.getSection().getCourseView());
                    value.append("] Term[");
                    value.append(m_term == null ? "No Term Found" : m_term.getCode());
                    value.append("] From ");
                    value.append(m_startDate == null ? "[No Date]" : m_startDate.toString());
                    value.append(" to ");
                    value.append(m_endDate == null ? "[No Date]" : m_endDate.toString());
                } else {
                    value.append("Teacher Unknown");
                }
                return value.toString();
            }

        }

        TNStaffMemberClassAssignmentData m_exportData;
        Collection<SisSchool> m_schools = new ArrayList<>();
        List<TeacherStatus> m_teachers = new ArrayList<TeacherStatus>();

        /**
         * Instantiates a new TN staff member class assignment entity.
         */
        public TNStaffMemberClassAssignmentEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            ScheduleTeacher mtc = (ScheduleTeacher) getBean();
            Staff staff = mtc.getStaff();
            String name = staff.getNameView()
                    + "[LASID: " + staff.getLocalId()
                    + " SASID: " + staff.getStateId()
                    + "] ";
            return name;
        }

        /**
         * Gets the mtc.
         *
         * @return Teacher status
         */
        public TeacherStatus getMTC() {
            return m_teachers.get(getCurrentRow());
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
            m_exportData = (TNStaffMemberClassAssignmentData) data;
            ScheduleTeacher mtc = (ScheduleTeacher) bean;
            ArrayList<SisSchool> schools = new ArrayList<>();
            if (m_exportData.isSchoolContext()) {
                schools.add((SisSchool) m_exportData.getSchool());
            } else if (!StringUtils.isEmpty(m_exportData.m_paramSchoolOidStaff)) {
                schools.add((SisSchool) m_exportData.getBroker().getBeanByOid(SisSchool.class,
                        m_exportData.m_paramSchoolOidStaff));
            } else {
                schools =
                        new ArrayList<>(
                                m_exportData.m_classSectionHelper.getSchoolsForSection(mtc.getSection().getOid()));
            }
            m_schools.addAll(schools.stream()
                    .filter(skl -> !StringUtils
                            .isEmpty((String) skl.getFieldValueByBeanPath(m_exportData.m_fieldStateSchoolId)))
                    .collect(Collectors.toList()));
            if (!m_schools.isEmpty()) {
                m_teachers = populateListOfTeachers(mtc);
            }
            if (m_teachers != null) {
                setRowCount(m_teachers.size());
            }
            m_exportData.addEntityRowsCount(getRowCount());
        }

        /**
         * Adds to list of TeacherStatus all possible combinations.
         *
         * @param mtc ScheduleTeacher
         * @return ArrayList
         */
        private ArrayList<TeacherStatus> populateListOfTeachers(ScheduleTeacher mtc) {
            ArrayList<TeacherStatus> teachers = new ArrayList<TeacherStatus>();

            for (SisSchool school : m_schools) {
                PlainDate startDate = m_exportData.getTeacherScheduleStartDate(mtc);
                PlainDate endDate = m_exportData.getTeacherScheduleEndDate(mtc);

                TeacherStatus mtcStatus = new TeacherStatus(school, mtc, mtc.getScheduleTerm(), startDate, endDate);

                if (!teachers.contains(mtcStatus)) {
                    teachers.add(mtcStatus);
                }

                PlainDate startDate2 = null;
                PlainDate endDate2 = null;
                try {
                    startDate2 =
                            (PlainDate) m_exportData.getPropertyAsJavaType(mtc, m_exportData.m_fieldClassStartDate2);
                    endDate2 = (PlainDate) m_exportData.getPropertyAsJavaType(mtc, m_exportData.m_fieldClassEndDate2);
                    if (endDate2 == null) {
                        endDate2 = m_exportData.getScheduleTermEndDate(mtc.getScheduleTerm());
                    }
                } catch (X2BaseException e) {
                    // do nothing;
                }

                if (startDate2 != null) {
                    TeacherStatus mtcStatus2 =
                            new TeacherStatus(school, mtc, mtc.getScheduleTerm(), startDate2, endDate2);
                    mtcStatus2.setAdditionalTor2(true);

                    if (!teachers.contains(mtcStatus2)) {
                        teachers.add(mtcStatus2);
                    }
                }

            }

            return teachers;
        }
    }

    /**
     * Retrieve bean information.
     */
    protected class FieldRetrieverBeanInfo implements FieldRetriever {
        protected static final String CALC_PARAM_CRS_VIEW = "MTC_CRS_VIEW";
        protected static final String CALC_PARAM_DISTR_ID = "MTC_DISTR_ID";
        protected static final String CALC_PARAM_FED_FUNDED = "MTC_FED_FUNDED";
        protected static final String CALC_PARAM_SKL_ID = "MTC_SKL_ID";
        protected static final String CALC_PARAM_STF_ID = "MTC_STAFF_ID";
        public static final String MTC_CALC_ID = "MTC_BEAN_INFO";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            TNStaffMemberClassAssignmentEntity seEntity = (TNStaffMemberClassAssignmentEntity) entity;
            TeacherStatus teacherStatus = seEntity.getMTC();
            ScheduleTeacher mtc = teacherStatus.getTeacher();

            String param = (String) field.getParameter();
            String value = null;
            if (param.equalsIgnoreCase(CALC_PARAM_DISTR_ID)) {
                return mtc.getStaff().getOrganization1().getId();
            }
            if (param.equalsIgnoreCase(CALC_PARAM_SKL_ID)) {
                SisSchool skl = null;

                if (teacherStatus != null && (skl = teacherStatus.getSchool()) != null) {
                    return skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                }
            }
            if (param.equalsIgnoreCase(CALC_PARAM_STF_ID)) {
                if (mtc != null && mtc.getStaff() != null) {
                    value = mtc.getStaff().getLocalId();
                }
            }
            if (param.equalsIgnoreCase(CALC_PARAM_CRS_VIEW)) {
                if (mtc != null && mtc.getSection() != null) {
                    return mtc.getSection().getCourseView();
                }
            }
            if (param.equalsIgnoreCase(CALC_PARAM_FED_FUNDED)) {
                if (BooleanAsStringConverter.TRUE.equals(mtc.getFieldValueByAlias(ALIAS_MTC_FED_FUNDED))) {
                    return Boolean.TRUE;
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for schoolDate and class-end-date fields.
     */
    protected class FieldRetrieverClassDates implements FieldRetriever {
        protected static final String CALC_PARAM_CLASS_END_DATE = "MTC_CLASS_END_DATE";
        protected static final String CALC_PARAM_CLASS_START_DATE = "MTC_CLASS_START_DATE";
        protected static final String MTC_CALC_ID = "MTC_CALC_CLASS";

        private SimpleDateFormat m_dateFormatter;

        /**
         * Instantiates a new field retriever class dates.
         */
        public FieldRetrieverClassDates() {
            m_dateFormatter = new SimpleDateFormat("yyyyMMdd");
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            TNStaffMemberClassAssignmentEntity mtcEntity = (TNStaffMemberClassAssignmentEntity) entity;
            TeacherStatus bean = mtcEntity.getMTC();
            String param = (String) field.getParameter();

            if (param.equalsIgnoreCase(CALC_PARAM_CLASS_START_DATE)) {
                PlainDate startDate = bean.getStartDate();
                if (startDate != null) {
                    value = m_dateFormatter.format(startDate);
                }
            }

            if (param.equalsIgnoreCase(CALC_PARAM_CLASS_END_DATE)) {
                PlainDate endDate = bean.getEndDate();
                if (endDate != null) {
                    value = m_dateFormatter.format(endDate);
                }
            }

            return value;
        }
    }

    /**
     * Field retriever for schoolDate and class-end-date fields.
     */
    protected class FieldRetrieverCustom implements FieldRetriever {
        protected static final String CALC_PARAM_SCHOOLYEAR = "MTC_SCHOOLYEAR";
        protected static final String MTC_CALC_ID = "MTC_CALC_OTHER";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberClassAssignmentData mtcData = (TNStaffMemberClassAssignmentData) data;

            String param = (String) field.getParameter();
            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                return mtcData.m_schoolYear;
            }

            return "##!P: " + param;
        }
    }

    /**
     * Field retriever for License field.
     */
    protected class FieldRetrieverLicense implements FieldRetriever {
        protected static final String MTC_CALC_ID = "MTC_CALC_LICENSE";

        private Map<String, String> m_licenses;

        /**
         * Instantiates a new field retriever license.
         *
         * @param broker X2Broker
         * @param staffCriteria X2Criteria
         */
        public FieldRetrieverLicense(X2Broker broker, X2Criteria staffCriteria) {
            PlainDate date = new PlainDate();
            X2Criteria criteria = new X2Criteria();

            // Select proper date range
            criteria.addLessOrEqualThan(StaffCertification.COL_ISSUE_DATE, date);
            X2Criteria endDate1Criteria = new X2Criteria();
            endDate1Criteria.addIsNull(StaffCertification.COL_EXPIRATION_DATE);
            X2Criteria endDate2Criteria = new X2Criteria();
            endDate2Criteria.addGreaterOrEqualThan(StaffCertification.COL_EXPIRATION_DATE, date);
            endDate1Criteria.addOrCriteria(endDate2Criteria);
            criteria.addAndCriteria(endDate1Criteria);

            // select primary only
            criteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, Boolean.TRUE);

            // select proper staff
            criteria.addIn(StaffCertification.COL_STAFF_OID,
                    new SubQuery(Staff.class, X2BaseBean.COL_OID, staffCriteria));
            QueryByCriteria query = new QueryByCriteria(StaffCertification.class, criteria);
            query.addOrderByAscending(StaffCertification.COL_ISSUE_DATE);

            m_licenses = new HashMap<String, String>();
            QueryIterator iterator = broker.getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    StaffCertification cert = (StaffCertification) iterator.next();
                    m_licenses.put(cert.getStaffOid(), cert.getCertificationNumber());
                }
            } finally {
                iterator.close();
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberClassAssignmentEntity mtcEntity = (TNStaffMemberClassAssignmentEntity) entity;
            TeacherStatus bean = mtcEntity.getMTC();

            Staff stf = bean.getTeacher().getStaff();
            return m_licenses.get(stf.getOid());
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
        protected static final String MTC_CALC_ID = "MTC_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberClassAssignmentEntity seEntity = (TNStaffMemberClassAssignmentEntity) entity;
            ScheduleTeacher schedule = seEntity.getMTC().getTeacher();

            Staff staff = schedule.getStaff();

            if (staff == null) {
                return "";
            }

            Person psn = staff.getPerson();

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
     * Teacher of record is determines on two values mtcPrimaryTeacherInd and DOE ADDITIONAL TOR.
     */
    protected class FieldRetrieverTor implements FieldRetriever {
        public static final String MTC_CALC_ID = "MTC_CALC_TOR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberClassAssignmentEntity seEntity = (TNStaffMemberClassAssignmentEntity) entity;
            TeacherStatus teacherStatus = seEntity.getMTC();
            ScheduleTeacher mtc = teacherStatus.getTeacher();

            String beanPath = teacherStatus.isAdditionalTor2() ? m_additionalTor2 : m_additionalTor;
            String doeValue = (String) mtc.getFieldValueByBeanPath(beanPath);
            if (mtc.getPrimaryTeacherIndicator() ||
                    (!StringUtils.isEmpty(doeValue) && BooleanAsStringConverter.TRUE.equals(doeValue))) {
                return Boolean.TRUE;
            }
            return Boolean.FALSE;
        }
    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FieldValidatorSSN implements FieldValidator {
        protected static final String MTC_VAL_ID = "MTC_VAL_SSN";
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
                        "SSN value must be 9 digits long"));
            }

            return errors;
        }

    }

    /**
     * Error if there are multiple teachers for the same date range with these flag set.
     */
    protected class FieldValidatorTor implements FieldValidator {
        public static final String MTC_VAL_ID = "MTC_VAL_TOR";
        private static final String ERROR_INTERSECTS = "Dates ranges for several TOR teachers intersect.";

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
            TNStaffMemberClassAssignmentEntity seEntity = (TNStaffMemberClassAssignmentEntity) entity;
            TeacherStatus teacherStatus = seEntity.getMTC();
            ScheduleTeacher originalMtc = teacherStatus.getTeacher();
            if (originalMtc.getPrimaryTeacherIndicator()
                    || BooleanAsStringConverter.TRUE.equals(originalMtc.getFieldValueByBeanPath(m_additionalTor))
                    || BooleanAsStringConverter.TRUE.equals(originalMtc.getFieldValueByBeanPath(m_additionalTor2))) {
                String crsNumber = originalMtc.getSection().getCourseView();
                List<ScheduleTeacher> schedules = m_schedulesBySections.get(originalMtc.getSectionOid());
                if (schedules != null && schedules.size() > 1) {
                    List<ScheduleTeacher> mtcInSection = new ArrayList<ScheduleTeacher>(schedules);
                    mtcInSection.remove(originalMtc);
                    ScheduleTerm originalMtcTerm = teacherStatus.getTerm();
                    PlainDate originalMtcStartDate = teacherStatus.getStartDate();
                    PlainDate originalMtcEndDate = teacherStatus.getEndDate();
                    for (ScheduleTeacher anotherMtc : mtcInSection) {
                        if ((anotherMtc.getPrimaryTeacherIndicator()
                                || BooleanAsStringConverter.TRUE
                                        .equals(anotherMtc.getFieldValueByBeanPath(m_additionalTor))
                                || BooleanAsStringConverter.TRUE
                                        .equals(anotherMtc.getFieldValueByBeanPath(m_additionalTor2)))) {
                            ScheduleTerm anotherMtcTerm = anotherMtc.getScheduleTerm();
                            if (originalMtcTerm != null) {
                                if (originalMtcTerm.equals(anotherMtcTerm)) {
                                    PlainDate anotherMtcStartDate = getTeacherScheduleStartDate(anotherMtc);
                                    PlainDate anotherMtcEndDate = getTeacherScheduleEndDate(anotherMtc);
                                    if (!anotherMtcStartDate.after(originalMtcEndDate) &&
                                            !anotherMtcEndDate.before(originalMtcStartDate)) {
                                        String message =
                                                "First teacher: " + teacherStatus.getTeacher().getStaff().getNameView()
                                                        + "; Second teacher: " + anotherMtc.getStaff().getNameView()
                                                        + "; Course Number: " + crsNumber + "; School Name: "
                                                        + originalMtc.getStaff().getSchool().getName();
                                        StateReportValidationError error =
                                                new StateReportValidationError(entity, field, ERROR_INTERSECTS,
                                                        message);
                                        errors.add(error);
                                    }
                                }
                            } else {
                                StateReportValidationError error = new StateReportValidationError(entity, field,
                                        "No Schedule Term", teacherStatus.toString());
                                errors.add(error);
                            }
                        }
                    }
                }
            }
            return errors;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_ADDITIONAL_TOR = "DOE ADDITIONAL TOR";
    protected static final String ALIAS_ADDITIONAL_TOR_2 = "DOE ADDITIONAL TOR2";
    protected static final String ALIAS_CLASS_END_DATE = "DOE STAFF CLASS END DATE";
    protected static final String ALIAS_CLASS_END_DATE_2 = "DOE STAFF CLASS END DATE2";
    protected static final String ALIAS_CLASS_FED_FUNDED = "DOE FEDERALLY FUNDED CLASS";
    protected static final String ALIAS_CLASS_START_DATE = "DOE STAFF CLASS START DATE";
    protected static final String ALIAS_CLASS_START_DATE_2 = "DOE STAFF CLASS START DATE2";
    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_MTC_FED_FUNDED = "DOE FEDERALLY FUNDED CLASS";
    protected static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";

    /**
     * Static String variables. Used by TN Restaging Procedure.
     */
    protected static final String PARAM_COURSE_SECTION = "courseSections";
    protected static final String PARAM_FROM_TAB = "fromTab";
    protected static final String PARAM_SCHOOL_OIDS = "schoolOidCourse";
    protected static final String PARAM_STF_OIDS = "staffsOids";
    protected static final String PARAM_STF_SKL_OID = "schoolOidStaff";
    protected static final String TAB_CRS_MST = "tabStaffMst";

    protected String m_activeStatus;
    protected String m_additionalTor;
    protected String m_additionalTor2;
    protected boolean m_bypassDuplicateSectionTest;
    protected TNClassSectionHelper m_classSectionHelper;
    protected Collection<String> m_courseSectionOids; // used by TN Restaging Procedure
    protected String m_fieldClassEndDate;
    protected String m_fieldClassEndDate2;
    protected String m_fieldClassStartDate;
    protected String m_fieldClassStartDate2;
    protected String m_fieldExcludeStaff;
    protected String m_fieldExcludeCourse;
    protected String m_fieldFedFundedClass;

    // This parameter is used by TN Restaging Procedure to flag if export run as result of running
    // of another export
    // (e.g. if TN 030, TN 031 is run TN 048 and TN 063 for selected Sections should be resent as
    // well.
    protected boolean m_fromCrsMstTab;

    protected Boolean m_paramEntireSchool; // used by TN Restaging Procedure
    protected String m_paramSchoolOidStaff; // used by TN Restaging Procedure
    protected String m_paramStaffsOids; // used by TN Restaging Procedure

    protected Map<String, List<ScheduleTeacher>> m_schedulesBySections;
    protected Collection<String> m_schoolOids; // used by TN Restaging Procedure
    protected Map<String, PlainDate> m_termEndDate;
    protected Map<String, PlainDate> m_termStartDate;
    protected String m_schoolYear;

    private Collection<String> m_activeSchedulesOids;
    private TNMultiYearHelper m_multiYearHelper;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        initActiveSchedulesOids();

        School school = null;
        if (this.isSchoolContext()) {
            school = getSchool();
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            school = (School) getBroker().getBeanByOid(SisSchool.class, m_paramSchoolOidStaff);
        }
        m_multiYearHelper = new TNStaffMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
        // Must set query before initializing TNClassSectionHelper
        QueryByCriteria query = initializeCriteriaAndQuery();
        setQuery(query);

        m_classSectionHelper = new TNClassSectionHelper(this, school, false, null, getBypassValue());

        if (getSetupErrors().size() != 0) {
            return;
        }

        initTorTeachersMap(query);
        setEntityClass(TNStaffMemberClassAssignmentEntity.class);

        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the schedule term end date.
     *
     * @param scheduleTerm ScheduleTerm
     * @return Plain date
     */
    protected PlainDate getScheduleTermEndDate(ScheduleTerm scheduleTerm) {
        PlainDate date = null;
        if (scheduleTerm != null) {
            if (m_termEndDate == null) {
                m_termEndDate = new HashMap();
            }
            if (m_termEndDate.containsKey(scheduleTerm.getOid())) {
                date = m_termEndDate.get(scheduleTerm.getOid());
            } else {
                for (ScheduleTermDate termDate : scheduleTerm.getScheduleTermDates()) {
                    if (date == null || termDate.getEndDate().after(date)) {
                        date = termDate.getEndDate();
                    }
                }
                m_termEndDate.put(scheduleTerm.getOid(), date);
            }
        }
        return date;
    }

    /**
     * Gets the schedule term start date.
     *
     * @param scheduleTerm ScheduleTerm
     * @return Plain date
     */
    protected PlainDate getScheduleTermStartDate(ScheduleTerm scheduleTerm) {
        PlainDate date = null;
        if (scheduleTerm != null) {
            if (m_termStartDate == null) {
                m_termStartDate = new HashMap();
            }
            if (m_termStartDate.containsKey(scheduleTerm.getOid())) {
                date = m_termStartDate.get(scheduleTerm.getOid());
            } else {
                for (ScheduleTermDate termDate : scheduleTerm.getScheduleTermDates()) {
                    if (date == null || termDate.getStartDate().before(date)) {
                        date = termDate.getStartDate();
                    }
                }
                m_termStartDate.put(scheduleTerm.getOid(), date);
            }
        }
        return date;
    }

    /**
     * Returns date when teacher end class.
     *
     * @param teacherSchedule ScheduleTeacher
     * @return Plain date
     */
    protected PlainDate getTeacherScheduleEndDate(ScheduleTeacher teacherSchedule) {
        PlainDate overrideDate;
        try {
            overrideDate = (PlainDate) getPropertyAsJavaType(teacherSchedule, m_fieldClassEndDate);
        } catch (X2BaseException e) {
            overrideDate = null;
        }
        if (overrideDate == null) {
            overrideDate = getScheduleTermEndDate(teacherSchedule.getScheduleTerm());
        } else {
            PlainDate startDate = getScheduleTermStartDate(teacherSchedule.getScheduleTerm());
            PlainDate endDate = getScheduleTermEndDate(teacherSchedule.getScheduleTerm());
            if (startDate != null && overrideDate.before(startDate)) {
                overrideDate = startDate;
            } else if (endDate != null && overrideDate.after(endDate)) {
                overrideDate = endDate;
            }
        }
        return overrideDate;
    }

    /**
     * Returns date when teacher start class.
     *
     * @param teacherSchedule ScheduleTeacher
     * @return Plain date
     */
    protected PlainDate getTeacherScheduleStartDate(ScheduleTeacher teacherSchedule) {
        PlainDate overrideDate;
        try {
            overrideDate = (PlainDate) getPropertyAsJavaType(teacherSchedule, m_fieldClassStartDate);
        } catch (X2BaseException e) {
            overrideDate = null;
        }
        if (overrideDate == null) {
            overrideDate = getScheduleTermStartDate(teacherSchedule.getScheduleTerm());
        } else {
            PlainDate startDate = getScheduleTermStartDate(teacherSchedule.getScheduleTerm());
            PlainDate endDate = getScheduleTermEndDate(teacherSchedule.getScheduleTerm());
            if (startDate != null && overrideDate.before(startDate)) {
                overrideDate = startDate;
            } else if (endDate != null && overrideDate.after(endDate)) {
                overrideDate = endDate;
            }
        }
        return overrideDate;
    }

    /**
     * get the parameter setting for bypass value. Default to false.
     *
     * @return boolean
     */
    private boolean getBypassValue() {
        return getParameter(PARAM_BYPASS_DUP_SECT_TEST) != null
                ? ((Boolean) getParameter(PARAM_BYPASS_DUP_SECT_TEST)).booleanValue()
                : false;
    }

    /**
     * Method for implementing business rule for schoolYear = (CTX_SCHOOL_YEAR - 1).
     *
     * @return string representation of school year
     */
    private final String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Method for building custom Staff criteria.
     *
     * @param pathPrefix String
     * @return X 2 criteria
     */
    private X2Criteria getStaffCriteria(String pathPrefix) {
        X2Criteria criteria = new X2Criteria();
        m_multiYearHelper.adjustCriteria(criteria, Strategy.NOT_EQUAL_TO,
                pathPrefix + m_fieldExcludeStaff, BooleanAsStringConverter.TRUE);
        if (isSchoolContext()) {
            m_multiYearHelper.adjustCriteria(criteria, Strategy.EQUAL_TO, pathPrefix + Staff.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            m_multiYearHelper.adjustCriteria(criteria, Strategy.EQUAL_TO, pathPrefix + Staff.COL_SCHOOL_OID,
                    m_paramSchoolOidStaff);
        }
        applyInputCriteria(criteria, false, pathPrefix);
        return criteria;
    }

    /**
     * Builds query for getting all needed {@link ScheduleTeacher}.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria initializeCriteriaAndQuery() {
        X2Criteria criteria = getStaffCriteria(ScheduleTeacher.REL_STAFF + PATH_DELIMITER);
        criteria.addNotEmpty(ScheduleTeacher.REL_STAFF + ModelProperty.PATH_DELIMITER + SisStaff.REL_SCHOOL
                + ModelProperty.PATH_DELIMITER + m_fieldStateSchoolId, getBroker().getPersistenceKey());
        // Filter by current year context
        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                Schedule.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER +
                DistrictSchoolYearContext.COL_CONTEXT_ID,
                getCurrentContext().getContextId());

        // Include Active Schedules only
        criteria.addIn(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.COL_SCHEDULE_OID, m_activeSchedulesOids);

        // Must include state course code
        criteria.addNotEmpty(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE +
                PATH_DELIMITER + m_fieldStateCourseCode, getBroker().getPersistenceKey());

        // Exclude courses with exclude flag
        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldExcludeCourse,
                BooleanAsStringConverter.TRUE);

        m_fromCrsMstTab = TAB_CRS_MST.equals(getParameter(PARAM_FROM_TAB));
        if (m_fromCrsMstTab) {
            Boolean wholeSchool = Boolean.FALSE;
            String courseSectionString = (String) getParameter(PARAM_COURSE_SECTION);
            if (StringUtils.isEmpty(courseSectionString)) {
                wholeSchool = Boolean.TRUE;
            }
            if (wholeSchool.booleanValue()) {
                m_schoolOids = new ArrayList<String>();
                String schoolOidsString = (String) getParameter(PARAM_SCHOOL_OIDS);
                splitPicklistBeanOids(m_schoolOids, schoolOidsString);
                criteria.addIn(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER +
                        SchoolCourse.COL_SCHOOL_OID, m_schoolOids);
            } else {
                m_courseSectionOids = new ArrayList<String>();
                splitPicklistBeanOids(m_courseSectionOids, courseSectionString);
                criteria.addIn(ScheduleTeacher.COL_SECTION_OID, m_courseSectionOids);
            }
        }

        m_paramSchoolOidStaff = (String) getParameter(PARAM_STF_SKL_OID);
        m_paramEntireSchool = Boolean.FALSE;
        m_paramStaffsOids = (String) getParameter(PARAM_STF_OIDS);
        if (StringUtils.isEmpty(m_paramStaffsOids)) {
            m_paramEntireSchool = Boolean.TRUE;
        }
        if (isSchoolContext()) {
            m_multiYearHelper.adjustCriteria(criteria, Strategy.EQUAL_TO,
                    ScheduleTeacher.REL_STAFF + PATH_DELIMITER + Staff.COL_SCHOOL_OID, getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    X2BaseBean.COL_OID, m_paramSchoolOidStaff);
        }
        if (!StringUtils.isEmpty(m_paramStaffsOids)) {
            String[] staffOids = m_paramStaffsOids.split(",");
            Collection<String> collection = Arrays.asList(staffOids);
            criteria.addIn(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + X2BaseBean.COL_OID, collection);
        }

        this.applyInputCriteria(criteria, true, null);
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);
        applyInputSort(query, null);

        return query;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        } else {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, true);
        m_fieldClassStartDate = translateAliasToJavaName(ALIAS_CLASS_START_DATE, true);
        m_fieldClassStartDate2 = translateAliasToJavaName(ALIAS_CLASS_START_DATE_2, true);
        m_fieldClassEndDate = translateAliasToJavaName(ALIAS_CLASS_END_DATE, true);
        m_fieldClassEndDate2 = translateAliasToJavaName(ALIAS_CLASS_END_DATE_2, true);
        m_fieldFedFundedClass = translateAliasToJavaName(ALIAS_CLASS_FED_FUNDED, true);
        m_additionalTor = translateAliasToJavaName(ALIAS_ADDITIONAL_TOR, true);
        m_additionalTor2 = translateAliasToJavaName(ALIAS_ADDITIONAL_TOR_2, true);
        m_schoolYear = getCurentSchoolYear();
    }

    /**
     * Initialize active schedules oids for selected context.
     */
    private void initActiveSchedulesOids() {
        m_activeSchedulesOids = new ArrayList<String>();

        X2Criteria schoolScheduleCriteria = new X2Criteria();
        schoolScheduleCriteria.addEqualTo(SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        SubQuery schedulesSubQuery = new SubQuery(SchoolScheduleContext.class,
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, schoolScheduleCriteria);

        X2Criteria activeSchedulesCriteria = new X2Criteria();
        activeSchedulesCriteria.addIn(X2BaseBean.COL_OID, schedulesSubQuery);

        QueryByCriteria queryByCriteria = new QueryByCriteria(Schedule.class, activeSchedulesCriteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(queryByCriteria);

        try {
            while (iterator.hasNext()) {
                Schedule schedule = (Schedule) iterator.next();
                m_activeSchedulesOids.add(schedule.getOid());
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Inits the tor teachers map.
     *
     * @param query QueryByCriteria
     */
    private void initTorTeachersMap(QueryByCriteria query) {
        Criteria torCriteria = new Criteria();
        torCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, BooleanAsStringConverter.TRUE);

        Criteria doeTorCriteria = new Criteria();
        doeTorCriteria.addEqualTo(m_additionalTor, BooleanAsStringConverter.TRUE);
        torCriteria.addOrCriteria(doeTorCriteria);

        Criteria criteria = query.getCriteria().copy(false, false, true);
        criteria.addAndCriteria(torCriteria);

        m_schedulesBySections = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_SECTION_OID, 50);
    }

    /**
     * Register custom field retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put(FieldRetrieverBeanInfo.MTC_CALC_ID, new FieldRetrieverBeanInfo());
        calcs.put(FieldRetrieverCustom.MTC_CALC_ID, new FieldRetrieverCustom());
        calcs.put(FieldRetrieverClassDates.MTC_CALC_ID, new FieldRetrieverClassDates());
        calcs.put(FieldRetrieverLicense.MTC_CALC_ID, new FieldRetrieverLicense(getBroker(), getStaffCriteria("")));
        calcs.put(FieldRetrieverSSN.MTC_CALC_ID, new FieldRetrieverSSN());
        calcs.put(FieldRetrieverTor.MTC_CALC_ID, new FieldRetrieverTor());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FieldValidatorSSN.MTC_VAL_ID, new FieldValidatorSSN());
        validators.put(FieldValidatorTor.MTC_VAL_ID, new FieldValidatorTor());
        super.addValidators(validators);
    }

    /**
     * Splits parameter from picklist to initialize collection of Oids of selected entities.
     *
     * @param collectionOids - collection that will be splitted.
     * @param oidsString - parameter of multiselect picklist for splitting.
     */
    private void splitPicklistBeanOids(Collection<String> collectionOids, String oidsString) {
        if (!StringUtils.isEmpty(oidsString) && collectionOids != null) {
            String[] oids = oidsString.split(",");
            collectionOids.addAll(Arrays.asList(oids));
        }
    }

}
