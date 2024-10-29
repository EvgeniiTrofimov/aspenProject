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
package com.x2dev.procedures.statereporting.ga;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.google.common.collect.Streams;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for GA Student Course File export.
 *
 * @author X2 Development Corporation
 */
public class GAStudentCourseFileV3 extends StateReportData {

    /**
     * Implementation of StateReportEntity to be used by the VA Student Record Collection export.
     * This must be a public
     * static inner class with a public no argument constructor so it can be instantiated through
     * reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentCourseFileEntity extends ToolsSharedContainer.StateReportEntity {
        /**
         * GAStudentCourseFileV3 object
         */
        GAStudentCourseFileV3 m_data;

        /**
         * Student schedule span for entity
         */
        List<Object> m_scheduleSpans;
        /**
         * List of a student schedule's additional teachers
         */
        List<GAScheduleTeacher> m_teachers;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentCourseFileEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Retrieves and returns a student's schedule span or transcript.
         *
         * @return StudentScheduleSpan for the current row
         */
        public Object getCurrentRecord() {
            Object currentSpan = null;
            if (m_scheduleSpans != null && getCurrentRow() < m_scheduleSpans.size() && getCurrentRow() >= 0) {
                currentSpan = m_scheduleSpans.get(getCurrentRow());
            }
            return currentSpan;
        }

        /**
         * Gets the current section.
         *
         * @return Tool Section
         */
        public ToolSection getCurrentSection() {
            Object currentObject = getCurrentRecord();
            ToolSection section = null;
            if (currentObject != null) {
                if (currentObject instanceof ToolTranscript) {
                    section = ((ToolTranscript) currentObject).getSection(m_data.getBroker());
                } else {
                    section = ((StudentScheduleSpan) currentObject).getSection();
                }
            }
            return section;
        }

        /**
         * Gets the current student schedule span.
         *
         * @return Student schedule span
         */
        public StudentScheduleSpan getCurrentStudentScheduleSpan() {
            StudentScheduleSpan span = null;
            Object currentObject = getCurrentRecord();
            if (currentObject != null) {
                if (currentObject instanceof StudentScheduleSpan) {
                    span = (StudentScheduleSpan) currentObject;
                }
            }
            return span;
        }

        /**
         * Gets the current trancript.
         *
         * @param useSpan boolean
         * @return ToolTranscript
         */
        public GATranscript getCurrentTrancript(boolean useSpan) {
            ToolTranscript transcript = null;
            Object currentObject = getCurrentRecord();
            if (currentObject != null) {
                if (currentObject instanceof ToolTranscript) {
                    transcript = (ToolTranscript) currentObject;
                } else if (useSpan) {
                    StudentScheduleSpan span = (StudentScheduleSpan) currentObject;
                    transcript = span.getTranscript();
                }
            }
            return (GATranscript) transcript;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return entity name
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            GAStudent student = (GAStudent) getBean();
            String name = STRING_EMPTY;

            name +=
                    student.getNameView() + " [Local ID: " + student.getLocalId() + ", State ID: "
                            + student.getStateId() + "] ";

            return name;
        }

        /**
         * Initialize.
         *
         * Create map of StudentScheduleSpan objects for each student.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (GAStudentCourseFileV3) data;
            GAStudent student = (GAStudent) bean;

            m_scheduleSpans = new ArrayList<Object>();

            // This will run once for each Student
            List<StudentScheduleSpan> studentScheduleSpans = null;
            List<AnnualSpan> studentEnrollmentSpans = null;

            studentScheduleSpans = student.getStudentScheduleSpans(m_data.getBroker());
            studentEnrollmentSpans = student.getEnrollmentSpans(m_data.getBroker(), true, false)
                    .stream()
                    // .filter(span -> !span.isSecondary())
                    .collect(Collectors.toList());

            if (studentScheduleSpans != null && studentScheduleSpans.size() > 0) {
                for (StudentScheduleSpan scheduleSpan : studentScheduleSpans) {

                    GAStudentSchedule sscToCheck = (GAStudentSchedule) scheduleSpan.getSchedule();
                    if (sscToCheck != null && BooleanAsStringConverter.TRUE.equals(sscToCheck.getExclude())) {
                        continue;
                    }
                    GAStudentScheduleChange sccToCheckEntry = (GAStudentScheduleChange) scheduleSpan.getEntryChange();
                    if (sccToCheckEntry != null
                            && BooleanAsStringConverter.TRUE.equals(sccToCheckEntry.getExclude())) {
                        continue;
                    }
                    GAStudentScheduleChange sccToCheckExit = (GAStudentScheduleChange) scheduleSpan.getExitChange();
                    if (sccToCheckExit != null
                            && BooleanAsStringConverter.TRUE.equals(sccToCheckExit.getExclude())) {
                        continue;
                    }
                    if (scheduleSpan.getEntryChange() != null
                            && scheduleSpan.getExitChange() != null
                            && scheduleSpan.getEntryChange().getEffectiveDate()
                                    .equals(scheduleSpan.getExitChange().getEffectiveDate())) {
                        continue;
                    }
                    PlainDate exitDate = scheduleSpan.getExitDate();
                    // NOT NECESSARY SINCE ExitDate will always be less than
                    // or equal ExitChange
                    // if there is a schedule change, and its date is before
                    // the
                    // schedule span exit date, use that as the end date.
                    if (scheduleSpan.getExitChange() != null
                            && scheduleSpan.getExitChange().getEffectiveDate().before(exitDate)) {
                        exitDate = scheduleSpan.getExitChange().getEffectiveDate();
                    }
                    // set the start date to the schedule span entry
                    // date.
                    PlainDate entryDate = scheduleSpan.getEntryDate();
                    // NOT NECESSARY SINCE EntryDate will always be
                    // greater than EntryChange
                    // if there is a schedule change, and its date
                    // is after the
                    // schedule span entry date, use that as the
                    // start date.
                    if (scheduleSpan.getEntryChange() != null
                            && scheduleSpan.getEntryChange().getEffectiveDate()
                                    .after(entryDate)) {
                        entryDate = scheduleSpan.getEntryChange().getEffectiveDate();
                    }
                    if (entryDate != null && exitDate != null && entryDate.equals(exitDate)) {
                        continue;
                    }

                    ToolScheduleTerm schedTerm = null;
                    String scheduleTermCode = null;
                    GASection section = (GASection) scheduleSpan.getSection();
                    PlainDate scheduleStartDate = scheduleSpan.getEntryDate();
                    PlainDate scheduleEndDate = scheduleSpan.getExitDate();
                    GASchool sectionSchool = null;
                    String sectionSchoolOid = null;
                    ToolStudentScheduleChange dropRecord = null;

                    String sectionSchoolStateCode = null;

                    if (section != null) {
                        schedTerm = section.getScheduleTerm(m_data.getBroker());
                        if (schedTerm != null) {
                            scheduleTermCode = schedTerm.getCode();
                        }
                        sectionSchool = section.getSchoolCourseSchool(m_data.getBroker());
                        sectionSchoolOid = sectionSchool.getOid();
                        sectionSchoolStateCode = sectionSchool.getSchoolCode();
                    }
                    if (m_selectedTermCodes == null || m_selectedTermCodes.size() == 0
                            || m_selectedTermCodes.contains(scheduleTermCode)) {
                        // Only include sections that started prior to the report date
                        if (scheduleStartDate != null && !scheduleStartDate.after(m_data.m_reportDate)) {
                            dropRecord = scheduleSpan.getExitChange();
                            // If no drop record is found for schedule span
                            // (section has not been dropped),
                            // automatically add the schedule span record.
                            if (dropRecord == null) {
                                m_scheduleSpans.add(scheduleSpan);
                                continue;
                            }
                            if (studentEnrollmentSpans != null && studentEnrollmentSpans.size() > 0) {
                                for (AnnualSpan enrollmentSpan : studentEnrollmentSpans) {
                                    GASchool enrollmentSchool = (GASchool) enrollmentSpan.getSchool();
                                    String enrollmentSchoolOid = enrollmentSchool.getOid();
                                    PlainDate enrollmentStartDate =
                                            enrollmentSpan.getFirstActiveEnrollment() == null ? null
                                                    : enrollmentSpan.getFirstActiveEnrollment().getEnrollmentDate();
                                    PlainDate enrollmentEndDate = enrollmentSpan.getLastActiveInSessionDate();

                                    String enrSchoolStateCode = enrollmentSchool.getSchoolCode();

                                    if (sectionSchoolOid.equals(enrollmentSchoolOid)
                                            || (!StringUtils.isEmpty(enrSchoolStateCode)
                                                    && !StringUtils.isEmpty(sectionSchoolStateCode)
                                                    && enrSchoolStateCode
                                                            .equals(sectionSchoolStateCode))) {
                                        boolean scheduleStartOverlaps = false;
                                        boolean scheduleEndOverlaps = false;
                                        // If school matches on section and
                                        // enrollment span, make sure the
                                        // schedule span's start or end date
                                        // falls
                                        // on or between the start and end dates
                                        // of the enrollment span. This
                                        // eliminates schedule spans
                                        // which occur while the student is not
                                        // enrolled at the school (usually from
                                        // bad drop records)
                                        if ((enrollmentStartDate == null || !scheduleStartDate
                                                .before(enrollmentStartDate))
                                                && (enrollmentEndDate == null || !scheduleStartDate
                                                        .after(enrollmentEndDate))) {
                                            scheduleStartOverlaps = true;
                                        }
                                        if ((enrollmentStartDate == null || !scheduleEndDate
                                                .before(enrollmentStartDate))
                                                && (enrollmentEndDate == null || !scheduleEndDate
                                                        .after(enrollmentEndDate))) {
                                            scheduleEndOverlaps = true;
                                        }
                                        if (scheduleStartOverlaps || scheduleEndOverlaps) {
                                            m_scheduleSpans.add(scheduleSpan);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Collection<ToolTranscript> trns = student.getStudentTranscripts(m_data.getBroker());
            for (ToolTranscript toolTrn : trns) {
                GATranscript trn = (GATranscript) toolTrn;
                if ((m_selectedTermCodes == null || m_selectedTermCodes.size() == 0
                        || m_selectedTermCodes.contains(trn.getTermCode()))
                        && trn.getDistrictContextOid().equals(m_data.getCurrentContext().getOid()) &&
                        (BooleanAsStringConverter.TRUE
                                .equals(trn.getOutOfDistrict())
                                || trn.getUserDescriptionIndicator())) {
                    m_scheduleSpans.add(trn);
                }
            }
            m_scheduleSpans.sort(new Comparator() {
                @Override
                public int compare(Object o1, Object o2) {
                    int res = 0;
                    if (o1 instanceof StudentScheduleSpan) {
                        if (o2 instanceof ToolTranscript) {
                            res = 1;
                        } else {
                            res = ((StudentScheduleSpan) o1).getSection().getOid()
                                    .compareTo(((StudentScheduleSpan) o2).getSection().getOid());
                        }
                        if (res == 0) {
                            res = ((StudentScheduleSpan) o1).getEntryDate()
                                    .compareTo(((StudentScheduleSpan) o2).getEntryDate());
                        }
                    } else {
                        if (o2 instanceof StudentScheduleSpan) {
                            res = -1;
                        } else {
                            res = ((ToolTranscript) o1).getOid().compareTo(((ToolTranscript) o2).getOid());
                        }
                    }
                    return res;
                }
            });
            // Individual records are a single Student Schedule Span which
            // represent one section that a student was in.
            setRowCount(m_scheduleSpans.size());
        }
    }

    /**
     * The Class Organization.
     */
    public static class GAOrganization extends ToolBean.ToolOrganization {
        private static final String ALIAS_DOE_DISTRICT = "DOE District";

        public static final ToolBeanColumn FIELD_DOE_DISTRICT =
                new ToolBeanColumn(SisBeanPaths.ORGANIZATION, ALIAS_DOE_DISTRICT);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolOrganization.FULL_DEFINITION.expand(FIELD_DOE_DISTRICT);

        /**
         * Instantiates a new Organization.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAOrganization(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the district.
         *
         * @return String
         */
        public String getDoeDistrict() {
            return getValueString(FIELD_DOE_DISTRICT);
        }
    }

    /**
     * The Class ScheduleTeacher.
     */
    public static class GAScheduleTeacher extends ToolScheduleTeacher {
        private static final String ALIAS_DOE_CLASS_ENTRY_DATE = "DOE CLASS ENTRY DATE";
        private static final String ALIAS_MTC_CHW_CODE = "all-mtc-CHWCertifiedFieldCode";

        public static final ToolBeanColumn FIELD_CHW_CERT_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_MTC_CHW_CODE);

        public static final ToolBeanColumn FIELD_CLASS_ENTRY_DATE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER_TEACHER, ALIAS_DOE_CLASS_ENTRY_DATE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolScheduleTeacher.FULL_DEFINITION
                .expand(FIELD_CLASS_ENTRY_DATE,
                        FIELD_CHW_CERT_CODE);

        /**
         * Instantiates a new Schedule Teacher.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAScheduleTeacher(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the class entry date.
         *
         * @return PlainDate
         */
        public PlainDate getClassEntryDate() {
            return getValueDate(FIELD_CLASS_ENTRY_DATE);
        }

        public String getCHWCertStateCode() {
            return getValueReferenceState(FIELD_CHW_CERT_CODE);
        }
    }

    /**
     * The Class School.
     */
    public static class GASchool extends ToolSchool {
        private static final String ALIAS_DOE_SCHOOL = "DOE School";
        private static final String ALIAS_DOE_SKL_PER_ATT = "DOE PERIOD ATTENDANCE SCHOOL";

        public static final ToolBeanColumn FIELD_PERIOD_ATTENDANCE_INDICATOR =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_DOE_SKL_PER_ATT);
        public static final ToolBeanColumn FIELD_SCHOOL_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHOOL, ALIAS_DOE_SCHOOL);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSchool.FULL_DEFINITION
                .expand(FIELD_PERIOD_ATTENDANCE_INDICATOR,
                        FIELD_SCHOOL_CODE);

        /**
         * Instantiates a new School.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GASchool(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }


        /**
         * Gets the period attendance indicator.
         *
         * @return boolean
         */
        public boolean getPeriodAttendanceIndicator() {
            return getValueLogical(FIELD_PERIOD_ATTENDANCE_INDICATOR);
        }

        /**
         * Gets the school code.
         *
         * @return String
         */
        public String getSchoolCode() {
            return getValueString(FIELD_SCHOOL_CODE);
        }

        /**
         * Gets the organization 1.
         *
         * @param broker X2Broker
         * @return GA Organization Tool Bean
         */
        @Override
        public GAOrganization getOrganization1(X2Broker broker) {
            String orgOid = getValueString(FIELD_ORGANIZATION_1_OID);
            return getBeanByOid(broker, GAOrganization.class, orgOid, true);
        }
    }


    /**
     * The Class Section.
     */
    public static class GASection extends ToolSection {

        private static final String ALIAS_CRS_DOE_WBL_CATEGORY = "DOE WBL COURSE";
        private static final String ALIAS_DOE_ALT_ED = "DOE Alt Ed Section";
        private static final String ALIAS_DOE_CREDIT_RECOVERY = "DOE Credit Recovery";
        private static final String ALIAS_DOE_EXCLUDE_CRS = "DOE EXCLUDE CRS";
        private static final String ALIAS_DOE_EXCLUDE_MST = "DOE EXCLUDE MST";
        private static final String ALIAS_DOE_MST_IMMERS_LANG = "DOE IMMERSION LANGUAGE";
        private static final String ALIAS_DOE_MST_IN_LIEU = "DOE Credit In Lieu";
        private static final String ALIAS_DOE_WBL_CATEGORY = "DOE WBL Program Category";
        private static final String ALIAS_DOE_ONLINE_COURSE = "DOE Online Course";
        private static final String ALIAS_MST_DELIVERY_MODEL = "all-mst-SectionDeliveryModel";
        private static final String ALIAS_MST_INCLUSION_CODE = "all-mst-SectionInclusionCode";

        public static final ToolBeanColumn FIELD_ALT_ED =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_DOE_ALT_ED);
        public static final ToolBeanColumn FIELD_CREDIT_RECOVERY =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_DOE_CREDIT_RECOVERY);
        public static final ToolBeanColumn FIELD_CRS_WBL_CATEGORY =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(), ALIAS_CRS_DOE_WBL_CATEGORY);
        public static final ToolBeanColumn FIELD_CSK_SKL_OID =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().schoolOid());
        public static final ToolBeanColumn FIELD_DELIVERY_MODEL =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_DELIVERY_MODEL);
        public static final ToolBeanColumn FIELD_EXCLUDE_CRS =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER.schoolCourse().course(),
                        new ToolBeanColumn.AliasDefinition(ALIAS_DOE_EXCLUDE_CRS, null, false));
        public static final ToolBeanColumn FIELD_EXCLUDE_MST =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER,
                        new ToolBeanColumn.AliasDefinition(ALIAS_DOE_EXCLUDE_MST, null, false));
        public static final ToolBeanColumn FIELD_IMMERS_LANG =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_DOE_MST_IMMERS_LANG);
        public static final ToolBeanColumn FIELD_IN_LIEU =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_DOE_MST_IN_LIEU);
        public static final ToolBeanColumn FIELD_INCLUSION_CODE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_MST_INCLUSION_CODE);
        public static final ToolBeanColumn FIELD_ONLINE_COURSE =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_DOE_ONLINE_COURSE);
        public static final ToolBeanColumn FIELD_WBL_CATEGORY =
                new ToolBeanColumn(SisBeanPaths.SCHEDULE_MASTER, ALIAS_DOE_WBL_CATEGORY);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolSection.FULL_DEFINITION
                .expand(FIELD_ALT_ED,
                        FIELD_CRS_WBL_CATEGORY,
                        FIELD_CREDIT_RECOVERY,
                        FIELD_CSK_CREDIT,
                        FIELD_CSK_SKL_OID,
                        FIELD_DELIVERY_MODEL,
                        FIELD_EXCLUDE_CRS,
                        FIELD_EXCLUDE_MST,
                        FIELD_IMMERS_LANG,
                        FIELD_IN_LIEU,
                        FIELD_INCLUSION_CODE,
                        FIELD_ONLINE_COURSE,
                        FIELD_WBL_CATEGORY);

        /**
         * Instantiates a new Section.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GASection(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the alt ed.
         *
         * @return String
         */
        public String getAltEd() {
            return getValueString(FIELD_ALT_ED);
        }

        /**
         * Gets the WBL category.
         *
         * @return boolean
         */
        public boolean getCourseWBLCategory() {
            return getValueLogical(FIELD_CRS_WBL_CATEGORY);
        }

        /**
         * Gets the credit recovery.
         *
         * @return String
         */
        public String getCreditRecovery() {
            return getValueString(FIELD_CREDIT_RECOVERY);
        }

        /**
         * Gets the delivery model.
         *
         * @return String
         */
        public String getDeliveryModel() {
            return getValueString(FIELD_DELIVERY_MODEL);
        }

        /**
         * Gets the immersion language.
         *
         * @return String
         */
        public String getImmersionLanguage() {
            return getValueString(FIELD_IMMERS_LANG);
        }

        /**
         * Gets the inclusion code.
         *
         * @return String
         */
        public String getInclusionCode() {
            return getValueString(FIELD_INCLUSION_CODE);
        }

        /**
         * Gets the credit in lieu.
         *
         * @return String
         */
        public String getInLieu() {
            return getValueString(FIELD_IN_LIEU);
        }

        /**
         * Gets the online course.
         *
         * @return String
         */
        public String getOnlineCourse() {
            return getValueString(FIELD_ONLINE_COURSE);
        }

        /**
         * Gets the school course credit.
         *
         * @param broker X2Broker
         * @return GASchool tool bean
         */
        public GASchool getSchoolCourseSchool(X2Broker broker) {
            String sklOid = getValueString(FIELD_CSK_SKL_OID);
            return getBeanByOid(broker, GASchool.class, sklOid, true);

        }

        /**
         * Gets the WBL category.
         *
         * @return String
         */
        public String getWBLCategory() {
            return getValueString(FIELD_WBL_CATEGORY);
        }

    }


    /**
     * The Class Staff.
     */
    public static class GAStaff extends ToolStaff {
        private static final String ALIAS_PSN_DOE_SSN = "DOE SSN";

        public static final ToolBeanColumn FIELD_SSN =
                new ToolBeanColumn(SisBeanPaths.STAFF.person(), ALIAS_PSN_DOE_SSN);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStaff.FULL_DEFINITION
                .expand(FIELD_SSN);

        /**
         * Instantiates a new Staff.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAStaff(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the ssn.
         *
         * @return String
         */
        public String getSSN() {
            return getValueString(FIELD_SSN);
        }

    }


    /**
     * The Class Student.
     */
    public static class GAStudent extends ToolStudent {
        private static final String ALIAS_DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
        private static final String ALIAS_DOE_REMOTE_STUDENT = "all-std-RemoteStudent";
        private static final String ALIAS_STD_EXCLUDE_FROM_REPORTING = "DOE EXCLUDE STD";
        private static final String ALIAS_STD_GTID = "GTID";
        private static final String ALIAS_STD_PRIM_EXCEPT = "DOE Primary Exceptionality";

        // Nonquery Fields
        public static final ToolBeanColumn FIELD_EXCLUDE_FROM_REPORTING =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_EXCLUDE_FROM_REPORTING);
        public static final ToolBeanColumn FIELD_GTID = new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_STD_GTID);
        public static final ToolBeanColumn FIELD_OVERRIDE_SCHOOL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT, ALIAS_DOE_OVERRIDE_SCHOOL);
        public static final ToolBeanColumn FIELD_PRIMARY_EXCEPTIONALITY =
                new ToolBeanColumn(SisBeanPaths.STUDENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_STD_PRIM_EXCEPT, null, false));
        public static final ToolBeanColumn FIELD_REMOTE_STUDENT =
                new ToolBeanColumn(SisBeanPaths.STUDENT,
                        new ToolBeanColumn.AliasDefinition(ALIAS_DOE_REMOTE_STUDENT, null, false));

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudent.FULL_DEFINITION
                .expand(FIELD_EXCLUDE_FROM_REPORTING,
                        FIELD_GTID,
                        FIELD_OVERRIDE_SCHOOL_CODE,
                        FIELD_PRIMARY_EXCEPTIONALITY,
                        FIELD_REMOTE_STUDENT);

        /**
         * Instantiates a new Student.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAStudent(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets override school code.
         *
         * @return String
         */
        public String getOverrideSchoolCode() {
            return getValueString(FIELD_OVERRIDE_SCHOOL_CODE);
        }

        /**
         * Gets the primary exceptionality.
         *
         * @return String
         */
        public String getPrimaryExceptionality() {
            return getValueString(FIELD_PRIMARY_EXCEPTIONALITY);
        }

        /**
         * Gets remote student indicator.
         *
         * @return boolean
         */
        public boolean getRemoteStudent() {
            return getValueLogical(FIELD_REMOTE_STUDENT);
        }
    }


    /**
     * The Class Student Schedule Change.
     */
    public static class GAStudentScheduleChange extends ToolBean.ToolStudentScheduleChange {
        public static final String ALIAS_SCC_ALT_SKL_CODE = "all-scc-AlternateSchoolCode";
        public static final String ALIAS_SCC_ALT_SYS_CODE = "all-scc-AlternateSystemCode";
        public static final String ALIAS_SCC_CALC_STATE_CRS_CODE = "DOE CALCULATED STATE CRS CODE HISTORY";
        public static final String ALIAS_SCC_EXCLUDE = "all-scc-ExcludefromStateReporting";
        public static final String ALIAS_SCC_INCLUDE_STD = "all-scc-DeliveryorInclusionStudent";
        public static final String ALIAS_SCC_STD_DELIVERY_MODEL = "all-scc-StudentDeliveryModel";
        public static final String ALIAS_SCC_STD_DELIVERY_MODEL_ADD = "all-scc-AdditionalStudentDeliveryModel";
        public static final String ALIAS_SCC_STD_INCLUSION_CODE = "all-scc-StudentInclusionCode";

        public static final ToolBeanColumn FIELD_ALT_SKL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_ALT_SKL_CODE);
        public static final ToolBeanColumn FIELD_ALT_SYS_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_ALT_SYS_CODE);
        public static final ToolBeanColumn FIELD_CALC_STATE_CRS_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_CALC_STATE_CRS_CODE);
        public static final ToolBeanColumn FIELD_EXCLUDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_EXCLUDE);
        public static final ToolBeanColumn FIELD_INCLUDE_STD =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_INCLUDE_STD);
        public static final ToolBeanColumn FIELD_STD_DELIVERY_MODEL =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_STD_DELIVERY_MODEL);
        public static final ToolBeanColumn FIELD_STD_DELIVERY_MODEL_ADD =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_STD_DELIVERY_MODEL_ADD);
        public static final ToolBeanColumn FIELD_STD_INCLUSION_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE_CHANGE, ALIAS_SCC_STD_INCLUSION_CODE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentScheduleChange.FULL_DEFINITION
                .expand(FIELD_ALT_SKL_CODE,
                        FIELD_ALT_SYS_CODE,
                        FIELD_CALC_STATE_CRS_CODE,
                        FIELD_EXCLUDE,
                        FIELD_INCLUDE_STD,
                        FIELD_STD_DELIVERY_MODEL,
                        FIELD_STD_DELIVERY_MODEL_ADD,
                        FIELD_STD_INCLUSION_CODE);

        /**
         * Instantiates a new Student Schedule Change.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAStudentScheduleChange(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the calculated state course code.
         *
         * @return String
         */
        public String getCalcStateCrsCode() {
            return getValueString(FIELD_CALC_STATE_CRS_CODE);
        }

        /**
         * Gets the calculated alternate school code.
         *
         * @return String
         */
        public String getAltSklCode() {
            return getValueString(FIELD_ALT_SKL_CODE);
        }

        /**
         * Gets the calculated alternate system code.
         *
         * @return String
         */
        public String getAltSysCode() {
            return getValueString(FIELD_ALT_SYS_CODE);
        }

        /**
         * Gets the exclusion indicator.
         *
         * @return String
         */
        public String getExclude() {
            return getValueString(FIELD_EXCLUDE);
        }

        /**
         * Gets the student inclusion indicator.
         *
         * @return String
         */
        public String getIncludeStd() {
            return getValueString(FIELD_INCLUDE_STD);
        }

        /**
         * Gets the student delivery model.
         *
         * @return String
         */
        public String getStdDeliveryModel() {
            return getValueString(FIELD_STD_DELIVERY_MODEL);
        }

        /**
         * Gets the additional student delivery model.
         *
         * @return String
         */
        public String getStdDeliveryModelAdd() {
            return getValueString(FIELD_STD_DELIVERY_MODEL_ADD);
        }

        /**
         * Gets the student inclusion code.
         *
         * @return String
         */
        public String getStdInclusionCode() {
            return getValueString(FIELD_STD_INCLUSION_CODE);
        }
    }


    /**
     * The Class Student Schedule.
     */
    public static class GAStudentSchedule extends ToolBean.ToolStudentSchedule {
        public static final String ALIAS_SSC_ALT_SKL_CODE = "all-ssc-AlternateSchoolCode";
        public static final String ALIAS_SSC_ALT_SYS_CODE = "all-ssc-AlternateSystemCode";
        public static final String ALIAS_SSC_CALC_STATE_CRS_CODE = "DOE CALCULATED STATE CRS CODE";
        public static final String ALIAS_SSC_EXCLUDE = "all-ssc-ExcludefromStateReporting";
        public static final String ALIAS_SSC_INCLUDE_STD = "all-ssc-DeliveryorInclusionStudent";
        public static final String ALIAS_SSC_STD_DELIVERY_MODEL = "all-ssc-StudentDeliveryModel";
        public static final String ALIAS_SSC_STD_DELIVERY_MODEL_ADD = "all-ssc-AdditionalStudentDeliveryModel";
        public static final String ALIAS_SSC_STD_INCLUSION_CODE = "all-ssc-StudentInclusionCode";

        public static final ToolBeanColumn FIELD_ALT_SKL_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_ALT_SKL_CODE);
        public static final ToolBeanColumn FIELD_ALT_SYS_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_ALT_SYS_CODE);
        public static final ToolBeanColumn FIELD_CALC_STATE_CRS_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_CALC_STATE_CRS_CODE);
        public static final ToolBeanColumn FIELD_EXCLUDE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_EXCLUDE);
        public static final ToolBeanColumn FIELD_INCLUDE_STD =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_INCLUDE_STD);
        public static final ToolBeanColumn FIELD_STD_DELIVERY_MODEL =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_STD_DELIVERY_MODEL);
        public static final ToolBeanColumn FIELD_STD_DELIVERY_MODEL_ADD =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_STD_DELIVERY_MODEL_ADD);
        public static final ToolBeanColumn FIELD_STD_INCLUSION_CODE =
                new ToolBeanColumn(SisBeanPaths.STUDENT_SCHEDULE, ALIAS_SSC_STD_INCLUSION_CODE);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION = ToolStudentSchedule.FULL_DEFINITION
                .expand(FIELD_ALT_SKL_CODE,
                        FIELD_ALT_SYS_CODE,
                        FIELD_CALC_STATE_CRS_CODE,
                        FIELD_EXCLUDE,
                        FIELD_INCLUDE_STD,
                        FIELD_STD_DELIVERY_MODEL,
                        FIELD_STD_DELIVERY_MODEL_ADD,
                        FIELD_STD_INCLUSION_CODE);

        /**
         * Instantiates a new Student Schedule.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GAStudentSchedule(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }


        /**
         * Gets the calculated alternate school code.
         *
         * @return String
         */
        public String getAltSklCode() {
            return getValueString(FIELD_ALT_SKL_CODE);
        }

        /**
         * Gets the calculated alternate system code.
         *
         * @return String
         */
        public String getAltSysCode() {
            return getValueString(FIELD_ALT_SYS_CODE);
        }

        /**
         * Gets the calculated state course code.
         *
         * @return String
         */
        public String getCalcStateCrsCode() {
            return getValueString(FIELD_CALC_STATE_CRS_CODE);
        }

        /**
         * Gets the exclusion indicator.
         *
         * @return String
         */
        public String getExclude() {
            return getValueString(FIELD_EXCLUDE);
        }

        /**
         * Gets the student inclusion indicator.
         *
         * @return String
         */
        public String getIncludeStd() {
            return getValueString(FIELD_INCLUDE_STD);
        }

        /**
         * Gets the student delivery model.
         *
         * @return String
         */
        public String getStdDeliveryModel() {
            return getValueString(FIELD_STD_DELIVERY_MODEL);
        }

        /**
         * Gets the additional student delivery model.
         *
         * @return String
         */
        public String getStdDeliveryModelAdd() {
            return getValueString(FIELD_STD_DELIVERY_MODEL_ADD);
        }

        /**
         * Gets the student inclusion code.
         *
         * @return String
         */
        public String getStdInclusionCode() {
            return getValueString(FIELD_STD_INCLUSION_CODE);
        }
    }


    /**
     * The Class Transcript.
     */
    public static class GATranscript extends ToolBean.ToolTranscript {
        public static final String ALIAS_TRN_EXCLUDE = "all-trn-ExcludefromStateReporting";
        public static final String ALIAS_TRN_OUT_OF_DISTR = "all-trn-OutOfDistrict";

        public static final ToolBeanColumn FIELD_EXCLUDE_FROM_REPORTING =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_EXCLUDE);
        public static final ToolBeanColumn FIELD_OUT_OF_DISTR =
                new ToolBeanColumn(SisBeanPaths.STUDENT_TRANSCRIPT, ALIAS_TRN_OUT_OF_DISTR);

        @SuppressWarnings("hiding")
        public static final ToolBeanDefinition FULL_DEFINITION =
                ToolTranscript.FULL_DEFINITION.expand(
                        FIELD_EXCLUDE_FROM_REPORTING,
                        FIELD_OUT_OF_DISTR);

        /**
         * Instantiates a new transcript.
         *
         * @param columns ToolBeanDefinition
         * @param data Object[]
         */
        public GATranscript(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * Gets the exclude indicator.
         *
         * @return boolean
         */
        public boolean getExcludeIndicator() {
            return getValueLogical(FIELD_EXCLUDE_FROM_REPORTING);
        }

        /**
         * Gets the out of district indicator.
         *
         * @return String
         */
        public String getOutOfDistrict() {
            return getValueString(FIELD_OUT_OF_DISTR);
        }
    }

    /**
     * RetrieveAlternateSchoolCode retrieves the value from the Program Participation table.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAlternateSchoolCode implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-ALT-SKL-CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";
            StudentScheduleSpan span = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (span != null) {
                GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                if (ssc != null) {
                    value = ssc.getAltSklCode();
                } else if (StringUtils.isEmpty(value) && span.getExitChange() != null) {
                    GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                    value = scc.getAltSklCode();
                }
            }
            return value;
        }
    }

    /**
     * RetrieveAlternateSystemCode retrieves the value from the Program Participation table.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAlternateSystemCode implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-ALT-SYS-CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = "";
            StudentScheduleSpan span = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (span != null) {
                GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                if (ssc != null) {
                    value = ssc.getAltSysCode();
                } else if (StringUtils.isEmpty(value) && span.getExitChange() != null) {
                    GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                    value = scc.getAltSysCode();
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the class start date or end date, as specified by the field parameter.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveClassDate implements ToolsSharedContainer.FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-DATE";

        private final String CLASS_END_DATE = "END";
        private final String CLASS_START_DATE = "START";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;
            String param = (String) field.getParameter();
            Object value = null;
            ToolTranscript transcript = scEntity.getCurrentTrancript(false);
            StudentScheduleSpan schSpan = scEntity.getCurrentStudentScheduleSpan();
            if (CLASS_END_DATE.equals(param)) {
                if (schSpan != null) {
                    // set the end date to the schedule span exit date.
                    PlainDate exitDate = schSpan.getExitDate();

                    // NOT NECESSARY SINCE ExitDate will always be less than
                    // or equal ExitChange
                    // if there is a schedule change, and its date is before
                    // the
                    // schedule span exit date, use that as the end date.
                    if (schSpan.getExitChange() != null
                            && schSpan.getExitChange().getEffectiveDate().before(exitDate)) {
                        exitDate = schSpan.getExitChange().getEffectiveDate();
                    }
                    value = exitDate;
                } else if (transcript != null && transcript.getSection(getBroker()) != null) {
                    PlainDate endDate =
                            transcript.getSection(getBroker()).getScheduleTerm(getBroker()).getEndDate(getBroker());
                    if (endDate != null) {
                        value = endDate;
                    }
                }
            } else if (CLASS_START_DATE.equals(param)) {
                if (schSpan != null) {
                    value = schSpan.getEntryDate();
                } else if (transcript != null && transcript.getSection(getBroker()) != null) {
                    PlainDate startDate =
                            transcript.getSection(getBroker()).getScheduleTerm(getBroker()).getStartDate(getBroker());
                    if (startDate != null) {
                        value = startDate;
                    }
                }
            }
            return value;
        }
    }

    /**
     * RetrieveCoTeacher retrieves the co-teacher name.
     *
     * @author Follett Software Company
     */
    protected class RetrieveCoTeacher implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-CO-TEACH";

        List<String> EIP_DELIVERY_MODEL_CODES = Arrays.asList("2", "4");
        List<String> ESOL_DELIVERY_CODES = Arrays.asList("1", "2");

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = CONST_NO;
            String teacherRole = STRING_EMPTY;
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;

            String eipDeliveryModel = entity.getFieldValue(EXPORT_FIELD_EIP_DELIVERY_MODEL);
            String esolDelivery = entity.getFieldValue(EXPORT_FIELD_ESOL_DELIVERY);
            if ("3".equals(scEntity.getFieldValue(EXPORT_FIELD_INCLUSION))
                    || "1".equals(scEntity.getFieldValue(EXPORT_FIELD_SE_DELIVERY_MODEL))
                    || (EIP_DELIVERY_MODEL_CODES.contains(eipDeliveryModel)
                            && !ESOL_DELIVERY_CODES.contains(esolDelivery))) {
                return value;
            }

            GASection currentSection = (GASection) scEntity.getCurrentSection();
            if (currentSection != null) {
                scEntity.m_teachers =
                        ((GAStudentCourseFileV3) data).getScheduleTeachers(currentSection, false, true, false);
                if (scEntity.m_teachers != null && !scEntity.m_teachers.isEmpty()) {
                    for (int idx = 0; idx < scEntity.m_teachers.size(); idx++) {
                        ToolScheduleTeacher teacher = scEntity.m_teachers.get(idx);

                        if (teacher != null) {
                            teacherRole = teacher.getRole();
                            if (CO_TEACHER.equals(teacherRole)) {
                                value = CONST_YES;
                            }
                        }
                    }
                }
            }
            return value;
        }
    }


    /**
     * Retrieve the total credit multiplied by 1000 as a string.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCredit implements FieldRetriever {
        private static final String CALC_ID = "GA-CRS-CREDIT";

        /**
         * Constants
         */
        private static final String EXPORT_FIELD_ALPHA_GRADE = "Alpha Grade";
        DecimalFormat m_format = new DecimalFormat("0000");

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            String alphaGradeValue = entity.getFieldValue(EXPORT_FIELD_ALPHA_GRADE);
            GASection currentSection = (GASection) ((StudentCourseFileEntity) entity).getCurrentSection();
            GATranscript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(true);
            if (currentSection != null) {
                BigDecimal cskCredit = currentSection.getCredit();
                if (cskCredit != null && cskCredit.compareTo(BigDecimal.ZERO) == 0) {
                    return value;
                }
                if (transcript != null) {
                    BigDecimal credit = transcript.getTotalCredit();
                    if (credit != null) {
                        value = m_format.format((long) (credit.doubleValue() * 1000));
                    }
                }
            }
            return "Z".equals(alphaGradeValue) ? null : "0000".equals(value) ? "0" : value;
        }
    }


    /**
     * Returns grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGrade implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CRS-GRADE";

        private static final String GRADE_PARAM_LETTER = "LETTER";
        private static final String GRADE_PARAM_NUMERIC = "NUMERIC";

        GradesManager m_gradesManager = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ToolTranscript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(true);
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            ToolStudentScheduleChange change = null;
            if (currentSpan != null) {
                change = currentSpan.getExitChange();
            }
            String parameter = (String) field.getParameter();
            Object returnObject = null;
            if (transcript != null) {
                m_gradesManager = new GradesManager(data.getBroker());

                // Check ungraded course.
                // GradeScale scale =
                // m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                String finalGrade = transcript.getFinalGrade();
                boolean isNumber = false;
                // See if the grade in the final grade column is a grade scale value
                // (letter grade).
                if (!StringUtils.isEmpty(finalGrade))// && scale != null)
                {
                    String regex = REGEX_NUMERIC;
                    isNumber = finalGrade.matches(regex);
                }

                // Determine which grade to return.
                if (GRADE_PARAM_LETTER.equals(parameter)) {
                    if (!isNumber) {
                        if (!StringUtils.isEmpty(finalGrade)) {
                            returnObject = finalGrade;
                        } else if (change != null) {
                            returnObject = "Z";
                        }
                    }
                } else if (GRADE_PARAM_NUMERIC.equals(parameter)) {
                    if (isNumber) {
                        returnObject = finalGrade;
                    }
                }
            } else if (GRADE_PARAM_LETTER.equals(parameter) && change != null) {
                returnObject = "Z";
            }

            return returnObject;
        }
    }

    /**
     * Returns grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        private static final String CALC_ID = "GRADE-LEVEL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ToolTranscript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(false);
            String returnObject = null;
            if (transcript != null) {
                returnObject = transcript.getGradeLevel();
            } else {
                GAStudent std = (GAStudent) entity.getBean();
                String grade = std.getGradeLevel();
                if (!StringUtils.isEmpty(grade)) {
                    returnObject = data.getDictionaryExtractor().lookupStateValue(ToolStudent.FIELD_GRADE_LEVEL, grade);
                }
            }
            return returnObject;
        }
    }


    /**
     * RetrievePrimaryArea retrieves the value of Primary Area from the Student. This value is
     * returned blank if the
     * Student's sped exit date is before or on the FTE Count date
     *
     * @author Follett Software Company
     */
    protected class RetrievePrimaryArea implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-PRIMARY-AREA";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = EMPTY_STRING;
            GAStudent student = (GAStudent) entity.getBean();
            String seDeliveryModel = entity.getFieldValue(EXPORT_FIELD_SE_DELIVERY_MODEL);
            if (!StringUtils.isEmpty(seDeliveryModel)) {
                String primaryExp = student.getPrimaryExceptionality();
                if (!StringUtils.isEmpty(primaryExp)) {
                    value = data.getDictionaryExtractor().lookupStateValue(GAStudent.FIELD_PRIMARY_EXCEPTIONALITY,
                            primaryExp);
                }
            }
            return value;
        }
    }

    /**
     * RetrievePrimaryTeacherFirstName retrieves first name of the primary teacher name.
     *
     * @author Follett Software Company
     */
    protected class RetrievePrimaryTeacherName implements FieldRetriever {
        private static final String CALC_ID = "GA-PRI-TEACH";

        private static final String CALC_PARAM_FIRST = "GA-PRI-TEACH-FIRST";
        private static final String CALC_PARAM_LAST = "GA-PRI-TEACH-LAST";

        Map<String, String[]> m_mapValues = new HashMap();

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = STRING_EMPTY;
            String[] nameArray = null;
            GASection section = (GASection) ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                if (m_mapValues.containsKey(section.getOid())) {
                    nameArray = m_mapValues.get(section.getOid());
                } else {
                    if (section.getStaffView() != null
                            && (section.getStaffView().contains(STRING_COMMA))) {

                        Collection<ToolScheduleTeacher> teachers = section.getTeacherSections(getBroker());
                        if (teachers.size() > 1) {
                            for (ToolScheduleTeacher scheduleTeacher : teachers) {
                                if (scheduleTeacher.getPrimaryTeacherIndicator() == true) {
                                    nameArray = scheduleTeacher.getStaffNameView().split(STRING_COMMA);
                                    break;
                                }
                            }
                        }
                        if (nameArray == null) {
                            nameArray = section.getStaffView().split(STRING_SEMICOLON);
                            nameArray = nameArray[0].split(STRING_COMMA);
                        }

                    }
                    m_mapValues.put(section.getOid(), nameArray);
                }
                if ((field.getParameter().equals(CALC_PARAM_FIRST)) && (nameArray != null)
                        && (nameArray.length > 1)) {
                    value = nameArray[1];
                }
                if ((field.getParameter().equals(CALC_PARAM_LAST)) && (nameArray != null)
                        && (nameArray.length > 0)) {
                    value = nameArray[0];
                }
            }
            return value;
        }
    }

    /**
     * Generate and return a record type code in the format "Pnn" where "nn" is an integer 1-75.
     *
     * Do this by counting how many StudentSchedule records we have processed for the current
     * student.
     * When changing students, reset the count.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRecordType implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-RECORD";

        private DecimalFormat m_format = new DecimalFormat("00");
        private Collection<String> m_schedOids = new ArrayList<String>();
        private Map<String, Collection<String>> m_schedOidsMap = new HashMap<String, Collection<String>>();
        private String m_studentOid = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ToolSection section = ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                GAStudent currentStudent = (GAStudent) entity.getBean();
                m_studentOid = currentStudent.getOid();
                if (m_schedOidsMap.containsKey(m_studentOid)) {
                    m_schedOids = m_schedOidsMap.get(m_studentOid);
                    m_schedOids.add(section.getOid());
                    m_schedOidsMap.put(m_studentOid, m_schedOids);
                } else {
                    m_schedOids.clear();
                    m_schedOids.add(section.getOid());
                    m_schedOidsMap.put(m_studentOid, m_schedOids);
                }
            }
            return FIELD_PREFIX + m_format.format(m_schedOids.size());
        }
    }

    /**
     * Retrieve the reporting period.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveReportingPeriod implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-PERIOD";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            GAStudentCourseFileV3 gaStudentClassData = (GAStudentCourseFileV3) entity.getData();
            return gaStudentClassData.m_reportingPeriod;
        }
    }

    /**
     * Retrieve the school from the entity. This will be the
     * school on report date or last withdrawal.
     * Optionally, a student can have an override school code assigned.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-SKL";

        private final String SCHOOL_CODE = "SCHOOL_CODE";
        private final String SYSTEM_CODE = "SYSTEM_CODE";
        private final String FISCAL_YEAR = "FISC";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String param = (String) field.getParameter();
            GAStudent student = (GAStudent) entity.getBean();
            GASection section = (GASection) ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                GASchool school = section.getSchoolCourseSchool(getBroker());
                GAOrganization organization = null;
                if (SCHOOL_CODE.equals(param)) {
                    value = student.getOverrideSchoolCode();
                    if (StringUtils.isEmpty(value)) {
                        if (school != null) {
                            value = school.getSchoolCode();
                        }
                    }
                } else if (SYSTEM_CODE.equals(param) || FISCAL_YEAR.equals(param)) {
                    if (school != null) {
                        organization = school.getOrganization1(getBroker());
                    }
                    if (SYSTEM_CODE.equals(param)) {
                        value = organization.getDoeDistrict();
                    }
                    if (FISCAL_YEAR.equals(param) && organization != null
                            && organization.getCurrentContext(getBroker()) != null) {
                        int schoolYear = organization.getCurrentContext(getBroker()).getSchoolYear();
                        value = String.valueOf(schoolYear);
                    }
                }
            }
            return value;
        }
    }


    /**
     * Retrieve the section information from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSectionInfo implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-SECTION-INFO";

        private static final String ALT_ED = "ALT_ED";
        private static final String CLASS_PERIOD = "CLASS_PERIOD";
        private static final String COURSE_NAME = "COURSE_NAME";
        private static final String COURSE_NUM = "COURSE_NUM";
        private static final String COURSE_TEACH_ID = "COURSE_TEACH_ID";
        private static final String CREDIT_RECOVERY = "CREDIT_RECOVERY";
        private static final String DOE_PARAPRO = "PARAPRO";
        private static final String EIP_DELIVERY_MODEL = "EIP_DELIV";
        private static final String ENRICHMENT = "ENRICHMENT";
        private static final String ESOL_DELIV = "ESOL_DELIV";
        private static final String GIFTED_DELIVERY = "GIFTED_DELIVERY";
        private static final String IMMERSION_LANG = "IMMERSION_LANG";
        private static final String INCLUSION = "INCLUSION";
        private static final String LIEU_CRS = "LIEU_CRS";
        private static final String ONLINE_COURSE = "ONLINE_COURSE";
        private static final String SE_DELIVERY = "SE_DELIVERY";
        private static final String SECTION_NUM = "SECTION_NUM";
        private static final String WBL_CATEGORY = "WBL_CATEGORY";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String param = (String) field.getParameter();
            StudentScheduleSpan span = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            GAStudent student = (GAStudent) entity.getBean();
            GATranscript trn = ((StudentCourseFileEntity) entity).getCurrentTrancript(false);
            GASection mst = (GASection) ((StudentCourseFileEntity) entity).getCurrentSection();

            if (mst != null) {
                if (DOE_PARAPRO.equals(param)) {
                    int count = 0;
                    mst.getTeacherSections(getBroker());
                    Collection<GAScheduleTeacher> sectionTeachers =
                            ((GAStudentCourseFileV3) data).getScheduleTeachers(mst, true, true, true);
                    if (sectionTeachers != null) {
                        for (ToolScheduleTeacher sectionTeacher : sectionTeachers) {
                            String teacherRole = sectionTeacher.getRole();
                            String type = data.getDictionaryExtractor().lookupStateValue(ToolScheduleTeacher.FIELD_ROLE,
                                    teacherRole);
                            if (PARAPRO_STATE_CODE.equals(type)) {
                                count++;
                            }
                        }
                    }
                    value = String.valueOf(count);
                } else if (ONLINE_COURSE.equals(param)) {
                    if (student.getRemoteStudent()) {
                        return "Y";
                    }
                    if (mst != null) {
                        value = mst.getOnlineCourse();
                    }
                } else if (INCLUSION.equals(param)) {
                    if (span != null) {
                        GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                        GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                        String mstFederalInclusion =
                                getDictionaryExtractor().lookupFederalValue(GASection.FIELD_INCLUSION_CODE,
                                        mst.getInclusionCode());
                        if (!StringUtils.isEmpty(mstFederalInclusion)
                                && CODE_FEDERAL_INCLUSION.equalsIgnoreCase(mstFederalInclusion)) {
                            value = getDictionaryExtractor().lookupStateValue(GASection.FIELD_INCLUSION_CODE,
                                    mst.getInclusionCode());
                        }
                        if (scc != null) {
                            if (value == null && BooleanAsStringConverter.TRUE.equals(scc.getIncludeStd())) {
                                String inclusionCode = scc.getStdInclusionCode();
                                if (!StringUtils.isEmpty(inclusionCode)) {
                                    value = getDictionaryExtractor().lookupStateValue(
                                            GAStudentScheduleChange.FIELD_STD_INCLUSION_CODE,
                                            inclusionCode);
                                }
                            }
                        } else {
                            if (value == null && BooleanAsStringConverter.TRUE.equals(ssc.getIncludeStd())) {
                                String inclusionCode = ssc.getStdInclusionCode();
                                if (!StringUtils.isEmpty(inclusionCode)) {
                                    value = getDictionaryExtractor().lookupStateValue(
                                            GAStudentSchedule.FIELD_STD_INCLUSION_CODE,
                                            inclusionCode);
                                }
                            }
                        }
                    }
                } else if (SE_DELIVERY.equals(param)) {
                    String mstFederalDelivery = getDictionaryExtractor()
                            .lookupFederalValue(GASection.FIELD_DELIVERY_MODEL, mst.getDeliveryModel());
                    if (!StringUtils.isEmpty(mstFederalDelivery)
                            && CODE_FEDERAL_SE.equalsIgnoreCase(mstFederalDelivery)) {
                        value = getDictionaryExtractor().lookupStateValue(GASection.FIELD_DELIVERY_MODEL,
                                mst.getDeliveryModel());
                    }
                    if (span != null) {
                        GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                        GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                        if (scc != null) {
                            if (value == null) {
                                String mstLocalDelivery = getDictionaryExtractor().lookupReferenceCodeByColumn(
                                        GASection.FIELD_DELIVERY_MODEL,
                                        mst.getDeliveryModel(), ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                                if (!StringUtils.isEmpty(mstLocalDelivery) && mstLocalDelivery.equals(CODE_LOCAL_M)
                                        && BooleanAsStringConverter.TRUE.equals(scc.getIncludeStd())) {
                                    String inclusionCode = scc.getStdDeliveryModel();
                                    String inclusionFederal = null;
                                    if (!StringUtils.isEmpty(inclusionCode)
                                            && !StringUtils.isEmpty(inclusionFederal =
                                                    getDictionaryExtractor().lookupFederalValue(
                                                            GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                                            inclusionCode))
                                            && CODE_FEDERAL_SE.equals(inclusionFederal)) {
                                        value = getDictionaryExtractor().lookupStateValue(
                                                GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                                inclusionCode);
                                    }
                                }
                            }
                            String addValue = scc.getStdDeliveryModelAdd();
                            if (!StringUtils.isEmpty(addValue) && CODE_FEDERAL_SE
                                    .equals(getDictionaryExtractor().lookupFederalValue(
                                            GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL_ADD,
                                            addValue))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL_ADD,
                                        addValue);
                            }
                        } else {
                            if (value == null) {
                                String mstLocalDelivery = getDictionaryExtractor().lookupReferenceCodeByColumn(
                                        GASection.FIELD_DELIVERY_MODEL,
                                        mst.getDeliveryModel(), ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                                if (!StringUtils.isEmpty(mstLocalDelivery) && mstLocalDelivery.equals(CODE_LOCAL_M)
                                        && BooleanAsStringConverter.TRUE.equals(ssc.getIncludeStd())) {
                                    String inclusionCode = ssc.getStdDeliveryModel();
                                    String inclusionFederal = null;
                                    if (!StringUtils.isEmpty(inclusionCode)
                                            && !StringUtils.isEmpty(inclusionFederal =
                                                    getDictionaryExtractor().lookupFederalValue(
                                                            GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                                            inclusionCode))
                                            && CODE_FEDERAL_SE.equals(inclusionFederal)) {
                                        value = getDictionaryExtractor().lookupStateValue(
                                                GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                                inclusionCode);
                                    }
                                }
                            }
                            String addValue = ssc.getStdDeliveryModelAdd();
                            if (!StringUtils.isEmpty(addValue) && CODE_FEDERAL_SE
                                    .equals(getDictionaryExtractor().lookupFederalValue(
                                            GAStudentSchedule.FIELD_STD_DELIVERY_MODEL_ADD,
                                            addValue))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentSchedule.FIELD_STD_DELIVERY_MODEL_ADD,
                                        addValue);
                            }
                        }
                    }
                } else if (LIEU_CRS.equals(param)) {
                    value = BooleanAsStringConverter.TRUE.equals(mst.getInLieu()) ? CONST_YES : CONST_NO;
                } else if (GIFTED_DELIVERY.equals(param)) {
                    if (span != null) {
                        GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                        GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                        if (scc != null) {
                            String sccCalcValue = scc.getCalcStateCrsCode();
                            if (!StringUtils.isEmpty(sccCalcValue) && sccCalcValue.matches(CRS_NUM_PATTERN)
                                    && "2".equals(String.valueOf(sccCalcValue.charAt(3)))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                        scc.getStdDeliveryModel());
                                if (value == null) {
                                    value = getDictionaryExtractor().lookupStateValue(GASection.FIELD_DELIVERY_MODEL,
                                            mst.getDeliveryModel());
                                }
                            }
                            String addValue = scc.getStdDeliveryModelAdd();
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_GIFTED.equals(
                                            getDictionaryExtractor().lookupFederalValue(
                                                    GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL_ADD,
                                                    addValue))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL_ADD,
                                        addValue);
                            }
                        } else {
                            String sscCalcValue = ssc.getCalcStateCrsCode();
                            if (!StringUtils.isEmpty(sscCalcValue) && sscCalcValue.matches(CRS_NUM_PATTERN)
                                    && "2".equals(String.valueOf(sscCalcValue.charAt(3)))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                        ssc.getStdDeliveryModel());
                                if (value == null) {
                                    value = getDictionaryExtractor().lookupStateValue(GASection.FIELD_DELIVERY_MODEL,
                                            mst.getDeliveryModel());
                                }
                            }
                            String addValue = ssc.getStdDeliveryModelAdd();
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_GIFTED
                                            .equals(getDictionaryExtractor().lookupFederalValue(
                                                    GAStudentSchedule.FIELD_STD_DELIVERY_MODEL_ADD,
                                                    addValue))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentSchedule.FIELD_STD_DELIVERY_MODEL_ADD,
                                        addValue);
                            }
                        }
                    }
                } else if (IMMERSION_LANG.equals(param)) {
                    String code = mst.getImmersionLanguage();
                    if (!StringUtils.isEmpty(code)) {
                        value = getDictionaryExtractor().lookupStateValue(GASection.FIELD_IMMERS_LANG, code);
                        if (!StringUtils.isEmpty(value) && value.length() < field.getMaxLength()) {
                            int zerosCount = field.getMaxLength() - value.length();
                            for (int i = 0; i < zerosCount; i++) {
                                value = "0" + value;
                            }
                        }
                    }
                } else if (WBL_CATEGORY.equals(param)) {
                    if (mst != null) {
                        if (mst.getCourseWBLCategory()) {
                            String code = mst.getWBLCategory();
                            if (!StringUtils.isEmpty(code)) {
                                value = getDictionaryExtractor().lookupStateValue(GASection.FIELD_WBL_CATEGORY, code);
                            }
                        }
                    }
                } else if (EIP_DELIVERY_MODEL.equals(param)) {
                    String stdGrade = entity.getFieldValue(EXPORT_FIELD_GRADE);
                    if (!StringUtils.isEmpty(stdGrade) && !ILLEGAL_GRADES_FOR_EIP.contains(stdGrade)) {
                        if (span != null) {
                            GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                            GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                            if (scc != null) {
                                String sccCalcValue = scc.getCalcStateCrsCode();
                                if (!StringUtils.isEmpty(sccCalcValue) && sccCalcValue.matches(CRS_NUM_PATTERN)
                                        && "1".equals(String.valueOf(sccCalcValue.charAt(3)))) {
                                    value = getDictionaryExtractor().lookupStateValue(
                                            GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                            scc.getStdDeliveryModel());
                                    if (value == null) {
                                        value = getDictionaryExtractor().lookupStateValue(
                                                GASection.FIELD_DELIVERY_MODEL,
                                                mst.getDeliveryModel());
                                    }
                                }
                            } else {
                                String sscCalcValue = ssc.getCalcStateCrsCode();
                                if (!StringUtils.isEmpty(sscCalcValue) && sscCalcValue.matches(CRS_NUM_PATTERN)
                                        && "1".equals(String.valueOf(sscCalcValue.charAt(3)))) {
                                    value = getDictionaryExtractor().lookupStateValue(
                                            GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                            ssc.getStdDeliveryModel());
                                    if (value == null) {
                                        value = getDictionaryExtractor().lookupStateValue(
                                                GASection.FIELD_DELIVERY_MODEL,
                                                mst.getDeliveryModel());
                                    }
                                }
                            }
                        }
                    }
                } else if (CREDIT_RECOVERY.equals(param)) {
                    value = mst.getCreditRecovery();
                } else if (ALT_ED.equals(param)) {
                    value = mst.getAltEd();
                } else if (SECTION_NUM.equals(param)) {
                    if (trn != null) {
                        value = "000";
                    } else {
                        value = mst.getSectionNumber();
                    }
                } else if (CLASS_PERIOD.equals(param)) {
                    if (trn != null) {
                        value = "00";
                    } else {
                        OptionalInt minPeriod = mst.getMasterTerms(getBroker()).stream()
                                .mapToInt(mtm -> mtm.getSchedulePeriod(getBroker()).getNumber()).min();
                        if (minPeriod.isPresent()) {
                            value = String.valueOf(minPeriod.getAsInt());
                        }
                    }
                } else if (COURSE_NAME.equals(param)) {
                    if (trn != null) {
                        value = trn.getCourseDescription();
                    } else {
                        int maxLength = field.getMaxLength();
                        value = mst.getDescription();
                        if (value.length() > maxLength) {
                            value = value.substring(0, maxLength);
                        }
                    }
                } else if (COURSE_TEACH_ID.equals(param)) {
                    GAStaff primaryStaff = null;
                    if (mst != null && mst.getPrimaryStaff(getBroker()) != null) {
                        primaryStaff = (GAStaff) mst.getPrimaryStaff(getBroker());
                    }
                    if (primaryStaff != null) {
                        value = primaryStaff.getSSN();
                        DataDictionaryField dictionaryField =
                                getDataDictionary().findDataDictionaryFieldByAlias(GAStudent.ALIAS_DOE_REMOTE_STUDENT);
                        if (dictionaryField != null && value != null && !value.startsWith(CODE_777)
                                && !value.startsWith(CODE_770) &&
                                student.getRemoteStudent()) {
                            value = "444444444";
                        }
                    }
                } else if (ESOL_DELIV.equals(param)) {
                    // inactive courses
                    if (span != null) {
                        GAStudentScheduleChange scc = (GAStudentScheduleChange) span.getExitChange();
                        GAStudentSchedule ssc = (GAStudentSchedule) span.getSchedule();
                        GASection spanSection = (GASection) span.getSection();
                        if (span.getExitChange() != null) {
                            String sccCalcValue = scc.getCalcStateCrsCode();
                            String mstDeliveryCode = spanSection.getDeliveryModel();
                            String mstDeliveryCodeLocal = "";
                            if (!StringUtils.isEmpty(mstDeliveryCode)) {
                                mstDeliveryCodeLocal =
                                        getDictionaryExtractor().lookupReferenceCodeByColumn(
                                                GASection.FIELD_DELIVERY_MODEL,
                                                mstDeliveryCode,
                                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                            }
                            if (!StringUtils.isEmpty(sccCalcValue) && sccCalcValue.matches(CRS_NUM_PATTERN)
                                    && "0".equals(String.valueOf(sccCalcValue.charAt(3)))) {
                                if (!StringUtils.isEmpty(mstDeliveryCodeLocal)) {
                                    if ("M".equals(mstDeliveryCodeLocal)) {
                                        String sccFederalValue =
                                                getDictionaryExtractor().lookupFederalValue(
                                                        GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                                        scc.getStdDeliveryModel());
                                        if (!StringUtils.isEmpty(sccFederalValue)
                                                && CODE_FEDERAL_ESOL.equalsIgnoreCase(sccFederalValue)) {
                                            value = getDictionaryExtractor().lookupStateValue(
                                                    GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                                    scc.getStdDeliveryModel());
                                        }
                                    } else if ("0".equals(mstDeliveryCodeLocal)) {
                                        String sccInclusionStdFieldValue = scc.getIncludeStd();
                                        if (!StringUtils.isEmpty(sccInclusionStdFieldValue)
                                                && BooleanAsStringConverter.TRUE
                                                        .equals(sccInclusionStdFieldValue)) {
                                            String mstFederalDelivery = getDictionaryExtractor().lookupFederalValue(
                                                    GASection.FIELD_DELIVERY_MODEL,
                                                    mstDeliveryCode);
                                            if (!StringUtils.isEmpty(mstFederalDelivery)
                                                    && CODE_FEDERAL_ESOL.equalsIgnoreCase(mstFederalDelivery)) {
                                                value = getDictionaryExtractor().lookupStateValue(
                                                        GASection.FIELD_DELIVERY_MODEL,
                                                        mstDeliveryCode);
                                            }
                                        }
                                    }
                                }
                            }
                            String addValue = scc.getStdDeliveryModelAdd();
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_ESOL.equals(
                                            getDictionaryExtractor().lookupFederalValue(
                                                    GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL_ADD,
                                                    addValue))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL_ADD,
                                        addValue);
                            }
                        } else {
                            String sscCalcValue = ssc.getCalcStateCrsCode();
                            String mstDeliveryCode = spanSection.getDeliveryModel();
                            String mstDeliveryCodeLocal = "";
                            if (!StringUtils.isEmpty(mstDeliveryCode)) {
                                mstDeliveryCodeLocal =
                                        getDictionaryExtractor().lookupReferenceCodeByColumn(
                                                GASection.FIELD_DELIVERY_MODEL,
                                                mstDeliveryCode,
                                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                            }
                            if (!StringUtils.isEmpty(sscCalcValue) && sscCalcValue.matches(CRS_NUM_PATTERN)
                                    && "0".equals(String.valueOf(sscCalcValue.charAt(3)))) {
                                if (!StringUtils.isEmpty(mstDeliveryCodeLocal)) {
                                    if ("M".equals(mstDeliveryCodeLocal)) {
                                        String sscFederalDelivery =
                                                getDictionaryExtractor().lookupFederalValue(
                                                        GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                                        ssc.getStdDeliveryModel());
                                        if (!StringUtils.isEmpty(sscFederalDelivery)
                                                && CODE_FEDERAL_ESOL.equalsIgnoreCase(sscFederalDelivery)) {
                                            value = getDictionaryExtractor().lookupStateValue(
                                                    GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                                    ssc.getStdDeliveryModel());
                                        }
                                    } else if ("0".equals(mstDeliveryCodeLocal)) {
                                        if (span.getSchedule() != null) {
                                            String sccInclusionStdFieldValue = ssc.getIncludeStd();
                                            if (!StringUtils.isEmpty(sccInclusionStdFieldValue)
                                                    && BooleanAsStringConverter.TRUE
                                                            .equals(sccInclusionStdFieldValue)) {
                                                String mstFederalDelivery = getDictionaryExtractor().lookupFederalValue(
                                                        GASection.FIELD_DELIVERY_MODEL,
                                                        mstDeliveryCode);
                                                if (!StringUtils.isEmpty(mstFederalDelivery)
                                                        && CODE_FEDERAL_ESOL.equalsIgnoreCase(mstFederalDelivery)) {
                                                    value = getDictionaryExtractor().lookupStateValue(
                                                            GASection.FIELD_DELIVERY_MODEL,
                                                            mstDeliveryCode);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            String addValue = ssc.getStdDeliveryModelAdd();
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_ESOL
                                            .equals(getDictionaryExtractor().lookupFederalValue(
                                                    GAStudentSchedule.FIELD_STD_DELIVERY_MODEL_ADD,
                                                    addValue))) {
                                value = getDictionaryExtractor().lookupStateValue(
                                        GAStudentSchedule.FIELD_STD_DELIVERY_MODEL_ADD,
                                        addValue);
                            }
                        }
                    }
                } else if (COURSE_NUM.equals(param)) {
                    if (trn != null) {
                        if (trn != null) {
                            if (trn.getUserDescriptionIndicator()
                                    && !StringUtils.isEmpty(trn.getEquivalentSchoolCourseNumber())) {
                                value = trn.getEquivalentSchoolCourseNumber();
                            } else {
                                value = trn.getSchoolCourseNumber();
                            }
                        }
                    } else if (span != null) {
                        if (span.getExitChange() != null) {
                            value = ((GAStudentScheduleChange) span.getExitChange()).getCalcStateCrsCode();
                        } else {
                            value = ((GAStudentSchedule) span.getSchedule()).getCalcStateCrsCode();
                        }

                    }
                } else if (ENRICHMENT.equals(param)) {
                    if (span != null) {
                        String federalValue = null;
                        if (span.getExitChange() != null) {
                            String deliveryCode =
                                    ((GAStudentScheduleChange) span.getExitChange()).getStdDeliveryModel();
                            federalValue = getDictionaryExtractor().lookupFederalValue(
                                    GAStudentScheduleChange.FIELD_STD_DELIVERY_MODEL,
                                    deliveryCode);
                        } else if (span.getSchedule() != null) {
                            String deliveryCode = ((GAStudentSchedule) span.getSchedule()).getStdDeliveryModel();
                            federalValue = getDictionaryExtractor().lookupFederalValue(
                                    GAStudentSchedule.FIELD_STD_DELIVERY_MODEL,
                                    deliveryCode);
                        }
                        if (CODE_FEDERAL_ENRICHMENT.equals(federalValue)) {
                            value = CONST_YES;
                        } else {
                            value = CONST_NO;
                        }
                    }
                }
            }
            return value;
        }
    }


    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-CLEAN";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) entity.getBean().getFieldValueByColumnName(field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(STRING_EMPTY);
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = STRING_EMPTY;
            }

            return cleanValue;
        }
    }


    /**
     * Retrieve the student's attendance.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudent90 implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-90";

        private Map<String, Map<PlainDate, Set<String>>> m_attMap = new HashMap();
        private Map<String, Map<PlainDate, Set<String>>> m_patMap = new HashMap();

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            GAStudent student = (GAStudent) entity.getBean();
            GASection currentSection = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (currentSpan != null) {
                currentSection = (GASection) currentSpan.getSection();
            }
            if (currentSection != null) {
                TreeSet<ToolSchoolCalendarDate> sectionDates =
                        currentSection.getCalendarDates(m_schoolYearStartDate, m_reportDate, getBroker());
                int numberOfMemberPeriods = 0;
                int numberOfAbsencePeriods = 0;
                int numberOfSectionPeriods = 0;
                for (ToolSchoolCalendarDate schoolDate : sectionDates) {
                    if (schoolDate.getInSessionIndicator()) {
                        Set<ToolSchedulePeriod> periods = currentSection.getSectionPeriods(m_broker, schoolDate);
                        if (periods != null) {
                            numberOfSectionPeriods += periods.size();
                            if (!currentSpan.getEntryDate().after(schoolDate.getDate())
                                    && !currentSpan.getExitDate().before(schoolDate.getDate())) {

                                numberOfMemberPeriods += periods.size();

                                if (isAbsent(student, currentSection, schoolDate.getDate())) {
                                    numberOfAbsencePeriods += periods.size();
                                }
                            }
                        }
                    }
                }
                if (numberOfSectionPeriods > 0) {
                    value = (numberOfMemberPeriods - numberOfAbsencePeriods) / (double) numberOfSectionPeriods >= 0.895
                            ? "Y"
                            : "N";
                }
            }
            return value;
        }

        /**
         * Checks if is absent.
         *
         * @param student GAStudent
         * @param section GASection
         * @param date PlainDate
         * @return true, if is absent
         */
        private boolean isAbsent(GAStudent student, GASection section, PlainDate date) {
            boolean absentIndicator = false;
            Map<PlainDate, Set<String>> dateMap = null;
            if (((GASchool) student.getSchool(getBroker())).getPeriodAttendanceIndicator()) {
                dateMap = m_patMap.get(section.getOid());
                if (dateMap == null) {
                    dateMap = loadPatDateMap(section.getOid());
                    m_patMap.put(section.getOid(), dateMap);
                }
            } else {
                dateMap = m_attMap.get(section.getSchedule(getBroker()).getSchoolOid());
                if (dateMap == null) {
                    dateMap = loadAttDateMap(section.getSchedule(getBroker()).getSchoolOid());
                    m_attMap.put(section.getSchedule(getBroker()).getSchoolOid(), dateMap);
                }
            }

            Set<String> stdOids = dateMap.get(date);
            if (stdOids != null && stdOids.contains(student.getOid())) {
                absentIndicator = true;
            }
            return absentIndicator;
        }

        /**
         * Load att date map.
         *
         * @param schoolOid String
         * @return Map
         */
        private Map<PlainDate, Set<String>> loadAttDateMap(String schoolOid) {
            Map<PlainDate, Set<String>> results = new HashMap();

            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_schoolYearStartDate);
            criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
            criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, schoolOid);

            String[] columns = new String[] {StudentAttendance.COL_DATE, StudentAttendance.COL_STUDENT_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);
            query.addOrderByAscending(StudentAttendance.COL_DATE);
            query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    PlainDate date = new PlainDate((java.util.Date) row[0]);
                    String stdOid = (String) row[1];

                    Set<String> stdOids = results.get(date);
                    if (stdOids == null) {
                        stdOids = new HashSet();
                        results.put(date, stdOids);
                    }
                    stdOids.add(stdOid);
                }
            } finally {
                iterator.close();
            }
            return results;
        }

        /**
         * Load pat date map.
         *
         * @param sectionOid String
         * @return Map
         */
        private Map<PlainDate, Set<String>> loadPatDateMap(String sectionOid) {
            Map<PlainDate, Set<String>> results = new HashMap();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, sectionOid);
            criteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

            String[] columns = new String[] {StudentPeriodAttendance.COL_DATE, StudentPeriodAttendance.COL_STUDENT_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(StudentPeriodAttendance.class, columns, criteria);
            query.addOrderByAscending(StudentPeriodAttendance.COL_DATE);
            query.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    PlainDate date = new PlainDate((java.util.Date) row[0]);
                    String stdOid = (String) row[1];

                    Set<String> stdOids = results.get(date);
                    if (stdOids == null) {
                        stdOids = new HashSet();
                        results.put(date, stdOids);
                    }
                    stdOids.add(stdOid);
                }
            } finally {
                iterator.close();
            }
            return results;
        }
    }

    /**
     * Get the course section's additional teachers' days
     *
     * Parameters: "1", "2", or "3".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacher90 implements FieldRetriever {
        private static final String CALC_ID = "GA-TEACHER-INFO";

        private static final String CALC_PARAM_PRIM_90 = "PRIM_90";

        private Map<String, String> m_mapValues = new HashMap();

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String result = null;
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;
            GAStudentCourseFileV3 scData = (GAStudentCourseFileV3) data;
            GASection currentSection = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (currentSpan != null) {
                currentSection = (GASection) currentSpan.getSection();
            }
            if (currentSection != null) {
                String param = (String) field.getParameter();
                if (CALC_PARAM_PRIM_90.equals(param)) {
                    GAScheduleTeacher primTeacher = ((GAStudentCourseFileV3) data).getPrimaryTeacher(currentSection);
                    result = getValueByTeacher(scData, primTeacher, currentSection);
                } else {
                    int idx = Integer.parseInt(param);
                    scEntity.m_teachers =
                            ((GAStudentCourseFileV3) data).getScheduleTeachers(currentSection, false, true, false);
                    if (scEntity.m_teachers != null && idx < scEntity.m_teachers.size()) {
                        GAScheduleTeacher teacher = scEntity.m_teachers.get(idx);
                        result = getValueByTeacher(scData, teacher, currentSection);
                    }
                }
            }
            return result;
        }

        /**
         * Evaluate if membership greater than 90%.
         *
         * @param scData GAStudentCourseFileV3
         * @param teacher GAScheduleTeacher
         * @param currentSection GASection
         * @return "Y" or "N"
         */
        private String getValueByTeacher(GAStudentCourseFileV3 scData,
                                         GAScheduleTeacher teacher,
                                         GASection currentSection) {

            String value = null;
            if (teacher != null && currentSection != null) {
                String key = teacher.getOid() + "|" + currentSection.getOid();
                if (m_mapValues.containsKey(key)) {
                    value = m_mapValues.get(key);
                } else {
                    PlainDate classEntryDate = null;
                    int numberOfSectionPeriods = 0;
                    int numberOfTeacherPeriods = 0;
                    TreeSet<ToolSchoolCalendarDate> sectionDates =
                            currentSection.getCalendarDates(m_schoolYearStartDate, m_reportDate, getBroker());

                    if (teacher != null && teacher.getClassEntryDate() != null) {
                        classEntryDate = teacher.getClassEntryDate();
                    }

                    for (ToolSchoolCalendarDate schoolDate : sectionDates) {
                        if (schoolDate.getInSessionIndicator()) {
                            Set<ToolSchedulePeriod> periods = currentSection.getSectionPeriods(getBroker(), schoolDate);

                            if (periods != null) {
                                numberOfSectionPeriods += periods.size();
                            }
                        }
                    }

                    if (classEntryDate == null) {
                        numberOfTeacherPeriods += numberOfSectionPeriods;
                    } else {
                        for (ToolSchoolCalendarDate schoolDate : sectionDates) {
                            if (schoolDate.getInSessionIndicator()) {
                                if (!schoolDate.getDate().before(classEntryDate)) {
                                    Set<ToolSchedulePeriod> periods =
                                            currentSection.getSectionPeriods(getBroker(), schoolDate);
                                    if (periods != null) {
                                        numberOfTeacherPeriods += periods.size();
                                    }
                                }
                            }
                        }
                    }

                    if (numberOfSectionPeriods > 0) {
                        value = numberOfTeacherPeriods / (double) numberOfSectionPeriods >= 0.895 ? "Y" : "N";
                    }
                    m_mapValues.put(key, value);
                }
            }

            return value;
        }
    }

    protected class RetrieveTeacherCHW implements FieldRetriever {
        private static final String CALC_ID = "GA-MTC-CHW";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String result = null;
            GASection currentSection = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (currentSpan != null) {
                currentSection = (GASection) currentSpan.getSection();
            }
            if (currentSection != null) {
                GAScheduleTeacher primTeacher = ((GAStudentCourseFileV3) data).getPrimaryTeacher(currentSection);
                if (primTeacher != null) {
                    result = primTeacher.getCHWCertStateCode();
                }
            }
            return result;
        }
    }

    /**
     * Get the course section's additional teachers' ID
     *
     * Parameters: "1", "2", or "3"
     * Returns: The SSN of additional teacher 1, 2, or 3, respectively.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacherId implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-TEACHER";

        List<String> INCLUSION_CODES = Arrays.asList("3", "4", "5", "6", "7");
        List<String> EIP_DELIVERY_MODEL_CODES = Arrays.asList("2", "4");
        List<String> ESOL_DELIVERY_CODES = Arrays.asList("1", "2");

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;

            String inclusion = entity.getFieldValue(EXPORT_FIELD_INCLUSION);
            String eipDeliveryModel = entity.getFieldValue(EXPORT_FIELD_EIP_DELIVERY_MODEL);
            String esolDelivery = entity.getFieldValue(EXPORT_FIELD_ESOL_DELIVERY);
            if (INCLUSION_CODES.contains(inclusion) ||
                    (EIP_DELIVERY_MODEL_CODES.contains(eipDeliveryModel)
                            && !ESOL_DELIVERY_CODES.contains(esolDelivery))) {
                return "";
            }

            String result = null;
            String param = (String) field.getParameter();
            GASection section = (GASection) ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                scEntity.m_teachers = ((GAStudentCourseFileV3) data).getScheduleTeachers(section, false, true, false);
                int idx = Integer.parseInt(param);
                if (scEntity.m_teachers != null && idx < scEntity.m_teachers.size()) {
                    ToolScheduleTeacher teacher = scEntity.m_teachers.get(idx);
                    GAStaff staff = null;
                    if (teacher != null) {
                        staff = (GAStaff) teacher.getStaff(getBroker());
                        if (staff != null) {
                            result = staff.getSSN();
                            if (!StringUtils.isEmpty(result)) {
                                result = result.replace(STRING_HYPHEN, STRING_EMPTY);
                            }
                        }
                    }
                }
            }

            return result;
        }
    }


    /**
     * Convert the schedule term of the class section to a
     * code for the course period.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermCode implements FieldRetriever {
        private static final String CALC_ID = "GA-STD-CLS-TERM";

        private Map<String, String> m_termMap = new HashMap<String, String>();
        @SuppressWarnings("hiding")
        private Map<String, String> m_termCodesMap = null;

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever#getFieldValue(com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportEntity,
         *      com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String periodCode = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            ToolTranscript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(false);
            if (currentSpan != null) {
                ToolSection section = currentSpan.getSection();
                if (section != null) {
                    String termOid = section.getScheduleTermOid();
                    if (!StringUtils.isEmpty(termOid)) {
                        periodCode = m_termMap.get(termOid);
                        if (periodCode == null) {
                            ToolScheduleTerm term = section.getScheduleTerm(getBroker());
                            if (term != null) {
                                String baseMap = term.getBaseTermMap();
                                periodCode = m_termCodesMap.get(baseMap);
                                m_termMap.put(termOid, periodCode);
                            }
                        }
                    }
                }
            } else if (transcript != null) {
                String code = transcript.getTermCode();
                if (!StringUtils.isEmpty(code)) {
                    periodCode = data.getDictionaryExtractor().lookupStateValue(ToolTranscript.FIELD_TERM_CODE, code);
                }
            }
            return periodCode;
        }

        /**
         * initialize the term code map.
         */
        {
            m_termCodesMap = new HashMap<String, String>();
            m_termCodesMap.put("1", "Y1");
            m_termCodesMap.put("10", "S1");
            m_termCodesMap.put("01", "S2");
            m_termCodesMap.put("100", "T1");
            m_termCodesMap.put("010", "T2");
            m_termCodesMap.put("001", "T3");
            m_termCodesMap.put("1000", "N1");
            m_termCodesMap.put("0100", "N2");
            m_termCodesMap.put("0010", "N3");
            m_termCodesMap.put("0001", "N4");
            m_termCodesMap.put("10000", "X1");
            m_termCodesMap.put("01000", "X2");
            m_termCodesMap.put("00100", "X3");
            m_termCodesMap.put("00010", "X4");
            m_termCodesMap.put("00001", "X5");
            m_termCodesMap.put("100000", "X1");
            m_termCodesMap.put("010000", "X2");
            m_termCodesMap.put("001000", "X3");
            m_termCodesMap.put("000100", "X4");
            m_termCodesMap.put("000010", "X5");
            m_termCodesMap.put("000001", "X6");
        }
    }

    /*
     * General Constants
     */
    private static final char CHAR_COMMA = ',';
    private static final String CO_TEACHER = "Co-Teach";
    private static final String CODE_770 = "770";
    private static final String CODE_777 = "777";
    private static final String CODE_FEDERAL_ENRICHMENT = "ENRICHMENT";
    private static final String CODE_FEDERAL_ESOL = "ESOL";
    private static final String CODE_FEDERAL_GIFTED = "GIFTED";
    private static final String CODE_FEDERAL_INCLUSION = "INCLUSION";
    private static final String CODE_FEDERAL_SE = "SE";
    private static final String CODE_LOCAL_M = "M";
    private static final String CONST_NO = "N";
    private static final String CONST_YES = "Y";
    private static final String CRS_NUM_PATTERN = "\\d{2}\\.\\d{7}";
    private static final String FIELD_PREFIX = "P";
    private static final String PARAPRO_STATE_CODE = "P";
    private static final String STRING_COMMA = ",";
    private static final String STRING_EMPTY = "";
    private static final String STRING_HYPHEN = "-";
    private static final String STRING_SEMICOLON = ";";

    /*
     * Illegal name characters to be removed from strings.
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final Collection ILLEGAL_GRADES_FOR_EIP = Arrays.asList("06", "07", "08", "09", "10", "11", "12");
    private static final String REGEX_NUMERIC = "\\d+";

    /**
     * Export's fields
     */
    private static final String EXPORT_FIELD_GRADE = "Grade level";
    private static final String EXPORT_FIELD_INCLUSION = "Inclusion";
    private static final String EXPORT_FIELD_ESOL_DELIVERY = "ESOL Delivery";
    private static final String EXPORT_FIELD_EIP_DELIVERY_MODEL = "EIP Delivery Model";
    private static final String EXPORT_FIELD_SE_DELIVERY_MODEL = "SE Delivery Model";

    /*
     * Parameters
     */
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_REPORTING_PERIOD = "reportingPeriod";
    private static final String PARAM_TERM = "term";

    /*
     * Reference Table
     */
    private static final String REF_TABLE_SCHEDULE_TERM_CODES = "rtbSchTermCode";
    private static final String REF_TABLE_STAFF_TYPE_CODES = "rtbStaffType";

    /*
     * Member variables
     */
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected PlainDate m_reportDate;
    protected String m_reportingPeriod;
    protected PlainDate m_schoolYearStartDate;
    protected static ArrayList<String> m_selectedTermCodes;
    protected Set<String> m_staffParaCodes;
    protected Map<String, String> m_termCodesMap = new HashMap();

    /**
     * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // Initialize fields+
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadTermCodes();

            // Get selected Term Codes
            String rcdOidList = (String) getParameter(PARAM_TERM);
            HashSet<String> rcdOids = new HashSet(StringUtils.convertDelimitedStringToList(rcdOidList, CHAR_COMMA));
            m_selectedTermCodes = new ArrayList();

            for (String rcdOid : rcdOids) {
                String termCode = m_termCodesMap.get(rcdOid);
                m_selectedTermCodes.add(termCode);
            }

            m_dictionaryExtractor = new DictionaryExtractor(getBroker());
            ToolBean.setDictionaryExtractor(m_dictionaryExtractor);
            ToolBean.registerClass(GAScheduleTeacher.class);
            ToolBean.registerClass(GASchool.class);
            ToolBean.registerClass(GASection.class);
            ToolBean.registerClass(GAStaff.class);
            ToolBean.registerClass(GAStudent.class);
            ToolBean.registerClass(GAStudentScheduleChange.class);
            ToolBean.registerClass(GAStudentSchedule.class);
            ToolBean.registerClass(GATranscript.class);
            ToolBean.registerClass(GAOrganization.class);
            ToolBean.setPreference(ToolBean.PREFERENCE_HISTORICAL_CUTOFF_DATE, m_schoolYearStartDate);
            ToolBean.setPreference(ToolBean.PREFERENCE_QUERY_AS_OF_DATE, m_reportDate);

            EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                    .setCurrentContext(getCurrentContext())
                    .setEndDate(m_reportDate)
                    .setExcludeStudent(GAStudent.FIELD_EXCLUDE_FROM_REPORTING)
                    .setExcludeSection(GASection.FIELD_EXCLUDE_MST)
                    .setExcludeCourse(GASection.FIELD_EXCLUDE_CRS);
            if (isSchoolContext()) {
                spanCriteria.setSchoolOids(Arrays.asList(getSchool().getOid()));
            }
            X2Criteria inputCriteria = new X2Criteria();
            applyInputCriteria(inputCriteria, false, null);
            if (!inputCriteria.isEmpty()) {
                spanCriteria.setStudentLimitingCriteria(inputCriteria);
            }

            // Preloads
            X2Criteria sscCriteria = CriteriaHelper.buildStudentScheduleCriteria(spanCriteria);
            ToolBean.addAndCriteria(getBroker(), GAStudentSchedule.class, sscCriteria);
            ToolBean.load(getBroker(), getDictionaryExtractor(), GAStudentSchedule.class);

            X2Criteria sccCriteria = CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria);
            ToolBean.addAndCriteria(getBroker(), GAStudentScheduleChange.class, sccCriteria);
            ToolBean.load(getBroker(), getDictionaryExtractor(), GAStudentScheduleChange.class);

            List<String> studentOids = Streams.concat(
                    ToolBean.getCachedToolBeans(GAStudentSchedule.class).stream().map(ssc -> ssc.getStudentOid()),
                    ToolBean.getCachedToolBeans(GAStudentScheduleChange.class).stream().map(scc -> scc.getStudentOid()))
                    .distinct().collect(Collectors.toList());
            ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), GAStudent.class, studentOids,
                    applyInputSort(null));

            List<String> sectionOids = Streams.concat(
                    ToolBean.getCachedToolBeans(GAStudentSchedule.class).stream().map(ssc -> ssc.getSectionOid()),
                    ToolBean.getCachedToolBeans(GAStudentScheduleChange.class).stream().map(scc -> scc.getSectionOid()))
                    .distinct().collect(Collectors.toList());
            ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), GASection.class, sectionOids);
            ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolSection.CHILD_MASTER_TERMS);
            ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolSection.CHILD_TEACHER_SCHEDULES);

            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                    ToolStudent.CHILD_STUDENT_ENROLLMENTS);
            ToolBean.preload(getBroker(), getDictionaryExtractor(),
                    Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                    ToolStudent.CHILD_STUDENT_SCHOOLS);
            ToolBean.reload(getBroker(), getDictionaryExtractor(), null, ToolStudentSchedule.PARENT_STUDENT);
            ToolBean.reload(getBroker(), getDictionaryExtractor(), null, ToolStudentScheduleChange.PARENT_STUDENT);

            // transcripts for this context year
            X2Criteria trnCriteria = new X2Criteria();
            trnCriteria.addEqualTo(ToolTranscript.FIELD_DISTRICT_CONTEXT_OID.resolve(getDictionaryExtractor()),
                    getCurrentContext().getOid());
            trnCriteria.addNotEqualTo(GATranscript.FIELD_EXCLUDE_FROM_REPORTING.resolve(getDictionaryExtractor()),
                    BooleanAsStringConverter.TRUE);
            ToolBean.addAndCriteria(getBroker(), GATranscript.class, trnCriteria);
            ToolBean.preload(getBroker(), getDictionaryExtractor(), null, ToolTranscript.PARENT_STUDENT);

            // load students using filterable
            setFilterable(FilterableFactory.createFilterableToolBeans(ToolBean.getCachedToolBeans(GAStudent.class)));

            setEntityClass(StudentCourseFileEntity.class);

            // Load in maps for sectionOids by period

            // Add any retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveClassDate.CALC_ID, new RetrieveClassDate());
            calcs.put(RetrieveReportingPeriod.CALC_ID, new RetrieveReportingPeriod());
            calcs.put(RetrieveRecordType.CALC_ID, new RetrieveRecordType());
            calcs.put(RetrieveSchool.CALC_ID, new RetrieveSchool());
            calcs.put(RetrieveTermCode.CALC_ID, new RetrieveTermCode());
            calcs.put(RetrieveTeacherId.CALC_ID, new RetrieveTeacherId());
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveCoTeacher.CALC_ID, new RetrieveCoTeacher());
            calcs.put(RetrievePrimaryTeacherName.CALC_ID, new RetrievePrimaryTeacherName());
            calcs.put(RetrieveAlternateSystemCode.CALC_ID, new RetrieveAlternateSystemCode());
            calcs.put(RetrieveAlternateSchoolCode.CALC_ID, new RetrieveAlternateSchoolCode());
            calcs.put(RetrieveSectionInfo.CALC_ID, new RetrieveSectionInfo());
            calcs.put(RetrievePrimaryArea.CALC_ID, new RetrievePrimaryArea());
            calcs.put(RetrieveGrade.CALC_ID, new RetrieveGrade());
            calcs.put(RetrieveCredit.CALC_ID, new RetrieveCredit());
            calcs.put(RetrieveStudent90.CALC_ID, new RetrieveStudent90());
            calcs.put(RetrieveTeacher90.CALC_ID, new RetrieveTeacher90());
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            calcs.put(RetrieveTeacherCHW.CALC_ID, new RetrieveTeacherCHW());
            super.addCalcs(calcs);
        }
    }

    /**
     * Gets the para codes.
     *
     * @return Set
     */
    private Set<String> getParaCodes() {
        if (m_staffParaCodes == null) {
            m_staffParaCodes = new HashSet();
            X2Criteria staffTypeExclusionCriteria = new X2Criteria();
            staffTypeExclusionCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TABLE_STAFF_TYPE_CODES);
            staffTypeExclusionCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, PARAPRO_STATE_CODE);
            BeanQuery staffTypeExclusionQuery = new BeanQuery(ReferenceCode.class, staffTypeExclusionCriteria);
            Collection<ReferenceCode> staffTypeExclusionCodes =
                    getBroker().getCollectionByQuery(staffTypeExclusionQuery);
            for (ReferenceCode staffType : staffTypeExclusionCodes) {
                m_staffParaCodes.add(staffType.getCode());
            }

        }
        return m_staffParaCodes;
    }

    /**
     * Gets the schedule teachers.
     *
     * @param section ToolSection
     * @param includePrimary boolean
     * @param includeSecondary boolean
     * @param includePara boolean
     * @return List
     */
    private List<GAScheduleTeacher> getScheduleTeachers(ToolSection mst,
                                                        boolean includePrimary,
                                                        boolean includeSecondary,
                                                        boolean includePara) {
        List<GAScheduleTeacher> teachers = new LinkedList();
        Collection<ToolScheduleTeacher> candidates = mst.getTeacherSections(getBroker());
        if (candidates != null) {
            for (ToolScheduleTeacher candidate : candidates) {
                if (getParaCodes().contains(candidate.getStaff(getBroker()).getStaffType())) {
                    if (includePara) {
                        teachers.add((GAScheduleTeacher) candidate);
                    }
                } else {
                    if (candidate.getPrimaryTeacherIndicator() && includePrimary) {
                        teachers.add((GAScheduleTeacher) candidate);
                    }
                    if (!candidate.getPrimaryTeacherIndicator() && includeSecondary) {
                        teachers.add((GAScheduleTeacher) candidate);
                    }
                }
            }
        }
        teachers.sort(new Comparator() {
            @Override
            public int compare(Object o1, Object o2) {
                GAScheduleTeacher t1 = (GAScheduleTeacher) o1;
                GAScheduleTeacher t2 = (GAScheduleTeacher) o2;
                return t1.getOid().compareTo(t2.getOid());
            }
        });
        return teachers;
    }

    /**
     * Gets the primary teacher.
     *
     * @param section ToolSection
     * @return GAScheduleTeacher teacher
     */
    private GAScheduleTeacher getPrimaryTeacher(ToolSection mst) {
        List<GAScheduleTeacher> teachers = getScheduleTeachers(mst, true, false, false);
        return teachers != null && !teachers.isEmpty() ? teachers.iterator().next() : null;
    }

    /**
     * Initialize fields. Translate aliases to java names.
     */
    private void initializeFields() {
        if (m_currentContext == null) {
            Organization organization = OrganizationManager.getRootOrganization(getBroker());
            m_currentContext = organization.getCurrentContext();
        }

        m_schoolYearStartDate = m_currentContext.getStartDate();

        // Load Input Parameters
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate.before(m_currentContext.getStartDate())) {
            m_reportDate = m_currentContext.getStartDate();
        }

        if (m_reportDate.after(m_currentContext.getEndDate())) {
            m_reportDate = m_currentContext.getEndDate();
        }
        //
        m_reportingPeriod = (String) getParameter(PARAM_REPORTING_PERIOD);
    }

    /**
     * Load in all the Schedule Term Codes.
     */
    private void loadTermCodes() {
        X2Criteria termCodesCriteria = new X2Criteria();
        termCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TABLE_SCHEDULE_TERM_CODES);
        BeanQuery termCodesQuery = new BeanQuery(ReferenceCode.class, termCodesCriteria);
        Collection<ReferenceCode> termCodes = getBroker().getCollectionByQuery(termCodesQuery);

        for (ReferenceCode referenceCode : termCodes) {
            m_termCodesMap.put(referenceCode.getOid(), referenceCode.getCode());
        }
    }
}
