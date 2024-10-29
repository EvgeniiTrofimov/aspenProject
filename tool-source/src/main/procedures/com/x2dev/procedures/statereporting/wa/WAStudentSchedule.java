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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Export procedure for WA Student Schedule.
 *
 * @author X2 Development Corporation
 */

public class WAStudentSchedule extends StateReportData {
    /**
     * Entity class for WA Student Schedule export.
     *
     */
    public static class WAStudentScheduleEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        WAStudentSchedule m_sdData;
        List<StudentScheduleSpan> m_scheduleSpans;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WAStudentScheduleEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the StudentScheduleSpan record for the current index.
         *
         * @return StudentScheduleSpan
         */
        public StudentScheduleSpan getScheduleSpan() {
            return m_scheduleSpans.get(getCurrentRow());
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
            StudentScheduleSpan span = getScheduleSpan();
            if (span != null) {
                name += span.getSection().getSchoolCourse().getSchool().getName();
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

            m_sdData = (WAStudentSchedule) data;

            // Remove schedule spans within enrollment spans that are no-show.
            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans((Student) bean, m_sdData);

            Set<String> includedSections = new HashSet<String>(16);

            if (districtSpans.size() > 0) {
                // Remove schedule spans for the same master section. The export wants only one
                // entry per section.
                m_scheduleSpans = m_sdData.m_helper.getStudentScheduleSpans((SisStudent) bean);
                Iterator<StudentScheduleSpan> spans = m_scheduleSpans.iterator();
                while (spans.hasNext()) {
                    StudentScheduleSpan schedSpan = spans.next();
                    MasterSchedule section = schedSpan.getSection();
                    if (m_sdData.includeSchool(section.getSchoolCourse().getSchoolOid())) {
                        String sectionOid = section.getOid();
                        if (includedSections.contains(sectionOid)) {
                            spans.remove();
                        } else {
                            includedSections.add(sectionOid);
                        }
                    } else {
                        spans.remove();
                    }
                }
            }

            if (m_scheduleSpans != null) {
                setRowCount(m_scheduleSpans.size());
            } else {
                setRowCount(0);
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
         * Look for spans with withdrawal codes that represent district withdrawal vs. indistrict
         * transfer.
         *
         * @param student Student
         * @param sdData WAStudentSchedule
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentSchedule sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                if (isMember(span)) {
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
                        withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode,
                                withdrawalCode, ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
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
                                                !sdData.getCurrentContext().getStartDate()
                                                        .after(currentSpan.m_exitDate))) {
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
            }
            if (currentSpan != null &&
                    (currentSpan.m_exitDate == null ||
                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
        }

        /**
         * Returns the first active date.
         *
         * @param span The span
         * @return PlainDate The first active date
         */
        private PlainDate getFirstActiveDate(StudentEnrollmentSpan span) {
            PlainDate firstActiveDate = null;

            if (span != null && span.getFirstActiveEnrollment() != null) {
                firstActiveDate = span.getFirstActiveEnrollment().getEnrollmentDate();
            }

            return firstActiveDate;
        }

        /**
         * Returns the last active date.
         *
         * @param span The span
         * @return PlainDate The last active date
         */
        private PlainDate getLastActiveDate(StudentEnrollmentSpan span) {
            PlainDate lastActiveDate = null;

            if (span != null && span.getFirstInactiveEnrollment() != null) {
                lastActiveDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
            }

            return lastActiveDate;
        }

        /**
         * Returns true if the span is a member.
         *
         * @param span The span
         *
         * @return boolean True if the span is a member
         */
        private boolean isMember(StudentEnrollmentSpan span) {
            PlainDate firstActiveDate = getFirstActiveDate(span);
            PlainDate lastActiveDate = getLastActiveDate(span);
            boolean isMember = true;

            if (firstActiveDate != null && lastActiveDate != null && firstActiveDate.equals(lastActiveDate)) {
                if (!m_sdData.m_memberOnEntryDate && !m_sdData.m_memberOnWithdrawalDate) {
                    isMember = false;
                } else if (m_sdData.m_memberOnEntryDate != m_sdData.m_memberOnWithdrawalDate) {
                    isMember = false;
                }
            }

            return isMember;
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
     * Returns the enrollment information from the current schedule span.
     */
    protected class RetrieveSchedule implements FieldRetriever {
        private final String PARAM_COURSE_ID = "COURSE_ID";
        private final String PARAM_SCHOOL_ID = "SCHOOL_ID";
        private final String PARAM_SECTION_ID = "SECTION_ID";
        private final String PARAM_TERM = "TERM";
        private final String PARAM_ALE_FUNDED = "ALE_FUNDED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentScheduleEntity wssEntity = ((WAStudentScheduleEntity) entity);
            StudentScheduleSpan span = ((WAStudentScheduleEntity) entity).getScheduleSpan();
            WAStudentSchedule wssData = (WAStudentSchedule) data;
            String parameter = (String) field.getParameter();
            Transcript transcript = span.getTranscript();
            X2BaseBean bean = wssEntity.getBean();
            SisStudent student = (SisStudent) bean;
            Object value = null;
            if (span != null) {
                if (PARAM_SCHOOL_ID.equals(parameter)) {
                    StudentEnrollment enrollment = null;
                    List<StudentEnrollmentSpan> spans = wssData.m_helper.getStudentEnrollmentSpans(student, true);
                    for (StudentEnrollmentSpan enrollSpan : spans) {
                        enrollment = enrollSpan.getFirstActiveEnrollment();
                    }
                    if (null != enrollment) {
                        value = enrollment.getFieldValueByBeanPath(m_fieldLocationOveride);
                    }

                    if (null == value) {
                        value = span.getSection().getSchoolCourse().getSchool().getSchoolId();
                    }
                } else if (PARAM_COURSE_ID.equals(parameter)) {
                    value = span.getSection().getSchoolCourse().getNumber();
                } else if (PARAM_SECTION_ID.equals(parameter)) {
                    value = span.getSection().getSectionNumber();
                } else if (PARAM_TERM.equals(parameter)) {
                    String termCode = span.getSection().getScheduleTerm().getCode();
                    value = data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                else if (PARAM_ALE_FUNDED.equals(parameter)) {
                    if (transcript != null) {
                        MasterSchedule masterSchedule = transcript.getMasterSchedule();
                        if (masterSchedule != null) {
                            SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();

                            if (schoolCourse != null) {
                                Course course = schoolCourse.getCourse();

                                if (course != null) {
                                    // value = tempMasterSchedule != null ?
                                    // tempMasterSchedule.getFieldValueByAlias(ALIAS_ALE_FUNDED):
                                    // null;
                                    String aLEFundedFlag = (String) course.getFieldValueByAlias(ALIAS_ALE_FUNDED);
                                    if (!StringUtils.isEmpty(aLEFundedFlag)
                                            && BooleanAsStringConverter.TRUE.equals(aLEFundedFlag)) {
                                        value = "Y";
                                    }
                                }
                            }
                        }
                    }

                    if (StringUtils.isEmpty((String) value)) {
                        value = "N";
                    }

                }
            }
            return value;
        }
    }

    /**
     * Validate term.
     *
     * @author Follett Software Company
     */
    protected class ValidateTerm implements FieldValidator {
        public static final String VAL_ID = "TERM-VAL";

        private final ArrayList<String> m_validCodes =
                new ArrayList(Arrays.asList("SEM1", "SEM2", "TRI1", "TRI2", "TRI3",
                        "Q1", "Q2", "Q3", "Q4", "ALLYR", "SIXWKT1", "SIXWKT2", "SIXWKT3", "SIXWKT4", "SIXWKT5",
                        "SIXWKT6", "TERM1of8",
                        "TERM2of8", "TERM3of8", "TERM4of8", "TERM5of8", "TERM6of8", "TERM7of8", "TERM8of8", "OTHER",
                        "SUM1", "SUM2"));

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && !m_validCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be appendix P codes: " + m_validCodes.toString(),
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /*
     * Constants: Aliases, Parameters, Fields
     */
    protected static final String ALIAS_ALE_FUNDED = "ALE FUNDED";
    protected static final String ALIAS_CONTENT_AREA = "DOE CONTENT AREA";
    protected static final String ALIAS_DESIGNATION_CODE = "DOE CRS DESIGNATION";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";

    protected static final String CODE_NO_SHOW = "NS";

    protected static final String PARAM_COURSES_STATEDEF = "stateDefCoursesOnly";
    protected static final String PARAM_REPORT_DATE = "reportDate";


    protected Map m_excludeSchool;
    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldContentArea;
    protected String m_fieldDesignationCode;
    protected String m_fieldLocationOveride;
    protected StudentHistoryHelper m_helper;
    protected boolean m_memberOnEntryDate;
    protected boolean m_memberOnWithdrawalDate;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected PlainDate m_reportDate;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_memberOnEntryDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(this.getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
        m_memberOnWithdrawalDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(this.getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        initializeFields();
        loadSchoolExcludeMap();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        Boolean stateDefCoursesOnly = (Boolean) getParameter(PARAM_COURSES_STATEDEF);

        // Update Student Schedule criteria to limit courses reported.
        if (stateDefCoursesOnly != null ? stateDefCoursesOnly.booleanValue() : false) {
            X2Criteria sscCriteria = m_helper.getStudentScheduleCriteria();
            X2Criteria sccCriteria = m_helper.getStudentScheduleChangeCriteria();
            sscCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldContentArea,
                    getBroker().getPersistenceKey());

            sccCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldContentArea,
                    getBroker().getPersistenceKey());
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(WAStudentScheduleEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SSCH-SCHEDULE", new RetrieveSchedule());
            super.addCalcs(calcs);

            HashMap<String, FieldValidator> vals = new HashMap<String, FieldValidator>();
            vals.put(ValidateTerm.VAL_ID, new ValidateTerm());
            super.addValidators(vals);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);
        m_fieldDesignationCode = translateAliasToJavaName(ALIAS_DESIGNATION_CODE, true);
        m_fieldContentArea = translateAliasToJavaName(ALIAS_CONTENT_AREA, true);
        m_fieldLocationOveride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);
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
