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

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA Student Course Section.
 *
 * @author X2 Development Corporation
 */

public class CAStudentCourseSection extends StateReportData {
    /**
     * Entity class for CA Student Course Section export.
     *
     */
    public static class CAStudentCourseSectionEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        List<StudentScheduleSpan> m_scheduleSpans;
        CAStudentCourseSection m_sdData;
        SisStudent m_student;
        List<Transcript> m_transcripts;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStudentCourseSectionEntity() {
            // public no argument constructor for dynamic instantiation.
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

            return name;
        }

        /**
         * Returns the schedule spans for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentScheduleSpan getScheduleSpan() {
            return m_scheduleSpans.get(getCurrentRow());
        }

        /**
         * Returns the Student record.
         *
         * @return StudentEnrollmentSpan
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Returns the Transcript record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public Transcript getTranscript() {
            return m_transcripts.get(getCurrentRow());
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

            m_sdData = (CAStudentCourseSection) data;
            m_student = (SisStudent) bean;

            if (PARAM_RECORD_TYPE_VALUE_SCSC.equals(m_sdData.getParameter(INPUT_PARAM_RECORD_TYPE_CODE))) {
                m_transcripts = m_sdData.getStudentTranscripts(m_student.getOid());
                setRowCount(m_transcripts == null
                        ? 0
                        : m_transcripts.size());
                return;
            }

            // if INPUT Param == SCSE
            List<StudentScheduleSpan> spans = m_sdData.m_helper.getStudentScheduleSpans(m_student);
            m_scheduleSpans = new LinkedList();
            for (StudentScheduleSpan span : spans) {
                if (!span.getEntryDate().after(m_sdData.m_reportDate)
                        && !span.getExitDate().before(m_sdData.m_reportDate)) {
                    m_scheduleSpans.add(span);
                }
            }
            setRowCount(m_scheduleSpans.size());
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
    }

    /**
     * Retrieve Record Type Code from the input parameter.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return getParameter(INPUT_PARAM_RECORD_TYPE_CODE);
        }
    }

    /**
     * Retrieve data from the student schedule record based on field parameter
     * This Retriever is for SCSC input parameter
     * Following fields is populated using this retriever
     *
     * <LI>ADM_REQ_CODE - UC/CSU Admission Requirement Code
     * <LI>COURSE_ID - Local Course ID
     * <LI>CREDITS_ATTEMPTED - Student Credits Attempted
     * <LI>CREDITS_EARNED - Student Credits Earned
     * <LI>CRS_SECTION_ID - Course Section ID
     * <LI>FINAL_GRADE - Student Course Final Grade
     * <LI>MARKING_PERIOD - Marking Period Code
     * <LI>OID - Local Record ID
     * <LI>SCHOOL_OF_CRS_DELIV - School of Course Delivery
     * <LI>TERM_CODE - Academic Term Code.
     *
     * @author X2 Development Corporation
     */
    protected class SectionCompletionFieldRetriver implements FieldRetriever {
        private static final String PARAM_ADM_REQ_CODE = "ADM_REQ_CODE";
        private static final String PARAM_COURSE_ID = "COURSE_ID";
        private static final String PARAM_CREDITS_ATTEMPTED = "CREDITS_ATTEMPTED";
        private static final String PARAM_CREDITS_EARNED = "CREDITS_EARNED";
        private static final String PARAM_CRS_SECTION_ID = "CRS_SECTION_ID";
        private static final String PARAM_FINAL_GRADE = "FINAL_GRADE";
        private static final String PARAM_MARKING_PERIOD = "MARKING_PERIOD";
        private static final String PARAM_OID = "OID";
        private static final String PARAM_SCHOOL_OF_CRS_DELIV = "SCHOOL_OF_CRS_DELIV";
        private static final String PARAM_TERM_CODE = "TERM_CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String fieldName = (String) field.getParameter();
            CAStudentCourseSectionEntity myEntity = (CAStudentCourseSectionEntity) entity;
            Transcript transcript = myEntity.getTranscript();
            if (PARAM_ADM_REQ_CODE.equals(fieldName)) {
                String code = (String) transcript.getSchoolCourse()
                        .getCourse()
                        .getFieldValueByBeanPath(m_fieldCollegeRequirement);
                if (code != null) {
                    return lookupReferenceCodeByBeanPath(Course.class, m_fieldCollegeRequirement, code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            if (PARAM_COURSE_ID.equals(fieldName)) {
                String nclb = null;
                if (transcript.getMasterSchedule() != null) {
                    nclb = (String) transcript.getMasterSchedule().getFieldValueByBeanPath(m_fieldNclbInsert);
                }
                return transcript.getSchoolCourse().getCourse().getNumber() + StringUtils.unNullify(nclb);
            }

            if (PARAM_CREDITS_ATTEMPTED.equals(fieldName) && isHighSchool(transcript.getSchool())) {
                if (!StringUtils.isEmpty(transcript.getPotentialCredit())) {
                    return new BigDecimal(transcript.getPotentialCredit());
                }

                return transcript.getEquivalentSchoolCourse() != null
                        ? transcript.getEquivalentSchoolCourse().getCredit()
                        : transcript.getSchoolCourse().getCredit();
            }

            if (PARAM_CREDITS_EARNED.equals(fieldName) && isHighSchool(transcript.getSchool())) {
                return transcript.getTotalCredit();
            }

            if (PARAM_CRS_SECTION_ID.equals(fieldName)) {
                String courseNumber = null;
                String sectionNumber = null;
                SchoolCourse csk = null;
                Course crs = null;
                MasterSchedule mst = null;
                String nclb = null;

                if (transcript != null && (mst = transcript.getMasterSchedule()) != null) {
                    nclb = (String) mst.getFieldValueByBeanPath(m_fieldNclbInsert);
                    sectionNumber = mst.getSectionNumber();

                    if ((csk = mst.getSchoolCourse()) != null && (crs = csk.getCourse()) != null) {
                        courseNumber = crs.getNumber();
                    }
                }

                return StringUtils.unNullify(courseNumber) + StringUtils.unNullify(nclb)
                        + StringUtils.unNullify(sectionNumber).replaceAll("-", "");
            }

            if (PARAM_FINAL_GRADE.equals(fieldName)) {
                String finalGrade = null;
                if (Arrays.asList("07", "08").contains(transcript.getGradeLevel())
                        && !StringUtils.isEmpty(m_gradeMiddleBeanPath)) {
                    finalGrade = (String) transcript.getFieldValueByBeanPath(m_gradeMiddleBeanPath);
                } else {
                    finalGrade = transcript.getFinalGrade();
                }
                return finalGrade;
            }
            if (PARAM_OID.equals(fieldName)) {
                SisStudent student = ((CAStudentCourseSectionEntity) entity).getStudent();
                return student.getOid() + "-" + transcript.getOid();
            }

            if (PARAM_SCHOOL_OF_CRS_DELIV.equals(fieldName)) {
                return transcript.getSchool().getFieldValueByBeanPath(m_fieldSchoolID);
            }

            if (PARAM_TERM_CODE.equals(fieldName) || PARAM_MARKING_PERIOD.equals(fieldName)) {
                MasterSchedule masterSchedule = transcript.getMasterSchedule();
                if (masterSchedule != null) {
                    String termCode = masterSchedule.getTermView();
                    if (termCode != null) {
                        return lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }

                String termCode = transcript.getTermCode();
                if (termCode != null) {
                    return lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            return null;
        }
    }

    /**
     * Retrieve data from the student schedule record based on field parameter
     * This Retriever is for SCSE input parameter
     *
     * <LI>ADM_REQ_CODE - UC/CSU Admission Requirement Code
     * <LI>COURSE_ID - Local Course ID
     * <LI>CREDITS_ATTEMPTED - Student Credits Attempted
     * <LI>CRS_SECTION_ID - Course Section ID
     * <LI>OID - Local Record ID
     * <LI>SCHOOL_OF_CRS_DELIV - School of Course Delivery
     * <LI>TERM_CODE - Academic Term Code.
     *
     * @author X2 Development Corporation
     */
    protected class SectionEnrollmentFieldRetriver implements FieldRetriever {
        private static final String PARAM_ADM_REQ_CODE = "ADM_REQ_CODE";
        private static final String PARAM_COURSE_ID = "COURSE_ID";
        private static final String PARAM_CREDITS_ATTEMPTED = "CREDITS_ATTEMPTED";
        private static final String PARAM_CRS_SECTION_ID = "CRS_SECTION_ID";
        private static final String PARAM_OID = "OID";
        private static final String PARAM_SCHOOL_OF_CRS_DELIV = "SCHOOL_OF_CRS_DELIV";
        private static final String PARAM_TERM_CODE = "TERM_CODE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String fieldName = (String) field.getParameter();

            StudentScheduleSpan span = ((CAStudentCourseSectionEntity) entity).getScheduleSpan();
            if (PARAM_ADM_REQ_CODE.equals(fieldName)) {
                String code = (String) span.getSection()
                        .getSchoolCourse()
                        .getCourse()
                        .getFieldValueByBeanPath(m_fieldCollegeRequirement);
                if (code != null) {
                    return lookupReferenceCodeByBeanPath(Course.class, m_fieldCollegeRequirement, code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            if (PARAM_COURSE_ID.equals(fieldName)) {
                if (!m_currContext.getContextId()
                        .equals(span.getSection().getSchoolCourse().getCourse().getDistrictContextOid())) {
                    System.out
                            .println("Exclude course: " + span.getSection().getSchoolCourse().getCourse().getNumber() +
                                    ". Student oid :" + ((SisStudent) entity.getBean()).getOid());
                }

                String nclb = (String) span.getSection().getFieldValueByBeanPath(m_fieldNclbInsert);
                return span.getSection().getSchoolCourse().getCourse().getNumber() + StringUtils.unNullify(nclb);
            }

            if (PARAM_CREDITS_ATTEMPTED.equals(fieldName)
                    && isHighSchool(span.getSection().getSchoolCourse().getSchool())) {
                return span.getSection().getSchoolCourse().getCredit();
            }

            if (PARAM_CRS_SECTION_ID.equals(fieldName)) {
                String courseNumber = null;
                String sectionNumber = null;
                SchoolCourse csk = null;
                Course crs = null;
                MasterSchedule mst = null;
                if (span != null && (mst = span.getSection()) != null) {
                    sectionNumber = mst.getSectionNumber();
                    if ((csk = mst.getSchoolCourse()) != null && (crs = csk.getCourse()) != null) {
                        courseNumber = crs.getNumber();
                    }
                }

                return (StringUtils.unNullify(courseNumber) + StringUtils.unNullify(sectionNumber)).replaceAll("-", "");
            }

            if (PARAM_OID.equals(fieldName)) {
                SisStudent student = ((CAStudentCourseSectionEntity) entity).getStudent();
                return student.getOid() + "-" + span.getSection().getOid();
            }

            if (PARAM_SCHOOL_OF_CRS_DELIV.equals(fieldName)) {
                return span.getSection().getSchoolCourse().getSchool().getFieldValueByBeanPath(m_fieldSchoolID);
            }

            if (PARAM_TERM_CODE.equals(fieldName)) {
                String termCode = span.getSection().getTermView();
                if (termCode != null) {
                    return lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }

            return null;
        }
    }

    /**
     * Validate values of:
     *
     * Student Credits Attempted,
     * Student Credits Earned
     * <p>
     * If Record Type Code = SCSC And If CRS-State Course Code <> 1000 And If Grade Level Code =
     * 9-12 then this field is required.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCredits implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (PARAM_RECORD_TYPE_VALUE_SCSC.equals(getParameter(INPUT_PARAM_RECORD_TYPE_CODE))) {
                Transcript transcript = ((CAStudentCourseSectionEntity) entity).getTranscript();

                String gradeLevel = transcript.getGradeLevel();
                if (gradeLevel != null) {
                    gradeLevel = lookupReferenceCodeByBeanPath(Transcript.class, Transcript.COL_GRADE_LEVEL, gradeLevel,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                Course course = transcript.getSchoolCourse() == null ? null : transcript.getSchoolCourse().getCourse();
                String courseCode = null;
                if (course != null) {
                    courseCode = (String) course.getFieldValueByBeanPath(m_fieldCourseCode);
                }
                if (courseCode != null) {
                    courseCode = lookupReferenceCodeByBeanPath(Course.class, m_fieldCourseCode, courseCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }

                if (gradeLevel != null) {
                    if (gradeLevel.matches(MATCH_GRADE_LEVEL_CODE_912) && !MATCH_COURSE_CODE_1000.equals(courseCode) &&
                            value == null) {
                        errors.add(new StateReportValidationError(entity, field,
                                "If Record Type Code = SCSC And If CRS-State Course Code <> 1000 And If Grade Level Code = 9-12 then this field is required",
                                "Record Type Code = " + STYLE_BOLD + "SCSC" + STYLE_END +
                                        " Grade Level Code = " + STYLE_BOLD + gradeLevel +
                                        STYLE_END +
                                        " This field value = " + STYLE_BOLD + "NULL" +
                                        STYLE_END));
                    }
                }
            }

            return errors;
        }
    }

    /**
     * Validate values of:
     *
     * Student Course Final Grade
     * Marking Period Code
     * <p>
     * If Record Type Code = SCSC then this field is required.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSCSC implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (PARAM_RECORD_TYPE_VALUE_SCSC.equals(getParameter(INPUT_PARAM_RECORD_TYPE_CODE)) && value == null) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Record Type Code = SCSC then this field is required",
                        "Record Type Code = " + STYLE_BOLD + "SCSC" + STYLE_END +
                                " This field value = " + STYLE_BOLD + "NULL" + STYLE_END));
            }
            return errors;
        }
    }

    /*
     * Aliases for fields to look up.
     */

    protected static final String ALIAS_COLLEGE_REQUIREMENT = "DOE COLLEGE REQUIREMENT";
    protected static final String ALIAS_COURSE_CODE = "DOE CRS CODE";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_NCLB_INSERT = "DOE NCLB INSERT";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SSID = "DOE SASID";

    /*
     * Codes
     */
    private static final String CODE_HIGH_SCHOOL = "High School";

    /*
     * Constants for history helper parameters from user input template.
     */
    protected static final String INPUT_PARAM_RECORD_TYPE_CODE = "recordTypeCode";

    /*
     * Match constants
     */
    protected static final Object DEF_COLUMN_OVERALL_S2 = "Overall Grade S2";
    protected static final String MATCH_COURSE_CODE_1000 = "1000";
    protected static final String MATCH_GRADE_LEVEL_CODE_912 = "^(09|10|11|12)$";
    protected static final String PARAM_ALL_SCHOOLS = "allSchools";
    protected static final String PARAM_RECORD_TYPE_VALUE_SCSC = "SCSC";
    protected static final String PARAM_RECORD_TYPE_VALUE_SCSE = "SCSE";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SCHOOLS = "schoolOids";
    protected static final String PARAM_WITHOUT_SSID = "withoutSsid";
    protected static final Object TRN_DEF_MIDDLE_SCHOOL = "Middle School Transcript";
    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldExcludeSchool;
    protected String m_fieldCollegeRequirement;
    protected String m_fieldCourseCode;
    protected String m_fieldSchoolID;
    protected String m_fieldNclbInsert;
    protected String m_fieldSsid;
    protected String m_gradeMiddleBeanPath;
    protected Map<String, GradeScale> m_gradeScales;
    protected GradesManager m_gradesManager;
    protected DistrictSchoolYearContext m_currContext;
    protected StudentHistoryHelper m_helper;
    protected Collection<String> m_highSchoolCodes;
    protected Map<String, Collection<Transcript>> m_stdTranscriptMap;
    PlainDate m_reportDate;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        m_gradesManager = new GradesManager(getBroker());
        m_currContext = getCurrentContext();
        initStudentHistoryHelper();
        loadGradescales();

        // Set the query to be used for student selection.
        setQuery(m_helper.getStudentQuery(false));

        setEntityClass(CAStudentCourseSectionEntity.class);

        initMiddleFinalGradeBeanPath();
        initFieldRetrivers();
        initFieldValidators();
    }

    /**
     * Return student transcripts form initialize map.
     *
     * @param stdOid String
     * @return List
     */
    protected List<Transcript> getStudentTranscripts(String stdOid) {
        List<Transcript> trns = new ArrayList<Transcript>();

        if (m_stdTranscriptMap.containsKey(stdOid) && m_stdTranscriptMap.get(stdOid) != null) {
            trns.addAll(m_stdTranscriptMap.get(stdOid));
        }

        return trns;
    }

    /**
     * Checks if is high school.
     *
     * @param school School
     * @return true if transcript belongs to High School
     */
    protected boolean isHighSchool(School school) {
        /*
         * A school will be considered high school if the sklSchoolType 'High School' or the state
         * reference code value
         * for sklSchoolType 'High School'
         */
        if (m_highSchoolCodes == null) {
            m_highSchoolCodes = new ArrayList<String>();
            m_highSchoolCodes.add(CODE_HIGH_SCHOOL);
            m_highSchoolCodes
                    .addAll(getRefCodesByStateCode(School.class, School.COL_SCHOOL_TYPE_CODE, CODE_HIGH_SCHOOL));
        }

        String schoolType = school.getSchoolTypeCode();

        if (schoolType != null) {
            if (m_highSchoolCodes.contains(schoolType)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Gets the ref codes by state code.
     *
     * @param beanClass Class<? extends X2BaseBean>
     * @param fieldId String
     * @param stateCode String
     * @return collection of reference codes from passed reference table for passed field ID by
     *         passed state code.
     */
    private Collection<String> getRefCodesByStateCode(Class<? extends X2BaseBean> beanClass,
                                                      String fieldId,
                                                      String stateCode) {
        ArrayList<String> codes = new ArrayList<String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty property = new ModelProperty(beanClass, fieldId, dictionary);
        DataDictionaryField ddf = dictionary.findDataDictionaryField(property.getFieldId());
        ReferenceTable refTable = ddf.getReferenceTable();

        Collection<ReferenceCode> refCodes = refTable.getReferenceCodes();

        for (ReferenceCode refCode : refCodes) {
            if (stateCode != null && refCode.getStateCode() != null && stateCode.equals(refCode.getStateCode())) {
                codes.add(refCode.getCode());
            }
        }

        return codes;
    }

    /**
     * Init Field retrievers.
     */
    private void initFieldRetrivers() {
        // Build a map of calculations/retrievers
        String inputParamReportType = (String) getParameter(INPUT_PARAM_RECORD_TYPE_CODE);
        boolean isSCSCRetriever = inputParamReportType.compareToIgnoreCase(PARAM_RECORD_TYPE_VALUE_SCSC) == 0;
        FieldRetriever fr = isSCSCRetriever
                ? new SectionCompletionFieldRetriver()
                : new SectionEnrollmentFieldRetriver();

        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("SSC-CODE", new RetrieveCode());
        calcs.put("SSC-SCHEDULE", fr);
        super.addCalcs(calcs);
    }

    /**
     * Init Field validators.
     */
    private void initFieldValidators() {
        // Build a map of validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("SSC-CREDITS-VAL", new ValidateCredits());
        validators.put("SSC-SCSC-VAL", new ValidateSCSC());
        super.addValidators(validators);
    }

    /**
     * Init bean path for Overall Grade S2 for the transcript to retrieve final grade for middle
     * schools.
     */
    private void initMiddleFinalGradeBeanPath() {
        X2Criteria colDefCriteria = new X2Criteria();
        colDefCriteria.addEqualTo(TranscriptColumnDefinition.REL_TRANSCRIPT_DEFINITION + ModelProperty.PATH_DELIMITER
                + TranscriptDefinition.COL_TRANSCRIPT_DEFINITION_NAME, TRN_DEF_MIDDLE_SCHOOL);
        colDefCriteria.addEqualTo(TranscriptColumnDefinition.COL_GRADE_NAME, DEF_COLUMN_OVERALL_S2);

        TranscriptColumnDefinition colDef = (TranscriptColumnDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(TranscriptColumnDefinition.class, colDefCriteria));

        if (colDef != null) {
            m_gradeMiddleBeanPath = colDef.getDataFieldConfig().getDataField().getJavaName();
        }
    }

    /**
     * Method for initialization of studentHistoryHelper object
     * Mode of operation is depends on value of INPUT_PARAM_RECORD_TYPE_CODE.
     */
    private void initStudentHistoryHelper() {
        m_helper = new StudentHistoryHelper(this);

        PlainDate currentDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        String inputParam = (String) getParameter(INPUT_PARAM_RECORD_TYPE_CODE);

        if (PARAM_RECORD_TYPE_VALUE_SCSC.equals(inputParam)) {
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            if (getCurrentContext().getEndDate().before(currentDate)) {
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getEndDate());
            }

            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

            X2Criteria trnCriteria = m_helper.getStudentTranscriptCriteria();
            if (m_currContext == null) {
                m_currContext = getCurrentContext();
            }

            trnCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_currContext.getOid());
            trnCriteria.addIn(Transcript.COL_STUDENT_OID, stdSubQuery);

            QueryByCriteria trnQuery = new QueryByCriteria(Transcript.class, trnCriteria);
            m_stdTranscriptMap = getBroker().getGroupedCollectionByQuery(trnQuery, Transcript.COL_STUDENT_OID, 2056);

        } else {
            m_reportDate = getParameter(PARAM_REPORT_DATE) == null
                    ? new PlainDate(OrganizationManager.getTimeZone(getOrganization()))
                    : (PlainDate) getParameter(PARAM_REPORT_DATE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            Boolean isWithoutSsid = (Boolean) getParameter(PARAM_WITHOUT_SSID);
            String schoolOids = (String) getParameter(PARAM_SCHOOLS);
            Boolean isAllSchools = (Boolean) getParameter(PARAM_ALL_SCHOOLS);
            X2Criteria sklCriteria = new X2Criteria();

            if (isAllSchools.booleanValue()) {
                sklCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                sklCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            } else {
                Set<String> setSchoolOids = new HashSet<String>();
                setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
                sklCriteria.addIn(X2BaseBean.COL_OID, setSchoolOids);
            }

            /**
             * Excluding schools with the Do not report indicator
             */
            sklCriteria.addNotEqualTo(m_fieldExcludeSchool, BooleanAsStringConverter.TRUE);

            SubQuery sklSubQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, sklCriteria);
            m_helper.getStudentCriteria().addIn(Student.COL_SCHOOL_OID, sklSubQuery);

            if (isWithoutSsid.booleanValue()) {
                m_helper.getStudentCriteria().addEmpty(m_fieldSsid, getBroker().getPersistenceKey());
            }

        }

    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldCollegeRequirement = translateAliasToJavaName(ALIAS_COLLEGE_REQUIREMENT, true);
        m_fieldCourseCode = translateAliasToJavaName(ALIAS_COURSE_CODE, true);
        m_fieldNclbInsert = translateAliasToJavaName(ALIAS_NCLB_INSERT, true);
        m_fieldSchoolID = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSsid = translateAliasToJavaName(ALIAS_SSID, true);
    }

    /**
     * Load grade scales for transcript grade translation.
     */
    private void loadGradescales() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));

        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) iterator.next();
                m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            iterator.close();
        }
    }
}
