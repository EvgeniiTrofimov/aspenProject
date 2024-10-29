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

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTerm;
import com.x2dev.sis.model.beans.ScheduleTermDate;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class PAStudentCourseEnr.
 */
public class PAStudentCourseEnr extends StateReportData {

    /**
     * The entity class generates a separate record for each qualifying StudentCourseEnr.
     */
    public static class PAStudentCourseEnrEntity extends StateReportEntity {

        /**
         * Helper class to store the state report values generated for a particular
         * StudentCourseEnr.
         */
        public class EntityVars {
            private String m_courseCode;
            private String m_deliveryModelCode;
            private String m_differentiator;
            private PlainDate m_effectiveDate;
            private String m_locationCode;
            private int m_requestSequence;
            private String m_sectionCode;

            /**
             * Instantiates a new entity vars.
             *
             * @param locationCode String
             * @param courseCode String
             * @param differentiator String
             * @param effectiveDate PlainDate
             * @param deliveryModelCode String
             * @param requestSequence int
             * @param sectionCode String
             */
            public EntityVars(String locationCode, String courseCode, String differentiator,
                    PlainDate effectiveDate, String deliveryModelCode,
                    int requestSequence, String sectionCode) {
                m_courseCode = courseCode;
                m_deliveryModelCode = deliveryModelCode;
                m_differentiator = differentiator;
                m_effectiveDate = effectiveDate;
                m_locationCode = locationCode;
                m_requestSequence = requestSequence;
                m_sectionCode = sectionCode;
            }

            /**
             * Gets the course code.
             *
             * @return the m_courseCode
             */
            public String getCourseCode() {
                return m_courseCode;
            }

            /**
             * Gets the delivery model code.
             *
             * @return the m_modelCode
             */
            public String getDeliveryModelCode() {
                return m_deliveryModelCode;
            }

            /**
             * Gets the differentiator.
             *
             * @return the m_differentiator
             */
            public String getDifferentiator() {
                return m_differentiator;
            }

            /**
             * Gets the effective date.
             *
             * @return the m_effectiveDate
             */
            public PlainDate getEffectiveDate() {
                return m_effectiveDate;
            }

            /**
             * Gets the location code.
             *
             * @return the location code
             */
            public String getLocationCode() {
                return m_locationCode;
            }

            /**
             * Gets the request sequence.
             *
             * @return the m_requestSequence
             */
            public int getRequestSequence() {
                return m_requestSequence;
            }

            /**
             * Gets the section code.
             *
             * @return the m_sectionCode
             */
            public String getSectionCode() {
                return m_sectionCode;
            }
        }

        Map<String, EntityVars> m_entVars;
        PAStudentCourseEnr m_sData;
        SisStudent m_student;

        /**
         * Instantiates a new PA student course enr entity.
         */
        public PAStudentCourseEnrEntity() {

        }

        /**
         * Return the vars for the current row.
         *
         * @return Entity vars
         */
        public EntityVars getCurrentVars() {
            return (new ArrayList<EntityVars>(m_entVars.values())).get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            m_student = (SisStudent) getBean();
            String name = m_student.getNameView() +
                    " [LASID: " + m_student.getLocalId() +
                    ", SASID: " + m_student.getStateId() +
                    "] " + getCurrentVars().getCourseCode() +
                    ", " + getCurrentVars().getSectionCode() +
                    ", " + getCurrentVars().getRequestSequence();

            return name;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_sData = (PAStudentCourseEnr) data;
            m_student = (SisStudent) getBean();
            m_entVars = new LinkedHashMap<String, EntityVars>();

            PlainDate startDate = getData().getOrganization().getCurrentContext().getStartDate();

            List<StudentScheduleSpan> scheduleSpans = m_sData.m_helper.getStudentScheduleSpans(m_student);

            for (StudentScheduleSpan span : scheduleSpans) {
                if (!startDate.after(span.getEntryDate()) &&
                        !m_sData.m_reportDate.before(span.getEntryDate())
                        && !span.getEntryDate().equals(span.getExitDate())) {
                    if (!data.isSchoolContext()
                            || data.getSchool().getOid().equals(span.getSection().getSchedule().getSchoolOid())) {
                        addCourseEnrollment(span);
                    }
                }
            }

            setRowCount(m_entVars.size());
        }

        /**
         * Add an course enrollment record to the list. Resolve state report values for various
         * codes.
         *
         * @param span StudentScheduleSpan
         */
        private void addCourseEnrollment(StudentScheduleSpan span) {
            if (span != null) {
                // set LOCATION CODE
                String locationCode = (String) span.getSection().getSchedule().getSchool()
                        .getFieldValueByBeanPath(m_sData.m_fieldLocationCode);

                // set COURSE CODE LONG
                String courseCode = span.getSection().getSchoolCourse().getCourse().getNumber();
                // set SUPPLEMENTARY COURSE DIFFERENTIATOR (formerly named SEMESTER)
                String differentiator = null;
                ScheduleTerm schTerm = m_sData.m_schTermCodes.get(span.getSection().getScheduleTermOid());
                if (schTerm != null) {
                    differentiator = schTerm.getCode();
                }

                // set EFFECTIVE DATE
                PlainDate effectiveDate = span.getEntryDate();
                // if entry date == null, then set EFFECTIVE DATE as start date of term
                if (effectiveDate == null) {
                    ScheduleTermDate schTrmDate = m_sData.m_termStartDates.get(span.getSection().getScheduleTermOid());
                    if (schTrmDate != null) {
                        effectiveDate = schTrmDate.getStartDate();
                    }
                }
                // set COURSE DELIVERY MODEL CODE
                String deliveryModelCode = (String) span.getSection().getSchoolCourse().getCourse()
                        .getFieldValueByBeanPath(m_sData.m_fieldDeliveryModelCode);

                // set REQUEST SEQUENCE
                int requestSequence = 1;

                // set SECTION CODE LONG
                String sectionCode = span.getSection().getSectionNumber();

                if (locationCode != null) {
                    EntityVars thisEntVars = new EntityVars(locationCode, courseCode, differentiator, effectiveDate,
                            deliveryModelCode, requestSequence, sectionCode);
                    if (m_entVars != null) {
                        while (m_entVars.containsKey(courseCode + sectionCode + requestSequence)) {
                            requestSequence++;
                        }
                        m_entVars.put(courseCode + sectionCode + requestSequence, thisEntVars);
                    }
                }
            }
        }
    }

    /**
     * The Class RetrieveDualEnrollmentCoursePass.
     */
    public class RetrieveDualEnrollmentCoursePass implements FieldRetriever {

        public static final String CALC_ID_CRS = "DUAL_ENR_CRS_PASS";
        public static final String EXPORT_FIELD_CRS_CODE = "COURSE CODE LONG";

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
            String returnValue = "";
            PAStudentCourseEnr paData = (PAStudentCourseEnr) data;
            PAStudentCourseEnrEntity paEntity = (PAStudentCourseEnrEntity) entity;
            SisStudent std = (SisStudent) entity.getBean();
            String crsCode = paEntity.getFieldValue(EXPORT_FIELD_CRS_CODE);
            if (!StringUtils.isEmpty(crsCode)) {
                Collection<Transcript> trns = paData.m_transcriptMap.get(std.getOid());
                if (trns != null && !trns.isEmpty()) {
                    for (Transcript trn : trns) {
                        if (trn.getSchoolCourse() != null && trn.getSchoolCourse().getCourse() != null
                                && crsCode.equals(trn.getSchoolCourse().getCourse().getNumber())
                                && BooleanAsStringConverter.TRUE.equals(trn.getSchoolCourse().getCourse()
                                        .getFieldValueByBeanPath(paData.m_fieldCrsDualFlag))) {
                            returnValue = "Y";
                            GradeScale scale = m_gradeScales.get(trn.getTranscriptDefinitionOid());
                            Collection<GradeScaleGradeDefinition> scaleDefs =
                                    scale.getGradeScaleDefinitions();
                            if (scaleDefs != null && !scaleDefs.isEmpty()) {
                                returnValue = "N";
                                for (GradeScaleGradeDefinition scaleDef : scaleDefs) {
                                    if (scaleDef.getCreditIndicator()) {
                                        returnValue = "Y";
                                        break;
                                    }
                                }
                            }
                            break;
                        }
                    }
                }
            }
            return returnValue;
        }
    }

    /**
     * Retriever used to return the appropriate value from EntityVars.
     */
    public class RetrieveEnrollmentInfo implements FieldRetriever {

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
            PAStudentCourseEnrEntity ent = (PAStudentCourseEnrEntity) entity;
            PAStudentCourseEnrEntity.EntityVars vars = ent.getCurrentVars();

            String parameter = (String) field.getParameter();

            if (CALC_PARAM_COURSE_CODE_LONG.equals(parameter)) {
                return vars.getCourseCode();
            } else if (CALC_PARAM_COURSE_DELIVERY_MODEL_CODE.equals(parameter)) {
                return vars.getDeliveryModelCode();
            } else if (CALC_PARAM_EFFECTIVE_DATE.equals(parameter)) {
                return vars.getEffectiveDate();
            } else if (CALC_PARAM_LOCATION_CODE.equals(parameter)) {
                return vars.getLocationCode();
            } else if (CALC_PARAM_REQUEST_SEQUENCE.equals(parameter)) {
                return Integer.valueOf(vars.getRequestSequence());
            } else if (CALC_PARAM_SECTION_CODE_LONG.equals(parameter)) {
                return vars.getSectionCode();
            } else if (CALC_PARAM_SUPPLEMENTARY_COURSE_DIFFERENTIATOR.equals(parameter)) {
                return vars.getDifferentiator();
            }
            return null;
        }
    }

    /**
     * Validate required student status.
     */
    protected class ValidateModelCode implements FieldValidator {
        private List<String> validCodes =
                Arrays.asList("0340", "0341", "0342", "0752", "0754", "0997", "1111", "2192", "3018", "9998", "9999");

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

            if (!StringUtils.isEmpty(value)) {
                if (!validCodes.contains(value)) {
                    errors.add(new StateReportValidationError(entity, field, "Invalid Value",
                            "Delivery Model Code is not a valid value."));
                }
            }
            return errors;
        }
    }

    /**
     * Aliases.
     */
    public static final String ALIAS_CRS_DOE_DUAL = "DOE DUAL";
    public static final String ALIAS_DOE_DELIVERY = "DOE COURSE DELIVERY";
    public static final String ALIAS_SCHOOL_LOCATION = "DOE SCHOOL STATE ID";

    public static final String CALC_ID = "ENR_CALC";
    public static final String CALC_PARAM_COURSE_CODE_LONG = "CRS_CODE";
    public static final String CALC_PARAM_COURSE_DELIVERY_MODEL_CODE = "MODEL_CODE";
    public static final String CALC_PARAM_EFFECTIVE_DATE = "EFF_DATE";
    public static final String CALC_PARAM_LOCATION_CODE = "LOC_CODE";
    public static final String CALC_PARAM_REQUEST_SEQUENCE = "REQ_SEQ";
    public static final String CALC_PARAM_SECTION_CODE_LONG = "CODE_LONG";
    public static final String CALC_PARAM_SUPPLEMENTARY_COURSE_DIFFERENTIATOR = "DIFFER";

    public static final String PARAM_REPORT_DATE = "reportDate";

    DateAsStringConverter m_dateConverter =
            (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(),
                    true);
    String m_fieldDeliveryModelCode;
    String m_fieldCrsDualFlag;
    String m_fieldLocationCode;
    Map<String, GradeScale> m_gradeScales;
    StudentHistoryHelper m_helper;
    PlainDate m_reportDate;
    Map<String, ScheduleTerm> m_schTermCodes;
    String m_schoolYearContext;
    Map<String, ScheduleTermDate> m_termStartDates;
    protected Map<String, Collection<Transcript>> m_transcriptMap;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() {
        m_schoolYearContext = getOrganization().getCurrentContextOid();
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        initializeFields();
        loadTermStartDates();
        loadSchTermCodes();
        if (getSetupErrors().size() == 0) {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, true);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

            applyInputCriteria(m_helper.getStudentCriteria(), true, null);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(PAStudentCourseEnrEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID, new RetrieveEnrollmentInfo());
            calcs.put(RetrieveDualEnrollmentCoursePass.CALC_ID_CRS, new RetrieveDualEnrollmentCoursePass());
            super.addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("VAL_MODEL_CODE", new ValidateModelCode());
            super.addValidators(validators);
        }
        loadTranscripts();
        loadGradeScales();
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_fieldDeliveryModelCode = translateAliasToJavaName(ALIAS_DOE_DELIVERY, true);
        m_fieldLocationCode = translateAliasToJavaName(ALIAS_SCHOOL_LOCATION, true);
        m_fieldCrsDualFlag = translateAliasToJavaName(ALIAS_CRS_DOE_DUAL, true);
    }

    /**
     * Load grade scales for transcript grade translation.
     */
    private void loadGradeScales() {
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

    /**
     * Load map with schedule term codes.
     */
    private void loadSchTermCodes() {
        m_schTermCodes = new HashMap<String, ScheduleTerm>();
        X2Criteria criteria = new X2Criteria();

        QueryByCriteria query = new QueryByCriteria(ScheduleTerm.class, criteria);

        m_schTermCodes = getBroker().getMapByQuery(query, X2BaseBean.COL_OID, 256);
    }

    /**
     * Load map with start dates of terms keyed on SheduleTermDate oids.
     */
    private void loadTermStartDates() {
        m_termStartDates = new HashMap<String, ScheduleTermDate>();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                ScheduleTerm.REL_SCHEDULE + ModelProperty.PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID,
                m_schoolYearContext);

        QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);

        m_termStartDates = getBroker().getMapByQuery(query, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 64);
    }

    /**
     * Load a map of transcripts for the students in the export.
     *
     */
    private void loadTranscripts() {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_schoolYearContext);
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
        transcriptCriteria.addNotEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
        transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER
                + SchoolCourse.REL_COURSE + ModelProperty.PATH_DELIMITER + m_fieldCrsDualFlag, Boolean.FALSE);
        BeanQuery query = new BeanQuery(Transcript.class, transcriptCriteria);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 1000);
    }
}
