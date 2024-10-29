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

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNClassSectionHelper.TNScheduleSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStudentClassAssignmentData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for student extract export.
     *
     */
    public static class TNStudentClassAssignmentEntity extends TNStateReportEntity {
        /**
         * Entity instance variables.
         */
        TNStudentClassAssignmentData m_exportData;
        private SisStudent m_student;
        private List<TNScheduleSpan> m_studentSchedules;

        /**
         * Instantiates a new TN student class assignment entity.
         */
        public TNStudentClassAssignmentEntity() {
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
            m_student = (SisStudent) getBean();
            String name = m_student.getNameView() +
                    " [LASID: " + m_student.getLocalId() +
                    ", SASID: " + m_student.getStateId() +
                    "] " + getTNScheduleSpan().getSection().getTermView();

            return name;
        }

        /**
         * Uses student schedule spans and teacher list to determine the export rows.
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
            m_exportData = (TNStudentClassAssignmentData) data;

            m_student = (SisStudent) getBean();
            List<TNStudentEnrollmentSpan> spans =
                    m_exportData.m_classSectionHelper.getStudentEnrollmentSpans(m_student);
            boolean isEnrolled = false;
            for (TNStudentEnrollmentSpan span : spans) {
                PlainDate enrStartDate = m_exportData.determineEnrollmentDate(m_student, span);
                if (!enrStartDate.after(m_exportData.m_reportDate)) {
                    isEnrolled = true;
                    break;
                }
            }

            m_studentSchedules = m_exportData.m_classSectionHelper.getStudentSchedules(m_student.getOid()).stream()
                    .filter(tnSscSpan -> !StringUtils
                            .isEmpty((String) tnSscSpan.getStateSchoolId(m_exportData.m_fieldStateSchoolId)))
                    .collect(Collectors.toList());

            filterSchedules(m_studentSchedules);

            setRowCount(isEnrolled ? m_studentSchedules.size() : 0);
            m_exportData.addEntityRowsCount(getRowCount());
        }

        /**
         * Get current TNScheduleSpan.
         *
         * @return TN schedule span
         */
        protected TNScheduleSpan getTNScheduleSpan() {
            return m_studentSchedules.get(getCurrentRow());
        }

        /**
         * Filter schedules.
         *
         * @param spans Collection<TNScheduleSpan>
         */
        private void filterSchedules(Collection<TNScheduleSpan> spans) {
            if (m_exportData.m_fromCrsMstTab) {
                Iterator<TNScheduleSpan> scheduleSpanIterator = spans.iterator();
                if (m_exportData.m_wholeSchool.booleanValue()) {
                    while (scheduleSpanIterator.hasNext()) {
                        TNScheduleSpan span = scheduleSpanIterator.next();
                        if (span.getSection() == null || span.getSection().getSchoolCourse() == null ||
                                !m_exportData.m_schoolOids
                                        .contains(span.getSection().getSchoolCourse().getSchoolOid())) {
                            scheduleSpanIterator.remove();
                        }
                    }
                } else {
                    while (scheduleSpanIterator.hasNext()) {
                        TNScheduleSpan span = scheduleSpanIterator.next();
                        if (span.getSection() == null ||
                                !m_exportData.m_courseSectionOids.contains(span.getSection().getOid())) {
                            scheduleSpanIterator.remove();
                        }
                    }
                }
            }
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
        protected static final String SSC_CALC_ID = "SSC_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentClassAssignmentEntity seEntity = (TNStudentClassAssignmentEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
            Person psn = student.getPerson();

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
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String SSC_VAL_ID = "SSC_VAL_SSN";
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
     * Field retriever for Instructional program and school year fields.
     */
    protected class RetrieveDefault implements FieldRetriever {
        public static final String CALC_ID_DEFAULT = "SSC_CALC_DEFAULT";

        protected static final String CALC_PARAM_SCHOOLYEAR = "SCHOOLYEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNStudentClassAssignmentData seData = (TNStudentClassAssignmentData) data;

            String param = (String) field.getParameter();

            if (param.equalsIgnoreCase(CALC_PARAM_SCHOOLYEAR)) {
                return seData.m_schoolYear;
            }

            return "";
        }

    }

    /**
     * Field retriever for fields from the StudentSchedule.
     */
    protected class RetrieveFromSSC implements FieldRetriever {
        public static final String CALC_ID_SSCHEDULE = "SSC_CALC_SSCHEDULE";

        protected static final String CALC_PARAM_CLASSBEGIN = "CLASSBEGIN";
        protected static final String CALC_PARAM_CLASSEND = "CLASSEND";
        protected static final String CALC_PARAM_LOCALCLASSNUM = "LOCALCLASSNUM";
        protected static final String CALC_PARAM_OUTSIDEIEP = "OUTSIDEIEP";
        protected static final String CALC_PARAM_SCHOOL = "SCHOOLID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            TNStudentClassAssignmentEntity seEntity = (TNStudentClassAssignmentEntity) entity;
            TNScheduleSpan scheduleSpan = seEntity.getTNScheduleSpan();

            Object value = null;

            if (param.equalsIgnoreCase(CALC_PARAM_LOCALCLASSNUM)) {
                value = scheduleSpan.getCourseView();
            } else if (param.equalsIgnoreCase(CALC_PARAM_CLASSBEGIN)) {
                value = scheduleSpan.getStartDate();
            } else if (param.equalsIgnoreCase(CALC_PARAM_CLASSEND)) {
                value = scheduleSpan.getEndDate();
            } else if (param.equalsIgnoreCase(CALC_PARAM_OUTSIDEIEP)) {
                if (scheduleSpan != null) {
                    value = scheduleSpan.getOutsideIep();
                }
            } else if (CALC_PARAM_SCHOOL.equals(param)) {
                if (scheduleSpan != null) {
                    SisSchool school =
                            (SisSchool) getBroker().getBeanByOid(SisSchool.class, scheduleSpan.getSchoolOid());
                    if (school != null) {
                        value = school.getFieldValueByBeanPath(m_fieldStateSchoolId);
                    }

                }
            }

            return value;
        }
    }

    /**
     * Field retriever for Instructional program.
     */
    protected class RetrieveInstrProgram implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_STD";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNStudentClassAssignmentEntity scaEntity = (TNStudentClassAssignmentEntity) entity;

            return scaEntity.getTNScheduleSpan().getInstrPgm();
        }

    }

    /**
     * Static String variables. Used by TN Restaging Procedure.
     */
    protected static final String PARAM_COURSE_SECTION = "courseSections";
    protected static final String PARAM_FROM_TAB = "fromTab";
    protected static final String PARAM_SCHOOL_OIDS = "schoolOidCourse";
    protected static final String TAB_CRS_MST = "tabStdMst";

    /**
     * Instance variables.
     *
     */
    protected TNClassSectionHelper m_classSectionHelper;
    protected Collection<String> m_courseSectionOids; // used by TN Restaging Procedure

    // This field is used by TN Restaging Procedure to flag if export run as result of running of
    // another export
    // (e.g. if TN 030, TN 031 is run TN 048 and TN 063 for selected Sections should be resent as
    // well.
    protected boolean m_fromCrsMstTab;

    protected Collection<String> m_schoolOids; // used by TN Restaging Procedure

    protected String m_schoolYear;

    protected Boolean m_wholeSchool; // used by TN Restaging Procedure

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
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        School school = null;
        if (this.isSchoolContext()) {
            school = getSchool();
        }

        m_classSectionHelper = new TNClassSectionHelper(this, school, true, null, getBypassValue());

        if (getSetupErrors().size() != 0) {
            return;
        }

        setQuery(getStudentQuery());
        setEntityClass(TNStudentClassAssignmentEntity.class);

        // Add any necessary FieldRetrievers
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * get the parameter setting for bypass value. Default to false.
     *
     * @return boolean
     */
    private boolean getBypassValue() {
        boolean value = false;
        if (getParameter(PARAM_BYPASS_DUP_SECT_TEST) != null) {
            value = ((Boolean) getParameter(PARAM_BYPASS_DUP_SECT_TEST)).booleanValue();
        }
        return value;
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Gets the student query.
     *
     * @return Query by criteria
     */
    private QueryByCriteria getStudentQuery() {
        QueryByCriteria studentQuery = null;
        m_fromCrsMstTab = TAB_CRS_MST.equals(getParameter(PARAM_FROM_TAB));
        QueryByCriteria standardStudentQuery = m_classSectionHelper.getStudentQuery(true);
        X2Criteria studentCriteria = (X2Criteria) standardStudentQuery.getCriteria();
        studentCriteria.addNotEmpty(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldStateSchoolId,
                getBroker().getPersistenceKey());
        if (m_fromCrsMstTab) {
            m_wholeSchool = Boolean.FALSE;
            String courseSectionString = (String) getParameter(PARAM_COURSE_SECTION);
            if (StringUtils.isEmpty(courseSectionString)) {
                m_wholeSchool = Boolean.TRUE;
            }
            if (m_wholeSchool.booleanValue()) {
                String schoolOidsString = (String) getParameter(PARAM_SCHOOL_OIDS);
                m_schoolOids = new ArrayList<String>();
                splitPicklistBeanOids(m_schoolOids, schoolOidsString);
            } else {
                m_courseSectionOids = new ArrayList<String>();
                splitPicklistBeanOids(m_courseSectionOids, courseSectionString);
            }
        }
        studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
        return studentQuery;
    }

    /**
     * Lookup field aliases and paths.
     *
     */
    private void initializeFields() {
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        }
        m_schoolYear = getCurentSchoolYear();
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveDefault.CALC_ID_DEFAULT, new RetrieveDefault());
        calcs.put(RetrieveFromSSC.CALC_ID_SSCHEDULE, new RetrieveFromSSC());
        calcs.put(FieldRetrieverSSN.SSC_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveInstrProgram.TN_CALC_INSTPGM_ID, new RetrieveInstrProgram());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.SSC_VAL_ID, new FiledValidatorSSN());
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
