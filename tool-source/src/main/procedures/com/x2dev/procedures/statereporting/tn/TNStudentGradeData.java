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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for Student Instructional Grade export.
 */
public class TNStudentGradeData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    /**
     * Entity class for Student Instructional Grade export.
     *
     */
    public static class TNStudentGradeEntity extends TNStateReportEntity
            implements TNStateReportData.HasStudentRecordHelper {
        private List<StudentRecordHelper> m_list;

        /**
         * Instantiates a new TN student grade entity.
         */
        public TNStudentGradeEntity() {
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
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";

            return name;
        }

        /**
         * Get the record helper for the current row.
         *
         * @return Student record helper
         */
        @Override
        public StudentRecordHelper getCurrentRecord() {
            return m_list.get(getCurrentRow());
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

            TNStudentGradeData tnData = (TNStudentGradeData) data;
            SisStudent student = (SisStudent) getBean();
            m_list = tnData.getStudentHelperMap().get(student.getOid());

            setRowCount(m_list == null ? 0 : m_list.size());

            tnData.addEntityRowsCount(getRowCount());
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalize SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String STDG_CALC_ID = "STDG_CALC_SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentGradeEntity seEntity = (TNStudentGradeEntity) entity;

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
    protected class FieldValidatorSSN implements FieldValidator {
        protected static final String STDG_VAL_ID = "STDG_VAL_SSN";
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
     * Field retriever for Instructional Grade Assignment field.
     */
    protected class RetrieveInstrlGA implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentGradeEntity seEntity = (TNStudentGradeEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
            StudentRecordHelper studentData = seEntity.getCurrentRecord();
            String gradeLevel = null;

            ReferenceCode gradeCode = getGradeLevel(student, studentData.getYog());
            if (gradeCode != null) {
                gradeLevel = gradeCode.getStateCode();
            } else {
                gradeLevel = lookupStateValue(Student.class, Student.COL_GRADE_LEVEL,
                        (String) getStudentMultiYearHelper().getFieldValueByBeanPath(student, Student.COL_GRADE_LEVEL));
            }

            return gradeLevel;
        }
    }

    /**
     * Field retriever for Instructional Grade Assignment Date field.
     */
    protected class RetrieveInstrlGAD implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentGradeEntity seEntity = (TNStudentGradeEntity) entity;
            StudentRecordHelper studentData = seEntity.getCurrentRecord();

            return studentData.getEnrollDate();
        }
    }

    /**
     * Field retriever for School field.
     */
    protected class RetrieveSchoolId implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentGradeEntity seEntity = (TNStudentGradeEntity) entity;
            StudentRecordHelper studentData = seEntity.getCurrentRecord();

            return studentData.getSchoolId();
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStudentGradeData seData = (TNStudentGradeData) data;
            return seData.m_schoolYear;
        }

    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";

    protected static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";

    private static final String CALC_ID_INSTRLGA = "STDG_CALC_INSTRLGA";
    private static final String CALC_ID_INSTRLGAD = "STDG_CALC_INSTRLGAD";
    private static final String CALC_ID_SCHOOL = "STDG_CALC_SCHOOL";
    private static final String CALC_ID_SCHOOLYEAR = "STDG_CALC_SCHOOLYEAR";
    /**
     * Instance variables.
     */
    protected String m_fieldExcludeStudent;
    protected String m_fieldSchoolStateId;
    protected String m_schoolYear;
    protected TNStudentHistoryHelper m_studentHelper;
    protected Map<String, PlainDate> m_firstDatesOfSchools;
    protected Map<String, ReferenceCode> m_referenceGradeCodeMap;

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

        setOnFirstDayWithdrew(true);

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        QueryByCriteria query = initializeCriteriaAndQuery();
        setQuery(query);
        setEntityClass(TNStudentGradeEntity.class);

        initStudentHelperMap(m_studentHelper, query);

        // Add any necessary FieldRetrievers an Field Validators
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Find first date in session for school for "standard" calendar.
     * Check span enrollment date to be not before first in-session date.
     *
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    protected PlainDate determineEnrollmentDate(TNStudentEnrollmentSpan span) {
        PlainDate enrDate = span.getFirstActiveDate();
        String key = "Standard" + span.getSchool().getSchoolId();
        PlainDate firstInSessionDate = null;
        if (m_firstDatesOfSchools.containsKey(key)) {
            firstInSessionDate = m_firstDatesOfSchools.get(key);
        } else {
            Set<PlainDate> dates = m_studentHelper.getCalendarDays(span.getSchool(), "Standard");
            if (dates != null) {
                for (PlainDate date : dates) {
                    if (firstInSessionDate == null || (firstInSessionDate != null && date.before(firstInSessionDate))) {
                        firstInSessionDate = date;
                    }
                }
            }
            m_firstDatesOfSchools.put(key, firstInSessionDate);
        }
        if (enrDate != null && firstInSessionDate != null && enrDate.after(firstInSessionDate)) {
            return enrDate;
        }
        return firstInSessionDate != null ? firstInSessionDate : getCurrentContext().getStartDate();
    }

    /**
     * Calculate grade code from StudentEnrollmentSpan.
     *
     * @param student SisStudent
     * @param yog int
     * @return Reference code
     */
    protected ReferenceCode getGradeLevel(SisStudent student, int yog) {
        ReferenceCode gradeCode = null;
        if (yog == ((Integer) getStudentMultiYearHelper().getFieldValueByBeanPath(student, SisStudent.COL_YOG))
                .intValue()) {
            String gradeLevel =
                    (String) getStudentMultiYearHelper().getFieldValueByBeanPath(student, SisStudent.COL_GRADE_LEVEL);
            gradeCode = m_referenceGradeCodeMap.get(gradeLevel);
        }
        if (gradeCode == null) {
            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
        }
        return gradeCode;
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
     * Function for building custom Student criteria.
     *
     * @return criteria for query for list of active students
     *         limited by reportDate, school and not excluded students
     */
    private X2Criteria getStudentCriteria() {
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);

        X2Criteria studentCriteria = new X2Criteria();
        studentCriteria = m_studentHelper.getStudentCriteria();

        return studentCriteria;
    }

    /**
     * Initialize criteria and query.
     *
     * @return QueryByCriteria
     */
    private QueryByCriteria initializeCriteriaAndQuery() {
        X2Criteria criteria = getStudentCriteria();
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        applyInputSort(query, null);
        return query;

    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldExcludeStudent = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, true);
        m_fieldSchoolStateId = translateAliasToJavaName(ALIAS_SCHOOL_STATE_ID, true);
        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        m_studentHelper = helper.getStudentHistoryHelper();
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_schoolYear = getCurentSchoolYear();
        m_firstDatesOfSchools = new HashMap<String, PlainDate>();
        loadGradeCodes();
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGradeCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        calcs.put(CALC_ID_INSTRLGAD, new RetrieveInstrlGAD());
        calcs.put(CALC_ID_INSTRLGA, new RetrieveInstrlGA());
        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(CALC_ID_SCHOOL, new RetrieveSchoolId());
        calcs.put(FieldRetrieverSSN.STDG_CALC_ID, new FieldRetrieverSSN());
        super.addCalcs(calcs);
    }

    /**
     * Register custom field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FieldValidatorSSN.STDG_VAL_ID, new FieldValidatorSSN());
        super.addValidators(validators);
    }
}
