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
package com.x2dev.procedures.statereporting.ct;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.types.PlainDate;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Connecticut state report for SASID Testing export.
 * This class implements the data export for CT SASID Testing export.
 *
 * @author X2 Development Corporation
 */
public class CT18SasidTesting extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SASID Testing export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SasidTestingEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        CT18SasidTesting m_testData = null;

        /**
         * Instantiates a new sasid register entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public SasidTestingEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment membership count and membership days parameter
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            return error;
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
            m_testData = (CT18SasidTesting) data;
            SisStudent student = (SisStudent) bean;
            List<StudentEnrollmentSpan> spans = m_testData.m_helper.getStudentEnrollmentSpans(student, false);
            /*
             * testSpan is the enrollment span active on the test date. When bad data is encountered
             * multiple spans may be returned. For multiple spans, use the span with the latest
             * entry date.
             */
            StudentEnrollmentSpan testSpan = null;
            for (StudentEnrollmentSpan span : spans) {
                if (span.getFirstActiveEnrollment() != null
                        && span.getFirstActiveEnrollment().getEnrollmentDate() != null) {
                    if (!m_testData.m_testDate.before(span.getFirstActiveDate()) &&
                            (span.getLastActiveDate() == null
                                    || !m_testData.m_testDate.after(span.getLastActiveDate()))) {
                        if (testSpan == null) {
                            testSpan = span;
                        } else if (testSpan.getFirstActiveEnrollment().getEnrollmentDate()
                                .before(span.getFirstActiveEnrollment().getEnrollmentDate())) {
                            testSpan = span;
                        }
                    }
                }
            }
            if (testSpan == null || !m_testData.m_setSchoolOids.contains(testSpan.getSchool().getOid())) {
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
    }

    /**
     * Retrieve Recently Arrived English Learner.
     *
     * If the field value in field "English Learners (EL) is "Y" and
     * the date in field "all-std-ELEntryIntoUS" is not null and
     * the date in field "all-std-ELEntryIntoUS" is less than two years before the input parameter
     * test date then
     * the value is "Y" otherwise
     * the value is "N"
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELEntryIntoUS implements FieldRetriever {
        private static final String CALC_ID = "ARRIVED-EL";

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
            String elEntry = STRING_NO;
            String el = entity.getFieldValue("EL");
            SisStudent student = (SisStudent) entity.getBean();
            if (el.equals(STRING_YES)) {
                String ellValue =
                        StringUtils.isEmpty(m_elEntryIntoUS) ? null
                                : (String) student.getFieldValueByBeanPath(m_elEntryIntoUS);
                if (ellValue != null) {
                    try {
                        PlainDate ellValueDate = (PlainDate) data.getPropertyAsJavaType(student, m_elEntryIntoUS);
                        if (withinTwoYears(ellValueDate, m_testDate)) {
                            elEntry = STRING_YES;
                        }
                    } catch (X2BaseException pe) {
                        throw new X2RuntimeException(pe);
                    }
                }
            }
            return elEntry;
        }

        /**
         * Difference in months.
         *
         * @param startDate Date
         * @param endDate Date
         * @return int
         */
        private boolean withinTwoYears(Date startDate, Date endDate) {
            Calendar startCalendar = new GregorianCalendar();
            startCalendar.setTime(startDate);
            startCalendar.add(Calendar.YEAR, 2);
            Calendar endCalendar = new GregorianCalendar();
            endCalendar.setTime(endDate);
            return startCalendar.after(endCalendar);
        }
    }

    /**
     * Retrieve if student has Ell program.
     */
    protected class RetrieveEnglishLanguageLearner extends RetrieveProgramCode {
        private static final String CALC_ID = "SASID-EL";
        private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
        private static final String NO = "N";
        private static final String YES = "Y";
        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        /**
         * Members
         */
        private String m_fieldStdEll;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve english language learner.
         */
        public RetrieveEnglishLanguageLearner() {
            super();

            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_ELL);
            if (dictionaryField != null) {
                m_fieldStdEll = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            } else {
                String aliasMsg =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
                addSetupError(aliasMsg, ALIAS_ELL);
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.SasidRegister.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_IMMIGRANT_STATUS);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.SasidRegister.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            Object item = getPropertyAsJavaType(entity.getBean(), m_fieldStdEll);
            String value = NO;
            if (item != null) {
                if (item instanceof Boolean) {
                    if (((Boolean) item).booleanValue()) {
                        value = YES;
                    }
                } else if (item instanceof String) {
                    String strValue = (String) item;
                    if (m_hasRefTable) {
                        strValue = lookupStateValue(SisStudent.class, m_fieldStdEll, strValue);
                    }
                    if (YES_VALUES.contains(strValue)) {
                        value = YES;
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /**
     * The Class RetrieveMilitaryFamily.
     */
    protected class RetrieveMilitaryFamily extends RetrieveProgramCode {
        private static final String ALIAS_STD_MILITARY_FAMILY = "all-std-MilitaryFamily";
        private static final String CALC_ID = "SASID-MILITARY";
        private static final String NO = "N";
        private static final String YES = "Y";
        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        /**
         * Members
         */
        private String m_fieldMilitaryFamily;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve military family.
         */
        public RetrieveMilitaryFamily() {
            super();
            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_STD_MILITARY_FAMILY);
            if (dictionaryField != null) {
                m_fieldMilitaryFamily = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_MILITARY_FAMILY);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            String value = NO;
            if (!StringUtils.isEmpty(m_fieldMilitaryFamily)) {
                Object item = getPropertyAsJavaType(entity.getBean(), m_fieldMilitaryFamily);
                if (item != null) {
                    if (item instanceof Boolean) {
                        if (((Boolean) item).booleanValue()) {
                            value = YES;
                        }
                    } else if (item instanceof String) {
                        String strValue = (String) item;
                        if (m_hasRefTable) {
                            strValue = lookupStateValue(SisStudent.class, m_fieldMilitaryFamily, strValue);
                        }
                        if (YES_VALUES.contains(strValue)) {
                            value = YES;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /**
     * The rule for the getValue function is first test if the student is contained in the set of
     * programs.
     * If they are, return the proper value from the program. If they are not, return the value
     * based on the student alias.
     *
     * @author Follett Software Company
     */
    protected abstract class RetrieveProgramCode implements FieldRetriever {
        /**
         * Members
         */
        protected X2Criteria m_pgmCriteria;
        protected Map<String, List<StudentProgramParticipation>> m_stdPgmMap;

        /**
         * Instantiates a new retrieve program code.
         */
        public RetrieveProgramCode() {
            applyCriteria();
            initPgmMap();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData data, StateReportEntity entity, FieldDefinition field) throws
         *      X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;

            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdPgmMap.containsKey(student.getOid())) {
                if (isIndicator()) {
                    value = "Y";
                } else {
                    List<StudentProgramParticipation> pgms = m_stdPgmMap.get(student.getOid());
                    StudentProgramParticipation programParticipation = pgms.get(0);
                    String programCode = programParticipation.getProgramCode();

                    if (!StringUtils.isEmpty(programCode)) {
                        value = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
                    }
                }
            } else {
                value = getCustomValue(entity);
            }

            return value;
        }

        /**
         * Adds the equal to pgm field.
         *
         * @param beanPath String
         * @param value String
         */
        protected void addEqualToPgmField(String beanPath, String value) {
            if (m_pgmCriteria == null) {
                m_pgmCriteria = new X2Criteria();

                m_pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_testDate);

                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_testDate);
                X2Criteria emptyEndDateCriteria = new X2Criteria();
                emptyEndDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE,
                        getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(emptyEndDateCriteria);

                m_pgmCriteria.addAndCriteria(endDateCriteria);
            }
            m_pgmCriteria.addEqualTo(beanPath, value);
        }

        /**
         * Apply criteria.
         */
        protected abstract void applyCriteria();

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         */
        protected abstract String getCustomValue(StateReportEntity entity) throws X2BaseException;

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         */
        protected boolean isIndicator() {
            return false;
        }

        /**
         * Inits the pgm map.
         */
        private void initPgmMap() {
            QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, m_pgmCriteria);
            pgmQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
            m_stdPgmMap =
                    getBroker().getGroupedCollectionByQuery(pgmQuery, StudentProgramParticipation.COL_STUDENT_OID, 200);
        }
    }

    /**
     * Constants.
     *
     */
    private static final String ALIAS_STD_ARRIVED_ENGLISH_LANGUAGE_LERNER = "all-std-ELEntryIntoUS";
    private static final String ALIAS_ELL = "PSIS14";
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_GRADES = "gradeCodes";
    private static final String INPUT_PARAM_INCLUDE_HEADING = "includeHeading";
    private static final String INPUT_PARAM_SCHOOLS = "schoolOids";
    private static final String INPUT_PARAM_TEST_DATE = "testDate";
    private static final String STRING_NO = "N";
    private static final String STRING_YES = "Y";
    private static final String PGM_CODE_IMMIGRANT_STATUS = "ESL";
    private static final String PGM_CODE_MILITARY_FAMILY = "MILITARY";

    /**
     * Class members.
     */
    protected String m_elEntryIntoUS;
    protected CTStudentHelper m_helper;
    protected Boolean m_isAllSchools;
    protected Set<String> m_setSchoolOids;
    protected PlainDate m_testDate;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        String heading = "";
        Boolean includeHeading = (Boolean) getParameter(INPUT_PARAM_INCLUDE_HEADING);
        if (includeHeading != null && includeHeading.booleanValue()) {
            heading = super.getHeading();
        }
        return heading;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();
        /*
         * Get core parameters
         */
        m_testDate = (PlainDate) getParameter(INPUT_PARAM_TEST_DATE);
        /*
         * Build helper object.
         */
        m_helper = new CTStudentHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, getCurrentContext().getStartDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_testDate);

        initializeIncludedSchools();
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            m_helper.getStudentCriteria().addAndCriteria(
                    StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
            List<String> grades = getInputGrades();
            if (!grades.isEmpty()) {
                m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, grades);
            }
            setQuery(m_helper.getStudentQuery(true));
            // Set the entity class
            setEntityClass(SasidTestingEntity.class);
            // Build maps of retriever functions and validator functions
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveEnglishLanguageLearner.CALC_ID, new RetrieveEnglishLanguageLearner());
            calcs.put(RetrieveMilitaryFamily.CALC_ID, new RetrieveMilitaryFamily());
            calcs.put(RetrieveELEntryIntoUS.CALC_ID, new RetrieveELEntryIntoUS());
            super.addCalcs(calcs);
        }
    }

    /**
     * Get a list of the grade codes selected.
     *
     * @return List
     */
    private List<String> getInputGrades() {
        List<String> grades = new LinkedList();
        if (!StringUtils.isEmpty((String) getParameter(INPUT_PARAM_GRADES))) {
            String[] rcdOids = ((String) getParameter(INPUT_PARAM_GRADES)).split(",");
            X2Criteria criteria = new X2Criteria();
            criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(rcdOids));
            ReportQueryByCriteria query =
                    new ReportQueryByCriteria(ReferenceCode.class, new String[] {ReferenceCode.COL_CODE}, criteria);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    grades.add((String) row[0]);
                }
            } finally {
                iterator.close();
            }
        }
        return grades;
    }

    /**
     * Initialize member fields.
     */
    private void initializeFields() {
        m_elEntryIntoUS = translateAliasToJavaName(ALIAS_STD_ARRIVED_ENGLISH_LANGUAGE_LERNER, false);
    }

    /**
     * Initialize included schools.
     */
    private void initializeIncludedSchools() {
        m_setSchoolOids = new HashSet();
        String schoolOids = (String) getParameter(INPUT_PARAM_SCHOOLS);
        m_isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);
        if (!m_isAllSchools.booleanValue() && !StringUtils.isEmpty(schoolOids)) {
            m_setSchoolOids = new HashSet<String>();
            m_setSchoolOids.addAll(Arrays.asList(schoolOids.split(",")));
        } else {
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            String[] columns = new String[] {X2BaseBean.COL_OID};
            ColumnQuery query = new ColumnQuery(SisSchool.class, columns, criteria);
            try (QueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    m_setSchoolOids.add((String) row[0]);
                }
            }
        }
        m_setSchoolOids.removeAll(m_helper.getExcludedSchools());
    }
}
