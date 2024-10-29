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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * New Jersey state procedure for PreID export.
 *
 * @author X2 Development Corporation
 */

public class PreIDForElls extends StateReportData {
    private static final String CALC_504 = "504STATUS";
    private static final String CALC_ENTRY_DATE = "ENTRYDATE";
    private static final String CALC_ETHNICITY = "ETHNICITY";
    private static final String CALC_IEP_STATUS = "IEPSTATUS";
    private static final String CALC_LENGTH_IN_ELL = "LENGTHINELL";
    private static final String CALC_SCHOOL_DATA = "SCHOOLDATA";
    private static final String CALC_TEST_TYPE = "TESTTYPE";
    private static final String CALC_TRUNCATE = "TRUNCATE";
    private static final String PGM_CODE_LEP = "LEP";

    /**
     * Entity class for PreId Record Student Level export.
     *
     * @author X2 Development Corporation
     */

    public static class PreIDEntity extends StateReportEntity {
        private SisSchool m_school;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public PreIDEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            List<StudentEnrollmentSpan> enrollmentSpans =
                    ((PreIDForElls) getData()).m_helper.getStudentEnrollmentSpans((Student) bean, true);
            setSchool(null);
            PlainDate reportDate = ((PreIDForElls) data).m_reportDate;
            boolean enrolledOnReportDate = false;

            for (StudentEnrollmentSpan span : enrollmentSpans) {
                // check if student has an enrollment span that overlaps the report date, if so set
                // the student's school as of date
                if (span.getFirstActiveDate() == null
                        || (span.getFirstActiveDate() != null && (span.getFirstActiveDate().equals(reportDate)
                                || span.getFirstActiveDate().before(reportDate))) &&
                                span.getLastActiveDate() == null
                        || (span.getLastActiveDate() != null && (span.getLastActiveDate().equals(reportDate)
                                || !span.getLastActiveDate().before(reportDate)))) {
                    setSchool(span.getSchool());
                    enrolledOnReportDate = true;
                    break;
                }
            }
            if (!enrolledOnReportDate) {
                setRowCount(0);
            }
        }

        /**
         * This method returns the student bean.
         *
         * @return Sis student
         */
        public SisStudent getStudent() {
            return (SisStudent) getBean();
        }

        /**
         * This method returns the latest student enrollment record for Entry type before the report
         * date.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getStudentEnrollment() {
            return ((PreIDForElls) getData()).m_helper.getEnrollmentForDate(
                    getBean().getOid(), ((PreIDForElls) getData()).m_reportDate, StudentEnrollment.ENTRY);
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
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Sets the school.
         *
         * @param m_school void
         */
        public void setSchool(SisSchool m_school) {
            this.m_school = m_school;
        }
    }

    /**
     * Retrieves the Entry Date into the district for the most recent enrollment span.
     */
    protected class RetrieveEntryDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            StudentEnrollment mostRecentEntry = ((PreIDEntity) entity).getStudentEnrollment();
            PlainDate mostRecentEntryDate = null;
            if (mostRecentEntry != null) {
                mostRecentEntryDate = mostRecentEntry.getEnrollmentDate();
            }
            return mostRecentEntryDate;
        }
    }

    /**
     * Returns "H" if student is flagged as hispanic/latino, otherwise returns "N".
     *
     */
    protected class RetrieveEthnicity implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = getProperty(entity.getBean(), field.getBeanPath());
            if (((Boolean) value).booleanValue()) {
                value = CONSTANT_HISPANIC;
            } else {
                value = CONSTANT_NO;
            }
            return value;
        }
    }

    /**
     * Retrieves the IEP status.
     */
    protected class RetrieveIepStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CONSTANT_NO;
            SisStudent student = ((PreIDEntity) entity).getStudent();
            String activeCode = PreferenceManager.getPreferenceValue(data.getOrganization(),
                    SisPreferenceConstants.SPED_ACTIVE_CODE);
            if (activeCode != null && activeCode.equalsIgnoreCase(student.getSpedStatusCode())) {
                value = CONSTANT_YES;
            }
            return value;
        }
    }

    /**
     * Retrieves the Length of time that a student has been in their most recent ELL program.
     * This value is exported in number of years rounding down. Maximum value of 25.
     *
     */
    protected class RetrieveLengthInEll implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            int lengthInEll = CONSTANT_ZERO;
            SisStudent student = ((PreIDEntity) entity).getStudent();
            String studentOid = student.getOid();

            // Look up most recent ELL program for student (student could have more than one)
            // If LEP has a null start date, it is not included.

            X2Criteria activeLEPCriteria = new X2Criteria();
            activeLEPCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_LEP);
            activeLEPCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
            activeLEPCriteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, studentOid);
            activeLEPCriteria.addNotNull(StudentProgramParticipation.COL_START_DATE);

            X2Criteria activeLEPEndDateCriteria1 = new X2Criteria();
            activeLEPEndDateCriteria1.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);

            X2Criteria activeLEPEndDateCriteria2 = new X2Criteria();
            activeLEPEndDateCriteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);

            activeLEPEndDateCriteria1.addOrCriteria(activeLEPEndDateCriteria2);

            activeLEPCriteria.addAndCriteria(activeLEPEndDateCriteria1);

            QueryByCriteria lepQuery = new QueryByCriteria(StudentProgramParticipation.class, activeLEPCriteria);
            lepQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);

            StudentProgramParticipation lepProgram = (StudentProgramParticipation) getBroker().getBeanByQuery(lepQuery);
            if (lepProgram != null) {
                PlainDate programStartDate = lepProgram.getStartDate();
                Calendar programDate = Calendar.getInstance();
                programDate.setTime(programStartDate);
                Calendar reportDate = Calendar.getInstance();
                reportDate.setTime(m_reportDate);
                reportDate.add(Calendar.YEAR, -1);

                while (programDate.before(reportDate) && lengthInEll < 26) {
                    lengthInEll++;
                    reportDate.add(Calendar.YEAR, -1);
                }

            }
            return Integer.valueOf(lengthInEll);
        }
    }

    /**
     * Retrieves the data which relies upon the students school as of report date.
     * Uses param to identify which field lookup to use
     */
    protected class RetrieveSchoolData implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            SisSchool school = ((PreIDEntity) entity).getSchool();
            String param = (String) field.getParameter();
            if (school != null) {
                if (PARAM_DIST_NUM.equals(param)) {
                    value = (String) school.getFieldValueByBeanPath(m_districtCode);
                } else if (PARAM_SCHOOL_NAME.equals(param)) {
                    value = school.getName();
                } else if (PARAM_SCHOOL_NUM.equals(param)) {
                    value = (String) school.getFieldValueByBeanPath(m_schoolCode);
                }
            }
            // truncate values based on the maximum length defined in the export format
            try {
                int max = field.getMaxLength();
                if (value != null && max > 0 && max < value.length()) {
                    value = value.substring(0, max);
                }
            } catch (NumberFormatException e) {
                // Improper parse value
            }
            return value;
        }
    }

    /**
     * Retrieves the Section 504 status.
     */
    protected class RetrieveSection504 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = CONSTANT_NO;
            SisStudent student = ((PreIDEntity) entity).getStudent();
            String activeCode = PreferenceManager.getPreferenceValue(student.getSchool(),
                    SisPreferenceConstants.SECTION_504_STUDENT_ACTIVE_CODE);
            if (activeCode != null && activeCode.equalsIgnoreCase(student.getSection504StatusCode())) {
                value = CONSTANT_YES;
            }
            return value;
        }
    }

    /**
     * Retrieves the Test Type.
     */
    protected class RetrieveTestType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((PreIDForElls) data).m_testType;
        }
    }

    /**
     * Class to truncate values based on their maximum length in export format.
     */
    protected class RetrieveTruncate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param dataDefault StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData dataDefault, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            try {
                int max = field.getMaxLength();
                if (value != null && max > 0 && max < value.length()) {
                    value = value.substring(0, max);
                }
            } catch (NumberFormatException e) {
                // Improper parse value
            }
            return value;
        }
    }

    /**
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_CODE = "DOE DISTRICT CODE";
    protected static final String ALIAS_SCHOOL_CODE = "DOE SCHOOL CODE";

    /**
     * Parameters
     */
    protected static final String PARAM_DIST_NUM = "DISTNUM";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SCHOOL_NAME = "SCHOOLNAME";
    protected static final String PARAM_SCHOOL_NUM = "SCHOOLNUM";
    protected static final String PARAM_TEST_TYPE = "testType";



    /**
     * Other Constants
     */
    protected static final int CONSTANT_ZERO = 0;
    protected static final String CONSTANT_HISPANIC = "H";
    protected static final String CONSTANT_NO = "N";
    protected static final String CONSTANT_YES = "Y";
    protected static final String CONSTANT_TRUE = "1";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected String m_testType;
    protected String m_districtCode;
    protected String m_schoolCode;

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            /*
             * Adding in the StudentProgramParticipationCriteria to the to the
             * StudentSelectionCriteria.
             * StudentProgramParticipationCriteria says that the Access for ELL's Pre-ID submission
             * are for those students who are tagged as active and have an active LEP Start Date
             * that falls within the range of the export date. If they have an end date,
             * they would not be included unless the end date fell inside the date range.
             *
             */
            X2Criteria studentSelectionCriteria = m_helper.getStudentCriteria();

            X2Criteria activeLEPProgramCriteria = getActiveLEPParticipationCriteria();
            if (!isSchoolContext()) {
                activeLEPProgramCriteria
                        .addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                                + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                                Boolean.TRUE);
                activeLEPProgramCriteria
                        .addNotEqualTo(StudentProgramParticipation.REL_STUDENT + ModelProperty.PATH_DELIMITER
                                + Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                                Boolean.TRUE);
            }
            SubQuery activeLEPProgramSubQuery = new SubQuery(StudentProgramParticipation.class,
                    StudentProgramParticipation.COL_STUDENT_OID, activeLEPProgramCriteria);
            studentSelectionCriteria.addIn(X2BaseBean.COL_OID, activeLEPProgramSubQuery);

            applyInputCriteria(studentSelectionCriteria, true, null);

            QueryByCriteria query = new QueryByCriteria(SisStudent.class, studentSelectionCriteria);

            setQuery(query);

            setEntityClass(PreIDEntity.class);

            // Build a map of calculations/retrievers

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_504, new RetrieveSection504());
            calcs.put(CALC_TRUNCATE, new RetrieveTruncate());
            calcs.put(CALC_TEST_TYPE, new RetrieveTestType());
            calcs.put(CALC_IEP_STATUS, new RetrieveIepStatus());
            calcs.put(CALC_SCHOOL_DATA, new RetrieveSchoolData());
            calcs.put(CALC_ENTRY_DATE, new RetrieveEntryDate());
            calcs.put(CALC_LENGTH_IN_ELL, new RetrieveLengthInEll());
            calcs.put(CALC_ETHNICITY, new RetrieveEthnicity());

            HashMap validators = new HashMap<String, FieldRetriever>();
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_testType = (String) getParameter(PARAM_TEST_TYPE);

        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        m_districtCode = translateAliasToJavaName(ALIAS_DISTRICT_CODE, true);
        m_schoolCode = translateAliasToJavaName(ALIAS_SCHOOL_CODE, true);

    }


    /**
     * Returns the criteria that retrieves all programs that should be included in the export.
     *
     * @return X2Criteria
     */
    private X2Criteria getActiveLEPParticipationCriteria() {
        X2Criteria activeLEPParticipationCriteria = new X2Criteria();
        activeLEPParticipationCriteria.addNotNull(StudentProgramParticipation.COL_PROGRAM_CODE);
        activeLEPParticipationCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, PGM_CODE_LEP);
        activeLEPParticipationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

        X2Criteria activeLEPEndDateCriteria1 = new X2Criteria();
        activeLEPEndDateCriteria1.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);

        X2Criteria activeLEPEndDateCriteria2 = new X2Criteria();
        activeLEPEndDateCriteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);

        activeLEPEndDateCriteria1.addOrCriteria(activeLEPEndDateCriteria2);

        activeLEPParticipationCriteria.addAndCriteria(activeLEPEndDateCriteria1);

        return activeLEPParticipationCriteria;
    }
}
