/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2012 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.in;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 *
 * This class is used by Indiana state for Enrollment and Mobility Report.
 *
 * @author Follett Software Company
 *
 */
public class EnrollmentAndMobility extends StateReportData {

    /**
     * This class has the entity for the Enrollment and Mobility Report to be used by Indiana State.
     * 
     * @author Follett Software Company
     *
     */
    public static class EnrollmentAndMobilityEntity extends StateReportEntity {

        /**
         * This list holds all the student enrollment spans for a particular student.
         */
        private List<StudentEnrollmentSpan> m_studentEnrollmentSpans = null;

        /**
         * This is the public no argument default constructor for dynamic instantiation.
         */
        public EnrollmentAndMobilityEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * This is the initialize method. This method loads the student enrollment spans for a
         * particular student
         * and then sets the row count so that the report can have multiple enrollment records for
         * the same
         * student.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) bean;
            m_studentEnrollmentSpans = ((EnrollmentAndMobility) data).m_helper.getStudentEnrollmentSpans(student, true);
            super.setRowCount(m_studentEnrollmentSpans.size());
        }

        /**
         * This method returns the current enrollment span row for the current student.
         *
         * @return Student enrollment span
         */
        public StudentEnrollmentSpan getEnrollmentSpan() {
            return m_studentEnrollmentSpans.get(getCurrentRow());
        }

    }

    /**
     * 
     * This retriever class is used to get the entry date, exit date, exit code and school id for a
     * valid
     * student enrollment record.
     * 
     * @author Follett Software Company
     *
     */
    public class EnrollmentRetriever implements FieldRetriever {

        /**
         * Some Constants for Entry date, Exit date, Exit code, School id and some specific exit
         * codes.
         * 
         */
        private static final String PARAM_ENTRY_DATE = "ENTRY_DATE";

        private static final String PARAM_EXIT_DATE = "EXIT_DATE";

        private static final String PARAM_EXIT_CODE = "EXIT_CODE";

        private static final String PARAM_SCHOOL_ID = "SCHOOL_ID";

        private static final String EXIT_CODE_00 = "00";

        private static final String EXIT_CODE_50 = "50";

        /**
         * This method retrieves the field value for each entity based on the enrollment span and
         * parameter value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            StudentEnrollment enrollment = null;
            StudentEnrollmentSpan span = ((EnrollmentAndMobilityEntity) entity).getEnrollmentSpan();

            String param = ((String) field.getParameter());

            if (span != null) {
                if (PARAM_SCHOOL_ID.equalsIgnoreCase(param)) {
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        SisSchool school = enrollment.getSchool();
                        if (school != null) {
                            value = school.getFieldValueByBeanPath(m_aliasSchoolIdField);
                        }
                    }

                } else if (PARAM_ENTRY_DATE.equalsIgnoreCase(param)) {
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getEnrollmentDate();
                    }
                } else if (PARAM_EXIT_DATE.equalsIgnoreCase(param)) {
                    enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getEnrollmentDate();
                    }
                } else if (PARAM_EXIT_CODE.equalsIgnoreCase(param)) {
                    enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        value = enrollment.getReasonCode();
                        value = lookupReferenceCodeByRefTbl(m_withdrawRefTableOid, (String) value,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        PlainDate exitDate = enrollment.getEnrollmentDate();

                        if (m_districtSchoolEndDate.equals(exitDate)) {
                            value = EXIT_CODE_50;
                        }

                    } else {
                        value = EXIT_CODE_00;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Alias for the Choice Scholarship variable.
     */
    protected static final String ALIAS_CHOICE_SCHOLARSHIP = "DOE CHOICE SCHOLARSHIP";

    /**
     * Alias for getting the school's state id.
     */
    protected static final String ALIAS_SCHOOL_ID = "StateId";

    /**
     * This constant defines the variable name of the choice scholarship check box on the input
     * form.
     */
    private static final String CHOICE_SCHOLARSHIP_STUDENTS_ONLY = "choiceOnly";

    /**
     * This field contains the java name for the alias of school's state id.
     */
    protected String m_aliasSchoolIdField;

    /**
     * This field holds the java name for the alias of choice scholarship field.
     */
    protected String m_aliasChoiceScholarshipField;

    /**
     * This field holds the boolean value for the choice scholarship check box on the input form.
     */
    protected Boolean m_choiceScholarshipStdOnly;

    /**
     * This is the end of school date for the district. It is retrieved using the current context of
     * the
     * current organization.
     */
    protected PlainDate m_districtSchoolEndDate;

    /**
     * StudentHistoryHelper class instance.
     */
    protected StudentHistoryHelper m_helper;

    /**
     * Reference table OID for the withdraw reference table
     */
    protected String m_withdrawRefTableOid;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     *      This method is the main initialization method for the entire export.
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_districtSchoolEndDate = getCurrentContext().getEndDate();
        m_withdrawRefTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_aliasSchoolIdField = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);

        m_aliasChoiceScholarshipField = translateAliasToJavaName(ALIAS_CHOICE_SCHOLARSHIP, true);
        m_choiceScholarshipStdOnly = (Boolean) getParameter(CHOICE_SCHOLARSHIP_STUDENTS_ONLY);

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_districtSchoolEndDate);

        QueryByCriteria query = m_helper.getStudentQuery(false);

        /*
         * Checking if the choice scholarship only check box is selected in the input form.
         * If it is, then we are adding it to the criteria.
         */
        if (m_choiceScholarshipStdOnly.booleanValue()) {
            X2Criteria choiceScholarshipCriteria = new X2Criteria();
            int choiceStudentOnly = 1;
            choiceScholarshipCriteria.addEqualTo(m_aliasChoiceScholarshipField, Integer.valueOf(choiceStudentOnly));
            query.getCriteria().addAndCriteria(choiceScholarshipCriteria);
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(query);
            setEntityClass(EnrollmentAndMobilityEntity.class);

            // Build a map of calculations/retrievers
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-ENROLL-RETRIEVE", new EnrollmentRetriever());
            super.addCalcs(calcs);

        }
    }
}
