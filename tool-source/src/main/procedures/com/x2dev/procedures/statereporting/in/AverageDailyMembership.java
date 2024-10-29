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

import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * This class is used by Indiana state for Average Daily Membership Report.
 *
 * @author Follett Software Company
 *
 */
public class AverageDailyMembership extends StateReportData {
    /**
     * This class has the entity for the Average Daily Membership Report to be used by Indiana
     * State.
     * 
     * @author Follett Software Company
     *
     */
    public static class AverageDailyMembershipEntity extends StateReportEntity {
        /**
         * This list holds all the student enrollment spans for a particular student.
         */
        private List<StudentEnrollmentSpan> m_studentEnrollmentSpans = null;

        /**
         * This is the public no argument default constructor for dynamic instantiation.
         */
        public AverageDailyMembershipEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         *
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         *
         *      This is the initialize method. This method loads the student enrollment spans for a
         *      particular student
         *      and then sets the row count so that the report can have multiple enrollment records
         *      for
         *      the same
         *      student.
         */

        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) bean;
            m_studentEnrollmentSpans =
                    ((AverageDailyMembership) data).m_helper.getStudentEnrollmentSpans(student, true);
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
     * The Class FullDayKGStudentRetriever.
     *
     * @author Follett Software Company
     *         This retriever class is used to get the flag boolean value to check if a student is
     *         full day
     *         kindergarten or part time kindergarten.
     */
    public class FullDayKGStudentRetriever implements FieldRetriever {
        /**
         * The below variables are constants that represent full day kindergarten
         * state code and local code respectively.
         */
        private static final String FULLDAY_KINDERGARTEN_STATE_CODE = "KG";
        private static final String FULLDAY_KINDERGARTEN_LOCAL_CODE = "FT";

        /**
         *
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         *      This method looks the state code and local code for a current grade code and then
         *      checks
         *      if the student
         *      is full time kindergarten or part time kindergarten.
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Boolean isFullDayKG = null;
            String aspenGradeCode = (String) getProperty(entity.getBean(), field.getBeanPath());
            String stateGradeCode = lookupReferenceCodeByRefTbl(m_gradeLevelRefTableOid, aspenGradeCode,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            String localGradeCode = lookupReferenceCodeByRefTbl(m_gradeLevelRefTableOid, aspenGradeCode,
                    ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
            isFullDayKG = Boolean.FALSE;
            if (FULLDAY_KINDERGARTEN_LOCAL_CODE.equals(localGradeCode)
                    && FULLDAY_KINDERGARTEN_STATE_CODE.equals(stateGradeCode)) {
                isFullDayKG = Boolean.TRUE;
            }
            return isFullDayKG;
        }
    }

    /**
     * This retriever class is used to get the instructional minutes, instructional days, adm type &
     * school id
     * for a valid student enrollment span record.
     * 
     * @author Follett Software Company
     *
     */
    public class MembershipDetailsRetriever implements FieldRetriever {
        /**
         * These are the param variables for the retriever.
         */
        private static final String PARAM_ADM_TYPE = "ADM_TYPE";

        private static final String PARAM_INSTRUCTIONAL_MINUTES = "INSTRUCTIONAL_MINUTES";

        private static final String PARAM_INSTRUCTIONAL_DAYS = "INSTRUCTIONAL_DAYS";

        private static final String PARAM_SCHOOL_ID = "SCHOOL_ID";

        /**
         *
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         *      This method retrieves the field value for each entity based on the enrollment span
         *      and
         *      parameter value.
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            StudentEnrollment enrollment = null;
            StudentEnrollmentSpan span = ((AverageDailyMembershipEntity) entity).getEnrollmentSpan();

            String param = (String) field.getParameter();
            if (PARAM_SCHOOL_ID.equals(param)) {
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    SisSchool school = enrollment.getSchool();
                    if (school != null) {
                        value = school.getFieldValueByBeanPath(m_aliasSchoolIdField);
                    }
                }
            } else if (PARAM_ADM_TYPE.equals(param)) {
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    value = enrollment.getEnrollmentCode();
                    value = lookupReferenceCodeByRefTbl(m_entryCodeRefTableOid, (String) value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (PARAM_INSTRUCTIONAL_DAYS.equals(param)) {
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    String enrollmentCode = enrollment.getEnrollmentCode();
                    enrollmentCode = lookupReferenceCodeByRefTbl(m_entryCodeRefTableOid, enrollmentCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (enrollmentCode != null && DUAL_ENROLLMENT_CODE.equals(enrollmentCode.trim())) {
                        value = enrollment.getFieldValueByBeanPath(m_aliasInstructionalDaysField);
                        if (value == null) {
                            value = Integer.toString(span.getMembershipDays());
                        }
                    }
                }
            } else if (PARAM_INSTRUCTIONAL_MINUTES.equals(param)) {
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    String enrollmentCode = enrollment.getEnrollmentCode();
                    enrollmentCode = lookupReferenceCodeByRefTbl(m_entryCodeRefTableOid, enrollmentCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (enrollmentCode != null && DUAL_ENROLLMENT_CODE.equals(enrollmentCode.trim())) {
                        value = enrollment.getFieldValueByBeanPath(m_aliasInstructionalMinutesField);
                    }
                }

            }
            return value;
        }

    }

    /**
     * This is the field validator class to validate the instructional minutes and instructional
     * days.
     * 
     * @author Follett Software Company
     *
     */
    public class InstrDaysMinsValidator implements FieldValidator {
        /**
         * This variable is a constant representing the field name of ADM Type in the Export Format.
         */
        private static final String ADM_TYPE = "ADM Type";

        /**
         * These are the param variables for the validator.
         */
        private static final String PARAM_INSTRUCTIONAL_MINUTES = "INSTRUCTIONAL_MINUTES";

        private static final String PARAM_INSTRUCTIONAL_DAYS = "INSTRUCTIONAL_DAYS";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         *      This method validates the instructional days and instructional minutes.
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            List<StateReportValidationError> errorList = new ArrayList<StateReportValidationError>();
            String entryCode = entity.getFieldValue(ADM_TYPE);

            if (entryCode != null && DUAL_ENROLLMENT_CODE.equals(entryCode.trim())) {
                String param = (String) field.getParameter();
                if (param != null && PARAM_INSTRUCTIONAL_DAYS.equals(param.trim())) {
                    // It is dual enrollment type
                    if (value == null || "".equals(value.trim())) {
                        errorList.add(new StateReportValidationError(entity, field, value,
                                "For Dual Enrollments students, Instructional Days cannot be empty."));
                    } else {
                        try {
                            int instrDays = Integer.parseInt(value);
                            if (instrDays < 1 || instrDays > 180) {
                                errorList.add(new StateReportValidationError(entity, field, value,
                                        "For Dual Enrollments students, Instructional Days should be between 1 and 180(Both inclusive)."));
                            }
                        } catch (NumberFormatException nfe) {
                            errorList.add(new StateReportValidationError(entity, field, value,
                                    "For Dual Enrollments students, Instructional Days is invalid string. It should be a number."));
                        }
                    }
                } else if (param != null && PARAM_INSTRUCTIONAL_MINUTES.equals(param.trim())) {
                    // It is dual enrollment type
                    if (value == null || "".equals(value.trim())) {
                        errorList.add(new StateReportValidationError(entity, field, value,
                                "For Dual Enrollments students, Instructional minutes cannot be empty."));
                    } else {
                        try {
                            int instrDays = Integer.parseInt(value);
                            if (instrDays < 1 || instrDays > 400) {
                                errorList.add(new StateReportValidationError(entity, field, value,
                                        "For Dual Enrollments students, Instructional minutes should be between 1 and 480(Both inclusive)."));
                            }
                        } catch (NumberFormatException nfe) {
                            errorList.add(new StateReportValidationError(entity, field, value,
                                    "For Dual Enrollments students, Instructional minutes is invalid string. It should be a number."));
                        }
                    }
                }
            }
            return errorList;
        }
    }

    /**
     * Alias for getting Organization table's [DOE CORP NUMBER] field.
     */
    protected static final String ALIAS_CORPORATION_ID = "DOE CORP NUMBER";

    /**
     * Alias for getting Student_Enrollment table's [DOE INSTR DAYS] field
     */
    protected static final String ALIAS_INSTR_DAYS = "DOE INSTR DAYS";

    /**
     * Alias for getting Student_Enrollment table's [DOE INSTR MINUTES] field
     */
    protected static final String ALIAS_INSTR_MINS = "DOE INSTR MINUTES";

    /**
     * Alias for getting the school's state id.
     */
    protected static final String ALIAS_SCHOOL_ID = "StateId";

    /**
     * This is the state code for DUAL ENROLLMENT MEMBERSHIP STUDENTS
     */
    protected static final String DUAL_ENROLLMENT_CODE = "6";

    /**
     * This field contains the java name for the alias of Student_Enrollment table's [DOE INSTR
     * DAYS] field.
     */
    protected String m_aliasInstructionalDaysField;

    /**
     * This field contains the java name for the alias of Student_Enrollment table's [DOE INSTR
     * MINUTES] field.
     */
    protected String m_aliasInstructionalMinutesField;

    /**
     * This field contains the java name for the alias of school's state id.
     */
    protected String m_aliasSchoolIdField;

    /**
     * This is the oid for the county codes reference table.
     */
    protected String m_countyCodesRefTableOid;

    /**
     * This is the end of school date for the district. It is retrieved using the current context of
     * the
     * current organization.
     */
    protected PlainDate m_districtSchoolEndDate;

    /**
     * This is the oid for the entry code reference table.
     */
    protected String m_entryCodeRefTableOid;

    /**
     * This is the oid for the grade level reference table.
     */
    protected String m_gradeLevelRefTableOid;

    /**
     * StudentHistoryHelper class instance.
     */
    protected StudentHistoryHelper m_helper;

    /**
     *
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     *      This method is the main initialization method for the entire export.
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_districtSchoolEndDate = getCurrentContext().getEndDate();
        m_entryCodeRefTableOid =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);

        m_aliasSchoolIdField = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);

        m_aliasInstructionalDaysField = translateAliasToJavaName(ALIAS_INSTR_DAYS, true);
        m_aliasInstructionalMinutesField = translateAliasToJavaName(ALIAS_INSTR_MINS, true);

        DataDictionaryField countyDataDictionaryField = getDataDictionaryField(Address.class, Address.COL_COUNTY);
        m_countyCodesRefTableOid = countyDataDictionaryField.getReferenceTableOid();

        DataDictionaryField dataDictionaryField = getDataDictionaryField(SisStudent.class, SisStudent.COL_GRADE_LEVEL);
        m_gradeLevelRefTableOid = dataDictionaryField.getReferenceTableOid();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, getCurrentContext().getStartDate());
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_districtSchoolEndDate);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(AverageDailyMembershipEntity.class);

            // Build a map of calculations/retrievers
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STD-ADM-RTRVE", new MembershipDetailsRetriever());
            calcs.put("STD-ADMFULLKG-RTRVE", new FullDayKGStudentRetriever());
            super.addCalcs(calcs);

            Map<String, FieldValidator> validatorMapForState = new HashMap<String, FieldValidator>();
            validatorMapForState.put("INSTR-DAYSMINS-VAL", new InstrDaysMinsValidator());
            addValidators(validatorMapForState);
        }
    }

}
