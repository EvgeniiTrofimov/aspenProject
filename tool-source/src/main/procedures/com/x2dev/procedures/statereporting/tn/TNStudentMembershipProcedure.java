/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SchoolCapacity;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.BusinessRules;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.sis.model.business.StudentEnrollmentProcedure;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * J Custom Student Membership Procedure.
 *
 * @author Follett Software Company
 */
public class TNStudentMembershipProcedure implements StudentEnrollmentProcedure {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    private static final String DOE_SCHOOL_WIDE = "DOE SCHOOL WIDE";
    private static final String ALIAS_ENR_INSTR_SERV_TYPE = "DOE INSTR SERVICE TYPE";
    private static final String ALIAS_EXCLUDE_FROM_AUTO = "Exclude from auto";
    private static final String ALIAS_STD_CAL_CODE = "all-enr-StudentCalendar";

    private static final String DEFAULT_ENR_INSTR_SERV_TYPE = "P";

    private static final String NIGHTLY_FIRST_NAME = "DOE NIGHTLY FNAME";
    private static final String NIGHTLY_LAST_NAME = "DOE NIGHTLY LNAME";
    private static final String NIGHTLY_MIDDLE_NAME = "DOE NIGHTLY MNAME";
    private static final String NIGHTLY_SSN = "DOE NIGHTLY SSN";
    private static final String NIGHTLY_STUDENT_PIN = "DOE NIGHTLY PIN";

    private static final String STUDENT_PIN = "DOE PIN";

    // Instance variables
    private X2Broker m_broker = null;
    private Map<String, StudentProgramParticipation> m_existingStudentPrograms;
    private Map<String, Boolean> m_excludeFromAuto;

    /**
     * Constructs a new TennesseeStudentEnrollmentProcedure.
     */
    public TNStudentMembershipProcedure() {
        m_existingStudentPrograms = new HashMap<String, StudentProgramParticipation>();
        m_excludeFromAuto = new HashMap<String, Boolean>();
    }

    // --------------------------------------------------------------------------------------------
    // StudentEnrollmentProcedure Methods
    // --------------------------------------------------------------------------------------------

    /**
     * Initialize.
     *
     * @param broker X2Broker
     * @param privilegeSet PrivilegeSet
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#initialize(
     *      com.follett.fsc.core.k12.business.X2Broker,
     *      com.follett.fsc.core.k12.business.PrivilegeSet)
     */
    @Override
    public void initialize(X2Broker broker, PrivilegeSet privilegeSet) {
        m_broker = broker;
        m_existingStudentPrograms = new HashMap<String, StudentProgramParticipation>();
        m_excludeFromAuto = new HashMap<String, Boolean>();

        loadProgramExcludeFlags();
    }

    /**
     * After change enrollment status.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterChangeEnrollmentStatus(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterChangeEnrollmentStatus(SisStudent student, StudentEnrollment enrollment) {
        addCalCodeToEnr(student, enrollment);
        addDefaultValues(enrollment);

        return new ArrayList<ValidationError>(0);
    }

    /**
     * After change year of graduation.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterChangeYearOfGraduation(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterChangeYearOfGraduation(SisStudent student, StudentEnrollment enrollment) {
        addCalCodeToEnr(student, enrollment);
        addDefaultValues(enrollment);

        return new ArrayList<ValidationError>(0);
    }

    /**
     * After registrater student.
     *
     * @param student SisStudent
     * @param entryEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterRegistraterStudent(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterRegistraterStudent(SisStudent student, StudentEnrollment entryEnrollment) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        createStudentPrograms(student, entryEnrollment, errors);

        updateStudentNightlyFields(student, errors);
        addDefaultValues(entryEnrollment);

        return errors;
    }

    /**
     * After transfer student.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @param entryEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterTransferStudent(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterTransferStudent(SisStudent student,
                                                      StudentEnrollment withdrawEnrollment,
                                                      StudentEnrollment entryEnrollment) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        SisSchool entrySchool = entryEnrollment.getSchool();

        m_existingStudentPrograms = getPrograms(student);

        for (StudentProgramParticipation withdrawalProgram : m_existingStudentPrograms.values()) {
            boolean entrySchoolwide;
            String programCode = withdrawalProgram.getProgramCode();

            if (programCode != null) {
                // Determines if the new school identifies the program as schoolwide
                entrySchoolwide = isSchoolWide(entrySchool, programCode);

                if (!entrySchoolwide) {
                    // Close program records that aren't schoolwide at the new school
                    // closeStudentProgram(student, programCode, withdrawEnrollment, errors);
                    closeStudentProgram(withdrawalProgram, withdrawEnrollment, false);
                }
            }
        }

        // Close transportation records at previous school
        closeAllTransportation(student, withdrawEnrollment, errors);

        // Creates all schoolwide programs at the new school that don't already exist
        createStudentPrograms(student, entryEnrollment, errors);

        // Copy UDFs from previous enrollment record to the new enrollment record
        copyEnrollmentUDFs(withdrawEnrollment, entryEnrollment);

        addCalCodeToEnr(student, withdrawEnrollment);
        addDefaultValues(entryEnrollment);
        addDefaultValues(withdrawEnrollment);

        return errors;
    }

    /**
     * After withdraw student.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterWithdrawStudent(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterWithdrawStudent(SisStudent student, StudentEnrollment withdrawEnrollment) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        closeAllStudentPrograms(student, withdrawEnrollment, errors);
        closeAllTransportation(student, withdrawEnrollment, errors);

        addCalCodeToEnr(student, withdrawEnrollment);
        addDefaultValues(withdrawEnrollment);

        return errors;
    }

    /**
     * Validate change enrollment status.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @param reason String
     * @param status String
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateChangeEnrollmentStatus(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      java.lang.String, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateChangeEnrollmentStatus(SisStudent student,
                                                                PlainDate date,
                                                                String reason,
                                                                String status,
                                                                StudentEnrollment studentEnrollment) {
        return new ArrayList<ValidationError>(0);
    }

    /**
     * Validate change year of graduation.
     *
     * @param student SisStudent
     * @param date PlainDate
     * @param reason String
     * @param yog int
     * @param gradeLevel String
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateChangeYearOfGraduation(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      int,
     *      java.lang.String, com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateChangeYearOfGraduation(SisStudent student,
                                                                PlainDate date,
                                                                String reason,
                                                                int yog,
                                                                String gradeLevel,
                                                                StudentEnrollment studentEnrollment) {
        return new ArrayList<ValidationError>(0);
    }

    /**
     * Validate register student.
     *
     * @param student SisStudent
     * @param yog int
     * @param status String
     * @param school SisSchool
     * @param schoolCapacity SchoolCapacity
     * @param enrollmentPropertyValues Map<ModelProperty,Object>
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateRegisterStudent(
     *      com.x2dev.sis.model.beans.SisStudent, int, java.lang.String,
     *      com.x2dev.sis.model.beans.SisSchool,
     *      com.follett.fsc.core.k12.beans.SchoolCapacity, java.util.Map)
     */
    @Override
    public List<ValidationError> validateRegisterStudent(SisStudent student,
                                                         int yog,
                                                         String status,
                                                         SisSchool school,
                                                         SchoolCapacity schoolCapacity,
                                                         Map<ModelProperty, Object> enrollmentPropertyValues) {
        return new ArrayList<ValidationError>(0);
    }

    /**
     * Validate transfer student.
     *
     * @param student SisStudent
     * @param user SisUser
     * @param withdrawalDate PlainDate
     * @param withdrawalCode String
     * @param withdrawalReason String
     * @param entryDate PlainDate
     * @param entryCode String
     * @param entryReason String
     * @param destinationSchool SisSchool
     * @param destinationSchoolCapacity SchoolCapacity
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateTransferStudent(
     *      com.x2dev.sis.model.beans.SisStudent, com.x2dev.sis.model.beans.SisUser,
     *      com.x2dev.utils.types.PlainDate, java.lang.String, java.lang.String,
     *      com.x2dev.utils.types.PlainDate, java.lang.String, java.lang.String,
     *      com.x2dev.sis.model.beans.SisSchool, com.follett.fsc.core.k12.beans.SchoolCapacity,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateTransferStudent(SisStudent student,
                                                         SisUser user,
                                                         PlainDate withdrawalDate,
                                                         String withdrawalCode,
                                                         String withdrawalReason,
                                                         PlainDate entryDate,
                                                         String entryCode,
                                                         String entryReason,
                                                         SisSchool destinationSchool,
                                                         SchoolCapacity destinationSchoolCapacity,
                                                         StudentEnrollment studentEnrollment) {
        List<ValidationError> errors = null;
        if (StringUtils.isEmpty(withdrawalCode) || StringUtils.isEmpty(entryCode)) {
            errors = new ArrayList<ValidationError>(1);
            errors.add(new ValidationError(ValidationConstants.BUSINESS_RULE_VIOLATION,
                    Integer.valueOf(BusinessRules.ENROLLMENT_MANAGER_ARGUMENTS), "Code"));
        }
        return errors;
    }

    /**
     * Validate withdraw student.
     *
     * @param student SisStudent
     * @param user User
     * @param date PlainDate
     * @param code String
     * @param reason String
     * @param status String
     * @param studentEnrollment StudentEnrollment
     * @return List
     * @see com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateWithdrawStudent(
     *      com.x2dev.sis.model.beans.SisStudent, com.follett.fsc.core.k12.beans.User,
     *      com.x2dev.utils.types.PlainDate, java.lang.String, java.lang.String, java.lang.String,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateWithdrawStudent(SisStudent student,
                                                         User user,
                                                         PlainDate date,
                                                         String code,
                                                         String reason,
                                                         String status,
                                                         StudentEnrollment studentEnrollment) {
        return null;
    }

    /**
     * Add current calendar code of a student to an enrollment.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     */
    private void addCalCodeToEnr(SisStudent student, StudentEnrollment enrollment) {
        if (!enrollment.isDirty()) {
            enrollment.setFieldValueByAlias(ALIAS_STD_CAL_CODE, student.getCalendarCode());
            m_broker.saveBean(enrollment);
        }
    }

    /**
     * Initialize fields with default values if needed.
     *
     * @param enrollment StudentEnrollment
     */
    private void addDefaultValues(StudentEnrollment enrollment) {
        String serviceType = (String) enrollment.getFieldValueByAlias(ALIAS_ENR_INSTR_SERV_TYPE);
        if (StringUtils.isEmpty(serviceType)) {
            enrollment.setFieldValueByAlias(ALIAS_ENR_INSTR_SERV_TYPE, DEFAULT_ENR_INSTR_SERV_TYPE);
            m_broker.saveBean(enrollment);
        }
    }

    /**
     * Closes all of the student's student program participation records.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @param errors List<ValidationError>
     */
    private void closeAllStudentPrograms(SisStudent student,
                                         StudentEnrollment withdrawEnrollment,
                                         List<ValidationError> errors) {
        SisSchool school = withdrawEnrollment.getSchool();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());
        criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, m_broker.getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                StudentProgramParticipation program = (StudentProgramParticipation) iterator.next();

                closeStudentProgram(program, withdrawEnrollment, true);
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Closes the student's transportation records at a given school.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @param errors List<ValidationError>
     */
    private void closeAllTransportation(SisStudent student,
                                        StudentEnrollment withdrawEnrollment,
                                        List<ValidationError> errors) {
        SisSchool school = withdrawEnrollment.getSchool();
        PlainDate withdrawalDate = withdrawEnrollment.getEnrollmentDate();

        /*
         * Build criteria for transportation
         */
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentTransportation.COL_SCHOOL_OID, school.getOid());
        criteria.addEqualTo(StudentTransportation.COL_STUDENT_OID, student.getOid());

        /*
         * Load records where end date is after the withdrawal date or null
         */
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addGreaterThan(StudentTransportation.COL_END_DATE, withdrawalDate);

        X2Criteria andCriteria = new X2Criteria();
        andCriteria.addIsNull(StudentTransportation.COL_END_DATE);
        andCriteria.addOrCriteria(orCriteria);

        criteria.addAndCriteria(andCriteria);

        /*
         * Build and execute the query
         */
        QueryByCriteria query = new QueryByCriteria(StudentTransportation.class, criteria);
        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                StudentTransportation transportation = (StudentTransportation) iterator.next();
                transportation.setEndDate(withdrawalDate);
                errors.addAll(m_broker.saveBean(transportation));
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Closes the student's student program participation record. </br>
     * Use it when close program after student was transferred or withdrawn . </br>
     * When close program after withdraw we will not check if program is flagged with exclude from
     * auto.
     *
     * @param withdrawalProgram StudentProgramParticipation
     * @param withdrawEnrollment StudentEnrollment
     * @param isWithdrawn boolean
     */
    private void closeStudentProgram(StudentProgramParticipation withdrawalProgram,
                                     StudentEnrollment withdrawEnrollment,
                                     boolean isWithdrawn) {
        String programCode = withdrawalProgram.getProgramCode();

        boolean closeProgram = !m_excludeFromAuto.get(programCode).booleanValue() || isWithdrawn;

        if (closeProgram) {
            PlainDate date = withdrawEnrollment.getEnrollmentDate();

            if (withdrawalProgram != null) {
                withdrawalProgram.setEndDate(date);
            }

            m_broker.saveBeanForced(withdrawalProgram);
        }
    }

    /**
     * Copies all of the user defined field values from the source student enrollment record to
     * the target student enrollment record.
     *
     * @param source StudentEnrollment
     * @param target StudentEnrollment
     */
    private void copyEnrollmentUDFs(StudentEnrollment source, StudentEnrollment target) {
        if (source != null && target != null) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
            List<DataDictionaryField> fields = dictionary.getFieldsForContext(StudentEnrollment.class.getName());

            for (DataDictionaryField field : fields) {
                if (field.isEnabled() && field.getDataFieldConfig().getDataField().isUserDefinedField()) {
                    Object value = source.getFieldValueByBeanPath(field.getJavaName());
                    target.setFieldValueByBeanPath(field.getJavaName(), value);
                }
            }
        }
    }

    /**
     * Creates a student program record.
     *
     * @param student SisStudent
     * @param entryEnrollment StudentEnrollment
     * @param programCode String
     * @param errors List<ValidationError>
     */
    private void createStudentProgram(SisStudent student,
                                      StudentEnrollment entryEnrollment,
                                      String programCode,
                                      List<ValidationError> errors) {
        boolean excludeFromAuto = m_excludeFromAuto.get(programCode).booleanValue();

        if (!excludeFromAuto) {
            m_existingStudentPrograms = getPrograms(student);

            // Retrieves the program record in the new school corresponding if one exists
            StudentProgramParticipation entryProgram = m_existingStudentPrograms.get(programCode);

            // Creates a program record at the incoming school if it does not already exist
            if (entryProgram == null) {
                StudentProgramParticipation program = new StudentProgramParticipation(m_broker.getPersistenceKey());

                PlainDate startDate = entryEnrollment.getEnrollmentDate();

                program.setStartDate(startDate);
                program.setStudentOid(student.getOid());
                program.setProgramCode(programCode);

                errors.addAll(m_broker.saveBean(program));
            }
        }
    }

    /**
     * Creates all necessary student programs at the incoming school.
     *
     * @param student SisStudent
     * @param entryEnrollment StudentEnrollment
     * @param errors List<ValidationError>
     */
    private void createStudentPrograms(SisStudent student,
                                       StudentEnrollment entryEnrollment,
                                       List<ValidationError> errors) {
        SisSchool school = entryEnrollment.getSchool();
        ArrayList<String> schoolwidePrograms = getSchoolwidePrograms(school);

        for (String programCode : schoolwidePrograms) {
            createStudentProgram(student, entryEnrollment, programCode, errors);
        }
    }

    /**
     * Loads the student programs to a map keyed off of the program code.
     *
     * @param student SisStudent
     * @return HashMap&lt;String, StudentProgramParticipation&gt;
     */
    private HashMap<String, StudentProgramParticipation> getPrograms(SisStudent student) {
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());
        criteria.addIsNull(StudentProgramParticipation.COL_END_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        HashMap<String, StudentProgramParticipation> programs = (HashMap<String, StudentProgramParticipation>) m_broker
                .getMapByQuery(query, StudentProgramParticipation.COL_PROGRAM_CODE, 1);

        return programs;
    }

    /**
     * Returns a list of school wide programs.
     *
     * @param school SisSchool
     * @return ArrayList&lt;String&gt;
     */
    private ArrayList<String> getSchoolwidePrograms(SisSchool school) {
        ArrayList<String> programs = new ArrayList<String>();

        if (school != null) {
            programs = StringUtils.convertDelimitedStringToList((String) school.getFieldValueByAlias(DOE_SCHOOL_WIDE),
                    ',', true);
        }

        return programs;
    }

    /**
     * Returns whether or not the program is school wide.
     *
     * @param school SisSchool
     * @param programCode String
     * @return boolean
     */
    private boolean isSchoolWide(SisSchool school, String programCode) {
        boolean isSchoolWide = false;

        ArrayList<String> schoolwidePrograms = null;

        schoolwidePrograms = getSchoolwidePrograms(school);

        if (!schoolwidePrograms.isEmpty()) {
            if (schoolwidePrograms.contains(programCode)) {
                isSchoolWide = true;
            }
        }

        return isSchoolWide;
    }

    /**
     * Loads the school wide programs marking which program are / aren't to be excluded from auto
     * create / closing
     * by this procedure.
     */
    private void loadProgramExcludeFlags() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();

                dictionary = DataDictionary.getDistrictDictionary(code.getExtendedDataDictionary(),
                        m_broker.getPersistenceKey());

                String exclude = (String) code.getFieldValueByAlias(ALIAS_EXCLUDE_FROM_AUTO, dictionary);
                String programCode = code.getCode();

                boolean excludeFromAuto = "1".equals(exclude);

                m_excludeFromAuto.put(programCode, Boolean.valueOf(excludeFromAuto));
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Set the nightly field .
     *
     * @param bean X2BaseBean
     * @param value String
     * @param alias String
     */
    private void setNightlyField(X2BaseBean bean, String value, String alias) {
        if (!StringUtils.isEmpty(value)) {
            bean.setFieldValueByAlias(alias, value);
        }
    }

    /**
     * Initialize the nightly fields used for comparisons to generate previous fields by calculated
     * field.
     *
     * @param student SisStudent
     * @param errors
     */
    private void updateStudentNightlyFields(SisStudent student, List<ValidationError> errors) {
        String studentPin = (String) student.getFieldValueByAlias(STUDENT_PIN);
        setNightlyField(student, studentPin, NIGHTLY_STUDENT_PIN);

        SisPerson person = student.getPerson();
        setNightlyField(person, person.getPersonId(), NIGHTLY_SSN);
        setNightlyField(person, person.getFirstName(), NIGHTLY_FIRST_NAME);
        setNightlyField(person, person.getLastName(), NIGHTLY_LAST_NAME);
        setNightlyField(person, person.getMiddleName(), NIGHTLY_MIDDLE_NAME);
        if (student.isDirty()) {
            errors.addAll(m_broker.saveBean(student));
        }
        if (person.isDirty()) {
            errors.addAll(m_broker.saveBean(person));
        }
    }

}
