
/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SchoolCapacity;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.SisUser;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentTransportation;
import com.x2dev.sis.model.business.StudentEnrollmentProcedure;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Membership procedure for Tennessee, which contains Ed-Fi integration logic for state IDs and
 * students during registration.
 *
 * @author mmastrangelo
 */
public class TennesseeStudentEnrollmentProcedure implements StudentEnrollmentProcedure {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String EDFI_DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss";
    private static final String EDFI_IDENTITY_ZONE = "TN-IDENTITY";

    private static final String DOE_SCHOOL_WIDE = "DOE SCHOOL WIDE";
    private static final String ALIAS_EXCLUDE_FROM_AUTO = "Exclude from auto";

    private X2Broker m_broker = null;
    private DateConverter m_dateConverter = null;
    private SimpleDateFormat m_edfiDateFormat = null;
    private Organization m_organization = null;
    private WebTarget m_webTarget = null;

    private HashMap<String, StudentProgramParticipation> m_existingStudentPrograms;
    private HashMap<String, Boolean> m_excludeFromAuto;

    /**
     * Constructs a new TennesseeStudentEnrollmentProcedure.
     */
    public TennesseeStudentEnrollmentProcedure() {
        m_edfiDateFormat = new SimpleDateFormat(EDFI_DATE_FORMAT);
        m_dateConverter = (DateConverter) ConverterFactory.getConverterForClass(Converter.DATE_CONVERTER);

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
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#initialize(com.follett.fsc.core.k12.
     *      business.X2Broker,
     *      com.follett.fsc.core.k12.business.PrivilegeSet)
     */
    @Override
    public void initialize(X2Broker broker, PrivilegeSet privilegeSet) {
        m_broker = broker;
        Collection<Organization> organizations = privilegeSet.getOrganizations();
        if (organizations != null && !organizations.isEmpty()) {
            m_organization = organizations.iterator().next();
        }

        loadProgramExcludeFlags();
    }

    /**
     * After change enrollment status.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterChangeEnrollmentStatus(com.x2dev
     *      .sis.model.beans.SisStudent,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterChangeEnrollmentStatus(SisStudent student,
                                                             StudentEnrollment enrollment) {
        /* Not used */
        return new ArrayList<ValidationError>(0);
    }

    /**
     * After change year of graduation.
     *
     * @param student SisStudent
     * @param enrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterChangeYearOfGraduation(com.x2dev
     *      .sis.model.beans.SisStudent,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterChangeYearOfGraduation(SisStudent student,
                                                             StudentEnrollment enrollment) {
        /* Not used */
        return new ArrayList<ValidationError>(0);
    }

    /**
     * Creates or updates an Ed-Fi student. If a student exists in Ed-Fi with the unique ID
     * (state ID), that student is updated with values entered during registration. If a student
     * does not exist with the unique ID, a new student is created in Ed-Fi.
     *
     * @param student SisStudent
     * @param entryEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterRegistraterStudent(com.x2dev.sis
     *      .model.beans.SisStudent,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterRegistraterStudent(SisStudent student, StudentEnrollment entryEnrollment) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        createStudentPrograms(student, entryEnrollment, errors);

        return errors;
    }

    /**
     * After transfer student.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @param entryEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterTransferStudent(com.x2dev.sis.
     *      model.beans.SisStudent,
     *      com.x2dev.sis.model.beans.StudentEnrollment,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterTransferStudent(SisStudent student,
                                                      StudentEnrollment withdrawEnrollment,
                                                      StudentEnrollment entryEnrollment) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        new HashSet<String>();

        SisSchool entrySchool = entryEnrollment.getSchool();

        m_existingStudentPrograms = getPrograms(student);

        for (StudentProgramParticipation withdrawalProgram : m_existingStudentPrograms.values()) {
            boolean entrySchoolwide;
            String programCode = withdrawalProgram.getProgramCode();

            if (programCode != null) {
                // Determines if the new school identifies the program as schoolwide
                entrySchoolwide = isSchoolWide(entrySchool, programCode);

                // Retrieves the program record in the new school corresponding with the program in
                // the withrawal school
                // entryProgram = m_existingStudentPrograms.get(programCode);

                if (!entrySchoolwide) {
                    // Close program records that aren't schoolwide at the new school
                    closeStudentProgram(student, programCode, withdrawEnrollment, errors);
                }
            }
        }

        // Close transportation records at previous school
        closeAllTransportation(student, withdrawEnrollment, errors);

        // Creates all schoolwide programs at the new school that don't already exist
        createStudentPrograms(student, entryEnrollment, errors);

        // Copy UDFs from previous enrollment record to the new enrollment record
        copyEnrollmentUDFs(withdrawEnrollment, entryEnrollment);

        return errors;
    }

    /**
     * After withdraw student.
     *
     * @param student SisStudent
     * @param withdrawEnrollment StudentEnrollment
     * @return List
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#afterWithdrawStudent(com.x2dev.sis.
     *      model.beans.SisStudent,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> afterWithdrawStudent(SisStudent student,
                                                      StudentEnrollment withdrawEnrollment) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        closeAllStudentPrograms(student, withdrawEnrollment, errors);

        closeAllTransportation(student, withdrawEnrollment, errors);

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
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateChangeEnrollmentStatus(com.
     *      x2dev.sis.model.beans.SisStudent,
     *      com.x2dev.utils.types.PlainDate, java.lang.String, java.lang.String,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateChangeEnrollmentStatus(SisStudent student,
                                                                PlainDate date,
                                                                String reason,
                                                                String status,
                                                                StudentEnrollment studentEnrollment) {
        /* Not used */
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
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateChangeYearOfGraduation(com.
     *      x2dev.sis.model.beans.SisStudent,
     *      com.x2dev.utils.types.PlainDate, java.lang.String, int, java.lang.String,
     *      com.x2dev.sis.model.beans.StudentEnrollment)
     */
    @Override
    public List<ValidationError> validateChangeYearOfGraduation(SisStudent student,
                                                                PlainDate date,
                                                                String reason,
                                                                int yog,
                                                                String gradeLevel,
                                                                StudentEnrollment studentEnrollment) {
        /* Not used */
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
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateRegisterStudent(com.x2dev.sis
     *      .model.beans.SisStudent,
     *      int, java.lang.String, com.x2dev.sis.model.beans.SisSchool,
     *      com.follett.fsc.core.k12.beans.SchoolCapacity, java.util.Map)
     */
    @Override
    public List<ValidationError> validateRegisterStudent(SisStudent student,
                                                         int yog,
                                                         String status,
                                                         SisSchool school,
                                                         SchoolCapacity schoolCapacity,
                                                         Map<ModelProperty, Object> enrollmentPropertyValues) {
        /* Not used */
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
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateTransferStudent(com.x2dev.sis
     *      .model.beans.SisStudent,
     *      com.x2dev.sis.model.beans.SisUser, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      java.lang.String, com.x2dev.utils.types.PlainDate,
     *      java.lang.String, java.lang.String, com.x2dev.sis.model.beans.SisSchool,
     *      com.follett.fsc.core.k12.beans.SchoolCapacity,
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
        /* Not used */
        return new ArrayList<ValidationError>(0);
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
     * @see
     *      com.x2dev.sis.model.business.StudentEnrollmentProcedure#validateWithdrawStudent(com.x2dev.sis
     *      .model.beans.SisStudent,
     *      com.follett.fsc.core.k12.beans.User, com.x2dev.utils.types.PlainDate, java.lang.String,
     *      java.lang.String, java.lang.String,
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
        /* Not used */
        return new ArrayList<ValidationError>(0);
    }

    // --------------------------------------------------------------------------------------------
    // Shared helper methods
    // --------------------------------------------------------------------------------------------

    /**
     * Converts the passed Ed-Fi date string (in the EDFI_DATE_FORMAT format) to an Aspen date
     * string, as defined by the DateConverter.
     *
     * @param edfiDateString String
     * @return String
     */
    private String getAspenDateString(String edfiDateString) {
        String aspenDateString = "";
        if (!StringUtils.isEmpty(edfiDateString)) {
            try {
                PlainDate date = new PlainDate(m_edfiDateFormat.parse(edfiDateString));
                aspenDateString = m_dateConverter.javaToString(date);
            } catch (ParseException pe) {
                AppGlobals.getLog().log(Level.SEVERE, "Unable to parse EdFi date String", pe);
            }
        }

        return aspenDateString;
    }

    /**
     * Converts the passed Aspen date string, as defined by the DateConverter, to an Ed-Fi date
     * string (in the EDFI_DATE_FORMAT format).
     *
     * @param aspenDateString String
     * @return String
     */
    private String getEdFiDateString(String aspenDateString) {
        String edfiDateString = "";
        if (!StringUtils.isEmpty(aspenDateString)) {
            PlainDate date = m_dateConverter.stringToJava(aspenDateString);
            edfiDateString = m_edfiDateFormat.format(date);
        }

        return edfiDateString;
    }

    /**
     * Validates the passed Ed-Fi web service response. True is returned if the response's HTTP
     * status is less than 400, unless an array of valid status codes is provided, in which case,
     * the HTTP status must be among the provided array of codes.
     * <p>
     * A validation error is added to the passed list of errors and log message is generated if
     * validation does not succeed.
     *
     * @param response Response
     * @param logMessagePrefix String
     * @param errors optional; to be provided if ValidationErrors should be collected
     * @param validCodes int[]
     * @return true if validation succeeds
     */
    private boolean validateResponse(Response response,
                                     String logMessagePrefix,
                                     List<ValidationError> errors,
                                     int... validCodes) {
        boolean valid = true;
        if (validCodes.length == 0) {
            valid = response.getStatus() < 400;
        } else {
            for (int validCode : validCodes) {
                valid = response.getStatus() == validCode;
                if (valid) {
                    break;
                }
            }
        }

        if (!valid) {
            if (errors != null) {
                errors.add(new ValidationError(ValidationConstants.SYSTEM_ERROR, null,
                        "An Ed-Fi Web Service error occurred. (HTTP Status "
                                + response.getStatus() + ")"));
            }

            String logDetails = logMessagePrefix + "; HTTP Status Code " + response.getStatus() + " "
                    + response.getStatusInfo().toString();
            AppGlobals.getLog().log(Level.SEVERE, logDetails);
        }

        return valid;
    }

    /*
     * --------------------------------------------------------------------
     * TN registration behavior
     * --------------------------------------------------------------------
     */
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
     * Returns a list of school wide programs.
     *
     * @param school SisSchool
     * @return ArrayList&lt;String&gt;
     */
    private ArrayList<String> getSchoolwidePrograms(SisSchool school) {
        ArrayList<String> programs = null;

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

        if (schoolwidePrograms != null) {
            if (schoolwidePrograms.contains(programCode)) {
                isSchoolWide = true;
            }
        }

        return isSchoolWide;
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
     * Loads the school wide programs marking which program are / aren't to be excluded from auto
     * create / closing
     * by this procedure.
     */
    private void loadProgramExcludeFlags() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(m_broker.getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                StudentProgramParticipation.COL_PROGRAM_CODE);
        ReferenceTable refTable = field.getReferenceTable();
        dictionary = DataDictionary.getDistrictDictionary(refTable.getExtendedDataDictionary(),
                m_broker.getPersistenceKey());

        Criteria criteria = new Criteria();

        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                ReferenceCode code = (ReferenceCode) iterator.next();

                String programCode = code.getCode();

                String exclude = (String) code.getFieldValueByAlias(ALIAS_EXCLUDE_FROM_AUTO, dictionary);

                boolean excludeFromAuto = "1".equals(exclude);

                m_excludeFromAuto.put(programCode, Boolean.valueOf(excludeFromAuto));
            }
        } finally {
            iterator.close();
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
     * Closes the student's student program participation record.
     *
     * @param student SisStudent
     * @param programCode String
     * @param withdrawEnrollment StudentEnrollment
     * @param errors List<ValidationError>
     */
    private void closeStudentProgram(SisStudent student,
                                     String programCode,
                                     StudentEnrollment withdrawEnrollment,
                                     List<ValidationError> errors) {
        boolean excludeFromAuto = m_excludeFromAuto.get(programCode).booleanValue();

        if (!excludeFromAuto) {
            X2Criteria criteria = new X2Criteria();

            PlainDate date = withdrawEnrollment.getEnrollmentDate();

            criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());
            criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, programCode);

            QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

            StudentProgramParticipation program = (StudentProgramParticipation) m_broker.getBeanByQuery(query);

            if (program != null) {
                program.setEndDate(date);
            }

            m_broker.saveBeanForced(program);
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
        X2Criteria criteria = new X2Criteria();

        SisSchool school = withdrawEnrollment.getSchool();
        PlainDate withdrawalDate = withdrawEnrollment.getEnrollmentDate();

        criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(StudentProgramParticipation.COL_STUDENT_OID, student.getOid());

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        QueryIterator iterator = m_broker.getIteratorByQuery(query);

        try {
            while (iterator.hasNext()) {
                StudentProgramParticipation program = (StudentProgramParticipation) iterator.next();

                program.setEndDate(withdrawalDate);

                m_broker.saveBeanForced(program);
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
        X2Criteria criteria = new X2Criteria();

        SisSchool school = withdrawEnrollment.getSchool();
        PlainDate withdrawalDate = withdrawEnrollment.getEnrollmentDate();

        criteria.addEqualTo(StudentTransportation.COL_SCHOOL_OID, school.getOid());
        criteria.addEqualTo(StudentTransportation.COL_STUDENT_OID, student.getOid());
        criteria.addIsNull(StudentTransportation.COL_END_DATE);

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
}
