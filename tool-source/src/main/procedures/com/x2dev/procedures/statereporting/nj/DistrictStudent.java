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


import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.CoreDataType;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for DistrictStudent export.
 *
 * @author X2 Development Corporation
 */

public class DistrictStudent extends StateReportData {
    /**
     * Entity class for District Student Data Submission export.
     *
     * @author X2 Development Corporation
     */

    public static class DistrictStudentEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        Map<String, String> countyDistrictSchoolCodeMap = null;
        List<StudentEnrollmentSpan> m_enrollmentSpans = new ArrayList<StudentEnrollmentSpan>();
        Collection<StudentProgramParticipation> m_programs;
        SisStudent m_student;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public DistrictStudentEntity() {
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
            m_student = (SisStudent) bean;
            m_programs = m_student.getProgramParticipation();
            DistrictStudent dsData = (DistrictStudent) data;
            String sameDistrictCodes = null;

            if (dsData.m_sameDistrictCodes != null) {
                School school = m_student.getSchool();
                sameDistrictCodes = (String) school.getFieldValueByBeanPath(dsData.m_sameDistrictCodes);
            }
            boolean activeOnReportDate = false;

            // Selecting only the enrollment spans that are after school's start date and those that
            // belong
            // to the related school districts only
            List<StudentEnrollmentSpan> spans =
                    dsData.getStudentHistoryHelper().getStudentEnrollmentSpans(m_student, false);
            for (StudentEnrollmentSpan span : spans) {
                PlainDate spanStart = span.getFirstActiveDate();
                PlainDate spanEnd = span.getLastActiveDate();
                StudentEnrollment activeEnrollment = span.getFirstActiveEnrollment();

                if (activeEnrollment != null && dsData.includeSchool(activeEnrollment.getSchoolOid())) {
                    // This looks for the enrollment span that overlaps the report date and sets a
                    // flag for whether the student was Active or Active No Primary
                    // on the report date. If student was any other status (such as PreReg), the
                    // flag is not set and the student will not export.
                    if (spanStart != null &&
                            (spanStart.equals(dsData.m_reportDate) || spanStart.before(dsData.m_reportDate))
                            && (spanEnd == null || spanEnd.equals(dsData.m_reportDate)
                                    || spanEnd.after(dsData.m_reportDate))) {
                        String enrollmentStatusForSpan = activeEnrollment.getStatusCode();
                        if (dsData.m_studentActiveStatus.equals(enrollmentStatusForSpan)
                                || dsData.m_studentActiveNoPrimaryStatus.equals(enrollmentStatusForSpan)) {
                            activeOnReportDate = true;
                        }
                    }

                    if (sameDistrictCodes != null) {
                        String countyDistrictSchoolCode =
                                (String) activeEnrollment.getFieldValueByBeanPath(dsData.m_fieldResidingSchool);
                        countyDistrictSchoolCode = dsData.lookupReferenceCodeByBeanPath(StudentEnrollment.class,
                                dsData.m_fieldResidingSchool,
                                countyDistrictSchoolCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        countyDistrictSchoolCodeMap = dsData.getCountyDistrictSchoolCode(countyDistrictSchoolCode);
                        if (countyDistrictSchoolCodeMap != null
                                && countyDistrictSchoolCodeMap.containsKey(KEY_DISTRICT_CODE)) {
                            String residingDistrictCode = countyDistrictSchoolCodeMap.get(KEY_DISTRICT_CODE);
                            if (residingDistrictCode == null || !sameDistrictCodes.contains(residingDistrictCode)) {
                                continue;
                            }
                        }
                    }
                    if ((dsData.getStartDate() != null && span.getFirstActiveDate() != null
                            && !span.getFirstActiveDate().before(dsData.getStartDate()))
                            || span.getLastActiveDate() == null
                            || (dsData.getStartDate() != null
                                    && span.getLastActiveDate().after(dsData.getStartDate()))) {
                        m_enrollmentSpans.add(span);
                    }
                }
            }
            if (m_enrollmentSpans.size() == 0 || !activeOnReportDate) {
                setRowCount(0);
            }
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
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public SisStudent getStudent() {
            return m_student;
        }

        /**
         * Returns the list of programs in which the student is participating.
         *
         * @return Collection
         */
        public Collection<StudentProgramParticipation> getPrograms() {
            return m_programs;
        }

        /**
         * Returns the list of enrollment spans for a student.
         *
         * @return List
         */
        public List<StudentEnrollmentSpan> getEnrollmentSpans() {
            return m_enrollmentSpans;
        }
    }

    /**
     * Retrieves the student related information like tuition code,
     * sped classification and lep start/end date.
     *
     */
    protected class RetrieveStudent implements FieldRetriever {
        private static final String ALIAS_DOE_BIRTH_COUNTRY = "DOE SID BIRTH COUNTRY";
        private static final String ALIAS_FIRST_ENTRY_DATE_IN_US = "DOE FIRST ENT DATE US SC";
        private static final String ALIAS_HEALTH_INSURANCE_PROVIDER = "DOE HEALTH INS PROVIDER";
        private static final String ALIAS_HEALTH_INSURANCE_STATUS = "DOE HEALTH INS STATUS";
        private static final String ALIAS_TUITION_CODE = "DOE TUITION CODE";

        private static final String PARAM_EIGHTH_TECH_LIT_CODE = "EIGHTH-TECH";
        private static final String PARAM_HEALTH_INS_PROVIDER = "HEALTH-INS-PROVIDER";
        private static final String PARAM_HEALTH_INS_STATUS = "HEALTH-INS-STATUS";
        private static final String PARAM_IMMIGRANT_STATUS = "IMMIGRANT";


        private final SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyyMMdd");

        private String m_birthCountry;
        private String m_firstEntryDateInUS;
        private String m_healthInsuranceProvider;
        private String m_healthInsuranceStatus;
        private DataDictionaryField m_healthInsuranceStatusField;
        private Date m_threeYrsPriorToReportDate;
        private String m_tuitionCode;

        public RetrieveStudent() {
            super();

            Calendar calendar = Calendar.getInstance();
            calendar.setTime(m_reportDate);
            calendar.add(Calendar.YEAR, -3);
            m_threeYrsPriorToReportDate = calendar.getTime();

            m_birthCountry = translateAliasToJavaName(ALIAS_DOE_BIRTH_COUNTRY, true);
            m_firstEntryDateInUS = translateAliasToJavaName(ALIAS_FIRST_ENTRY_DATE_IN_US, true);
            m_healthInsuranceProvider = translateAliasToJavaName(ALIAS_HEALTH_INSURANCE_PROVIDER, true);
            m_healthInsuranceStatus = translateAliasToJavaName(ALIAS_HEALTH_INSURANCE_STATUS, true);
            m_tuitionCode = translateAliasToJavaName(ALIAS_TUITION_CODE, true);

            if (!StringUtils.isEmpty(m_healthInsuranceStatus)) {
                ModelProperty prop =
                        new ModelProperty(SisStudent.class, m_healthInsuranceStatus, getBroker().getPersistenceKey());
                m_healthInsuranceStatusField = getDataDictionary().findDataDictionaryField(prop.getFieldId());
            }

        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        // DS-STDINFO
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            Object value = null;
            DistrictStudentEntity distStdEntity = ((DistrictStudentEntity) entity);
            SisStudent student = distStdEntity.getStudent();
            if (PARAM_EIGHTH_TECH_LIT_CODE.equalsIgnoreCase(param)) {
                StudentEnrollment enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, "EWSY");
                int studentYog = enrollment.getYog();
                if (studentYog == 0) {
                    studentYog = student.getYog();
                }
                if (studentYog == m_eghthGradeYog) {
                    value = student.getFieldValueByBeanPath(m_eighthTech);
                }
            } else if (PARAM_TUITION_CODE.equalsIgnoreCase(param)) {
                StudentEnrollment enrollment =
                        m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, StudentEnrollment.ENTRY);
                if (enrollment != null) {
                    String tuitionCode = (String) enrollment.getFieldValueByBeanPath(m_tuitionCode);
                    value = lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_tuitionCode, tuitionCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (PARAM_SPED_CLASSIFICATION.equalsIgnoreCase(param)) {
                // get active iep, if there is one.
                IepData currentIep = null;
                if (m_activeIeps.containsKey(student.getOid())) {
                    currentIep = m_activeIeps.get(student.getOid());
                }

                if (currentIep != null) {
                    // get disability code if there is one.
                    IepDisability disability = currentIep.getPrimaryDisability();
                    if (disability != null) {
                        value = disability.getDisabilityCode();
                        value = lookupReferenceCodeByBeanPath(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                                (String) value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                }
            } else if (PARAM_LEP_START_DATE.equalsIgnoreCase(param)) {
                Collection<StudentProgramParticipation> programs = distStdEntity.getPrograms();
                for (StudentProgramParticipation program : programs) {
                    String programCode = program.getProgramCode();
                    PlainDate startDate = program.getStartDate();
                    if (PROGRAM_CODE_LEP.equalsIgnoreCase(programCode) && startDate != null) {
                        value = dateFormatter.format(startDate);
                        break;
                    }
                }
            } else if (PARAM_LEP_END_DATE.equalsIgnoreCase(param)) {
                Collection<StudentProgramParticipation> programs = distStdEntity.getPrograms();
                for (StudentProgramParticipation program : programs) {
                    String programCode = program.getProgramCode();
                    PlainDate endDate = program.getEndDate();
                    if (PROGRAM_CODE_LEP.equalsIgnoreCase(programCode) && endDate != null) {
                        value = dateFormatter.format(endDate);
                        break;
                    }
                }
            } else if (PARAM_IMMIGRANT_STATUS.equalsIgnoreCase(param)) {
                SisPerson person = student.getPerson();
                if (person != null) {
                    String birthCountry = (String) person.getFieldValueByBeanPath(m_birthCountry);
                    birthCountry = lookupReferenceCodeByBeanPath(SisPerson.class, m_birthCountry, birthCountry,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (birthCountry != null && !BIRTH_COUNTRY_CODE_PUERTO_RICO.equals(birthCountry) &&
                            !BIRTH_COUNTRY_CODE_USA.equals(birthCountry)) {
                        value = IMMIGRANT_STATUS_NO;
                        if (m_threeYrsPriorToReportDate != null) {
                            PlainDate firstEntryDateInUS =
                                    (PlainDate) data.getPropertyAsJavaType(student, m_firstEntryDateInUS);
                            if (firstEntryDateInUS != null && m_threeYrsPriorToReportDate != null
                                    && firstEntryDateInUS.after(m_threeYrsPriorToReportDate)) {
                                value = IMMIGRANT_STATUS_YES;
                            }
                        }
                    }
                }
            } else if (PARAM_HEALTH_INS_PROVIDER.equalsIgnoreCase(param)) {
                SisPerson person = student.getPerson();
                if (person != null) {
                    String healthInsuranceProvider =
                            (String) student.getFieldValueByBeanPath(m_healthInsuranceProvider);
                    if (healthInsuranceProvider != null) {
                        value = healthInsuranceProvider.replaceAll(REGEX_HEALTH_INSURANCE_EXCLUDED_CHARACTERS,
                                SPACE_CHARACTER);
                    }
                }
            } else if (PARAM_HEALTH_INS_STATUS.equalsIgnoreCase(param)) {
                SisPerson person = student.getPerson();
                if (person != null && m_healthInsuranceStatusField != null) {
                    if (m_healthInsuranceStatusField.getType() == CoreDataType.LOGICAL) {
                        Boolean logicalValue = (Boolean) data.getPropertyAsJavaType(student, m_healthInsuranceStatus);
                        value = logicalValue == null || !logicalValue.booleanValue() ? "N" : "Y";
                    } else {
                        String textValue = (String) student.getFieldValueByBeanPath(m_healthInsuranceStatus);
                        if (!StringUtils.isEmpty(textValue) && m_healthInsuranceStatusField.hasReferenceTable()) {
                            value = data.lookupStateValue(SisStudent.class, m_healthInsuranceStatus, textValue);
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * validate "Eighth Tech Literacy" field
     * rule:
     * If student Grade level = 08 and field is blank/null - appear validation error
     *
     * @author Follett Software Company
     */
    public class ValidateEighthTL implements FieldValidator {
        private static final String ENROLLMENT_STATUSES =
                StudentEnrollment.ENTRY + StudentEnrollment.WITHDRAWAL + StudentEnrollment.STATUS_CHANGE +
                        StudentEnrollment.YOG_CHANGE;
        private static final String ERROR_MESSAGE = "Eighth Technological Literacy required for student in Grade 08.";
        private static final String VAL_ID = "validateEighthTL";

        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            DistrictStudentEntity distStdEntity = ((DistrictStudentEntity) entity);
            SisStudent student = distStdEntity.getStudent();
            StudentEnrollment enrollment =
                    m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, ENROLLMENT_STATUSES);
            int studentYog = enrollment.getYog();
            if (studentYog == 0) {
                studentYog = student.getYog();
            }
            if (studentYog == m_eghthGradeYog && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(distStdEntity, field, REQUIRED_VALUE, ERROR_MESSAGE));
            }

            return errors;
        }
    }

    /**
     * validate "First Ent Date US Sc" field
     * rule:
     * If Immigrant Status = Y and DOE FIRST ENT DATE US SC is blank/null - appear validation error
     *
     * @author Follett Software Company
     */
    public class ValidateFirstEDUS implements FieldValidator {
        private static final String ERROR_MESSAGE = "First Entry Date Into US School required if Immigrant = Y.";
        private static final String FIELD_NAME_IMIGRANT_STATUS = "Immigrant Status";
        private static final String VAL_ID = "validateFirstEDUS";

        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            DistrictStudentEntity distStdEntity = ((DistrictStudentEntity) entity);
            String status = distStdEntity.getFieldValue(FIELD_NAME_IMIGRANT_STATUS);
            if (status != null && status.equals(VALUE_Y) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(distStdEntity, field, REQUIRED_VALUE, ERROR_MESSAGE));
            }

            return errors;
        }
    }

    /**
     * validate "Health Ins Provider" field
     * rule:
     * If DOE HEALTH INS STATUS = Y and Health Insurance Provider r is blank/null - appear
     * validation error
     *
     * @author Follett Software Company
     */
    public class ValidateHealthIP implements FieldValidator {
        private static final String ERROR_MESSAGE = "Health Insurance required when Health Insurance Status = Y.";
        private static final String FIELD_NAME_HEALTH_INS_STATUS = "Health Ins Status";
        private static final String VAL_ID = "validateHealthIP";

        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            DistrictStudentEntity distStdEntity = ((DistrictStudentEntity) entity);
            String status = distStdEntity.getFieldValue(FIELD_NAME_HEALTH_INS_STATUS);
            if (status != null && status.equals(VALUE_Y) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(distStdEntity, field, REQUIRED_VALUE, ERROR_MESSAGE));
            }

            return errors;
        }
    }


    /**
     * validate "Homeless Nighttime" field
     * rule:
     * If Homeless = Y1 or Y2 and Homeless Primary Nightime Residence is blank/null - appear
     * validation error
     *
     * @author Follett Software Company
     */
    public class ValidateHomelessN implements FieldValidator {
        private static final String ERROR_MESSAGE = "Homeless Nighttime required if Homeless = 'Y1' or 'Y2'.";
        private static final String FIELD_NAME_HOMELESS = "Homeless";
        private static final String VAL_ID = "validateHomelessN";
        private List<String> m_homelessValues = Arrays.asList("Y1", "Y2");

        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new LinkedList<StateReportValidationError>();

            DistrictStudentEntity distStdEntity = ((DistrictStudentEntity) entity);
            String homelessValue = distStdEntity.getFieldValue(FIELD_NAME_HOMELESS);
            if (homelessValue != null && m_homelessValues.contains(homelessValue) && StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(distStdEntity, field, REQUIRED_VALUE, ERROR_MESSAGE));
            }

            return errors;
        }
    }


    /**
     * Aliases
     */
    protected static final String ALIAS_DOE_EIGHTH = "DOE EIGHTH TECH LIT";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_RESIDENT_SCHOOL = "DOE RESIDING SCHOOL";
    protected static final String ALIAS_SAME_DISTRICT_CODES_LIST = "RELATED RESIDENT DIST CODES";

    /**
     * Parameters
     */
    protected static final String PARAM_DAYS_MEMBERSHIP = "MEMBERSHIP";
    protected static final String PARAM_DAYS_PRESENT = "PRESENT";
    protected static final String PARAM_DAYS_TRUANT = "TRUANT";
    protected static final String PARAM_LEP_END_DATE = "LEP-END";
    protected static final String PARAM_LEP_START_DATE = "LEP-START";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SPED_CLASSIFICATION = "CLASSIFICATION";
    protected static final String PARAM_TUITION_CODE = "TUITION";

    /**
     * Other Constants
     */
    protected static final String ABSENT_REASON_EXCUSED_ABSENCE_STATE_CODE = "3";
    protected static final String ABSENT_REASON_HOME_INSTRUCTION_STATE_CODE = "7";
    protected static final String ABSENT_REASON_TRUANT_STATE_CODE = "TT";
    protected static final String BIRTH_COUNTRY_CODE_AMERICAN_SAMOA = "0050";
    protected static final String BIRTH_COUNTRY_CODE_GUAM = "0930";
    protected static final String BIRTH_COUNTRY_CODE_MARSHALL_ISLANDS = "1390";
    protected static final String BIRTH_COUNTRY_CODE_FEDERATED_STATES_OF_INDONESIA = "1450";
    protected static final String BIRTH_COUNTRY_CODE_NORTHERN_MARIANA_ISLANDS = "1650";
    protected static final String BIRTH_COUNTRY_CODE_PALAU = "1690";
    protected static final String BIRTH_COUNTRY_CODE_PUERTO_RICO = "1790";
    protected static final String BIRTH_COUNTRY_CODE_USA = "2330";
    protected static final String BIRTH_COUNTRY_CODE_USA_MINOR_OUTLYING_ISLANDS = "2340";
    protected static final String BIRTH_COUNTRY_CODE_VIRGIN_ISLANDS = "2410";
    protected static final String IMMIGRANT_STATUS_NO = "N";
    protected static final String IMMIGRANT_STATUS_YES = "Y";
    protected static final String KEY_COUNTY_CODE = "countyCode";
    protected static final String KEY_DISTRICT_CODE = "districtCode";
    protected static final String KEY_SCHOOL_CODE = "schoolCode";
    protected static final String PROGRAM_CODE_LEP = "LEP";
    protected static final String REGEX_HEALTH_INSURANCE_EXCLUDED_CHARACTERS = "[-_,//\\\\&()#\\.]";
    protected static final String REQUIRED_VALUE = "Required value";
    protected static final String SPACE_CHARACTER = " ";
    protected static final String VALUE_Y = "Y";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, IepData> m_activeIeps;
    protected Map<String, RefAttendanceStudent> m_portionAbsentByCode;
    protected int m_eghthGradeYog;
    protected String m_eighthTech;
    protected String m_fieldResidingSchool;
    protected StudentHistoryHelper m_helper;
    protected String m_excludeSchool;
    protected Map m_excludeSchoolMap;
    protected PlainDate m_reportDate;
    protected String m_sameDistrictCodes;
    protected PlainDate m_startDate;
    protected String m_tuitionCodeRefTblOid;
    protected String m_studentActiveStatus;
    protected String m_studentActiveNoPrimaryStatus;

    /**
     * Checks if schoolOid given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
    }

    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();
        loadSchoolExcludeMap();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        /*
         * Initialize eighth grade level yog
         */
        int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
        int currentSchoolYear = getCurrentContext().getSchoolYear();
        m_eghthGradeYog = currentSchoolYear + (maxGradeLevel - 8);

        populateActiveIeps(m_helper.getStudentCriteria());

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(DistrictStudentEntity.class);

            // Build a map of calculations/retrievers

            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("DS-STDINFO", new RetrieveStudent());


            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(ValidateEighthTL.VAL_ID, new ValidateEighthTL());
            validators.put(ValidateFirstEDUS.VAL_ID, new ValidateFirstEDUS());
            validators.put(ValidateHealthIP.VAL_ID, new ValidateHealthIP());
            validators.put(ValidateHomelessN.VAL_ID, new ValidateHomelessN());
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * This method returns the start date.
     *
     * @return Plain date
     */
    protected PlainDate getStartDate() {
        return m_startDate;
    }

    /**
     * This method returns the StudentHistoryHelper.
     *
     * @return Student history helper
     */
    protected StudentHistoryHelper getStudentHistoryHelper() {
        return m_helper;
    }

    /**
     * This method returns the county, district, school code by splitting the
     * countyDistrictSchoolCode code.
     *
     * @param countyDistrictSchoolCode String
     * @return Map
     */
    protected Map<String, String> getCountyDistrictSchoolCode(String countyDistrictSchoolCode) {
        Map<String, String> codeMap = null;
        if (!StringUtils.isEmpty(countyDistrictSchoolCode) && countyDistrictSchoolCode.length() > 6)// check
                                                                                                    // for
                                                                                                    // length
        {
            codeMap = new HashMap<String, String>();
            codeMap.put(KEY_COUNTY_CODE, countyDistrictSchoolCode.substring(0, 2));
            codeMap.put(KEY_DISTRICT_CODE, countyDistrictSchoolCode.substring(2, 6));
            codeMap.put(KEY_SCHOOL_CODE, countyDistrictSchoolCode.substring(6));// can be 3-digit or
                                                                                // 4-digit
            // If it is 4-digit, then it is actually municipal code
            // and not school code.
        }
        return codeMap;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        getRefAttendanceCodeByAttendanceCode();

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        m_startDate = getCurrentContext().getStartDate();
        m_fieldResidingSchool = translateAliasToJavaName(ALIAS_RESIDENT_SCHOOL, true);
        m_sameDistrictCodes = translateAliasToJavaName(ALIAS_SAME_DISTRICT_CODES_LIST, false);
        m_studentActiveStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_studentActiveNoPrimaryStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE_NO_PRIMARY);
        m_eighthTech = translateAliasToJavaName(ALIAS_DOE_EIGHTH, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }

    /**
     * Get IEPS that were active as of report date.
     *
     * @param studentCriteria X2Criteria
     */
    private void populateActiveIeps(X2Criteria studentCriteria) {
        SubQuery activeStudentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Preload the Active IEPs for students
        Criteria activeIepCriteria = new Criteria();
        Criteria iepExitedBeforeReportDate = new Criteria();
        Criteria currentlyActiveIepCriteria = new Criteria();

        // Consider any currently active IEPs
        currentlyActiveIepCriteria.addEqualTo(IepData.COL_STATUS_CODE,
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));
        // Consider any IEPs exited before or on report date
        iepExitedBeforeReportDate.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(2));
        iepExitedBeforeReportDate.addGreaterOrEqualThan(IepData.COL_EXIT_DATE, m_reportDate);
        currentlyActiveIepCriteria.addOrCriteria(iepExitedBeforeReportDate);

        // Order by status first, to have active appear before exited,
        // and order descending by date second to get most recent exited iep.
        activeIepCriteria.addAndCriteria(currentlyActiveIepCriteria);
        activeIepCriteria.addIn(IepData.COL_STUDENT_OID, activeStudentSubQuery);
        QueryByCriteria activeIepQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        activeIepQuery.addOrderByAscending(IepData.COL_STATUS_CODE);
        activeIepQuery.addOrderByDescending(IepData.COL_EXIT_DATE);

        m_activeIeps = getBroker().getMapByQuery(activeIepQuery, IepData.COL_STUDENT_OID, 128);
    }

    /**
     * This populates a map of reference attendance codes by attendance code.
     *
     * @return void
     */
    private void getRefAttendanceCodeByAttendanceCode() {
        m_portionAbsentByCode = new HashMap<String, RefAttendanceStudent>();

        Criteria refTableCriteria = new Criteria();
        refTableCriteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE, "0");

        QueryByCriteria query = new QueryByCriteria(RefAttendanceStudent.class, refTableCriteria);
        m_portionAbsentByCode = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 10);
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }
}
