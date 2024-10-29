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
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEdPlan;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Illinois state export procedure for ELL (English Language Learner)
 *
 * Searches the student program participation table for program code's that
 * matches "ELL" and is within the district's current context year.
 *
 * @author X2 Development Corporation
 */
public class ELL extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL. This must be a
     * public static inner class with a public no argument constructor so it can
     * be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ELLEntity extends StateReportEntity {

        /**
         * ELL data.
         */
        ELL m_eData = null;

        /**
         * ENTRY enrollment for this entity.
         */
        StudentEnrollment m_enrollment = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ELLEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize and increment counter
         *
         * If there is no recent entry enrollment record before the report date, ignore it.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_eData = (ELL) data;
            StudentProgramParticipation pgm = (StudentProgramParticipation) bean;
            SisStudent student = pgm.getStudent();
            m_enrollment = m_eData.m_helper.getEnrollmentForDate(student.getOid(), m_eData.m_reportDate, "E");
            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_eData.m_excludeSklField))) {
                m_rcdtsMap = lookupOverrides();

                // keep count of records
                ((ELL) data).m_totalProgramCount++;
            } else {
                setRowCount(0);
            }
            if (pgm.getEndDate() != null) {
                String stateEndReason = m_eData.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                        m_eData.m_fieldEllEndReason,
                        (String) pgm.getFieldValueByBeanPath(m_eData.m_fieldEllEndReason),
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                if (StringUtils.isEmpty(stateEndReason)) {
                    setRowCount(0);
                }
            }
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_eData.m_fieldSchoolCode);
            }

            return school;
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StudentProgramParticipation spp = (StudentProgramParticipation) getBean();
            String name = spp.getStudent().getNameView() +
                    " [LASID: " + spp.getStudent().getLocalId() +
                    ", SASID: " + spp.getStudent().getStateId() + "]";

            return name;
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

        /**
         * Filter only records from students that are active by report date.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            StudentEnrollment recentEnrollment = getEffectiveEnrollment();
            if (recentEnrollment == null) {
                error = new StateReportValidationError(getEntityName(), "Enrollments", "No student enrollment records",
                        "");
            } else if (recentEnrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                error = new StateReportValidationError(getEntityName(), "Enrollment date", "Before report date", "");
            }
            return error;
        }

        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            ELL eData = (ELL) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (eData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = eData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode = eData.lookupStateValue(StudentEnrollment.class, eData.m_fieldServiceSchoolCode,
                            serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_504 = "DOE 504 ACCOM IND";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DOE_CLS_PERIODS_WEEK = "DOE CLS PERIODS WEEK";
    protected static final String ALIAS_EL_SERVICES_PROVIDED = "DOE EL SERVICES PROVIDED";
    protected static final String ALIAS_ELL_END_REASON = "DOE ELL END REASON";
    protected static final String ALIAS_ELL_IND = "DOE ELL IND";
    protected static final String ALIAS_ELL_SECTION = "DOE ELL SECTION";
    protected static final String ALIAS_ELL_STATUS = "DOE ELL STATUS";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    // TODO: The current client imlementation used [DOE NEW ARRIVAL] Cyril asked to change to [DOE
    // FIRST YEAR IN US] which exists on client dictionary but is not populated
    protected static final String ALIAS_FIRST_YEAR_IN_US = "DOE NEW ARRIVAL";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SCHOOL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_PERSON_BIRTH_COUNTRY = "DOE BIRTH COUNTRY";
    protected static final String ALIAS_PROGRAM_CODE = "DOE PROGRAM CODE";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_TRANS_BILINGUAL = "DOE TRANS BI LANG";

    /*
     * Static fields
     */
    protected static final String CODE_US = "US";

    /*
     * Parameters
     */
    protected static final String PARAM_QUERY_BY = "queryBy";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SORT = "sort";

    /*
     * Instance variables
     */
    protected Map<String, String> m_beanPathByAliasConsDDX;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_excludeStdField;
    protected String m_excludeSklField;
    protected String m_field504;
    protected String m_fieldBirthCountry;
    protected String m_fieldClsPeriodWeek;
    protected String m_fieldElServicesProvided;
    protected String m_fieldEllStatus;
    protected String m_fieldEllEndReason;
    protected String m_fieldEslInd;
    protected String m_fieldDateEntryUS;
    protected String m_fieldDistrictCode;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldEnrSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldTransBilingual;
    protected StudentHistoryHelper m_helper;
    protected Collection<String> m_newArrivalToUS;
    protected DataDictionary m_pgmDictionary;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<SchoolCalendarDate>> m_schoolCalendarDatesMap;
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();
    protected Map<String, Collection<StudentEdPlan>> m_studentActiveEdPlansMap = null;


    /**
     * A map of student's courses that are within the ELL department, for use in retrieve the ESL
     * field
     */
    Map<String, Collection<StudentSchedule>> m_ellCourseMap;

    /**
     * A map of student enrollments (Collection[StudentEnrollment]) by student oid
     *
     * Sorted in descending order. (First entry is recent, last entry is earliest)
     */
    Map<String, List<StudentEnrollment>> m_enrollmentMap;

    /**
     * Keep track of number of ELL programs
     */
    int m_totalProgramCount;

    /**
     * Retrieve the student's most earliest enrollment date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFirstEnroll implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            StudentProgramParticipation participation = (StudentProgramParticipation) entity.getBean();
            List<StudentEnrollment> enrollments = m_enrollmentMap.get(participation.getStudentOid());
            PlainDate firstDate = new PlainDate();
            if (enrollments != null) {
                for (int i = 1; i <= enrollments.size(); i++) {
                    StudentEnrollment earliest = enrollments.get(enrollments.size() - i);

                    if (!BooleanAsStringConverter.TRUE
                            .equals(earliest.getSchool().getFieldValueByBeanPath(m_excludeSklField))) {
                        firstDate = earliest.getEnrollmentDate();
                        break;
                    }
                }
            }
            return firstDate;
        }

    }

    /**
     * Retrieve value (state code) of Student Program Participation by alias. Alias can be in PGM
     * table or PGM-ELL exdended dictionary
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePgmValueByAlias implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String alias = (String) field.getParameter();
            String javaName = m_beanPathByAliasConsDDX.get(alias);
            String value = (String) entity.getBean().getFieldValueByBeanPath(javaName);
            if (m_pgmDictionary != null) {
                DataDictionaryField ddf =
                        m_pgmDictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(), javaName);
                if (ddf.hasReferenceTable() && !StringUtils.isEmpty(value)) {
                    String refTableOid = ddf.getReferenceTableOid();
                    value = lookupReferenceCodeByRefTbl(refTableOid, value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else {
                value = lookupReferenceCodeByAlias(alias, value,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }

            return value;
        }

    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRCDTS implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            ELLEntity eEntity = (ELLEntity) entity;
            String rcdts = null;
            if (param.equals("H") && eEntity.getEffectiveEnrollment() != null
                    && eEntity.getEffectiveEnrollment().getSchool() != null) {
                ELL eData = (ELL) data;
                String enrSchoolCode =
                        (String) eEntity.getEffectiveEnrollment().getFieldValueByBeanPath(eData.m_fieldEnrSchoolCode);

                if (!StringUtils.isEmpty(enrSchoolCode)) {
                    rcdts = data.lookupReferenceCodeByAlias(ALIAS_SCHOOL_HOME, enrSchoolCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else {
                    rcdts = (String) eEntity.getEffectiveEnrollment().getSchool()
                            .getFieldValueByBeanPath(eData.m_fieldSchoolCode);
                }
            }
            // Field #35 should be blank for this export.
            else if (param.equals("S")) {
                rcdts = eEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = eEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Start date of program. When date is < First Day of school, use First school date of current
     * calendar year.
     */
    protected class RetrieveDateEnrolled implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            ELLEntity ellEntity = (ELLEntity) entity;
            StudentProgramParticipation pgm = ((StudentProgramParticipation) entity.getBean());
            PlainDate startDate = pgm.getStartDate();

            PlainDate firstSchoolDate = getFirstSchoolDate(ellEntity.getEffectiveEnrollment().getSchoolOid());

            if (startDate.before(firstSchoolDate)) {
                startDate = firstSchoolDate;
            }
            return startDate;
        }
    }

    /**
     * "Yes" if student is new arrival to US, "No" otherwise.
     */
    protected class RetrieveDateEntryUS implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String studentOid = ((StudentProgramParticipation) entity.getBean()).getStudentOid();
            return m_newArrivalToUS.contains(studentOid) ? "01" : "02";
        }
    }

    /**
     * The student must be at least 3 years old on their first enrollment date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEnroll implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStudent student = ((StudentProgramParticipation) entity.getBean()).getStudent();
            SisPerson person = student.getPerson();

            try {
                PlainDate enrollDate = new PlainDate(m_dateFormat.parse(value));
                Date studentDob = m_dateFormat.parse(entity.getFieldValue("Birth Date"));
                if (person.getAgeAsOfDate(enrollDate) < 3) {
                    errors.add(new StateReportValidationError(entity, field,
                            "The student must be at least 3 years old on their first enrollment date",
                            "Date Enrolled = " + STYLE_BOLD + m_dateFormat.format(enrollDate) + STYLE_END +
                                    ", Student's birthdate = " + STYLE_BOLD + m_dateFormat.format(studentDob)
                                    + STYLE_END));
                }
            } catch (ParseException e) {
                errors.add(new StateReportValidationError(entity, field,
                        "Dates formatted incorrectly",
                        "Enroll date = " + STYLE_BOLD + value + STYLE_END +
                                ", Student DOB = " + STYLE_BOLD + entity.getFieldValue("Birth Date")));
            }

            return errors;
        }

    }

    /**
     * The ELL end date must be equal to or greater than the ELL start date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateServicesEnded implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            try {
                Date enrollDate = m_dateFormat.parse(entity.getFieldValue("Date Enrolled"));

                Date endDate = null;
                if (!StringUtils.isEmpty(value)) {
                    endDate = m_dateFormat.parse(value);
                    if (enrollDate.after(endDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Date ELL Services Ended must be equal to or greater than Date Student Enrolled",
                                "Date enrolled = " + STYLE_BOLD + entity.getFieldValue("Date Enrolled") + STYLE_END
                                        + ", Date ended = " + STYLE_BOLD + value + STYLE_END));
                    }
                }

            } catch (ParseException e) {
                errors.add(new StateReportValidationError(entity, field,
                        "Date formatted incorrectly, must be in MM/dd/yyyy format",
                        "Date enrolled = " + STYLE_BOLD + entity.getFieldValue("Date Enrolled") + STYLE_END
                                + ", Date ended = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder();
        heading.append("EL");
        heading.append(',');
        heading.append(m_totalProgramCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, "PGM-ELL");
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
        m_pgmDictionary = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());

        initializeFields();
        loadNewArrivalStudents();
        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        m_schoolCalendarDatesMap = getCSDMap();

        PlainDate schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        PlainDate schoolYearEndDate = getOrganization().getCurrentContext().getEndDate();

        Map<String, ReferenceCode> programCode = new HashMap<String, ReferenceCode>();
        DataDictionaryField programCodeField =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                        StudentProgramParticipation.class.getName(), StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && programCodeField.getReferenceTableOid() != null) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, programCriteria);
            programCode = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 4);
        }

        /*
         * SELECT *
         * FROM STUDENT_PROGRAM_PARTICIPATION
         * WHERE PGM_PROGRAM_CODE = "ELL" AND
         * PGM_START_DATE < endOfSchooL AND
         * (PGM_END_DATE > startOfSchool OR
         * PGM_END_DATE IS NULL)
         */
        String ellCode = programCode.get("ELL").getCode();

        // grab programs that are (1) ELL and (2) within the current context's start and end date
        Criteria programCriteria = getProgramCriteria();
        programCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, ellCode);
        programCriteria.addLessThan(StudentProgramParticipation.COL_START_DATE, schoolYearEndDate);
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE, schoolYearStartDate);
        X2Criteria endDateNullCriteria = new X2Criteria();
        endDateNullCriteria.addIsNull(StudentProgramParticipation.COL_END_DATE);
        endDateCriteria.addOrCriteria(endDateNullCriteria);
        programCriteria.addAndCriteria(endDateCriteria);

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        Integer sort = (Integer) getParameter(PARAM_SORT);
        switch (sort != null ? sort.intValue() : 0) {
            case 0: // Name
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 2: // School
                programQuery.addOrderByAscending(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER
                        + SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 3: // LASID
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID);
                break;

            case 4: // SASID
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID);
                break;

            default:
                programQuery.addOrderByAscending(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;
        }

        setQuery(programQuery);
        setEntityClass(ELLEntity.class);
        initializeStdEdPlansMap();

        // Map of student enrollments by student oid's
        SubQuery studentSubQuery = new SubQuery(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_STUDENT_OID, programCriteria);
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        m_enrollmentMap =
                getBroker().getGroupedCollectionByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 500);

        // Map of student ELL courses
        Criteria courseCriteria = new Criteria();
        courseCriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentSubQuery);
        courseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                m_fieldEslInd, BooleanAsStringConverter.TRUE);
        courseCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_DISTRICT_CONTEXT + PATH_DELIMITER +
                X2BaseBean.COL_OID, getOrganization().getCurrentContext().getOid());
        QueryByCriteria courseQuery = new QueryByCriteria(StudentSchedule.class, courseCriteria);
        m_ellCourseMap = getBroker().getGroupedCollectionByQuery(courseQuery, StudentSchedule.COL_STUDENT_OID, 1024);

        // Additional rule for secondary OUTPLACEMENT school
        Criteria secondaryOutplacementCriteria = new X2Criteria();
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                "OUTPLACEMENT");
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());
        secondaryOutplacementCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);
        X2Criteria sskEndDate = new X2Criteria();
        sskEndDate.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, m_reportDate);
        X2Criteria sskEndDateNull = new X2Criteria();
        sskEndDateNull.addEmpty(StudentSchool.COL_END_DATE, getBroker().getPersistenceKey());
        sskEndDate.addOrCriteria(sskEndDateNull);
        secondaryOutplacementCriteria.addAndCriteria(sskEndDate);

        QueryByCriteria secondaryOutplacementQuery =
                new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
        QueryIterator iter = getBroker().getIteratorByQuery(secondaryOutplacementQuery);

        try {
            while (iter.hasNext()) {
                StudentSchool item = (StudentSchool) iter.next();
                m_secondaryOutplacementSchoolMap.put(item.getStudentOid(),
                        (String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool));
            }
        } finally {
            iter.close();
        }

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("ELL-1ST-ENROLL", new RetrieveFirstEnroll());
        calcs.put("ELL-RCDTS", new RetrieveRCDTS());
        calcs.put("ELL-DATE-ENROLL", new RetrieveDateEnrolled());
        calcs.put("ELL-DATE-ENTRY-US", new RetrieveDateEntryUS());
        calcs.put("PGM-ALIAS", new RetrievePgmValueByAlias());
        super.addCalcs(calcs);

        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("ELL-V-ENROLL", new ValidateEnroll());
        validators.put("ELL-V-ENDED", new ValidateServicesEnded());
        super.addValidators(validators);

    }

    /**
     * Returns first calendar date of most common calendar for passed school.
     *
     * @param schoolOid String
     * @return PlainDate
     */
    protected PlainDate getFirstSchoolDate(String schoolOid) {
        Collection<SchoolCalendarDate> schoolCalDates = m_schoolCalendarDatesMap.get(schoolOid);
        SchoolCalendarDate scd = schoolCalDates.iterator().next();
        return scd.getDate();
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER
                + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * The method which build map of all school calendar dates keyed on school oid.
     *
     * @return Map<String, Collection<SchoolCalendarDate>>
     */
    private Map<String, Collection<SchoolCalendarDate>> getCSDMap() {
        X2Criteria csdCriteria = new X2Criteria();
        csdCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, getMostCommonCalendars());

        QueryByCriteria csdQuery = new QueryByCriteria(SchoolCalendarDate.class, csdCriteria);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, true);
        csdQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);

        return getBroker().getGroupedCollectionByQuery(csdQuery,
                SchoolCalendarDate.REL_SCHOOL_CALENDAR
                        + ModelProperty.PATH_DELIMITER + SchoolCalendar.COL_SCHOOL_OID,
                2056);
    }

    /**
     * Generate the filename for the report.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Build the list of most Common Calendars for schools.
     *
     * @return Collection of SchoolCalendars oids
     */
    private Collection getMostCommonCalendars() {
        Map<String, Map<String, Collection<SchoolCalendar>>> mapSchoolCalendars = getSchoolCalendars();

        HashMap<String, String> schoolCalendars = new HashMap();

        X2Criteria criteria = new X2Criteria();

        String[] columns = new String[] {SisStudent.COL_SCHOOL_OID, SisStudent.COL_CALENDAR_CODE, "count(*)"};

        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class, columns, criteria);
        query.addGroupBy(SisStudent.COL_SCHOOL_OID);
        query.addGroupBy(SisStudent.COL_CALENDAR_CODE);
        query.addOrderByDescending("count(*)");

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                String calendarCode = (String) row[1];

                if (!schoolCalendars.containsKey(schoolOid)) {
                    Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(schoolOid);
                    if (mapCalendars != null && mapCalendars.containsKey(calendarCode)) {
                        SchoolCalendar schoolCalendar = mapCalendars.get(calendarCode).iterator().next();
                        schoolCalendars.put(schoolOid, schoolCalendar.getOid());
                    }
                }
            }
        } finally {
            iterator.close();
        }

        // Add schools without students - any calendar will do
        for (String oid : mapSchoolCalendars.keySet()) {
            if (!schoolCalendars.containsKey(oid)) {
                Map<String, Collection<SchoolCalendar>> mapCalendars = mapSchoolCalendars.get(oid);
                SchoolCalendar schoolCalendar = mapCalendars.values().iterator().next().iterator().next();
                schoolCalendars.put(oid, schoolCalendar.getOid());
            }
        }

        return schoolCalendars.values();
    }

    /**
     * Returns the criteria that retrieves all programs that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getProgramCriteria() {
        Criteria programCriteria = getReportingCriteria();

        // Check student selection criteria user input
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                programCriteria.addEqualTo(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                programCriteria.addEqualTo(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID,
                        queryString);
                break;

            case 3: // SASID
                programCriteria.addEqualTo(
                        StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(programCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return programCriteria;
    }

    /**
     * Get criteria for students that were active before the report date.
     *
     * @return criteria for students that were active before report date
     */
    private Criteria getReportingCriteria() {
        List<String> enrollTypes = new ArrayList<String>();
        enrollTypes.add(StudentEnrollment.WITHDRAWAL);
        enrollTypes.add(StudentEnrollment.ENTRY);

        PlainDate startDate = getOrganization().getCurrentContext().getStartDate();

        // With Enrollment records within the active date range and of the type E,W.
        X2Criteria activityCriteria = new X2Criteria();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollTypes);

        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activityCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT
                    + PATH_DELIMITER + m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        // build the query for students to report.
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        X2Criteria activeCriteria = new X2Criteria();
        activeCriteria.addEqualTo(
                StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS,
                activeCode);

        if (isSchoolContext()) {
            activeCriteria.addEqualTo(
                    StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            activeCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL
                    + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            activeCriteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL
                    + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            activeCriteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addOrCriteria(activeCriteria);
        reportingCriteria.addOrCriteria(enrollCriteria);

        return reportingCriteria;
    }

    /**
     * Build map of maps of SchoolCalendars keyed on school oid and school calendar id.
     *
     * @return Map<String, Map<String, Collection<SchoolCalendar>>>
     */
    private Map<String, Map<String, Collection<SchoolCalendar>>> getSchoolCalendars() {
        X2Criteria casCriteria = new X2Criteria();

        casCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        // Filter to eliminate unused schools.
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR,
                Boolean.TRUE);
        casCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR,
                Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, casCriteria);

        return getBroker().getGroupedCollectionByQuery(query, new String[] {SchoolCalendar.COL_SCHOOL_OID,
                SchoolCalendar.COL_CALENDAR_ID}, new int[] {100, 5});
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldEnrSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_HOME, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldEslInd = translateAliasToJavaName(ALIAS_ELL_IND, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldBirthCountry = translateAliasToJavaName(ALIAS_PERSON_BIRTH_COUNTRY, true);
        m_fieldDateEntryUS = translateAliasToJavaName(ALIAS_FIRST_YEAR_IN_US, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_fieldTransBilingual = translateAliasToJavaNameConsDDX(ALIAS_TRANS_BILINGUAL);
        m_fieldEllStatus = translateAliasToJavaNameConsDDX(ALIAS_ELL_STATUS);
        m_fieldEllEndReason = translateAliasToJavaNameConsDDX(ALIAS_ELL_END_REASON);
        m_fieldElServicesProvided = translateAliasToJavaNameConsDDX(ALIAS_EL_SERVICES_PROVIDED);
        m_fieldClsPeriodWeek = translateAliasToJavaNameConsDDX(ALIAS_DOE_CLS_PERIODS_WEEK);
        m_field504 = translateAliasToJavaName(ALIAS_504, true);
    }

    /**
     * Initialize std ed plans map.
     */
    private void initializeStdEdPlansMap() {
        X2Criteria sepCriteria = new X2Criteria();

        Criteria programCriteria = getQuery().getCriteria();
        SubQuery programSubQuery = new SubQuery(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_STUDENT_OID, programCriteria);

        sepCriteria.addIn(StudentEdPlan.COL_STUDENT_OID, programSubQuery);

        sepCriteria.addLessOrEqualThan(StudentEdPlan.COL_EFFECTIVE_DATE, m_reportDate);
        X2Criteria andCriteria = new X2Criteria();
        andCriteria.addEmpty(StudentEdPlan.COL_END_DATE, getBroker().getPersistenceKey());
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addGreaterOrEqualThan(StudentEdPlan.COL_END_DATE, m_reportDate);
        andCriteria.addOrCriteria(orCriteria);
        sepCriteria.addAndCriteria(andCriteria);

        sepCriteria.addEqualTo(StudentEdPlan.COL_STATUS_CODE, Integer.valueOf(StudentEdPlan.StatusCode.ACTIVE.ordinal()));

        QueryByCriteria query = new QueryByCriteria(StudentEdPlan.class, sepCriteria);

        m_studentActiveEdPlansMap = getBroker().getGroupedCollectionByQuery(query, StudentEdPlan.COL_STUDENT_OID, 200);
    }

    /**
     * Loads students who birth country isn't US and date entry US is on or after May 1 of the
     * previous school year.
     */
    private void loadNewArrivalStudents() {
        X2Criteria studentCriteria = new X2Criteria();
        if (isSchoolContext()) {
            studentCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
            studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    BooleanAsStringConverter.TRUE);
        }
        studentCriteria.addNotEmpty(SisStudent.REL_PERSON + PATH_DELIMITER + m_fieldBirthCountry,
                getBroker().getPersistenceKey());
        studentCriteria.addNotEqualTo(SisStudent.REL_PERSON + PATH_DELIMITER + m_fieldBirthCountry, CODE_US);


        // May 1 of the previous school year
        Calendar cornerDate = Calendar.getInstance();
        cornerDate.setTime(getOrganization().getCurrentContext().getStartDate());
        cornerDate.set(Calendar.MONTH, Calendar.MAY);
        cornerDate.set(Calendar.DAY_OF_MONTH, 1);
        studentCriteria.addGreaterOrEqualThan(m_fieldDateEntryUS, new PlainDate(cornerDate.getTime()));

        SubQuery studentQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        m_newArrivalToUS = getBroker().getSubQueryCollectionByQuery(studentQuery);
    }

    /**
     * Translate alias to java name cons DDX.
     *
     * @param alias String
     * @return String
     */
    private String translateAliasToJavaNameConsDDX(String alias) {
        String javaName = super.translateAliasToJavaName(alias, false);

        if (StringUtils.isEmpty(javaName) && m_pgmDictionary != null) {
            DataDictionaryField dataDictionaryField = m_pgmDictionary.findDataDictionaryFieldByAlias(alias);
            if (dataDictionaryField != null) {
                javaName = dataDictionaryField.getJavaName();
            }
        }

        if (StringUtils.isEmpty(javaName)) {
            super.translateAliasToJavaName(alias, true);
        }

        if (m_beanPathByAliasConsDDX == null) {
            m_beanPathByAliasConsDDX = new HashMap<String, String>();
        }

        m_beanPathByAliasConsDDX.put(alias, javaName);

        return javaName;
    }
}
