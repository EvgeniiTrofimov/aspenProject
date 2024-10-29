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
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import com.x2dev.procedures.statereporting.il.DemoExitDataHelper.SpansFactory.StudentDemoDatasets.DemoDataset;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Student Demographic/Enrollment.
 *
 * @author X2 Development Corporation
 */
public class StudentDemographics extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentDemographicEntity extends StateReportEntity {
        private StudentEnrollment m_primaryEnrollment;
        private SisStudent m_student;
        /**
         * All the "Free/Reduced Lunch" programs for this entry by descending start date order
         */
        List<StudentProgramParticipation> m_programs;

        List<DemoDataset> m_datasets = new ArrayList<DemoDataset>();
        /**
         * StudentDemographics data.
         */
        StudentDemographics m_sdData = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentDemographicEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the enrollment.
         *
         * @return Student enrollment
         */
        public StudentEnrollment getEnrollment() {
            return m_primaryEnrollment;
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
                    ", SASID: " + student.getStateId() + "]";

            return name;
        }

        /**
         * Gets the school with fte.
         *
         * @return Demo dataset
         */
        public DemoDataset getSchoolWithFte() {
            return m_datasets.get(getCurrentRow());
        }

        /**
         * Initialize and increment counter.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_sdData = (StudentDemographics) data;
            m_student = (SisStudent) bean;
            m_primaryEnrollment =
                    m_sdData.m_helper.getEnrollmentForDate(m_student.getOid(), m_sdData.m_reportDate, "EYS");

            m_datasets = new ArrayList<DemoDataset>(
                    m_sdData.m_ilExitDemoHelper.getDatasets(m_student, m_sdData.m_reportDate));

            setRowCount(m_datasets.size());

            m_sdData.m_totalCount += m_datasets.size();
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
     * Retrieve the student's alt assessment value
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveAltAssessment implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();

            String altAssessmentInd = (String) student.getFieldValueByBeanPath(m_fieldAlternateAssessmentInd);

            if (BooleanAsStringConverter.TRUE.equals(altAssessmentInd)) {
                return "01";
            }
            return "02";
        }
    }

    /**
     * Retrieve entry date.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveEntryDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PlainDate entryDate = null;

            StudentDemographicEntity sdEntity = (StudentDemographicEntity) entity;
            entryDate = sdEntity.getSchoolWithFte().getBeginDate();

            return entryDate;
        }
    }

    /**
     * Retrieve the student's most recent enrollment's entry type state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEntryType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String entryCode = "";
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool skl = student.getSchool();
            StudentEnrollment enrollment = null;
            if (BooleanAsStringConverter.TRUE
                    .equals(skl.getFieldValueByBeanPath(m_ilExitDemoHelper.m_fieldSklNonCalcFte))) {
                enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, StudentEnrollment.ENTRY);
            } else {
                enrollment = ((StudentDemographicEntity) entity).getEnrollment();
            }
            if (enrollment != null) {
                if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType()) && !StringUtils
                        .isEmpty(entryCode = (String) enrollment.getFieldValueByBeanPath(m_fieldEnrTypeForY))) {
                    entryCode = lookupStateValue(StudentEnrollment.class, m_fieldEnrTypeForY, entryCode);
                } else {
                    String enrollmentCode = enrollment.getEnrollmentCode();
                    if (m_enrollmentCodes.containsKey(enrollmentCode)) {
                        ReferenceCode refCode = m_enrollmentCodes.get(enrollmentCode);
                        entryCode = refCode.getStateCode();
                    }
                }
            }
            return entryCode;
        }
    }

    /**
     * Returns the calculated FTE for the current school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFte implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((StudentDemographicEntity) entity).getSchoolWithFte().getFte();
        }
    }

    /**
     * Returns the calculated FTE for the current school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLvl implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((StudentDemographicEntity) entity).getSchoolWithFte().getGradeLvl();
        }
    }

    /**
     * Retrieve the student's Homeless status based on existing PGMs
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveHomelessInd implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            SisStudent student = (SisStudent) entity.getBean();
            return m_participationHomelessMap.get(student.getOid()) != null
                    && !m_participationHomelessMap.get(student.getOid()).isEmpty() ? Boolean.valueOf(true)
                            : Boolean.valueOf(false);
        }
    }

    /**
     * Retrieve the student's IEP status based on existing IEPs with iepStatus = "Active" on report
     * date
     * or
     * stdSpedStatus.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveIDEA implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean isIep = false;

            SisStudent student = (SisStudent) entity.getBean();
            /*
             * Apply a hard rule that when student.[DOE PRIVATE SCHOOL IND] = True, that IEP
             * Indicator
             * (File position 240) must = 02 in the output.
             */
            String privateSchoolInd = (String) student.getFieldValueByBeanPath(m_fieldPrivateSchoolInd);

            if (BooleanAsStringConverter.TRUE.equals(privateSchoolInd)) {
                return Boolean.valueOf(true);
            }

            if (m_stdActiveIepMap.containsKey(student.getOid())) {
                isIep = true;
            } else {
                if (!StringUtils.isEmpty(student.getSpedStatusCode()) &&
                        m_activeIepCodes.contains(student.getSpedStatusCode())) {
                    isIep = true;
                }
            }

            return Boolean.valueOf(isIep);
        }
    }

    /**
     * Retrieve the student's LEP status based on their program participation. If there exists a
     * program participation that is ESL and the report date falls within:
     *
     * (1) - between its start date and end date, or
     * (2) - after the start date if an end date is not specified
     *
     * Then the student is considered having an LEP
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveLep implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentProgramParticipation> participations = m_participationMap.get(student.getOid());
            Boolean isInLep = Boolean.FALSE;
            if (participations != null /* && checkAccessScore(student.getOid()) */ ) {
                for (StudentProgramParticipation participation : participations) {
                    Date participationStartDate = participation.getStartDate();
                    Date participationEndDate = participation.getEndDate();

                    if (participationEndDate != null &&
                            !m_reportDate.before(participationStartDate) &&
                            !m_reportDate.after(participationEndDate)) {
                        isInLep = Boolean.TRUE;
                        break;
                    } else if (participationEndDate == null &&
                            !m_reportDate.before(participationStartDate)) {
                        isInLep = Boolean.TRUE;
                        break;
                    }
                }
            }

            return isInLep;
        }
    }

    /**
     * Retrieve the student's FRL/Low Income Indicator.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveLowIncInd implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean lowIncInd = false;

            SisStudent student = (SisStudent) entity.getBean();
            StudentEnrollment enrollment = ((StudentDemographicEntity) entity).getEnrollment();

            if (BooleanAsStringConverter.TRUE
                    .equals(enrollment.getSchool().getFieldValueByBeanPath(m_fieldSklWideFarms))) {
                lowIncInd = true;
            } else if (m_lowIncMap.get(student.getOid()) != null && !m_lowIncMap.get(student.getOid()).isEmpty()) {
                lowIncInd = true;
            }
            return Boolean.valueOf(lowIncInd);
        }
    }

    /**
     * Retrieve the student's native language's state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveNativeLang implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String nativeLanguage = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (StringUtils.isEmpty(nativeLanguage)) {
                if (!StringUtils.isEmpty(student.getHomeLanguageCode())) {
                    nativeLanguage = student.getHomeLanguageCode();
                }
            }

            return nativeLanguage;
        }
    }

    /**
     * Retrieve the student's race's state code. If the student has the Hispanic/Latino indicator
     * checked,
     * return the Hispanic state code. If the student has more than 1 race, return the
     * "Two or More Race"
     * state code. Otherwise, return the student's race's state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {

            String raceCode = "";
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();
            if (person != null && person.getHispanicLatinoIndicator()) {
                raceCode = "11";
            } else {
                Collection<Race> races = m_helper.getRaces(student);
                if (races != null) {
                    if (races.size() > 1) {
                        raceCode = "17";
                    } else {
                        for (Race race : races) {
                            if (m_raceCodes.containsKey(race.getRaceCode())) {
                                ReferenceCode refCode = m_raceCodes.get(race.getRaceCode());
                                raceCode = refCode.getStateCode() != null ? refCode.getStateCode() : "";
                            }
                        }
                    }
                }
            }

            return raceCode;
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
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            String rcdts = null;
            if (param.equals("H")) {
                rcdts = ((StudentDemographicEntity) entity).getSchoolWithFte().getHomeSchoolCode();
            } else if (param.equals("S")) {
                rcdts = ((StudentDemographicEntity) entity).getSchoolWithFte().getServiceSchoolCode();
            }
            return rcdts;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z -]";

        private Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * If school.[DOE SCHOOLWIDE TITLE 1] set to true, all students
     * enrolled in that school shall return with '11' for data element Title I.
     * If school set to false, look at student.DOE TITLE 1 IND
     *
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTitleIInd implements FieldRetriever {

        private static final String CALC_ID = "TITLE-1-IND";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String value = null;
            School school = ((StudentDemographicEntity) entity).getSchoolWithFte().getSchool();
            if (BooleanAsStringConverter.TRUE.equals(school.getFieldValueByBeanPath(m_fieldSklWideTitle))) {
                value = "11";
            } else {
                SisStudent student = (SisStudent) entity.getBean();
                value = (String) student.getFieldValueByBeanPath(m_fieldStdTitleInd);
                if (!StringUtils.isEmpty(value)) {
                    value = lookupStateValue(SisStudent.class, m_fieldStdTitleInd, value);
                }
            }
            return value;
        }
    }

    /**
     * Career/Tech Ed Indicator for Pre-K thru Grade 8 must be set to "No."
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateCareerTechEd implements FieldValidator {

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
            String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            if (gradeLevel.matches("14|15|0[0-8]") && value.equals("1")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Career/Tech Ed Indicator for Pre-K thru Grade 8 must be set to No.",
                        "Grade Level = " + STYLE_BOLD + gradeLevel + STYLE_END + ", Career/Tech Ed Ind = " + STYLE_BOLD
                                + "0" + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validates the student's most recent enrollment's entry date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEntryDate implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();
            PlainDate studentDob = null;
            try {
                studentDob = person.getDob();
            } catch (NullPointerException npe) {
                System.out.println(student.getNameView());
            }
            PlainDate enrollmentDate = null;

            // Enrollment Date cannot be a future date
            try {
                enrollmentDate = new PlainDate(m_dateFormat.parse(value));
            } catch (ParseException e1) {
                errors.add(new StateReportValidationError(entity, field,
                        "Entry date needs to be in MM/dd/yyyy format",
                        "Entry date = " + STYLE_BOLD + value + STYLE_END));
            }

            if (enrollmentDate != null) {
                if (enrollmentDate.after(m_reportDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment Date cannot be a future date",
                            "Enrollment date = " + STYLE_BOLD + m_dateFormat.format(value) + STYLE_END));
                }

                // All students must be at least 3 years old on the 1st day of class.
                int ageOfStudentOn1stDay = student.getPerson().getAgeAsOfDate(enrollmentDate);
                if (ageOfStudentOn1stDay < 3) {

                    String studentDobAsString = null;
                    if (studentDob != null) {
                        studentDobAsString = m_dateFormat.format(studentDob);
                    } else {
                        studentDobAsString = "empty";
                    }

                    errors.add(new StateReportValidationError(entity, field,
                            "All students must be at least 3 years old on the 1st day of class",
                            "Student's DOB = " + STYLE_BOLD + studentDobAsString + STYLE_END +
                                    ", Entry date = " + STYLE_BOLD + value + STYLE_END));
                }

                // Pre-K students with IEP = No. Must be less than 5 years old on September 1 of the
                // current school year
                int currentSchoolYear = student.getOrganization1().getCurrentContext().getSchoolYear();
                String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
                String hasIep = entity.getFieldValue(COLUMN_IEP_INDICATOR);
                if (gradeLevel.equals("14") && hasIep.equals(BooleanAsStringConverter.FALSE)) {
                    Calendar cal = Calendar.getInstance();
                    cal.set(Calendar.MONTH, Calendar.SEPTEMBER);
                    cal.set(Calendar.DAY_OF_MONTH, 1);
                    cal.set(Calendar.YEAR, currentSchoolYear - 1);
                    cal.add(Calendar.YEAR, -5);
                    PlainDate sept1ofCurSchYear = new PlainDate(cal.getTimeInMillis());

                    int ageOnSept1 = student.getPerson().getAgeAsOfDate(sept1ofCurSchYear);
                    if (5 <= ageOnSept1) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Pre-K students with IEP must not be less than 5 years old on September 1 of the current school year",
                                "Student's DOB = " + STYLE_BOLD + m_dateFormat.format(studentDob) + STYLE_END));
                    }
                }

            } else {
                errors.add(new StateReportValidationError(entity, field,
                        "Student does not have an enrollment record",
                        ""));
            }

            return errors;
        }
    }

    /**
     * Validates the student's FTE (Full-time Equivalency).
     *
     * @author X2 Development Corporation
     */
    protected class ValidateFte implements FieldValidator {

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
            String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            double fte = Double.parseDouble(value);

            // FTE for Pre-K (Code 14) Entry/Grade Level must be 1.0
            if (gradeLevel.equals("14") && fte != 1.00) {
                errors.add(new StateReportValidationError(entity, field,
                        "FTE for Pre-K (Code 14) Entry/Grade Level must be 1.0",
                        "Grade level = " + STYLE_BOLD + gradeLevel + STYLE_END + ", FTE = " + STYLE_BOLD + value
                                + STYLE_END));
            }

            // FTE for Kindergarten (Code 15) Entry/Grade Level must be 0.5 for half-day or 1.0 for
            // full-day
            if (gradeLevel.equals("15") && (fte != 0.5 || fte != 1.0)) {
                errors.add(new StateReportValidationError(entity, field,
                        "FTE for Kindergarten (Code 15) Entry/Grade Level must be 0.5 for half-day or 1.0 for full-day",
                        "Grade level = " + STYLE_BOLD + gradeLevel + STYLE_END + ", FTE = " + STYLE_BOLD + value
                                + STYLE_END));
            }

            // FTE must be in a range of 0 and 1
            if (fte <= 0 || fte > 1) {
                errors.add(new StateReportValidationError(entity, field,
                        "FTE is out of range.  Must be greater than 0 and less than or equal to 1.00",
                        "FTE = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validates the student's native language.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateLang implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            // If Student is enrolled as Pre-K, the Native language must be included.
            String entryLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            if (entryLevel != null && entryLevel.equals("14") && (StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "If student is enrolled as Pre-K, the Native language must be included",
                        "Native Language = " + STYLE_BOLD + value
                                + STYLE_END));
            }

            // If Student is enrolled as LEP the Native language must be included and cannot be
            // English
            String isLEP = entity.getFieldValue(COLUMN_LEP_INDICATOR);
            if ("1".equals(isLEP) && "000".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Student is enrolled as LEP the Native language must be included and cannot be English",
                        "Native Language = " + STYLE_BOLD + value
                                + STYLE_END + ", LEP = " + isLEP));
            }

            return errors;
        }
    }

    /**
     * SES indicator for Birth to 3 and Pre-K must be set to 'No.'
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateSes implements FieldValidator {

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
            String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            if (gradeLevel.matches("00|14") && value.equals("1")) {
                errors.add(new StateReportValidationError(entity, field,
                        "SES indicator for Pre-K must be set to 'No.'",
                        "Grade level = " + STYLE_BOLD + gradeLevel + STYLE_END + ", SES = " + STYLE_BOLD + "0" + value
                                + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate the student's age on report date
     *
     * Applies to: Reading First, Reading Improv. Ind, Elig. for Imm Edu, & Century 21 Indicator
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateStudentAge implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();

            // Students ages Birth to 3 should be set to "No"
            int ageOnReportDate = student.getPerson().getAgeAsOfDate(m_reportDate);
            if (ageOnReportDate < 3 && !value.equals("2")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Students ages Birth to 3 years of age should be set to 'No'",
                        "Student's age = " + STYLE_BOLD + student.getPerson().getAge() + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate the student's age on report date
     *
     * Applies to: Title 1st Indicator.
     *
     * @author Follett Software Company
     */
    protected class ValidateTitle1Age implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();

            int ageOnReportDate = student.getPerson().getAgeAsOfDate(m_reportDate);
            if (ageOnReportDate < 3 && !value.equals("13")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Students ages Birth to 3 years of age should be set to '13'",
                        "Student's age = " + STYLE_BOLD + student.getPerson().getAge() + STYLE_END + ", " +
                                "Title 1 = " + STYLE_BOLD + value + STYLE_END));
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
        StringBuilder heading = new StringBuilder(100);
        heading.append("Student Demographics");
        heading.append(',');
        heading.append(m_totalCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ALTERNATE_ASSESSMENT_IND = "all-std-AlternateAssessment";
    protected static final String ALIAS_DOE_NON_CALC_FTE = "DOE NON-CALCULATING FTE";
    protected static final String ALIAS_ENR_TYPE_FOR_Y = "DOE ENROLLMENT TYPE";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_ENR_SKL_SERVICE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_ENR_FTE_OVERRIDE = "all-enr-FTE";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_OUTPLACED_FTE_OVERRIDE = "DOE OUTPLACED FTE OVERRIDE";
    protected static final String ALIAS_PRIVATE_SCHOOL_IND = "DOE PRIVATE SCHOOL IND";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SKL_WIDE_FARMS = "DOE SCHOOL-WIDE FARMS";
    protected static final String ALIAS_SKL_WIDE_TITLE = "DOE SCHOOLWIDE TITLE 1";
    protected static final String ALIAS_STD_TITLE_IND = "DOE TITLE 1 IND";
    protected static final String ALIAS_SUMMER_IND = "DOE SUMMER IND";

    /*
     * Columns
     */
    protected static final String COLUMN_ENTRY_GRADE_LEVEL = "Entry/Grade Level";
    protected static final String COLUMN_IEP_INDICATOR = "IEP Indicator";
    protected static final String COLUMN_LEP_INDICATOR = "LEP Indicator";

    /*
     * Parameters
     */
    private static final String PARAM_HELPER_MODE = "helperMode";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_REQUEST_UPDATE = "requestUpdate";

    /*
     * Other internal constants
     */
    protected static final String ACTIVE_IEP_CODE = "01";
    protected static final String REPORTABLE_CODE = "report";

    /*
     * Instance variables
     */
    protected String m_activeCode;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_fieldDistrictCode;
    protected String m_fieldExcludeSchool;
    protected String m_fieldEnrTypeForY;
    protected String m_fieldPrivateSchoolInd;
    protected String m_fieldAlternateAssessmentInd;
    protected PlainDate m_reportDate;
    protected String m_fieldServiceDistrictCode;
    protected String m_fieldSklCode;
    protected String m_fieldSklWideFarms;
    protected String m_fieldSklWideTitle;
    protected String m_fieldStdTitleInd;

    /**
     * Helper class:
     * For student selection by enrollment.
     * For Student schedule span.
     */
    protected StudentHistoryHelper m_helper;
    protected StudentHistoryHelper m_helperSched;
    protected String m_helperMode;
    protected DemoExitDataHelper m_ilExitDemoHelper;

    /**
     * Keep track of number of students
     */
    protected int m_totalCount;

    protected Collection<String> m_activeIepCodes = new ArrayList<String>();

    /**
     * A map of student assessments, for use in retrieving LEP
     */
    protected Map<String, Collection<StudentAssessment>> m_assessmentMap =
            new HashMap<String, Collection<StudentAssessment>>();

    /**
     * A map of reference codes for enrollment types, for use in the entry type retriever
     */
    protected Map<String, ReferenceCode> m_enrollmentCodes;

    /**
     * A map of student program participations ("FRL/Low Income Indicator"), for use in the LowInc
     * retriever
     */
    protected Map<String, List<StudentProgramParticipation>> m_lowIncMap;

    /**
     * A map of student program participations ("ESL"), for use in the LEP retriever
     */
    protected Map<String, Collection<StudentProgramParticipation>> m_participationMap;

    /**
     * A map of student program participations state ("HMLS"), for use in the Homeless Ind retriever
     */
    protected Map<String, Collection<StudentProgramParticipation>> m_participationHomelessMap;

    /**
     * A map of reference codes for race codes, for use in the race code retriever.
     */
    protected Map<String, ReferenceCode> m_raceCodes;

    /**
     * A map of student's IEPs that are active on report date, for use in the IEP retriever
     */
    protected Map<String, IepData> m_stdActiveIepMap = new HashMap<String, IepData>();

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));

        m_ilExitDemoHelper = new DemoExitDataHelper(this);
        m_ilExitDemoHelper.setLastSubmissionDate(m_reportDate);
        m_ilExitDemoHelper.setCurrentSubmissionDate(m_reportDate);

        m_helperMode = (String) getParameter(PARAM_HELPER_MODE);
        if (!StringUtils.isEmpty(m_helperMode) &&
                m_helperMode.equals(DemoExitDataHelper.MODE_SUMMER_SCHOOL)) {
            m_ilExitDemoHelper.setHelperMode(DemoExitDataHelper.MODE_SUMMER_SCHOOL);
        }

        initializeFields();
        setEntityClass(StudentDemographicEntity.class);

        m_helper = m_ilExitDemoHelper.getEnrollmentHelper();

        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helperSched = m_ilExitDemoHelper.getScheduleHelper();

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addNotIn(SisStudent.COL_GRADE_LEVEL, getExcludedGradeCodes());

        X2Criteria schedCriteria = m_helperSched.getStudentScheduleCriteria();
        X2Criteria schedChangeCriteria = m_helperSched.getStudentScheduleChangeCriteria();
        X2Criteria schedTransCriteria = m_helperSched.getStudentTranscriptCriteria();

        schedCriteria.addGreaterThan(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_CREDIT,
                BigDecimal.valueOf(0));
        schedChangeCriteria.addGreaterThan(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_CREDIT,
                BigDecimal.valueOf(0));
        schedTransCriteria.addGreaterThan(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_CREDIT,
                BigDecimal.valueOf(0));

        // check for request (students without state ids) / update (students with state ids)
        int requestUpdateSelection = ((Integer) getParameter(PARAM_REQUEST_UPDATE)).intValue();
        switch (requestUpdateSelection) {
            case 1:
                studentCriteria.addEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
                schedCriteria.addEmpty(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
                schedChangeCriteria.addEmpty(
                        StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
                schedTransCriteria.addEmpty(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
                break;
            case 2:
                studentCriteria.addNotEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
                schedCriteria.addNotEmpty(StudentSchedule.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
                schedChangeCriteria.addNotEmpty(
                        StudentScheduleChange.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
                schedTransCriteria.addNotEmpty(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID,
                        getBroker().getPersistenceKey());
                break;
            default:
                break;
        }

        // Set the query to be used for student selection.
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        setQuery(studentQuery);

        m_ilExitDemoHelper.initializeDatasets();

        loadHMLSProgramsByStudent(studentSubQuery);

        // Get enrollment reference codes for use in the entry type
        String referenceTableOid =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        if (referenceTableOid != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, enrollmentCriteria);
            m_enrollmentCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);
        }

        // Get race code reference codes for use in the race retriever.
        DataDictionaryField raceCodeField = getDataDictionaryField(Race.class, Race.COL_RACE_CODE);
        if (raceCodeField != null && !StringUtils.isEmpty(raceCodeField.getReferenceTableOid())) {
            m_raceCodes = getReferenceCodes(raceCodeField.getReferenceTableOid());
        }

        // Map of student ACCESS scores by studentOid
        Criteria assessmentCriteria = new Criteria();
        assessmentCriteria.addEqualTo(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER + AssessmentDefinition.COL_NAME, "ACCESS");
        assessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria assessmentQuery = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
        assessmentQuery.addOrderByAscending(StudentAssessment.COL_DATE);

        Map<String, ReferenceCode> programCode = new HashMap<String, ReferenceCode>();
        DataDictionaryField programCodeField =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, programCriteria);
            programCode = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 4);
        }
        String ellCode = programCode.get("ELL").getCode();

        // Map of student program participations that has "ELL" as its code by studentOid
        Criteria participationCriteria = new Criteria();
        participationCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, ellCode);
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria participationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, participationCriteria);
        participationQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_participationMap = getBroker().getGroupedCollectionByQuery(participationQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 512);

        // Map of IepData on report date with active status
        X2Criteria activeIepCriteria = new X2Criteria();

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDate);
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(IepData.COL_END_DATE, getBroker().getPersistenceKey());
        endDateCriteria.addOrCriteria(orCriteria);

        activeIepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDate);
        activeIepCriteria.addAndCriteria(endDateCriteria);

        activeIepCriteria.addIn(IepData.COL_STATUS_CODE, Arrays.asList(
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal())));
        QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        m_stdActiveIepMap = getBroker().getMapByQuery(iepQuery, IepData.COL_STUDENT_OID, 200);

        DataDictionaryField iepStatusField = getDataDictionaryField(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE);
        if (iepStatusField != null && !StringUtils.isEmpty(iepStatusField.getReferenceTableOid())) {
            Map<String, ReferenceCode> referenceCodes = getReferenceCodes(iepStatusField.getReferenceTableOid());
            Collection<ReferenceCode> codes = referenceCodes.values();
            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (ACTIVE_IEP_CODE.equals(stateCode)) {
                    m_activeIepCodes.add(code.getCode());
                }
            }
        }

        // Map of student program participations that has "FARMS" as its state code by
        // studentOid
        DataDictionaryField pgmCodeField = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        ReferenceTable pgmCodesRefTable = pgmCodeField.getReferenceTable();
        ArrayList<String> stateFarmsCodes = new ArrayList<String>();
        if (pgmCodesRefTable != null) {
            Collection<ReferenceCode> pgmCodes = pgmCodesRefTable.getReferenceCodes();
            for (ReferenceCode pgmCode : pgmCodes) {
                if ("FARMS".equals(pgmCode.getStateCode())) {
                    stateFarmsCodes.add(pgmCode.getCode());
                }
            }
        }

        X2Criteria programsLowIncCriteria = new X2Criteria();
        programsLowIncCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        X2Criteria pgmEndDateCriteria = new X2Criteria();
        pgmEndDateCriteria.addGreaterThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
        X2Criteria pgmEndDateNullCriteria = new X2Criteria();
        pgmEndDateNullCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        pgmEndDateCriteria.addOrCriteria(pgmEndDateNullCriteria);
        programsLowIncCriteria.addAndCriteria(pgmEndDateCriteria);
        programsLowIncCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, stateFarmsCodes);
        programsLowIncCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria programLowIncQuery =
                new QueryByCriteria(StudentProgramParticipation.class, programsLowIncCriteria);
        programLowIncQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_lowIncMap = getBroker().getGroupedCollectionByQuery(programLowIncQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 2048);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("DEMO-STRIPCHAR", new RetrieveStripNameChar());
        calcs.put("DEMO-RACE", new RetrieveRace());
        calcs.put("DEMO-RCDTS", new RetrieveRcdts());
        calcs.put("DEMO-ALT-ASSESSMENT", new RetrieveAltAssessment());
        calcs.put("DEMO-ENTRY-DATE", new RetrieveEntryDate());
        calcs.put("DEMO-ENTRY-TYPE", new RetrieveEntryType());
        calcs.put("DEMO-NATIVE-LANG", new RetrieveNativeLang());
        calcs.put("DEMO-IDEA", new RetrieveIDEA());
        calcs.put("DEMO-LEP", new RetrieveLep());
        calcs.put("DEMO-FTE", new RetrieveFte());
        calcs.put("DEMO-LOW-INC", new RetrieveLowIncInd());
        calcs.put("DEMO-GRD-LVL", new RetrieveGradeLvl());
        calcs.put("DEMO-HMLS-IND", new RetrieveHomelessInd());
        calcs.put(RetrieveTitleIInd.CALC_ID, new RetrieveTitleIInd());
        super.addCalcs(calcs);

        // Build a map of validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("DEMO-LANG", new ValidateLang());
        validators.put("DEMO-V-ENTRY-DATE", new ValidateEntryDate());
        validators.put("DEMO-FTE", new ValidateFte());
        validators.put("DEMO-AGE", new ValidateStudentAge());
        validators.put("DEMO-CAREER-TECH", new ValidateCareerTechEd());
        validators.put("DEMO-TITLE1", new ValidateTitle1Age());
        super.addValidators(validators);
    }

    /**
     * If an ACCESS score exists with overall proficiency level of 4.8 or higher
     * AND with literacy proficiency level of 4.2 or higher
     *
     * Used by RetrieveLep
     *
     * @param studentOid String
     * @return true if overall proficiency level < 4.8 AND literacy proficiency level < 4.2
     */
    protected boolean checkAccessScore(String studentOid) {
        StudentAssessment assessment = (StudentAssessment) m_assessmentMap.get(studentOid);
        boolean result = false;
        if (assessment != null) {
            try {
                double overallProfLvl = Double.parseDouble(assessment.getFieldA011()); // TODO:
                                                                                       // Change
                                                                                       // when we
                                                                                       // can use
                                                                                       // extended
                                                                                       // dictionaries
                double literacyProfLvl = Double.parseDouble(assessment.getFieldA013()); // TODO:
                                                                                        // Chance
                                                                                        // when we
                                                                                        // can use
                                                                                        // extended
                                                                                        // dictionaries
                if (overallProfLvl < 4.8 || literacyProfLvl < 4.2) {
                    result = true;
                }
            } catch (NullPointerException npe) {
                result = false;
            } catch (NumberFormatException nfe) {
                result = false;
            }
        }
        return result;
    }

    /**
     * Load grade codes.
     */
    private Set<String> getExcludedGradeCodes() {
        Set<String> gradesToExclude = new HashSet<>();
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        for (ReferenceCode refCode : referenceTable.getCodeMap().values()) {
            if ("14".equals(refCode.getStateCode())) {
                gradesToExclude.add(refCode.getCode());
            }
        }
        return gradesToExclude;
    }

    /**
     * Generate the filename for this export.
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
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldExcludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldPrivateSchoolInd = translateAliasToJavaName(ALIAS_PRIVATE_SCHOOL_IND, true);
        m_fieldAlternateAssessmentInd = translateAliasToJavaName(ALIAS_ALTERNATE_ASSESSMENT_IND, true);
        m_fieldSklCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSklWideFarms = translateAliasToJavaName(ALIAS_SKL_WIDE_FARMS, true);
        m_fieldSklWideTitle = translateAliasToJavaName(ALIAS_SKL_WIDE_TITLE, true);
        m_fieldStdTitleInd = translateAliasToJavaName(ALIAS_STD_TITLE_IND, true);
        m_fieldEnrTypeForY = translateAliasToJavaName(ALIAS_ENR_TYPE_FOR_Y, true);
    }

    /**
     * Load map of homeless programs keyed on student oid.
     *
     * @param studentSubQuery
     */
    private void loadHMLSProgramsByStudent(SubQuery studentSubQuery) {
        Collection<String> programCodes = new ArrayList<String>();
        DataDictionaryField programCodeField =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            programCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, "HMLS");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, programCriteria);
            programCodes.addAll(getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 10).keySet());
        }

        // Map of student program participations that has "ELL" as its code by studentOid
        Criteria participationCriteria = new Criteria();
        if (!programCodes.isEmpty()) {
            participationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCodes);
        } else {
            participationCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, "___dummy___");
        }
        participationCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        participationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getEndDate());
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria participationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, participationCriteria);
        participationQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_participationHomelessMap = getBroker().getGroupedCollectionByQuery(participationQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 512);
    }
}
