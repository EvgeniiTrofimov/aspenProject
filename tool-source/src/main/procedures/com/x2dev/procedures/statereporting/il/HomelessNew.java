/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Illinois state export procedure for Homeless Data Collection
 *
 * Search through generic form data that uses the Homeless extended
 * dictionary and belongs to active students.
 *
 * @author X2 Development Corporation
 */
public class HomelessNew extends StateReportData {


    /**
     * Implementation of StateReportEntity to be used by the IL. This must be a
     * public static inner class with a public no argument constructor so it can
     * be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class HomelessEntity extends StateReportEntity {
        /**
         * Primary enrollment of the student.
         */
        StudentEnrollment m_primaryEnrollment = null;

        /**
         *
         */
        StudentSchool m_secondaryStdSchool = null;

        /**
         * Homeless data.
         */
        HomelessNew m_hData = null;

        /**
         * The student for this entity
         */
        SisStudent m_student = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public HomelessEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Filter only records from students that are active by report date.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            StudentEnrollment recentEnrollment = getPrimaryEnrollment();
            if (recentEnrollment == null) {
                error = new StateReportValidationError(getEntityName(), "Enrollments", "No student enrollment records",
                        "");
            } else if (recentEnrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                error = new StateReportValidationError(getEntityName(), "Enrollment date", "Before report date", "");
            }
            return error;
        }

        /**
         * Gets the secondary student school.
         *
         * @return Student school
         */
        public StudentSchool getSecondaryStudentSchool() {
            return m_secondaryStdSchool;
        }

        /**
         * Gets the secondary school id.
         *
         * @return String
         */
        public String getSecondarySchoolId() {
            String serviceSchoolId = null;

            if (m_secondaryStdSchool != null) {
                serviceSchoolId =
                        (String) m_secondaryStdSchool.getFieldValueByBeanPath(m_hData.m_fieldRcdtsForServingSchool);
                if (!StringUtils.isEmpty(serviceSchoolId)) {
                    serviceSchoolId =
                            m_hData.lookupStateValue(StudentEnrollment.class, m_hData.m_fieldServiceSchoolCode,
                                    serviceSchoolId);

                }
            }
            return serviceSchoolId;
        }

        /**
         * Gets the primary school.
         *
         * @return School
         */
        public School getPrimarySchool() {
            return m_primaryEnrollment.getSchool();
        }

        /**
         * Gets the primary school id.
         *
         * @return StudentSchool
         */
        public String getPrimarySchoolId() {
            String school = null;
            if (m_primaryEnrollment.getSchool() != null) {
                school = (String) m_primaryEnrollment.getSchool().getFieldValueByBeanPath(m_hData.m_fieldSchoolId);
            }

            return school;
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getPrimaryEnrollment() {
            return m_primaryEnrollment;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = m_student.getNameView() +
                    " [LASID: " + m_student.getLocalId() +
                    ", SASID: " + m_student.getStateId() + "]";
            return name;
        }

        /**
         * Initialize and increment counter
         *
         * If there is no recent entry enrollment record, ignore it.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_hData = (HomelessNew) data;
            StudentProgramParticipation program = (StudentProgramParticipation) bean;
            m_student = program.getStudent();
            m_primaryEnrollment = m_hData.getPrimaryEnrollment(m_student.getOid());
            m_secondaryStdSchool = m_hData.getSecondaryStudentSchool(m_student.getOid());

            if (m_primaryEnrollment != null && m_primaryEnrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE.equals(
                            m_primaryEnrollment.getSchool().getFieldValueByBeanPath(m_hData.m_excludeSklField))) {
                // keep count of records
                m_hData.m_totalHomelessRecords++;
            } else {
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
     * StudentHistoryHelper with limited student by report criteria.
     *
     * @author Follett Software Company
     */
    private class HomelessStdHistoryHelper extends StudentHistoryHelper {

        private X2Criteria m_parentScheduleCriteria = null;
        private X2Criteria m_parentScheduleChangeCriteria = null;
        private X2Criteria m_parentTranscriptCriteria = null;

        /**
         * Instantiates a new homeless std history helper.
         *
         * @param data StateReportData
         */
        public HomelessStdHistoryHelper(StateReportData data) {
            super(data);
        }


        /**
         * Gets the student schedule criteria.
         *
         * @return X 2 criteria
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentScheduleCriteria()
         */
        @Override
        public X2Criteria getStudentScheduleCriteria() {
            if (m_parentScheduleCriteria == null) {
                m_parentScheduleCriteria = super.getStudentScheduleCriteria();

                X2Criteria programParticipationCriteria = getCopyProgramParticipationCriteria();
                SubQuery programSubQuery = new SubQuery(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_STUDENT_OID, programParticipationCriteria);
                m_parentScheduleCriteria.addIn(StudentSchedule.COL_STUDENT_OID, programSubQuery);

            }

            return m_parentScheduleCriteria;
        }

        /**
         * Gets the student schedule change criteria.
         *
         * @return X 2 criteria
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentScheduleChangeCriteria()
         */
        @Override
        public X2Criteria getStudentScheduleChangeCriteria() {
            if (m_parentScheduleChangeCriteria == null) {
                m_parentScheduleChangeCriteria = super.getStudentScheduleChangeCriteria();

                X2Criteria programParticipationCriteria = getCopyProgramParticipationCriteria();
                SubQuery programSubQuery = new SubQuery(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_STUDENT_OID, programParticipationCriteria);
                m_parentScheduleChangeCriteria.addIn(StudentScheduleChange.COL_STUDENT_OID, programSubQuery);
            }

            return m_parentScheduleChangeCriteria;
        }

        /**
         * Gets the student transcript criteria.
         *
         * @return X 2 criteria
         * @see com.x2dev.sis.tools.stateexports.StudentHistoryHelper#getStudentTranscriptCriteria()
         */
        @Override
        public X2Criteria getStudentTranscriptCriteria() {
            if (m_parentTranscriptCriteria == null) {
                m_parentTranscriptCriteria = super.getStudentTranscriptCriteria();

                X2Criteria programParticipationCriteria = getCopyProgramParticipationCriteria();
                SubQuery programSubQuery = new SubQuery(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_STUDENT_OID, programParticipationCriteria);
                m_parentTranscriptCriteria.addIn(Transcript.COL_STUDENT_OID, programSubQuery);
            }
            return m_parentTranscriptCriteria;
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
            HomelessNew m_hData = (HomelessNew) data;
            String param = (String) field.getParameter();
            HomelessEntity hEntity = (HomelessEntity) entity;
            String rcdts = null;
            if (param.equals("H")) {
                StudentEnrollment primaryEnr = hEntity.getPrimaryEnrollment();
                String enrRcdts = null;
                if (primaryEnr != null && !StringUtils
                        .isEmpty(enrRcdts = (String) primaryEnr.getFieldValueByBeanPath(m_fieldEnrSklHome))) {
                    rcdts = lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, enrRcdts);
                }
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = hEntity.getPrimarySchoolId();
                }
            } else if (param.equals("S")) {
                Float primaryFte = null;
                Float secondaryFte = null;

                StudentSchool secondaryStdSch = hEntity.getSecondaryStudentSchool();
                if (secondaryStdSch == null) {
                    primaryFte = Float.valueOf(1);
                    secondaryFte = Float.valueOf(0);
                } else {
                    secondaryFte = getFloatByBeanPath(secondaryStdSch, m_hData.m_fieldFteOverride);
                    if (secondaryFte != null && secondaryFte.floatValue() > 0) {
                        primaryFte = Float.valueOf(1.0F - secondaryFte.floatValue());

                    } else {
                        School primarySchool = hEntity.getPrimarySchool();
                        School secondarySchool = secondaryStdSch.getSchool();
                        Collection<StudentScheduleSpan> scheduleSpans = getScheduleForReportDate(hEntity.m_student);
                        Map<School, Float> primaySecondaryFte = new HashMap<School, Float>();
                        primaySecondaryFte.put(primarySchool, Float.valueOf(0));
                        primaySecondaryFte.put(secondarySchool, Float.valueOf(0));


                        for (StudentScheduleSpan span : scheduleSpans) {
                            School spanSchool = span.getSection().getSchedule().getSchool();
                            BigDecimal credit = span.getSection().getSchoolCourse().getCourse().getCredit();
                            // count section in FTE calculation if
                            // 1)span from primary or secondary school
                            // 2)it was added on or before Report Date
                            // 3)it has exit date on or after Report Date
                            // 4)crsCredit not null and > 0
                            if (primaySecondaryFte.containsKey(spanSchool) &&
                                    credit != null && credit.compareTo(new BigDecimal(0)) > 0) {
                                /*
                                 * Get number of periods for this class per day.
                                 */
                                Integer numOfPeriods = getSectionNumOfPeriods(span.getSection().getOid());
                                Integer numOfDays = getSectionNumOfSheduleDays(span.getSection().getOid());

                                float periodsPerScheduleDay = numOfPeriods.floatValue() / numOfDays.floatValue();

                                float totalSchoolPeriodsNum = primaySecondaryFte.get(spanSchool).floatValue();
                                primaySecondaryFte.put(spanSchool,
                                        Float.valueOf(totalSchoolPeriodsNum + periodsPerScheduleDay));
                            }
                        }

                        float totalPeriodsNum = 0F;
                        for (Float periodNumbers : primaySecondaryFte.values()) {
                            totalPeriodsNum += periodNumbers.floatValue();
                        }

                        if (totalPeriodsNum == 0F || primaySecondaryFte.get(primarySchool).floatValue() == 0F ||
                                primaySecondaryFte.get(secondarySchool).floatValue() == 0F) {
                            primaryFte = Float.valueOf(1F);
                            secondaryFte = Float.valueOf(0F);
                        } else {
                            float primarySchoolFTE =
                                    primaySecondaryFte.get(primarySchool).floatValue() / totalPeriodsNum;

                            primaryFte = Float.valueOf(primarySchoolFTE);
                            secondaryFte = Float.valueOf(1.0F - primarySchoolFTE);
                        }
                    }
                }

                if (primaryFte.floatValue() > secondaryFte.floatValue()) {
                    rcdts = hEntity.getPrimarySchoolId();
                } else {
                    rcdts = hEntity.getSecondarySchoolId();
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author Follett Software Company
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "STRIPCHAR";
        private static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z -]";

        private Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
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

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_OUTPLACED_FTE_OVERRIDE = "DOE OUTPLACED FTE OVERRIDE";

    /*
     * Parameters
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_QUERY_STRING = "queryString";
    protected static final String PARAM_QUERY_BY = "queryBy";


    private static final String STATE_CODE_HMLS = "HMLS";
    /*
     * Instance variables
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected StudentHistoryHelper m_enrollmentHelper = null;
    protected String m_excludeStdField;
    protected String m_excludeSklField;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSklHome;
    protected String m_fieldFteOverride;
    protected String m_fieldSchoolId;
    protected String m_fieldServiceSchoolCode;

    protected PlainDate m_reportDate;
    protected int m_totalHomelessRecords;

    protected String m_fieldRcdtsForServingSchool;
    protected Map<String, List<StudentScheduleSpan>> m_stdScheduleSpansMap =
            new HashMap<String, List<StudentScheduleSpan>>();


    /**
     * Map of night-time residences codes by refrence code, used in retrieving the residence field
     */
    protected Map<String, ReferenceCode> m_residencesMap = null;
    private Map<String, StudentSchool> m_secondarySchoolMap = null;
    private DistrictSchoolYearContext m_schoolYearContext = null;
    protected Map<String, Integer> m_sectionNumOfPeriodsMap = null;
    protected Map<String, Integer> m_sectionNumOfSheduleDays = null;
    private StudentHistoryHelper m_scheduleHelper = null;
    private X2Criteria m_studentCriteria = null;
    X2Criteria m_programParticipationCriteria = null;



    /**
     * Gets the enrollment helper.
     *
     * @return StudentHistoryHelper in mode MODE_STUDENT_ACTIVE_ANY_TIME
     */
    public StudentHistoryHelper getEnrollmentHelper() {
        if (m_enrollmentHelper == null) {

            m_enrollmentHelper = new StudentHistoryHelper(this);
            m_enrollmentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
            m_enrollmentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_enrollmentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
        }

        return m_enrollmentHelper;
    }

    /**
     * Gets the float by bean path.
     *
     * @param bean X2BaseBean
     * @param path String
     * @return Float value from bean
     */
    public Float getFloatByBeanPath(X2BaseBean bean, String path) {
        Float value = null;
        String stringValue = (String) bean.getFieldValueByBeanPath(path);
        if (!StringUtils.isEmpty(stringValue) && StringUtils.isNumeric(stringValue)) {
            value = Float.valueOf(Float.parseFloat(stringValue));
        }
        return value;
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

        if (!getSetupErrors().isEmpty()) {
            for (StateReportValidationError error : getSetupErrors()) {
                heading.append("Setup Error - " + error.getFieldName() + "; ");
                heading.append("Message - " + error.getErrorMessage());
                heading.append("\n");
            }
        } else if (m_reportDate == null || m_fieldDistrictCode == null) {
            heading.append("Export initiation error. Please check aliases and field paths in export format definition");
            heading.append("\n");
        } else {
            heading.append("Homeless");
            heading.append(',');
            heading.append(m_totalHomelessRecords);
            heading.append(',');
            heading.append(getFileName());
            heading.append(',');
            heading.append(m_dateFormat.format(m_reportDate));
            heading.append(',');
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
            heading.append("\n");
        }
        return heading.toString();
    }


    /**
     * return primary enrollment for student on report date.
     *
     * @param studentOid String
     * @return Student enrollment
     */
    public StudentEnrollment getPrimaryEnrollment(String studentOid) {
        return getEnrollmentHelper().getEnrollmentForDate(studentOid, m_reportDate, "E");
    }


    /**
     * Gets the schedule for report date.
     *
     * @param student SisStudent
     * @return List<StudentScheduleSpan> on reprt date for student
     */
    public List<StudentScheduleSpan> getScheduleForReportDate(SisStudent student) {
        List<StudentScheduleSpan> schedules = m_stdScheduleSpansMap.get(student.getOid());
        if (schedules == null) {
            Collection<StudentScheduleSpan> schedSpans = getScheduleHelper().getStudentScheduleSpans(student);
            schedules = getScheduleSpansOnDate(schedSpans, m_reportDate);

            m_stdScheduleSpansMap.put(student.getOid(), schedules);
        }

        return schedules;
    }


    /**
     * Gets the schedule helper.
     *
     * @return StudentHistoryHelper in mode MODE_SCHEDULE_SPANS
     */
    public StudentHistoryHelper getScheduleHelper() {
        if (m_scheduleHelper == null) {

            m_scheduleHelper = new HomelessStdHistoryHelper(this);
            m_scheduleHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                    getSchoolYearContextByReportDate());
            m_scheduleHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        }

        return m_scheduleHelper;
    }

    /**
     * Gets the secondary student school.
     *
     * @param studentOid String
     * @return secondary StudentSchool for student on report date
     */
    public StudentSchool getSecondaryStudentSchool(String studentOid) {
        if (m_secondarySchoolMap == null) {
            initSecondarySchoolMap();
        }

        return m_secondarySchoolMap.get(studentOid);
    }


    /**
     * return number of periods for section.
     *
     * @param sectionOid String
     * @return Integer
     */
    public Integer getSectionNumOfPeriods(String sectionOid) {
        if (m_sectionNumOfPeriodsMap == null) {
            initializeNumOfPeriods();
        }
        Integer returnValue = m_sectionNumOfPeriodsMap.get(sectionOid);
        if (returnValue == null) {
            returnValue = Integer.valueOf(0);
        }
        return returnValue;
    }


    /**
     * Gets the section num of shedule days.
     *
     * @param sectionOid String
     * @return number of schedule days for section
     */
    public Integer getSectionNumOfSheduleDays(String sectionOid) {
        if (m_sectionNumOfSheduleDays == null) {
            initializeNumOfScheduleDays();
        }
        return m_sectionNumOfSheduleDays.get(sectionOid);
    }


    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        // initialize m_reportDate before check setup error because it used in getHeader. getHeader
        // call even export has setup error
        // and if report date is not filled - NPE on getHeader
        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        if (getSetupErrors().isEmpty()) {

            /*
             * Reference codes for row 9 - Primary Night-time Residence
             */
            // initializeResidenceMap();

            setQuery(getExportQuery());
            setEntityClass(HomelessEntity.class);


            // Build a map of calculations/retrievers.
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("HOMELESS-STD-RCDTS", new RetrieveRCDTS());
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            super.addCalcs(calcs);
        }

    }


    /**
     * Get criteria for students that were active before the report date.
     *
     * @return criteria for students that were active before report date
     */
    protected X2Criteria getCopyProgramParticipationCriteria() {
        if (m_programParticipationCriteria == null) {
            m_programParticipationCriteria = new X2Criteria();

            // try find codes with HMLS state code
            List<String> hMLScodes = getHMLScodes();
            m_programParticipationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, hMLScodes);


            // **** start
            // find program with span from context start date to report date. End date can be null
            DistrictSchoolYearContext context = getSchoolYearContextByReportDate();
            PlainDate endDate = m_reportDate;
            PlainDate startDate = context.getStartDate();

            // The student Program Start Date must be within the current calendar year. (This is
            // working as specified). However, the student program end date should not be
            // considered.
            // If the student has an end date in the program, but is still active, the record must
            // be returned.
            // 1 )If a student has a program end date, where the start date is within the current
            // school year and the student has an Active enrollment, the record should return
            // 2)-Records that have a start date outside of the current school year where end date
            // is blank/null are not considered
            // and should not be returned.
            m_programParticipationCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, startDate);
            m_programParticipationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, endDate);
            // **** end

            // include active student on date + input criteria for student
            X2Criteria studentCriteria = getCopyStudentCriteria();
            SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
            m_programParticipationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        }

        return m_programParticipationCriteria.copy();
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

        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }


    /**
     * Criteria for students. Active on date + input criteria
     *
     * @return Criteria
     */
    private X2Criteria getCopyStudentCriteria() {
        if (m_studentCriteria == null) {
            // get active student on date
            m_studentCriteria = getEnrollmentHelper().getStudentCriteria().copy();

            // Check student selection criteria user input
            String queryString = (String) getParameter(PARAM_QUERY_STRING);
            int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
            switch (queryBy) {
                case 1: // YOG
                    m_studentCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                    break;

                case 2: // LASID
                    m_studentCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                    break;

                case 3: // SASID
                    m_studentCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                    break;

                case 4: // Snapshot
                    addRecordSetCriteria(m_studentCriteria, queryString);
                    break;

                default:
                    // Take all students in the district
                    break;
            }

        }
        return m_studentCriteria.copy();
    }


    /**
     * get Query for export.
     *
     * @return Query by criteria
     */
    private QueryByCriteria getExportQuery() {
        // get student who has program participation for HMLS with correct date range
        X2Criteria programmParticipationCriteria = getCopyProgramParticipationCriteria();
        QueryByCriteria programQuery =
                new QueryByCriteria(StudentProgramParticipation.class, programmParticipationCriteria);
        return programQuery;
    }


    /**
     * Generate the filename for the report.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {

        StringBuilder fileName = new StringBuilder();
        if (m_fieldDistrictCode == null || m_reportDate == null) {
            fileName.append("initializing error");
            fileName.append("_");
        } else {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
            fileName.append("_");
            fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
            fileName.append("_");
        }
        fileName.append("001.csv");



        return fileName.toString();
    }


    /**
     * get program codes where state code is "HMLS".
     *
     * @return List
     */
    private List<String> getHMLScodes() {
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        Criteria refCodesCriteria = new X2Criteria();
        refCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        refCodesCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, STATE_CODE_HMLS);
        Collection<ReferenceCode> codes =
                getBroker().getCollectionByQuery(new QueryByCriteria(ReferenceCode.class, refCodesCriteria));
        List<String> stringCodes = new ArrayList<String>();
        for (ReferenceCode code : codes) {
            stringCodes.add(code.getCode());

        }
        return stringCodes;
    }


    /**
     * determine DistrictSchoolYearContext by date<br>
     * try find context where <code>date</code> between span start and end date context<br>
     * start and end date include in span .
     *
     * @return District school year context
     */
    private DistrictSchoolYearContext getSchoolYearContextByReportDate() {
        if (m_schoolYearContext == null) {
            Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
            criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
            QueryByCriteria byCriteria = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
            m_schoolYearContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(byCriteria);
        }

        return m_schoolYearContext;
    }

    /**
     * Gets the schedule spans on date.
     *
     * @param spans Collection<StudentScheduleSpan>
     * @param date PlainDate
     * @return collection of StudentScheduleSpan for student on passed date.
     */
    private List<StudentScheduleSpan> getScheduleSpansOnDate(Collection<StudentScheduleSpan> spans, PlainDate date) {
        ArrayList<StudentScheduleSpan> schedSpansOnDate = new ArrayList<StudentScheduleSpan>();
        for (StudentScheduleSpan span : spans) {
            if (!span.getEntryDate().after(date) &&
                    !span.getExitDate().before(date)) {
                schedSpansOnDate.add(span);
            }
        }

        return schedSpansOnDate;
    }

    /**
     * Initialize number of periods for sections.
     */
    private void initializeNumOfPeriods() {
        if (m_sectionNumOfPeriodsMap == null) {
            m_sectionNumOfPeriodsMap = new HashMap<String, Integer>();

            X2Criteria criteria = getScheduleHelper().getStudentScheduleCriteria();

            SubQuery subQuery =
                    new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            X2BaseBean.COL_OID, criteria);

            X2Criteria mstCriteria = new X2Criteria();
            mstCriteria.addIn(X2BaseBean.COL_OID, subQuery);

            String[] columns = {X2BaseBean.COL_OID, MasterSchedule.REL_MASTER_TERMS + ModelProperty.PATH_DELIMITER +
                    MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                    MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                    ScheduleMatrix.REL_SCHEDULE_PERIOD + ModelProperty.PATH_DELIMITER +
                    X2BaseBean.COL_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(MasterSchedule.class, columns, mstCriteria, true);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] item = (Object[]) iterator.next();

                    String sectionOid = (String) item[0];

                    Integer curNumOfPeriods = m_sectionNumOfPeriodsMap.get(sectionOid);
                    if (curNumOfPeriods == null) {
                        curNumOfPeriods = Integer.valueOf(0);
                    }
                    int newNumOfPeriods = curNumOfPeriods.intValue() + 1;
                    m_sectionNumOfPeriodsMap.put(sectionOid, Integer.valueOf(newNumOfPeriods));
                }
            } finally {
                iterator.close();
            }
        }
    }


    /**
     * Initialize number of schedule days for sections.
     */
    private void initializeNumOfScheduleDays() {
        if (m_sectionNumOfSheduleDays == null) {
            m_sectionNumOfSheduleDays = new HashMap<String, Integer>();

            X2Criteria ssCriteria = getScheduleHelper().getStudentScheduleCriteria();

            SubQuery ssSubQuery =
                    new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER +
                            X2BaseBean.COL_OID, ssCriteria);

            X2Criteria sscCriteria = getScheduleHelper().getStudentScheduleChangeCriteria();
            SubQuery sscSubQuery = new SubQuery(StudentScheduleChange.class, StudentScheduleChange.REL_MASTER_SCHEDULE +
                    ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID, sscCriteria);

            X2Criteria andCriteria = new X2Criteria();
            X2Criteria orCriteria = new X2Criteria();

            andCriteria.addIn(X2BaseBean.COL_OID, ssSubQuery);
            orCriteria.addIn(X2BaseBean.COL_OID, sscSubQuery);
            andCriteria.addOrCriteria(orCriteria);

            X2Criteria mstCriteria = new X2Criteria();
            mstCriteria.addAndCriteria(andCriteria);

            String[] columns = {X2BaseBean.COL_OID, MasterSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                    Schedule.REL_SCHEDULE_DAYS + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(MasterSchedule.class, columns, mstCriteria, true);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] item = (Object[]) iterator.next();

                    String sectionOid = (String) item[0];

                    Integer curNumOfScheduleDays = m_sectionNumOfSheduleDays.get(sectionOid);
                    if (curNumOfScheduleDays == null) {
                        curNumOfScheduleDays = Integer.valueOf(0);
                    }
                    int newNumOfScheduleDays = curNumOfScheduleDays.intValue() + 1;
                    m_sectionNumOfSheduleDays.put(sectionOid, Integer.valueOf(newNumOfScheduleDays));
                }
            } finally {
                iterator.close();
            }
        }
    }



    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_fieldFteOverride = translateAliasToJavaName(ALIAS_OUTPLACED_FTE_OVERRIDE, true);
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
    }



    /**
     * Initializes map of Secondary Schools.
     */
    private void initSecondarySchoolMap() {
        m_secondarySchoolMap = new HashMap<String, StudentSchool>();
        X2Criteria secondaryOutplacementCriteria = new X2Criteria();

        Criteria programCriteria = getCopyProgramParticipationCriteria();


        SubQuery programSubQuery = new SubQuery(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_STUDENT_OID, programCriteria);
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, programSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                getSchoolYearContextByReportDate().getOid());

        // limit by dates
        Criteria sumOrCriteria = new X2Criteria();
        Criteria orCriteria1 = new X2Criteria();
        orCriteria1.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);
        orCriteria1.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, m_reportDate);
        Criteria orCriteria2 = new X2Criteria();
        orCriteria2.addIsNull(StudentSchool.COL_END_DATE);
        orCriteria2.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);
        sumOrCriteria.addOrCriteria(orCriteria1);
        sumOrCriteria.addOrCriteria(orCriteria2);
        secondaryOutplacementCriteria.addAndCriteria(sumOrCriteria);

        QueryByCriteria secondaryOutplacementQuery =
                new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
        secondaryOutplacementQuery.addOrderBy(StudentSchool.COL_START_DATE, true);

        Collection<StudentSchool> secondarySchools = getBroker().getCollectionByQuery(secondaryOutplacementQuery);

        for (StudentSchool secSchool : secondarySchools) {
            m_secondarySchoolMap.put(secSchool.getStudentOid(), secSchool);
        }
    }



}
