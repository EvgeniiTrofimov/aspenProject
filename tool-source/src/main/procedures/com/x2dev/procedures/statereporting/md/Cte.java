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
package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.model.business.EnrollmentSnapshot;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: CTE Enrollment export.
 * This class implements the data export for MD CTE Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class Cte extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD CTE export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class CteEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        Cte m_cteData = null;
        List<CteProgram> m_programs = null;
        EnrollmentSnapshot m_snapshot = null;
        String m_withdrawCode = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CteEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment school/membership
         * to determine if the student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            if (m_programs == null || m_programs.size() == 0) {
                error = new StateReportValidationError(getEntityName(), "CTE Courses", "No CTE Courses found", "");
            }

            return error;
        }

        /**
         * Returns the CTE Program record for the current index.
         *
         * @return CteProgram
         */
        public CteProgram getCteProgram() {
            return m_programs.get(getCurrentRow());
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
                    ", SASID: " + student.getStateId();

            CteProgram program = getCteProgram();
            String cipCode = program.getCipCode();
            if (cipCode != null) {
                name += ", CIP: " + cipCode;
            }

            name += "]";

            return name;
        }

        /**
         * Return the enrollment snapshot that is used by some
         * fieldRetrievers to get enrollment data.
         *
         * @return the EnrollmentSnapshot for the student.
         */
        public EnrollmentSnapshot getSnapshot() {
            return m_snapshot;
        }

        /**
         * Returns the students withdraw status code and reason.
         *
         * @return String
         */
        public String getWithdrawCode() {
            return m_withdrawCode;
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

            m_cteData = (Cte) data;

            // Get enrollment snapshot on report as of date.
            SisStudent student = (SisStudent) bean;
            m_snapshot = getSnapshot(student, m_cteData.m_reportDate);

            // Build the map of CIP codes and values.
            m_programs = new ArrayList<CteProgram>();
            // Get courses for the student.
            Collection<StudentSchedule> courses = m_cteData.m_studentCourses.get(student.getOid());
            if (courses != null) {
                for (StudentSchedule schedule : courses) {
                    Course course = schedule.getSection().getSchoolCourse().getCourse();
                    String leaCourseCode = course.getNumber();
                    String cipCode = (String) course.getFieldValueByBeanPath(m_cteData.m_cteCipCodeField);
                    String concentrator = (String) course.getFieldValueByBeanPath(m_cteData.m_cteConcentratorField);
                    String isAssessment = (String) course.getFieldValueByBeanPath(m_cteData.m_assessmentField);
                    String scedCode = (String) course.getFieldValueByBeanPath(m_cteData.m_scedCourseCodeField);
                    if (!StringUtils.isEmpty(scedCode)) {
                        scedCode = data.lookupStateValue(Course.class, m_cteData.m_scedCourseCodeField, scedCode);
                    }
                    // boolean concentrator = (!StringUtils.isEmpty(isConcentrator) &&
                    // !"0".equals(isConcentrator));
                    boolean assessment = (!StringUtils.isEmpty(isAssessment) && !"0".equals(isAssessment));
                    SisSchool school = schedule.getSection().getSchoolCourse().getSchool();

                    if (!StringUtils.isEmpty(cipCode)) {
                        // Find an existing record with the CIP code.
                        CteProgram program = null;
                        /*
                         * for (CteProgram checkProgram : m_programs)
                         * {
                         * if (cipCode.equals(checkProgram.getCipCode()))
                         * {
                         * program = checkProgram;
                         * break;
                         * }
                         * }
                         */
                        /*
                         * if (program != null)
                         * {
                         * if (!program.getConcentrator() && concentrator)
                         * {
                         * program.setConcentrator(concentrator);
                         * }
                         * if (!program.getAssessment() && assessment)
                         * {
                         * program.setAssessment(assessment);
                         * }
                         * }
                         */
                        /*
                         * else
                         * {
                         */
                        program = new CteProgram(cipCode, leaCourseCode, scedCode, school, concentrator, assessment);
                        m_programs.add(program);
                        // }
                    }
                }
            }
            setRowCount(m_programs.size());

            // Look up most recent enrollment record. If it is a withdrawal, Keep the status code.
            if (m_programs.size() > 0) {
                List<StudentEnrollment> enrollments = m_cteData.m_enrollmentManager.getOrderedEnrollment(student,
                        null,
                        m_cteData.m_reportDate,
                        null,
                        false);
                if (enrollments.size() > 0) {
                    StudentEnrollment enrollment = enrollments.iterator().next();
                    {
                        if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            m_withdrawCode = enrollment.getEnrollmentCode();
                        }
                    }
                }
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

        /**
         * Returns the enrollment snapshot for the student as of the report date.
         *
         * @param student SisStudent
         * @param reportDate PlainDate
         * @return EnrollmentSnapshot
         */
        private EnrollmentSnapshot getSnapshot(SisStudent student, PlainDate reportDate) {
            EnrollmentSnapshot snapshot = new EnrollmentSnapshot(student, reportDate, getData().getBroker());

            return snapshot;
        }
    }



    /**
     * A CteProgram holds information about one students enrollment in one CTE program.
     * This includes the CIP code, concentrator flag and assessment flags.
     *
     * @author X2 Development Corporation
     */
    protected static class CteProgram {
        private boolean m_assessment;
        private String m_cipCode;
        private String m_concentrator;
        private String m_leaCourseCode;
        private String m_scedCode;
        private SisSchool m_school;

        /**
         * Construct a CteProgram object with all values.
         *
         * @param cipCode String
         * @param leaCourseCode String
         * @param scedCode String
         * @param school SisSchool
         * @param concentrator String
         * @param assessment boolean
         */
        protected CteProgram(String cipCode, String leaCourseCode, String scedCode, SisSchool school,
                String concentrator,
                boolean assessment) {
            m_cipCode = cipCode;
            m_concentrator = concentrator;
            m_school = school;
            m_assessment = assessment;
            m_leaCourseCode = leaCourseCode;
            m_scedCode = scedCode;
        }

        /**
         * Returns the assessment flag.
         *
         * @return boolean
         */
        public boolean getAssessment() {
            return m_assessment;
        }

        /**
         * Returns an indicator of whether the course is a concentrator course.
         *
         * @return boolean
         */
        public String getConcentrator() {
            return m_concentrator;
        }

        /**
         * Returns the CIP code for the students CTE academy track.
         *
         * @return String
         */
        public String getCipCode() {
            return m_cipCode;
        }

        /**
         * Returns the CIP code for the students CTE academy track.
         *
         * @return String
         */
        public String getLeaCourseCode() {
            return m_leaCourseCode;
        }

        /**
         * Gets the sced code.
         *
         * @return String
         */
        public String getScedCode() {
            return m_scedCode;
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
         * Sets the assessment flag.
         *
         * @param assessment void
         */
        public void setAssessment(boolean assessment) {
            m_assessment = assessment;
        }

        /**
         * Sets the concentrator indicator.
         *
         * @param concentrator void
         */
        public void setConcentrator(String concentrator) {
            m_concentrator = concentrator;
        }
    }

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * DOE field alias constants.
     *
     * Fields without aliases must have a bean path or retriever specified in the Field Definition.
     */

    private static final String PARAM_ENROLLMENT_STATUS = "STATUS";
    private static final String PARAM_ENROLLMENT_CODE = "CODE";

    /*
     * Reference codes for the student program code
     */
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";

    /*
     * Aliases for other table lookups.
     */
    private static final String ALIAS_STD_DOE_LUNCH = "DOE LUNCH";
    private static final String DOE_CTE_ASSESSMENT = "DOE CTE ASSESSMENT";
    private static final String DOE_CTE_CIP_CODE = "DOE CTE CIP";
    private static final String DOE_CTE_CONCENTRATOR = "DOE CTE CONCENTRATOR";
    private static final String DOE_SCED_CODE = "DOE SCED CODE";

    /*
     * Field alias/field value for querying options on the export
     */
    private static final String DOE_DISTRICT_CODE = "DOE DISTRICT CODE";
    private static final String DOE_SCHOOL_CODE = "DOE SCHOOL CODE";
    private static final String DOE_SPED_BEGIN = "DOE SPED BEGIN";
    private static final String DOE_SPED_END = "DOE SPED END";
    private static final String DOE_SPED = "DOE special ed";
    private static final String DOE_SPED_504 = "DOE SPED 504";
    private static final String DOE_SPED_TO_504 = "DOE SPED TO 504";

    /*
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_assessmentField;
    protected String m_cteCipCodeField;
    protected String m_cteConcentratorField;
    protected SimpleDateFormat m_dateFormat;
    protected SimpleDateFormat m_dateInnerFormat = new SimpleDateFormat();
    protected String m_doeStatusField;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_fieldStdLunch;
    protected TreeMap m_gradeLevelMap;
    protected TreeMap<String, String> m_gradeLevelTransMap;
    protected Pattern m_illegalNameCharacters;
    protected String m_leaId;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected PlainDate m_reportDate;
    protected Integer m_reportFormat;
    protected int m_reportPeriod;
    protected String m_scedCourseCodeField;
    protected String m_schoolCodeField;
    protected String m_spedStartDate;
    protected String m_spedEndDate;
    protected String m_sped;
    protected String m_sped504;
    protected String m_spedTo504;
    protected String m_spedStatus = "N";
    protected Map<String, Collection<StudentSchedule>> m_studentCourses;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;

    /**
     * Returns the assessment flag for the current program course.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAssessment implements FieldRetriever {

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
            Boolean value = null;
            CteEntity cteEntity = (CteEntity) entity;
            CteProgram program = cteEntity.getCteProgram();
            if (program != null) {
                value = Boolean.valueOf(program.getAssessment());
            }
            return value;
        }
    }

    /**
     * Returns the CIP code for the current entity instance.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCipCode implements FieldRetriever {

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
            String value = null;
            CteEntity cteEntity = (CteEntity) entity;
            CteProgram program = cteEntity.getCteProgram();
            if (program != null) {
                value = program.getCipCode();
            }
            return value;
        }
    }

    /**
     * Returns the concentrator designator for the student/Cip code academy.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveConcentrator implements FieldRetriever {

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
            Object value = "";
            String concentrator = "";
            SisStudent student = (SisStudent) entity.getBean();
            CteProgram cteProgram = ((CteEntity) entity).getCteProgram();
            String programCourseNo = cteProgram.getLeaCourseCode();
            // Get courses for the student.
            Collection<StudentSchedule> courses = ((Cte) data).m_studentCourses.get(student.getOid());
            if (courses != null) {
                for (StudentSchedule schedule : courses) {
                    Course course = schedule.getSection().getSchoolCourse().getCourse();
                    String courseNo = course.getNumber();
                    if (courseNo.equals(programCourseNo)) {
                        concentrator = cteProgram.getConcentrator();
                        break;
                    }
                }

                value = concentrator.equals("Y") ? "C" : "0";
            }
            return value;
        }
    }

    /**
     * Returns the state equivalent for the enrollment status, code or date of the student based on
     * the membership period.
     * The student may have an multiple membership periods. The membership period comes from the
     * entry and current index.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollmentStatus implements FieldRetriever {

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
            CteEntity emrEnt = (CteEntity) entity;
            String param = (String) field.getParameter();
            Object value = null;
            if (PARAM_ENROLLMENT_STATUS.equals(param)) {
                String code = emrEnt.getWithdrawCode();
                if (code != null && code.length() > 0) {
                    value = code.substring(0, 1);
                }
            } else if (PARAM_ENROLLMENT_CODE.equals(param)) {
                String code = emrEnt.getWithdrawCode();
                if (code != null && code.length() > 2) {
                    value = code.substring(1, 3);
                }
            }
            return value;
        }
    }

    /**
     * This finds the value in the specified field and determine if it is contained in the parameter
     * value.
     * This can be used to determine if a field contains one or more of specified values.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveFieldContains implements FieldRetriever {

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            String param = (String) field.getParameter();
            Boolean contains = Boolean.FALSE;
            if (param != null && value != null && param.contains(value)) {
                contains = Boolean.TRUE;
            }
            return contains;
        }
    }

    /**
     * DOE LUNCH to Y/N. If DOE LUNCH = D, blank, or null, return N; otherwise return Y.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveLunch implements FieldRetriever {

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
            String value = null;
            SisStudent std = (SisStudent) entity.getBean();
            String lunch = (String) std.getFieldValueByBeanPath(m_fieldStdLunch);
            String stateLunch = null;
            if (!StringUtils.isEmpty(lunch)) {
                stateLunch = data.lookupStateValue(SisStudent.class, m_fieldStdLunch, lunch);
            }
            if (StringUtils.isEmpty(stateLunch) || "N".equals(stateLunch)) {
                value = "N";
            } else {
                value = "Y";
            }
            return value;
        }
    }

    /**
     * Returns an indicator as to whether the student is participating in a program.
     *
     * The program is identified by the field.getParameter() value matching the State code value
     * of the reference code behind the program code.
     *
     * To match, the student program must have a start date before the report date
     * and the end date must be after the report date or must be empty.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {

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
            SisStudent student = (SisStudent) entity.getBean();
            PlainDate reportDate = ((Cte) data).m_reportDate;
            Collection<StudentProgramParticipation> programs = m_studentPrograms.get(student.getOid());
            Boolean inProgram = Boolean.FALSE;
            if (programs != null) {
                for (StudentProgramParticipation program : programs) {
                    String code = program.getProgramCode();
                    String alias = lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, code);
                    if (DOE_PROG_CODE_ELL.equals(alias)) {
                        if (program.getStartDate() != null &&
                                ((reportDate.after(program.getStartDate()) ||
                                        reportDate.equals(program.getStartDate())) &&
                                        (program.getEndDate() == null ||
                                                reportDate.before(program.getEndDate()) ||
                                                reportDate.before(program.getEndDate())))) {
                            inProgram = Boolean.TRUE;
                            break;
                        }
                    }
                }
            }
            return inProgram;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a string value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with one character:
     * The reference code state code value in the reference table for race codes.
     * In MD, this is:
     * "1" - Indian/Native/Alaskan
     * "2" - Asian
     * "3" - Black
     * "4" - Pacific
     * "5" - White
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

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
            String param = (String) field.getParameter();
            String yesCode = param.substring(0, 1);
            String noCode = param.substring(1, 2);
            String stateCode = param.substring(2);
            String raceCode = noCode;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());
            // Find the reference code that we are looking for.
            ReferenceCode refCode = m_raceCodes.get(stateCode);
            if (refCode != null && races != null) {
                for (Race race : races) {
                    if (refCode.getCode().equals(race.getRaceCode())) {
                        raceCode = yesCode;
                        break;
                    }
                }
            }

            return raceCode;
        }
    }

    /**
     * Returns the school code for the given student.
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            CteEntity cteEnt = (CteEntity) entity;
            CteProgram program = cteEnt.getCteProgram();
            EnrollmentSnapshot snapshot = cteEnt.getSnapshot();
            SisSchool school = null;
            if (program != null) {
                school = program.getSchool();
            }
            if (school == null) {
                school = snapshot.getSchool();
            }
            String schoolCode = "XXXX";
            if (school != null) {
                // Left pad with zeros.
                schoolCode = "0000" + getProperty(school, m_schoolCodeField);
                schoolCode = schoolCode.substring(schoolCode.length() - 4);

            } else {
                entity.addRetrievalError(field.getFieldId(),
                        new StateReportValidationError(entity, field,
                                "Could not find School Code", null));
            }

            return schoolCode;
        }
    }

    /**
     * Returns the spedStatus value .
     */
    protected class RetrieveSpedStatus implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            // String spedStatus = "N";
            /**
             * N = Student has no IEP, IFSP, nor receiving SPEd services – Student Table, Alias [DOE
             * Special ed] code is “N”
             * Y = Yes student is receiving SpEd services – Student Table, Alias [DOE Special ed]
             * code is “Y”
             * E = Exited program – Student Table Alias [DOE SPED END], date is = or < than two
             * years past, then “E” else “N”
             * 2 = student is in Section 504 - Student Table, Alias [DOE SPED 504], code is “504”
             * 3 = Exited SpEd placed in Section 504 - Student Table, Alias [DOE SPED TO 504],
             * Logical field, if checked, yes.
             *
             **/
            PlainDate spedEndDate = (PlainDate) getProperty(entity.getBean(), m_spedEndDate);
            String sped = (String) getProperty(entity.getBean(), m_sped);
            String sped504 = (String) getProperty(entity.getBean(), m_sped504);
            String spedTo504 = (String) getProperty(entity.getBean(), m_spedTo504);

            PlainDate currentSchoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
            PlainDate twoYearsInThePast = DateUtils.add(currentSchoolYearStartDate, Calendar.YEAR, -2);

            if (!StringUtils.isEmpty(sped) && sped.equals("Y")) {
                m_spedStatus = "Y";
            } else if (null != spedEndDate
                    && (spedEndDate.before(twoYearsInThePast) || spedEndDate.equals(twoYearsInThePast))) {
                m_spedStatus = "E";
            } else if (!StringUtils.isEmpty(sped504) && sped504.equals("504")) {
                m_spedStatus = "2";
            } else if (!StringUtils.isEmpty(spedTo504) && spedTo504.equals("Y")) {
                m_spedStatus = "3";
            } else {
                m_spedStatus = "N";
            }

            return m_spedStatus;
        }
    }

    /**
     * Returns the spedEndDate value with respect to spedStatus .
     */
    protected class RetrieveSpedEndDate implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PlainDate spedEndDate = null;
            if (m_spedStatus.equals("E")) {
                spedEndDate = (PlainDate) getProperty(entity.getBean(), m_spedEndDate);
            }
            return spedEndDate;
        }
    }

    /**
     * Retrieve the student SSN.
     * If one is not available, generate one from the district ID and student local Id.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSSN implements FieldRetriever {

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (StringUtils.isEmpty(value)) {
                value = "000000" + ((SisStudent) entity.getBean()).getLocalId();
                value = "9" + m_leaId + value.substring(value.length() - 6);
            } else {
                value = value.replace("-", "").replace(" ", "");
            }
            return value;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * Trim the resulting string to the field maximum length.
     * For first and last names, and middle initial.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

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
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            Boolean upper = (Boolean) field.getParameter();
            // Strip illegal characters (punctuation).
            if (value != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(value);
                value = matcher.replaceAll("");

                // Map to upper case if required.
                if (upper != null && upper.booleanValue()) {
                    value = value.toUpperCase();
                }

                // Trim to valid field length.
                if (value.length() > field.getMaxLength()) {
                    value = value.substring(0, field.getMaxLength());
                }
            } else {
                value = "";
            }
            return value;
        }
    }

    /**
     * Retrieve the ELL Indicator.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELLIndicator implements FieldRetriever {

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
            Object value = "";
            PlainDate ellStartDate = null;
            PlainDate ellEndDate = null;
            ArrayList<PlainDate> ellDates = new ArrayList();
            PlainDate reportDate = ((Cte) data).m_reportDate;
            /**
             * If no ELLStartdate or ReportDate < ELL Start Date then 'N'
             * If ReportDate >= ELLStartdate and (No ELL End date OR ReportDate <= ELL End Date then
             * 'Y'
             * If ELL End Date AND ReportDate > ELL End Date then 'E'
             *
             */
            ellDates = getELLDates(entity);
            ellStartDate = ellDates.get(0);
            ellEndDate = ellDates.get(1);
            if (null == ellStartDate || reportDate.before(ellStartDate)) {
                value = "N";
            } else if ((reportDate.after(ellStartDate) || reportDate.equals(ellStartDate))
                    && null == ellEndDate || (reportDate.before(ellEndDate) || reportDate.equals(ellEndDate))) {
                value = "Y";
            } else if (null != ellEndDate && reportDate.after(ellEndDate)) {
                value = "E";
            } else {
                // not supposed to hit this.
                value = "";
            }
            return value;
        }
    }

    /**
     * Returns the ELL Date values.
     *
     * @param entity StateReportEntity
     * @return Array list
     * @throws X2BaseException exception
     */
    static protected ArrayList<PlainDate> getELLDates(StateReportEntity entity)
            throws X2BaseException {
        PlainDate ellStartDate = null;
        PlainDate ellEndDate = null;
        ArrayList<PlainDate> ellDates = new ArrayList();

        SisStudent student = (SisStudent) entity.getBean();
        Collection<StudentEdPlan> studentEdplans = student.getStudentEdPlans();
        for (StudentEdPlan studentEdPlan : studentEdplans) {
            if (studentEdPlan.getExtendedDataDictionary().getId().equals("ELL Plan")) {
                ellStartDate = studentEdPlan.getEffectiveDate();
                ellEndDate = studentEdPlan.getEndDate();
                break;
            }
        }
        ellDates.add(ellStartDate);
        ellDates.add(ellEndDate);
        return ellDates;
    }

    /**
     * Retrieve the ELL Begin date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELLBeginDate implements FieldRetriever {

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
            Object value = "";
            ArrayList<PlainDate> ellDates = new ArrayList();
            ellDates = getELLDates(entity);
            value = ellDates.get(0);
            return value;
        }
    }

    /**
     * Retrieve the ELL End date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveELLEndDate implements FieldRetriever {

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
            Object value = "";
            ArrayList<PlainDate> ellDates = new ArrayList();
            ellDates = getELLDates(entity);
            value = ellDates.get(1);
            return value;
        }
    }

    /**
     * Returns the SCED code for the current entity instance.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveScedCode implements FieldRetriever {

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
            String value = null;
            CteEntity cteEntity = (CteEntity) entity;
            CteProgram program = cteEntity.getCteProgram();
            if (program != null) {
                value = program.getScedCode();
            }
            return value;
        }
    }

    /**
     * Retrieve the Submission Date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSubmissionDate implements FieldRetriever {

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
            PlainDate reportDate = ((Cte) data).m_reportDate;
            return reportDate;
        }
    }

    /**
     * Retrieve the LEA Course Code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveLEACourseCode implements FieldRetriever {

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
            Object value = "";
            SisStudent student = (SisStudent) entity.getBean();
            CteProgram cteProgram = ((CteEntity) entity).getCteProgram();
            String programCourseNo = cteProgram.getLeaCourseCode();
            // Get courses for the student.
            Collection<StudentSchedule> courses = ((Cte) data).m_studentCourses.get(student.getOid());
            if (courses != null) {
                for (StudentSchedule schedule : courses) {
                    String courseNo = schedule.getSection().getSchoolCourse().getCourse().getNumber();
                    if (courseNo.equals(programCourseNo)) {
                        value = courseNo;
                        break;
                    }
                }
            }
            return value;
        }
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        // Load initialization data
        initializeFields();

        /*
         * Get core parameters
         */
        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        m_leaId = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_CODE);

        /*
         * Build query object that will be used to retrieve export students.
         */
        Criteria studentCriteria = getStudentCriteria();
        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);

        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 0: // Name
                studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG
                studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                break;

            case 2: // LASID
                studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                break;

            case 3: // SASID
                studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                break;

            default:
                studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                break;
        }

        // Set the query to be used for student selection.
        setQuery(studentQuery);
        setEntityClass(CteEntity.class);

        int count = getBroker().getCount(studentQuery);

        // Load codes and support data from database.
        loadEnrollmentData(studentCriteria);

        // Load student programs map.
        loadStudentPrograms(studentCriteria, count);

        // Load student courses.
        loadStudentCourses(studentCriteria, count);

        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("CTE-ASSESSMENT", new RetrieveAssessment());
        calcs.put("CTE-CLEAN", new RetrieveStripNameChar());
        calcs.put("CTE-CIP", new RetrieveCipCode());
        calcs.put("CTE-CONCENTRATOR", new RetrieveConcentrator());
        calcs.put("CTE-CONTAINS", new RetrieveFieldContains());
        calcs.put("CTE-ELL", new RetrieveProgram());
        calcs.put("CTE-ENROLLMENT", new RetrieveEnrollmentStatus());
        calcs.put("CTE-RACE", new RetrieveRace());
        calcs.put("CTE-SSN", new RetrieveSSN());
        calcs.put("CTE-ELL-INDICATOR", new RetrieveELLIndicator());
        calcs.put("CTE-ELL-BEGIN-DATE", new RetrieveELLBeginDate());
        calcs.put("CTE-ELL-END-DATE", new RetrieveELLEndDate());
        calcs.put("CTE-SUBMISSION-DATE", new RetrieveSubmissionDate());
        calcs.put("CTE-COURSE-NO", new RetrieveLEACourseCode());
        calcs.put("CTE-SPED-STATUS", new RetrieveSpedStatus());
        calcs.put("CTE-SPED-END-DATE", new RetrieveSpedEndDate());
        calcs.put("CTE-SCED-CODE", new RetrieveScedCode());
        calcs.put("CTE-LUNCH", new RetrieveLunch());

        super.addCalcs(calcs);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */

    private Criteria getReportingCriteria() {
        /*
         * Who should be included? Primary students scheduled for CTE courses in the year.
         *
         * The export is being run for either (A) the entire district or (B) a single school
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case A:
         *
         * Students in an active, non-archived school in the district, is scheduled for a CTE course
         * in the year.
         *
         * ----------------------------------------------------------------------------------------
         *
         * Primary students, case B:
         *
         * Students is scheduled for a CTE course in the school in the year.
         *
         */

        /*
         * Primary criteria
         */
        X2Criteria reportingCriteria = new X2Criteria();

        // Get the students current primary school, Or in students with enrollment activity.
        if (!StringUtils.isEmpty(m_cteCipCodeField)) {
            X2Criteria courseCriteria = new X2Criteria();

            // Course must be a CTE course.
            courseCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_cteCipCodeField,
                    getBroker().getPersistenceKey());

            // Course is in the schools current active schedule
            courseCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    StudentSchedule.COL_SCHEDULE_OID);

            if (isSchoolContext()) {
                courseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
            }

            SubQuery courseQuery = new SubQuery(StudentSchedule.class, StudentSchedule.COL_STUDENT_OID, courseCriteria);
            courseQuery.setDistinct(true);

            reportingCriteria.addIn(X2BaseBean.COL_OID, courseQuery);
        }

        return reportingCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Sets the Java names (bean paths) for some required fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_spedStartDate = translateAliasToJavaName(DOE_SPED_BEGIN, true);
        m_spedEndDate = translateAliasToJavaName(DOE_SPED_END, true);
        m_sped = translateAliasToJavaName(DOE_SPED, true);
        m_sped504 = translateAliasToJavaName(DOE_SPED_504, true);
        m_spedTo504 = translateAliasToJavaName(DOE_SPED_TO_504, true);
        m_fieldStdLunch = translateAliasToJavaName(ALIAS_STD_DOE_LUNCH, true);
        m_assessmentField = translateAliasToJavaName(DOE_CTE_ASSESSMENT, true);
        m_cteCipCodeField = translateAliasToJavaName(DOE_CTE_CIP_CODE, true);
        m_cteConcentratorField = translateAliasToJavaName(DOE_CTE_CONCENTRATOR, true);
        m_scedCourseCodeField = translateAliasToJavaName(DOE_SCED_CODE, true);

        m_schoolCodeField = translateAliasToJavaName(DOE_SCHOOL_CODE, true);
    }

    /**
     * Prepare enrollment data required by this export.
     * Load set of non-promoted students from non-promoted snapshot.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollmentData(Criteria studentCriteria) {
        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        // Get race code reference codes for use in the race retriever.
        DataDictionaryField raceCodeField = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                .findDataDictionaryField(Race.class.getName(), Race.COL_RACE_CODE);
        if (raceCodeField != null && raceCodeField.getReferenceTableOid() != null) {
            Criteria raceCriteria = new Criteria();
            raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, raceCodeField.getReferenceTableOid());
            raceCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);
        }

        // Map of race codes by personOid.
        SubQuery subQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, studentCriteria);
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
        QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);


    }

    /**
     * Load student programs into a map by student Oid.
     *
     * @param studentCriteria Criteria
     * @param count int
     */
    private void loadStudentPrograms(Criteria studentCriteria, int count) {
        /*
         * Load student program participation records.
         */
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria programCriteria = new Criteria();
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        m_studentPrograms = getBroker().getGroupedCollectionByQuery(programQuery,
                StudentProgramParticipation.COL_STUDENT_OID, count);
    }

    /**
     * Load student courses into a map by student Oid.
     *
     * @param studentCriteria Criteria
     * @param count int
     */
    private void loadStudentCourses(Criteria studentCriteria, int count) {
        /*
         * Load student program participation records.
         */
        if (!StringUtils.isEmpty(m_cteCipCodeField)) {
            Criteria courseCriteria = new Criteria();
            // Find CTE courses scheduled for the student.
            courseCriteria.addNotNull(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_cteCipCodeField);
            // current year courses only
            courseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());

            // School specific.
            if (isSchoolContext()) {
                courseCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
            }

            // Search for selected students only.
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            courseCriteria.addIn(StudentSchedule.COL_STUDENT_OID, stdSubQuery);

            QueryByCriteria courseQuery = new QueryByCriteria(StudentSchedule.class, courseCriteria);

            m_studentCourses =
                    getBroker().getGroupedCollectionByQuery(courseQuery, StudentSchedule.COL_STUDENT_OID, count);
        }
    }
}
