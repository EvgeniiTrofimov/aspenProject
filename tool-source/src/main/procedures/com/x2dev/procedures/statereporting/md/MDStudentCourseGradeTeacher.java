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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland state report for SCGT export.
 * This class implements the data export for MD SCGT export.
 *
 * @author X2 Development Corporation
 */
public class MDStudentCourseGradeTeacher extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SCGT export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class MDStudentCourseGradeTeacherEntity extends StateReportEntity {

        /**
         * Courses the student has taken
         */
        ArrayList<ScheduleInfo> m_courses;

        /**
         * Instantiates a new student course grade entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public MDStudentCourseGradeTeacherEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize.
         *
         * Grabs all StudentSchedules and Transcripts of the student. If any of these two
         * StudentSchedule's or Transcript's MasterSchedule oid match, the Transcript overrides
         * the StudentSchedule's.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            MDStudentCourseGradeTeacher scg = (MDStudentCourseGradeTeacher) data;
            SisStudent student = (SisStudent) bean;

            /*
             * Put student schedules into the map first, THEN transcripts second
             * This is because transcript records should override the student schedule records
             */
            Map<String, ScheduleInfo> list = new HashMap<String, ScheduleInfo>();
            Collection<StudentSchedule> studentSchedules = scg.m_scheduleMap.remove(student.getOid());
            if (studentSchedules != null) {
                for (StudentSchedule studentSchedule : studentSchedules) {
                    ScheduleInfo info = new ScheduleInfo();
                    info.m_schedule = studentSchedule;
                    list.put(studentSchedule.getSectionOid(), info);
                }
            }
            Collection<Transcript> transcripts = scg.m_transcriptMap.remove(student.getOid());
            if (transcripts != null) {
                for (Transcript transcript : transcripts) {
                    ScheduleInfo info = new ScheduleInfo();
                    info.m_transcript = transcript;
                    list.put(transcript.getMasterScheduleOid(), info);
                }
            }

            m_courses = new ArrayList<MDStudentCourseGradeTeacher.ScheduleInfo>(list.values());

            setRowCount(list.size());
        }

        /**
         * Retrieve the current ScheduleInfo this entity is on.
         *
         * @return current schedule info
         */
        public ScheduleInfo getCurrentSchedule() {
            return m_courses.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = null;
            ScheduleInfo info = m_courses.get(getCurrentRow());
            SchoolCourse course = null;
            MasterSchedule section = null;
            String name = "";

            if (info.m_transcript != null) {
                name += "TRN: ";
                student = info.m_transcript.getStudent();
                course = info.m_transcript.getSchoolCourse();
                section = info.m_transcript.getMasterSchedule();
            } else if (info.m_schedule != null) {
                name += "SSC: ";
                student = info.m_schedule.getStudent();
                section = info.m_schedule.getSection();
                course = section.getSchoolCourse();
            }

            if (section != null) {
                name += section.getCourseView() + " ";
            } else if (course != null) {
                name += course.getNumber() + " ";
            }
            if (student != null) {
                name +=
                        student.getNameView() + " [LASID: " + student.getLocalId() + ", SASID: " + student.getStateId()
                                + "] ";
            }
            if (section != null) {
                name += section.getCourseView();
            } else if (course != null) {
                name += course.getNumber();
            }

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
    }

    /**
     * A container class for schedule information for one scheduled class.
     * This will contain one of a StudentSchedule or Transcript
     *
     * @author X2 Development Corporation
     */
    protected static class ScheduleInfo {
        public Transcript m_transcript;
        public StudentSchedule m_schedule;
    }

    /**
     * Remove embedded "-" characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCleanValue implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (value != null) {
                value = value.replace("-", "");
            }

            return value;
        }
    }

    /**
     * Retrieve master schedule information based from the beanpath (which is passed
     * from the calc ID).
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveMasterScheduleInfo implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            MDStudentCourseGradeTeacherEntity scge = (MDStudentCourseGradeTeacherEntity) entity;
            ScheduleInfo info = scge.getCurrentSchedule();

            boolean msFound = true;
            MasterSchedule ms = null;
            if (info.m_transcript != null) {
                ms = info.m_transcript.getMasterSchedule();
            } else if (info.m_schedule != null) {
                ms = info.m_schedule.getSection();
            }

            if (ms == null) {
                msFound = false;
            }

            Object value = null;
            if (msFound) {
                param = param.replace("[" + DOE_SUBJECT + "]", m_doeSubjectCodeField);
                param = param.replace("[" + DOE_IB + "]", m_doeIb);
                param = param.replace("[" + DOE_HSA_PREREQ + "]", m_doeHsaPreReq);
                param = param.replace("[" + DOE_CLASS_OF_RECORD + "]", m_doeClassOfRecord);
                param = param.replace("[" + DOE_CTE_CONCENTRATOR + "]", m_doeCteConcentrator);

                if (!StringUtils.isEmpty(m_doeCteAssessment) && !StringUtils.isEmpty(m_doeAdvTechEd)) {
                    param = param.replace("[" + DOE_CTE_CIP + "]", m_doeCteCip);
                    param = param.replace("[" + DOE_CTE_ASSESSMENT + "]", m_doeCteAssessment);
                    param = param.replace("[" + DOE_TECH_ED + "]", m_doeTechEd);
                    param = param.replace("[" + DOE_ADV_TECH_ED + "]", m_doeAdvTechEd);
                }

                value = getProperty(ms, param);

                if (field.getFieldId().matches("AP/Honors|MSA Subject Flag")) {
                    value = lookupStateValue(ms.getClass(), param, (String) value);
                }
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
        String m_fieldToRetreive = null;
        String m_programStateCode = null;
        boolean m_checkCutoff = false;

        /**
         * Constructor loads a map of programs by student.
         *
         * @param programStateCode The state code of the reference code for the program code.
         * @param fieldToRetreive String
         * @param subQuery A subquery of student oids to use for selecting programs into a map.
         * @param checkCutoff boolean
         */
        public RetrieveProgram(String programStateCode, String fieldToRetreive, SubQuery subQuery,
                boolean checkCutoff) {
            m_fieldToRetreive = fieldToRetreive;
            m_programStateCode = programStateCode;
            m_checkCutoff = checkCutoff;

            // Load programs for use in the retriever.
            Criteria progCriteria = new Criteria();
            progCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
            QueryByCriteria progQuery = new QueryByCriteria(StudentProgramParticipation.class, progCriteria);
            m_studentPrograms = getBroker().getGroupedCollectionByQuery(progQuery,
                    StudentProgramParticipation.COL_STUDENT_OID, 100);
        }

        /**
         * Retrieve programs that match the field parameter as a state code for the program code,
         * and
         * included in the program start/end date range.
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
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Collection<StudentProgramParticipation> programs = m_studentPrograms.get(entity.getBean().getOid());
            Object value = null;
            if (programs != null && !StringUtils.isEmpty(m_programStateCode)) {
                for (StudentProgramParticipation program : programs) {
                    String code = program.getProgramCode();
                    String alias = lookupStateValue(StudentProgramParticipation.class,
                            StudentProgramParticipation.COL_PROGRAM_CODE, code);
                    if (m_programStateCode.equals(alias)) {
                        if (!StringUtils.isEmpty(m_fieldToRetreive)) {
                            value = getProperty(program, m_fieldToRetreive);
                        } else if (program.getStartDate() != null &&
                                ((m_reportDate.after(program.getStartDate()) ||
                                        m_reportDate.equals(program.getStartDate())) &&
                                        (program.getEndDate() == null ||
                                                m_reportDate.before(program.getEndDate()) ||
                                                m_reportDate.equals(program.getEndDate())))) {

                            value = "Y";
                        } else if (m_checkCutoff && program.getEndDate() != null
                                && m_twoYearCutoff.before(program.getEndDate())) {
                            value = "E";
                        }
                        break;
                    }
                }
            }

            return value;
        }
    }


    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a string value
     * indicating the presence of the race code record.
     *
     * The parameter input can represent one or more race indicators to display.
     * Each separate race indicator should be separated by a semi-colon (;)
     *
     * Each race indicator should have three parts separated by a comma:
     * 1. true value, the value to output if the race code is present.
     * 2. false value, the value to output if the race code is not present.
     * 3. The state code value in the reference code for the race code in the person_race table.
     *
     * EX: param = 1,0,1;2,0,2;3,0,3;4,0,4;5,0,5
     *
     * The expected output would be five parts with vaue or zero in each position.
     * 10040
     * 00300
     *
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
        private Map<String, Collection<Race>> m_raceCodeMap = new HashMap<String, Collection<Race>>();
        private Map<String, ReferenceCode> m_raceCodes;

        /**
         * Constructor.
         * Lookup race codes reference codes.
         */
        public RetrieveRace() {
            // Get race code reference codes for use in the race retriever.
            DataDictionaryField raceCodeField =
                    DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                            Race.class.getName(), Race.COL_RACE_CODE);
            if (raceCodeField != null && raceCodeField.getReferenceTableOid() != null) {
                Criteria raceCriteria = new Criteria();
                raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, raceCodeField.getReferenceTableOid());
                raceCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
                m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);
            }
        }

        /**
         * Perform the race lookup based on the personOid provided.
         * This allows alternate person source for the teacher retriever.
         *
         * @param personOid String
         * @param param String
         * @return Object
         */
        protected Object lookupCodes(String personOid, String param) {
            String raceCode = "";

            if (!StringUtils.isEmpty(personOid)) {
                String[] raceIndList = param.split(";");

                // Parse the parameter to determine what to look up.
                for (String raceInd : raceIndList) {
                    String[] raceIndParts = raceInd.split(",");
                    if (raceIndParts.length >= 3) {
                        String yesCode = raceIndParts[0];
                        String noCode = raceIndParts[1];
                        String stateCode = raceIndParts[2];
                        String appender = noCode;

                        Collection<Race> races = getRaces(personOid, getBroker());

                        // Find the reference code that we are looking for.
                        ReferenceCode refCode = m_raceCodes.get(stateCode);
                        if (refCode != null && races != null) {
                            for (Race race : races) {
                                if (refCode.getCode().equals(race.getRaceCode())) {
                                    appender = yesCode;
                                    break;
                                }
                            }
                        }
                        raceCode += appender;
                    }
                }
            }

            return raceCode;
        }

        /**
         * Lookup race codes for a person and generate race codes output.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            // Get the person oid from the field specification.
            String personOid = (String) getProperty(entity.getBean(), field.getBeanPath());
            return lookupCodes(personOid, (String) field.getParameter());
        }

        /**
         * Lookup the races collection for the given personOid. Use a local cache
         * for repeats.
         *
         * @param personOid String
         * @param broker X2Broker
         * @return Collection
         */
        private Collection<Race> getRaces(String personOid, X2Broker broker) {
            Collection<Race> races = null;
            if (!m_raceCodeMap.containsKey(personOid)) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(Race.COL_PERSON_OID, personOid);
                QueryByCriteria query = new QueryByCriteria(Race.class, criteria);
                races = broker.getCollectionByQuery(query);
                m_raceCodeMap.put(personOid, races);
            }
            races = m_raceCodeMap.get(personOid);
            return races;
        }
    }

    /**
     * Returns the sped placement info value. If the "retrieve sped values" input parameter
     * was selected, the following logic is used to derive the return value:
     * <p>
     * <table border="1">
     * <tr>
     * <th>Returned state code equivalent</th>
     * <th>Circumstance</th>
     * </tr>
     * <tr>
     * <td>N</td>
     * <td>Special education status is not active and student did not exit special education during
     * the last two school years</td>
     * </tr>
     * <tr>
     * <td>Y</td>
     * <td>Special education status is active</td>
     * </tr>
     * <tr>
     * <td>E</td>
     * <td>Special education status is not active, the student exited special education during the
     * last two school years</td>
     * </tr>
     * <tr>
     * <td>2</td>
     * <td>Special education status is not active, the student is in 504</td>
     * </tr>
     * <tr>
     * <td>3</td>
     * <td>Special education status is exited, the student is in 504</td>
     * </tr>
     * </table>
     *
     */
    protected class RetrieveSped implements FieldRetriever {
        private String m_sped504Alias = null;
        private String m_spedStatusAlias = null;
        private String m_spedTo504Alias = null;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = "N";

            // Get the transcript/studentschedule bean
            MDStudentCourseGradeTeacherEntity scge = (MDStudentCourseGradeTeacherEntity) entity;
            ScheduleInfo info = scge.getCurrentSchedule();
            X2BaseBean bean = null;
            if (info.m_transcript != null) {
                bean = info.m_transcript;

            } else if (info.m_schedule != null) {
                bean = info.m_schedule;
            }

            PlainDate spedEndDate =
                    (PlainDate) getProperty(bean, Transcript.REL_STUDENT + PATH_DELIMITER
                            + SisStudent.COL_SPED_EXIT_DATE);

            // Check Alleganey custom IEP/504 alias.
            if (m_sped504Alias == null || m_spedStatusAlias == null || m_spedTo504Alias == null) {
                m_sped504Alias = translateAliasToJavaName("DOE SPED 504", false);
                m_spedStatusAlias = translateAliasToJavaName("DOE special ed", false);
                m_spedTo504Alias = translateAliasToJavaName("DOE SPED TO 504", false);
                if (m_sped504Alias == null || m_spedStatusAlias == null || m_spedTo504Alias == null) {
                    m_sped504Alias = "";
                    m_spedStatusAlias = "";
                    m_spedTo504Alias = "";
                }
            }

            if (StringUtils.isEmpty(m_sped504Alias) || StringUtils.isEmpty(m_spedStatusAlias)) {
                // Standard sped and 504 status fields.
                // Get 504 and sped status and dates from student fields.
                String spedStatusCode =
                        (String) getProperty(bean, Transcript.REL_STUDENT + PATH_DELIMITER
                                + SisStudent.COL_SPED_STATUS_CODE);
                String status504Code =
                        (String) getProperty(bean, Transcript.REL_STUDENT + PATH_DELIMITER
                                + SisStudent.COL_SECTION504_STATUS_CODE);

                if (m_spedActiveCode.equals(spedStatusCode)) {
                    value = "Y";
                } else if (spedEndDate != null
                        && (spedEndDate.before(m_reportDate) || spedEndDate.equals(m_reportDate))) {
                    // Check exited sped status, in 504 program.
                    if (m_spedActiveCode.equals(status504Code)) {
                        value = "3";
                    } else {
                        value = "E";
                    }
                } else if (m_spedActiveCode.equals(status504Code)) {
                    value = "2";
                }
            } else {
                // Alleganey uses alternate sped and 504 status field.
                String sped504Value =
                        (String) getProperty(bean, Transcript.REL_STUDENT + PATH_DELIMITER + m_sped504Alias);
                String spedStatusValue =
                        (String) getProperty(bean, Transcript.REL_STUDENT + PATH_DELIMITER + m_spedStatusAlias);
                String spedTo504Value =
                        (String) getProperty(bean, Transcript.REL_STUDENT + PATH_DELIMITER + m_spedTo504Alias);

                if ("Y".equals(spedStatusValue)) {
                    value = "Y";
                } else if (spedEndDate != null && spedEndDate.after(m_reportDate)) {
                    value = "Y"; // Exited after report date, report as still in Sped
                } else if ("1".equals(spedTo504Value)) {
                    value = "3";
                } else if ("504".equals(sped504Value)) {
                    value = "2";
                } else if (spedEndDate != null
                        && (spedEndDate.before(m_reportDate) || spedEndDate.equals(m_reportDate))) {
                    value = "E";
                } else {
                    value = "N";
                }
            }

            return value;
        }
    }

    /**
     * Retriever to return sped exit date.
     * If sped status is an E or a 3, then return date, if not it should be zeros since this is a
     * numeric field.
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    protected class RetrieveSpedEndDate implements FieldRetriever {

        private static final String FIELD_SPED_STATUS = "SPED Status";
        private final List<String> VALID_STATUS = Arrays.asList("E", "3");

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            SisStudent student = (SisStudent) entity.getBean();
            String spedStatus = entity.getFieldValue(FIELD_SPED_STATUS);
            if (!StringUtils.isEmpty(spedStatus) && VALID_STATUS.contains(spedStatus)) {
                value = student.getSpedExitDate();
            }

            return value;
        }
    }

    /**
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscriptInfo implements FieldRetriever {
        /**
         * Values for field calculation parameters. Grade related values.
         */
        private static final String PARAM_COMPLETION_STATUS = "COMPLETION_STATUS";
        private static final String PARAM_COURSE_LEVEL = "COURSE_LEVEL";
        private static final String PARAM_EARNED_CRS_CERT = "EARNED_CERT";
        private static final String PARAM_GRADE_ALPHA = "GRADE_ALPHA";
        private static final String PARAM_GRADE_NON_TRAD = "GRADE_NON_TRAD";
        private static final String PARAM_GRADE_MIN = "GRADE_MIN";
        private static final String PARAM_GRADE_MAX = "GRADE_MAX";
        private static final String PARAM_GPE = "GPE";
        private static final String PARAM_TRADITIONAL_COURSE_FLAG = "TRADITIONAL-CRS-FLAG";

        private static final String COMPLETION_STATUS_PASS = "P";
        private static final String COMPLETION_STATUS_IN_PROGRESS = "IP";
        private static final String COMPLETION_STATUS_NOT_STARTED = "NS";
        private static final String COMPLETION_STATUS_WITHDRAWAL = "WD";

        private static final String FINAL_GRADE_DATA_FIELD = "trnFinalGrade";
        /**
         * GradesManager object for handling all grade lookup tasks.
         */
        private GradesManager m_gradesManager = null;
        private Map<String, Boolean> m_isFinalGradeCollected = new HashMap();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            if (m_gradesManager == null) {
                m_gradesManager = new GradesManager(data.getBroker());
            }
            Object value = null;

            String parameter = (String) field.getParameter();

            MDStudentCourseGradeTeacherEntity scge = (MDStudentCourseGradeTeacherEntity) entity;
            SisStudent student = (SisStudent) scge.getBean();
            MDStudentCourseGradeTeacher scgData = (MDStudentCourseGradeTeacher) data;
            ScheduleInfo info = scge.getCurrentSchedule();
            if (info.m_transcript != null) {
                Transcript transcript = info.m_transcript;
                String finalGrade = transcript.getFinalGrade();
                GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                // Find the grade scale definition for the grade, if possible.
                GradeScaleGradeDefinition gradeDef = null;
                BigDecimal numericGrade = null;
                if (scale != null && !StringUtils.isEmpty(finalGrade)) {
                    gradeDef = m_gradesManager.getGradeDefinition(finalGrade, scale,
                            transcript.getSchoolOid(), transcript.getSchoolCourseOid());
                    if (gradeDef == null) {
                        try {
                            numericGrade = new BigDecimal(finalGrade);
                            gradeDef = m_gradesManager.getGradeDefinition(numericGrade, scale,
                                    transcript.getSchoolOid(), transcript.getSchoolCourseOid());
                        } catch (NumberFormatException nfe) {
                            // final grade is not numeric.
                        }
                    }
                }

                if (PARAM_COMPLETION_STATUS.equals(parameter)) {
                    // Determine whether course is in progress, not yet completed, or completed but
                    // not taken for credit.

                    MasterSchedule masterSchedule = transcript.getMasterSchedule();

                    if (masterSchedule != null) {
                        Collection<ScheduleTermDate> scheduleTermDates =
                                scgData.getScheduleTermDates(masterSchedule.getScheduleTermOid());

                        PlainDate courseStartDate = null;
                        PlainDate courseEndDate = null;
                        for (ScheduleTermDate date : scheduleTermDates) {
                            PlainDate currentStartDate = date.getStartDate();
                            if (courseStartDate == null
                                    || (currentStartDate != null && currentStartDate.before(courseStartDate))) {
                                courseStartDate = currentStartDate;
                            }
                            PlainDate currentEndDate = date.getEndDate();
                            if (courseEndDate == null
                                    || (currentEndDate != null && currentEndDate.before(courseEndDate))) {
                                courseEndDate = currentEndDate;
                            }
                        }
                        if (m_reportDate != null && courseStartDate != null && courseEndDate != null
                                && !courseStartDate.after(m_reportDate) && !m_reportDate.after(courseEndDate)) {
                            value = COMPLETION_STATUS_IN_PROGRESS;
                        } else if (m_reportDate != null && courseStartDate != null
                                && m_reportDate.before(courseStartDate)) {
                            value = COMPLETION_STATUS_NOT_STARTED;
                        } else if (m_reportDate != null && courseEndDate != null && m_reportDate.after(courseEndDate)) {
                            ArrayList<StudentEnrollment> enrs = scgData.getStudentEnrollments(student.getOid());

                            if (isFinalGradeCollected(transcript)) {
                                /*
                                 * Look up reference code for the grade scale grade represented by
                                 * the final grade.
                                 * The local code in the reference code represents the completion
                                 * status, unless the
                                 * course is still in progress, not yet completed or not taken for
                                 * credit but is complete
                                 */
                                String localCode = null;
                                if (gradeDef != null) {
                                    ReferenceCode refCode = m_gradeCodes.get(gradeDef.getGradeCode());
                                    if (refCode != null) {
                                        localCode = refCode.getLocalCode();
                                    }
                                } else {
                                    ReferenceCode refCode = m_gradeCodes.get(finalGrade);
                                    if (refCode != null) {
                                        localCode = refCode.getLocalCode();
                                    }
                                }

                                value = localCode;
                            } else if (isEnrolledOnLastSectionDate(enrs, courseEndDate)) {
                                value = COMPLETION_STATUS_PASS;
                            } else {
                                value = COMPLETION_STATUS_WITHDRAWAL;
                            }
                        }
                    }
                } else if (PARAM_COMPLETION_STATUS.equals(parameter)) {
                    value = COMPLETION_STATUS_WITHDRAWAL;
                } else if (PARAM_GRADE_ALPHA.equals(parameter)) {
                    if (gradeDef != null) {
                        ReferenceCode refCode = m_gradeCodes.get(gradeDef.getGradeCode());
                        if (refCode != null) {
                            value = refCode.getStateCode();
                        } else {
                            value = gradeDef.getGradeCode();
                        }
                    } else {
                        ReferenceCode refCode = m_gradeCodes.get(finalGrade);
                        if (refCode != null) {
                            value = refCode.getStateCode();
                        } else {
                            value = finalGrade;
                        }
                    }
                } else if (PARAM_GRADE_NON_TRAD.equals(parameter)) {
                    // If a grade scale grade is non-numeric, then look up the reference code for
                    // the
                    // grade code and retrieve the description.
                    value = "NA"; // No non-traditional grade.
                    if (gradeDef != null && gradeDef.getNoNumericIndicator()) {
                        ReferenceCode refCode = m_gradeCodes.get(gradeDef.getGradeCode());
                        if (refCode != null) {
                            value = refCode.getDescription();
                        }
                    }
                } else if (PARAM_GRADE_MIN.equals(parameter)) {
                    if (numericGrade != null) {
                        value = numericGrade;
                    } else if (gradeDef != null) {
                        if (!gradeDef.isNoNumericIndicatorDirty()) {
                            value = gradeDef.getGradeCutoffValue();
                        }
                    }
                } else if (PARAM_GRADE_MAX.equals(parameter)) {
                    if (numericGrade != null) {
                        value = numericGrade;
                    } else if (gradeDef != null) {
                        BigDecimal nextHigher = null;
                        if (!gradeDef.isNoNumericIndicatorDirty()) {
                            // Find the next higher grade definition.
                            for (GradeScaleGradeDefinition def2 : scale.getGradeScaleDefinitions(getBroker())) {
                                if (!def2.isNoNumericIndicatorDirty()
                                        && def2.getGradeCutoffValue().compareTo(gradeDef.getGradeCutoffValue()) > 0
                                        && (nextHigher == null
                                                || def2.getGradeCutoffValue().compareTo(nextHigher) < 0)) {
                                    nextHigher = def2.getGradeCutoffValue();
                                }
                            }
                            if (nextHigher == null) {
                                nextHigher = scale.getMaximumPoints();
                            } else {
                                // Subtract one from the next higher cutoff to avoid range overlap.
                                nextHigher = nextHigher.subtract(BigDecimal.valueOf(0.1));
                            }
                        }
                        value = nextHigher;
                    }
                } else if (PARAM_GPE.equals(parameter)) {
                    SchoolCourse course = transcript.getSchoolCourse();
                    if (gradeDef != null && course != null) {
                        Collection<GradeScalePoints> points = m_gradePoints.get(gradeDef.getOid());
                        if (points != null) {
                            for (GradeScalePoints point : points) {
                                if (course.getAcademicLevel().equals(point.getAcademicLevel())) {
                                    value = point.getGradePoints();
                                    break;
                                }
                            }
                        }
                    }
                } else if (PARAM_COURSE_LEVEL.equals(parameter)) {
                    value = "N";
                    MasterSchedule masterSchedule = transcript.getMasterSchedule();
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                String courseLevelCode = (String) course.getFieldValueByBeanPath(m_doeTransitionCourse);

                                if (!StringUtils.isEmpty(courseLevelCode)) {
                                    courseLevelCode =
                                            lookupStateValue(Course.class, m_doeTransitionCourse, courseLevelCode);
                                    if (!StringUtils.isEmpty(courseLevelCode)) {
                                        value = courseLevelCode;
                                    }
                                }
                            }
                        }
                    }
                } else if (PARAM_EARNED_CRS_CERT.equals(parameter)) {
                    value =
                            BooleanAsStringConverter.TRUE.equals(transcript.getFieldValueByBeanPath(m_doeTrnAsmPassed))
                                    ? "Y"
                                    : "N";
                } else if (PARAM_TRADITIONAL_COURSE_FLAG.equals(parameter)) {
                    value = BooleanAsStringConverter.FALSE;
                    MasterSchedule masterSchedule = transcript.getMasterSchedule();
                    if (masterSchedule != null) {
                        SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                value = course.getFieldValueByBeanPath(m_doeTraditionalCourseFlag);
                            }
                        }
                    }
                } else {
                    // bean path
                    value = transcript.getFieldValueByBeanPath(parameter);
                }
            } else if (info.m_schedule != null) {
                if (PARAM_COMPLETION_STATUS.equals(parameter)) {
                    // Determine whether course is in progress, not yet completed, or completed but
                    // not taken for credit.

                    MasterSchedule masterSchedule = info.m_schedule.getSection();

                    if (masterSchedule != null) {
                        Collection<ScheduleTermDate> scheduleTermDates =
                                scgData.getScheduleTermDates(masterSchedule.getScheduleTermOid());
                        PlainDate courseStartDate = null;
                        PlainDate courseEndDate = null;
                        for (ScheduleTermDate date : scheduleTermDates) {
                            PlainDate currentStartDate = date.getStartDate();
                            if (courseStartDate == null
                                    || (currentStartDate != null && currentStartDate.before(courseStartDate))) {
                                courseStartDate = currentStartDate;
                            }
                            PlainDate currentEndDate = date.getEndDate();
                            if (courseEndDate == null
                                    || (currentEndDate != null && currentEndDate.before(courseEndDate))) {
                                courseEndDate = currentEndDate;
                            }
                        }
                        if (m_reportDate != null && courseStartDate != null && courseEndDate != null
                                && !courseStartDate.after(m_reportDate) && !m_reportDate.after(courseEndDate)) {
                            value = COMPLETION_STATUS_IN_PROGRESS;
                        } else if (m_reportDate != null && courseStartDate != null
                                && m_reportDate.before(courseStartDate)) {
                            value = COMPLETION_STATUS_NOT_STARTED;
                        } else if (m_reportDate != null && courseEndDate != null && m_reportDate.after(courseEndDate)) {
                            value = COMPLETION_STATUS_WITHDRAWAL;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Retrieve if student is enrolled in given date.
         *
         * @param enrs Collection
         * @param lastSectiondate PlainDate
         * @return true, if is enrolled on last section date
         */
        private boolean isEnrolledOnLastSectionDate(ArrayList<StudentEnrollment> enrs, PlainDate lastSectiondate) {
            boolean isEnrolled = false;

            if (!enrs.isEmpty()) {
                StudentEnrollment enr = enrs.get(0);

                if (enr != null && lastSectiondate.equals(enr.getEnrollmentDate())
                        && "E".equals(enr.getEnrollmentCode())) {
                    isEnrolled = true;
                }
            }
            return isEnrolled;
        }

        /**
         * "Is final grade collected" is determined by examining the transcript column definition
         * for the final grade column and inspecting the collection type.
         *
         * @param trn Transcript
         * @return true, if is final grade collected
         */
        private boolean isFinalGradeCollected(Transcript trn) {
            boolean isFinalGradeCollected = false;
            if (m_isFinalGradeCollected.containsKey(trn.getTranscriptDefinitionOid())) {
                isFinalGradeCollected = m_isFinalGradeCollected.get(trn.getTranscriptDefinitionOid()).booleanValue();
            } else {
                Collection<TranscriptColumnDefinition> gtcCollection =
                        trn.getTranscriptDefinition().getTranscriptColumnDefinitions();

                for (TranscriptColumnDefinition gtc : gtcCollection) {
                    if (FINAL_GRADE_DATA_FIELD.equals(gtc.getDataFieldConfig().getDataFieldOid())) {
                        isFinalGradeCollected = TranscriptColumnCollectionType.NONE.value() != gtc.getCollectionType();
                        break;
                    }
                }
                m_isFinalGradeCollected.put(trn.getTranscriptDefinitionOid(), isFinalGradeCollected ? Boolean.TRUE
                        : Boolean.FALSE);
            }


            return isFinalGradeCollected;
        }

    }

    /**
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacherInfo implements FieldRetriever {

        /**
         * Values for field calculation parameters. Course related values.
         */
        private static final String PARAM_MULTIPLE_TEACHER = "MULTIPLE_TEACHER";
        private static final String PARAM_SATID = "SATID";
        private static final String PARAM_LATID = "LATID";
        private static final String PARAM_LAST_NAME = "LAST_NAME";
        private static final String PARAM_FIRST_NAME = "FIRST_NAME";
        private static final String PARAM_MIDDLE_NAME = "MIDDLE_NAME";
        private static final String PARAM_NAME_SUFFIX = "NAME_SUFFIX";
        private static final String PARAM_BCLN = "BCLN";
        private static final String PARAM_DOB = "DOB";
        private static final String PARAM_GENDER = "GENDER";
        private static final String PARAM_RACE = "RACE";
        private static final String PARAM_ETHNICITY = "ETHNICITY";

        protected RetrieveRace m_retrieveRace;

        /**
         * @param m_retrieveRace
         */
        protected RetrieveTeacherInfo(RetrieveRace retrieveRace) {
            m_retrieveRace = retrieveRace;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            MDStudentCourseGradeTeacherEntity scge = (MDStudentCourseGradeTeacherEntity) entity;
            ScheduleInfo info = scge.getCurrentSchedule();
            Object value = null;

            MasterSchedule section = null;
            if (info.m_transcript != null) {
                section = info.m_transcript.getMasterSchedule();
            } else if (info.m_schedule != null) {
                section = info.m_schedule.getSection();
            }

            String param = (String) field.getParameter();

            // Need the section for anything.
            if (section != null) {
                if (PARAM_MULTIPLE_TEACHER.equals(param)) {
                    // Check count of teachers.
                    Collection<ScheduleTeacher> teachers = m_teacherSchedules.get(section.getOid());
                    value = Boolean.valueOf(teachers != null && teachers.size() > 1);
                } else {
                    // Specific teacher information.
                    Staff staff = null;

                    char posChar = param.charAt(param.length() - 1);
                    param = param.substring(0, param.length() - 1);

                    // First teacher is primary.
                    if (posChar == '1') {
                        staff = section.getPrimaryStaff();
                    } else {
                        // second and third are in teacher schedule collection and not primary.
                        int stfPos = (posChar == '2' ? 1 : 2);
                        int curPos = 0;
                        Collection<ScheduleTeacher> teachers = m_teacherSchedules.get(section.getOid());
                        if (teachers != null) {
                            for (ScheduleTeacher teacher : teachers) {
                                if (!teacher.getPrimaryTeacherIndicator()) {
                                    curPos++;
                                    if (curPos == stfPos) {
                                        staff = teacher.getStaff();
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if (staff != null) {
                        if (PARAM_SATID.equals(param)) {
                            value = staff.getStateId();
                        } else if (PARAM_LATID.equals(param)) {
                            value = staff.getLocalId();
                        } else if (PARAM_LAST_NAME.equals(param)) {
                            value = staff.getPerson().getLastName();
                        } else if (PARAM_FIRST_NAME.equals(param)) {
                            value = staff.getPerson().getFirstName();
                        } else if (PARAM_MIDDLE_NAME.equals(param)) {
                            value = staff.getPerson().getMiddleName();
                        } else if (PARAM_NAME_SUFFIX.equals(param)) {
                            value = staff.getPerson().getNameSuffixCode();
                        } else if (PARAM_BCLN.equals(param)) {
                            value = staff.getPerson().getFieldValueByAlias("DOE ORIGINAL LAST NAME");
                        } else if (PARAM_DOB.equals(param)) {
                            value = staff.getPerson().getDob();
                        } else if (PARAM_GENDER.equals(param)) {
                            value =
                                    data.lookupStateValue(SisPerson.class, SisPerson.COL_GENDER_CODE, staff.getPerson()
                                            .getGenderCode());
                        } else if (PARAM_RACE.equals(param)) {
                            value = m_retrieveRace.lookupCodes(staff.getPersonOid(), "1,0,1;2,0,2;3,0,3;4,0,4;5,0,5");
                        } else if (PARAM_ETHNICITY.equals(param)) {
                            value = Boolean.valueOf(staff.getPerson().getHispanicLatinoIndicator());
                        }
                    }
                }
            } else if (PARAM_MULTIPLE_TEACHER.equals(param)) {
                // default to false if no info, to save some potential missing data issues.
                value = Boolean.FALSE;
            }
            return value;
        }
    }

    /**
     * Returns term code and other information from the section in the transcript.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermInfo implements FieldRetriever {
        private static final String PARAM_TERM_CODE = "TERM_CODE";
        private static final String PARAM_PERIOD = "PERIOD";

        private Map<String, String> m_baseTermMap = null;

        /**
         * Constructor.
         * Initialize term code map.
         */
        public RetrieveTermInfo() {
            m_baseTermMap = new HashMap<String, String>();
            m_baseTermMap.put("1", "SY");
            m_baseTermMap.put("10", "1S");
            m_baseTermMap.put("01", "2S");
            m_baseTermMap.put("100", "1T");
            m_baseTermMap.put("010", "2T");
            m_baseTermMap.put("001", "3T");
            m_baseTermMap.put("1000", "1Q");
            m_baseTermMap.put("0100", "2Q");
            m_baseTermMap.put("0010", "3Q");
            m_baseTermMap.put("0001", "4Q");
        }

        /**
         * Looks up term information from the section term.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            ScheduleTerm term = null;
            String param = (String) field.getParameter();
            MDStudentCourseGradeTeacherEntity scge = (MDStudentCourseGradeTeacherEntity) entity;
            ScheduleInfo info = scge.getCurrentSchedule();

            MasterSchedule section = null;
            String termCode = null;
            if (info.m_transcript != null) {
                section = info.m_transcript.getMasterSchedule();
                termCode = info.m_transcript.getTermCode();
            } else if (info.m_schedule != null) {
                section = info.m_schedule.getSection();
                termCode = info.m_schedule.getTermView();
            }

            if (section != null) {
                if (PARAM_TERM_CODE.equals(param)) {
                    term = section.getScheduleTerm();
                    if (term != null) {
                        String baseTermMap = term.getBaseTermMap();
                        if (baseTermMap != null) {
                            value = m_baseTermMap.get(baseTermMap);
                        }
                        if (value == null) {
                            if (baseTermMap != null && baseTermMap.length() > 4) {
                                value = "M";
                            } else {
                                value = "O";
                            }
                        }
                    }
                } else if (PARAM_PERIOD.equals(param)) {
                    // Lookup period as first character of the schedule display.
                    String scheduleView = section.getScheduleDisplay();
                    if (!StringUtils.isEmpty(scheduleView)) {
                        value = scheduleView.substring(0, 1);
                    }
                }
            } else if (PARAM_TERM_CODE.equals(param)) {
                // No section.
                // Alleganey has some term info in older transcript records with no schedule.
                if ("Y".equals(termCode)) {
                    value = "SY";
                } else if ("F".equals(termCode)) {
                    value = "1S";
                } else if ("S".equals(termCode)) {
                    value = "2S";
                }
            }

            return value;
        }
    }

    /**
     * Validate Aplha Grade
     */
    public class ValidateAlphaGrade implements FieldValidator {

        private static final String EXPORT_FIELD_COMPL_STATUS = "LEA Course Code";
        private static final String VAL_ID = "VAL_ALPHA";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();

            String completionStatus = entity.getFieldValue(EXPORT_FIELD_COMPL_STATUS);

            if (Arrays.asList("P", "F").contains(completionStatus) && StringUtils.isEmpty(value)) {
                String stdLasid = ((SisStudent) entity.getBean()).getLocalId();
                errors.add(new StateReportValidationError("Student LASID = " + stdLasid, field.getFieldId(),
                        "Missing value", "Field should not be empty if " + EXPORT_FIELD_COMPL_STATUS
                                + " is \"P\" or \"F\""));
            }
            return errors;
        }
    }

    /**
     * Validate Course Code.
     */
    public class ValidateCourseCode implements FieldValidator {

        private static final String EXPORT_FIELD_CRS_CODE = "LEA Course Code";
        private static final String VAL_ID = "VAL_CRS_CODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();

            String crsCode = entity.getFieldValue(EXPORT_FIELD_CRS_CODE);

            if ("N".equals(value) && StringUtils.isEmpty(crsCode)) {
                String stdLasid = ((SisStudent) entity.getBean()).getLocalId();
                errors.add(new StateReportValidationError("Student with LASID = " + stdLasid, EXPORT_FIELD_CRS_CODE,
                        "Missing value", EXPORT_FIELD_CRS_CODE + " should not be empty if " + field.getFieldId()
                                + " is N."));
            }
            return errors;
        }
    }

    /**
     * Validate Secondary Teacher if exist
     */
    public class ValidateSecondaryTeacher implements FieldValidator {
        private final List<String> OPTIONAL_FIELD_PARAMETERS = Arrays.asList("BCLN2", "NAME_SUFFIX2");
        private static final String EXPORT_FIELD_MULT_TEACHER = "Multiple Teacher";
        private static final String VAL_ID = "VAL_SEC_TEACHER";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            String multiplyTeacher = entity.getFieldValue(EXPORT_FIELD_MULT_TEACHER);

            if ("Y".equals(multiplyTeacher) && StringUtils.isEmpty(value)
                    && !OPTIONAL_FIELD_PARAMETERS.contains(field.getParameter())) {
                errors.add(new StateReportValidationError(entity, field, "Missing value", field.getFieldId()
                        + " should not be empty if secondary teacher exists."));
            }
            return errors;
        }
    }

    /**
     * Name for "Two year cutoff date" parameter. This indicates the two year period for SPED, ELL
     * exit.
     */
    public static final String TWO_YEAR_CUTOFF_PARAM = "twoYearCutoff";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Private constants.
     * Aliases.
     */
    private static final String ALIAS_TRN_ASM_PASSED = "all-trn-TechnicalAssessmentPassed";
    private static final String ALIAS_TRANSITION_COURSE = "all-crs-TransitionCourse";
    private static final String DOE_CLASS_OF_RECORD = "DOE CLASS_OF_RECORD";
    private static final String DOE_HSA_PREREQ = "DOE HSA PREREQ";
    private static final String DOE_IB = "DOE IB";
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    private static final String DOE_SUBJECT = "DOE SUBJECT";
    private static final String DOE_TECH_ED = "DOE TECH ED";
    private static final String DOE_ADV_TECH_ED = "DOE ADV TECH ED";
    private static final String DOE_CTE_CIP = "DOE CTE CIP";
    private static final String DOE_CTE_ASSESSMENT = "DOE CTE ASSESSMENT";
    private static final String DOE_CTE_CONCENTRATOR = "DOE CTE CONCENTRATOR";
    private static final String DOE_TRADITIONAL_COURSE_FLAG = "DOE TRADITIONAL COURSE FLAG";
    private static final String DOE_HOMELESS_BEGIN = "DOE HL BEGIN";
    private static final String DOE_HOMELESS_END = "DOE HL END";
    private static final String DOE_PROG_CODE_HOMELESS = "DOE PR HOMELESS";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_doeAdvTechEd; // added for allegany
    protected String m_doeClassOfRecord;
    protected String m_doeCteAssessment;
    protected String m_doeCteCip;
    protected String m_doeCteConcentrator;
    protected String m_doeHsaPreReq;
    protected String m_doeIb;
    protected String m_doeSubjectCodeField;
    protected String m_homelessBegin;
    protected String m_homelessEnd;
    protected String m_doeTechEd; // added for allegany (and wicomico)
    protected String m_doeTraditionalCourseFlag;
    protected String m_doeTransitionCourse;
    protected String m_doeTrnAsmPassed;
    private Map<String, Collection<StudentEnrollment>> m_enrMap;
    protected Map<String, ReferenceCode> m_gradeCodes;
    protected Map<String, ReferenceCode> m_techEduCodes; // added for wicomico
    protected Map<String, Collection<GradeScalePoints>> m_gradePoints;
    protected Map<String, GradeScale> m_gradeScales;
    protected PlainDate m_reportDate;
    protected Integer m_reportingPeriod;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected Map<String, Collection<ScheduleTermDate>> m_scheduleTermDateMap = new HashMap();
    protected String m_spedActiveCode;
    protected Map<String, Collection<ScheduleTeacher>> m_teacherSchedules;
    protected Map<String, Collection<ScheduleTermDate>> m_termDates;
    protected Map<String, Collection<Transcript>> m_transcriptMap;
    protected Set<String> m_withdrawalCodes;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;
    protected PlainDate m_twoYearCutoff;



    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Get core parameters
         */
        m_spedActiveCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization())); // Today.
        m_twoYearCutoff = (PlainDate) getParameter(TWO_YEAR_CUTOFF_PARAM);

        initializeFields();
        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadTranscripts();
            loadStudentSchedules();

            // Set query and entity
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);
            setQuery(studentQuery);
            setEntityClass(MDStudentCourseGradeTeacherEntity.class);

            // Load a map of grade score information.
            loadGradeScales();
            loadSchedules();

            // Load a map of technology education codes
            // this is only for Wicomico
            loadTechEduReferenceCodes();

            // Load map of ENRs
            loadStudentEnrollments(studentCriteria);

            // Load student programs map.
            loadStudentPrograms(studentCriteria);

            // Build maps of retriever functions and validator functions
            RetrieveRace retrieveRace = new RetrieveRace();
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SCG-CLEAN", new RetrieveCleanValue());
            calcs.put("SCG-ELL-IND", new RetrieveProgram(DOE_PROG_CODE_ELL, null, subQuery, true));
            calcs.put("SCG-ELL-START", new RetrieveProgram(DOE_PROG_CODE_ELL, "startDate", subQuery, true));
            calcs.put("SCG-ELL-END", new RetrieveProgram(DOE_PROG_CODE_ELL, "endDate", subQuery, true));
            calcs.put("SCG-GRADE", new RetrieveTranscriptInfo());
            calcs.put("SCG-RACE", retrieveRace);
            calcs.put("SCG-SPED504", new RetrieveSped());
            calcs.put("SCG-TEACHER", new RetrieveTeacherInfo(retrieveRace));
            calcs.put("SCG-TERM", new RetrieveTermInfo());
            calcs.put("SCG-MASTER", new RetrieveMasterScheduleInfo());
            calcs.put("SCG-SPED-EXIT", new RetrieveSpedEndDate());
            calcs.put("ATT-HOMELESS-PRG", new RetrieveProgram(DOE_PROG_CODE_HOMELESS, null, subQuery, false));

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateCourseCode.VAL_ID, new ValidateCourseCode());
            validators.put(ValidateAlphaGrade.VAL_ID, new ValidateAlphaGrade());
            validators.put(ValidateSecondaryTeacher.VAL_ID, new ValidateSecondaryTeacher());
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Gets the schedule term dates.
     *
     * @param scheduleTermOid String
     * @return Collection
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> termDates = null;
        if (m_scheduleTermDateMap.containsKey(scheduleTermOid)) {
            termDates = m_scheduleTermDateMap.get(scheduleTermOid);
        } else {
            ScheduleTerm term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, scheduleTermOid);
            if (term != null) {
                termDates = term.getScheduleTermDates();
                m_scheduleTermDateMap.put(scheduleTermOid, termDates);
            }
        }
        return termDates != null ? termDates : new ArrayList<ScheduleTermDate>();
    }

    /**
     * Returns ENR of the given student Oid.
     *
     * @param stdOid String
     * @return Array list
     */
    protected ArrayList<StudentEnrollment> getStudentEnrollments(String stdOid) {
        ArrayList<StudentEnrollment> enrList = new ArrayList<StudentEnrollment>(m_enrMap.get(stdOid));
        return enrList;
    }

    /**
     * Translate aliases to java bean path names.
     * Log errors if aliases don't exist.
     */
    private void initializeFields() {
        m_homelessBegin = translateAliasToJavaName(DOE_HOMELESS_BEGIN, true);
        m_homelessEnd = translateAliasToJavaName(DOE_HOMELESS_END, true);
        m_doeSubjectCodeField = translateAliasToJavaName(DOE_SUBJECT, true);
        m_doeIb = translateAliasToJavaName(DOE_IB, true);
        m_doeHsaPreReq = translateAliasToJavaName(DOE_HSA_PREREQ, true);
        m_doeClassOfRecord = translateAliasToJavaName(DOE_CLASS_OF_RECORD, true);
        m_doeCteCip = translateAliasToJavaName(DOE_CTE_CIP, true);
        m_doeCteConcentrator = translateAliasToJavaName(DOE_CTE_CONCENTRATOR, true);
        m_doeCteAssessment = translateAliasToJavaName(DOE_CTE_ASSESSMENT, false);
        m_doeTechEd = translateAliasToJavaName(DOE_TECH_ED, false); // for allegany + wicomico
        m_doeAdvTechEd = translateAliasToJavaName(DOE_ADV_TECH_ED, false); // for allegany only
        m_doeTraditionalCourseFlag = translateAliasToJavaName(DOE_TRADITIONAL_COURSE_FLAG, true);
        m_doeTrnAsmPassed = translateAliasToJavaName(ALIAS_TRN_ASM_PASSED, true);
        m_doeTransitionCourse = translateAliasToJavaName(ALIAS_TRANSITION_COURSE, true);
    }

    /**
     * Load all Transcripts and put them into a map by student OID.
     */
    private void loadTranscripts() {
        /*
         * Build query object that will be used to retrieve export students.
         */
        Criteria transcriptCriteria = getTranscriptCriteria();
        QueryByCriteria transcriptQuery = new QueryByCriteria(Transcript.class, transcriptCriteria);

        Integer sort = (Integer) getParameter(SORT_PARAM);
        switch (sort != null ? sort.intValue() : 0) {
            case 0: // Name
                transcriptQuery.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 1: // YOG
                transcriptQuery.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG);
                transcriptQuery.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 2: // SisSchool
                transcriptQuery.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL
                        + PATH_DELIMITER + SisSchool.COL_NAME);
                transcriptQuery.addOrderByAscending(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_NAME_VIEW);
                break;

            case 3: // LASID
                transcriptQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                break;

            case 4: // SASID
                transcriptQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                break;

            default:
                transcriptQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                break;
        }

        // Set the query to be used for student selection.
        transcriptQuery.addOrderByAscending(Transcript.COL_COURSE_DESCRIPTION);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(transcriptQuery, Transcript.COL_STUDENT_OID, 2000);
    }

    /**
     * Load all StudentSchedules and load them into a map by student OID.
     */
    private void loadStudentSchedules() {
        X2Criteria studentScheduleCriteria = new X2Criteria();

        if (isSchoolContext()) {
            studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER
                    + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }
        studentScheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.COL_MASTER_TYPE,
                "Class");
        studentScheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL
                + PATH_DELIMITER + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID, StudentSchedule.COL_SCHEDULE_OID);
        // Exclude courses with no subject code.
        studentScheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE + PATH_DELIMITER
                + m_doeSubjectCodeField, getBroker().getPersistenceKey());

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                studentScheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + ModelProperty.PATH_DELIMITER
                        + SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(studentScheduleCriteria, queryString);
                break;

            default:
                // Take all students
                break;
        }

        QueryByCriteria studentScheduleQuery = new QueryByCriteria(StudentSchedule.class, studentScheduleCriteria);
        m_scheduleMap =
                getBroker().getGroupedCollectionByQuery(studentScheduleQuery, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Wicomico-specific. Loads reference code for technology education reference codes
     */
    private void loadTechEduReferenceCodes() {
        // get the data dictionary field for tech education (which is shared by allegany and
        // wicomico)
        // only wicomico will have a reference code
        DataDictionaryField ddField =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                        Course.class.getName(), m_doeTechEd);
        String referenceTableOid = ddField.getReferenceTableOid();
        if (!StringUtils.isEmpty(referenceTableOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
            m_techEduCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 4);
        }
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME, recordSetName);

        criteria.addIn(Transcript.COL_STUDENT_OID, new SubQuery(RecordSetKey.class, RecordSetKey.COL_OBJECT_OID,
                recordSetCriteria));
    }

    /**
     * Builds the criteria for student to be reported.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        Criteria activityCriteria = new Criteria();
        PlainDate startDate = getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        Criteria enrollCriteria = new Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        Criteria primaryCriteria = new Criteria();
        primaryCriteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                SisStudent.COL_ENROLLMENT_STATUS));
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        X2Criteria reportingCriteria = new X2Criteria();
        reportingCriteria.addOrCriteria(primaryCriteria);
        reportingCriteria.addOrCriteria(enrollCriteria);

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                reportingCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                reportingCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                reportingCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(reportingCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }
        return reportingCriteria;
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        Criteria primaryCriteria = new Criteria();

        // Reporting school.
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(Transcript.COL_SCHOOL_OID, getSchool().getOid());
        }

        // Reporting year
        primaryCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getTranscriptCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER + SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        // Exclude courses with no subject code.
        userCriteria.addNotEmpty(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + m_doeSubjectCodeField, getBroker().getPersistenceKey());
        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Load grade scales for transcript grade translation.
     */
    private void loadGradeScales() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE, Integer.valueOf(
                TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        QueryByCriteria query = new QueryByCriteria(TranscriptColumnDefinition.class, criteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                TranscriptColumnDefinition tcd = (TranscriptColumnDefinition) iterator.next();
                m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
            }
        } finally {
            iterator.close();
        }

        // Load map of grade code reference codes
        m_gradeCodes = new HashMap<String, ReferenceCode>();
        DataDictionaryField gradeCodeField =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey()).findDataDictionaryField(
                        GradeScaleGradeDefinition.class.getName(), GradeScaleGradeDefinition.COL_GRADE_CODE);
        if (gradeCodeField != null && gradeCodeField.getReferenceTable() != null) {
            for (ReferenceCode code : gradeCodeField.getReferenceTable().getReferenceCodes(getBroker())) {
                m_gradeCodes.put(code.getCode(), code);
            }
        }

        // Load a map of all grade scale point definitions for GPA lookup.
        query = new QueryByCriteria(GradeScalePoints.class);
        m_gradePoints = getBroker().getGroupedCollectionByQuery(query, GradeScalePoints.COL_GRADE_SCALE_GRADE_OID, 10);
    }

    /**
     * Load maps of master schedules and teacher schedules.
     *
     * Map of collection of ScheduleTeacher by section OID.
     * Map of MasterSchedule by section OID.
     */
    private void loadSchedules() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER
                + Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }
        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);
        m_teacherSchedules = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_SECTION_OID, 100);

        criteria = new Criteria();
        criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                + PATH_DELIMITER + Schedule.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.REL_SCHEDULE
                    + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
        }
        query = new QueryByCriteria(ScheduleTermDate.class, criteria);
        m_termDates = getBroker().getGroupedCollectionByQuery(query, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 100);
    }

    /**
     * Load collection of students' enrollments keyed on students' oids.
     *
     * @param stdCriteria Criteria
     */
    private void loadStudentEnrollments(Criteria stdCriteria) {
        X2Criteria enrCriteria = new X2Criteria();
        SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);
        enrCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, stdSubQuery);
        QueryByCriteria enrQuery = new QueryByCriteria(StudentEnrollment.class, enrCriteria);
        enrQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);

        m_enrMap = getBroker().getGroupedCollectionByQuery(enrQuery, StudentEnrollment.COL_STUDENT_OID, 1024);
    }

    /**
     * Load student programs into a map by student Oid.
     *
     * @param studentCriteria Criteria
     */
    private void loadStudentPrograms(Criteria studentCriteria) {
        /*
         * Load student program participation records.
         */
        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria programCriteria = new Criteria();
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        m_studentPrograms =
                getBroker().getGroupedCollectionByQuery(programQuery, StudentProgramParticipation.COL_STUDENT_OID, 100);
    }

}
