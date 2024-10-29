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
package com.x2dev.procedures.statereporting.md;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Maryland State Report: Class Membership by course section export.
 * This class implements the data export for MD Class Membership export.
 * 
 * @author X2 Development Corporation
 */
public class ClassMembership extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the MD Class Membership export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ClassMembershipEntity extends StateReportEntity {
        public static String[] PARAM_COUNTS = {"ETH-M", "ETH-F", "R1-M", "R1-F", "R2-M", "R2-F",
                "R3-M", "R3-F", "R4-M", "R4-F", "R5-M", "R5-F", "RS-M", "RS-F",
                "T1", "FARMS", "DIS", "504", "LEP"};

        /*
         * Cached values for retrievers to share.
         */
        int m_classNumber = 0;
        ClassMembership m_CLMData = null;
        String m_gradeLevel = null;
        int[] m_counts = new int[19];
        int m_students = 0;
        SisStaff m_teacher = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ClassMembershipEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return the sequential class number for the teacher for this section.
         *
         * @return int
         */
        public int getClassNumber() {
            return m_classNumber;
        }

        /**
         * Identify one of the precalculated count fields and return an Integer.
         *
         * @param countId string identifying which count to return.
         *
         * @return Integer
         */
        public Integer getCount(String countId) {
            Integer count = null;
            for (int idx = 0; idx < PARAM_COUNTS.length; idx++) {
                if (PARAM_COUNTS[idx].equals(countId)) {
                    count = Integer.valueOf(m_counts[idx]);
                    break;
                }
            }
            return count;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Section section = (Section) getBean();
            String name = "Section: " + section.getCourseView() +
                    " [School: " + section.getSchoolCourse().getSchool().getName() + "]";

            return name;
        }

        /**
         * Returns the grade level of the course.
         *
         * @return String
         */
        public String getGradeLevel() {
            return m_gradeLevel;
        }

        /**
         * Return the teacher Staff object for the course.
         *
         * @return SisStaff
         */
        public SisStaff getTeacher() {
            return m_teacher;
        }

        /**
         * Returns an error message or null indicating if this entity should be filtered out of the
         * export or included.
         * Implementing classes can perform run time filtering on entities.
         *
         * @return a validation error if this entity should not be exported.
         *         a null indicates the entity should be exported.
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            if (m_students == 0) {
                error = new StateReportValidationError(this, m_CLMData.getFieldDefinition(1),
                        "No students", "No students found for course");
            }
            return error;
        }

        /**
         * Get students and staff for the course. Gather statistics on student gender, race and
         * programs.
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

            m_CLMData = (ClassMembership) data;

            Section section = (Section) bean;

            // Get the primary teacher for the course.
            m_teacher = section.getPrimaryStaff();

            // Get the grade level for the course.
            // If it is empty, get the grade level from the majority
            // of students taking the course.
            m_gradeLevel = section.getSchoolCourse().getCourse().getGradeLevel();

            // Map by grade level of counts of students in course.
            HashMap<String, Integer> gradeLevelMap = new HashMap<String, Integer>();

            // Get the next class number for the teacher.
            if (m_teacher != null) {
                String teacherOid = m_teacher.getOid();
                Integer clsNum = m_CLMData.m_teacherClassNumber.get(teacherOid);
                if (clsNum != null) {
                    clsNum = Integer.valueOf(clsNum.intValue() + 1);
                    m_CLMData.m_teacherClassNumber.put(teacherOid, clsNum);
                } else {
                    clsNum = Integer.valueOf(1);
                    m_CLMData.m_teacherClassNumber.put(teacherOid, clsNum);
                }
                m_classNumber = clsNum.intValue();
            }

            SisSchool school = section.getSchedule().getSchool();
            String ind = (String) school.getFieldValueByAlias(DOE_TITLE1_SCHOOL);

            // Gather statistics about the students in the course.
            Collection<SisStudent> students = m_CLMData.getStudentsForSection(section);
            CMStudentContainer cmStudentContainer;
            for (SisStudent student : students) {
                String studentOid = student.getOid();
                if (!m_CLMData.m_studentMap.containsKey(studentOid)) {
                    m_CLMData.populateStudentInMap(student);
                }

                cmStudentContainer = m_CLMData.m_studentMap.get(studentOid);
                m_students++;

                // Get student counts by grade level.
                String stdGrade = student.getGradeLevel();
                Integer count = gradeLevelMap.get(stdGrade);
                if (count == null) {
                    count = Integer.valueOf(1);
                    gradeLevelMap.put(stdGrade, count);
                } else {
                    count = Integer.valueOf(count.intValue() + 1);
                    gradeLevelMap.put(stdGrade, count);
                }

                // Get race and ethnicity statistics for this student.
                int iGender = (cmStudentContainer.isFemale() ? 1 : 0);
                if (cmStudentContainer.isLatino()) {
                    m_counts[iGender]++;
                } else {
                    // Get race and gender statistics for this student with no hispanic ethnicity.
                    if (cmStudentContainer.isMultiRacial()) {
                        // No hispanic ethnicity, two or more races.
                        m_counts[12 + iGender] += 1;
                    } else {
                        int iRace = cmStudentContainer.getRace();
                        if (iRace > 0 && iRace <= 5) {
                            m_counts[(iRace * 2) + iGender] += 1;
                        }

                    }
                }


                // Lookup program statistics for this student.

                // Check Title I program by school.
                if (BooleanAsStringConverter.TRUE.equals(ind)) {
                    m_counts[14]++;
                }

                // Check the FARMS/Lunch program.
                // TODO boolean isDOELunch() (Find out what the "D" program is, extract into a
                // constant so someone can read it easier and name method that way
                if (cmStudentContainer.isdLunch()) {
                    m_counts[15]++;
                }

                // Check the Disabled status.
                if (cmStudentContainer.isStudentDisabled()) {
                    m_counts[16]++;
                }

                // Check the 504 status from student.
                // TODO boolean isStudent504Sped()
                if (cmStudentContainer.isStudent504Sped()) {
                    m_counts[17]++;
                }

                // Check the ELL program.
                // TODO boolean isStudentELL()
                if (cmStudentContainer.isStudentELL()) {
                    m_counts[18]++;
                }
            }

            // Check the grade level for the course.
            // If the course grade is not set, use the grade of the highest number of students.
            if (StringUtils.isEmpty(m_gradeLevel)) {
                int highest = 0;
                for (String stdGrade : gradeLevelMap.keySet()) {
                    Integer count = gradeLevelMap.get(stdGrade);
                    if (count.intValue() > highest) {
                        highest = count.intValue();
                        m_gradeLevel = stdGrade;
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
     * Name for the report format parameter. This indicated CSV or Column delimited report.
     */
    public static final String REPORT_FORMAT_PARAM = "reportFormat";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /*
     * Field aliases for "Student Program Participation" program type code
     */
    // private static final String DOE_PROG_CODE_LUNCH = "DOE PR LUNCH";
    // private static final String DOE_PROG_CODE_TITLE1 = "DOE PR TITLE1";
    private static final String DOE_PROG_CODE_ELL = "DOE PR ELL";
    // private static final String DOE_PROG_CODE_504 = "DOE PR 504";

    /*
     * Field alias/field value for querying options on the export
     */
    // private static final String DOE_TITLE1 = "DOE TITLE1";
    private static final String DOE_TITLE1_SCHOOL = "DOE Title 1";
    private static final String DOE_LUNCH = "DOE LUNCH";
    private static final String DOE_SPED_504 = "DOE SPED 504";
    private static final String DOE_DISABLED = "DOE DISABLED";
    private static final String DOE_ELL = "DOE ELL";
    private static final String DOE_SUBJECT = "DOE SUBJECT";

    private static final String VALUE_504 = "504";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    // protected SimpleDateFormat m_dateInnerFormat;
    // protected String m_doeSubjectField;
    protected String m_ellField;
    // protected String m_ltSubField;
    // protected String m_lunchField;
    // protected String m_raceField;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected Map<String, ReferenceCode> m_gradeCodes;
    protected Map<String, ReferenceCode> m_raceCodes;
    protected PlainDate m_reportDate;
    protected String m_subjectField;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentPrograms;
    protected Map<String, Integer> m_teacherClassNumber;
    protected Map<String, String> m_stateRaceCodes = new HashMap<String, String>();
    // protected String m_title1Field;

    /*
     * Cached map of students and their information
     */
    HashMap<String, CMStudentContainer> m_studentMap = new HashMap<String, CMStudentContainer>();

    /**
     * Returns the class number for the teacher of this course.
     * The class number is calculated by the entity class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveClassNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ClassMembershipEntity clmEnt = (ClassMembershipEntity) entity;
            Integer classNum = Integer.valueOf(clmEnt.getClassNumber());
            return classNum;
        }
    }

    /**
     * Returns the grade level for the course.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLevel implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ClassMembershipEntity clmEnt = (ClassMembershipEntity) entity;
            String gradeCode = clmEnt.getGradeLevel();
            if (gradeCode != null) {
                ReferenceCode refCode = m_gradeCodes.get(gradeCode);
                if (refCode != null) {
                    gradeCode = refCode.getStateCode();
                }
            }

            return gradeCode;
        }
    }

    /**
     * Populate the student map with the details. Making sure that it is not getting populated twice
     * for each student.
     * 
     * @param student
     *        void
     */
    protected void populateStudentInMap(SisStudent student) {
        String studentOid = student.getOid();
        CMStudentContainer container = new CMStudentContainer();
        Collection<StudentProgramParticipation> programs = m_studentPrograms.get(studentOid);
        String ind = null;
        SisPerson person = student.getPerson();

        container.setLatino(person.getHispanicLatinoIndicator());

        container.setFemale("F".equals(person.getGenderCode()));

        ind = (String) student.getFieldValueByAlias(DOE_SPED_504);
        container.setStudent504Sped(VALUE_504.equals(ind));

        ind = (String) student.getFieldValueByAlias(DOE_LUNCH);
        container.setdLunch(!StringUtils.isEmpty(ind) && !"D".equals(ind));

        ind = (String) student.getFieldValueByAlias(DOE_DISABLED);
        container.setStudentDisabled(BooleanAsStringConverter.TRUE.equals(ind));

        container.setStudentELL(getProgramInd(student, programs, m_ellField, DOE_PROG_CODE_ELL));

        // Race Calculator
        Collection<Race> races = person.getRaces();
        if (races.size() > 1) {
            container.setMultiRacial(true);
        } else {
            for (Race race : races) {
                String ssRace = getStateRaceValue(race.getRaceCode());
                if (ssRace != null) {
                    try {
                        container.setRace(Integer.parseInt(ssRace));
                    } catch (NumberFormatException nfe) {
                        // ssRace did not convert, ignore as this will not count in statistics.
                    }
                }
            }
        }
        m_studentMap.put(studentOid, container);
    }

    /**
     * Return an indicator if the student is in the ELL program.
     * This checks two possible sources for the data:
     * 1. A field on the student record indicated by the studentField parameter.
     * 2. A program participation records with a program code whose reference code state value is
     * programCode
     *
     * @param student SisStudent
     * @param programs Collection<StudentProgramParticipation>
     * @param studentField String
     * @param programCode String
     * @return boolean
     */
    private boolean getProgramInd(SisStudent student,
                                  Collection<StudentProgramParticipation> programs,
                                  String studentField,
                                  String programCode) {
        boolean isProgram = false;
        if (!StringUtils.isEmpty(studentField)) {
            String progInd = (String) student.getFieldValueByBeanPath(studentField);
            if (!StringUtils.isEmpty(progInd) && !"0".equals(progInd)) {
                isProgram = true;
            }
        } else {
            isProgram = getProgram(programs, programCode);
        }
        return isProgram;
    }

    /**
     * Search the programs collection for a program matching the provided program code.
     * The program code is compared to the state value of the program code reference code.
     * If the program dates match the report date, indicate the program is in effect.
     *
     * @param programs Collection<StudentProgramParticipation>
     * @param programCode String
     * @return boolean
     */
    private boolean getProgram(Collection<StudentProgramParticipation> programs, String programCode) {
        PlainDate reportDate = m_reportDate;
        boolean inProgram = false;
        if (programs != null && !StringUtils.isEmpty(programCode)) {
            for (StudentProgramParticipation program : programs) {
                String code = program.getProgramCode();
                String alias = lookupStateValue(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE, code);
                if (programCode.equals(alias)) {
                    PlainDate pgmStartDate = program.getStartDate();
                    PlainDate pgmEndDate = program.getStartDate();
                    if (pgmStartDate != null &&
                            ((reportDate.after(pgmStartDate) ||
                                    reportDate.equals(pgmStartDate)) &&
                                    (program.getEndDate() == null ||
                                            reportDate.before(pgmEndDate) ||
                                            reportDate.before(pgmEndDate)))) {
                        inProgram = true;
                        break;
                    }
                }
            }
        }
        return inProgram;
    }

    /**
     * Gets the state race value.
     *
     * @param raceCode String
     * @return String
     */
    protected String getStateRaceValue(String raceCode) {
        String stateValue = m_stateRaceCodes.get(raceCode);
        if (StringUtils.isEmpty(stateValue)) {
            stateValue = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceCode);
            m_stateRaceCodes.put(raceCode, stateValue);
        }
        return stateValue;
    }

    /**
     * The Containter with getters and setters to populate the student data.
     */
    protected class CMStudentContainer {
        public boolean student504Sped;
        public boolean dLunch;
        public boolean studentDisabled;
        public boolean studentELL;
        public boolean title1School;
        public boolean multiRacial;
        public boolean female;
        public boolean latino;
        public int race;

        /**
         * Checks if is latino.
         *
         * @return true, if is latino
         */
        public boolean isLatino() {
            return latino;
        }

        /**
         * Sets the latino.
         *
         * @param latino void
         */
        public void setLatino(boolean latino) {
            this.latino = latino;
        }

        /**
         * Gets the race.
         *
         * @return int
         */
        public int getRace() {
            return race;
        }

        /**
         * Sets the race.
         *
         * @param race void
         */
        public void setRace(int race) {
            this.race = race;
        }

        /**
         * Checks if is female.
         *
         * @return true, if is female
         */
        public boolean isFemale() {
            return female;
        }

        /**
         * Sets the female.
         *
         * @param female void
         */
        public void setFemale(boolean female) {
            this.female = female;
        }

        /**
         * Checks if is multi racial.
         *
         * @return true, if is multi racial
         */
        public boolean isMultiRacial() {
            return multiRacial;
        }

        /**
         * Sets the multi racial.
         *
         * @param multiRacial void
         */
        public void setMultiRacial(boolean multiRacial) {
            this.multiRacial = multiRacial;
        }

        /**
         * Checks if is title 1 school.
         *
         * @return true, if is title 1 school
         */
        public boolean isTitle1School() {
            return title1School;
        }

        /**
         * Sets the title 1 school.
         *
         * @param title1School void
         */
        public void setTitle1School(boolean title1School) {
            this.title1School = title1School;
        }

        /**
         * Checks if is student 504 sped.
         *
         * @return true, if is student 504 sped
         */
        public boolean isStudent504Sped() {
            return student504Sped;
        }

        /**
         * Sets the student 504 sped.
         *
         * @param student504Sped void
         */
        public void setStudent504Sped(boolean student504Sped) {
            this.student504Sped = student504Sped;
        }

        /**
         * Checks if is d lunch.
         *
         * @return true, if is d lunch
         */
        public boolean isdLunch() {
            return dLunch;
        }

        /**
         * Sets the d lunch.
         *
         * @param dLunch void
         */
        public void setdLunch(boolean dLunch) {
            this.dLunch = dLunch;
        }

        /**
         * Checks if is student disabled.
         *
         * @return true, if is student disabled
         */
        public boolean isStudentDisabled() {
            return studentDisabled;
        }

        /**
         * Sets the student disabled.
         *
         * @param studentDisabled void
         */
        public void setStudentDisabled(boolean studentDisabled) {
            this.studentDisabled = studentDisabled;
        }

        /**
         * Checks if is student ELL.
         *
         * @return true, if is student ELL
         */
        public boolean isStudentELL() {
            return studentELL;
        }

        /**
         * Sets the student ELL.
         *
         * @param studentELL void
         */
        public void setStudentELL(boolean studentELL) {
            this.studentELL = studentELL;
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

            MasterSchedule section = (MasterSchedule) entity.getBean();
            SisStaff staff = section.getPrimaryStaff();
            if (staff != null) {
                Collection<Race> races = m_raceCodeMap.get(staff.getPersonOid());
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
            }

            return raceCode;
        }
    }

    /**
     * Returns a student count statistic for the course.
     * The statistic is determined by the parameter included in the field definition.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudentStat implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ClassMembershipEntity clmEnt = (ClassMembershipEntity) entity;
            String param = (String) field.getParameter();
            Integer count = clmEnt.getCount(param);
            return count;
        }
    }

    /**
     * Return a collection of student records for a given course section.
     *
     * @param section Section
     * @return Collection<Student>
     */
    public Collection<SisStudent> getStudentsForSection(Section section) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisStudent.REL_STUDENT_SCHEDULES + "." + StudentSchedule.COL_SECTION_OID, section.getOid());
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        Collection<SisStudent> students = getBroker().getCollectionByQuery(query);

        return students;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     * 
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
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
         * Set up tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_teacherClassNumber = new HashMap<String, Integer>();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria sectionCriteria = getSectionCriteria();
            QueryByCriteria sectionQuery = new QueryByCriteria(MasterSchedule.class, sectionCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Course
                    sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
                    sectionQuery.addOrderByAscending(
                            MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.COL_NAME_VIEW);
                    break;

                case 1: // School
                    sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(
                            MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.COL_NAME_VIEW);
                    sectionQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
                    break;

                case 2: // Staff
                    sectionQuery.addOrderByAscending(
                            MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.COL_NAME_VIEW);
                    sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
                    break;

                default:
                    sectionQuery.addOrderByAscending(
                            MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.COL_NAME_VIEW);
                    sectionQuery.addOrderByAscending(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER
                            + SchoolCourse.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME);
                    sectionQuery.addOrderByAscending(MasterSchedule.COL_COURSE_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(sectionQuery);
            setEntityClass(ClassMembershipEntity.class);

            // Load student programs map and teacher map.
            loadStudentPrograms();

            // Get race code reference codes for use in the race retriever.
            Criteria raceCriteria = new Criteria();
            raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbRaceCodes");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, raceCriteria);
            m_raceCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5);

            // Map of race codes by personOid.
            SubQuery subQuery = new SubQuery(MasterSchedule.class,
                    MasterSchedule.REL_PRIMARY_STAFF + PATH_DELIMITER + Staff.COL_PERSON_OID,
                    sectionCriteria);
            raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, subQuery);
            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);

            // Get grade level reference code map.
            Criteria gradeCriteria = new Criteria();
            gradeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, "rtbGradeLevel");
            query = new QueryByCriteria(ReferenceCode.class, gradeCriteria);
            m_gradeCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 20);

            // Add any retrievers or validators.
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("CLM-COUNT", new RetrieveStudentStat());
            calcs.put("CLM-RACE", new RetrieveRace());
            calcs.put("CLM-CLASS-NUM", new RetrieveClassNumber());
            calcs.put("CLM-GRADE", new RetrieveGradeLevel());
            super.addCalcs(calcs);
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
         * Sections in the term of the reporting date.
         */
        X2Criteria sectionCriteria = new X2Criteria();
        if (isSchoolContext()) {
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }
        sectionCriteria.addEqualTo(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                Course.COL_MASTER_TYPE, "Class");
        sectionCriteria.addNotEmpty(MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER +
                m_subjectField, getBroker().getPersistenceKey());
        sectionCriteria.addEqualToField(MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                MasterSchedule.COL_SCHEDULE_OID);
        return sectionCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getSectionCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all sections in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(userCriteria);
        criteria.addAndCriteria(getReportingCriteria());

        return criteria;
    }

    /**
     * Lookup field names for some aliases.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_ellField = translateAliasToJavaName(DOE_ELL, false);
        m_subjectField = translateAliasToJavaName(DOE_SUBJECT, true);
    }

    /**
     * Load student programs into a map by student Oid.
     */
    private void loadStudentPrograms() {
        /*
         * Load student program participation records.
         */
        Criteria programCriteria = new Criteria();
        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        m_studentPrograms = getBroker().getGroupedCollectionByQuery(programQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 1000);
    }
}

