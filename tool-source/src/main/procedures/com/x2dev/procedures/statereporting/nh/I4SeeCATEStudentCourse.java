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

package com.x2dev.procedures.statereporting.nh;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for New Hampshire's i4see Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class I4SeeCATEStudentCourse extends StateReportData {
    /**
     * Term ID Strings
     */
    private static final String TERM_ID_FIRST_SEMESTER = "1";
    private static final String TERM_ID_SECOND_SEMESTER = "2";
    private static final String TERM_ID_FULL_YEAR = "30";


    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        /**
         * Place holders for calculated unmapped fields. These can be written back to the database
         * in postProcess if update flag is set. Also, holds some calculated values that have
         * been overridden with default or related values.
         *
         * Map key should be field alias constant.
         */
        private ArrayList<List<StudentScheduleChange>> m_StudentScheduleChangeList = null;
        protected static Map<String, List<StudentScheduleChange>> m_studentScheduleChanges;

        protected static Map<String, StudentSchedule> m_studentSchedules;

        protected Map<String, PlainDate> m_programExitDate;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Filter out inactive schedules, non-CATE courses and courses dropped before they began.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            I4SeeCATEStudentCourse cateData = (I4SeeCATEStudentCourse) getData();

            /*
             * Filter out inactive schedules
             */
            if (getStudentScheduleChangeList() != null) {
                try {
                    String scheduleOid = (String) WebUtils.getProperty(getStudentScheduleChangeList().get(0),
                            StudentScheduleChange.COL_SCHEDULE_OID);

                    if (!cateData.m_activeScheduleOids.contains(scheduleOid)) {
                        error = new StateReportValidationError("", "", "", "not an error");
                    }

                    /*
                     * Filter out non-CATE courses
                     */
                    if (error == null) {
                        try {
                            String javaName = cateData.translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);
                            String cipCode = (String) WebUtils.getProperty(getStudentScheduleChangeList().get(0),
                                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                                            MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                                            SchoolCourse.REL_COURSE + PATH_DELIMITER +
                                            javaName);
                            if (StringUtils.isEmpty(cipCode)) {
                                error = new StateReportValidationError("", "", "", "not an error");
                            }
                        } catch (X2BaseException e) {
                            error = new StateReportValidationError(this.toString(), "Course.Program Id", "",
                                    "not an error");
                        }
                    }

                    /*
                     * Filter out courses dropped before they began
                     */
                    if (error == null) {
                        PlainDate entryDate = null;
                        PlainDate exitDate = null;
                        FieldDefinition fieldDef = cateData.getFieldDefinition(I4SEE_230_CATE_ENTRY_DATE); // entry
                                                                                                           // date
                        if (fieldDef != null) {
                            entryDate = (PlainDate) fieldDef.getRetriever().getFieldValue(cateData, this, fieldDef);
                        }
                        fieldDef = cateData.getFieldDefinition(I4SEE_250_CATE_EXIT_DATE); // exit
                                                                                          // date
                        if (fieldDef != null) {
                            exitDate = (PlainDate) fieldDef.getRetriever().getFieldValue(cateData, this, fieldDef);
                            if (exitDate != null && entryDate != null && exitDate.before(entryDate)) {
                                error = new StateReportValidationError(this.toString(), "Exit before entry", "",
                                        "not an error");
                            }

                            StudentScheduleChange change = getStudentScheduleChangeList().get(0);
                            String javaName = cateData.translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);
                            String cipCode = (String) WebUtils.getProperty(change,
                                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                                            MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                                            SchoolCourse.REL_COURSE + PATH_DELIMITER +
                                            javaName);
                            PlainDate programExitDate = m_programExitDate.get(cipCode);
                            Collection<ScheduleTermDate> dates =
                                    change.getMasterSchedule().getScheduleTerm().getScheduleTermDates();
                            boolean classBeforeExitDate = true;
                            if (programExitDate != null) {
                                for (ScheduleTermDate date : dates) {
                                    if (!date.getStartDate().after(programExitDate)) {
                                        classBeforeExitDate = false;
                                    }
                                }
                            }
                            if (!classBeforeExitDate) {
                                error = new StateReportValidationError(this.toString(), "Class start date", "", "");
                            }

                        }
                    }
                } catch (X2BaseException e) {
                    error = new StateReportValidationError(this.toString(), "StudentSchedule.COL_SCHEDULE_OID", "",
                            "not an error");
                }
            } else {
                error = new StateReportValidationError(this.toString(), "No courses/changes.", "", "not an error");
            }

            return error;
        }

        /**
         * Returns the entry enrollment record for the current student.
         *
         * return StudentEnrollment
         *
         * @return List
         */
        public List<StudentScheduleChange> getStudentScheduleChangeList() {
            List<StudentScheduleChange> info = null;
            int index = getCurrentRow();

            if (m_StudentScheduleChangeList != null && index >= 0 && index < m_StudentScheduleChangeList.size()) {
                info = m_StudentScheduleChangeList.get(index);
            }

            return info;
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
                    (getData().isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";

            return name;
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

            String studentOid = bean.getOid();
            I4SeeCATEStudentCourse cateData = (I4SeeCATEStudentCourse) getData();

            Collection<StudentProgramParticipation> programs = cateData.getStudentProgramParticipation(studentOid);
            if (programs != null) {
                /*
                 * Load student programs and its exit dates
                 */
                m_programExitDate = new HashMap<String, PlainDate>();
                for (StudentProgramParticipation program : programs) {
                    String programId = (String) program.getFieldValueByAlias(I4SEE_1710_PROGRAM_ID);
                    m_programExitDate.put(programId, program.getEndDate());
                }

                /*
                 * Load Student Schedules and changes
                 */
                loadStudentSchedules(studentOid);
                loadStudentScheduleChanges(studentOid);

                // combine schedule and schedule changes (e.g. dropped courses)
                setRowCount(combineScheduleData(studentOid));

                if (m_studentScheduleChanges != null) {
                    m_StudentScheduleChangeList = new ArrayList<List<StudentScheduleChange>>();
                    m_StudentScheduleChangeList.addAll(m_studentScheduleChanges.values());
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
         * Combines all schedule data and add/drop data into a single add/drop list.
         *
         * @param studentOid String
         * @return int
         */
        private int combineScheduleData(String studentOid) {
            Map<String, StudentSchedule> studentSchedules = m_studentSchedules;

            if (m_studentScheduleChanges == null) {
                m_studentScheduleChanges = new HashMap<String, List<StudentScheduleChange>>();
            }

            for (StudentSchedule studentSchedule : studentSchedules.values()) {
                /*
                 * If there is not an add or drop change for this course, create one
                 */
                if (!m_studentScheduleChanges.containsKey(studentSchedule.getSectionOid())) {
                    List<StudentScheduleChange> scheduleChangeList =
                            createStudentScheduleChangeList(studentOid, studentSchedule, true);

                    m_studentScheduleChanges.put(studentSchedule.getSectionOid(), scheduleChangeList);
                }
            }

            return m_studentScheduleChanges.size();
        }

        /**
         * Creates a list of StudentScheduleChanges for a particular StudentSchedule entry.
         * An 'Add' record is always created. A 'Drop' record is added to the list if requested
         *
         * @param studentOid String
         * @param studentSchedule StudentSchedule
         * @param addDrop boolean
         * @return List<StudentScheduleChange>
         */
        private List<StudentScheduleChange> createStudentScheduleChangeList(String studentOid,
                                                                            StudentSchedule studentSchedule,
                                                                            boolean addDrop) {
            List<StudentScheduleChange> scheduleChangeList = null;
            I4SeeCATEStudentCourse cateData = (I4SeeCATEStudentCourse) getData();

            try {
                PlainDate entryDate = null;
                PlainDate exitDate = null;
                String entryCode = null;
                String exitCode = null;

                String entryDateName = getData().translateAliasToJavaName(I4SEE_230_CATE_ENTRY_DATE, true);
                String exitDateName = getData().translateAliasToJavaName(I4SEE_250_CATE_EXIT_DATE, true);
                String entryCodeFieldName = getData().translateAliasToJavaName(I4SEE_240_CATE_ENTRY_CODE, true);
                String exitCodeFieldName = getData().translateAliasToJavaName(I4SEE_260_CATE_EXIT_CODE, true);

                // Search StudentProgramParticipation for matching program Id.
                String programId = (String) studentSchedule.getSection().getSchoolCourse().getCourse()
                        .getFieldValueByAlias(I4SEE_1740_PROGRAM_ID);
                List<StudentProgramParticipation> programs = cateData.getStudentProgramParticipation(studentOid);
                if (programs != null) {
                    boolean found = false;
                    for (StudentProgramParticipation program : programs) {
                        if (programId == null
                                || programId.equals(program.getFieldValueByAlias(I4SEE_1710_PROGRAM_ID))) {
                            entryCode = (String) WebUtils.getProperty(program, entryCodeFieldName);
                            exitCode = (String) WebUtils.getProperty(program, exitCodeFieldName);
                            entryDate = (PlainDate) WebUtils.getProperty(program, entryDateName);
                            exitDate = (PlainDate) WebUtils.getProperty(program, exitDateName);
                            found = true;
                            m_programExitDate.put(programId, exitDate);
                            break;
                        }
                    }
                    if (!found) {
                        StudentProgramParticipation program = programs.get(0);
                        entryCode = (String) WebUtils.getProperty(program, entryCodeFieldName);
                        exitCode = (String) WebUtils.getProperty(program, exitCodeFieldName);
                        entryDate = (PlainDate) WebUtils.getProperty(program, entryDateName);
                        exitDate = (PlainDate) WebUtils.getProperty(program, exitDateName);
                    }
                }

                StudentScheduleChange scheduleChange =
                        new StudentScheduleChange(getData().getBroker().getPersistenceKey());
                scheduleChange.setStudentOid(studentOid);
                scheduleChange.setScheduleOid(studentSchedule.getScheduleOid());
                scheduleChange.setMasterScheduleOid(studentSchedule.getSectionOid());
                scheduleChange.setScheduleDisplay(studentSchedule.getScheduleDisplay());

                scheduleChange.setDate(entryDate);
                scheduleChange.setChangeTypeCode(StudentScheduleChange.CODE_ADD);

                scheduleChange.setFieldValueByAlias(I4SEE_240_CATE_ENTRY_CODE_ALT, entryCode);
                scheduleChange.setFieldValueByAlias(I4SEE_260_CATE_EXIT_CODE_ALT, exitCode);

                scheduleChangeList = new ArrayList<StudentScheduleChange>();
                scheduleChangeList.add(scheduleChange);

                if (exitDate != null && addDrop) {
                    scheduleChange = (StudentScheduleChange) scheduleChange.cloneBean();
                    scheduleChange.setDate(exitDate);
                    scheduleChange.setChangeTypeCode(StudentScheduleChange.CODE_DROP);
                    scheduleChangeList.add(scheduleChange);
                }
            } catch (X2BaseException e) {
                e.printStackTrace();
            }

            return scheduleChangeList;
        }

        /**
         * Load student schedules.
         *
         * @param studentOid String
         * @return void
         */
        private void loadStudentSchedules(String studentOid) {
            X2Criteria criteria = new X2Criteria();
            String javaName = getData().translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);

            criteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, studentOid);
            criteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    javaName, getData().getBroker().getPersistenceKey());
            BeanQuery query = new BeanQuery(StudentSchedule.class, criteria, false);

            int initialMapSize = getData().getBroker().getCount(query);
            m_studentSchedules =
                    getData().getBroker().getMapByQuery(query, StudentSchedule.COL_SECTION_OID, initialMapSize);
        }

        /**
         * Load student schedule changes.
         *
         * @param studentOid String
         * @return void
         */
        private void loadStudentScheduleChanges(String studentOid) {
            Criteria criteria = new Criteria();
            I4SeeCATEStudentCourse cateData = (I4SeeCATEStudentCourse) getData();

            criteria.addEqualTo(StudentScheduleChange.COL_STUDENT_OID, studentOid);
            criteria.addGreaterOrEqualThan(StudentScheduleChange.COL_EFFECTIVE_DATE, cateData.m_firstDayDate);
            criteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

            QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, criteria);

            int initialMapSize = getData().getBroker().getCount(query);
            m_studentScheduleChanges = getData().getBroker().getGroupedCollectionByQuery(query,
                    StudentScheduleChange.COL_MASTER_SCHEDULE_OID, initialMapSize);

            /*
             * ensure all changes include an 'Add' record
             */
            List<StudentScheduleChange> scheduleChangeList = null;
            for (String mstOid : m_studentScheduleChanges.keySet()) {
                scheduleChangeList = m_studentScheduleChanges.get(mstOid);
                boolean isAddExists = false;
                for (StudentScheduleChange scheduleChange : scheduleChangeList) {
                    if (scheduleChange.getChangeTypeCode().equals(StudentScheduleChange.CODE_ADD)
                            && scheduleChange.getEffectiveDate().before(cateData.m_reportDate)) {
                        isAddExists = true;
                        break;
                    } else if (scheduleChange.getChangeTypeCode().equals(StudentScheduleChange.CODE_DROP)
                            && scheduleChange.getEffectiveDate().after(cateData.m_reportDate)) {
                        break;
                    }
                }

                /*
                 * If there was no add record, create one
                 */
                if (!isAddExists) {
                    StudentScheduleChange studentScheduleChange = scheduleChangeList.get(0);
                    StudentSchedule studentSchedule = new StudentSchedule(getData().getBroker().getPersistenceKey());
                    studentSchedule.setScheduleOid(studentScheduleChange.getScheduleOid());
                    studentSchedule.setSectionOid(studentScheduleChange.getMasterScheduleOid());
                    studentSchedule.setScheduleDisplay(studentScheduleChange.getScheduleDisplay());

                    scheduleChangeList.addAll(createStudentScheduleChangeList(studentOid, studentSchedule, false));
                }
            }
        }
    }

    /**
     * Converts TRM_BASE_TERM_COVER_MAP to TermID
     * <p>
     * CATE Valid Codes are:
     * <ul>
     * <li>1 - 1st Semester
     * <li>2 - 2nd Semester
     * <li>30 - Full Year
     * </ul>
     * .
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1430TermId implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.x2dev.sis.
         *      tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String fieldValue = "";

            /*
             * Convert the TRM_BASE_TERM_COVER_MAP to one of the valid codes. The map is a string of
             * 1's and 0's. A single digit indicates FULL YEAR. Multiple characters indicate the
             * number of terms and whether the class is in session or not for those terms.
             */
            String termMap = (String) WebUtils.getProperty(((I4SeeEntity) entity).getStudentScheduleChangeList().get(0),
                    StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + Section.REL_SCHEDULE_TERM
                            + PATH_DELIMITER + ScheduleTerm.COL_BASE_TERM_MAP);
            Integer termsPerYear =
                    (Integer) WebUtils.getProperty(((I4SeeEntity) entity).getStudentScheduleChangeList().get(0),
                            StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER + Section.REL_SCHEDULE_TERM
                                    + PATH_DELIMITER + ScheduleTerm.COL_BASE_TERMS_PER_YEAR);

            try {
                /*
                 * Ensure the termMap is the correct length. This is done at the bean level, so this
                 * should never happen
                 */
                if (termMap == null || termsPerYear == null || termMap.length() != termsPerYear.intValue()) {
                    throw new Exception();
                }

                /*
                 * Not knowing how to handle tri-mesters, we'll assume that 1) the CATE schools will
                 * not have them, or 2) if they do, then we'll just take the 1st and last character
                 * from the map to see which 'semester' is in session, essentially ignoring the
                 * middle term. This is true for 'quarters' as well.
                 */
                if (!termMap.contains("0")) {
                    fieldValue = TERM_ID_FULL_YEAR; // full year
                } else if (termMap.startsWith("1")) {
                    fieldValue = TERM_ID_FIRST_SEMESTER;
                } else if (termMap.endsWith("1")) {
                    fieldValue = TERM_ID_SECOND_SEMESTER;
                }
            } catch (Exception e) {
                entity.addRetrievalError(field.getFieldId(), new StateReportValidationError(entity,
                        field, "ERROR", VALIDATION_INVALID_VALUE));
            }

            return fieldValue;
        }

    }

    /**
     * Retrieves the local class code and reduces it to MAX characters, if possible, by
     * removing leading zeros and non-numeric characters (e.g. periods, hypens, etc.)
     *
     * - 1st leading zeros are removed (e.g. '0000123-0023' == '00123-0023')
     * - 2nd duplicate hyphens are removed (e.g. '1234567-A-1' == '1234567A-1')
     * - 3rd leading zeros after a hyphen are removed (e.g. '1234567-0023' == '1234567-23')
     * - 4th periods are removed (e.g. '1234567.A-1' == '1234567A-1')
     * - Last resort ... the course ID needs to be changed.
     */
    protected class Retrieve1470LocalClassCode implements FieldRetriever {
        String m_javaName;

        /**
         * Instantiates a new retrieve 1470 local class code.
         *
         * @param javaName String
         */
        Retrieve1470LocalClassCode(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            StudentScheduleChange studentScheduleChange = ((I4SeeEntity) entity).getStudentScheduleChangeList().get(0);
            MasterSchedule masterSchedule = studentScheduleChange.getMasterSchedule();
            String courseId = (String) WebUtils.getProperty(masterSchedule, m_javaName);

            /*
             * A bit of a kludge....
             * Go to the School Course record and see if there's an alias called 'i4see 1470
             * prefix'.
             * If there is, and the value is not empty, then prepend the courseID with that value.
             * This is to differentiate identical courses offered at more than one school. (E.g.
             * Nashua NH offers the same course at both North and South High Schools)
             */
            if (!StringUtils.isEmpty(m_I4see1470Prefix)) {
                try {
                    SchoolCourse schoolCourse = masterSchedule.getSchoolCourse();
                    String value = (String) WebUtils.getProperty(schoolCourse, m_I4see1470Prefix);
                    if (!StringUtils.isEmpty(value)) {
                        courseId = value + courseId;
                    }
                } catch (Exception e) {
                    // ignore errors
                }
            }

            /*
             * Remove any leading zeros
             */
            while (courseId.length() > field.getMaxLength() && courseId.charAt(0) == '0') {
                courseId = courseId.substring(1);
            }

            /*
             * Remove duplicate hyphens
             */
            while (courseId.length() > field.getMaxLength() && courseId.matches("([^-]*-[^-]*)*-[^-]*")) {
                courseId = courseId.replaceFirst("-", "");
            }

            /*
             * Removed zeros after a hyphen
             */
            while (courseId.length() > field.getMaxLength() && courseId.contains("-0")) {
                courseId = courseId.replace("-0", "-");
            }

            /*
             * Removed periods
             */
            while (courseId.length() > field.getMaxLength() && courseId.contains(".")) {
                courseId = courseId.replace(".", "");
            }

            return courseId;
        }

    }
    /**
     * Retrieve days in attendance for the student. If the count is zero, use the 555 placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1760NumMeetingsInAttendance implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String startDateStr = entity.getFieldValue(I4SEE_230_CATE_ENTRY_DATE);
            PlainDate startDate = null;
            String endDateStr = entity.getFieldValue(I4SEE_250_CATE_EXIT_DATE);
            PlainDate endDate = null;
            try {
                startDate = StringUtils.isEmpty(startDateStr) ? null : new PlainDate(m_dateFormat.parse(startDateStr));
                endDate = StringUtils.isEmpty(endDateStr) ? null : new PlainDate(m_dateFormat.parse(endDateStr));
            } catch (ParseException e) {
                addSetupError("Membership days", "Could not calculate membership: exception\n\t" + e.getMessage());
            }
            I4SeeEntity i4See = (I4SeeEntity) entity;

            StudentScheduleChange studentScheduleChange =
                    getStudentScheduleChange(i4See, StudentScheduleChange.CODE_ADD);

            // See if the student was added before the course begins.
            if (studentScheduleChange != null) {
                MasterSchedule masterSchedule = studentScheduleChange.getMasterSchedule();
                ScheduleTermDate scheduleTermDate =
                        (ScheduleTermDate) masterSchedule.getScheduleTerm().getScheduleTermDates().toArray()[0];
                if (startDate != null) {
                    if (startDate.before(scheduleTermDate.getStartDate())) {
                        startDate = scheduleTermDate.getStartDate();
                    }
                }
            }

            String attendanceCount = getNumMeetingsInAttendance(startDate, endDate, studentScheduleChange);

            int attendanceCountAsInt = 0;

            if (attendanceCount != null) {
                try {
                    attendanceCountAsInt = Integer.parseInt(attendanceCount);
                } catch (NumberFormatException nfe) {
                    // Do nothing, this error has already been logged.
                }
            }

            if (attendanceCountAsInt != 0) {
                if (attendanceCountAsInt < 0) {
                    attendanceCountAsInt = 0; // it's possible for a student to be absent more often
                                              // than the class has met
                }

                attendanceCount = String.valueOf(attendanceCountAsInt);
            } else {
                attendanceCount = "0";
            }

            return attendanceCount;
        }
    }
    /**
     * Returns the entry or exit date for the student.
     */
    protected class Retrieve230_250_EntryExitDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String javaName = translateAliasToJavaName(I4SEE_230_250_DATE, true);
            PlainDate entryExitDate = null;
            I4SeeEntity i4See = (I4SeeEntity) entity;

            /*
             * Get the change code we're looking for
             */
            String changeCode = StudentScheduleChange.CODE_DROP;
            if (field.getFieldId().equals(I4SEE_230_CATE_ENTRY_DATE)) {
                changeCode = StudentScheduleChange.CODE_ADD;
            }

            StudentScheduleChange studentScheduleChange = getStudentScheduleChange(i4See, changeCode);

            if (studentScheduleChange != null) {
                String studentOid = studentScheduleChange.getStudentOid();
                if (getStudentProgramParticipation(studentOid) != null) {
                    try {
                        MasterSchedule masterSchedule = studentScheduleChange.getMasterSchedule();
                        ScheduleTermDate scheduleTermDate =
                                (ScheduleTermDate) masterSchedule.getScheduleTerm().getScheduleTermDates().toArray()[0];
                        entryExitDate = (PlainDate) WebUtils.getProperty(studentScheduleChange, javaName);
                        if (entryExitDate.before(scheduleTermDate.getStartDate())) {
                            entryExitDate = scheduleTermDate.getStartDate();
                        }
                    } catch (X2BaseException e) {
                        entity.addRetrievalError(javaName, new StateReportValidationError(entity, field,
                                VALIDATION_ERROR_ENTRY_EXIT_DATE, e.getMessage()));
                    }
                }
            }

            return entryExitDate;
        }

    }
    /**
     * Returns the entry code for the student.
     */
    protected class Retrieve240_260_EntryExitCode implements FieldRetriever {
        private final String m_javaName;

        /**
         * Instantiates a new retrieve 240 260 entry exit code.
         *
         * @param javaName String
         */
        Retrieve240_260_EntryExitCode(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String entryExitCode = null;
            I4SeeEntity i4See = (I4SeeEntity) entity;
            List<StudentScheduleChange> studentScheduleChangeList = i4See.getStudentScheduleChangeList();

            /*
             * Get the change code we're looking for
             */
            String changeCode = StudentScheduleChange.CODE_DROP;
            if (field.getFieldId().equals(I4SEE_240_CATE_ENTRY_CODE_ALT)) {
                changeCode = StudentScheduleChange.CODE_ADD;
            }

            StudentScheduleChange studentScheduleChange = null;
            for (StudentScheduleChange scheduleChange : studentScheduleChangeList) {
                if (scheduleChange.getChangeTypeCode().equals(changeCode)) {
                    studentScheduleChange = scheduleChange;
                    break;
                }
            }

            if (studentScheduleChange != null) {
                String studentOid = studentScheduleChange.getStudentOid();
                if (getStudentProgramParticipation(studentOid) != null) {
                    String code = null;
                    try {
                        code = (String) WebUtils.getProperty(studentScheduleChange, m_javaName);
                    } catch (X2BaseException e) {
                        e.printStackTrace();
                    }

                    // If the code on the Schedule change record is empty, and the OID exists,
                    // then we may want to go get the appropriate code from the program record
                    // instead.
                    if (StringUtils.isEmpty(code) && studentScheduleChange.getOid() != null) {
                        I4SeeCATEStudentCourse cateData = (I4SeeCATEStudentCourse) entity.getData();

                        String entryCode = null;
                        String exitCode = null;
                        String entryCodeFieldName = cateData.translateAliasToJavaName(I4SEE_240_CATE_ENTRY_CODE, true);
                        String exitCodeFieldName = cateData.translateAliasToJavaName(I4SEE_260_CATE_EXIT_CODE, true);

                        String programId = (String) studentScheduleChange.getMasterSchedule().getSchoolCourse()
                                .getCourse().getFieldValueByAlias(I4SEE_1740_PROGRAM_ID);
                        List<StudentProgramParticipation> programs =
                                cateData.getStudentProgramParticipation(studentOid);
                        if (programs != null) {
                            try {
                                boolean found = false;
                                for (StudentProgramParticipation program : programs) {
                                    if (programId == null
                                            || programId.equals(program.getFieldValueByAlias(I4SEE_1710_PROGRAM_ID))) {
                                        entryCode = (String) WebUtils.getProperty(program, entryCodeFieldName);
                                        exitCode = (String) WebUtils.getProperty(program, exitCodeFieldName);
                                        found = true;
                                        break;
                                    }
                                }
                                if (!found) {
                                    StudentProgramParticipation program = programs.get(0);
                                    entryCode = (String) WebUtils.getProperty(program, entryCodeFieldName);
                                    exitCode = (String) WebUtils.getProperty(program, exitCodeFieldName);
                                }

                                if (changeCode.equals(StudentScheduleChange.CODE_ADD)) {
                                    code = entryCode;
                                } else if (changeCode.equals(StudentScheduleChange.CODE_DROP)) {
                                    code = exitCode;
                                }
                            } catch (X2BaseException e) {
                                AppGlobals.getLog().log(Level.SEVERE, e.getLocalizedMessage(), e);
                            }
                        }
                    }

                    entryExitCode = data.lookupStateValue(StudentScheduleChange.class, m_javaName, code);
                }
            }

            return entryExitCode;
        }
    }

    /**
     * The Class RetrieveCourse.
     */
    /*
     * Retrieves SchoolCourse data from the current entity
     */
    protected class RetrieveCourse implements FieldRetriever {
        private final String m_javaName;

        /**
         * Instantiates a new retrieve course.
         *
         * @param javaName String
         */
        RetrieveCourse(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;

            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentScheduleChange studentScheduleChange = i4seeEntity.getStudentScheduleChangeList().get(0);
            Course course = studentScheduleChange.getMasterSchedule().getSchoolCourse().getCourse();

            value = (String) WebUtils.getProperty(course, m_javaName);

            return value;
        }

    }

    /**
     * The Class RetrieveSchoolNumber.
     */
    /*
     * Retrieves receiving School Number data from the current entity
     */
    protected class RetrieveSchoolNumber implements FieldRetriever {
        private final String m_javaName;

        /**
         * Instantiates a new retrieve school number.
         *
         * @param javaName String
         */
        RetrieveSchoolNumber(String javaName) {
            m_javaName = javaName;
        }

        /**
         * Gets the field value.
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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;

            if (StringUtils.isEmpty((String) getParameter("receivingSchoolNumber"))) {
                I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
                StudentScheduleChange studentScheduleChange = i4seeEntity.getStudentScheduleChangeList().get(0);
                SisSchool school = studentScheduleChange.getMasterSchedule().getSchoolCourse().getSchool();

                value = (String) WebUtils.getProperty(school, m_javaName);
            } else {
                value = (String) getParameter("receivingSchoolNumber");
            }

            return value;
        }

    }

    /**
     * The Class Validate1700CATEEnrollmentStatus.
     */
    protected class Validate1700CATEEnrollmentStatus implements FieldValidator {

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
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            try {
                if (!VALID_1700_ENROLLMENT_STATUS_VALUES.contains(value)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "I4SEE 1700 Enrollment Status " + value + " not a valid Enrollment Status",
                            "I4SEE 1700 = " + value));
                }
            } catch (Exception e) {
                // This should be handled by the date formatter already.
            }
            return errors;
        }
    }

    /**
     * Validate entry code.
     *
     */
    protected class Validate240EntryCode implements FieldValidator {
        String m_javaName;

        /**
         * Instantiates a new validate 240 entry code.
         *
         * @param javaName String
         */
        public Validate240EntryCode(String javaName) {
            m_javaName = javaName;
        }

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String entryDate = entity.getFieldValue(I4SEE_230_CATE_ENTRY_DATE);

            if ((!StringUtils.isEmpty(entryDate) && StringUtils.isEmpty(value)) ||
                    (StringUtils.isEmpty(entryDate) && !StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "I4SEE 240 Entry Code and I4See 230 Entry Date must either both be empty, or both be filled in",
                        "I4SEE 230 = " + entryDate + ", I4SEE 240 = " + value));
            }

            return errors;
        }
    }


    /*
     * Alias constants
     */
    private static final String STUDENT_NAME = "name view";
    private static final String I4SEE_010_SASID = "i4see 010";
    private static final String I4SEE_030_SAU_NUMBER = "i4see 030";
    private static final String I4SEE_040_DISTRICT_NUMBER = "i4see 040";
    private static final String I4SEE_050_SCHOOL_NUMBER = "i4see 050";

    // @SuppressWarnings("unused")
    // private static final String I4SEE_1600_SAU_NBR_RECEIVE = "i4see 1600";
    //
    // @SuppressWarnings("unused")
    // private static final String I4SEE_1610_DIST_NBR_RECEIVE = "i4see 1610";
    //
    // @SuppressWarnings("unused")
    // private static final String I4SEE_1620_SCHOOL_NBR_RECEIVE = "i4see 1620";
    private static final String I4SEE_100_DOB = "i4see 100";
    private static final String I4SEE_230_250_DATE = "i4see 230/250 CATE";
    private static final String I4SEE_230_CATE_ENTRY_DATE = "i4see 230 CATE";
    private static final String I4SEE_240_CATE_ENTRY_CODE = "i4see 240 CATE";
    private static final String I4SEE_240_CATE_ENTRY_CODE_ALT = "i4see 240 CATE alt";
    private static final String I4SEE_250_CATE_EXIT_DATE = "i4see 250 CATE";
    private static final String I4SEE_260_CATE_EXIT_CODE = "i4see 260 CATE";
    private static final String I4SEE_260_CATE_EXIT_CODE_ALT = "i4see 260 CATE alt";
    private static final String I4SEE_1470_LOCAL_CLASS_CODE = "i4see 1470";
    private static final String I4SEE_1470_PREFIX = "i4see 1470 prefix";
    private static final String I4SEE_1420_SCHOOL_YEAR = "i4see 1420";
    private static final String I4SEE_1430_TERM_ID = "i4see 1430";
    private static final String I4SEE_1700_CATE_ENROLLMENT_STATUS = "i4see 1700";
    private static final String I4SEE_1710_PROGRAM_ID = "i4see 1710";
    private static final String I4SEE_1740_PROGRAM_ID = "i4see 1740";
    private static final String I4SEE_1750_CONCENTRATOR = "i4see 1750";
    private static final String I4SEE_1755_TOTAL_COMP_COMPLETED = "i4see 1755";
    private static final String I4SEE_1760_NUM_MEETINGS_IN_ATTENDANCE = "i4see 1760";
    private static final String I4SEE_CATE_CONTEXT = "i4see CATE CONTEXT";

    static final Collection VALID_1700_ENROLLMENT_STATUS_VALUES = Arrays.asList("1", "4", "9", "18", "22");

    /*
     * Field aliases for "adjusted" fields (these fields may have manually entered values that
     * override/adjust calculated values).
     *
     * Note: The alias for the adjusted status is optional (is does not have to be defined in the
     * Data Dictionary).
     */
    private static final String ADJUSTED_SCHOOL_NUMBER_FIELD = "i4see Adj School Number";
    /*
     * Field alias/field value for querying options on the export
     */
    private static final String I4SEE_CATE_STATUS_FIELD = "i4see CATE Status";
    private static final String I4SEE_STATUS_FIELD_REPORT_CODE = "Report";

    /**
     * Name for default 210 parameter (Enrollment Status)
     */
    public static final String DEFAULT_I4SEE_210_PARAM = "defaultI4see210";
    /**
     * Name for default 220 parameter (Town Responsible)
     */
    public static final String DEFAULT_I4SEE_220_PARAM = "defaultI4see220";
    /**
     * Name for default 225 parameter (District Responsible)
     */
    public static final String DEFAULT_I4SEE_225_PARAM = "defaultI4see225";
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
     * Name for the "include student names" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";



    /**
     * Name for the "include summer withdrawals" parameter. The value is a Boolean.
     */
    public static final String INCLUDE_SUMMER_WITHDRAWALS_PARAM = "includeSummerWithdrawals";
    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String REPORT_DATE_PARAM = "reportDate";

    /**
     * Reference Table OID for CATE Entry codes
     */
    public static final String CATE_ENTRY_CODES_REF_OID = "i4see CATE Entry Codes";

    /**
     * Reference Table OID for CATE Exit codes
     */
    public static final String CATE_EXIT_CODES_REF_OID = "i4see CATE Exit Codes";

    /**
     * Student Program Participation identifier for CATE
     *
     * see StudentProgramParticipation.COL_PROGRAM_CODE
     *
     */
    public static final String I4SEE_CATE_PROGRAM_IDENTIFIER = "CATE";

    /**
     * Name for the "require report status" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_REPORT_STATUS_PARAM = "requireReportStatus";
    /**
     * Name for the "require at lease one member day" parameter. The value is a Boolean.
     */
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    /**
     * Name for the "summer end date" parameter. The corresponding values is a java.sql.Date object.
     */
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";
    /**
     * Name for the "summer start date" parameter. The corresponding values is a java.sql.Date
     * object.
     */
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";
    /**
     * Name for the "update student records" parameter. The value is a Boolean.
     */
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";

    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String REGEX_NUMERIC = "[0-9]*";
    private static final String VALIDATION_INVALID_VALUE = "Invalid value";
    private static final String VALIDATION_ERROR_ENTRY_EXIT_DATE = "Error retrieving Entry/Exit date";
    private static final String OFF_TRACK_CODE = "OFTR";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Map<String, Collection<StudentAttendance>> m_absences;

    protected String m_adjustedSchoolCode;

    protected SimpleDateFormat m_dateFormat;

    protected EnrollmentManager m_enrollmentManager;

    protected String m_defaultI4see210;

    protected String m_defaultI4see220;

    protected String m_defaultI4see225;

    protected String m_I4see1470Prefix;

    protected PlainDate m_firstDayDate;

    protected Set m_firstDayMembers;

    protected TreeMap m_gradeLevelMap;

    protected Boolean m_includeStudentNames;

    protected Converter m_integerConverter;

    protected Map<String, Collection<Race>> m_raceCodeMap;

    protected PlainDate m_reportDate;

    protected Set<String> m_activeScheduleOids;

    protected String m_reportStatusField;

    protected Integer m_reportType;

    protected Map<String, LinkedList<StudentProgramParticipation>> m_studentProgramParticipation;

    protected Map<String, Schedule> m_scheduleMap;

    protected Map<String, SisSchool> m_schoolMap;

    protected HashMap m_schoolsToCalendars;

    protected Collection m_suspensionInCodes;

    protected Collection m_suspensionOutCodes;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "I4SEE CATE STUDENT COURSE";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Returns the number of days the student has attended this class
     * report date.
     *
     * @param startDateStr PlainDate
     * @param endDateStr PlainDate
     * @param studentScheduleChange StudentScheduleChange
     * @return String
     */
    public String getNumMeetingsInAttendance(PlainDate startDateStr,
                                             PlainDate endDateStr,
                                             StudentScheduleChange studentScheduleChange) {
        MasterSchedule masterSchedule = studentScheduleChange.getMasterSchedule();
        ScheduleTermDate scheduleTermDate =
                (ScheduleTermDate) masterSchedule.getScheduleTerm().getScheduleTermDates().toArray()[0];

        String count = "0";
        SisStudent student = studentScheduleChange.getStudent();

        // Check the active schedule for the school.
        SisSchool school = m_schoolMap.get(student.getSchoolOid());
        Schedule schedule = null;

        if (school != null) {
            schedule = m_scheduleMap.get(school.getOid());
            if (schedule != null) {
                try {
                    Criteria criteria = new Criteria();
                    criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());

                    criteria.addEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);

                    PlainDate startDate = scheduleTermDate.getStartDate();
                    PlainDate endDate = m_reportDate;

                    if (startDateStr != null) {
                        if (startDate.before(startDateStr)) {
                            startDate = startDateStr;
                        }
                    }
                    criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);

                    if (endDateStr != null) {
                        if (endDate.after(endDateStr)) {
                            endDate = endDateStr;
                        }
                        if (scheduleTermDate.getEndDate().before(endDate)) {
                            endDate = schedule.getEndDate();
                        }
                    }

                    // make sure the end date is not past the end of the scheduled class
                    if (endDate.after(scheduleTermDate.getEndDate())) {
                        endDate = scheduleTermDate.getEndDate();
                    }

                    criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);

                    /*
                     * Make sure the drop date isn't the same as the add date
                     */
                    if (endDate.after(startDate)) {
                        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
                        int offTrackDays = getBroker().getCount(query);

                        Set<PlainDate> calendarDates = getCalendarDays(m_schoolMap.get(student.getSchoolOid()),
                                student.getCalendarCode(), startDate, endDate);

                        /*
                         * accumulate the number of absence dates that match the schedule
                         */
                        int absences = 0;
                        if (m_absences.get(student.getOid()) != null) {
                            for (StudentAttendance studentAttendance : m_absences.get(student.getOid())) {
                                if (calendarDates.contains(studentAttendance.getDate())) {
                                    absences++;
                                }
                            }
                        }

                        count = String.valueOf(
                                m_enrollmentManager.getMembershipTotal(
                                        student,
                                        calendarDates,
                                        true,
                                        startDate,
                                        endDate,
                                        null) - offTrackDays - absences);
                    }
                } catch (Exception e) {
                    addSetupError("Membership days", "Could not calculate membership: exception\n\t" + e.getMessage());
                }
            }
        }

        return count;
    }

    /**
     * Gets the report type.
     *
     * @return Object
     */
    public Integer getReportType() {
        return m_reportType;
    }

    /**
     * Returns the program record for the current student.
     *
     * return StudentProgramParticipation
     *
     * @param studentOid String
     * @return List
     */
    public List<StudentProgramParticipation> getStudentProgramParticipation(String studentOid) {
        return m_studentProgramParticipation.get(studentOid);
    }

    /**
     * Gets the value delimiter.
     *
     * @return Character
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getValueDelimiter()
     */
    @Override
    public Character getValueDelimiter() {
        return Character.valueOf(',');
    }

    /**
     * Gets the use value delimiters.
     *
     * @return boolean
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getUseValueDelimiters()
     */
    @Override
    public boolean getUseValueDelimiters() {
        return true;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Load initialization data
         */
        initializeFields();

        /*
         * Get core parameters
         */
        m_includeStudentNames = (Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM);
        if (m_includeStudentNames == null) {
            m_includeStudentNames = Boolean.FALSE;
        }

        m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_integerConverter =
                ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);

        /*
         * Set the field definition array
         */
        ArrayList<FieldDefinition> fieldDefinitions =
                new ArrayList<FieldDefinition>(m_includeStudentNames.booleanValue() ? 26 : 25);

        if (m_includeStudentNames.booleanValue()) {
            fieldDefinitions.add(getName());
        }

        fieldDefinitions.add(getI4see010_Sasid());
        fieldDefinitions.add(getI4see100_Dob());

        fieldDefinitions.add(getI4see1600_SauNumber());
        fieldDefinitions.add(getI4see1610_DistrictNumber());
        fieldDefinitions.add(getI4see1620_SchoolNumber());

        fieldDefinitions.add(getI4See1470_LocalClassCode());
        fieldDefinitions.add(getI4See1740_ProgramId());
        fieldDefinitions.add(getI4See1420_SchoolYear());
        fieldDefinitions.add(getI4See1430_TermId());

        fieldDefinitions.add(getI4see1750_Concentrator());
        fieldDefinitions.add(getI4see1755_TotalCompCompleted());
        fieldDefinitions.add(getI4see1700_EnrollmentStatus());


        fieldDefinitions.add(getI4see230_EntryDate());
        fieldDefinitions.add(getI4see240_EntryCode());
        fieldDefinitions.add(getI4see250_ExitDate());
        fieldDefinitions.add(getI4see260_ExitCode());

        fieldDefinitions.add(getI4see1760_NumMeetingsInAttendance());

        setFieldDefinitions(fieldDefinitions);

        // for (FieldDefinition field : fieldDefinitions)
        // {
        // addSetupError(field.getFieldId(), field.getBeanPath());
        // }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Load Schools
             */
            loadSchools();

            /*
             * Load Active Schedules
             */
            loadActiveSchedules();

            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            studentCriteria.addAndCriteria(getStudentSchoolCriteria());
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort != null ? sort.intValue() : 0) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            translateAliasToJavaName(I4SEE_050_SCHOOL_NUMBER, true));
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(I4SeeEntity.class);

            /*
             * get all of the student program participation records for CATE
             */
            loadCATEStudentProgramParticipation();

            /*
             * Load absence days for all students included in the export.
             */
            loadAbsenceDaysMaps(getStudentCriteria());

        }
    }

    /**
     * Gets the appropriate schedule change for the changeCode.
     *
     * @param i4See I4SeeEntity
     * @param changeCode String
     * @return StudentScheduleChange
     */
    protected StudentScheduleChange getStudentScheduleChange(I4SeeEntity i4See, String changeCode) {
        List<StudentScheduleChange> studentScheduleChangeList = i4See.getStudentScheduleChangeList();
        StudentScheduleChange studentScheduleChange = null;

        for (StudentScheduleChange scheduleChange : studentScheduleChangeList) {
            if (scheduleChange.getChangeTypeCode().equals(changeCode)) {
                studentScheduleChange = scheduleChange;
                break;
            }
        }
        return studentScheduleChange;
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     *
     * @param school SisSchool
     * @param calendar String
     * @param startDate optional - first day of dates to include in set
     * @param endDate optional - last day of dates to include in set
     * @return Set of PlainDate objects
     */
    private Set getCalendarDays(SisSchool school, String calendar, PlainDate startDate, PlainDate endDate) {
        if (startDate == null) {
            startDate = m_scheduleMap.get(school.getOid()).getStartDate();
        }
        if (endDate == null) {
            endDate = m_reportDate;
        }

        String key = school.getOid() + startDate.toString() + endDate.toString();
        if (!m_schoolsToCalendars.containsKey(key)) {
            Map calendarData = m_enrollmentManager.getCalendarLookup(school, startDate, endDate);
            m_schoolsToCalendars.put(key, calendarData);
        }

        return (Set) ((Map) m_schoolsToCalendars.get(key)).get(calendar);
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getStudentSchoolCriteria() {
        /*
         * Primary students
         */
        Criteria primaryCriteria = new Criteria();
        if (isSchoolContext()) {
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + X2BaseBean.COL_OID,
                    getSchool().getOid());
        } else {
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            primaryCriteria.addEqualTo(SisStudent.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        return primaryCriteria;
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

        Boolean requireReportStatus = (Boolean) getParameter(REQUIRE_REPORT_STATUS_PARAM);
        if (requireReportStatus.booleanValue()) {
            if (!StringUtils.isEmpty(m_reportStatusField)) {
                userCriteria.addEqualTo(m_reportStatusField, I4SEE_STATUS_FIELD_REPORT_CODE);
            }
        }

        /*
         * Limit search to courses with CIP codes (only true for CATE courses)
         */
        // This is done through the entity filter, as it would not work here!

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

        return userCriteria;
    }

    /**
     * Returns a query that finds the students who withdrew during the given date range (filtered
     * by school as appropriate).
     *
     * @return A SubQuery that returns the student OID from StudentEnrollment records
     */
    // private SubQuery getStudentWithdrawalQuery(PlainDate startDate, PlainDate endDate)
    // {
    // Criteria enrollmentCriteria = new Criteria();
    // enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE,
    // StudentEnrollment.WITHDRAWAL);
    // enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
    // enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, endDate);
    //
    // if (isSchoolContext())
    // {
    // enrollmentCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
    // }
    //
    // return new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID,
    // enrollmentCriteria);
    // }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        /*
         * Resolve some support aliases, log an error and stop the export if an alias is not found.
         */
        m_adjustedSchoolCode = translateAliasToJavaName(ADJUSTED_SCHOOL_NUMBER_FIELD, true);

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayDate = getCurrentContext().getStartDate();
        // m_firstDayMembers = m_enrollmentManager.getMembershipAsOf(m_firstDayDate,
        // getOrganization());
        m_reportStatusField = translateAliasToJavaName(I4SEE_CATE_STATUS_FIELD, true);
        m_schoolsToCalendars = new HashMap();
        m_I4see1470Prefix = translateAliasToJavaName(I4SEE_1470_PREFIX, false);
    }

    /**
     * Loads a map by student of absence days for that student.
     *
     * @param studentCriteria Criteria
     */
    private void loadAbsenceDaysMaps(Criteria studentCriteria) {
        /*
         * Part I. Absence days from attendance.
         */
        // subQuery - Students.
        ReportQueryByCriteria studentQuery =
                new ReportQueryByCriteria(SisStudent.class, new String[] {X2BaseBean.COL_OID}, studentCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(studentQuery);
        List<String> studentOids = new ArrayList<String>();
        try {
            while (iterator.hasNext()) {
                Object[] code = (Object[]) iterator.next();
                studentOids.add((String) code[0]);
            }
        } finally {
            iterator.close();
        }

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentAttendance.COL_STUDENT_OID, studentOids);
        criteria.addGreaterOrEqualThanField(StudentAttendance.COL_DATE,
                StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);
        criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
        criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentAttendance.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL + PATH_DELIMITER
                    + X2BaseBean.COL_OID, getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);

        // Build the map of student to courses.
        m_absences = getBroker().getGroupedCollectionByQuery(query, StudentAttendance.COL_STUDENT_OID,
                getBroker().getCount(query));
    }


    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        m_activeScheduleOids = new HashSet<String>();
        m_scheduleMap = new HashMap();
        Collection<SisSchool> schools = m_schoolMap.values();
        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
            if (school.getActiveSchedule() != null) {
                m_activeScheduleOids.add(school.getActiveSchedule().getOid());
            }
        }
    }

    /**
     * Returns a Collection of Student Program Participation records.
     *
     * @return Collection of StudentEnrollment records for any date on or after the start of school
     */
    private void loadCATEStudentProgramParticipation() {
        String cateContext = translateAliasToJavaName(I4SEE_CATE_CONTEXT, true);
        Criteria criteria = new Criteria();
        // criteria.addAndCriteria(studentScheduleCriteria);
        if (isSchoolContext()) {
            criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER + SisStudent.REL_SCHOOL
                    + PATH_DELIMITER + X2BaseBean.COL_OID, getSchool().getOid());
        }
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, I4SEE_CATE_PROGRAM_IDENTIFIER);
        criteria.addEqualTo(cateContext, getCurrentContext().getContextId());

        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);
        query.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);

        m_studentProgramParticipation = getBroker().getGroupedCollectionByQuery(query,
                StudentProgramParticipation.COL_STUDENT_OID, getBroker().getCount(query));
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new Criteria());
        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

    /**
     * Build Field definition for the student SASID (i4see 010).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see010_Sasid() {
        FieldDefinition field = new FieldDefinition(I4SEE_010_SASID,
                SisStudent.COL_STATE_ID,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build Field definition for the student DOB (i4see 100).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getI4see100_Dob() {
        FieldDefinition field = new FieldDefinition(I4SEE_100_DOB,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_DOB,
                null,
                false,
                8,
                10,
                null,
                m_dateFormat,
                null,
                null,
                null);
        return field;
    }

    /**
     * Build a field definition for i4see 1420 (school year).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4See1420_SchoolYear() {
        String javaName = translateAliasToJavaName(I4SEE_1420_SCHOOL_YEAR, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1420_SCHOOL_YEAR,
                SisStudent.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER +
                        SisOrganization.REL_CURRENT_CONTEXT + PATH_DELIMITER + javaName,
                null,
                false,
                4,
                4,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1430 term id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1430_TermId() {
        String javaName = translateAliasToJavaName(I4SEE_1430_TERM_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1430_TERM_ID,
                LABEL_PREFIX_CHAR + StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHEDULE_TERM + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                2,
                null,
                null,
                new Retrieve1430TermId(),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1470 local class code.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1470_LocalClassCode() {
        String javaName = translateAliasToJavaName(I4SEE_1470_LOCAL_CLASS_CODE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1470_LOCAL_CLASS_CODE,
                LABEL_PREFIX_CHAR + StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + PATH_DELIMITER + javaName,
                null,
                false,
                1,
                10,
                null,
                null,
                new Retrieve1470LocalClassCode(javaName),
                null,
                null);
        return field;
    }

    /**
     * Build a Field Definition for i4see 030 (sau number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1600_SauNumber() {
        String javaName = translateAliasToJavaName(I4SEE_030_SAU_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_030_SAU_NUMBER,
                SisStudent.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER +
                        javaName,
                null,
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 040 (district number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1610_DistrictNumber() {
        String javaName = translateAliasToJavaName(I4SEE_040_DISTRICT_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_040_DISTRICT_NUMBER,
                SisStudent.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ORGANIZATION1 + PATH_DELIMITER +
                        javaName,
                null,
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 050 (school number).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1620_SchoolNumber() {
        String javaName = translateAliasToJavaName(I4SEE_050_SCHOOL_NUMBER, true);
        FieldDefinition field = new FieldDefinition(I4SEE_050_SCHOOL_NUMBER,
                LABEL_PREFIX_CHAR +
                        StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_SCHOOL + PATH_DELIMITER +
                        javaName,
                (String) getParameter("receivingSchoolNumber"),
                false,
                1,
                5,
                REGEX_NUMERIC,
                null,
                new RetrieveSchoolNumber(javaName),
                null,
                null);

        return field;
    }

    /**
     * Gets the i 4 see 1700 enrollment status.
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see1700_EnrollmentStatus() {
        String javaName = translateAliasToJavaName(I4SEE_1700_CATE_ENROLLMENT_STATUS, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1700_CATE_ENROLLMENT_STATUS,
                javaName,
                null,
                false,
                0,
                2,
                REGEX_NUMERIC,
                null,
                null,
                new Validate1700CATEEnrollmentStatus(),
                null);

        return field;
    }

    /**
     * Gets the i 4 see 1740 program id.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4See1740_ProgramId() {
        String javaName = translateAliasToJavaName(I4SEE_1740_PROGRAM_ID, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1740_PROGRAM_ID,
                LABEL_PREFIX_CHAR + StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        javaName,
                null,
                false,
                0,
                10,
                REGEX_NUMERIC,
                null,
                new RetrieveCourse(javaName),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1750 concentrator.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4see1750_Concentrator() {
        String javaName = translateAliasToJavaName(I4SEE_1750_CONCENTRATOR, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1750_CONCENTRATOR,
                LABEL_PREFIX_CHAR + StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        javaName,
                null,
                false,
                0,
                1,
                null,
                null,
                new RetrieveCourse(javaName),
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1755 total comp completed.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4see1755_TotalCompCompleted() {
        String javaName = translateAliasToJavaName(I4SEE_1755_TOTAL_COMP_COMPLETED, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1755_TOTAL_COMP_COMPLETED,
                javaName,
                null,
                false,
                0,
                3,
                REGEX_NUMERIC,
                null,
                null,
                null,
                null);
        return field;
    }

    /**
     * Gets the i 4 see 1760 num meetings in attendance.
     *
     * @return Field definition
     */
    protected FieldDefinition getI4see1760_NumMeetingsInAttendance() {
        String javaName = translateAliasToJavaName(I4SEE_1760_NUM_MEETINGS_IN_ATTENDANCE, true);
        FieldDefinition field = new FieldDefinition(I4SEE_1760_NUM_MEETINGS_IN_ATTENDANCE,
                LABEL_PREFIX_CHAR + javaName,
                null,
                false,
                0,
                3,
                REGEX_NUMERIC,
                null,
                new Retrieve1760NumMeetingsInAttendance(),
                null,
                null);
        return field;
    }

    /**
     * Build a Field Definition for i4see 230 (entry date).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see230_EntryDate() {
        FieldDefinition field = new FieldDefinition(I4SEE_230_CATE_ENTRY_DATE,
                LABEL_PREFIX_CHAR + SisStudent.REL_ENROLLMENTS + "." + StudentEnrollment.COL_ENROLLMENT_DATE,
                null,
                false,
                0,
                10,
                null,
                m_dateFormat,
                new Retrieve230_250_EntryExitDate(),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 240 (entry code).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see240_EntryCode() {
        String javaName = translateAliasToJavaName(I4SEE_240_CATE_ENTRY_CODE_ALT, true);
        FieldDefinition field = new FieldDefinition(I4SEE_240_CATE_ENTRY_CODE_ALT,
                LABEL_PREFIX_CHAR + I4SEE_240_CATE_ENTRY_CODE_ALT,
                null,
                false,
                0,
                3,
                null,
                null,
                new Retrieve240_260_EntryExitCode(javaName),
                new Validate240EntryCode(javaName),
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 250 (exit date).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see250_ExitDate() {
        FieldDefinition field = new FieldDefinition(I4SEE_250_CATE_EXIT_DATE,
                SisStudent.REL_ENROLLMENTS + "." + StudentEnrollment.COL_ENROLLMENT_DATE,
                null,
                false,
                0,
                10,
                null,
                m_dateFormat,
                new Retrieve230_250_EntryExitDate(),
                null,
                null);

        return field;
    }

    /**
     * Build a Field Definition for i4see 260 (exit code).
     *
     * @return FieldDefinition
     */
    protected FieldDefinition getI4see260_ExitCode() {
        String javaName = translateAliasToJavaName(I4SEE_260_CATE_EXIT_CODE_ALT, true);
        FieldDefinition field = new FieldDefinition(I4SEE_260_CATE_EXIT_CODE_ALT,
                LABEL_PREFIX_CHAR + I4SEE_260_CATE_EXIT_CODE_ALT,
                null,
                false,
                0,
                3,
                null,
                null,
                new Retrieve240_260_EntryExitCode(javaName),
                null,
                null);

        return field;
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME,
                SisStudent.COL_NAME_VIEW,
                null,
                false,
                1,
                32,
                null,
                null,
                null,
                null,
                null);
        return field;
    }
}
