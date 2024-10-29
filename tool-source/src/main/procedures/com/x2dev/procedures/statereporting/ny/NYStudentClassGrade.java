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

package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Student Class Grade Detail export.
 *
 * @author X2 Development Corporation
 */
public class NYStudentClassGrade extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used for Student Class Grade Detail export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentClassGradeEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        NYStudentClassGrade m_scgData = null;
        List<Transcript> m_studentTranscripts = null;
        List<String> m_studentTranscriptSectionOids = null;
        Collection<StudentScheduleChange> m_studentScheduleChanges = new ArrayList<StudentScheduleChange>();
        ArrayList<MasterSchedule> m_studentSectionsAddedDroppedList = new ArrayList<MasterSchedule>();
        NumberFormat m_numberFormatter = new DecimalFormat("00000000000000000000");

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentClassGradeEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Create a list of StudentScheduleChange for sections that were added or dropped during the
         * class's Schedule Term Dates.
         */
        public void addStudentScheduleChangesAddedDroppedDuring() {
            HashMap<String, MasterSchedule> studentSectionsAddedDroppedMap = new HashMap<String, MasterSchedule>();
            PlainDate changeDate = null;

            // Get the last scheduleChange before its term end date
            // If it is a drop and it is between the start and ends then include
            // If it is an add and it is before the ends date then include

            if (m_studentScheduleChanges != null && m_studentScheduleChanges.size() > 0) {
                ArrayList<MasterSchedule> sections = getAllSections(m_studentScheduleChanges);

                // for (StudentScheduleChange studentScheduleChange : m_studentScheduleChanges)
                for (MasterSchedule section : sections) {
                    String scheduleTermOid = section.getScheduleTermOid();
                    Collection<ScheduleTermDate> scheduleTermDates =
                            m_scgData.m_scheduleTermDatesList.get(scheduleTermOid);

                    if (scheduleTermDates != null && scheduleTermDates.size() > 0) {
                        PlainDate termStartDate = null;
                        PlainDate termEndDate = null;
                        for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                            termStartDate = scheduleTermDate.getStartDate();
                            termEndDate = scheduleTermDate.getEndDate();
                        }

                        StudentScheduleChange lastStudentScheduleChange =
                                getLastScheduleChange(section.getOid(), m_studentScheduleChanges, termEndDate);

                        if (lastStudentScheduleChange != null) {
                            changeDate = lastStudentScheduleChange.getEffectiveDate();

                            boolean addSection = false;
                            if (StudentScheduleChange.CODE_DROP.equals(lastStudentScheduleChange.getChangeTypeCode())) {
                                if ((changeDate.after(termStartDate) || changeDate.equals(termStartDate))
                                        && changeDate.equals(termEndDate)) {
                                    addSection = true;
                                }
                            } else if (StudentScheduleChange.CODE_ADD
                                    .equals(lastStudentScheduleChange.getChangeTypeCode())) {
                                if ((changeDate.before(termEndDate) || changeDate.equals(termEndDate))) {
                                    addSection = true;
                                }
                            }

                            if (addSection) {
                                if (!studentSectionsAddedDroppedMap
                                        .containsKey(lastStudentScheduleChange.getMasterScheduleOid())) {
                                    studentSectionsAddedDroppedMap.put(lastStudentScheduleChange.getMasterScheduleOid(),
                                            lastStudentScheduleChange.getMasterSchedule());
                                }
                            }
                        }
                    }
                }

                if (!studentSectionsAddedDroppedMap.isEmpty()) {
                    m_studentSectionsAddedDroppedList.addAll(studentSectionsAddedDroppedMap.values());
                }
            }
        }


        /**
         * Gets the all sections.
         *
         * @param studentScheduleChanges Collection<StudentScheduleChange>
         * @return Array list
         */
        public ArrayList<MasterSchedule> getAllSections(Collection<StudentScheduleChange> studentScheduleChanges) {
            HashMap<String, MasterSchedule> sectionsMap = new HashMap<String, MasterSchedule>();
            ArrayList<MasterSchedule> sectionsList = new ArrayList<MasterSchedule>();

            if (studentScheduleChanges != null && studentScheduleChanges.size() > 0) {
                for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                    MasterSchedule section = studentScheduleChange.getMasterSchedule();

                    if (!sectionsMap.containsKey(section.getOid())) {
                        sectionsMap.put(section.getOid(), section);
                    }
                }

                sectionsList.addAll(sectionsMap.values());
            }

            return sectionsList;
        }


        /**
         * Gets the last schedule change.
         *
         * @param sectionOid String
         * @param studentScheduleChanges Collection<StudentScheduleChange>
         * @param endDate PlainDate
         * @return Student schedule change
         */
        public StudentScheduleChange getLastScheduleChange(String sectionOid,
                                                           Collection<StudentScheduleChange> studentScheduleChanges,
                                                           PlainDate endDate) {
            StudentScheduleChange lastStudentScheduleChange = null;

            if (sectionOid != null && studentScheduleChanges != null && studentScheduleChanges.size() > 0) {
                long lastStudentScheduleChangeTimestamp = 0;

                for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                    if (sectionOid.equals(studentScheduleChange.getMasterScheduleOid())) {
                        PlainDate changeDate = studentScheduleChange.getEffectiveDate();
                        if (changeDate.before(endDate) || changeDate.equals(endDate)) {
                            if (studentScheduleChange.getTimestamp() > lastStudentScheduleChangeTimestamp) {
                                lastStudentScheduleChange = studentScheduleChange;

                                lastStudentScheduleChangeTimestamp = studentScheduleChange.getTimestamp();
                            }
                        }
                    }
                }
            }

            return lastStudentScheduleChange;
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

            Transcript transcript = getStudentTranscript();
            if (transcript != null) {
                MasterSchedule section = null;

                // Is Not Mocked
                if (transcript.getOid() != null) {
                    section = transcript.getMasterSchedule();
                    name += section.getCourseView();
                } else if (transcript.getMasterScheduleOid() != null) {
                    name += transcript.getOid();
                }
            }

            return name;
        }

        /**
         * Returns the schedule span record based in the current row value.
         *
         * @return StudentScheduleSpan
         */
        public Transcript getStudentTranscript() {
            return m_studentTranscripts.get(getCurrentRow());
        }

        /**
         * Get an ordered list of a student's StudetnScheduleChanges
         * Order by the time-stamp field .
         *
         * @param studentOid String
         */
        public void orderedStudentScheduleChanges(String studentOid) {
            Collection<StudentScheduleChange> studentScheduleChanges =
                    m_scgData.m_studentScheduleChangeList.get(studentOid);

            // Order by sectionOid and them by time-stamp.
            TreeMap<String, StudentScheduleChange> orderedStudentScheduleChanges =
                    new TreeMap<String, StudentScheduleChange>();
            if (studentScheduleChanges != null && studentScheduleChanges.size() > 0) {
                for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                    SchoolCourse schoolCourse = studentScheduleChange.getMasterSchedule().getSchoolCourse();

                    if (schoolCourse != null) {
                        Course course = schoolCourse.getCourse();

                        if (course != null) {
                            // Only include relevant courses.
                            boolean validStateCode = true;
                            String stateCourse = (String) course.getFieldValueByAlias(ALIAS_STATE_CODE);

                            // Check if the course has a value State Code
                            if (StringUtils.isEmpty(stateCourse)) {
                                validStateCode = false;
                            }

                            boolean excludeCourse = false;
                            String excludeCourseFlag = (String) course.getFieldValueByAlias(ALIAS_EXCLUDE_SCG);
                            if ((!StringUtils.isEmpty(excludeCourseFlag)
                                    && BooleanAsStringConverter.TRUE.equals(excludeCourseFlag))
                                    || BooleanAsStringConverter.TRUE
                                            .equals(course.getFieldValueByBeanPath(m_scgData.m_fieldCrsExclude))) {
                                excludeCourse = true;
                            }

                            if (validStateCode && !excludeCourse) {
                                String sectionOid = studentScheduleChange.getMasterScheduleOid();
                                String changeTimeStampStr =
                                        m_numberFormatter.format(studentScheduleChange.getTimestamp());
                                String key = sectionOid + "-" + changeTimeStampStr;
                                orderedStudentScheduleChanges.put(key, studentScheduleChange);
                            }
                        }
                    }
                }

                m_studentScheduleChanges = orderedStudentScheduleChanges.values();
            }
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method finds the student schedule and student schedule change records for the
         * student
         * and generates a list of reportable schedule items.
         * The entity can produce multiple rows from these results.
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

            m_scgData = (NYStudentClassGrade) data;
            SisStudent student = (SisStudent) bean;
            m_studentTranscripts = new ArrayList<Transcript>();
            m_studentTranscriptSectionOids = new ArrayList<String>();

            List<Transcript> studentTranscripts = m_scgData.m_helper.getStudentTranscripts(student);

            orderedStudentScheduleChanges(student.getOid());

            if (studentTranscripts != null) {
                for (Transcript transcript : studentTranscripts) {
                    String transcriptSectionOid = transcript.getMasterScheduleOid();

                    if (transcriptSectionOid != null) {
                        m_studentTranscriptSectionOids.add(transcriptSectionOid);

                        MasterSchedule masterSchedule = transcript.getMasterSchedule();

                        if (masterSchedule != null) {
                            SchoolCourse schoolCourse = transcript.getSchoolCourse();

                            if (schoolCourse != null) {
                                Course course = schoolCourse.getCourse();

                                if (course != null) {
                                    // Only include relevant courses.
                                    boolean validStateCode = true;
                                    String stateCourse = (String) course.getFieldValueByAlias(ALIAS_STATE_CODE);

                                    // Check if the course has a value State Code
                                    if (StringUtils.isEmpty(stateCourse)) {
                                        validStateCode = false;
                                    }

                                    boolean excludeCourse = false;
                                    String excludeCourseFlag = (String) course.getFieldValueByAlias(ALIAS_EXCLUDE_SCG);
                                    if ((!StringUtils.isEmpty(excludeCourseFlag)
                                            && BooleanAsStringConverter.TRUE.equals(excludeCourseFlag))
                                            || BooleanAsStringConverter.TRUE
                                                    .equals(course
                                                            .getFieldValueByBeanPath(m_scgData.m_fieldCrsExclude))) {
                                        excludeCourse = true;
                                    }

                                    if (validStateCode && !excludeCourse) {
                                        /*
                                         * Has the Student dropped the course before the course
                                         * starts or before the end of the course
                                         * There should be only one set of schedule term dates
                                         */

                                        boolean droppedBefore = wasDroppedBefore(masterSchedule);

                                        // don't include the transcript if the section was dropped
                                        if (!droppedBefore) {
                                            m_studentTranscripts.add(transcript);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Get a list of StudentScheduleChange record that were Added or dropped during the
            // school year but don't have a Transcript for it.
            addStudentScheduleChangesAddedDroppedDuring();
            if (m_studentSectionsAddedDroppedList.size() > 0) {
                for (MasterSchedule section : m_studentSectionsAddedDroppedList) {
                    Transcript transcript = new Transcript(null);
                    transcript.setMasterScheduleOid(section.getOid());

                    // Add mocked transcripts for section that were dropped during the schedule term
                    // but don't already have a transcript
                    if (!m_studentTranscriptSectionOids.contains(section.getOid())) {
                        m_studentTranscripts.add(transcript);
                    }
                }
            }
            int rowCount = 0;
            if (m_studentTranscripts != null) {
                rowCount = m_studentTranscripts.size();
            }
            setRowCount(rowCount);
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
         * Check if a section was dropped before the start of a class.
         *
         * @param section MasterSchedule
         * @return boolean
         */
        public boolean wasDroppedBefore(MasterSchedule section) {
            boolean droppedBefore = false;

            if (section != null) {
                String sectionOid = section.getOid();
                Collection<ScheduleTermDate> scheduleTermDates =
                        m_scgData.m_scheduleTermDatesList.get(section.getScheduleTermOid());

                PlainDate termStartDate = null;
                PlainDate termEndDate = null;
                if (scheduleTermDates != null) {
                    for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                        termStartDate = scheduleTermDate.getStartDate();
                        termEndDate = scheduleTermDate.getEndDate();
                    }
                }
                String lastChangeCode = null;
                PlainDate lastChangeDate = null;
                if (termStartDate != null && m_studentScheduleChanges != null && m_studentScheduleChanges.size() > 0) {
                    for (StudentScheduleChange studentScheduleChange : m_studentScheduleChanges) {
                        if (sectionOid.equals(studentScheduleChange.getMasterScheduleOid())) {
                            // What was the last status before the end of the class.
                            PlainDate currChangeDate = studentScheduleChange.getEffectiveDate();
                            if (currChangeDate.before(termEndDate) || currChangeDate.equals(termEndDate)) {
                                lastChangeCode = studentScheduleChange.getChangeTypeCode();
                                lastChangeDate = studentScheduleChange.getEffectiveDate();
                            }
                        }
                    }

                    if (StudentScheduleChange.CODE_DROP.equals(lastChangeCode)) {
                        // What was the last status before the end of the class.
                        if (lastChangeDate != null
                                && (lastChangeDate.before(termStartDate) || lastChangeDate.before(termEndDate))) {
                            droppedBefore = true;
                        }
                    }
                }
            }
            return droppedBefore;
        }
    }

    /**
     * Returns information for the current course from the schedule span.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourse implements FieldRetriever {
        // keys are ctxOid, sklOid, termCode
        Map<String, Map<String, Map<String, ScheduleTerm>>> m_mapCtxTerm = new HashMap();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            Transcript transcript = ((StudentClassGradeEntity) entity).getStudentTranscript();
            String stdOid = entity.getBean().getOid();
            MasterSchedule section = null;
            if (!isMocked(transcript)) {
                section = transcript.getMasterSchedule();
            } else if (transcript.getMasterScheduleOid() != null) {
                section = getMasterScheduleByOid(transcript.getMasterScheduleOid());
            }

            if (section != null) {
                SchoolCourse course = section.getSchoolCourse();
                if (PARAM_DUAL_CREDIT.equals(param)) {
                    Collection<StudentSchedule> sscList = m_studentSchedulesMap.get(stdOid);
                    if (sscList != null && !sscList.isEmpty()) {
                        for (StudentSchedule ssc : sscList) {
                            if (ssc.getSectionOid().equals(section.getOid())) {
                                String dualCreditCode = (String) ssc.getFieldValueByBeanPath(m_fieldSscDualCredit);
                                if (!StringUtils.isEmpty(dualCreditCode)) {
                                    value = data.lookupStateValue(StudentSchedule.class, m_fieldSscDualCredit,
                                            dualCreditCode);
                                }
                                break;
                            }
                        }
                    }
                    if (StringUtils.isEmpty((String) value) && course.getCourse() != null) {
                        value = data.lookupStateValue(Course.class, m_fieldCrsDualCredit,
                                (String) course.getCourse().getFieldValueByBeanPath(m_fieldCrsDualCredit));
                    }
                } else if (PARAM_CODE.equals(param)) {
                    value = course.getNumber();
                } else if (PARAM_CREDIT_ATTEMPT.equals(param)) {
                    value = course.getCredit();
                } else if (PARAM_GPA_IMPACT.equals(param)) {
                    value = CODE_NO;
                    if (course.getGpaIndicator()) {
                        value = CODE_YES;
                    }
                } else if (PARAM_GRADE_DETAIL.equals(param)) {
                    value = course.getFieldValueByAlias(ALIAS_GRADE_DETAIL_CODE);
                } else if (PARAM_MARKING_PERIOD.equals(param)) {
                    value = getMarkingPeriodCode(transcript);
                } else if (PARAM_TERM_CODE.equals(param)) {
                    ScheduleTerm term = section.getScheduleTerm();

                    if (term != null) {
                        String termCode = term.getCode();
                        String stateTermCode =
                                data.lookupReferenceCodeByBeanPath(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (stateTermCode == null) {
                            stateTermCode = termCode;
                        }

                        value = stateTermCode;
                    }
                }
            }
            return value;
        }

        /**
         * Gets the marking period code.
         *
         * @param transcript Transcript
         * @return String
         */
        private String getMarkingPeriodCode(Transcript transcript) {
            int ival = 0;
            ScheduleTerm term = null;

            MasterSchedule section = null;
            if (!isMocked(transcript)) {
                section = transcript.getMasterSchedule();
            } else if (transcript.getMasterScheduleOid() != null) {
                section = getMasterScheduleByOid(transcript.getMasterScheduleOid());
            }

            if (section != null) {
                term = section.getScheduleTerm();
            }

            if (term != null && term.getGradeTermMap() != null) {
                int idx = 0;

                for (char test : term.getGradeTermMap().toCharArray()) {
                    ++idx;
                    if ('1' == test) {
                        ival = idx;
                    }
                }

            }

            return ival == 0 ? null : Integer.toString(ival);
        }
    }

    /**
     * Returns calculated reporting date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveReportingDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((NYStudentClassGrade) data).m_reportingDate;
        }
    }

    /**
     * Returns information for the current class from the schedule span.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSection implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            Transcript transcript = ((StudentClassGradeEntity) entity).getStudentTranscript();

            MasterSchedule section = null;
            if (!isMocked(transcript)) {
                section = transcript.getMasterSchedule();
            } else if (transcript.getMasterScheduleOid() != null) {
                section = getMasterScheduleByOid(transcript.getMasterScheduleOid());
            }

            if (section != null) {
                if (PARAM_SECTION_CODE.equals(param)) {
                    value = section.getSectionNumber();
                }

                String platoonCode = (String) section.getFieldValueByAlias(ALIAS_PLATOON_CODE);
                if (!StringUtils.isEmpty(platoonCode)) {
                    value = platoonCode;
                }
            }

            return value;
        }
    }

    /**
     * Returns information for the current student's transcript from the schedule span.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscript implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            Transcript transcript = ((StudentClassGradeEntity) entity).getStudentTranscript();

            MasterSchedule section = null;
            if (!isMocked(transcript)) {
                section = transcript.getMasterSchedule();
            } else if (transcript.getMasterScheduleOid() != null) {
                section = getMasterScheduleByOid(transcript.getMasterScheduleOid());
            }

            value = null;

            if (transcript != null) {
                String finalGrade = transcript.getFinalGrade();
                Float finalGradeValue = null;
                if (transcript.getFinalGrade() != null) {
                    try {
                        finalGradeValue = Float.valueOf(finalGrade);
                    } catch (NumberFormatException nfe) {
                        // this is ok
                    }
                }
                if (PARAM_ALPHA_GRADE.equals(param)) {
                    if (finalGradeValue == null) {
                        value = finalGrade;
                    }
                } else if (PARAM_LOCATION_CODE.equals(param)) {
                    if (!isMocked(transcript)) {
                        value = transcript.getFieldValueByAlias(ALIAS_SCHOOL_OVERRIDE);
                        if (value == null) {
                            value = transcript.getSchool().getFieldValueByAlias(ALIAS_LOCATION_CODE);
                        }
                    } else {
                        value = section.getSchoolCourse().getSchool().getFieldValueByAlias(ALIAS_LOCATION_CODE);
                    }
                } else if (PARAM_CREDIT_EARNED.equals(param)) {
                    value = BigDecimal.ZERO;
                    if (!isMocked(transcript)) {
                        String classOutcome = (String) getClassOutcomeValue(transcript, finalGrade);
                        if (!classOutcome.matches(REGEX_CLASS_OUTCOME)) {
                            value = transcript.getTotalCredit();
                        }
                    }
                } else if (PARAM_CREDIT_TYPE.equals(param)) {
                    if (!isMocked(transcript)) {
                        value = transcript.getSchoolCourse().getFieldValueByAlias(ALIAS_CREDIT_TYPE_CODE);
                    } else {
                        value = section.getSchoolCourse().getFieldValueByAlias(ALIAS_CREDIT_TYPE_CODE);
                    }
                } else if (PARAM_DISP_GRADE.equals(param)) {
                    value = transcript.getFinalGrade();
                } else if (PARAM_GRADE_COMMENT.equals(param)) {
                    if (!isMocked(transcript)) {
                        // value = transcript.getFieldValueByAlias(ALIAS_STUDENT_GRADE_COMMENT);
                        value = null;
                    }
                } else if (PARAM_ASSESSMENT_IND.equals(param)) {
                    value = CODE_NO;
                    if (!isMocked(transcript)) {
                        SchoolCourse schoolCourse = transcript.getSchoolCourse();
                        if (schoolCourse != null) {
                            Course course = schoolCourse.getCourse();
                            if (course != null) {
                                String assessmentIndicator =
                                        (String) course.getFieldValueByAlias(ALIAS_STATE_ASSESSMENT);
                                if (!StringUtils.isEmpty(assessmentIndicator)
                                        && BooleanAsStringConverter.TRUE.equals(assessmentIndicator)) {
                                    value = CODE_YES;
                                }
                            }
                        }
                    }
                } else if (PARAM_NUM_GRADE.equals(param)) {
                    if (finalGradeValue != null) {
                        value = finalGradeValue;
                    }
                } else if (PARAM_CREDIT_RECOVERY.equals(param)) {
                    if (!isMocked(transcript)) {
                        String recovery = (String) transcript.getFieldValueByAlias(ALIAS_CREDIT_RECOVERY);
                        if (recovery == null) {
                            recovery = "No";
                        }
                        value = lookupReferenceCodeByAlias(ALIAS_CREDIT_RECOVERY, recovery,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                } else if (PARAM_CLASS_OUTCOME.equals(param)) {
                    if (!isMocked(transcript)) {
                        value = getClassOutcomeValue(transcript, finalGrade);
                    } else {
                        value = GRADE_CODE_NONE;
                    }
                }
            }
            return value;
        }

        /**
         * Gathers the return for class outcome from the final grade. Default is N
         *
         * @param transcript Transcript
         * @param finalGrade String
         * @return Object
         */
        private Object getClassOutcomeValue(Transcript transcript, String finalGrade) {
            boolean droppedDuring = wasDroppedDuring(transcript.getStudentOid(), transcript.getMasterSchedule());

            String outcome = null;
            if (droppedDuring) {
                outcome = GRADE_CODE_NONE;
            } else {
                if (!StringUtils.isEmpty(finalGrade)) {
                    Float finalGradeNumber = null;
                    try {
                        finalGradeNumber = Float.valueOf(finalGrade);
                    } catch (NumberFormatException nfe) {
                        // this is ok
                    }
                    if (finalGradeNumber != null) {
                        outcome = getFinalGradeStringFromFloat(finalGradeNumber);
                    } else {
                        outcome = getFinalGradeString(finalGrade);
                    }
                } else {
                    String numericValue = (String) transcript.getFieldValueByAlias(ALIAS_NUMERIC_GRADE);
                    if (!StringUtils.isEmpty(numericValue)) {
                        outcome = getFinalGradeStringFromFloat(Float.valueOf(numericValue));
                    }
                }

                if (StringUtils.isEmpty(outcome)) {
                    outcome = GRADE_CODE_NONE;
                }
            }

            return outcome;
        }

        /**
         * Gets the final grade string from float.
         *
         * @param gradeValue Float
         * @return String
         */
        /*
         * The logic needs to be changed to:
         * any grade scale value of 64.5 and above, the class outcome is P
         * -----or, if just using code, any grade scale code of 65 and above, A+ -D, P and S, class
         * outcome = P
         * any grade scale value less than 64.5, the class outcome is F
         * -----or, if just using code, any grade scale code of 64 and under, NG, NE, HT, F, ABS,
         * class outcome = F
         * any grade scale value of W, INC, N, EX, class outcome = N
         * any empty final grade, class outcome = N
         */
        private String getFinalGradeStringFromFloat(Float gradeValue) {
            String finalGradeCode = GRADE_CODE_PASS;
            if (gradeValue.floatValue() < MINIMUM_PASS_GRADE_VALUE) {
                finalGradeCode = GRADE_CODE_FAIL;
            }

            return finalGradeCode;
        }

        /**
         * Gets the final grade string.
         *
         * @param finalGrade String
         * @return String
         */
        private String getFinalGradeString(String finalGrade) {
            String convertedGrade = GRADE_CODE_PASS;
            if (finalGrade != null) {
                if (finalGrade.matches(REGEX_N)) {
                    convertedGrade = GRADE_CODE_NONE;
                }
                if (finalGrade.matches(REGEX_F)) {
                    convertedGrade = GRADE_CODE_FAIL;
                }
            } else {
                convertedGrade = GRADE_CODE_FAIL;
            }

            return convertedGrade;
        }
    }

    /**
     * Validate credits.
     *
     * @author Follett Software Company
     */
    private class ValidateCredits implements FieldValidator {
        public static final String VAL_ID = "VAL_CREDITS";

        /**
         * Instantiates a new validate credits.
         */
        public ValidateCredits() {
            // to avoid warning
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!StringUtils.isEmpty(value)) {
                Double dValue = Double.valueOf(value);

                if (dValue.doubleValue() < 0.0d || dValue.doubleValue() > 5.0d) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId()
                                    + " is outside valid range (0.0 - 5.0, with no more than 4 decimal places)",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
            }

            return errors;
        }
    }

    /**
     * Validates grade code.
     *
     * @author Follett Software Company
     */
    private class ValidateGradeCode implements FieldValidator {
        public static final String VAL_ID = "VAL_GRADE_CODE";

        private List<String> m_validCodes;

        /**
         * Instantiates a new validate grade code.
         */
        public ValidateGradeCode() {
            m_validCodes = Arrays.asList("MP", "MT", "FG", "QZ", "EX", "FE", "HW", "OA");
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (!m_validCodes.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be valid code " + m_validCodes,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validates numeric grade.
     *
     * @author Follett Software Company
     */
    private class ValidateNumericGrade implements FieldValidator {
        public static final String VAL_ID = "VAL_NUMERIC_GRADE";

        /**
         * Instantiates a new validate numeric grade.
         */
        public ValidateNumericGrade() {
            // to avoid warning
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            Double dValue = Double.valueOf(value);

            if (dValue.doubleValue() < 0.0d || dValue.doubleValue() > 110.0d) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " is outside valid range (0-110, with no more than 3 decimal places)",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validates section code.
     *
     * @author Follett Software Company
     */
    private class ValidateSectionCode implements FieldValidator {
        public static final String VAL_ID = "VAL_SECTION_ID";

        /**
         * Instantiates a new validate section code.
         */
        public ValidateSectionCode() {
            // to avoid warning
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
            ArrayList<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String courseCode = entity.getFieldValue(FIELD_ID_COURSE_CODE);

            int courseCodeLength = StringUtils.isEmpty(courseCode) ? 0 : courseCode.length();
            int sectionCodeLength = StringUtils.isEmpty(value) ? 0 : value.length();

            if ((courseCodeLength + sectionCodeLength) > 29) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " and " + FIELD_ID_COURSE_CODE + " combination > 29 characters",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + ", " +
                                FIELD_ID_COURSE_CODE + " = " + STYLE_BOLD + courseCode + STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Export format fields names
     */
    protected static final String FIELD_ID_COURSE_CODE = "Course Code";

    /*
     * Tool input definition parameter names.
     */
    protected static final String PARAM_ALPHA_GRADE = "ALPHA-GRADE";
    protected static final String PARAM_ASSESSMENT_IND = "ASSESSMENT-IND";
    protected static final String PARAM_CLASS_OUTCOME = "CLASS-OUTCOME";
    protected static final String PARAM_CODE = "CODE";
    protected static final String PARAM_CREDIT_ATTEMPT = "CREDIT-ATTEMPT";
    protected static final String PARAM_CREDIT_EARNED = "CREDIT-EARNED";
    protected static final String PARAM_CREDIT_RECOVERY = "CREDIT-RECOVERY";
    protected static final String PARAM_CREDIT_TYPE = "CREDIT-TYPE";
    protected static final String PARAM_DETAIL_OUTCOME = "DETAIL-OUTCOME";
    protected static final String PARAM_DISP_GRADE = "DISPLAY-GRADE";
    protected static final String PARAM_DUAL_CREDIT = "DUAL-CREDIT";
    protected static final String PARAM_GPA_IMPACT = "GPA-IMPACT";
    protected static final String PARAM_GRADE_COMMENT = "GRADE-COMMENT";
    protected static final String PARAM_GRADE_DETAIL = "GRADE-DETAIL";
    protected static final String PARAM_LOCATION_CODE = "LOCATION";
    protected static final String PARAM_MARKING_PERIOD = "MARKING-PERIOD";
    protected static final String PARAM_NUM_GRADE = "NUM-GRADE";
    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_SECTION_CODE = "SECTION-CODE";
    protected static final String PARAM_TERM_CODE = "TERM-CODE";

    /*
     * Field alias names
     */
    protected static final String ALIAS_ACTIVE = "DOE ACTIVE ";
    protected static final String ALIAS_ALPHA_GRADE = "DOE ALPHA GRADE";
    protected static final String ALIAS_CLASS_OUTCOME_CODE = "DOE CLASS OUTCOME CODE";
    protected static final String ALIAS_CREDIT_RECOVERY = "DOE CREDIT RECOVERY";
    protected static final String ALIAS_CREDIT_TYPE_CODE = "DOE CREDIT TYPE CODE";
    protected static final String ALIAS_CREDITS_EARNED = "DOE CREDITS EARNED";
    protected static final String ALIAS_CRS_DUAL_CREDIT = "all-crs-DualCreditSetting";
    protected static final String ALIAS_CRS_EXCLUDE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_DISPLAY_GRADE = "DOE DISPLAY GRADE";
    protected static final String ALIAS_EXCLUDE_SCG = "DOE EXCLUDE SCGD";
    protected static final String ALIAS_GPA_IMPACT_CODE = "DOE GPA IMPACT CODE";
    protected static final String ALIAS_GRADE_DETAIL_CODE = "DOE GRADE DETAIL CODE";
    protected static final String ALIAS_LOCATION_CODE = "LOCATION CODE";
    protected static final String ALIAS_MARKING_PERIOD_CODE = "DOE MARKING PERIOD CODE";
    protected static final String ALIAS_NUMERIC_GRADE = "DOE NUMERIC GRADE";
    protected static final String ALIAS_PLATOON_CODE = "DOE SECTION OVERRIDE";
    protected static final String ALIAS_SCHOOL_OVERRIDE = "NY TRANS SCHOOL OVERRIDE";
    protected static final String ALIAS_SECTION_CODE = "DOE SECTION CODE";
    protected static final String ALIAS_SKL_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_SSC_DUAL_CREDIT = "all-ssc-DualCreditSetting";
    protected static final String ALIAS_STATE_ASSESS_INCLUDE_IND = "DOE STATE ASSESS INCLUDE IND";
    protected static final String ALIAS_STATE_ASSESSMENT = "DOE STATE ASSESSMENT";
    protected static final String ALIAS_STATE_CODE = "DOE STATE COURSE";
    protected static final String ALIAS_STUDENT_GRADE_COMMENT = "DOE STUDENT GRADE COMMENT";
    protected static final String ALIAS_SUPP_COURSE_DIFF = "DOE SUPP COURSE DIFF";
    protected static final String ALIAS_TRANSCRIPT_TERM_CODE = "DOE TRANSCRIPT TERM CODE";

    // Codes
    protected static final String REGEX_CLASS_OUTCOME = "N|F";
    protected static final String REGEX_MARKINGPERIOD = "T|Q";
    protected static final String REGEX_N = "W|INC|N|EX";
    protected static final String REGEX_F = "NG|NE|HT|F|ABS";

    protected static final String GRADE_CODE_PASS = "P";
    protected static final String GRADE_CODE_FAIL = "F";
    protected static final String GRADE_CODE_NONE = "N";

    protected static final float MINIMUM_PASS_GRADE_VALUE = (float) 64.5;

    protected static final String CODE_YES = "Y";
    protected static final String CODE_NO = "N";

    protected static final int MAX_COMMENT_LENGTH = 250;

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldCrsExclude;
    protected String m_fieldCrsDualCredit;
    protected String m_fieldSklExcludeSchool;
    protected String m_fieldSscDualCredit;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected PlainDate m_reportingDate;
    protected Boolean m_removeHeader;

    protected HashMap<String, Collection<ScheduleTermDate>> m_scheduleTermDatesList =
            new HashMap<String, Collection<ScheduleTermDate>>();
    protected HashMap<String, Collection<StudentScheduleChange>> m_studentScheduleChangeList =
            new HashMap<String, Collection<StudentScheduleChange>>();
    protected HashMap<String, Collection<StudentSchedule>> m_studentSchedulesMap =
            new HashMap<String, Collection<StudentSchedule>>();

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (m_removeHeader == null || m_removeHeader.booleanValue()) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        initializeFields();

        // Build helper object.
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_EXCLUDE_FUTURE_SCHEDULES, Boolean.TRUE);

        if (!isSchoolContext()) {
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.FALSE);
        }

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addNotEqualTo(SisStudent.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool,
                Boolean.TRUE);
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        loadScheduleTermDates();

        loadStudentScheduleChanges(studentSubQuery);
        loadStudentSchedules(studentSubQuery);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(StudentClassGradeEntity.class);

            // Build maps of retriever functions
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SCG-COURSE", new RetrieveCourse());
            calcs.put("SCG-REP-DATE", new RetrieveReportingDate());
            calcs.put("SCG-SECTION", new RetrieveSection());
            calcs.put("SCG-TRANSCRIPT", new RetrieveTranscript());

            HashMap vals = new HashMap<String, FieldValidator>();
            vals.put(ValidateSectionCode.VAL_ID, new ValidateSectionCode());
            vals.put(ValidateGradeCode.VAL_ID, new ValidateGradeCode());
            vals.put(ValidateNumericGrade.VAL_ID, new ValidateNumericGrade());
            vals.put(ValidateCredits.VAL_ID, new ValidateCredits());

            addCalcs(calcs);
        }
    }

    /**
     * Get a Section by it's oid.
     *
     * @param sectionOid String
     * @return MasterSchedule
     */
    protected MasterSchedule getMasterScheduleByOid(String sectionOid) {
        MasterSchedule section = null;

        if (!StringUtils.isEmpty(sectionOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, sectionOid);
            BeanQuery query = new BeanQuery(MasterSchedule.class, criteria, true, true);
            section = (MasterSchedule) getBroker().getBeanByQuery(query);
        }

        return section;
    }

    /**
     * Check to see it a Transcript was "mocked".
     * This means not an original student transcript but generated from StudentScheduleChange
     * record.
     * These are generated in the code only for the output of the export and are not represented in
     * the database.
     *
     * @param transcript Transcript
     * @return boolean
     */
    protected boolean isMocked(Transcript transcript) {
        boolean mocked = false;

        if (StringUtils.isEmpty(transcript.getOid())) {
            mocked = true;
        }

        return mocked;
    }

    /**
     * Check whether a section was dropped during its Schedule Term.
     *
     * @param studentOid String
     * @param section MasterSchedule
     * @return boolean
     */
    protected boolean wasDroppedDuring(String studentOid, MasterSchedule section) {
        boolean droppedDuring = false;

        String scheduleTermOid = section.getScheduleTermOid();
        Collection<ScheduleTermDate> scheduleTermDates = m_scheduleTermDatesList.get(scheduleTermOid);
        Collection<StudentScheduleChange> studentScheduleChanges = m_studentScheduleChangeList.get(studentOid);

        String transcriptSectionOid = section.getOid();

        PlainDate termStartDate = null;
        PlainDate termEndDate = null;
        if (scheduleTermDates != null) {
            for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                termStartDate = scheduleTermDate.getStartDate();
                termEndDate = scheduleTermDate.getEndDate();
            }
        }
        long lastStudentScheduleChangeTimestamp = 0;
        String lastChangeCode = "";
        String lastSectionOid = "";
        PlainDate lastChangeDate = null;
        if (termStartDate != null && studentScheduleChanges != null && studentScheduleChanges.size() > 0) {
            for (StudentScheduleChange studentScheduleChange : studentScheduleChanges) {
                if (transcriptSectionOid.equals(studentScheduleChange.getMasterScheduleOid())) {
                    PlainDate changeDate = studentScheduleChange.getEffectiveDate();

                    // Only interested the last change before the end of the Schedule Term
                    if (changeDate.before(termEndDate) || changeDate.equals(termEndDate)) {
                        if (studentScheduleChange.getTimestamp() > lastStudentScheduleChangeTimestamp) {
                            lastStudentScheduleChangeTimestamp = studentScheduleChange.getTimestamp();
                            lastChangeCode = studentScheduleChange.getChangeTypeCode();
                            lastChangeDate = studentScheduleChange.getEffectiveDate();
                            lastSectionOid = studentScheduleChange.getMasterScheduleOid();
                        }
                    }
                }
            }

            if (StudentScheduleChange.CODE_DROP.equals(lastChangeCode)) {
                if (transcriptSectionOid.equals(lastSectionOid)) {
                    // Dropped within the Schedule Term Dates
                    if ((lastChangeDate.after(termStartDate) || lastChangeDate.equals(termStartDate))
                            && (lastChangeDate.before(termEndDate) || lastChangeDate.equals(termEndDate))) {
                        droppedDuring = true;
                    }
                }
            }
        }

        return droppedDuring;
    }

    /**
     * Initialize Fields.
     */
    private void initializeFields() {
        // Set Parameters
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
        X2Criteria ctxCriteria = new X2Criteria();
        ctxCriteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
        ctxCriteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);
        QueryByCriteria ctxQuery = new QueryByCriteria(DistrictSchoolYearContext.class, ctxCriteria);
        DistrictSchoolYearContext reportingCtx = (DistrictSchoolYearContext) getBroker().getBeanByQuery(ctxQuery);
        if (reportingCtx != null) {
            m_reportingDate = reportingCtx.getEndDate();
        } else {
            m_reportingDate = getCurrentContext().getEndDate();
        }

        m_removeHeader = (Boolean) getParameter(PARAM_REMOVE_HEADER);
        if (m_removeHeader == null) {
            m_removeHeader = Boolean.valueOf(false);
        }
        m_fieldCrsDualCredit = translateAliasToJavaName(ALIAS_CRS_DUAL_CREDIT, true);
        m_fieldSscDualCredit = translateAliasToJavaName(ALIAS_SSC_DUAL_CREDIT, true);
        m_fieldCrsExclude = translateAliasToJavaName(ALIAS_CRS_EXCLUDE, true);
        m_fieldSklExcludeSchool = translateAliasToJavaName(ALIAS_SKL_EXCLUDE_SCHOOL, true);
    }

    /**
     * Load Schedule Term Dates map.
     */
    private void loadScheduleTermDates() {
        X2Criteria scheduleTermDatesCriteria = new X2Criteria();
        BeanQuery scheduleTermDatesQuery = new BeanQuery(ScheduleTermDate.class, scheduleTermDatesCriteria);

        scheduleTermDatesQuery.addOrderByAscending(ScheduleTermDate.COL_SCHEDULE_TERM_OID);
        scheduleTermDatesQuery.addOrderByAscending(ScheduleTermDate.COL_START_DATE);

        m_scheduleTermDatesList = (HashMap<String, Collection<ScheduleTermDate>>) getBroker()
                .getGroupedCollectionByQuery(scheduleTermDatesQuery, ScheduleTermDate.COL_SCHEDULE_TERM_OID, 128);
    }

    /**
     * Load Schedule Changes.
     *
     * @param studentQuery SubQuery
     */
    private void loadStudentScheduleChanges(SubQuery studentQuery) {
        X2Criteria studentScheduleChangesCriteria = new X2Criteria();
        studentScheduleChangesCriteria.addIn(StudentScheduleChange.COL_STUDENT_OID, studentQuery);

        if (isSchoolContext() && getSchool() != null) {
            studentScheduleChangesCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + "."
                    + MasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        }
        studentScheduleChangesCriteria
                .addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER
                        + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_SCHOOL
                        + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);

        // TODO
        studentScheduleChangesCriteria
                .addEqualToField(StudentScheduleChange.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        StudentScheduleChange.COL_SCHEDULE_OID);

        studentScheduleChangesCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        QueryByCriteria studentScheduleChangesQuery =
                new QueryByCriteria(StudentScheduleChange.class, studentScheduleChangesCriteria);
        studentScheduleChangesQuery.addOrderByAscending(StudentScheduleChange.COL_MASTER_SCHEDULE_OID);
        studentScheduleChangesQuery.addOrderByAscending(StudentScheduleChange.COL_TIMESTAMP);

        m_studentScheduleChangeList = (HashMap<String, Collection<StudentScheduleChange>>) getBroker()
                .getGroupedCollectionByQuery(studentScheduleChangesQuery, StudentScheduleChange.COL_STUDENT_OID, 128);
    }

    /**
     * Load Student Schedules.
     *
     * @param studentQuery SubQuery
     */
    private void loadStudentSchedules(SubQuery studentQuery) {
        X2Criteria studentSchedulesCriteria = new X2Criteria();
        studentSchedulesCriteria.addIn(StudentSchedule.COL_STUDENT_OID, studentQuery);

        if (isSchoolContext() && getSchool() != null) {
            studentSchedulesCriteria.addEqualTo(StudentSchedule.REL_SECTION + "."
                    + MasterSchedule.REL_SCHOOL_COURSE + "." + SchoolCourse.COL_SCHOOL_OID, getSchool().getOid());
        }
        studentSchedulesCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + ModelProperty.PATH_DELIMITER
                + MasterSchedule.REL_SCHOOL_COURSE + ModelProperty.PATH_DELIMITER + SchoolCourse.REL_SCHOOL
                + ModelProperty.PATH_DELIMITER + m_fieldSklExcludeSchool, Boolean.TRUE);

        studentSchedulesCriteria
                .addEqualToField(StudentSchedule.REL_SCHEDULE + ModelProperty.PATH_DELIMITER +
                        Schedule.REL_SCHOOL + ModelProperty.PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + ModelProperty.PATH_DELIMITER +
                        SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                        StudentScheduleChange.COL_SCHEDULE_OID);

        QueryByCriteria studentSchedulesQuery =
                new QueryByCriteria(StudentSchedule.class, studentSchedulesCriteria);
        studentSchedulesQuery.addOrderByAscending(StudentSchedule.COL_SECTION_OID);

        m_studentSchedulesMap = (HashMap<String, Collection<StudentSchedule>>) getBroker()
                .getGroupedCollectionByQuery(studentSchedulesQuery, StudentSchedule.COL_STUDENT_OID, 128);
    }
}
