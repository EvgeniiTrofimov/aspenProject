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

package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
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
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Rhode Island state report for the TCS Section Student export.
 * This class implements the data export for the TCS Section Student export.
 *
 * @author X2 Development Corporation
 */
public class TCSStudent extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TCS Section Student export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class TCSStudentEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        TCSStudent m_tcsData = null;
        List<ScheduleInfo> m_schedules = null;

        /**
         * Instantiates a new TCS student entity.
         */
           /*
            * Public no argument constructor for dynamic instantiation.
            */
        public TCSStudentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            ScheduleInfo info = getScheduleInfo();
            if (info != null) {
                name += info.m_section.getCourseView();
            }

            return name;
        }

        /**
         * Returns the schedule info record for the schedule record(s)
         * based in the current row value.
         *
         * @return ScheduleInfo
         */
        public ScheduleInfo getScheduleInfo() {
            ScheduleInfo info = null;
            if (m_schedules != null && getCurrentRow() < m_schedules.size() && getCurrentRow() >= 0) {
                info = m_schedules.get(getCurrentRow());
            }
            return info;
        }

        /**
         * Gets the todays date.
         *
         * @return Plain date
         */
        public PlainDate getTodaysDate() {
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
            String strDate = dateFormat.format(new Date());
            PlainDate today = DateUtils.getDate(strDate);
            return today;
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
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            Map<String, ScheduleInfo> scheduleMap = new HashMap<String, ScheduleInfo>();
            m_tcsData = (TCSStudent) data;
            SisStudent student = (SisStudent) bean;

            /*
             * Collect all schedule relevant to this student.
             * Then assign transcript records to the student schedule records if possible.
             *
             * schedules = collection of student schedules for this student.
             *
             * scheduleChanges = collection of student schedule change drops for this student.
             *
             * transcriptMap = map from student schedule OID or Schedule Change OID to a student
             * transcript is assigned to it.
             * (prevent duplicate/multi assignment.)
             */
            Collection<StudentSchedule> schedules = m_tcsData.m_scheduleMap.get(student.getOid());
            Collection<StudentScheduleChange> scheduleChanges = m_tcsData.m_scheduleChangeMap.get(student.getOid());
            Collection<Transcript> transcripts = m_tcsData.m_transcriptMap.get(student.getOid());
            Collection<StudentEnrollment> enrollments = m_tcsData.m_enrollmentMap.get(student.getOid());

            // To save memory at the end of the report, remove the collection from the map as we go.
            // Noted large memory usage of report.
            m_tcsData.m_scheduleMap.remove(student.getOid());
            m_tcsData.m_scheduleChangeMap.remove(student.getOid());
            m_tcsData.m_transcriptMap.remove(student.getOid());

            // Add current schedule sections to the map.
            if (schedules != null) {
                for (StudentSchedule schedule : schedules) {
                    ScheduleInfo info = new ScheduleInfo();

                    info.m_section = schedule.getSection();
                    scheduleMap.put(schedule.getSectionOid(), info);
                }
            }

            // Add reportable dropped schedule add/drop sections to the map.
            if (scheduleChanges != null) {
                checkScheduleChanges(scheduleChanges, scheduleMap);
            }

            // Fill in any empty entry/exit dates with term dates for the section term.
            fillTermDates(scheduleMap);

            // try to match schedule to transcript on master schedule oid.
            if (transcripts != null) {
                for (Transcript trn : transcripts) {
                    /*
                     * For each transcript, find a scheduleMap entry with the same section.
                     * If there are more than one, pick the latest one that does not have
                     * a transcript on it already.
                     * Latest is determined by exit date, with null being the latest.
                     */
                    ScheduleInfo lastInfo = null;
                    for (ScheduleInfo info : scheduleMap.values()) {
                        if (info.m_section.getOid().equals(trn.getMasterScheduleOid()) &&
                                info.m_transcript == null) {
                            if (lastInfo == null) {
                                lastInfo = info;
                            } else if (info.m_exitDate == null && lastInfo.m_exitDate != null) {
                                lastInfo = info;
                            } else if (info.m_exitDate != null && lastInfo.m_exitDate != null &&
                                    info.m_exitDate.after(lastInfo.m_exitDate)) {
                                lastInfo = info;
                            }
                        }
                    }
                    if (lastInfo != null) {
                        lastInfo.m_transcript = trn;
                    }
                }
            }

            // Put these into a List for ordered processing. Insertion sort by section course view.
            m_schedules = new ArrayList<ScheduleInfo>(20);
            for (ScheduleInfo info : scheduleMap.values()) {
                boolean found = false;
                for (int i = 0; i < m_schedules.size(); i++) {
                    ScheduleInfo arrayInfo = m_schedules.get(i);
                    if (arrayInfo.getCourseView() != null
                            && arrayInfo.getCourseView().compareTo(info.getCourseView()) > 0) {
                        m_schedules.add(i, info);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    m_schedules.add(info);
                }
            }

            for (ScheduleInfo info : scheduleMap.values()) {
                Transcript transcript = info.m_transcript;

                String letterGradeValue = null;

                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();
                    letterGradeValue = finalGrade;

                    GradeScale scale = m_tcsData.m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                    String letterGrade = null;


                    if (StringUtils.isNumeric(letterGradeValue)) {
                        if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                            BigDecimal numericGradeBD = m_tcsData.m_gradesManager.getNumericValue(finalGrade, scale,
                                    transcript.getSchool(), transcript.getSchoolCourseOid());
                            if (numericGradeBD != null) {
                                letterGrade = finalGrade;
                            }

                            if (letterGrade == null) {
                                // Try the final grade as a number.
                                BigDecimal gradeAsNumber = null;
                                try {
                                    gradeAsNumber = new BigDecimal(finalGrade);
                                } catch (NumberFormatException nfe) {
                                    // nothing. The grade is not numeric.
                                }

                                if (gradeAsNumber != null) {
                                    letterGrade = m_tcsData.m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                            transcript.getSchool(), transcript.getSchoolCourseOid());
                                }
                            }

                            if (!StringUtils.isEmpty(letterGrade)) {
                                letterGradeValue = m_tcsData.lookupStateValue(GradeScaleGradeDefinition.class,
                                        GradeScaleGradeDefinition.COL_GRADE_CODE,
                                        letterGrade);
                            }
                        }
                    } else if (!StringUtils.isEmpty(letterGradeValue)) {
                        GradeScaleGradeDefinition gsgLetter =
                                m_tcsData.m_gradesManager.getGradeDefinition(letterGradeValue, scale,
                                        transcript.getSchoolOid(),
                                        transcript.getSchoolCourseOid());
                        if (gsgLetter != null) {
                            letterGrade = m_tcsData.lookupStateValue(GradeScaleGradeDefinition.class,
                                    GradeScaleGradeDefinition.COL_GRADE_CODE,
                                    gsgLetter.getGradeCode());
                            if (!StringUtils.isEmpty(letterGrade)) {
                                letterGradeValue = letterGrade;
                            }
                        }
                    }
                }
                if (null == letterGradeValue
                        && m_tcsData.m_middleHighSchoolGradeLevels.contains(student.getGradeLevel())) {
                    PlainDate today = getTodaysDate();
                    if (null != info.m_exitDate && (info.m_exitDate.before(today) || info.m_exitDate.equals(today))) {
                        letterGradeValue = "W";
                    }
                }

                info.m_letterGrade = letterGradeValue;

                Object numericGradeValue = null;
                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();
                    GradeScale scale = m_tcsData.m_gradeScales.get(transcript.getTranscriptDefinitionOid());

                    if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                        // If the final grade is a letter value, convert it to a number.
                        numericGradeValue = m_tcsData.m_gradesManager.getNumericValue(finalGrade, scale,
                                transcript.getSchool(), transcript.getSchoolCourseOid());

                        // Otherwise try to convert the finalGrade to a BigDecimal.
                        if (numericGradeValue == null) {
                            try {
                                if (finalGrade.length() > 5) {
                                    finalGrade = finalGrade.substring(0, 5);
                                }
                                numericGradeValue = new BigDecimal(finalGrade);
                            } catch (NumberFormatException nfe) {
                                // nothing. The grade is not numeric.
                            }
                        } else {
                            String valueAsString = numericGradeValue + "";
                            if (valueAsString.length() > 5) {
                                numericGradeValue = new BigDecimal(valueAsString.substring(0, 5));
                            }
                        }
                    }
                }

                if (null == numericGradeValue
                        && m_tcsData.m_middleHighSchoolGradeLevels.contains(student.getGradeLevel())) {
                    PlainDate today = getTodaysDate();
                    if (null != info.m_exitDate && (info.m_exitDate.before(today) || info.m_exitDate.equals(today))) {
                        numericGradeValue = "0";
                    }
                }

                info.m_numericGrade = numericGradeValue;
            }

            // Clean out one day spans that conflict with other spans.
            // Caused by repeated scheduling changes.
            cleanSameDaySpans(m_schedules);

            /*
             * With m_schedules populated, we now know how many schedule records for this student.
             */
            setRowCount(m_schedules.size());
            setCurrentRow(0);

            /*
             * Remove student if withdrew from school before the current year's start date
             */
            PlainDate orgStartDate = null;
            if (student != null &&
                    student.getOrganization1() != null &&
                    student.getOrganization1().getCurrentContext() != null &&
                    student.getOrganization1().getCurrentContext().getStartDate() != null) {
                orgStartDate = student.getOrganization1().getCurrentContext().getStartDate();
            }
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    PlainDate enrollmentDate = enrollment.getEnrollmentDate();
                    String enrollmentType = StringUtils.coalesce(enrollment.getEnrollmentType(), "");
                    if ("W".equals(enrollmentType) &&
                            orgStartDate != null &&
                            enrollmentDate != null &&
                            orgStartDate.after(enrollmentDate)) {
                        setRowCount(0);
                    }
                    break;
                }
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

        /**
         * Search through schedule change records for alternate start dates and other
         * sections to report.
         *
         * Loop through schedule change records for each section, in date order.
         * If a section is found that is not already in the students schedule map,
         * check it's chain of change records against the report dates and its term dates.
         * If it was in session on a report date or at the end of its term, include it.
         * Some will have been dropped after the end of a term but should still be counted.
         *
         * @param scheduleChanges Collection<StudentScheduleChange>
         * @param scheduleMap Map<String,ScheduleInfo>
         */
        private void checkScheduleChanges(Collection<StudentScheduleChange> scheduleChanges,
                                          Map<String, ScheduleInfo> scheduleMap) {
            MasterSchedule lastSection = null;
            StudentScheduleChange lastChange = null;
            PlainDate termStart = null;
            PlainDate termEnd = null;

            /*
             * Work backward in time through schedule changes.
             * DROP will open a new section and the ADD before it will finish that section.
             * A DROP without a following ADD will be considered open at start of term.
             * Any activity entirely before start of term will be ignored.
             */
            for (StudentScheduleChange change : scheduleChanges) {
                // Check for a new section.
                if (lastSection == null || !lastSection.getOid().equals(change.getMasterScheduleOid())) {
                    // Save the working section if necessary.
                    if (lastChange != null) {
                        // The last change record for this section (in reverse chronological order)
                        // was a drop. Assume the section was scheduled from the beginning of the
                        // term/year.
                        ScheduleInfo info = new ScheduleInfo();
                        info.m_section = lastSection;
                        info.m_entryDate = termStart;
                        if (lastChange.getEffectiveDate().after(termEnd)) {
                            info.m_exitDate = termEnd;
                        } else {
                            info.m_exitDate = lastChange.getEffectiveDate();
                        }
                        // Avoid recording sections scheduled out entirely
                        // before the start of it's term. This is just scheduling activity.
                        if (!info.m_exitDate.before(termStart)) {
                            scheduleMap.put(lastChange.getOid(), info);
                        }
                    }

                    // Initialize the new section
                    lastChange = null;
                    lastSection = change.getMasterSchedule();
                    termStart = null;
                    termEnd = null;
                    Collection<ScheduleTermDate> termDates = m_tcsData.getTermDates(lastSection.getScheduleTermOid());

                    for (ScheduleTermDate termDate : termDates) {
                        if (termStart == null || termStart.after(termDate.getStartDate())) {
                            termStart = termDate.getStartDate();
                        }
                        if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                            termEnd = termDate.getEndDate();
                        }
                    }
                }

                // For a section, see if its dates compare with report dates or term dates.
                if (StudentScheduleChange.CODE_DROP.equals(change.getChangeTypeCode())) {
                    lastChange = change;
                } else if (StudentScheduleChange.CODE_ADD.equals(change.getChangeTypeCode())) {
                    if (lastChange == null) {
                        // No previous record, assume current student schedule. Find based on master
                        // OID.
                        ScheduleInfo info = scheduleMap.get(change.getMasterScheduleOid());
                        if (info != null) {
                            info.m_entryDate = change.getEffectiveDate();
                            if (info.m_entryDate.before(termStart)) {
                                info.m_entryDate = termStart;
                            }
                        }
                    } else {
                        ScheduleInfo info = new ScheduleInfo();
                        info.m_section = change.getMasterSchedule();
                        info.m_entryDate = change.getEffectiveDate();
                        if (info.m_entryDate.before(termStart)) {
                            info.m_entryDate = termStart;
                        }
                        if (lastChange.getEffectiveDate().after(termEnd)) {
                            info.m_exitDate = termEnd;
                        } else {
                            info.m_exitDate = lastChange.getEffectiveDate();
                        }
                        // Avoid recording sections scheduled out entirely
                        // before the start of it's term. This is just scheduling activity.
                        if (!info.m_exitDate.before(termStart)) {
                            scheduleMap.put(change.getOid(), info);
                        }
                    }
                    lastChange = null;
                }
            }
            if (lastChange != null) {
                // The last change record for this section (in reverse chronological order)
                // was a drop. Assume the section was scheduled from the beginning of the term/year.
                ScheduleInfo info = new ScheduleInfo();
                info.m_section = lastSection;
                info.m_entryDate = termStart;
                if (lastChange.getEffectiveDate().after(termEnd)) {
                    info.m_exitDate = termEnd;
                } else {
                    info.m_exitDate = lastChange.getEffectiveDate();
                }
                // Avoid recording sections scheduled out entirely
                // before the start of it's term. This is just scheduling activity.
                if (!info.m_exitDate.before(termStart)) {
                    scheduleMap.put(lastChange.getOid(), info);
                }
            }
        }

        /**
         * Search for spans where:
         * 1. The span covers one day, entry = exit.
         * 2. Another span exists for the same section, and coincides with either the entry date or
         * exit date.
         * If such spans are found, remove them.
         *
         * @param schedules List<ScheduleInfo>
         */
        private void cleanSameDaySpans(List<ScheduleInfo> schedules) {
            int checkPos = 0;
            while (checkPos < schedules.size()) {
                boolean removed = false;
                ScheduleInfo info = schedules.get(checkPos);
                if (info.m_entryDate.equals(info.m_exitDate)) {
                    // a single day span. search for matching section and date.
                    for (ScheduleInfo otherInfo : schedules) {
                        if (!otherInfo.equals(info) &&
                                otherInfo.m_section.equals(info.m_section) &&
                                (otherInfo.m_entryDate.equals(info.m_entryDate) ||
                                        otherInfo.m_exitDate.equals(info.m_exitDate))) {
                            // found a match. Must remove the span at "checkPos".
                            schedules.remove(checkPos);
                            removed = true;
                            break;
                        }
                    }
                }
                if (!removed) {
                    checkPos++;
                }
            }
        }

        /**
         * For all populated sections, if the entry date or exit date is missing, populate with term
         * dates.
         *
         * @param scheduleMap Map<String,ScheduleInfo>
         */
        private void fillTermDates(Map<String, ScheduleInfo> scheduleMap) {
            PlainDate termStart = null;
            PlainDate termEnd = null;

            for (ScheduleInfo info : scheduleMap.values()) {
                if (info.m_entryDate == null || info.m_exitDate == null) {
                    termStart = null;
                    termEnd = null;
                    Collection<ScheduleTermDate> termDates =
                            m_tcsData.getTermDates(info.m_section.getScheduleTermOid());
                    for (ScheduleTermDate termDate : termDates) {
                        if (termStart == null || termStart.after(termDate.getStartDate())) {
                            termStart = termDate.getStartDate();
                        }
                        if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                            termEnd = termDate.getEndDate();
                        }
                    }

                    if (info.m_entryDate == null) {
                        info.m_entryDate = termStart;
                    }
                    if (info.m_exitDate == null) {
                        info.m_exitDate = termEnd;
                    }
                }
            }
        }
    }

    /**
     * A container class for schedule information for one scheduled class.
     * This will contain one of a StudentSchedule or StudentScheduleChange,
     * and optionally one Transcript record.
     *
     * @author X2 Development Corporation
     */
    protected static class ScheduleInfo {
        public PlainDate m_entryDate;
        public PlainDate m_exitDate;
        public Object m_letterGrade;
        public Object m_numericGrade;
        public MasterSchedule m_section;
        public Transcript m_transcript;


        /**
         * Return the name of the section for use in sorting.
         *
         * @return String
         */
        protected String getCourseView() {
            String name = "";
            if (m_section != null) {
                name = m_section.getCourseView();
            }
            return name;
        }
    }

    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- X2-specific code below
    // ---------------------------------------------------------------------------------------------

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "requireSced" parameter. The value is an Boolean.
     */
    public static final String REQUIRE_SCED_PARAM = "requireSced";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the active students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    /**
     * Include State Exclusion. The value is a boolean.
     */
    public static final String INCLUDE_STATE_EXCLUSION = "includeStateExclusion";

    /**
     * Alias fields
     */
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_STUDENT = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_SECTION = "DOE EXCLUDE MST";
    protected static final String ALIAS_SCED_CODE = "RI Course ID";

    private final String GRADE_END = "12";
    private final String GRADE_START = "06";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_excludeCourseField;
    protected String m_excludeSectionField;
    protected String m_excludeStudentField;
    protected GradesManager m_gradesManager;
    protected Map<String, GradeScale> m_gradeScales;
    protected List<String> m_middleHighSchoolGradeLevels;
    protected String m_orgFieldStr;
    protected String m_orgOid;
    protected PlainDate m_reportDate;
    protected boolean m_sasidStudentsOnly;
    protected boolean m_includeStateExclusion;
    protected X2Criteria m_enrollmentCriteria;
    protected X2Criteria m_scheduleChangeCriteria;
    protected X2Criteria m_scheduleCriteria;
    protected Map<String, Collection<StudentSchedule>> m_scheduleMap;
    protected Map<String, Collection<StudentScheduleChange>> m_scheduleChangeMap;
    protected Map<String, Collection<Transcript>> m_transcriptMap;
    protected Map<String, Collection<StudentEnrollment>> m_enrollmentMap;
    protected Map<String, Collection<ScheduleTermDate>> m_termDateMap;


    /**
     * Returns credit value for the section course.
     *
     * This returns a String object.
     * The field definition should use a character
     * formatter to format the value appropriately.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSection implements FieldRetriever {
        private final String PARAM_ENTRY_DATE = "ENTRY_DATE";
        private final String PARAM_EXIT_DATE = "EXIT_DATE";
        private final String PARAM_ID = "ID";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleInfo info = ((TCSStudentEntity) entity).getScheduleInfo();
            MasterSchedule section = info.m_section;
            if (PARAM_ID.equals(param)) {
                value = section.getCourseView();
            } else if (PARAM_ENTRY_DATE.equals(param)) {
                if (info.m_entryDate != null) {
                    value = info.m_entryDate;
                }
            } else if (PARAM_EXIT_DATE.equals(param)) {
                if (info.m_exitDate != null && !info.m_exitDate.after(m_reportDate)) {
                    value = info.m_exitDate;
                }
            }
            return value;
        }
    }

    /**
     * Returns the district code or school code for the given class section.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveDistrictSchool implements FieldRetriever {
        private final String PARAM_DISTRICT = "DISTRICT";
        private final String PARAM_SCHOOL = "SCHOOL";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            // Check the student adjusted school code.
            Object value = null;
            String param = (String) field.getParameter();
            ScheduleInfo info = ((TCSStudentEntity) entity).getScheduleInfo();

            if (PARAM_DISTRICT.equals(param)) {
                String adjustedDistrCode =
                        (String) info.m_section.getSchedule().getSchool().getFieldValueByBeanPath(m_sklFieldAdjDistr);
                value = !StringUtils.isEmpty(adjustedDistrCode) ? adjustedDistrCode
                        : info.m_section.getSchedule().getSchool().getOrganization1()
                                .getFieldValueByBeanPath(m_districtIdField);
            } else if (PARAM_SCHOOL.equals(param)) {
                value = info.m_section.getSchedule().getSchool().getFieldValueByBeanPath(m_sklIdField);
            }

            return value;
        }
    }

    /**
     * Validate that if a student is in grade 6-12 that they have a letter grade
     * for the section they took.
     */
    protected class ValidateLetterGrade implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();
            int gradeLevel = Integer.valueOf(student.getGradeLevel()).intValue();
            if ((gradeLevel >= 6 || gradeLevel <= 12) &&
                    StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "A letter grade for the section must be reported for a student in grades 6-12.",
                        "Grade Level = " + STYLE_BOLD + gradeLevel + STYLE_END + ", Final Letter Grade ="));
            }

            return errors;
        }

    }

    /**
     * Validate that if a student is in grade 6-12 that they have a course credits
     * for the section they took.
     */
    protected class ValidateCredits implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();
            int gradeLevel = Integer.valueOf(student.getGradeLevel()).intValue();
            if ((gradeLevel >= 6 || gradeLevel <= 12) &&
                    StringUtils.isEmpty(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "A letter grade for the section must be reported for a student in grades 6-12.",
                        "Grade Level = " + STYLE_BOLD + gradeLevel + STYLE_END + ", Course Credits ="));
            }

            return errors;
        }

    }

    /**
     * Returns the heading with an end of line character appended.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        String heading = super.getHeading();
        if (heading != null && !StringUtils.isEmpty(heading) && !heading.endsWith("\n")) {
            heading += "\r\n";
        }
        return heading;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        // Load initialization data
        initializeFields();
        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Load a map of student transcript records my student.
            // Must load before student criteria is generated. That depends on schedule and change
            // criteria.
            loadStudentSchedules();
            loadStudentScheduleChanges();
            loadTranscripts();
            loadEnrollments();
            validateTermDates();
        }

        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            Integer sort = (Integer) getParameter(SORT_PARAM);
            switch (sort == null ? 0 : sort.intValue()) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.COL_NAME);
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
            setEntityClass(TCSStudentEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TCS-DISTRICT-SCHOOL", new RetrieveDistrictSchool());
            calcs.put("TCS-SECTION", new RetrieveSection());

            addCalcs(calcs);
        }
    }

    /**
     * Load the schedule term dates for a schedule term oid.
     * Keep a map of existing codes for lookup.
     *
     * @param scheduleTermOid String
     * @return Collection<ScheduleTermDate>
     */
    protected Collection<ScheduleTermDate> getTermDates(String scheduleTermOid) {
        Collection<ScheduleTermDate> dates = null;

        if (m_termDateMap == null) {
            m_termDateMap = new HashMap<String, Collection<ScheduleTermDate>>();
        }

        if (!m_termDateMap.containsKey(scheduleTermOid)) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, scheduleTermOid);
            QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
            dates = getBroker().getCollectionByQuery(query);
            m_termDateMap.put(scheduleTermOid, dates);
        }

        return m_termDateMap.get(scheduleTermOid);
    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + PATH_DELIMITER +
                RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(StudentSchedule.COL_STUDENT_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Builds a criteria for the students that should be reported.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        // Sub criteria for existing schedules
        SubQuery scheduleSubquery = new SubQuery(StudentSchedule.class, X2BaseBean.COL_OID, m_scheduleCriteria);
        SubQuery changeSubquery =
                new SubQuery(StudentScheduleChange.class, X2BaseBean.COL_OID, m_scheduleChangeCriteria);
        SubQuery enrollmentSubQuery = new SubQuery(StudentEnrollment.class, X2BaseBean.COL_OID, m_enrollmentCriteria);

        /*
         * Primary students
         */
        X2Criteria sub1Criteria = new X2Criteria();
        sub1Criteria.addExists(scheduleSubquery);
        X2Criteria sub2Criteria = new X2Criteria();
        sub2Criteria.addExists(changeSubquery);
        X2Criteria sub3Criteria = new X2Criteria();
        sub3Criteria.addExists(enrollmentSubQuery);
        sub2Criteria.addAndCriteria(sub2Criteria);
        sub1Criteria.addOrCriteria(sub3Criteria);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addAndCriteria(sub1Criteria);

        return primaryCriteria;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        m_excludeCourseField = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, false);
        m_excludeStudentField = translateAliasToJavaName(ALIAS_EXCLUDE_STUDENT, false);
        m_excludeSectionField = translateAliasToJavaName(ALIAS_EXCLUDE_SECTION, false);

        m_sasidStudentsOnly = true;
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly != null) {
            m_sasidStudentsOnly = sasidStudentsOnly.booleanValue();
        }
        m_includeStateExclusion = true;
        Boolean includeStateExclusion = (Boolean) getParameter(INCLUDE_STATE_EXCLUSION);
        if (includeStateExclusion != null) {
            m_includeStateExclusion = includeStateExclusion.booleanValue();
        }

        m_gradesManager = new GradesManager(getBroker());

        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria gradeScaleCriteria = new X2Criteria();
        gradeScaleCriteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
        BeanQuery gradeScaleQuery = new BeanQuery(TranscriptColumnDefinition.class, gradeScaleCriteria);
        Collection<TranscriptColumnDefinition> tcds = getBroker().getCollectionByQuery(gradeScaleQuery);
        for (TranscriptColumnDefinition tcd : tcds) {
            m_gradeScales.put(tcd.getTranscriptDefinitionOid(), tcd.getGradeScale());
        }

        m_middleHighSchoolGradeLevels = StudentManager.getGradeLevelRange(GRADE_START, GRADE_END, getBroker());
    }

    /**
     * Loads a map of sets of Student enrollment records by studentOid for students in the export.
     */
    private void loadEnrollments() {
        m_enrollmentCriteria = new X2Criteria();

        // check school or organization selection.
        if (isSchoolContext()) {
            m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL,
                    getSchool().getOid());
        } else if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
            m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    m_orgFieldStr,
                    m_orgOid);
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        } else {
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check include exclusion falg and exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            m_enrollmentCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            m_enrollmentCriteria.addNotEmpty(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        // Add user entered selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // YOG
                m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                m_enrollmentCriteria.addEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(m_enrollmentCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, m_enrollmentCriteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);

        m_enrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 500);
    }

    /**
     * Load a map of sets of student schedule records by student oid for the students in the export.
     */
    private void loadStudentSchedules() {
        m_scheduleCriteria = new X2Criteria();

        // Master type Class
        m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        // From active Schedule
        m_scheduleCriteria.addEqualToField(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentSchedule.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check include exclusion flag as well as exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            m_scheduleCriteria.addNotEmpty(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        // Check if the section exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeSectionField)) {
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    m_excludeSectionField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            m_scheduleCriteria.addNotEqualTo(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                m_scheduleCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Add user entered selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // YOG
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                m_scheduleCriteria.addEqualTo(StudentSchedule.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(m_scheduleCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        QueryByCriteria query = new QueryByCriteria(StudentSchedule.class, m_scheduleCriteria);
        m_scheduleMap = getBroker().getGroupedCollectionByQuery(query, StudentSchedule.COL_STUDENT_OID, 500);
    }

    /**
     * Load a map of sets of student schedule change records by student oid for the students in the
     * export.
     * This method only loads Drops and does extra filtering to eliminate schedule changes
     * that happen before the scheduled term begins.
     */
    private void loadStudentScheduleChanges() {
        m_scheduleChangeCriteria = new X2Criteria();

        // From Class type section
        m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE, "Class");

        // From active Schedule
        m_scheduleChangeCriteria.addEqualToField(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                StudentScheduleChange.COL_SCHEDULE_OID);

        // check school or organization selection.
        if (isSchoolContext()) {
            m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                        Schedule.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check inlcude exclusion flag as well as exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            m_scheduleChangeCriteria.addNotEmpty(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        // Check if the section exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeSectionField)) {
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    m_excludeSectionField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            m_scheduleChangeCriteria.addNotEqualTo(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                m_scheduleChangeCriteria.addNotEmpty(StudentScheduleChange.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Add user entered selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // YOG
                m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                m_scheduleChangeCriteria.addEqualTo(StudentScheduleChange.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(m_scheduleChangeCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        m_scheduleChangeCriteria.addNotNull(StudentScheduleChange.COL_ACTION_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentScheduleChange.class, m_scheduleChangeCriteria);
        query.addOrderBy(StudentScheduleChange.COL_STUDENT_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_MASTER_SCHEDULE_OID, true);
        query.addOrderBy(StudentScheduleChange.COL_EFFECTIVE_DATE, false);
        query.addOrderBy(StudentScheduleChange.COL_TIMESTAMP, false);
        m_scheduleChangeMap =
                getBroker().getGroupedCollectionByQuery(query, StudentScheduleChange.COL_STUDENT_OID, 500);
    }

    /**
     * Loads a map of sets of transcript records by studentOid for students in the export.
     */
    private void loadTranscripts() {
        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addNotEmpty(Transcript.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());

        // check school or organization selection.
        if (isSchoolContext()) {
            transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                transcriptCriteria.addEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                        m_orgFieldStr,
                        m_orgOid);
            }
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }

        // Check include exclusion flag as well as exclusion flags for student, section and student
        // schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            transcriptCriteria.addNotEmpty(Transcript.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        // Check if the section exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeSectionField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    m_excludeSectionField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the course exclusion custom field is present.
        if (!StringUtils.isEmpty(m_excludeCourseField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                    MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_excludeCourseField,
                    BooleanAsStringConverter.TRUE);
        }

        // Check if the SCED code is required.
        Boolean requireSced = (Boolean) getParameter(REQUIRE_SCED_PARAM);
        if (requireSced != null && requireSced.booleanValue()) {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCED_CODE);
            if (field != null) {
                transcriptCriteria.addNotEmpty(Transcript.REL_MASTER_SCHEDULE + PATH_DELIMITER +
                        MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                        SchoolCourse.REL_COURSE + PATH_DELIMITER +
                        field.getJavaName(),
                        getBroker().getPersistenceKey());
            }
        }

        // Add user entered selection criteria.
        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        Integer queryBy = (Integer) getParameter(QUERY_BY_PARAM);
        switch ((queryBy == null ? 0 : queryBy.intValue())) {
            case 1: // YOG
                transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                transcriptCriteria.addEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(transcriptCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addOrderBy(Transcript.COL_STUDENT_OID, true);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);
    }

    /**
     * Verify that all master schedules contain terms with appropriate dates.
     */
    private void validateTermDates() {
        // Create set of master schedules
        Set<MasterSchedule> sections = new HashSet();
        for (Collection<StudentSchedule> sscValues : m_scheduleMap.values()) {
            for (StudentSchedule ssc : sscValues) {
                if (ssc.getSection() != null) {
                    sections.add(ssc.getSection());
                }
            }
        }
        for (Collection<StudentScheduleChange> sccValues : m_scheduleChangeMap.values()) {
            for (StudentScheduleChange scc : sccValues) {
                if (scc.getMasterSchedule() != null) {
                    sections.add(scc.getMasterSchedule());
                }
            }
        }

        for (MasterSchedule section : sections) {
            PlainDate termStart = null;
            PlainDate termEnd = null;
            Collection<ScheduleTermDate> termDates = getTermDates(section.getScheduleTermOid());

            for (ScheduleTermDate termDate : termDates) {
                if (termStart == null || termStart.after(termDate.getStartDate())) {
                    termStart = termDate.getStartDate();
                }
                if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                    termEnd = termDate.getEndDate();
                }
            }
            if (termStart == null || termEnd == null) {
                this.addSetupError("Section Date Range Not Found",
                        section.getSchedule().getSchool().getName() + "-[" + section.getCourseView() + "]");
            }
        }
    }

}
