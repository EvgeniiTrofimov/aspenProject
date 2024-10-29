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

package com.x2dev.procedures.statereporting.tn;

/* DEBUG */
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Export procedure for TN Student Final Grade.
 *
 * @author X2 Development Corporation
 */
public class TNStudentFinalGradeData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Entity class for TN Student Final Grade.
     */
    public static class TNStudentFinalGradeEntity extends TNStateReportEntity
            implements TNStateReportData.HasStudentRecordHelper {
        /**
         * List of the reportable transcript records for the current student.
         */
        private List<Transcript> m_transcripts;
        private List<StudentRecordHelper> m_allStdHelper;
        private List<StudentRecordHelper> m_recordStdHelper;
        private TNStudentFinalGradeData m_sghData;
        private Map<String, Schedule> m_schedileMap = new HashMap<String, Schedule>();
        private Map<String, Pair<PlainDate, PlainDate>> m_schdeduleGapMap =
                new HashMap<String, Pair<PlainDate, PlainDate>>();



        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Transcript transcript = getTranscript();
            String name = "";
            if (transcript != null) {
                if (transcript.getStudent() != null) {
                    name += transcript.getStudent().getNameView() +
                            " [LASID: " + transcript.getStudent().getLocalId() +
                            ", SASID: " + transcript.getStudent().getStateId() +
                            "] ";
                }
                if (transcript.getMasterSchedule() != null) {
                    name += "Course: " + transcript.getMasterSchedule().getCourseView() + ",  ";
                } else if (transcript.getSchoolCourse() != null && transcript.getSchoolCourse().getCourse() != null) {
                    name += transcript.getSchoolCourse().getCourse().getNumber();
                }
            }
            return name;
        }

        /**
         * Return the current transcript record for the current row.
         *
         * @return Transcript
         */
        public Transcript getTranscript() {
            Transcript transcript = null;
            if (m_transcripts.size() > getCurrentRow()) {
                transcript = m_transcripts.get(getCurrentRow());
            }
            return transcript;
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
            m_sghData = (TNStudentFinalGradeData) data;

            // If the student or course are missing, skip this transcript
            // record.
            SisStudent student = (SisStudent) getBean();

            m_transcripts = (List<Transcript>) m_sghData.m_transcriptMap.get(student.getOid());

            m_allStdHelper = m_sghData.getStudentHelperMap().get(student.getOid());

            if (m_transcripts != null) {
                calculateRecords();

                setRowCount(m_transcripts.size());
            } else {
                setRowCount(0);
            }

            m_sghData.addEntityRowsCount(getRowCount());
        }

        /**
         * Determine gap.
         *
         * @param masterSchedule MasterSchedule
         * @return Pair
         */
        private Pair<PlainDate, PlainDate> determineGap(MasterSchedule masterSchedule) {
            Pair<PlainDate, PlainDate> pair = null;
            PlainDate termEnd = null;
            PlainDate termStart = null;
            Collection<ScheduleTermDate> termDates = masterSchedule.getScheduleTerm().getScheduleTermDates();
            for (ScheduleTermDate termDate : termDates) {
                if (termEnd == null || termEnd.before(termDate.getEndDate())) {
                    termEnd = termDate.getEndDate();
                }
                if (termStart == null || termStart.after(termDate.getStartDate())) {
                    termStart = termDate.getStartDate();
                }
            }
            pair = Pair.of(termStart, termEnd);
            return pair;
        }

        /**
         * Gets the current shceule.
         *
         * @param transcript Transcript
         * @param schoolYear int
         * @return Schedule
         */
        private Schedule getCurrentShceule(Transcript transcript, int schoolYear) {
            String key = transcript.getOid() + schoolYear;
            Schedule schedule = m_schedileMap.get(key);
            if (schedule == null) {

                for (Schedule currentChedule : transcript.getSchool().getSchedules()) {
                    if (currentChedule.getDistrictContext().getSchoolYear() == schoolYear) {
                        schedule = currentChedule;
                        break;
                    }
                }
                if (schedule != null) {
                    m_schedileMap.put(key, schedule);
                }
            }
            return schedule;
        }

        /**
         * Gets the gap from chedule.
         *
         * @param schedule Schedule
         * @param termCode String
         * @return Pair
         */
        private Pair<PlainDate, PlainDate> getGapFromChedule(Schedule schedule, String termCode) {
            String key = schedule.getOid() + termCode;
            Pair<PlainDate, PlainDate> gap = m_schdeduleGapMap.get(key);
            if (gap == null) {
                PlainDate termStart = null;
                PlainDate termEnd = null;
                for (MasterSchedule mst : schedule.getMasterSchedules()) {
                    String term = mst.getTeamCode() == null ? mst.getTermView() : mst.getTeamCode();

                    if (term.equals(termCode)) {
                        Pair<PlainDate, PlainDate> tempGap = determineGap(mst);
                        PlainDate tempStartDate = tempGap.getLeft();
                        PlainDate tempEndDate = tempGap.getRight();

                        if (termEnd == null || (tempEndDate != null && tempEndDate.after(termEnd))) {
                            termEnd = tempEndDate;
                            termStart = tempStartDate;
                        }
                    }
                }
                gap = Pair.of(termStart, termEnd);
                m_schdeduleGapMap.put(key, gap);
            }
            return gap;
        }

        /**
         * Determine gap.
         *
         * @param transcript Transcript
         * @return Pair
         */
        private Pair determineGap(Transcript transcript) {
            PlainDate termStart = null;
            PlainDate termEnd = null;
            String termCode = transcript.getTermCode();
            MasterSchedule masterSchedule = transcript.getMasterSchedule();
            // if masterSchedule exist we can determine period from ScheduleTermDates in this
            // masterSchedule
            if (masterSchedule != null && masterSchedule.getScheduleTerm() != null) {
                Pair<PlainDate, PlainDate> tempGap = determineGap(masterSchedule);
                termStart = tempGap.getLeft();
                termEnd = tempGap.getRight();
            }
            // if masterShdeule does not exist but we have term - try find masterSchedule form
            // current school
            // masterSchedule need have school year context like user selected and the same termCode
            else if (termCode != null) {
                Schedule schedule = getCurrentShceule(transcript, m_sghData.getCurrentContext().getSchoolYear());
                Pair<PlainDate, PlainDate> tempGap = getGapFromChedule(schedule, termCode);
                termStart = tempGap.getLeft();
                termEnd = tempGap.getRight();

            }
            // if nothing to found - using full year
            if (termStart == null) {
                SisDistrictSchoolYearContext context = transcript.getDistrictContext();
                termStart = context.getStartDate();
                termEnd = context.getEndDate();
            }
            Pair<PlainDate, PlainDate> gap = Pair.of(termStart, termEnd);
            return gap;
        }

        /**
         * Calculate records.
         */
        private void calculateRecords() {

            m_recordStdHelper = new ArrayList<TNStateReportData.StudentRecordHelper>();
            Iterator<Transcript> iterator = m_transcripts.iterator();
            while (iterator.hasNext()) {
                Transcript transcript = iterator.next();
                StudentRecordHelper recordHelper = null;

                // first situation when transcript belong to selected school year context
                if (transcript.getDistrictContext().getSchoolYear() == m_sghData.getCurrentContext().getSchoolYear()) {
                    Pair<PlainDate, PlainDate> gap = determineGap(transcript);

                    // inside we have situation when transcript is transfer
                    // this situation has separate logic
                    if (transcript.getFieldValueByBeanPath(m_sghData.m_fieldTransferShcool) != null) {
                        // we try find near student record helper with start date after or equals
                        // start transcript gap
                        recordHelper = getNearNextStdRecordHelperForTransfer(gap, transcript);
                    }
                    // transcript belong to selected school year context and there is no transfer
                    else {
                        recordHelper = getAppropriateStdRecordHelper(gap, transcript);

                    }
                }
                // second situation when transcript has less then current school year context
                // this record always will transfer. For this record we need find first student
                // record helper
                // at the beginning of the reporting period
                else {
                    recordHelper = getNearestStdRecordHelperForTransfer();
                }
                if (recordHelper != null) {
                    m_recordStdHelper.add(recordHelper);
                } else {
                    iterator.remove();
                }
            }
        }

        /**
         * Try find near appropriate student record helper for transfer record<br>
         * By priority using first student record helper with start date after or equal than begin
         * of gap date<br>
         * If not found using standard method for not transfer transcript <br>
         * .
         *
         * @param gap Pair<PlainDate,PlainDate>
         * @param transcript Transcript
         * @return Student record helper
         */
        private StudentRecordHelper getNearNextStdRecordHelperForTransfer(Pair<PlainDate, PlainDate> gap,
                                                                          Transcript transcript) {
            PlainDate startDate = gap.getLeft();
            PlainDate earlestDate = null;
            StudentRecordHelper earlestRecordHelper = null;
            for (StudentRecordHelper recordHelper : m_allStdHelper) {
                PlainDate enrollDate = recordHelper.getEnrollDate();
                if (!enrollDate.before(startDate)) {

                    if (earlestDate == null || earlestDate.after(enrollDate)) {
                        earlestDate = enrollDate;
                        earlestRecordHelper = recordHelper;
                    }
                }

            }
            // as a rule after transfer transcript student has enrollment record, but sometimes
            // student learn in current school few year and one transfer transcript record falls
            // from the sky
            // in this case enrollment record will not exist after transfer transcript record and we
            // need use
            // standard method which using for not transfer record
            if (earlestRecordHelper == null) {
                earlestRecordHelper = getAppropriateStdRecordHelper(gap, transcript);
            }
            return earlestRecordHelper;
        }

        /**
         * This method need to be called for transfer transcript which belong to previous than
         * selected school year context.
         *
         * @return Student record helper
         */
        private StudentRecordHelper getNearestStdRecordHelperForTransfer() {
            PlainDate nearDate = null;
            StudentRecordHelper nearRecordHelper = null;
            for (StudentRecordHelper recordHelper : m_allStdHelper) {
                // as a rule after "transfer transcript" student has enrollment record with transfer
                // code reason.
                // But unfortunately not always. Before I used next "if":
                // if(m_sghData.getTransferEnrollmentCodes().contains(recordHelper.getEnrollReason()))
                PlainDate curentDate = recordHelper.getEnrollDate();
                if (nearDate == null || curentDate.before(nearDate)) {
                    nearDate = curentDate;
                    nearRecordHelper = recordHelper;
                }
            }
            return nearRecordHelper;
        }

        /**
         * Gets the appropriate std record helper.
         *
         * @param gap Pair<PlainDate,PlainDate>
         * @param transcript Transcript
         * @return Student record helper
         */
        private StudentRecordHelper getAppropriateStdRecordHelper(Pair<PlainDate, PlainDate> gap,
                                                                  Transcript transcript) {
            StudentRecordHelper returnRecordHelper = null;
            String schoolId = (String) transcript.getSchool().getFieldValueByBeanPath(m_sghData.m_fieldStateSchoolId);
            Student student = transcript.getStudent();
            PlainDate targetEndDate = gap.getRight();
            if (targetEndDate != null) {
                for (StudentRecordHelper recordHelper : m_allStdHelper) {
                    if (recordHelper.getSchoolId().equals(schoolId)) {
                        PlainDate startDate = recordHelper.getEnrollDate();
                        PlainDate endDate = recordHelper.getExitDate();
                        if (isBetween(startDate, endDate, targetEndDate)) {
                            returnRecordHelper = recordHelper;
                        }
                    }
                }
            }
            PlainDate targetStartDate = gap.getLeft();
            if (returnRecordHelper == null && targetStartDate != null) {
                PlainDate lastActiveDate = null;
                List<TNStudentEnrollmentSpan> enrollSpans =
                        m_sghData.m_historyHelper.getTNStudentEnrollmentSpans(student, true);
                boolean returnHelper = false;
                for (TNStudentEnrollmentSpan enrollSpan : enrollSpans) {
                    String spanschoolId =
                            (String) enrollSpan.getSchool().getFieldValueByBeanPath(m_sghData.m_fieldStateSchoolId);
                    String spanschoolIdByFirstEntry = enrollSpan.getFirstEntryEnrollment() == null ? null
                            : (String) enrollSpan.getFirstEntryEnrollment().getSchool()
                                    .getFieldValueByBeanPath(m_sghData.m_fieldStateSchoolId);
                    if (!StringUtils.isEmpty(spanschoolIdByFirstEntry)
                            && spanschoolIdByFirstEntry.equals(schoolId)) {
                        returnHelper = true;
                    } else {
                        returnHelper = false;
                    }
                    if ((!StringUtils.isEmpty(spanschoolId) && spanschoolId.equals(schoolId)) || returnHelper) {
                        PlainDate startDate = enrollSpan.getFirstActiveDate();
                        PlainDate endDate = enrollSpan.getLastActiveDate();
                        if (isBetween(startDate, endDate, targetStartDate)
                                || isBetween(targetStartDate, targetEndDate, startDate)
                                || (endDate != null && endDate.before(targetStartDate))) {
                            if (lastActiveDate == null || (endDate != null && endDate.after(lastActiveDate))) {
                                lastActiveDate = endDate;
                                if (lastActiveDate == null) {
                                    lastActiveDate = targetEndDate;
                                }
                            }
                        }
                    }
                }
                if (lastActiveDate != null) {
                    for (StudentRecordHelper recordHelper : m_allStdHelper) {
                        if (recordHelper.getSchoolId().equals(schoolId) || returnHelper) {
                            PlainDate startDate = recordHelper.getEnrollDate();
                            PlainDate endDate = recordHelper.getExitDate();
                            if (isBetween(startDate, endDate, lastActiveDate) || lastActiveDate.equals(targetEndDate)) {
                                returnRecordHelper = recordHelper;
                            }
                        }
                    }
                }
            }
            return returnRecordHelper;
        }

        /**
         * Checks if is between.
         *
         * @param startPeriod PlainDate
         * @param endPeriod PlainDate
         * @param findingDate PlainDate
         * @return true, if is between
         */
        private boolean isBetween(PlainDate startPeriod, PlainDate endPeriod, PlainDate findingDate) {

            boolean isBetween = false;
            if (!findingDate.before(startPeriod) && (endPeriod == null || !findingDate.after(endPeriod))) {
                isBetween = true;
            }
            return isBetween;
        }

        /**
         * Gets the current record.
         *
         * @return Student record helper
         * @see com.x2dev.procedures.statereporting.tn.TNStateReportData.HasStudentRecordHelper#getCurrentRecord()
         */
        @Override
        public StudentRecordHelper getCurrentRecord() {
            return m_recordStdHelper.get(getCurrentRow());
        }
    }



    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    protected static class DistrictEnrollmentSpan {
        StudentEnrollment m_entryEnrollment;
        PlainDate m_exitDate;
        StudentEnrollment m_exitEnrollment;
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String TRN_CALC_ID = "TRN_CALC_SSN";

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
            TNStudentFinalGradeEntity seEntity = (TNStudentFinalGradeEntity) entity;
            SisStudent student = (SisStudent) seEntity.getBean();
            Person psn = student.getPerson();

            if (psn == null) {
                return "";
            }

            String ssn = psn.getPersonId();
            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FiledValidatorSSN implements FieldValidator {
        protected static final String TRN_VAL_ID = "TRN_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long"));
            }

            return errors;
        }

    }

    /**
     * Retriever for getting value related to course and school course objects.
     */
    protected class RetrieveCourseInfo implements FieldRetriever {
        public static final String TRN_CALC_ID = "TRN_CALC_COURSE";
        private static final String CALC_PARAM_QUALITY_POINTS = "QUALITY_POINTS";
        private static final String CALC_PARAM_COURSE = "COURSE";
        private static final String CALC_PARAM_COURSE_CODE = "COURSE_CODE";
        private static final String CALC_PARAM_COURSE_SCHOOL = "COURSE_SCHOOL";

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
            String parameter = (String) field.getParameter();
            Object value = null;

            TNStudentFinalGradeEntity tnEntity = (TNStudentFinalGradeEntity) entity;
            TNStudentFinalGradeData tnData = (TNStudentFinalGradeData) data;
            Transcript transcript = tnEntity.getTranscript();

            SchoolCourse schoolCourse = null;
            Course course = null;

            schoolCourse = transcript.getEquivalentSchoolCourse() != null ? transcript.getEquivalentSchoolCourse()
                    : transcript.getSchoolCourse();
            if (schoolCourse != null) {
                course = schoolCourse.getCourse();
            }

            if (CALC_PARAM_COURSE.equals(parameter)) {
                MasterSchedule masterSchedule = transcript.getMasterSchedule();
                if (masterSchedule != null) {
                    value = masterSchedule.getCourseView();
                } else if (schoolCourse != null) {
                    value = schoolCourse.getNumber();
                }
                if (value != null) {
                    value = ((String) value).replaceFirst("^0+(?!$)", "");
                }
            } else if (CALC_PARAM_COURSE_CODE.equals(parameter)) {
                if (course != null) {
                    value = course.getFieldValueByBeanPath(m_fieldCourseCode);
                }
                if (value != null) {
                    value = ((String) value).replaceFirst("^0+(?!$)", "");
                }
            } else if (CALC_PARAM_COURSE_SCHOOL.equals(parameter)) {
                if (schoolCourse != null) {
                    value = schoolCourse.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId);
                }
            } else if (CALC_PARAM_QUALITY_POINTS.equalsIgnoreCase(parameter)) {
                if (transcript.getMasterSchedule() != null && !StringUtils.isEmpty((String) data
                        .getPropertyAsJavaType(transcript.getMasterSchedule(), m_fieldRigorPointsOverride))) {
                    value = data.lookupReferenceCodeByBeanPath(MasterSchedule.class, m_fieldRigorPointsOverride,
                            (String) data.getPropertyAsJavaType(transcript.getMasterSchedule(),
                                    m_fieldRigorPointsOverride),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                if (course != null) {
                    value = data.lookupReferenceCodeByBeanPath(Course.class, m_fieldRigorPoints,
                            (String) data.getPropertyAsJavaType(course, tnData.m_fieldRigorPoints),
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return value;
        }

    }



    /**
     * Field retriever for school year and Instructional program field.
     */
    protected class RetrieveDefault implements FieldRetriever {
        public static final String TRN_CALC_ID = "TRN_CALC_DEFAULT";

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
            TNStudentFinalGradeData sghData = (TNStudentFinalGradeData) data;

            String parameter = (String) field.getParameter();
            Object value = null;

            if (CALC_PARAM_SCHOOLYEAR.equals(parameter)) {
                value = sghData.m_schoolYear;
            }
            return value;
        }

    }

    /**
     * Returns the term information from the Master Schedule if available.
     * The Transcript record is used otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveOther implements FieldRetriever {
        public static final String TRN_CALC_ID = "TRN_CALC_OTHER";

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
            Object value = null;
            String parameter = (String) field.getParameter();
            if (CALC_PARAM_TRMCODE.equals(parameter)) {
                TNStudentFinalGradeEntity sghEntity = (TNStudentFinalGradeEntity) entity;
                Transcript transcript = sghEntity.getTranscript();
                MasterSchedule masterSchedule = transcript.getMasterSchedule();
                String termCode = null;

                if (masterSchedule != null && masterSchedule.getScheduleTerm() != null) {
                    termCode = masterSchedule.getScheduleTerm().getCode();
                } else {
                    termCode = transcript.getTermCode();
                }
                if (termCode != null) {
                    value = data.lookupStateValue(ScheduleTerm.class, ScheduleTerm.COL_CODE, termCode);
                }
            }
            return value != null ? value : "YR";
        }
    }

    /**
     * Returns the transcript and course information
     * from the Equivalent School Course if available.
     * School Course is used otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTranscript implements FieldRetriever {
        public static final String TRN_CALC_ID = "TRN_CALC_TRANSCRIPT";

        private final String CALC_PARAM_ALPHA_GRADE = "ALPHA_GRADE";
        private final String CALC_PARAM_CREDITS_ATTEMPTED = "CREDITS_ATTEMPTED";
        private final String CALC_PARAM_CREDITS_EARNED = "CREDITS_EARNED";
        private final String CALC_PARAM_GRADE_LEVEL = "GRADE_LEVEL";
        private final String CALC_PARAM_NUMERIC_GRADE = "NUMERIC_GRADE";
        private final String CALC_PARAM_PRIV_OF_STATE_TRANSFR = "PRIV_OF_STATE_TRANSFR";

        final BigDecimal BIG_DECIMAL_100 = new BigDecimal(100);

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
            TNStudentFinalGradeEntity sghEntity = (TNStudentFinalGradeEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            String parameter = (String) field.getParameter();
            Object value = null;
            SchoolCourse schoolCourse = null;
            Course course = null;

            schoolCourse = transcript.getEquivalentSchoolCourse() != null ? transcript.getEquivalentSchoolCourse()
                    : transcript.getSchoolCourse();

            if (schoolCourse != null) {
                course = schoolCourse.getCourse();

            }
            if (CALC_PARAM_CREDITS_ATTEMPTED.equals(parameter)) {
                if (!StringUtils.isEmpty(transcript.getPotentialCredit())) {
                    BigDecimal creditAsNumber = null;
                    try {
                        creditAsNumber = new BigDecimal(transcript.getPotentialCredit());
                        creditAsNumber = creditAsNumber.multiply(BIG_DECIMAL_100);
                    } catch (NumberFormatException nfe) {
                        // nothing. The credit is not numeric.
                    }
                    value = creditAsNumber;
                }
                if (value == null) {
                    if (schoolCourse != null && schoolCourse.getCredit() != null) {
                        value = schoolCourse.getCredit().multiply(BIG_DECIMAL_100);
                    } else if (course != null && course.getCredit() != null) {
                        value = course.getCredit().multiply(BIG_DECIMAL_100);
                    } else {
                        value = new BigDecimal(0);
                        StateReportValidationError error =
                                new StateReportValidationError(entity, field, "Credit is empty", "");
                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                }
            } else if (CALC_PARAM_CREDITS_EARNED.equals(parameter)) {
                value = transcript.getTotalCredit() == null ? BigDecimal.ZERO
                        : transcript.getTotalCredit().multiply(BIG_DECIMAL_100);
            } else if (CALC_PARAM_GRADE_LEVEL.equals(parameter)) {
                String gradeLevel = transcript.getGradeLevel();
                value = data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (CALC_PARAM_PRIV_OF_STATE_TRANSFR.equals(parameter)) {
                value = transcript.getFieldValueByBeanPath(m_fieldPrivOfStateTransfer);
            } else if (CALC_PARAM_NUMERIC_GRADE.equals(parameter)) {
                String finalGrade = transcript.getFinalGrade();

                // See if the grade in the final grade column is a grade scale
                // value (letter grade).
                if (!StringUtils.isEmpty(finalGrade)) {
                    if (StringUtils.isNumeric(finalGrade)) {
                        // Try the final grade as a number.
                        BigDecimal gradeAsNumber = null;
                        try {
                            gradeAsNumber = new BigDecimal(finalGrade);
                        } catch (NumberFormatException nfe) {
                            // nothing. The grade is not numeric.
                        }

                        value = gradeAsNumber;
                    } else {
                        GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                        value = scale != null ? m_gradesManager.getNumericValue(finalGrade, scale,
                                transcript.getSchool(), transcript.getSchoolCourseOid()) : null;
                    }
                }
            } else if (CALC_PARAM_ALPHA_GRADE.equals(parameter)) {
                String finalGrade = transcript.getFinalGrade();

                // See if the grade in the final grade column is a grade scale
                // value (letter grade).
                if (!StringUtils.isEmpty(finalGrade)) {
                    GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                    if (StringUtils.isNumeric(finalGrade) && scale != null) {
                        // Try the final grade as a number.
                        BigDecimal gradeAsNumber = null;
                        try {
                            gradeAsNumber = new BigDecimal(finalGrade);
                        } catch (NumberFormatException nfe) {
                            // nothing. The grade is not numeric.
                        }

                        if (gradeAsNumber != null) {
                            value = m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                    transcript.getSchool(), transcript.getSchoolCourseOid());
                        }
                    }
                }
            } else if (CALC_PARAM_SCHOOLYEAR.equals(parameter)) {
                SisDistrictSchoolYearContext context = transcript.getDistrictContext();
                if (context != null) {
                    value = Integer.toString(context.getSchoolYear() - 1);
                }
            }
            return value;
        }
    }

    /**
     * Constants: Aliases
     */

    protected static final String ALIAS_COURSE_CODE = "DOE SDE COURSE CODE";
    protected static final String ALIAS_EXCLUDE_COURSE = "DOE EXCLUDE CRS";
    protected static final String ALIAS_EXCLUDE_TRN = "DOE EXCLUDE TRN";
    protected static final String ALIAS_PRIV_OF_STATE_TRANSFR = "DOE PRIVATE OR OUT STATE TRN";
    protected static final String ALIAS_RIGOR_POINTS = "DOE RIGOR POINTS";
    protected static final String ALIAS_RIGOR_POINTS_OVERRIDE = "DOE RIGOR POINTS OVERRIDE";
    protected static final String ALIAS_TRANSFER_SCHOOL = "Transfer School";

    /**
     * Input Parameters
     */
    protected static final String PARAM_GRADES = "grades";
    protected static final String PARAM_REQUIRE_FINAL_GRADE = "requireFinalGrade";
    protected static final String PARAM_TN_TRN_EXP = "tnTrnExport";

    protected String m_fieldCourseCode;
    /**
     * Instance variables.
     */
    protected String m_fieldExcludeCourse;
    protected String m_fieldPrivOfStateTransfer;
    protected String m_fieldRigorPoints;
    protected String m_fieldRigorPointsOverride;
    protected String m_fieldTransferShcool;
    protected Map<String, GradeScale> m_gradeScales;
    protected Set<String> m_grades;
    protected GradesManager m_gradesManager;
    protected TNStudentHistoryHelper m_historyHelper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected Boolean m_requireFinalGrade;
    protected String m_schoolYear;
    protected Map<String, Collection<Transcript>> m_transcriptMap;
    protected Set<String> m_transferCodes = null;

    private final String CALC_PARAM_SCHOOLYEAR = "SCHOOLYEAR";
    private final String CALC_PARAM_TRMCODE = "TRMCODE";
    private static final String[] TRANSFER_CODES = {"E1", "TR", "TC"};

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        initializeFields();

        getCalendarsForContextOid(m_contextOid);

        m_gradesManager = new GradesManager(getBroker());
        loadGradescales();

        if (getSetupErrors().size() != 0) {
            return;
        }

        if (getCurrentContext().getSchoolYear() > 2019) {
            setExportVersion(2);
        }

        m_grades = getGradeCodesByRef();

        QueryByCriteria stdQuery = m_historyHelper.getStudentQuery(false);
        initStudentHelperMap(m_historyHelper, stdQuery);
        setQuery(stdQuery);

        setEntityClass(TNStudentFinalGradeEntity.class);

        // load grade scale information.
        loadTranscripts(m_historyHelper.getStudentCriteria());

        // Add any necessary FieldRetrievers
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Get collection of program codes corresponding to the state enrollment transfer codes.
     *
     * @return Sets the
     */
    protected Set<String> getTransferEnrollmentCodes() {
        if (m_transferCodes == null) {
            X2Criteria criteria = new X2Criteria();
            DataDictionaryField field =
                    getDataDictionaryField(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE);

            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addIn(ReferenceCode.COL_STATE_CODE, Arrays.asList(TRANSFER_CODES));

            String[] columns = new String[] {ReferenceCode.COL_CODE};

            ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

            m_transferCodes = new HashSet<String>();
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Object[] record = (Object[]) iterator.next();
                    String code = (String) record[0];
                    m_transferCodes.add(code);
                }
            } finally {
                iterator.close();
            }

        }
        return m_transferCodes;
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Populate list of grades from given list of reference codeds.
     *
     * @return Sets the
     */
    private Set<String> getGradeCodesByRef() {
        ArrayList<String> refGrades =
                StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_GRADES), ",");
        Set<String> grades = new HashSet<String>();

        for (String refOid : refGrades) {
            ReferenceCode refCode = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, refOid);
            grades.add(refCode.getCode());
        }

        return grades;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_requireFinalGrade = (Boolean) getParameter(PARAM_REQUIRE_FINAL_GRADE);
        if (m_requireFinalGrade == null) {
            m_requireFinalGrade = Boolean.FALSE;
        }

        m_fieldTransferShcool = translateAliasToJavaName(ALIAS_TRANSFER_SCHOOL, true);
        m_fieldCourseCode = translateAliasToJavaName(ALIAS_COURSE_CODE, true);
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_COURSE, true);
        m_fieldPrivOfStateTransfer = translateAliasToJavaName(ALIAS_PRIV_OF_STATE_TRANSFR, true);
        m_fieldRigorPoints = translateAliasToJavaName(ALIAS_RIGOR_POINTS, true);
        m_fieldRigorPointsOverride = translateAliasToJavaName(ALIAS_RIGOR_POINTS_OVERRIDE, true);

        TNEnrollmentHelper helper = new TNEnrollmentHelper(this);
        m_historyHelper = helper.getStudentHistoryHelper();
        m_historyHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        m_schoolYear = getCurentSchoolYear();

    }


    /**
     * Load grade scales for transcript grade translation.
     */
    private void loadGradescales() {
        /*
         * map grade scales by transcript definition Oid for easier retrieval.
         */
        m_gradeScales = new HashMap<String, GradeScale>();
        X2Criteria criteria = new X2Criteria();

        // Find the column definition that points to TRN_FINAL_GRADE
        criteria.addEqualTo(TranscriptColumnDefinition.COL_COLUMN_TYPE_CODE,
                Integer.valueOf(TranscriptColumnDefinition.COLUMN_TYPE_FINAL));
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
    }

    /**
     * Load a map of transcripts for the students in the export.
     * (Not using the StudentHistoryHelper due to a bug, and that it is transcript
     * first then student rather than transcripts for student.)
     *
     *
     * @param studentCriteria X2Criteria
     */
    private void loadTranscripts(X2Criteria studentCriteria) {
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);

        boolean isOriginExportRuns = ((Boolean) getParameter(PARAM_TN_TRN_EXP)).booleanValue();

        if (isOriginExportRuns && !m_grades.isEmpty()) {
            transcriptCriteria.addIn(Transcript.COL_GRADE_LEVEL, m_grades);
        }

        transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER + SchoolCourse.REL_COURSE
                + PATH_DELIMITER + m_fieldExcludeCourse,
                BooleanAsStringConverter.TRUE);
        if (m_requireFinalGrade.booleanValue()) {
            transcriptCriteria.addNotEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
        }

        BeanQuery query = new BeanQuery(Transcript.class, transcriptCriteria);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 1000);

        loadTransferTranscripts(studentCriteria);
    }

    /**
     * Loads the transfer transcripts that should be loaded to the state. The rules include:
     * <ol>
     * <li>The student has a transfer period of qualification beginning with the previous
     * withdrawal record from a transfer record that occurs in this school year</li>
     * <li>The transcript was for a context year that ends after the previous withdrawal and before
     * the current year</li>
     * <li>The transcript record has no related Master Schedule</li>
     * </ol>
     *
     * @param studentCriteria X2Criteria
     */
    private void loadTransferTranscripts(X2Criteria studentCriteria) {
        X2Criteria criteria = studentCriteria.copy();
        X2Criteria enrollmentCriteria = new X2Criteria();
        Set<String> transferCodes = getTransferEnrollmentCodes();
        enrollmentCriteria.addIn(StudentEnrollment.COL_ENROLLMENT_CODE, transferCodes);
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE,
                getCurrentContext().getStartDate());
        enrollmentCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, getCurrentContext().getEndDate());
        criteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria));

        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);

        QueryIterator students = getBroker().getIteratorByQuery(query);
        try {
            while (students.hasNext()) {
                SisStudent student = (SisStudent) students.next();
                List<StudentEnrollment> enrollments = m_historyHelper.getStudentEnrollments(student);
                if (enrollments != null) {
                    boolean foundEnrollment = false;
                    StudentEnrollment previousWithdrawal = null;
                    for (StudentEnrollment enrollment : enrollments) {
                        if (foundEnrollment && StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            previousWithdrawal = enrollment;
                            break;
                        }
                        if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType()) &&
                                transferCodes.contains(enrollment.getEnrollmentCode()) &&
                                !enrollment.getEnrollmentDate().before(getCurrentContext().getStartDate()) &&
                                !enrollment.getEnrollmentDate().after(getCurrentContext().getEndDate())) {
                            foundEnrollment = true;
                        }
                    }
                    loadTransferTranscripts(student.getOid(), previousWithdrawal);
                }
            }
        } finally {
            if (students != null) {
                students.close();
            }
        }
    }

    /**
     * Loads the transcripts for an individual qualifying student and adds the transcripts to the
     * collection.
     *
     * @param studentOid String
     * @param previous StudentEnrollment
     */
    private void loadTransferTranscripts(String studentOid, StudentEnrollment previous) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Transcript.COL_STUDENT_OID, studentOid);
        criteria.addEmpty(Transcript.COL_MASTER_SCHEDULE_OID, getBroker().getPersistenceKey());
        criteria.addLessThan(
                Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + SisDistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(getCurrentContext().getSchoolYear()));
        if (previous != null) {
            criteria.addGreaterOrEqualThan(
                    Transcript.REL_DISTRICT_CONTEXT + PATH_DELIMITER + SisDistrictSchoolYearContext.COL_END_DATE,
                    previous.getEnrollmentDate());
        }
        QueryByCriteria query = new QueryByCriteria(Transcript.class, criteria);

        QueryIterator transcripts = getBroker().getIteratorByQuery(query);
        try {
            while (transcripts.hasNext()) {
                Transcript transcript = (Transcript) transcripts.next();
                if (m_grades.isEmpty() || m_grades.contains(transcript.getGradeLevel())) {
                    Collection<Transcript> collection = m_transcriptMap.get(studentOid);
                    if (collection == null) {
                        collection = new LinkedList<Transcript>();
                        m_transcriptMap.put(studentOid, collection);
                    }
                    collection.add(transcript);
                }
            }
        } finally {
            if (transcripts != null) {
                transcripts.close();
            }
        }
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveDefault.TRN_CALC_ID, new RetrieveDefault());
        calcs.put(RetrieveOther.TRN_CALC_ID, new RetrieveOther());
        calcs.put(RetrieveTranscript.TRN_CALC_ID, new RetrieveTranscript());
        calcs.put(FieldRetrieverSSN.TRN_CALC_ID, new FieldRetrieverSSN());
        calcs.put(RetrieveCourseInfo.TRN_CALC_ID, new RetrieveCourseInfo());
        calcs.put(RetrieveInstProgramStdBean.TN_CALC_INSTPGM_ID, new RetrieveInstProgramStdBean());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FiledValidatorSSN.TRN_VAL_ID, new FiledValidatorSSN());
        super.addValidators(validators);
    }
}

