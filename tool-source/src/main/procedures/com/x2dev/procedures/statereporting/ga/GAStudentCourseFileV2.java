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
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for GA Student Course File export.
 *
 * @author X2 Development Corporation
 */
public class GAStudentCourseFileV2 extends StateReportData {
    /**
     * Entity class for GA Student Course Profile export.
     *
     * @author X2 Development Corporation
     */
    public static class StudentCourseFileEntity extends StateReportEntity {
        /**
         * List of a student schedule's additional teachers
         */
        List<ScheduleTeacher> m_teachers;

        /**
         * Student schedule span for entity
         */
        List<Object> m_scheduleSpans;

        /**
         * GAStudentClass object
         */
        GAStudentCourseFileV2 m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentCourseFileEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return entity name
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = STRING_EMPTY;

            name +=
                    student.getNameView() + " [Local ID: " + student.getLocalId() + ", State ID: "
                            + student.getStateId() + "] ";

            return name;
        }

        /**
         * Retrieves and returns a student's schedule span or transcript.
         *
         * @return StudentScheduleSpan for the current row
         */
        public Object getCurrentRecord() {
            Object currentSpan = null;
            if (m_scheduleSpans != null && getCurrentRow() < m_scheduleSpans.size() && getCurrentRow() >= 0) {
                currentSpan = m_scheduleSpans.get(getCurrentRow());
            }
            return currentSpan;
        }

        /**
         * Gets the current section.
         *
         * @return Master schedule
         */
        public MasterSchedule getCurrentSection() {
            Object currentObject = getCurrentRecord();
            MasterSchedule section = null;
            if (currentObject != null) {
                if (currentObject instanceof Transcript) {
                    section = ((Transcript) currentObject).getMasterSchedule();
                } else {
                    section = ((StudentScheduleSpan) currentObject).getSection();
                }
            }
            return section;
        }

        /**
         * Gets the current student schedule span.
         *
         * @return Student schedule span
         */
        public StudentScheduleSpan getCurrentStudentScheduleSpan() {
            StudentScheduleSpan span = null;
            Object currentObject = getCurrentRecord();
            if (currentObject != null) {
                if (currentObject instanceof StudentScheduleSpan) {
                    span = (StudentScheduleSpan) currentObject;
                }
            }
            return span;
        }

        /**
         * Gets the current trancript.
         *
         * @param useSpan boolean
         * @return Transcript
         */
        public Transcript getCurrentTrancript(boolean useSpan) {
            Transcript transcript = null;
            Object currentObject = getCurrentRecord();
            if (currentObject != null) {
                if (currentObject instanceof Transcript) {
                    transcript = (Transcript) currentObject;
                } else if (useSpan) {
                    StudentScheduleSpan span = (StudentScheduleSpan) currentObject;
                    transcript = span.getTranscript();
                }
            }
            return transcript;
        }

        /**
         * Initialize.
         *
         * Create map of StudentScheduleSpan objects for each student.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.stateexports.StateReportData,
         *      com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (GAStudentCourseFileV2) data;
            SisStudent student = (SisStudent) bean;

            m_scheduleSpans = new ArrayList<Object>();

            // This will run once for each Student
            List<StudentScheduleSpan> studentScheduleSpans = null;
            List<StudentEnrollmentSpan> studentEnrollmentSpans = null;
            studentScheduleSpans = m_data.m_helper.getStudentScheduleSpans(student);
            studentEnrollmentSpans = m_data.m_helper.getStudentEnrollmentSpans(student, true);

            if (studentScheduleSpans != null && studentScheduleSpans.size() > 0) {
                for (StudentScheduleSpan scheduleSpan : studentScheduleSpans) {

                    StudentSchedule sscToCheck = scheduleSpan.getSchedule();
                    if (sscToCheck != null && BooleanAsStringConverter.TRUE
                            .equals(sscToCheck.getFieldValueByBeanPath(m_data.m_sscExclude))) {
                        continue;
                    }
                    StudentScheduleChange sccToCheckEntry = scheduleSpan.getEntryChange();
                    if (sccToCheckEntry != null && BooleanAsStringConverter.TRUE
                            .equals(sccToCheckEntry.getFieldValueByBeanPath(m_data.m_sccExclude))) {
                        continue;
                    }
                    StudentScheduleChange sccToCheckExit = scheduleSpan.getExitChange();
                    if (sccToCheckExit != null && BooleanAsStringConverter.TRUE
                            .equals(sccToCheckExit.getFieldValueByBeanPath(m_data.m_sccExclude))) {
                        continue;
                    }
                    if (scheduleSpan.getEntryChange() != null
                            && scheduleSpan.getExitChange() != null
                            && scheduleSpan.getEntryChange().getEffectiveDate()
                                    .equals(scheduleSpan.getExitChange().getEffectiveDate())) {
                        continue;
                    }
                    PlainDate exitDate = scheduleSpan.getExitDate();
                    // NOT NECESSARY SINCE ExitDate will always be less than
                    // or equal ExitChange
                    // if there is a schedule change, and its date is before
                    // the
                    // schedule span exit date, use that as the end date.
                    if (scheduleSpan.getExitChange() != null
                            && scheduleSpan.getExitChange().getEffectiveDate().before(exitDate)) {
                        exitDate = scheduleSpan.getExitChange().getEffectiveDate();
                    }
                    // set the start date to the schedule span entry
                    // date.
                    PlainDate entryDate = scheduleSpan.getEntryDate();
                    // NOT NECESSARY SINCE EntryDate will always be
                    // greater than EntryChange
                    // if there is a schedule change, and its date
                    // is after the
                    // schedule span entry date, use that as the
                    // start date.
                    if (scheduleSpan.getEntryChange() != null
                            && scheduleSpan.getEntryChange().getEffectiveDate()
                                    .after(entryDate)) {
                        entryDate = scheduleSpan.getEntryChange().getEffectiveDate();
                    }
                    if (entryDate != null && exitDate != null && entryDate.equals(exitDate)) {
                        continue;
                    }

                    ScheduleTerm schedTerm = null;
                    String scheduleTermCode = null;
                    MasterSchedule section = scheduleSpan.getSection();
                    PlainDate scheduleStartDate = scheduleSpan.getEntryDate();
                    PlainDate scheduleEndDate = scheduleSpan.getExitDate();
                    SisSchool sectionSchool = null;
                    String sectionSchoolOid = null;
                    SchoolCourse schoolCourse = null;
                    StudentScheduleChange dropRecord = null;

                    String sectionSchoolStateCode = null;

                    if (section != null) {
                        schoolCourse = section.getSchoolCourse();
                        schedTerm = section.getScheduleTerm();
                        if (schedTerm != null) {
                            scheduleTermCode = schedTerm.getCode();
                        }
                        if (schoolCourse != null) {
                            sectionSchool = schoolCourse.getSchool();
                            sectionSchoolOid = sectionSchool.getOid();

                            sectionSchoolStateCode =
                                    (String) sectionSchool.getFieldValueByBeanPath(m_data.m_schoolCodeField);
                        }
                    }
                    if (m_selectedTermCodes == null || m_selectedTermCodes.size() == 0
                            || m_selectedTermCodes.contains(scheduleTermCode)) {
                        // Only include sections that started prior to the report date
                        if (scheduleStartDate != null && !scheduleStartDate.after(m_data.m_reportDate)) {
                            dropRecord = scheduleSpan.getExitChange();
                            // If no drop record is found for schedule span
                            // (section has not been dropped),
                            // automatically add the schedule span record.
                            if (dropRecord == null) {
                                m_scheduleSpans.add(scheduleSpan);
                                continue;
                            }
                            if (studentEnrollmentSpans != null && studentEnrollmentSpans.size() > 0) {
                                for (StudentEnrollmentSpan enrollmentSpan : studentEnrollmentSpans) {
                                    SisSchool enrollmentSchool = enrollmentSpan.getSchool();
                                    String enrollmentSchoolOid = enrollmentSchool.getOid();
                                    PlainDate enrollmentStartDate = enrollmentSpan.getFirstActiveDate();
                                    PlainDate enrollmentEndDate = enrollmentSpan.getLastActiveDate();

                                    String enrSchoolStateCode =
                                            (String) enrollmentSchool.getFieldValueByBeanPath(m_data.m_schoolCodeField);

                                    if (sectionSchoolOid.equals(enrollmentSchoolOid)
                                            || (!StringUtils.isEmpty(enrSchoolStateCode)
                                                    && !StringUtils.isEmpty(sectionSchoolStateCode)
                                                    && enrSchoolStateCode
                                                            .equals(sectionSchoolStateCode))) {
                                        boolean scheduleStartOverlaps = false;
                                        boolean scheduleEndOverlaps = false;
                                        // If school matches on section and
                                        // enrollment span, make sure the
                                        // schedule span's start or end date
                                        // falls
                                        // on or between the start and end dates
                                        // of the enrollment span. This
                                        // eliminates schedule spans
                                        // which occur while the student is not
                                        // enrolled at the school (usually from
                                        // bad drop records)
                                        if ((enrollmentStartDate == null || !scheduleStartDate
                                                .before(enrollmentStartDate))
                                                && (enrollmentEndDate == null || !scheduleStartDate
                                                        .after(enrollmentEndDate))) {
                                            scheduleStartOverlaps = true;
                                        }
                                        if ((enrollmentStartDate == null || !scheduleEndDate
                                                .before(enrollmentStartDate))
                                                && (enrollmentEndDate == null || !scheduleEndDate
                                                        .after(enrollmentEndDate))) {
                                            scheduleEndOverlaps = true;
                                        }
                                        if (scheduleStartOverlaps || scheduleEndOverlaps) {
                                            m_scheduleSpans.add(scheduleSpan);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Collection<Transcript> trns = student.getTranscripts();
            for (Transcript trn : trns) {
                if ((m_selectedTermCodes == null || m_selectedTermCodes.size() == 0
                        || m_selectedTermCodes.contains(trn.getTermCode()))
                        && trn.getDistrictContextOid().equals(m_data.getCurrentContext().getOid()) &&
                        (BooleanAsStringConverter.TRUE
                                .equals(trn.getFieldValueByBeanPath(m_data.m_fieldTrnOutOfDistrict))
                                || trn.getUserDescriptionIndicator())) {
                    m_scheduleSpans.add(trn);
                }
            }
            m_scheduleSpans.sort(new Comparator() {
                @Override
                public int compare(Object o1, Object o2) {
                    int res = 0;
                    if (o1 instanceof StudentScheduleSpan) {
                        if (o2 instanceof Transcript) {
                            res = 1;
                        } else {
                            res = ((StudentScheduleSpan) o1).getSection().getOid()
                                    .compareTo(((StudentScheduleSpan) o2).getSection().getOid());
                        }
                        if (res == 0) {
                            res = ((StudentScheduleSpan) o1).getEntryDate()
                                    .compareTo(((StudentScheduleSpan) o2).getEntryDate());
                        }
                    } else {
                        if (o2 instanceof StudentScheduleSpan) {
                            res = -1;
                        } else {
                            res = ((Transcript) o1).getOid().compareTo(((Transcript) o2).getOid());
                        }
                    }
                    return res;
                }
            });
            // Individual records are a single Student Schedule Span which
            // represent one section that a student was in.
            setRowCount(m_scheduleSpans.size());
        }
    }

    /**
     * RetrieveAlternateSchoolCode retrieves the value from the Program Participation table.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAlternateSchoolCode implements FieldRetriever {

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
            MasterSchedule mst = ((StudentCourseFileEntity) entity).getCurrentSection();
            return mst == null ? null : mst.getFieldValueByBeanPath(m_mstAltSklCodeField);
        }
    }

    /**
     * RetrieveAlternateSystemCode retrieves the value from the Program Participation table.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAlternateSystemCode implements FieldRetriever {

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
            MasterSchedule mst = ((StudentCourseFileEntity) entity).getCurrentSection();
            return mst == null ? null : mst.getFieldValueByBeanPath(m_mstAltSysCodeField);
        }
    }

    /**
     * Retrieve the class start date or end date, as specified by the field parameter.
     *
     * @author Follett Software Company
     *
     */
    protected class RetrieveClassDate implements FieldRetriever {
        private final String CLASS_END_DATE = "END";
        private final String CLASS_START_DATE = "START";

        private Map<String, KeyValuePair<PlainDate, PlainDate>> m_datesMap = new HashMap<>();

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
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;
            GAStudentCourseFileV2 gaData = (GAStudentCourseFileV2) data;
            String param = (String) field.getParameter();
            Object value = null;
            Transcript transcript = scEntity.getCurrentTrancript(false);
            StudentScheduleSpan schSpan = scEntity.getCurrentStudentScheduleSpan();
            if (CLASS_END_DATE.equals(param)) {
                if (schSpan != null) {
                    // set the end date to the schedule span exit date.
                    PlainDate exitDate = schSpan.getExitDate();

                    // NOT NECESSARY SINCE ExitDate will always be less than
                    // or equal ExitChange
                    // if there is a schedule change, and its date is before
                    // the
                    // schedule span exit date, use that as the end date.
                    if (schSpan.getExitChange() != null
                            && schSpan.getExitChange().getEffectiveDate().before(exitDate)) {
                        exitDate = schSpan.getExitChange().getEffectiveDate();
                    }
                    value = exitDate;
                } else if (transcript != null && transcript.getMasterSchedule() != null) {
                    String termOid = transcript.getMasterSchedule().getScheduleTermOid();
                    KeyValuePair<PlainDate, PlainDate> pair = getScheduleTermDates(gaData, termOid);
                    if (pair != null) {
                        value = pair.getValue();
                    }
                }
            } else if (CLASS_START_DATE.equals(param)) {
                if (schSpan != null) {
                    value = schSpan.getEntryDate();
                } else if (transcript != null && transcript.getMasterSchedule() != null) {
                    String termOid = transcript.getMasterSchedule().getScheduleTermOid();
                    KeyValuePair<PlainDate, PlainDate> pair = getScheduleTermDates(gaData, termOid);
                    if (pair != null) {
                        value = pair.getKey();
                    }
                }
            }
            return value;
        }

        /**
         * Gets the schedule term dates.
         *
         * @param data GAStudentCourseFileV2
         * @param oid String
         * @return Pair
         */
        protected KeyValuePair<PlainDate, PlainDate> getScheduleTermDates(GAStudentCourseFileV2 data, String oid) {
            KeyValuePair<PlainDate, PlainDate> pairToReturn = m_datesMap.get(oid);
            if (pairToReturn == null) {
                Collection<ScheduleTermDate> termDates = data.getScheduleTermDates(oid);
                if (termDates != null) {
                    for (ScheduleTermDate schDate : termDates) {
                        PlainDate startDate = schDate.getStartDate();
                        PlainDate endDate = schDate.getEndDate();
                        pairToReturn = new KeyValuePair<PlainDate, PlainDate>(startDate, endDate);
                        m_datesMap.put(oid, pairToReturn);
                        break;
                    }
                }
            }
            return pairToReturn;
        }
    }

    /**
     * RetrieveCoTeacher retrieves the co-teacher name.
     *
     * @author Follett Software Company
     */
    protected class RetrieveCoTeacher implements FieldRetriever {
        List<String> EIP_DELIVERY_MODEL_CODES = Arrays.asList("2", "4");
        List<String> ESOL_DELIVERY_CODES = Arrays.asList("1", "2");

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
            String value = CONST_NO;
            String teacherRole = STRING_EMPTY;
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;

            String eipDeliveryModel = entity.getFieldValue(EXPORT_FIELD_EIP_DELIVERY_MODEL);
            String esolDelivery = entity.getFieldValue(EXPORT_FIELD_ESOL_DELIVERY);
            if ("3".equals(scEntity.getFieldValue(EXPORT_FIELD_INCLUSION))
                    || "1".equals(scEntity.getFieldValue(EXPORT_FIELD_SE_DELIVERY_MODEL))
                    || (EIP_DELIVERY_MODEL_CODES.contains(eipDeliveryModel)
                            && !ESOL_DELIVERY_CODES.contains(esolDelivery))) {
                return value;
            }

            MasterSchedule currentSection = scEntity.getCurrentSection();
            if (currentSection != null) {
                String sectionOid = currentSection.getOid();
                scEntity.m_teachers =
                        ((GAStudentCourseFileV2) data).getScheduleTeachers(sectionOid, false, true, false);
                if (scEntity.m_teachers != null && !scEntity.m_teachers.isEmpty()) {
                    for (int idx = 0; idx < scEntity.m_teachers.size(); idx++) {
                        ScheduleTeacher teacher = scEntity.m_teachers.get(idx);

                        if (teacher != null) {
                            teacherRole = teacher.getRole();
                            if (CO_TEACHER.equals(teacherRole)) {
                                value = CONST_YES;
                            }
                        }
                    }
                }
            }
            return value;
        }
    }


    /**
     * For T20127373 Export needs to look to the Grade Scale associated to the student transcript
     * and retrieve the Completer Content value assigned to the grade on the scale
     * If final grade is blank or if the code selected does not have an associated state code
     * export should default exporting a value of "N".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveContentCompleter implements FieldRetriever {
        String FINAL_GRADE_EMPTY = "N";
        String REGEX_PATTERN_NUMERIC = "^[0-9]+$";
        final Set<String> CONTENT_COMPLETER_GRADES = new HashSet<String>(Arrays.asList("01", "02", "03", "04", "05",
                "06", "07", "08"));

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
            SisStudent student = (SisStudent) entity.getBean();
            Transcript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(true);
            String gradeLevelOfStudent = student.getGradeLevel();
            String contentCompleterValue = "";
            if (transcript != null && CONTENT_COMPLETER_GRADES.contains(gradeLevelOfStudent)) {
                double finalGradeNumber = 0.0;
                GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                String finalGrade = transcript.getFinalGrade();
                Collection<GradeScaleGradeDefinition> gradeScaleGradeDefinitionCollection =
                        scale.getGradeScaleDefinitions();

                for (GradeScaleGradeDefinition gradeScaleGradeDefinition : gradeScaleGradeDefinitionCollection) {
                    if (!StringUtils.isEmpty(finalGrade)) {
                        /*
                         * Check if the final grade is numeric.
                         * Marietta and Laurens have different ways of calculating the grade scale.
                         */
                        try {
                            if (finalGrade.matches(REGEX_PATTERN_NUMERIC)) {
                                finalGradeNumber = Double.parseDouble(finalGrade);
                            }
                            double gradeCutOffValue = gradeScaleGradeDefinition.getGradeCutoffValue().doubleValue();
                            double gradeValue = gradeScaleGradeDefinition.getGradeValue().doubleValue();
                            if ((finalGrade.equals(gradeScaleGradeDefinition.getGradeCode()))
                                    || (finalGrade.matches(REGEX_PATTERN_NUMERIC)
                                            && (finalGradeNumber >= gradeCutOffValue)
                                            && (finalGradeNumber <= gradeValue))) {
                                contentCompleterValue =
                                        (String) gradeScaleGradeDefinition
                                                .getFieldValueByBeanPath(m_doeGsgContentCompleter);
                                break;
                            }
                        } catch (NumberFormatException nfe) {
                            // Nothing.
                        }
                    } else {
                        contentCompleterValue = FINAL_GRADE_EMPTY;
                    }
                }
            }
            return contentCompleterValue;
        }
    }


    /**
     * Retrieve the total credit multiplied by 1000 as a string.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCredit implements FieldRetriever {
        /**
         * Constants
         */
        DecimalFormat m_format = new DecimalFormat("0000");
        private static final String EXPORT_FIELD_ALPHA_GRADE = "Alpha Grade";

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
            String alphaGradeValue = entity.getFieldValue(EXPORT_FIELD_ALPHA_GRADE);
            MasterSchedule currentSection = ((StudentCourseFileEntity) entity).getCurrentSection();
            Transcript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(true);
            if (currentSection != null) {
                Course course = currentSection.getSchoolCourse().getCourse();
                if (course.getCredit().compareTo(BigDecimal.ZERO) == 0) {
                    return value;
                }
                if (transcript != null) {
                    BigDecimal credit = transcript.getTotalCredit();
                    if (credit != null) {
                        value = m_format.format((long) (credit.doubleValue() * 1000));
                    }
                }
            }
            return "Z".equals(alphaGradeValue) ? null : "0000".equals(value) ? "0" : value;
        }
    }


    /**
     * Returns grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGrade implements FieldRetriever {
        private static final String GRADE_PARAM_LETTER = "LETTER";
        private static final String GRADE_PARAM_NUMERIC = "NUMERIC";

        GradesManager m_gradesManager = null;

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
            Transcript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(true);
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            StudentScheduleChange change = null;
            if (currentSpan != null) {
                change = currentSpan.getExitChange();
            }
            String parameter = (String) field.getParameter();
            Object returnObject = null;
            if (transcript != null) {
                m_gradesManager = new GradesManager(data.getBroker());

                // Check ungraded course.
                // GradeScale scale =
                // m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                String finalGrade = transcript.getFinalGrade();
                boolean isNumber = false;
                // See if the grade in the final grade column is a grade scale value
                // (letter grade).
                if (!StringUtils.isEmpty(finalGrade))// && scale != null)
                {
                    String regex = REGEX_NUMERIC;
                    isNumber = finalGrade.matches(regex);
                }

                // Determine which grade to return.
                if (GRADE_PARAM_LETTER.equals(parameter)) {
                    if (!isNumber) {
                        if (!StringUtils.isEmpty(finalGrade)) {
                            returnObject = finalGrade;
                        } else if (change != null) {
                            returnObject = "Z";
                        }
                    }
                } else if (GRADE_PARAM_NUMERIC.equals(parameter)) {
                    if (isNumber) {
                        returnObject = finalGrade;
                    }
                }
            } else if (GRADE_PARAM_LETTER.equals(parameter) && change != null) {
                returnObject = "Z";
            }

            return returnObject;
        }
    }


    /**
     * Returns grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        private static final String CALC_ID = "GRADE-LEVEL";

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
            Transcript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(false);
            String returnObject = null;
            if (transcript != null) {
                returnObject = transcript.getGradeLevel();
            } else {
                SisStudent std = (SisStudent) entity.getBean();
                String grade = std.getGradeLevel();
                if (!StringUtils.isEmpty(grade)) {
                    returnObject = data.lookupStateValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, grade);
                }
            }
            return returnObject;
        }
    }


    /**
     * RetrievePrimaryArea retrieves the value of Primary Area from the Student. This value is
     * returned blank if the
     * Student's sped exit date is before or on the FTE Count date
     *
     * @author Follett Software Company
     */
    protected class RetrievePrimaryArea implements FieldRetriever {

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
            String value = EMPTY_STRING;
            SisStudent student = (SisStudent) entity.getBean();
            String seDeliveryModel = entity.getFieldValue(EXPORT_FIELD_SE_DELIVERY_MODEL);
            if (!StringUtils.isEmpty(seDeliveryModel)) {
                String primaryExp = (String) student.getFieldValueByBeanPath(m_doeFieldStdPrimExp);
                if (!StringUtils.isEmpty(primaryExp)) {
                    value = data.lookupStateValue(SisStudent.class, m_doeFieldStdPrimExp, primaryExp);
                }
            }
            return value;
        }
    }

    /**
     * RetrievePrimaryTeacherFirstName retrieves first name of the primary teacher name.
     *
     * @author Follett Software Company
     */
    protected class RetrievePrimaryTeacherName implements FieldRetriever {

        private static final String CALC_PARAM_FIRST = "GA-PRI-TEACH-FIRST";
        private static final String CALC_PARAM_LAST = "GA-PRI-TEACH-LAST";

        Map<String, String[]> m_mapValues = new HashMap();

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
            String value = STRING_EMPTY;
            String[] nameArray = null;
            MasterSchedule section = ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                if (m_mapValues.containsKey(section.getOid())) {
                    nameArray = m_mapValues.get(section.getOid());
                } else {
                    if (section.getStaffView() != null
                            && (section.getStaffView().contains(STRING_COMMA))) {

                        Collection<ScheduleTeacher> teachers = section.getTeacherSections();
                        if (teachers.size() > 1) {
                            for (ScheduleTeacher scheduleTeacher : teachers) {
                                if (scheduleTeacher.getPrimaryTeacherIndicator() == true) {
                                    nameArray = scheduleTeacher.getStaff().getNameView().split(STRING_COMMA);
                                    break;
                                }
                            }
                        }
                        if (nameArray == null) {
                            nameArray = section.getStaffView().split(STRING_SEMICOLON);
                            nameArray = nameArray[0].split(STRING_COMMA);
                        }

                    }
                    m_mapValues.put(section.getOid(), nameArray);
                }
                if ((field.getParameter().equals(CALC_PARAM_FIRST)) && (nameArray != null)
                        && (nameArray.length > 1)) {
                    value = nameArray[1];
                }
                if ((field.getParameter().equals(CALC_PARAM_LAST)) && (nameArray != null)
                        && (nameArray.length > 0)) {
                    value = nameArray[0];
                }
            }
            return value;
        }
    }


    /**
     * Generate and return a record type code in the format "Pnn" where "nn" is an integer 1-75.
     *
     * Do this by counting how many StudentSchedule records we have processed for the current
     * student.
     * When changing students, reset the count.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRecordType implements FieldRetriever {
        private DecimalFormat m_format = new DecimalFormat("00");
        private Map<String, Collection<String>> m_schedOidsMap = new HashMap<String, Collection<String>>();
        private Collection<String> schedOids = new ArrayList<String>();
        private String m_studentOid = null;

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
            MasterSchedule section = ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                SisStudent currentStudent = (SisStudent) entity.getBean();
                m_studentOid = currentStudent.getOid();
                if (m_schedOidsMap.containsKey(m_studentOid)) {
                    schedOids = m_schedOidsMap.get(m_studentOid);
                    schedOids.add(section.getOid());
                    m_schedOidsMap.put(m_studentOid, schedOids);
                } else {
                    schedOids.clear();
                    schedOids.add(section.getOid());
                    m_schedOidsMap.put(m_studentOid, schedOids);
                }
            }
            return FIELD_PREFIX + m_format.format(schedOids.size());
        }
    }


    /**
     * Retrieve the reporting period.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveReportingPeriod implements FieldRetriever {

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
            GAStudentCourseFileV2 gaStudentClassData = (GAStudentCourseFileV2) entity.getData();

            return gaStudentClassData.m_reportingPeriod;
        }
    }


    /**
     * Retrieve a bean property and strip off invalid characters.
     * Useful for cleaning names for reporting.
     * Also trim the name to maximum field length to avoid validation warnings.
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
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) entity.getBean().getFieldValueByBeanPath(field.getBeanPath());
            int max = field.getMaxLength();
            if (!StringUtils.isEmpty(nameValue)) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll(STRING_EMPTY);
                if (cleanValue.length() > max) {
                    cleanValue = cleanValue.substring(0, max);
                }
            } else {
                cleanValue = STRING_EMPTY;
            }

            return cleanValue;
        }
    }


    /**
     * Retrieve the school from the entity. This will be the
     * school on report date or last withdrawal.
     * Optionally, a student can have an override school code assigned.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchool implements FieldRetriever {
        private final String SCHOOL_CODE = "SCHOOL_CODE";
        private final String SYSTEM_CODE = "SYSTEM_CODE";
        private final String FISCAL_YEAR = "FISC";

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
            String value = null;
            String param = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();
            MasterSchedule section = ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                SisSchool school = section.getSchoolCourse().getSchool();
                SisOrganization organization = null;
                if (SCHOOL_CODE.equals(param)) {
                    value = (String) student.getFieldValueByBeanPath(m_overrideSchoolCodeField);
                    if (StringUtils.isEmpty(value)) {
                        if (school != null && m_schoolCodeField != null) {
                            value = (String) school.getFieldValueByBeanPath(m_schoolCodeField);
                        }
                    }
                } else if (SYSTEM_CODE.equals(param) || FISCAL_YEAR.equals(param)) {
                    if (school != null) {
                        organization = school.getOrganization1();
                    }
                    if (SYSTEM_CODE.equals(param)) {
                        value = (String) organization.getFieldValueByBeanPath(m_doeDistrict);
                    }
                    if (FISCAL_YEAR.equals(param) && organization != null && organization.getCurrentContext() != null) {
                        int schoolYear = organization.getCurrentContext().getSchoolYear();
                        value = String.valueOf(schoolYear);
                    }
                }
            }
            return value;
        }
    }


    /**
     * Retrieve the section information from the entity.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSectionInfo implements FieldRetriever {
        private static final String ALT_ED = "ALT_ED";
        private static final String CLASS_PERIOD = "CLASS_PERIOD";
        private static final String COURSE_NAME = "COURSE_NAME";
        private static final String COURSE_NUM = "COURSE_NUM";
        private static final String COURSE_TEACH_ID = "COURSE_TEACH_ID";
        private static final String CREDIT_RECOVERY = "CREDIT_RECOVERY";
        private static final String DOE_PARAPRO = "PARAPRO";
        private static final String EIP_DELIVERY_MODEL = "EIP_DELIV";
        private static final String ENRICHMENT = "ENRICHMENT";
        private static final String ESOL_DELIV = "ESOL_DELIV";
        private static final String FIELD_NAME_ROLE = "role";
        private static final String GIFTED_DELIVERY = "GIFTED_DELIVERY";
        private static final String IMMERSION_LANG = "IMMERSION_LANG";
        private static final String INCLUSION = "INCLUSION";
        private static final String LIEU_CRS = "LIEU_CRS";
        private static final String ONLINE_COURSE = "ONLINE_COURSE";
        private static final String SE_DELIVERY = "SE_DELIVERY";
        private static final String SECTION_NUM = "SECTION_NUM";
        private static final String WBL_CATEGORY = "WBL_CATEGORY";

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
            String value = null;
            String param = (String) field.getParameter();
            StudentScheduleSpan span = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            Student student = (Student) entity.getBean();
            Transcript trn = ((StudentCourseFileEntity) entity).getCurrentTrancript(false);
            MasterSchedule mst = ((StudentCourseFileEntity) entity).getCurrentSection();
            SchoolCourse schoolCourse = null;
            Course course = null;

            if (mst != null) {
                if (DOE_PARAPRO.equals(param)) {
                    int count = 0;
                    Collection<ScheduleTeacher> sectionTeachers =
                            ((GAStudentCourseFileV2) data).getScheduleTeachers(mst.getOid(), true, true, true);
                    if (sectionTeachers != null) {
                        for (ScheduleTeacher sectionTeacher : sectionTeachers) {
                            String teacherRole = sectionTeacher.getRole();
                            String type = lookupStateValue(ScheduleTeacher.class, FIELD_NAME_ROLE,
                                    teacherRole);
                            if (PARAPRO_STATE_CODE.equals(type)) {
                                count++;
                            }
                        }
                    }
                    value = String.valueOf(count);
                } else if (ONLINE_COURSE.equals(param)) {
                    DataDictionaryField dictionaryField =
                            getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_REMOTE_STUDENT);
                    if (dictionaryField != null
                            && StringUtils.isEqual(
                                    (String) student.getFieldValueByBeanPath(dictionaryField.getJavaName()),
                                    BooleanAsStringConverter.TRUE)) {
                        return "Y";
                    }
                    if (mst != null) {
                        value = (String) mst.getFieldValueByBeanPath(m_doeOnlineCourse);
                    }
                } else if (INCLUSION.equals(param)) {
                    if (span != null) {
                        if (span.getExitChange() != null) {
                            String mstFederalInclusion =
                                    lookupFederalValue(MasterSchedule.class, m_mstInclusionField,
                                            (String) mst.getFieldValueByBeanPath(m_mstInclusionField));
                            if (!StringUtils.isEmpty(mstFederalInclusion)
                                    && CODE_FEDERAL_INCLUSION.equalsIgnoreCase(mstFederalInclusion)) {
                                value = lookupStateValue(MasterSchedule.class, m_mstInclusionField,
                                        (String) mst.getFieldValueByBeanPath(m_mstInclusionField));
                            }
                            if (value == null
                                    && span.getExitChange().getFieldValueByBeanPath(m_sccInclusionStdField) != null
                                    && BooleanAsStringConverter.TRUE.equals(
                                            span.getExitChange().getFieldValueByBeanPath(m_sccInclusionStdField))) {
                                String inclusionCode = (String) span.getExitChange()
                                        .getFieldValueByBeanPath(m_sccInclusionStdCodeField);
                                if (!StringUtils.isEmpty(inclusionCode)) {
                                    value = lookupStateValue(StudentScheduleChange.class, m_sccInclusionStdCodeField,
                                            inclusionCode);
                                }
                            }
                        } else {
                            String mstFederalInclusion =
                                    lookupFederalValue(MasterSchedule.class, m_mstInclusionField,
                                            (String) mst.getFieldValueByBeanPath(m_mstInclusionField));
                            if (!StringUtils.isEmpty(mstFederalInclusion)
                                    && CODE_FEDERAL_INCLUSION.equalsIgnoreCase(mstFederalInclusion)) {
                                value = lookupStateValue(MasterSchedule.class, m_mstInclusionField,
                                        (String) mst.getFieldValueByBeanPath(m_mstInclusionField));
                            }
                            if (value == null
                                    && span.getSchedule().getFieldValueByBeanPath(m_sscInclusionStdField) != null
                                    && BooleanAsStringConverter.TRUE.equals(
                                            span.getSchedule().getFieldValueByBeanPath(m_sscInclusionStdField))) {
                                String inclusionCode = (String) span.getSchedule()
                                        .getFieldValueByBeanPath(m_sscInclusionStdCodeField);
                                if (!StringUtils.isEmpty(inclusionCode)) {
                                    value = lookupStateValue(StudentSchedule.class, m_sscInclusionStdCodeField,
                                            inclusionCode);
                                }
                            }
                        }
                    }
                } else if (SE_DELIVERY.equals(param)) {
                    if (span != null) {
                        if (span.getExitChange() != null) {
                            String mstFederalDelivery =
                                    lookupFederalValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                            (String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                            if (!StringUtils.isEmpty(mstFederalDelivery)
                                    && CODE_FEDERAL_SE.equalsIgnoreCase(mstFederalDelivery)) {
                                value = lookupStateValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                        (String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                            }
                            if (value == null) {
                                String mstLocalDelivery =
                                        lookupReferenceCodeByBeanPath(MasterSchedule.class, m_mstDeliveryModelStdField,
                                                (String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField),
                                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                                if (!StringUtils.isEmpty(mstLocalDelivery) && mstLocalDelivery.equals(CODE_LOCAL_M)
                                        && span.getExitChange().getFieldValueByBeanPath(m_sccInclusionStdField) != null
                                        && BooleanAsStringConverter.TRUE.equals(
                                                span.getExitChange().getFieldValueByBeanPath(m_sccInclusionStdField))) {
                                    String inclusionCode = (String) span.getExitChange()
                                            .getFieldValueByBeanPath(m_sccDeliveryModelStdField);
                                    String inclusionFederal = null;
                                    if (!StringUtils.isEmpty(inclusionCode)
                                            && !StringUtils.isEmpty(inclusionFederal = lookupFederalValue(
                                                    StudentSchedule.class, m_sccDeliveryModelStdField, inclusionCode))
                                            && CODE_FEDERAL_SE.equals(inclusionFederal)) {
                                        value = lookupStateValue(StudentScheduleChange.class,
                                                m_sccDeliveryModelStdField,
                                                inclusionCode);
                                    }
                                }
                            }
                            String addValue = (String) span.getExitChange()
                                    .getFieldValueByBeanPath(m_sccDeliveryModelStdFieldAdd);
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_SE.equals(lookupFederalValue(StudentScheduleChange.class,
                                            m_sccDeliveryModelStdFieldAdd, addValue))) {
                                value = lookupStateValue(StudentScheduleChange.class, m_sccDeliveryModelStdFieldAdd,
                                        addValue);
                            }
                        } else {
                            String mstFederalDelivery =
                                    lookupFederalValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                            (String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                            if (!StringUtils.isEmpty(mstFederalDelivery)
                                    && CODE_FEDERAL_SE.equalsIgnoreCase(mstFederalDelivery)) {
                                value = lookupStateValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                        (String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                            }
                            if (value == null) {
                                String mstLocalDelivery =
                                        lookupReferenceCodeByBeanPath(MasterSchedule.class, m_mstDeliveryModelStdField,
                                                (String) mst.getFieldValueByBeanPath(m_mstDeliveryModelStdField),
                                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                                if (!StringUtils.isEmpty(mstLocalDelivery) && mstLocalDelivery.equals(CODE_LOCAL_M)
                                        && span.getSchedule().getFieldValueByBeanPath(m_sscInclusionStdField) != null
                                        && BooleanAsStringConverter.TRUE.equals(
                                                span.getSchedule().getFieldValueByBeanPath(m_sscInclusionStdField))) {
                                    String inclusionCode = (String) span.getSchedule()
                                            .getFieldValueByBeanPath(m_sscDeliveryModelStdField);
                                    String inclusionFederal = null;
                                    if (!StringUtils.isEmpty(inclusionCode)
                                            && !StringUtils.isEmpty(inclusionFederal = lookupFederalValue(
                                                    StudentSchedule.class, m_sscDeliveryModelStdField, inclusionCode))
                                            && CODE_FEDERAL_SE.equals(inclusionFederal)) {
                                        value = lookupStateValue(StudentSchedule.class,
                                                m_sscDeliveryModelStdField,
                                                inclusionCode);
                                    }
                                }
                            }
                            String addValue = (String) span.getSchedule()
                                    .getFieldValueByBeanPath(m_sscDeliveryModelStdFieldAdd);
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_SE.equals(lookupFederalValue(StudentSchedule.class,
                                            m_sscDeliveryModelStdFieldAdd, addValue))) {
                                value = lookupStateValue(StudentSchedule.class, m_sscDeliveryModelStdFieldAdd,
                                        addValue);
                            }
                        }
                    }
                } else if (LIEU_CRS.equals(param)) {
                    value =
                            BooleanAsStringConverter.TRUE.equals(mst.getFieldValueByBeanPath(m_doeMstInLieuCrs))
                                    ? CONST_YES
                                    : CONST_NO;
                } else if (GIFTED_DELIVERY.equals(param)) {
                    if (span != null) {
                        if (span.getExitChange() != null) {
                            String sccCalcValue =
                                    (String) span.getExitChange().getFieldValueByBeanPath(m_sccCalcStateCodeField);
                            if (!StringUtils.isEmpty(sccCalcValue) && sccCalcValue.matches(CRS_NUM_PATTERN)
                                    && "2".equals(String.valueOf(sccCalcValue.charAt(3)))) {
                                value = lookupStateValue(StudentScheduleChange.class, m_sccDeliveryModelStdField,
                                        (String) span.getExitChange()
                                                .getFieldValueByBeanPath(m_sccDeliveryModelStdField));
                                if (value == null) {
                                    value = lookupStateValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                            (String) span.getSection()
                                                    .getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                                }
                            }
                            String addValue = (String) span.getExitChange()
                                    .getFieldValueByBeanPath(m_sccDeliveryModelStdFieldAdd);
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_GIFTED.equals(lookupFederalValue(StudentScheduleChange.class,
                                            m_sccDeliveryModelStdFieldAdd, addValue))) {
                                value = lookupStateValue(StudentScheduleChange.class, m_sccDeliveryModelStdFieldAdd,
                                        addValue);
                            }
                        } else {
                            String sscCalcValue =
                                    (String) span.getSchedule().getFieldValueByBeanPath(m_sscCalcStateCodeField);
                            if (!StringUtils.isEmpty(sscCalcValue) && sscCalcValue.matches(CRS_NUM_PATTERN)
                                    && "2".equals(String.valueOf(sscCalcValue.charAt(3)))) {
                                value = lookupStateValue(StudentSchedule.class, m_sscDeliveryModelStdField,
                                        (String) span.getSchedule()
                                                .getFieldValueByBeanPath(m_sscDeliveryModelStdField));
                                if (value == null) {
                                    value = lookupStateValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                            (String) span.getSection()
                                                    .getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                                }
                            }
                            String addValue = (String) span.getSchedule()
                                    .getFieldValueByBeanPath(m_sscDeliveryModelStdFieldAdd);
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_GIFTED.equals(lookupFederalValue(StudentSchedule.class,
                                            m_sscDeliveryModelStdFieldAdd, addValue))) {
                                value = lookupStateValue(StudentSchedule.class, m_sscDeliveryModelStdFieldAdd,
                                        addValue);
                            }
                        }
                    }
                } else if (IMMERSION_LANG.equals(param)) {
                    String code = (String) mst.getFieldValueByBeanPath(m_doeMstImmersionLang);
                    if (!StringUtils.isEmpty(code)) {
                        value = lookupStateValue(MasterSchedule.class, m_doeMstImmersionLang, code);
                        if (!StringUtils.isEmpty(value) && value.length() < field.getMaxLength()) {
                            int zerosCount = field.getMaxLength() - value.length();
                            for (int i = 0; i < zerosCount; i++) {
                                value = "0" + value;
                            }
                        }
                    }
                } else if (WBL_CATEGORY.equals(param)) {
                    if (mst != null) {
                        schoolCourse = mst.getSchoolCourse();
                        if (schoolCourse != null) {
                            course = schoolCourse.getCourse();
                            if (course != null) {
                                if (BooleanAsStringConverter.TRUE
                                        .equals(course.getFieldValueByBeanPath(m_fieldCrsWblCategory))) {
                                    String code = (String) mst.getFieldValueByBeanPath(m_fieldMstWblCategory);
                                    if (!StringUtils.isEmpty(code)) {
                                        value = lookupStateValue(MasterSchedule.class, m_fieldMstWblCategory, code);
                                    }
                                }
                            }
                        }
                    }
                } else if (EIP_DELIVERY_MODEL.equals(param)) {
                    String stdGrade = entity.getFieldValue(EXPORT_FIELD_GRADE);
                    if (!StringUtils.isEmpty(stdGrade) && !ILLEGAL_GRADES_FOR_EIP.contains(stdGrade)) {
                        if (span != null) {
                            if (span.getExitChange() != null) {
                                String sccCalcValue =
                                        (String) span.getExitChange()
                                                .getFieldValueByBeanPath(m_sccCalcStateCodeField);
                                if (!StringUtils.isEmpty(sccCalcValue) && sccCalcValue.matches(CRS_NUM_PATTERN)
                                        && "1".equals(String.valueOf(sccCalcValue.charAt(3)))) {
                                    value = lookupStateValue(StudentScheduleChange.class, m_sccDeliveryModelStdField,
                                            (String) span.getExitChange()
                                                    .getFieldValueByBeanPath(m_sccDeliveryModelStdField));
                                    if (value == null) {
                                        value = lookupStateValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                                (String) span.getSection()
                                                        .getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                                    }
                                }
                            } else {
                                String sscCalcValue =
                                        (String) span.getSchedule().getFieldValueByBeanPath(m_sscCalcStateCodeField);
                                if (!StringUtils.isEmpty(sscCalcValue) && sscCalcValue.matches(CRS_NUM_PATTERN)
                                        && "1".equals(String.valueOf(sscCalcValue.charAt(3)))) {
                                    value = lookupStateValue(StudentSchedule.class, m_sscDeliveryModelStdField,
                                            (String) span.getSchedule()
                                                    .getFieldValueByBeanPath(m_sscDeliveryModelStdField));
                                    if (value == null) {
                                        value = lookupStateValue(MasterSchedule.class, m_mstDeliveryModelStdField,
                                                (String) span.getSection()
                                                        .getFieldValueByBeanPath(m_mstDeliveryModelStdField));
                                    }
                                }
                            }
                        }
                    }
                } else if (CREDIT_RECOVERY.equals(param)) {
                    value = (String) mst.getFieldValueByBeanPath(m_doeCreditRecovery);
                } else if (ALT_ED.equals(param)) {
                    value = (String) mst.getFieldValueByBeanPath(m_doeAltEd);
                } else if (SECTION_NUM.equals(param)) {
                    if (trn != null) {
                        value = "000";
                    } else {
                        value = mst.getSectionNumber();
                    }
                } else if (CLASS_PERIOD.equals(param)) {
                    if (trn != null) {
                        value = "00";
                    } else {
                        Schedule schedule = mst.getSchedule();
                        String scheduleOid = schedule.getOid();
                        int numberOfPeriods = schedule.getPeriods();
                        String sectionOid = mst.getOid();
                        for (int i = 1; i <= numberOfPeriods; i++) {
                            Collection<String> sectionOidsForPeriod = m_periodMapBySchedule.get(scheduleOid + i);
                            if (sectionOidsForPeriod.contains(sectionOid)) {
                                value = String.valueOf(i);
                                break;
                            }
                        }
                    }
                } else if (COURSE_NAME.equals(param)) {
                    if (trn != null) {
                        value = trn.getCourseDescription();
                    } else {
                        int maxLength = field.getMaxLength();
                        value = mst.getDescription();
                        if (value.length() > maxLength) {
                            value = value.substring(0, maxLength);
                        }
                    }
                } else if (COURSE_TEACH_ID.equals(param)) {
                    SisStaff primaryStaff = null;
                    SisPerson staffPerson = null;
                    if (mst != null && mst.getPrimaryStaff() != null) {
                        primaryStaff = mst.getPrimaryStaff();
                    }
                    if (primaryStaff != null) {
                        staffPerson = primaryStaff.getPerson();
                    }
                    if (staffPerson != null) {
                        value = (String) staffPerson.getFieldValueByBeanPath(m_doeFieldSsn);
                        DataDictionaryField dictionaryField =
                                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_DOE_REMOTE_STUDENT);
                        if (dictionaryField != null && value != null && !value.startsWith(CODE_777)
                                && !value.startsWith(CODE_770) &&
                                StringUtils.isEqual(
                                        (String) student.getFieldValueByBeanPath(dictionaryField.getJavaName()),
                                        BooleanAsStringConverter.TRUE)) {
                            value = "444444444";
                        }
                    }
                } else if (ESOL_DELIV.equals(param)) {
                    // inactive courses
                    if (span != null) {
                        if (span.getExitChange() != null) {
                            String sccCalcValue =
                                    (String) span.getExitChange().getFieldValueByBeanPath(m_sccCalcStateCodeField);
                            String mstDeliveryCode =
                                    (String) span.getSection().getFieldValueByBeanPath(m_mstDeliveryModelStdField);
                            String mstDeliveryCodeLocal = "";
                            if (!StringUtils.isEmpty(mstDeliveryCode)) {
                                mstDeliveryCodeLocal =
                                        lookupReferenceCodeByBeanPath(MasterSchedule.class, m_mstDeliveryModelStdField,
                                                mstDeliveryCode,
                                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                            }
                            if (!StringUtils.isEmpty(sccCalcValue) && sccCalcValue.matches(CRS_NUM_PATTERN)
                                    && "0".equals(String.valueOf(sccCalcValue.charAt(3)))) {
                                if (!StringUtils.isEmpty(mstDeliveryCodeLocal)) {
                                    if ("M".equals(mstDeliveryCodeLocal)) {
                                        String sccFederalValue = lookupFederalValue(StudentScheduleChange.class,
                                                m_sccDeliveryModelStdField,
                                                (String) span.getExitChange()
                                                        .getFieldValueByBeanPath(m_sccDeliveryModelStdField));
                                        if (!StringUtils.isEmpty(sccFederalValue)
                                                && CODE_FEDERAL_ESOL.equalsIgnoreCase(sccFederalValue)) {
                                            value = lookupStateValue(StudentScheduleChange.class,
                                                    m_sccDeliveryModelStdField,
                                                    (String) span.getExitChange()
                                                            .getFieldValueByBeanPath(m_sccDeliveryModelStdField));
                                        }
                                    } else if ("0".equals(mstDeliveryCodeLocal)) {
                                        String sccInclusionStdFieldValue = (String) span.getExitChange()
                                                .getFieldValueByBeanPath(m_sccInclusionStdField);
                                        if (!StringUtils.isEmpty(sccInclusionStdFieldValue)
                                                && BooleanAsStringConverter.TRUE
                                                        .equals(sccInclusionStdFieldValue)) {
                                            String mstFederalDelivery = lookupFederalValue(MasterSchedule.class,
                                                    m_mstDeliveryModelStdField, mstDeliveryCode);
                                            if (!StringUtils.isEmpty(mstFederalDelivery)
                                                    && CODE_FEDERAL_ESOL.equalsIgnoreCase(mstFederalDelivery)) {
                                                value = lookupStateValue(MasterSchedule.class,
                                                        m_mstDeliveryModelStdField, mstDeliveryCode);
                                            }
                                        }
                                    }
                                }
                            }
                            String addValue = (String) span.getExitChange()
                                    .getFieldValueByBeanPath(m_sccDeliveryModelStdFieldAdd);
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_ESOL.equals(lookupFederalValue(StudentScheduleChange.class,
                                            m_sccDeliveryModelStdFieldAdd, addValue))) {
                                value = lookupStateValue(StudentScheduleChange.class, m_sccDeliveryModelStdFieldAdd,
                                        addValue);
                            }
                        } else {
                            String sscCalcValue =
                                    (String) span.getSchedule().getFieldValueByBeanPath(m_sscCalcStateCodeField);
                            String mstDeliveryCode =
                                    (String) span.getSection().getFieldValueByBeanPath(m_mstDeliveryModelStdField);
                            String mstDeliveryCodeLocal = "";
                            if (!StringUtils.isEmpty(mstDeliveryCode)) {
                                mstDeliveryCodeLocal =
                                        lookupReferenceCodeByBeanPath(MasterSchedule.class, m_mstDeliveryModelStdField,
                                                mstDeliveryCode,
                                                ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                            }
                            if (!StringUtils.isEmpty(sscCalcValue) && sscCalcValue.matches(CRS_NUM_PATTERN)
                                    && "0".equals(String.valueOf(sscCalcValue.charAt(3)))) {
                                if (!StringUtils.isEmpty(mstDeliveryCodeLocal)) {
                                    if ("M".equals(mstDeliveryCodeLocal)) {
                                        String sscFederalDelivery = lookupFederalValue(StudentSchedule.class,
                                                m_sscDeliveryModelStdField,
                                                (String) span.getSchedule()
                                                        .getFieldValueByBeanPath(m_sscDeliveryModelStdField));
                                        if (!StringUtils.isEmpty(sscFederalDelivery)
                                                && CODE_FEDERAL_ESOL.equalsIgnoreCase(sscFederalDelivery)) {
                                            value = lookupStateValue(StudentSchedule.class,
                                                    m_sscDeliveryModelStdField,
                                                    (String) span.getSchedule()
                                                            .getFieldValueByBeanPath(m_sscDeliveryModelStdField));
                                        }
                                    } else if ("0".equals(mstDeliveryCodeLocal)) {
                                        if (span.getSchedule() != null) {
                                            String sccInclusionStdFieldValue = (String) span.getSchedule()
                                                    .getFieldValueByBeanPath(m_sscInclusionStdField);
                                            if (!StringUtils.isEmpty(sccInclusionStdFieldValue)
                                                    && BooleanAsStringConverter.TRUE
                                                            .equals(sccInclusionStdFieldValue)) {
                                                String mstFederalDelivery = lookupFederalValue(MasterSchedule.class,
                                                        m_mstDeliveryModelStdField, mstDeliveryCode);
                                                if (!StringUtils.isEmpty(mstFederalDelivery)
                                                        && CODE_FEDERAL_ESOL.equalsIgnoreCase(mstFederalDelivery)) {
                                                    value = lookupStateValue(MasterSchedule.class,
                                                            m_mstDeliveryModelStdField, mstDeliveryCode);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            String addValue = (String) span.getSchedule()
                                    .getFieldValueByBeanPath(m_sscDeliveryModelStdFieldAdd);
                            if (!StringUtils.isEmpty(addValue)
                                    && CODE_FEDERAL_ESOL.equals(lookupFederalValue(StudentSchedule.class,
                                            m_sscDeliveryModelStdFieldAdd, addValue))) {
                                value = lookupStateValue(StudentSchedule.class, m_sscDeliveryModelStdFieldAdd,
                                        addValue);
                            }
                        }
                    }
                } else if (COURSE_NUM.equals(param)) {
                    if (trn != null) {
                        if (trn != null) {
                            if (trn.getUserDescriptionIndicator() && trn.getEquivalentSchoolCourse() != null) {
                                value = trn.getEquivalentSchoolCourse().getNumber();
                            } else {
                                value = trn.getSchoolCourse().getNumber();
                            }
                        }
                    } else if (span != null) {
                        if (span.getExitChange() != null) {
                            value = (String) span.getExitChange().getFieldValueByBeanPath(m_sccCalcStateCodeField);
                        } else {
                            value = (String) span.getSchedule().getFieldValueByBeanPath(m_sscCalcStateCodeField);
                        }

                    }
                } else if (ENRICHMENT.equals(param)) {
                    if (span != null) {
                        String federalValue = null;
                        if (span.getExitChange() != null) {
                            String deliveryCode =
                                    (String) span.getExitChange().getFieldValueByBeanPath(m_sccDeliveryModelStdField);
                            federalValue = lookupFederalValue(StudentScheduleChange.class, m_sccDeliveryModelStdField,
                                    deliveryCode);
                        } else if (span.getSchedule() != null) {
                            String deliveryCode =
                                    (String) span.getSchedule().getFieldValueByBeanPath(m_sscDeliveryModelStdField);
                            federalValue = lookupFederalValue(StudentSchedule.class, m_sscDeliveryModelStdField,
                                    deliveryCode);
                        }
                        if (CODE_FEDERAL_ENRICHMENT.equals(federalValue)) {
                            value = CONST_YES;
                        } else {
                            value = CONST_NO;
                        }
                    }
                }
            }
            return value;
        }
    }


    /**
     * Retrieve the student's attendance.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStudent90 implements FieldRetriever {
        // Map<sklOid, Map<patDate, Set<stdOid>>>
        private Map<String, Map<PlainDate, Set<String>>> m_attMap = new HashMap();
        // Map<mstOid, Map<patDate, Set<stdOid>>>
        private Map<String, Map<PlainDate, Set<String>>> m_patMap = new HashMap();

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
            String value = null;
            GAStudentCourseFileV2 scData = (GAStudentCourseFileV2) data;
            SisStudent student = (SisStudent) entity.getBean();
            MasterSchedule currentSection = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (currentSpan != null) {
                currentSection = currentSpan.getSection();
            }
            if (currentSection != null) {
                TreeSet<SisSchoolCalendarDate> sectionDates = scData.getSectionCalendarDates(currentSection);
                int numberOfMemberPeriods = 0;
                int numberOfAbsencePeriods = 0;
                int numberOfSectionPeriods = 0;
                for (SisSchoolCalendarDate schoolDate : sectionDates) {
                    Set<SchedulePeriod> periods = scData.getSectionPeriods(currentSection, schoolDate);
                    if (periods != null) {
                        numberOfSectionPeriods += periods.size();
                        if (!currentSpan.getEntryDate().after(schoolDate.getDate())
                                && !currentSpan.getExitDate().before(schoolDate.getDate())) {

                            numberOfMemberPeriods += periods.size();

                            if (isAbsent(student, currentSection, schoolDate.getDate())) {
                                numberOfAbsencePeriods += periods.size();
                            }
                        }
                    }
                }
                if (numberOfSectionPeriods > 0) {
                    value = (numberOfMemberPeriods - numberOfAbsencePeriods) / (double) numberOfSectionPeriods >= 0.895
                            ? "Y"
                            : "N";
                    if (m_debug) {
                        value += ": " + numberOfSectionPeriods + "," + numberOfMemberPeriods + ","
                                + numberOfAbsencePeriods;
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is absent.
         *
         * @param student SisStudent
         * @param section MasterSchedule
         * @param date PlainDate
         * @return true, if is absent
         */
        private boolean isAbsent(SisStudent student, MasterSchedule section, PlainDate date) {
            boolean absentIndicator = false;
            Map<PlainDate, Set<String>> dateMap = null;
            if (BooleanAsStringConverter.TRUE
                    .equals(student.getSchool().getFieldValueByBeanPath(m_doeFieldSklPerAtt))) {
                dateMap = m_patMap.get(section.getOid());
                if (dateMap == null) {
                    dateMap = loadPatDateMap(section.getOid());
                    m_patMap.put(section.getOid(), dateMap);
                }
            } else {
                dateMap = m_attMap.get(section.getSchedule().getSchoolOid());
                if (dateMap == null) {
                    dateMap = loadAttDateMap(section.getSchedule().getSchoolOid());
                    m_attMap.put(section.getSchedule().getSchoolOid(), dateMap);
                }
            }

            Set<String> stdOids = dateMap.get(date);
            if (stdOids != null && stdOids.contains(student.getOid())) {
                absentIndicator = true;
            }
            return absentIndicator;
        }

        /**
         * Load att date map.
         *
         * @param schoolOid String
         * @return Map
         */
        private Map<PlainDate, Set<String>> loadAttDateMap(String schoolOid) {
            Map<PlainDate, Set<String>> results = new HashMap();

            X2Criteria criteria = new X2Criteria();
            criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_schoolYearStartDate);
            criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_reportDate);
            criteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
            criteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, schoolOid);

            String[] columns = new String[] {StudentAttendance.COL_DATE, StudentAttendance.COL_STUDENT_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(StudentAttendance.class, columns, criteria);
            query.addOrderByAscending(StudentAttendance.COL_DATE);
            query.addOrderByAscending(StudentAttendance.COL_STUDENT_OID);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    PlainDate date = new PlainDate((java.util.Date) row[0]);
                    String stdOid = (String) row[1];

                    Set<String> stdOids = results.get(date);
                    if (stdOids == null) {
                        stdOids = new HashSet();
                        results.put(date, stdOids);
                    }
                    stdOids.add(stdOid);
                }
            } finally {
                iterator.close();
            }
            return results;
        }

        /**
         * Load pat date map.
         *
         * @param sectionOid String
         * @return Map
         */
        private Map<PlainDate, Set<String>> loadPatDateMap(String sectionOid) {
            Map<PlainDate, Set<String>> results = new HashMap();

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(StudentPeriodAttendance.COL_MASTER_SCHEDULE_OID, sectionOid);
            criteria.addEqualTo(StudentPeriodAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

            String[] columns = new String[] {StudentPeriodAttendance.COL_DATE, StudentPeriodAttendance.COL_STUDENT_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(StudentPeriodAttendance.class, columns, criteria);
            query.addOrderByAscending(StudentPeriodAttendance.COL_DATE);
            query.addOrderByAscending(StudentPeriodAttendance.COL_STUDENT_OID);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    PlainDate date = new PlainDate((java.util.Date) row[0]);
                    String stdOid = (String) row[1];

                    Set<String> stdOids = results.get(date);
                    if (stdOids == null) {
                        stdOids = new HashSet();
                        results.put(date, stdOids);
                    }
                    stdOids.add(stdOid);
                }
            } finally {
                iterator.close();
            }
            return results;
        }
    }


    /**
     * Get the course section's additional teachers' days
     *
     * Parameters: "1", "2", or "3".
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacher90 implements FieldRetriever {

        private static final String CALC_PARAM_PRIM_90 = "PRIM_90";

        private Map<String, String> m_mapValues = new HashMap();

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
            String result = null;
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;
            GAStudentCourseFileV2 scData = (GAStudentCourseFileV2) data;
            MasterSchedule currentSection = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            if (currentSpan != null) {
                currentSection = currentSpan.getSection();
            }
            if (currentSection != null) {
                String sectionOid = currentSection.getOid();
                String param = (String) field.getParameter();
                if (CALC_PARAM_PRIM_90.equals(param)) {
                    ScheduleTeacher primTeacher = ((GAStudentCourseFileV2) data).getPrimaryTeacher(sectionOid);
                    result = getValueByTeacher(scData, primTeacher, currentSection);
                } else {
                    int idx = Integer.parseInt(param);
                    scEntity.m_teachers =
                            ((GAStudentCourseFileV2) data).getScheduleTeachers(sectionOid, false, true, false);
                    if (scEntity.m_teachers != null && idx < scEntity.m_teachers.size()) {
                        ScheduleTeacher teacher = scEntity.m_teachers.get(idx);
                        result = getValueByTeacher(scData, teacher, currentSection);
                    }
                }
            }
            return result;
        }

        /**
         * Evaluate if membership greater than 90%.
         *
         * @param scData GAStudentCourseFile
         * @param teacher ScheduleTeacher
         * @param currentSection MasterSchedule
         * @return "Y" or "N"
         */
        private String getValueByTeacher(GAStudentCourseFileV2 scData,
                                         ScheduleTeacher teacher,
                                         MasterSchedule currentSection) {

            String value = null;
            if (teacher != null && currentSection != null) {
                String key = teacher.getOid() + "|" + currentSection.getOid();
                if (m_mapValues.containsKey(key)) {
                    value = m_mapValues.get(key);
                } else {
                    PlainDate classEntryDate = null;
                    int numberOfSectionPeriods = 0;
                    int numberOfTeacherPeriods = 0;
                    TreeSet<SisSchoolCalendarDate> sectionDates = scData.getSectionCalendarDates(currentSection);

                    DateAsStringConverter dateConverter = (DateAsStringConverter) ConverterFactory
                            .getConverterForClass(PlainDate.class.getName(), Locale.getDefault(), true);
                    if (teacher != null && teacher.getFieldValueByBeanPath(m_doeMtcClassEntryDate) != null) {
                        classEntryDate = (PlainDate) dateConverter
                                .parseSystemString((String) teacher.getFieldValueByBeanPath(m_doeMtcClassEntryDate));
                    }

                    for (SisSchoolCalendarDate schoolDate : sectionDates) {
                        Set<SchedulePeriod> periods = scData.getSectionPeriods(currentSection, schoolDate);

                        if (periods != null) {
                            numberOfSectionPeriods += periods.size();
                        }
                    }

                    if (classEntryDate == null) {
                        numberOfTeacherPeriods += numberOfSectionPeriods;
                    } else {
                        for (SisSchoolCalendarDate schoolDate : sectionDates) {
                            if (!schoolDate.getDate().before(classEntryDate)) {
                                Set<SchedulePeriod> periods = scData.getSectionPeriods(currentSection, schoolDate);
                                if (periods != null) {
                                    numberOfTeacherPeriods += periods.size();
                                }
                            }
                        }
                    }

                    if (numberOfSectionPeriods > 0) {
                        value = numberOfTeacherPeriods / (double) numberOfSectionPeriods >= 0.895 ? "Y" : "N";
                    }
                    m_mapValues.put(key, value);
                }
            }

            return value;
        }
    }


    /**
     * Get the course section's additional teachers' ID
     *
     * Parameters: "1", "2", or "3"
     * Returns: The SSN of additional teacher 1, 2, or 3, respectively.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTeacherId implements FieldRetriever {
        List<String> INCLUSION_CODES = Arrays.asList("3", "4", "5", "6", "7");
        List<String> EIP_DELIVERY_MODEL_CODES = Arrays.asList("2", "4");
        List<String> ESOL_DELIVERY_CODES = Arrays.asList("1", "2");

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
            StudentCourseFileEntity scEntity = (StudentCourseFileEntity) entity;

            String inclusion = entity.getFieldValue(EXPORT_FIELD_INCLUSION);
            String eipDeliveryModel = entity.getFieldValue(EXPORT_FIELD_EIP_DELIVERY_MODEL);
            String esolDelivery = entity.getFieldValue(EXPORT_FIELD_ESOL_DELIVERY);
            if (INCLUSION_CODES.contains(inclusion) ||
                    (EIP_DELIVERY_MODEL_CODES.contains(eipDeliveryModel)
                            && !ESOL_DELIVERY_CODES.contains(esolDelivery))) {
                return "";
            }

            String result = null;
            String param = (String) field.getParameter();
            MasterSchedule section = ((StudentCourseFileEntity) entity).getCurrentSection();
            if (section != null) {
                String sectionOid = section.getOid();
                scEntity.m_teachers =
                        ((GAStudentCourseFileV2) data).getScheduleTeachers(sectionOid, false, true, false);
                int idx = Integer.parseInt(param);
                if (scEntity.m_teachers != null && idx < scEntity.m_teachers.size()) {
                    ScheduleTeacher teacher = scEntity.m_teachers.get(idx);
                    SisStaff staff = null;
                    SisPerson person = null;
                    if (teacher != null) {
                        staff = teacher.getStaff();
                        if (staff != null) {
                            person = staff.getPerson();
                            if (person != null) {
                                result = (String) person.getFieldValueByBeanPath(m_doeFieldSsn);
                                if (!StringUtils.isEmpty(result)) {
                                    result = result.replace(STRING_HYPHEN, STRING_EMPTY);
                                }
                            }
                        }
                    }
                }
            }

            return result;
        }
    }


    /**
     * Convert the schedule term of the class section to a
     * code for the course period.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTermCode implements FieldRetriever {
        private Map<String, String> m_termMap = new HashMap<String, String>();
        @SuppressWarnings("hiding")
        private Map<String, String> m_termCodesMap = null;

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
            String periodCode = null;
            StudentScheduleSpan currentSpan = ((StudentCourseFileEntity) entity).getCurrentStudentScheduleSpan();
            Transcript transcript = ((StudentCourseFileEntity) entity).getCurrentTrancript(false);
            if (currentSpan != null) {
                MasterSchedule section = currentSpan.getSection();
                if (section != null) {
                    String termOid = section.getScheduleTermOid();
                    if (!StringUtils.isEmpty(termOid)) {
                        periodCode = m_termMap.get(termOid);
                        if (periodCode == null) {
                            ScheduleTerm term = section.getScheduleTerm();
                            if (term != null) {
                                String baseMap = term.getBaseTermMap();
                                periodCode = m_termCodesMap.get(baseMap);
                                m_termMap.put(termOid, periodCode);
                            }
                        }
                    }
                }
            } else if (transcript != null) {
                String code = transcript.getTermCode();
                if (!StringUtils.isEmpty(code)) {
                    periodCode = data.lookupStateValue(Transcript.class, Transcript.COL_TERM_CODE, code);
                }
            }
            return periodCode;
        }

        /**
         * initialize the term code map.
         */
        {
            m_termCodesMap = new HashMap<String, String>();
            m_termCodesMap.put("1", "Y1");
            m_termCodesMap.put("10", "S1");
            m_termCodesMap.put("01", "S2");
            m_termCodesMap.put("100", "T1");
            m_termCodesMap.put("010", "T2");
            m_termCodesMap.put("001", "T3");
            m_termCodesMap.put("1000", "N1");
            m_termCodesMap.put("0100", "N2");
            m_termCodesMap.put("0010", "N3");
            m_termCodesMap.put("0001", "N4");
            m_termCodesMap.put("10000", "X1");
            m_termCodesMap.put("01000", "X2");
            m_termCodesMap.put("00100", "X3");
            m_termCodesMap.put("00010", "X4");
            m_termCodesMap.put("00001", "X5");
            m_termCodesMap.put("100000", "X1");
            m_termCodesMap.put("010000", "X2");
            m_termCodesMap.put("001000", "X3");
            m_termCodesMap.put("000100", "X4");
            m_termCodesMap.put("000010", "X5");
            m_termCodesMap.put("000001", "X6");
        }
    }

    /*
     * General Constants
     */
    private static final String ALT_SLO_REF_NAME = "DOE Alternate SLO";
    private static final char CHAR_COMMA = ',';
    private static final String CO_TEACHER = "Co-Teach";
    private static final String CODE_770 = "770";
    private static final String CODE_777 = "777";
    private static final String CODE_FEDERAL_ENRICHMENT = "ENRICHMENT";
    private static final String CODE_FEDERAL_ESOL = "ESOL";
    private static final String CODE_FEDERAL_GIFTED = "GIFTED";
    private static final String CODE_FEDERAL_INCLUSION = "INCLUSION";
    private static final String CODE_FEDERAL_SE = "SE";
    private static final String CODE_LOCAL_M = "M";
    private static final String CONST_NO = "N";
    private static final String CONST_YES = "Y";
    private static final String CRS_NUM_PATTERN = "\\d{2}\\.\\d{7}";
    private static final String FIELD_PREFIX = "P";
    private static final String PARAPRO_STATE_CODE = "P";
    private static final String SLO_GRADE_SCALE_NAME = "SLO Numeric";
    private static final String STRING_COMMA = ",";
    private static final String STRING_SEMICOLON = ";";
    private static final String STRING_EMPTY = "";
    private static final String STRING_HYPHEN = "-";

    /*
     * Illegal name characters to be removed from strings.
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final Collection ILLEGAL_GRADES_FOR_EIP = Arrays.asList("06", "07", "08", "09", "10", "11", "12");
    private static final String REGEX_NUMERIC = "\\d+";

    /*
     * Aliases
     */
    private static final String ALIAS_DOE_ALT_ED = "DOE Alt Ed Section";
    private static final String ALIAS_DOE_ALT_SKL_CODE = "DOE Alt School Number";
    private static final String ALIAS_DOE_ALT_SLO = "DOE ALTERNATE SLO";
    private static final String ALIAS_DOE_ALT_SYS_CODE = "DOE Alt System Code";
    private static final String ALIAS_DOE_CLASS_ENTRY_DATE = "DOE CLASS ENTRY DATE";
    private static final String ALIAS_DOE_CO_TEACHER = "DOE CO-TEACHER";
    private static final String ALIAS_DOE_GIFTED_DELIV_CRS = "DOE Gifted Delivery";
    private static final String ALIAS_DOE_CREDIT_RECOVERY = "DOE Credit Recovery";
    private static final String ALIAS_DOE_DISTRICT = "DOE District";
    private static final String ALIAS_DOE_EOCT_CODE = "EOCT Course Code";
    private static final String ALIAS_DOE_ESOL_DELIV = "DOE ESOL Delivery";
    private static final String ALIAS_DOE_GSG_CONT_COMPL = "DOE Content Completer";
    private static final String ALIAS_DOE_MST_IMMERS_LANG = "DOE IMMERSION LANGUAGE";
    private static final String ALIAS_DOE_MST_IN_LIEU = "DOE Credit In Lieu";
    private static final String ALIAS_DOE_ONLINE_COURSE = "DOE Online Course";
    private static final String ALIAS_DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    private static final String ALIAS_DOE_REMOTE_STUDENT = "all-std-RemoteStudent";
    private static final String ALIAS_DOE_SCHOOL = "DOE School";
    private static final String ALIAS_DOE_SKL_PER_ATT = "DOE PERIOD ATTENDANCE SCHOOL";
    private static final String ALIAS_DOE_SLO_COURSE = "DOE SLO CRS";
    private static final String ALIAS_DOE_SLO_MEASURE_TYPE = "DOE SLO MEAS TYPE";
    private static final String ALIAS_DOE_SLO_POST_MEASURE = "DOE SLO POST-MEAS";
    private static final String ALIAS_DOE_SLO_PRE_MEASURE = "DOE SLO PRE-MEAS";
    private static final String ALIAS_DOE_SSN = "DOE SSN";
    private static final String ALIAS_DOE_WBL_CATEGORY_CRS = "DOE WBL COURSE";
    private static final String ALIAS_DOE_WBL_CATEGORY_STD = "DOE WBL PROGRAM CATEGORY";
    private static final String ALIAS_DOE_WBL_CATEGORY_MST = "DOE WBL Program Category";
    private static final String ALIAS_STD_PRIM_EXCEPT = "DOE Primary Exceptionality";
    private static final String ALIAS_PGM_TYPE = "DOE Program Type";
    private static final String ALIAS_TRN_OUT_OF_DISTR = "all-trn-OutOfDistrict";

    /**
     * New Aliases for SSC and SCC.
     */
    private static final String ALIAS_MST_DELIVERY_MODEL = "all-mst-SectionDeliveryModel";
    private static final String ALIAS_MST_INCLUSION_CODE = "all-mst-SectionInclusionCode";
    private static final String ALIAS_MST_ALTERNATE_SYS_CODE = "all-mst-AlternateSystemCode";
    private static final String ALIAS_MST_ALTERNATE_SKL_CODE = "all-mst-AlternateSchoolCode";
    private static final String ALIAS_SCC_CALC_STATE_CRS_CODE = "DOE CALCULATED STATE CRS CODE HISTORY";
    private static final String ALIAS_SCC_EXCLUDE = "all-scc-ExcludefromStateReporting";
    private static final String ALIAS_SCC_INCLUDE_STD = "all-scc-DeliveryorInclusionStudent";
    private static final String ALIAS_SCC_STD_DELIVERY_MODEL = "all-scc-StudentDeliveryModel";
    private static final String ALIAS_SCC_STD_DELIVERY_MODEL_ADD = "all-scc-AdditionalStudentDeliveryModel";
    private static final String ALIAS_SCC_STD_INCLUSION_CODE = "all-ssc-StudentInclusionCode";
    private static final String ALIAS_SSC_CALC_STATE_CRS_CODE = "DOE CALCULATED STATE CRS CODE";
    private static final String ALIAS_SSC_EXCLUDE = "all-ssc-ExcludefromStateReporting";
    private static final String ALIAS_SSC_INCLUDE_STD = "all-ssc-DeliveryorInclusionStudent";
    private static final String ALIAS_SSC_STD_DELIVERY_MODEL = "all-ssc-StudentDeliveryModel";
    private static final String ALIAS_SSC_STD_DELIVERY_MODEL_ADD = "all-ssc-AdditionalStudentDeliveryModel";
    private static final String ALIAS_SSC_STD_INCLUSION_CODE = "all-ssc-StudentInclusionCode";


    /**
     * Export's fields
     */
    private static final String EXPORT_FIELD_GRADE = "Grade level";
    private static final String EXPORT_FIELD_SE_DELIVERY_MODEL = "SE Delivery Model";
    private static final String EXPORT_FIELD_INCLUSION = "Inclusion";
    private static final String EXPORT_FIELD_ESOL_DELIVERY = "ESOL Delivery";
    private static final String EXPORT_FIELD_EIP_DELIVERY_MODEL = "EIP Delivery Model";

    /*
     * Parameters
     */
    private static final String PARAM_DEBUG = "debug";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_REPORTING_PERIOD = "reportingPeriod";
    private static final String PARAM_TERM = "term";

    /*
     * Reference Table
     */
    private static final String REF_TABLE_SCHEDULE_TERM_CODES = "rtbSchTermCode";
    private static final String REF_TABLE_STAFF_TYPE_CODES = "rtbStaffType";

    /*
     * Calculation Ids
     */
    private static final String STUDENT_CLASS_ALT_SKL_CODE = "GA-STD-ALT-SKL-CODE";
    private static final String STUDENT_CLASS_ALT_SYS_CODE = "GA-STD-ALT-SYS-CODE";
    private static final String STUDENT_CLASS_CLEAN = "GA-STD-CLS-CLEAN";
    private static final String STUDENT_CLASS_CO_TEACHER = "GA-STD-CLS-CO-TEACH";
    private static final String STUDENT_CLASS_DATE = "GA-STD-CLS-DATE";
    private static final String STUDENT_CLASS_PERIOD = "GA-STD-CLS-PERIOD";
    private static final String STUDENT_CLASS_PRI_TEACH = "GA-PRI-TEACH";
    private static final String STUDENT_CLASS_RECORD = "GA-STD-CLS-RECORD";
    private static final String STUDENT_CLASS_SCHOOL = "GA-STD-CLS-SKL";
    private static final String STUDENT_CLASS_TEACHER = "GA-STD-CLS-TEACHER";
    private static final String STUDENT_CLASS_TERM = "GA-STD-CLS-TERM";
    private static final Object STUDENT_CRS_90 = "GA-STD-90";
    private static final String STUDENT_CRS_CONTENT_COMPL = "GA-CRS-CONT-COMP";
    private static final String STUDENT_CRS_CREDIT = "GA-CRS-CREDIT";
    private static final String STUDENT_CRS_GRADE = "GA-STD-CRS-GRADE";
    private static final String STUDENT_PRIMARY_AREA = "GA-STD-PRIMARY-AREA";
    private static final String STUDENT_SECTION_INFO = "GA-STD-SECTION-INFO";
    private static final Object TEACHER_CRS_90 = "GA-TEACHER-INFO";

    /*
     * Member variables
     */
    protected String m_activeCode;
    protected Collection<DistrictSchoolYearContext> m_contexts;
    protected boolean m_debug = false;
    protected String m_doeAltEd;
    protected String m_doeAlternateSlo;
    protected String m_doeAltSloRefTable;
    protected String m_doeCreditRecovery;
    protected String m_doeDistrict;
    protected String m_doeEoctCode;
    protected String m_doeEsolDelivery;
    protected String m_doeFieldAltSchoolCode;
    protected String m_doeFieldAltSystemCode;
    protected String m_doeFieldCoTeacher;
    protected String m_doeFieldSklPerAtt;
    protected String m_doeFieldSloCourse;
    protected String m_doeFieldSloMeasureType;
    protected String m_doeFieldSloPostMeasure;
    protected String m_doeFieldSloPreMeasure;
    protected String m_doeFieldSsn;
    protected String m_doeFieldStdPrimExp;
    protected String m_doeGiftedDeliveryCourse;
    protected String m_doeOnlineCourse;
    protected String m_doeMstImmersionLang;
    protected String m_doeMstInLieuCrs;
    protected String m_doeMtcClassEntryDate;
    protected String m_doeGsgContentCompleter;
    protected String m_fieldCrsWblCategory;
    protected String m_fieldMstWblCategory;
    protected String m_fieldPgmType;
    protected String m_fieldStdWblCategory;
    protected String m_fieldTrnOutOfDistrict;
    protected Map<String, String> m_gradeCodeToValueMap;
    protected Map<String, GradeScale> m_gradeScales;
    protected StudentHistoryHelper m_helper;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected Set<String> m_initializedSchedules = new HashSet();
    protected String m_mstDeliveryModelStdField;
    protected String m_mstInclusionField;
    protected String m_mstAltSysCodeField;
    protected String m_mstAltSklCodeField;
    protected String m_overrideSchoolCodeField;
    protected Map<String, Collection<String>> m_periodMapBySchedule = new HashMap<String, Collection<String>>();
    protected Map<String, Collection<StudentProgramParticipation>> m_programParticipationMap;
    protected PlainDate m_reportDate;
    protected String m_reportingPeriod;
    protected Map<String, List<ScheduleDay>> m_scheduleDays = new HashMap();
    protected String m_sccCalcStateCodeField;
    protected String m_sccDeliveryModelStdField;
    protected String m_sccDeliveryModelStdFieldAdd;
    protected String m_sccExclude;
    protected String m_sccInclusionStdField;
    protected String m_sccInclusionStdCodeField;
    protected String m_sscExclude;
    protected String m_sscInclusionStdCodeField;
    protected ScheduleManager m_scheduleMgr;
    protected Map<String, SchedulePeriod> m_schedulePeriods = new HashMap();
    protected Map<String, Collection<ScheduleTermDate>> m_scheduleTermDates = new HashMap();
    protected Map<String, ScheduleTerm> m_scheduleTerms = new HashMap();
    protected Comparator<SisSchoolCalendarDate> m_schoolCalendarDatesComparator;
    protected String m_schoolCodeField;
    protected PlainDate m_schoolYearStartDate;
    protected Map<String, Map<String, TreeSet<SisSchoolCalendarDate>>> m_sectionDates = new HashMap();
    protected Map<String, Set<SchedulePeriod>> m_sectionTermDayPeriods = new HashMap();
    protected Map<String, Collection<ScheduleTerm>> m_sectionTermsMap = new HashMap();
    protected Map<String, String> m_sectionToCalendarCode = new HashMap();
    protected static ArrayList<String> m_selectedTermCodes;
    protected String m_sscCalcStateCodeField;
    protected String m_sscDeliveryModelStdField;
    protected String m_sscDeliveryModelStdFieldAdd;
    protected String m_sscInclusionStdField;
    protected Set<String> m_staffParaCodes;
    protected GAStudentCourseFileV2 m_studentClassData = null;
    protected Map<String, String> m_termCodesMap = new HashMap();
    protected Set<String> m_termOids;


    private Map<String, Collection<ScheduleTeacher>> m_scheduleTeacherMap2;

    /**
     * Close.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
     */
    @Override
    public void close() {
        super.close();
        if (m_helper != null) {
            m_helper.close();
        }
    }

    /**
     * Gets the school calendar dates comparator.
     *
     * @return the school calendar dates comparator
     */
    public Comparator<SisSchoolCalendarDate> getSchoolCalendarDatesComparator() {
        if (m_schoolCalendarDatesComparator == null) {
            m_schoolCalendarDatesComparator = new Comparator<SisSchoolCalendarDate>() {
                @Override
                public int compare(SisSchoolCalendarDate o1, SisSchoolCalendarDate o2) {
                    PlainDate date1 = o1.getDate();
                    PlainDate date2 = o2.getDate();
                    if (date1 == null) {
                        return -1;
                    } else if (date2 == null) {
                        return 1;
                    }
                    return date1.compareTo(date2);
                }
            };
        }
        return m_schoolCalendarDatesComparator;
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        // Initialize fields+
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            loadTermCodes();

            // Get selected Term Codes
            String rcdOidList = (String) getParameter(PARAM_TERM);
            HashSet<String> rcdOids = new HashSet(StringUtils.convertDelimitedStringToList(rcdOidList, CHAR_COMMA));
            m_selectedTermCodes = new ArrayList();

            for (String rcdOid : rcdOids) {
                String termCode = m_termCodesMap.get(rcdOid);
                m_selectedTermCodes.add(termCode);
            }

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_schoolYearStartDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.materializeStudentCriteria();

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(StudentCourseFileEntity.class);

            // Load in maps for sectionOids by period
            loadSectionsByPeriod();

            loadStudentProgramParticipationMap();

            // Populate map with the codes/values from the grade scale used to
            // translate non-numeric codes to the state codes.
            loadSloGradeScale();
            loadGradescales();

            // Add any retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(STUDENT_CLASS_DATE, new RetrieveClassDate());
            calcs.put(STUDENT_CLASS_PERIOD, new RetrieveReportingPeriod());
            calcs.put(STUDENT_CLASS_RECORD, new RetrieveRecordType());
            calcs.put(STUDENT_CLASS_SCHOOL, new RetrieveSchool());
            calcs.put(STUDENT_CLASS_TERM, new RetrieveTermCode());
            calcs.put(STUDENT_CLASS_TEACHER, new RetrieveTeacherId());
            calcs.put(STUDENT_CLASS_CLEAN, new RetrieveStripNameChar());
            calcs.put(STUDENT_CLASS_CO_TEACHER, new RetrieveCoTeacher());
            calcs.put(STUDENT_CLASS_PRI_TEACH, new RetrievePrimaryTeacherName());
            calcs.put(STUDENT_CLASS_ALT_SYS_CODE, new RetrieveAlternateSystemCode());
            calcs.put(STUDENT_CLASS_ALT_SKL_CODE, new RetrieveAlternateSchoolCode());
            calcs.put(STUDENT_SECTION_INFO, new RetrieveSectionInfo());
            calcs.put(STUDENT_PRIMARY_AREA, new RetrievePrimaryArea());
            calcs.put(STUDENT_CRS_GRADE, new RetrieveGrade());
            calcs.put(STUDENT_CRS_CREDIT, new RetrieveCredit());
            calcs.put(STUDENT_CRS_CONTENT_COMPL, new RetrieveContentCompleter());
            calcs.put(STUDENT_CRS_90, new RetrieveStudent90());
            calcs.put(TEACHER_CRS_90, new RetrieveTeacher90());
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            super.addCalcs(calcs);
        }
    }

    /**
     * Gets the primary teacher.
     *
     * @param sectionOid String
     * @return Schedule teacher
     */
    protected ScheduleTeacher getPrimaryTeacher(String sectionOid) {
        List<ScheduleTeacher> teachers = getScheduleTeachers(sectionOid, true, false, false);
        return teachers != null && !teachers.isEmpty() ? teachers.iterator().next() : null;
    }

    /**
     * Get the ScheduleDay for a schedule and schedule day number.
     *
     * @param sch Schedule
     * @param dayNumber int
     * @return String
     */
    protected ScheduleDay getScheduleDay(Schedule sch, int dayNumber) {
        List<ScheduleDay> days = getScheduleDays(sch);
        for (ScheduleDay day : days) {
            if (day.getNumber() == dayNumber) {
                return day;
            }
        }
        return null;
    }

    /**
     * return sorted list of schedule days for the schedule.
     *
     * @param sch Schedule
     * @return List
     */
    protected List<ScheduleDay> getScheduleDays(Schedule sch) {
        List<ScheduleDay> items = m_scheduleDays.get(sch.getOid());
        if (items == null) {
            Collection<ScheduleDay> days = sch.getScheduleDays();
            items = new ArrayList(days.size());
            items.addAll(days);
            Collections.sort(items, new Comparator<ScheduleDay>() {

                @Override
                public int compare(ScheduleDay o1, ScheduleDay o2) {
                    return o1.getNumber() - o2.getNumber();
                }
            });
            m_scheduleDays.put(sch.getOid(), items);
        }
        return items;
    }

    /**
     * Get the SchedulePeriod for a period oid.
     *
     * @param perOid String
     * @return SchedulePeriod
     */
    protected SchedulePeriod getSchedulePeriod(String perOid) {
        SchedulePeriod period = m_schedulePeriods.get(perOid);
        if (!m_schedulePeriods.containsKey(perOid)) {
            period = (SchedulePeriod) getBroker().getBeanByOid(SchedulePeriod.class, perOid);
            m_schedulePeriods.put(perOid, period);
        }
        return period;
    }

    /**
     * Gets the schedule teachers.
     *
     * @param sectionOid String
     * @param includePrimary boolean
     * @param includeSecondary boolean
     * @param includePara boolean
     * @return List
     */
    protected List<ScheduleTeacher> getScheduleTeachers(String sectionOid,
                                                        boolean includePrimary,
                                                        boolean includeSecondary,
                                                        boolean includePara) {
        if (m_scheduleTeacherMap2 == null) {
            X2Criteria scheduleTeacherCriteria = new X2Criteria();

            if (isSchoolContext()) {
                scheduleTeacherCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + Section.REL_SCHEDULE
                        + PATH_DELIMITER + Schedule.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                scheduleTeacherCriteria
                        .addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + Section.REL_SCHEDULE
                                + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                                + SisSchool.COL_INACTIVE_INDICATOR,
                                Boolean.TRUE);
                scheduleTeacherCriteria
                        .addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + Section.REL_SCHEDULE
                                + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                                + SisSchool.COL_ARCHIVE_INDICATOR,
                                Boolean.TRUE);
            }

            // get all ScheduleTeacher's that are in active schedules
            scheduleTeacherCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER
                    + MasterSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER
                    + SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                    + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    ScheduleTeacher.REL_SECTION + PATH_DELIMITER + MasterSchedule.COL_SCHEDULE_OID);

            BeanQuery scheduleTeacherQuery = new BeanQuery(ScheduleTeacher.class, scheduleTeacherCriteria);

            m_scheduleTeacherMap2 =
                    getBroker().getGroupedCollectionByQuery(scheduleTeacherQuery, ScheduleTeacher.COL_SECTION_OID, 100);
        }
        List<ScheduleTeacher> teachers = new LinkedList();
        Collection<ScheduleTeacher> candidates = m_scheduleTeacherMap2.get(sectionOid);
        if (candidates != null) {
            for (ScheduleTeacher candidate : candidates) {
                if (getParaCodes().contains(candidate.getStaff().getStaffType())) {
                    if (includePara) {
                        teachers.add(candidate);
                    }
                } else {
                    if (candidate.getPrimaryTeacherIndicator() && includePrimary) {
                        teachers.add(candidate);
                    }
                    if (!candidate.getPrimaryTeacherIndicator() && includeSecondary) {
                        teachers.add(candidate);
                    }
                }
            }
        }
        teachers.sort(new Comparator() {
            @Override
            public int compare(Object o1, Object o2) {
                ScheduleTeacher t1 = (ScheduleTeacher) o1;
                ScheduleTeacher t2 = (ScheduleTeacher) o2;
                return t1.getOid().compareTo(t2.getOid());
            }
        });
        return teachers;
    }

    /**
     * Get the schedule term.
     *
     * @param oid String
     * @return Schedule term
     */
    protected ScheduleTerm getScheduleTerm(String oid) {
        ScheduleTerm term = null;
        if (m_scheduleTerms.containsKey(oid)) {
            term = m_scheduleTerms.get(oid);
        } else {
            term = (ScheduleTerm) getBroker().getBeanByOid(ScheduleTerm.class, oid);
            m_scheduleTerms.put(oid, term);
        }
        return term;
    }

    /**
     * Get the schedule term dates.
     *
     * @param oid String
     * @return Collection<ScheduleTermDate>
     */
    protected Collection<ScheduleTermDate> getScheduleTermDates(String oid) {
        Collection<ScheduleTermDate> dates = null;
        if (m_scheduleTermDates.containsKey(oid)) {
            dates = m_scheduleTermDates.get(oid);
        } else {
            ScheduleTerm term = getScheduleTerm(oid);
            if (term != null) {
                dates = term.getScheduleTermDates();
                m_scheduleTermDates.put(oid, dates);
            }
        }
        return dates;
    }

    /**
     * Retrieve the most common calendar code for a section.
     *
     * @param section MasterSchedule
     * @return String
     */
    protected String getSectionCalendarCode(MasterSchedule section) {
        String calendarCode;
        if (m_sectionToCalendarCode.containsKey(section.getOid())) {
            calendarCode = m_sectionToCalendarCode.get(section.getOid());
        } else {
            if (m_scheduleMgr == null) {
                m_scheduleMgr = new ScheduleManager(getBroker());
            }
            calendarCode = m_scheduleMgr.getMostCommonCalendar(section.getSchedule(), null);
            m_sectionToCalendarCode.put(section.getOid(), calendarCode);
        }
        return calendarCode;
    }

    /**
     * Calculate set of dates that are potential days for the section to meet.
     *
     * @param section MasterSchedule
     * @return Tree set
     */
    protected TreeSet<SisSchoolCalendarDate> getSectionCalendarDates(MasterSchedule section) {
        String calendarCode = getSectionCalendarCode(section);

        String key =
                section.getSchedule().getDistrictContextOid() + section.getSchedule().getSchoolOid() + calendarCode
                        + m_schoolYearStartDate + m_reportDate;
        Map<String, TreeSet<SisSchoolCalendarDate>> termDates = m_sectionDates.get(key);
        if (termDates == null) {
            termDates = new HashMap<String, TreeSet<SisSchoolCalendarDate>>();
            m_sectionDates.put(key, termDates);

            X2Criteria criteriaDates = new X2Criteria();
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_DISTRICT_CONTEXT_OID, section.getSchedule().getDistrictContextOid());
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID, section.getSchedule().getSchoolOid());
            criteriaDates.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                    SchoolCalendar.COL_CALENDAR_ID, calendarCode);
            criteriaDates.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            criteriaDates.addGreaterOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_schoolYearStartDate);
            criteriaDates.addLessOrEqualThan(SisSchoolCalendarDate.COL_DATE, m_reportDate);

            BeanQuery queryDates = new BeanQuery(SisSchoolCalendarDate.class, criteriaDates);
            queryDates.addOrderBy(SisSchoolCalendarDate.COL_DATE, true);

            Collection<SisSchoolCalendarDate> dates = getBroker().getCollectionByQuery(queryDates);

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + ModelProperty.PATH_DELIMITER +
                    ScheduleTerm.COL_SCHEDULE_OID, section.getScheduleOid());

            BeanQuery query = new BeanQuery(ScheduleTermDate.class, criteria);
            Map<String, Collection<ScheduleTermDate>> terms = getBroker().getGroupedCollectionByQuery(query,
                    ScheduleTermDate.COL_SCHEDULE_TERM_OID,
                    32);

            for (String termOid : terms.keySet()) {
                TreeSet<SisSchoolCalendarDate> csdSet =
                        new TreeSet<SisSchoolCalendarDate>(getSchoolCalendarDatesComparator());
                for (ScheduleTermDate tmd : terms.get(termOid)) {
                    for (SisSchoolCalendarDate csd : dates) {

                        if (tmd.getStartDate() != null && tmd.getEndDate() != null &&
                                !csd.getDate().before(tmd.getStartDate())
                                && !csd.getDate().after(tmd.getEndDate())) {
                            csdSet.add(csd);
                        }
                    }
                }
                termDates.put(termOid, csdSet);
            }
        }
        return termDates.get(section.getScheduleTermOid());
    }

    /**
     * Get the schedule periods for a particular section on a particular day.
     *
     * @param section MasterSchedule
     * @param csd SisSchoolCalendarDate
     * @return Collection<SchedulePeriod>
     */
    protected Set<SchedulePeriod> getSectionPeriods(MasterSchedule section, SisSchoolCalendarDate csd) {
        initializeSection(section);

        Set<SchedulePeriod> periods = null;
        Collection<ScheduleTerm> terms = m_sectionTermsMap.get(section.getOid());
        if (terms != null) {
            for (ScheduleTerm trm : terms) {
                Collection<ScheduleTermDate> dates = getScheduleTermDates(trm.getOid());
                if (dates != null) {
                    for (ScheduleTermDate tmd : dates) {
                        if (!csd.getDate().before(tmd.getStartDate()) && !csd.getDate().after(tmd.getEndDate())) {
                            ScheduleDay day = getScheduleDay(section.getSchedule(), csd.getScheduleDayNumber());
                            periods = getSectionPeriods(section, tmd, day);
                            break;
                        }
                    }
                }
                if (periods != null) {
                    break;
                }
            }
        }
        return periods;
    }

    /**
     * Gets the section periods.
     *
     * @param section MasterSchedule
     * @param tmd ScheduleTermDate
     * @param day ScheduleDay
     * @return Sets the
     */
    protected Set<SchedulePeriod> getSectionPeriods(MasterSchedule section, ScheduleTermDate tmd, ScheduleDay day) {
        initializeSection(section);
        String key = null;
        Set<SchedulePeriod> periods = null;
        if (section != null && tmd != null && day != null) {
            key = section.getOid() + tmd.getScheduleTermOid() + day.getOid();
        }

        if (!StringUtils.isEmpty(key)) {
            periods = m_sectionTermDayPeriods.get(key);
        }
        return periods;
    }

    /**
     * Gets the para codes.
     *
     * @return Sets the
     */
    private Set<String> getParaCodes() {
        if (m_staffParaCodes == null) {
            m_staffParaCodes = new HashSet();
            X2Criteria staffTypeExclusionCriteria = new X2Criteria();
            staffTypeExclusionCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TABLE_STAFF_TYPE_CODES);
            staffTypeExclusionCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, PARAPRO_STATE_CODE);
            BeanQuery staffTypeExclusionQuery = new BeanQuery(ReferenceCode.class, staffTypeExclusionCriteria);
            Collection<ReferenceCode> staffTypeExclusionCodes =
                    getBroker().getCollectionByQuery(staffTypeExclusionQuery);
            for (ReferenceCode staffType : staffTypeExclusionCodes) {
                m_staffParaCodes.add(staffType.getCode());
            }

        }
        return m_staffParaCodes;
    }

    /**
     * Initialize fields. Translate aliases to java names.
     */
    private void initializeFields() {
        m_contexts = getBroker().getCollectionByQuery(new QueryByCriteria(DistrictSchoolYearContext.class));
        m_gradeCodeToValueMap = new HashMap<String, String>();
        m_programParticipationMap = new HashMap<String, Collection<StudentProgramParticipation>>();
        m_schoolYearStartDate = getCurrentContext().getStartDate();
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        X2Criteria doeAltRefCriteria = new X2Criteria();
        doeAltRefCriteria.addEqualToIgnoreCase(ReferenceTable.COL_USER_NAME, ALT_SLO_REF_NAME);
        QueryByCriteria query = new BeanQuery(ReferenceTable.class, doeAltRefCriteria);
        ReferenceTable doeAltRefTbl = (ReferenceTable) getBroker().getBeanByQuery(query);
        if (doeAltRefTbl != null) {
            m_doeAltSloRefTable = doeAltRefTbl.getOid();
        }

        // Load Input Parameters
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate.before(getCurrentContext().getStartDate())) {
            m_reportDate = getCurrentContext().getStartDate();
        }

        if (m_reportDate.after(getCurrentContext().getEndDate())) {
            m_reportDate = getCurrentContext().getEndDate();
        }

        m_reportingPeriod = (String) getParameter(PARAM_REPORTING_PERIOD);

        if (getParameter(PARAM_DEBUG) != null && getParameter(PARAM_DEBUG) instanceof Boolean) {
            m_debug = ((Boolean) getParameter(PARAM_DEBUG)).booleanValue();
        }

        // Load java paths for aliases
        m_doeAltEd = translateAliasToJavaName(ALIAS_DOE_ALT_ED, true);
        m_doeAlternateSlo = translateAliasToJavaName(ALIAS_DOE_ALT_SLO, true);
        m_doeCreditRecovery = translateAliasToJavaName(ALIAS_DOE_CREDIT_RECOVERY, true);
        m_doeDistrict = translateAliasToJavaName(ALIAS_DOE_DISTRICT, true);
        m_doeEoctCode = translateAliasToJavaName(ALIAS_DOE_EOCT_CODE, true);
        m_doeEsolDelivery = translateAliasToJavaName(ALIAS_DOE_ESOL_DELIV, true);
        m_doeFieldAltSchoolCode = translateAliasToJavaName(ALIAS_DOE_ALT_SKL_CODE, true);
        m_doeFieldAltSystemCode = translateAliasToJavaName(ALIAS_DOE_ALT_SYS_CODE, true);
        m_doeFieldCoTeacher = translateAliasToJavaName(ALIAS_DOE_CO_TEACHER, true);
        m_doeFieldSloCourse = translateAliasToJavaName(ALIAS_DOE_SLO_COURSE, true);
        m_doeFieldSloMeasureType = translateAliasToJavaName(ALIAS_DOE_SLO_MEASURE_TYPE, true);
        m_doeFieldSloPostMeasure = translateAliasToJavaName(ALIAS_DOE_SLO_POST_MEASURE, true);
        m_doeFieldSloPreMeasure = translateAliasToJavaName(ALIAS_DOE_SLO_PRE_MEASURE, true);
        m_doeFieldSsn = translateAliasToJavaName(ALIAS_DOE_SSN, true);
        m_doeGiftedDeliveryCourse = translateAliasToJavaName(ALIAS_DOE_GIFTED_DELIV_CRS, true);
        m_doeOnlineCourse = translateAliasToJavaName(ALIAS_DOE_ONLINE_COURSE, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(ALIAS_DOE_OVERRIDE_SCHOOL, true);
        m_schoolCodeField = translateAliasToJavaName(ALIAS_DOE_SCHOOL, true);
        m_doeMstInLieuCrs = translateAliasToJavaName(ALIAS_DOE_MST_IN_LIEU, true);
        m_doeGsgContentCompleter = translateAliasToJavaName(ALIAS_DOE_GSG_CONT_COMPL, true);
        m_doeMstImmersionLang = translateAliasToJavaName(ALIAS_DOE_MST_IMMERS_LANG, true);
        m_fieldCrsWblCategory = translateAliasToJavaName(ALIAS_DOE_WBL_CATEGORY_CRS, true);
        m_fieldMstWblCategory = translateAliasToJavaName(ALIAS_DOE_WBL_CATEGORY_MST, true);
        m_fieldStdWblCategory = translateAliasToJavaName(ALIAS_DOE_WBL_CATEGORY_STD, true);
        m_doeMtcClassEntryDate = translateAliasToJavaName(ALIAS_DOE_CLASS_ENTRY_DATE, true);
        m_doeFieldSklPerAtt = translateAliasToJavaName(ALIAS_DOE_SKL_PER_ATT, true);
        m_sccCalcStateCodeField = translateAliasToJavaName(ALIAS_SCC_CALC_STATE_CRS_CODE, true);
        m_sscCalcStateCodeField = translateAliasToJavaName(ALIAS_SSC_CALC_STATE_CRS_CODE, true);
        m_sccDeliveryModelStdField = translateAliasToJavaName(ALIAS_SCC_STD_DELIVERY_MODEL, true);
        m_sscDeliveryModelStdField = translateAliasToJavaName(ALIAS_SSC_STD_DELIVERY_MODEL, true);
        m_sccDeliveryModelStdFieldAdd = translateAliasToJavaName(ALIAS_SCC_STD_DELIVERY_MODEL_ADD, true);
        m_sscDeliveryModelStdFieldAdd = translateAliasToJavaName(ALIAS_SSC_STD_DELIVERY_MODEL_ADD, true);
        m_mstDeliveryModelStdField = translateAliasToJavaName(ALIAS_MST_DELIVERY_MODEL, true);
        m_mstInclusionField = translateAliasToJavaName(ALIAS_MST_INCLUSION_CODE, true);
        m_sccInclusionStdField = translateAliasToJavaName(ALIAS_SCC_INCLUDE_STD, true);
        m_sccInclusionStdCodeField = translateAliasToJavaName(ALIAS_SCC_STD_INCLUSION_CODE, true);
        m_sscInclusionStdCodeField = translateAliasToJavaName(ALIAS_SSC_STD_INCLUSION_CODE, true);
        m_sscInclusionStdField = translateAliasToJavaName(ALIAS_SSC_INCLUDE_STD, true);
        m_fieldTrnOutOfDistrict = translateAliasToJavaName(ALIAS_TRN_OUT_OF_DISTR, true);
        m_doeFieldStdPrimExp = translateAliasToJavaName(ALIAS_STD_PRIM_EXCEPT, true);
        m_fieldPgmType = translateAliasToJavaName(ALIAS_PGM_TYPE, true);
        m_mstAltSysCodeField = translateAliasToJavaName(ALIAS_MST_ALTERNATE_SYS_CODE, true);
        m_mstAltSklCodeField = translateAliasToJavaName(ALIAS_MST_ALTERNATE_SKL_CODE, true);
        m_sccExclude = translateAliasToJavaName(ALIAS_SCC_EXCLUDE, true);
        m_sscExclude = translateAliasToJavaName(ALIAS_SSC_EXCLUDE, true);
    }

    /**
     * Initialize section.
     *
     * @param section MasterSchedule
     */
    private void initializeSection(MasterSchedule section) {
        if (!m_initializedSchedules.contains(section.getSchedule().getOid())) {
            m_initializedSchedules.add(section.getSchedule().getOid());

            // Key is mst, trm, day oids
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(
                    MasterTerm.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER + MasterSchedule.COL_SCHEDULE_OID,
                    section.getSchedule().getOid());

            String[] columns = new String[] {MasterTerm.COL_MASTER_SCHEDULE_OID,
                    MasterTerm.COL_SCHEDULE_TERM_OID,
                    MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                            MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                            ScheduleMatrix.COL_SCHEDULE_DAY_OID,
                    MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                            MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                            ScheduleMatrix.COL_SCHEDULE_PERIOD_OID};

            ReportQueryByCriteria query = new ReportQueryByCriteria(MasterTerm.class, columns, criteria);
            query.addOrderByAscending(MasterTerm.COL_MASTER_SCHEDULE_OID);
            query.addOrderByAscending(MasterTerm.COL_SCHEDULE_TERM_OID);
            query.addOrderByAscending(MasterTerm.REL_MASTER_MATRICES + ModelProperty.PATH_DELIMITER +
                    MasterScheduleMatrix.REL_SCHEDULE_MATRIX + ModelProperty.PATH_DELIMITER +
                    ScheduleMatrix.COL_SCHEDULE_DAY_OID);

            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);

            try {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    String mstOid = (String) row[0];
                    String trmOid = (String) row[1];
                    String dayOid = (String) row[2];
                    String perOid = (String) row[3];
                    String key = mstOid + trmOid + dayOid;

                    Collection<ScheduleTerm> terms = m_sectionTermsMap.get(mstOid);
                    if (terms == null) {
                        terms = new HashSet();
                        m_sectionTermsMap.put(mstOid, terms);
                    }
                    ScheduleTerm term = getScheduleTerm(trmOid);
                    if (term != null) {
                        terms.add(term);
                    }
                    Set<SchedulePeriod> periods = m_sectionTermDayPeriods.get(key);
                    if (periods == null) {
                        periods = new HashSet<SchedulePeriod>();
                        m_sectionTermDayPeriods.put(key, periods);
                    }
                    periods.add(getSchedulePeriod(perOid));
                }
            } finally {
                iterator.close();
            }
        }
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
    }

    /**
     * Load in sections by period.
     */
    private void loadSectionsByPeriod() {
        Schedule activeSchedule = null;
        String activeScheduleOid = null;
        ScheduleManager scheduleManager = new ScheduleManager(getBroker());
        Collection mastersOnPeriod = new ArrayList<String>();

        if (isSchoolContext()) {
            SisSchool school = (SisSchool) getSchool();
            activeSchedule = school.getActiveSchedule();
            if (activeSchedule != null) {
                activeScheduleOid = activeSchedule.getOid();
                Collection<SchedulePeriod> periods = activeSchedule.getSchedulePeriods();
                for (SchedulePeriod period : periods) {
                    String periodNum = String.valueOf(period.getNumber());
                    String periodOid = period.getOid();
                    mastersOnPeriod =
                            scheduleManager.getMasterOidsForPeriod(MasterSchedule.class, periodOid, null,
                                    activeScheduleOid);
                    m_periodMapBySchedule.put(activeScheduleOid + periodNum, mastersOnPeriod);
                }
            }
        } else {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            schoolCriteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
            BeanQuery schoolQuery = new BeanQuery(SisSchool.class, schoolCriteria);
            Collection<SisSchool> schools = getBroker().getCollectionByQuery(schoolQuery);

            for (SisSchool currentSchool : schools) {
                activeSchedule = currentSchool.getActiveSchedule();
                if (activeSchedule != null) {
                    activeScheduleOid = activeSchedule.getOid();
                    Collection<SchedulePeriod> periods = activeSchedule.getSchedulePeriods();
                    for (SchedulePeriod period : periods) {
                        String periodNum = String.valueOf(period.getNumber());
                        String periodOid = period.getOid();
                        mastersOnPeriod =
                                scheduleManager.getMasterOidsForPeriod(MasterSchedule.class, periodOid, null,
                                        activeScheduleOid);
                        m_periodMapBySchedule.put(activeScheduleOid + periodNum, mastersOnPeriod);
                    }
                }
            }
        }
    }

    /**
     * Load in the SLO Grade scale and the grade values for usage in retrievers.
     */
    private void loadSloGradeScale() {
        X2Criteria scaleCriteria = new X2Criteria();
        scaleCriteria.addEqualTo(GradeScale.COL_GRADE_SCALE_NAME, SLO_GRADE_SCALE_NAME);
        QueryByCriteria query = new BeanQuery(GradeScale.class, scaleCriteria);
        GradeScale scale = (GradeScale) getBroker().getBeanByQuery(query);
        Collection<GradeScaleGradeDefinition> grades = scale.getGradeScaleDefinitions();
        for (GradeScaleGradeDefinition grade : grades) {
            m_gradeCodeToValueMap.put(grade.getGradeCode(), grade.getGradeValue().toString());
        }
    }

    /**
     * Load in all the Student Program Participation records.
     */
    private void loadStudentProgramParticipationMap() {
        X2Criteria criteria = new X2Criteria();
        criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getCurrentContext().getEndDate());
        criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        QueryByCriteria query =
                m_helper.getStudentSelectionQuery(StudentProgramParticipation.class, criteria,
                        StudentProgramParticipation.COL_STUDENT_OID);

        m_programParticipationMap =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 50);
    }

    /**
     * Load in all the Schedule Term Codes.
     */
    private void loadTermCodes() {
        X2Criteria termCodesCriteria = new X2Criteria();
        termCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TABLE_SCHEDULE_TERM_CODES);
        BeanQuery termCodesQuery = new BeanQuery(ReferenceCode.class, termCodesCriteria);
        Collection<ReferenceCode> termCodes = getBroker().getCollectionByQuery(termCodesQuery);

        for (ReferenceCode referenceCode : termCodes) {
            m_termCodesMap.put(referenceCode.getOid(), referenceCode.getCode());
        }
    }
}
