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

package com.x2dev.procedures.statereporting.wa;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
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
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Export procedure for WA Student Grade History.
 *
 * @author X2 Development Corporation
 */
public class WAStudentGradeHistory extends StateReportData {
    /**
     * Entity class for WA Student Grade History.
     *
     */
    public static class WAStudentGradeHistoryEntity extends StateReportEntity {
        /**
         * List of the reportable transcript records for the current student.
         */
        private List<Transcript> m_transcripts;

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
                    name += transcript.getMasterSchedule().getCourseView();
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
            WAStudentGradeHistory sghData = (WAStudentGradeHistory) data;

            // If the student or course are missing, skip this transcript record.
            SisStudent student = (SisStudent) getBean();
            // Skip the program if the student is not in the District Student file.
            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, sghData);
            if (districtSpans.size() == 0) {
                setRowCount(0);
            } else {
                m_transcripts = (List<Transcript>) sghData.m_transcriptMap.get(student.getOid());
                if (m_transcripts != null) {
                    Iterator<Transcript> iterator = m_transcripts.iterator();
                    while (iterator.hasNext()) {
                        Transcript transcript = iterator.next();

                        if (sghData.includeSchool(transcript.getSchoolOid())) {
                            // Check the exclude transcript indicator.
                            if (!StringUtils.isEmpty(sghData.m_fieldExcludeTrn)) {
                                Object exclude = transcript.getFieldValueByBeanPath(sghData.m_fieldExcludeTrn);
                                if ((exclude instanceof Boolean && ((Boolean) exclude).booleanValue()) ||
                                        (exclude instanceof String && BooleanAsStringConverter.TRUE.equals(exclude))) {
                                    iterator.remove();
                                    continue;
                                }
                            }

                            // Only report transcript records for credit.
                            if (transcript.getSchoolCourse() != null) {
                                Course course = transcript.getSchoolCourse().getCourse();
                                BigDecimal credit = course.getCredit();
                                String creditOverride = transcript.getPotentialCredit();
                                if (course.getHideTranscriptIndicator() ||
                                        (credit == null || credit.floatValue() == 0) &&
                                                StringUtils.isEmpty(creditOverride)) {
                                    iterator.remove();
                                    continue;
                                }
                            }
                        } else {
                            iterator.remove();
                            continue;
                        }
                    }

                    setRowCount(m_transcripts.size());
                } else {
                    setRowCount(0);
                }
            }
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. in district
         * transfer.
         *
         * @param student Student
         * @param sdData WAStudentGradeHistory
         * @return List<DistrictEnrollmentSpan>
         */
        protected List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentGradeHistory sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans =
                    sdData.m_historyHelper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans.
                 * A no-show span represents an enrollment where the student never showed up.
                 * It is identified by a withdrawal code that has NS in the local code of the
                 * reference table.
                 */
                boolean doNotShow = false;
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode, withdrawalCode,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        doNotShow = true;
                    }
                }

                if (!doNotShow) {
                    // Check the span for entry type (internal or external)
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(stateCode) || "2".equals(stateCode)) {
                            if (currentSpan != null &&
                                    (currentSpan.m_exitDate == null ||
                                            !sdData.getOrganization().getCurrentContext().getStartDate()
                                                    .after(currentSpan.m_exitDate))) {
                                districtSpans.add(currentSpan);
                            }
                            currentSpan = null;

                            currentSpan = new DistrictEnrollmentSpan();
                            currentSpan.m_entryEnrollment = enrollment;
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        } else {
                            if (currentSpan == null) {
                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                            }
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        }
                    }
                }
            }
            if (currentSpan != null &&
                    (currentSpan.m_exitDate == null ||
                            !sdData.getOrganization().getCurrentContext().getStartDate()
                                    .after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
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
     * Returns the course information from the Equivalent School Course if available.
     * School Course is used otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourse implements FieldRetriever {
        private final String PARAM_ALE_FUNDED = "ALE_FUNDED";
        private final String PARAM_APIB_CODE = "APIB_CODE";
        private final String PARAM_CIP_CODE = "CIP_CODE";
        private final String PARAM_CONTENT_AREA = "CONTENT_AREA";
        private final String PARAM_COURSE_ID = "COURSE_ID";
        private final String PARAM_COURSE_TITLE = "COURSE_TITLE";
        private final String PARAM_CREDITS_ATTEMPTED = "CREDITS_ATTEMPTED";
        private final String PARAM_CREDITS_EARNED = "CREDITS_EARNED";
        private final String PARAM_DESIGNATION_CODE = "DESIGNATION_CODE";
        private final String PARAM_GRADE_LEVEL = "GRADE_LEVEL";
        private final String PARAM_SCHOOL = "SCHOOL";
        private final String PARAM_STATE_COURSE_CODE = "STATE_COURSE_CODE";
        private final String PARAM_YEAR = "YEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentGradeHistoryEntity sghEntity = (WAStudentGradeHistoryEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            String parameter = (String) field.getParameter();
            Object value = null;
            Course course = null;

            SchoolCourse schoolCourse = transcript.getSchoolCourse();
            if (schoolCourse != null) {
                course = schoolCourse.getCourse();
            } else {
                return null;
            }

            if (PARAM_APIB_CODE.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldAPIBCode);

                if (m_apIbCodesMap.get(code) != null) {
                    value = m_apIbCodesMap.get(code).getStateCode();
                }
            } else if (PARAM_CIP_CODE.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldCIPCode);
                value = lookupIfExists(Course.class, m_fieldCIPCode, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_CONTENT_AREA.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldContentArea);
                value = lookupIfExists(Course.class, m_fieldContentArea, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_CREDITS_ATTEMPTED.equals(parameter)) {
                String credit = transcript.getPotentialCredit();
                Object courseCredit = course.getCredit();

                if (credit != null) {
                    BigDecimal creditAsNumber = null;
                    try {
                        creditAsNumber = new BigDecimal(credit);
                    } catch (NumberFormatException nfe) {
                        // nothing. The credit is not numeric.
                    }
                    value = creditAsNumber;
                } else if (courseCredit != null) {
                    value = courseCredit;
                } else {
                    value = schoolCourse.getCourse().getCredit();
                }
            } else if (PARAM_CREDITS_EARNED.equals(parameter)) {
                value = transcript.getTotalCredit();
            } else if (PARAM_DESIGNATION_CODE.equals(parameter)) {
                String designationCode;
                if (BooleanAsStringConverter.TRUE
                        .equals(transcript.getFieldValueByBeanPath(m_fieldCollegeDesignationCode))) {
                    designationCode = "C";
                } else {
                    designationCode = (String) course.getFieldValueByBeanPath(m_fieldDesignationCode);
                    designationCode = lookupIfExists(Course.class, m_fieldDesignationCode, designationCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
                value = designationCode;
            } else if (PARAM_GRADE_LEVEL.equals(parameter)) {
                String gradeLevel = transcript.getGradeLevel();
                value = data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_SCHOOL.equals(parameter)) {
                Object stateCode = transcript.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
                Object transferCourse = course.getFieldValueByBeanPath(m_fieldTransferCourse);
                if (BooleanAsStringConverter.TRUE.equals(transferCourse)) {
                    value = CODE_OUT_OF_DISTRICT;
                } else {
                    value = stateCode;
                }
            } else if (PARAM_STATE_COURSE_CODE.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldStateCourseCode);
                value = lookupIfExists(Course.class, m_fieldStateCourseCode, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_COURSE_ID.equals(parameter)) {
                value = course.getNumber();
            } else if (PARAM_ALE_FUNDED.equals(parameter)) {
                if (transcript != null) {
                    MasterSchedule tempMasterSchedule = transcript.getMasterSchedule();
                    value = tempMasterSchedule != null ? tempMasterSchedule.getFieldValueByAlias(ALIAS_ALE_FUNDED)
                            : null;
                }

                if (StringUtils.isEmpty((String) value)) {
                    value = "N";
                }
            } else if (PARAM_COURSE_TITLE.equals(parameter)) {
                if (transcript.getUserDescriptionIndicator()) {
                    value = transcript.getCourseDescription();
                } else {
                    value = course.getDescription();
                }
            } else if (PARAM_YEAR.equals(parameter)) {
                value = Integer.valueOf(transcript.getDistrictContext().getSchoolYear());
            }

            return value;
        }
    }

    /**
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveLetterGrade implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentGradeHistoryEntity sghEntity = (WAStudentGradeHistoryEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            Object value = null;

            /*
             * Check ungraded course.
             */
            GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
            String finalGrade = transcript.getFinalGrade();

            // See if the grade in the final grade column is a grade scale value (letter grade).
            if (!StringUtils.isEmpty(finalGrade)) {
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
                if (value == null) {
                    value = finalGrade;
                }
            }

            if (scale != null && !StringUtils.isEmpty((String) value)) {
                String grade = value.toString();
                String stateVal = null;
                Collection<GradeScaleGradeDefinition> definitions = getDefinitions(scale);
                if (definitions != null) {
                    for (GradeScaleGradeDefinition definition : definitions) {
                        if (grade.equals(definition.getGradeCode())) {
                            stateVal = (String) definition.getFieldValueByBeanPath(m_fieldGradeDefinitionState);
                        }
                    }
                    if (stateVal != null) {
                        value = stateVal;
                    }
                }
            }

            return value;
        }
    }

    /**
     * Map Lookup of gradeScale definitions.
     *
     * @param scale GradeScale
     * @return Collection
     */
    protected Collection<GradeScaleGradeDefinition> getDefinitions(GradeScale scale) {
        if (!m_gradeScalesDefinitons.containsKey(scale)) {
            m_gradeScalesDefinitons.put(scale, scale.getGradeScaleDefinitions());
        }
        return m_gradeScalesDefinitons.get(scale);
    }

    /**
     * Returns the term information from the Master Schedule if available.
     * The Transcript record is used otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTerm implements FieldRetriever {
        private final String PARAM_END_DATE = "END_DATE";
        private final String PARAM_TERM_CODE = "TERM_CODE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentGradeHistoryEntity sghEntity = (WAStudentGradeHistoryEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            String parameter = (String) field.getParameter();
            String termCode = getTranscriptTermCode(transcript);

            Object value = null;

            if (PARAM_END_DATE.equals(parameter)) {
                // See if override term date field exists and has a value.
                if (m_fieldTransTermEndDate != null) {
                    value = getPropertyAsJavaType(transcript, m_fieldTransTermEndDate);
                }

                if (value == null) {
                    value = m_termEndDateHelper.getEndDateSchool(transcript.getDistrictContextOid(),
                            transcript.getSchoolOid(), termCode);
                }
            } else if (PARAM_TERM_CODE.equals(parameter)) {
                value = lookupIfExists(Transcript.class, Transcript.COL_TERM_CODE, termCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * Retrieve a value from the transcript record by alias.
     */
    protected class RetrieveTranscriptValue implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentGradeHistoryEntity sghEntity = (WAStudentGradeHistoryEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            String parameter = (String) field.getParameter();
            Object value = transcript.getFieldValueByAlias(parameter);
            return value;
        }
    }

    /**
     * The Term End Date Helper that helps get the final grading period for the course or courses
     * completed
     * <ul>
     * <li>getEndDateSchedule()</li>
     * <li>getEndDateSchool()</li>
     * </ul>
     * .
     *
     * @author X2 Development Corporation
     */
    protected class TermEndDateHelper {

        /**
         * Return the term end date for a term code in a school.
         *
         * @param districtContextOid String
         * @param schoolOid String
         * @param termCode String
         * @return PlainDate
         */
        PlainDate getEndDateSchool(String districtContextOid, String schoolOid, String termCode) {
            PlainDate endDate = null;
            X2Criteria criteria = new X2Criteria();
            if (!StringUtils.isEmpty(termCode) && !StringUtils.isEmpty(schoolOid)
                    && !StringUtils.isEmpty(districtContextOid)) {
                criteria.addEqualTo(GradeTermDate.REL_GRADE_TERM + PATH_DELIMITER + GradeTerm.COL_GRADE_TERM_ID,
                        termCode);
                criteria.addEqualTo(GradeTermDate.COL_SCHOOL_OID, schoolOid);
                criteria.addEqualTo(GradeTermDate.COL_DISTRICT_CONTEXT_OID, districtContextOid);
                QueryByCriteria query = new QueryByCriteria(GradeTermDate.class, criteria);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        GradeTermDate gradeTermDate = (GradeTermDate) iterator.next();
                        if (endDate == null || gradeTermDate.getEndDate().after(endDate)) {
                            endDate = gradeTermDate.getEndDate();
                        }
                    }
                } finally {
                    iterator.close();
                }
            }

            // If no school/term is found, fall back to school year end date.
            if (endDate == null) {
                criteria = new X2Criteria();
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                        SchoolCalendar.COL_DISTRICT_CONTEXT_OID, districtContextOid);
                criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                        SchoolCalendar.COL_SCHOOL_OID, schoolOid);
                criteria.addEqualTo(SchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
                ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(SchoolCalendarDate.class,
                        new String[] {"MAX(" + SchoolCalendarDate.COL_DATE + ")"},
                        criteria);
                ReportQueryIterator reportIterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
                try {
                    while (reportIterator.hasNext()) {
                        // Data was coming back as a timestamp and causing a class cast exception
                        // This is a difference in database formats mysql vs sqlserver
                        Object[] results = (Object[]) reportIterator.next();
                        if (results[0] instanceof java.sql.Date) {
                            java.sql.Date sqlDate = (java.sql.Date) results[0];
                            if (sqlDate != null) {
                                endDate = new PlainDate(sqlDate);
                            }
                        } else if (results[0] instanceof java.sql.Timestamp) {
                            java.sql.Timestamp sqlDate = (java.sql.Timestamp) results[0];
                            if (sqlDate != null) {
                                endDate = new PlainDate(sqlDate);
                            }
                        }
                    }
                } finally {
                    reportIterator.close();
                }
            }
            return endDate;
        }
    }


    /**
     * Validate CourseDesignationCod.
     */
    protected class ValidateCourseDesignCode implements FieldValidator {

        private static final String VAL_ID = "VAL-ALE-FUNDED";

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

            if (!StringUtils.isEmpty(value) && value.equals("O")) {
                String designCode = entity.getFieldValue(FIELD_COURSE_DESIGNATION_COD);
                if (designCode == null || !designCode.contains("O")) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If " + field.getFieldId() + " contains a valid value of O -  "
                                    + FIELD_COURSE_DESIGNATION_COD +
                                    " must contain a valid value of O ",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_COURSE_DESIGNATION_COD + " = " + STYLE_BOLD + designCode + STYLE_END));
                }


            }

            return errors;
        }
    }


    /**
     * Validate CTEAssesment.
     */
    protected class ValidateAssessnentVal implements FieldValidator {
        private static final String CODE_Y = "Y";
        private static final String FIELD_HAS_INDUSTRY_CERTIF = "HasIndustryCertif";
        private static final String VAL_ID = "CTE-ASSESMENT-VAL";

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
            String cipCode = entity.getFieldValue(FIELD_CIP_CODE);

            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(cipCode)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " data is required if " + FIELD_CIP_CODE + " is intered.",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_CIP_CODE + " = " + STYLE_BOLD + cipCode + STYLE_END));
            }
            String hasIndustry = entity.getFieldValue(FIELD_HAS_INDUSTRY_CERTIF);
            if (!StringUtils.isEmpty(hasIndustry) && hasIndustry.equals(CODE_Y) &&
                    ((StringUtils.isEmpty(value)) || !value.equals("2"))) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be 2 if " + FIELD_HAS_INDUSTRY_CERTIF + " is " + CODE_Y,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_HAS_INDUSTRY_CERTIF + " = " + STYLE_BOLD + hasIndustry + STYLE_END));
            }


            return errors;
        }
    }

    /**
     * Validate IsTechPrepCompleter.
     */
    protected class ValidateTechPrepCompl implements FieldValidator {
        private static final String CODE_N = "N";
        private static final String CODE_T = "T";
        private static final String FIELD_SCHOOL_CODE = "School Code";
        private static final String VAL_ID = "TECHPREPCOMPL-VAL";

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
            String desgnCodes = entity.getFieldValue(FIELD_COURSE_DESIGNATION_COD);
            String schoolCode = entity.getFieldValue(FIELD_SCHOOL_CODE);
            if (StringUtils.isEmpty(value)
                    && !StringUtils.isEmpty(desgnCodes) && desgnCodes.contains(CODE_T)
                    && !StringUtils.isEmpty(schoolCode) && !schoolCode.equals("9999")) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " data is required  if " + FIELD_COURSE_DESIGNATION_COD + " contain "
                                + CODE_T +
                                " and " + FIELD_SCHOOL_CODE + " is not transfer",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_CIP_CODE + " = " + STYLE_BOLD + desgnCodes + STYLE_END +
                                "; " + FIELD_SCHOOL_CODE + " = " + STYLE_BOLD + schoolCode + STYLE_END));
            }

            if ((StringUtils.isEmpty(value) || !value.equals(CODE_N))
                    && !StringUtils.isEmpty(desgnCodes) && !desgnCodes.contains(CODE_T)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be " + CODE_N + " if " + FIELD_COURSE_DESIGNATION_COD
                                + " doesn't contain " +
                                CODE_T,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_CIP_CODE + " = " + STYLE_BOLD + desgnCodes + STYLE_END));
            }


            return errors;
        }
    }

    /**
     * Validate ApIb depend.
     */
    protected class ValidateCIPDepend implements FieldValidator {
        private static final String FIELD_SCHOOL_CODE = "School Code";
        private static final String VAL_ID = "CIP-DEPEND-VAL";

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

            String cipCode = entity.getFieldValue(FIELD_CIP_CODE);
            String schoolCode = entity.getFieldValue(FIELD_SCHOOL_CODE);
            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(cipCode)
                    && !StringUtils.isEmpty(schoolCode) && !schoolCode.equals("9999")) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " data is required  if " + FIELD_CIP_CODE + " is not empty and " +
                                FIELD_SCHOOL_CODE + " is not transfer",
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_CIP_CODE + " = " + STYLE_BOLD + cipCode + STYLE_END +
                                "; " + FIELD_SCHOOL_CODE + " = " + STYLE_BOLD + schoolCode + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate ApIbCode.
     */
    protected class ValidateApIbCode implements FieldValidator {
        private static final String CODE_A = "A";
        private static final String CODE_I = "I";

        private static final String VAL_ID = "APIB-COURSE-CODE-VAL";

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
            String designationCodes = entity.getFieldValue(FIELD_COURSE_DESIGNATION_COD);

            if (StringUtils.isEmpty(value) && !StringUtils.isEmpty(designationCodes)
                    && (designationCodes.contains(CODE_I) || designationCodes.contains(CODE_A))) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + "   must be a valid value if " + FIELD_COURSE_DESIGNATION_COD + " is " +
                                CODE_A + " or " + CODE_I,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                "; " + FIELD_COURSE_DESIGNATION_COD + " = " + STYLE_BOLD + designationCodes
                                + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate CourseDesignationCod.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateContenAreaCode implements FieldValidator {
        private static final String VAL_ID = "CONTENTAREACODE-VAL";
        private List<String> m_validValues = new ArrayList<String>(Arrays.asList("1", "2", "3", "4", "5", "6", "7", "8",
                "9", "10", "11", "12", "13", "14", "107", "108", "109", "110", "111", "112", "113", "114", "115", "116",
                "117", "118", "119", "120", "121", "122", "ZZZ", "123"));

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

            if (!StringUtils.isEmpty(value)) {
                if (!m_validValues.contains(String.valueOf(value))) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must contain  valid values: " + m_validValues.toString(),
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Validate CourseDesignationCod.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateCourseDesignationCod implements FieldValidator {
        private static final String VAL_ID = "DESIGNATION-CODE-VAL";
        private List<String> m_validValues = new ArrayList<String>(
                Arrays.asList("I", "C", "T", "H", "A", "R", "B", "K", "O", "L", "N", "Q", "S", "Z"));
        private List<String> m_nonInstractionalWrongNeighbor =
                new ArrayList<String>(Arrays.asList("A", "K", "C", "I", "R", "S", "T"));
        private List<String> m_runningStartWrongNeighbor =
                new ArrayList<String>(Arrays.asList("I", "C", "T", "A", "K", "L", "N"));

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

            boolean invalidValue = false;
            if (!StringUtils.isEmpty(value)) {
                for (char validValue : value.toCharArray()) {
                    if (!m_validValues.contains(String.valueOf(validValue))) {
                        invalidValue = true;
                        break;
                    }
                }
            }

            if (!StringUtils.isEmpty(value) && !invalidValue) {

                if (value.contains("R")) {
                    for (String wrong : m_runningStartWrongNeighbor) {
                        if (value.contains(wrong)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Course Designation " + STYLE_BOLD + "R" + STYLE_END
                                            + " may not also be reported as " + m_runningStartWrongNeighbor.toString(),
                                    "Course Designation=" + STYLE_BOLD + value + STYLE_END
                                            + ", may not also be reported as=" + STYLE_BOLD + wrong + STYLE_END));
                        }
                    }
                }

                if (value.contains("Z")) {
                    for (String wrong : m_nonInstractionalWrongNeighbor) {
                        if (value.contains(wrong)) {
                            errors.add(new StateReportValidationError(entity, field,
                                    "Course Designation " + STYLE_BOLD + "Z" + STYLE_END
                                            + " may not also be reported as "
                                            + m_nonInstractionalWrongNeighbor.toString(),
                                    "Course Designation=" + STYLE_BOLD + value + STYLE_END
                                            + ", may not also be reported as=" + STYLE_BOLD + wrong + STYLE_END));
                        }
                    }
                }


            }
            if (invalidValue) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + "  must contain  valid values: " + m_validValues.toString(),
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }



            return errors;
        }
    }

    /**
     * Validate CreditsAttempted.
     */
    protected class ValidateCredit implements FieldValidator {
        private static final String FIELD_CREDIT_EARNED = "Credit Earned";
        private static final String VAL_ID = "CREDITS-A-VAL";

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
            String creditEarned = entity.getFieldValue(FIELD_CREDIT_EARNED);

            if (!StringUtils.isEmpty(creditEarned) && !StringUtils.isEmpty(value)
                    && StringUtils.isNumeric(creditEarned) && StringUtils.isNumeric(value)) {

                Double creditEarnedD = getDouble(creditEarned);
                Double creditAttemted = getDouble(value);
                if (creditEarnedD != null && creditAttemted != null
                        && creditAttemted.doubleValue() < creditEarnedD.doubleValue()) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must be equal to or greater than the " + FIELD_CREDIT_EARNED,
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; " + FIELD_CREDIT_EARNED + " = " + STYLE_BOLD + creditEarned + STYLE_END));
                }
                if (!(creditAttemted.doubleValue() >= 0 && creditAttemted.doubleValue() <= 99.99)) {
                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + "  must be from 0.00 to 99.99 ",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
                }

                if (!(creditEarnedD.doubleValue() >= 0 && creditEarnedD.doubleValue() <= 99.99)) {
                    errors.add(new StateReportValidationError(entity, field,
                            FIELD_CREDIT_EARNED + "  must be from 0.00 to 99.99 ",
                            FIELD_CREDIT_EARNED + " = " + STYLE_BOLD + creditEarnedD + STYLE_END));
                }
            }
            return errors;
        }
    }

    /**
     * Constants: Aliases
     */
    protected static final String ALIAS_APIB_CODE = "DOE AP IB CODE";
    protected static final String ALIAS_CIP_CODE = "DOE CIP CODE";
    protected static final String ALIAS_CONTENT_AREA = "DOE CONTENT AREA";
    protected static final String ALIAS_DESIGNATION_CODE = "DOE CRS DESIGNATION";
    protected static final String ALIAS_COLLEGE_DESIGNATION_CODE = "DOE COLLEGE DESIGNATION";
    protected static final String ALIAS_EXCLUDE_GRDHST = "DOE EXCLUDE GRDHST";
    protected static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_TRN = "DOE EXCLUDE TRN";
    protected static final String ALIAS_STATE_COURSE_CODE = "DOE STATE COURSE CODE";

    protected static final String ALIAS_ALE_FUNDED = "ALE FUNDED";
    protected static final String ALIAS_CERT_NUM = "DOE CERTIFICATION NUM";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_GRADE_STATE_CODE = "DOE GRADE STATE CODE";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_STD_ASSESSMENT_PASSER = "DOE STD ASSESSMENT PASSER";
    protected static final String ALIAS_STD_CTE_COMPLETER = "DOE STD CTE COMPLETER";
    protected static final String ALIAS_STD_STD_CERTIFICATION = "DOE STD CERTIFICATION";
    protected static final String ALIAS_STD_TECH_COMPLETER = "DOE STD TECH COMPLETER";
    protected static final String ALIAS_TRANSFER_COURSE = "DOE TRANSFER COURSE";
    protected static final String ALIAS_TRN_END_DATE = "DOE TRN END DATE";

    protected static final String CODE_NO_SHOW = "NS";
    protected static final String CODE_OUT_OF_DISTRICT = "9999";

    protected static final String PARAM_REQUIRE_FINAL_GRADE = "requireFinalGrade";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";

    /*
     * Export format field names
     */
    protected static final String FIELD_CIP_CODE = "CIPCode";
    protected static final String FIELD_CTE_COURSE = "CTECourseEquiv";
    protected static final String FIELD_COURSE_DESIGNATION_COD = "CourseDesignationCod";

    /**
     * Other constants
     */
    protected static final String VALUE_DEFAULT_CTE_EQ = "C";

    /**
     * Instance variables.
     */
    protected String m_fieldAPIBCode;
    protected String m_fieldCIPCode;
    protected String m_fieldCollegeDesignationCode;
    protected String m_fieldContentArea;
    protected String m_fieldDesignationCode;
    protected String m_fieldExcludeGrdhst;
    protected String m_fieldExcludeTrn;
    protected String m_fieldGradeDefinitionState;
    protected String m_fieldSchoolCode;
    protected String m_fieldStateCourseCode;
    protected String m_fieldTransferCourse;
    protected String m_fieldTransTermEndDate;
    protected String m_aliasSchoolExclude;

    protected Map<String, ReferenceCode> m_apIbCodesMap;
    protected GradesManager m_gradesManager;
    protected Map<String, GradeScale> m_gradeScales;
    protected Map<GradeScale, Collection<GradeScaleGradeDefinition>> m_gradeScalesDefinitons =
            new HashMap<GradeScale, Collection<GradeScaleGradeDefinition>>();
    protected StudentHistoryHelper m_historyHelper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected Boolean m_requireFinalGrade;
    protected TermEndDateHelper m_termEndDateHelper;
    protected Map<String, Collection<Transcript>> m_transcriptMap;

    public Map m_excludeSchool;

    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolOid);
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
    protected void initialize() {
        initializeFields();
        initAllApIbCodes();

        boolean isSchoolExclude = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();
        if (isSchoolExclude) {
            loadSchoolExcludeMap();
        }


        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            m_historyHelper = new StudentHistoryHelper(this);
            m_historyHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            setQuery(m_historyHelper.getStudentQuery(false));

            setEntityClass(WAStudentGradeHistoryEntity.class);

            // load grade scale information.
            loadGradescales();
            loadTranscripts(m_historyHelper.getStudentCriteria());

            m_termEndDateHelper = new TermEndDateHelper();
            m_gradesManager = new GradesManager(getBroker());

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TRN-COURSE", new RetrieveCourse());
            calcs.put("TRN-LETTERGRADE", new RetrieveLetterGrade());
            calcs.put("TRN-TERM", new RetrieveTerm());
            calcs.put("TRN-TRN", new RetrieveTranscriptValue());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateCredit.VAL_ID, new ValidateCredit());
            validators.put(ValidateCourseDesignationCod.VAL_ID, new ValidateCourseDesignationCod());
            validators.put(ValidateContenAreaCode.VAL_ID, new ValidateContenAreaCode());
            validators.put(ValidateApIbCode.VAL_ID, new ValidateApIbCode());
            validators.put(ValidateCIPDepend.VAL_ID, new ValidateCIPDepend());
            validators.put(ValidateAssessnentVal.VAL_ID, new ValidateAssessnentVal());
            validators.put(ValidateCourseDesignCode.VAL_ID, new ValidateCourseDesignCode());
            validators.put(ValidateTechPrepCompl.VAL_ID, new ValidateTechPrepCompl());

            super.addValidators(validators);
        }
    }

    /**
     * Performs reference code translation if the field has a reference table associated with it.
     * If the fields does not have a reference table, return the same value that was passed in.
     *
     * @param beanClass Class
     * @param beanPath String
     * @param value String
     * @param referenceMap int
     * @return String
     */
    protected String lookupIfExists(Class beanClass, String beanPath, String value, int referenceMap) {
        String stateValue = null;
        DataDictionaryField dictionaryField = getDataDictionaryField(beanClass, beanPath);
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            stateValue = lookupReferenceCodeByRefTbl(dictionaryField.getReferenceTableOid(), value, referenceMap);
        } else {
            stateValue = value;
        }
        return stateValue;
    }

    /**
     * Gets the transcript term code.
     *
     * @param transcript Transcript
     * @return term code according to approved logic.
     */
    static String getTranscriptTermCode(Transcript transcript) {
        String termCode = null;
        if (transcript.getMasterSchedule() != null &&
                transcript.getMasterSchedule().getScheduleTerm() != null &&
                !StringUtils.isEmpty(transcript.getMasterSchedule().getScheduleTerm().getCode())) {
            termCode = transcript.getMasterSchedule().getScheduleTerm().getCode();
        } else {
            termCode = transcript.getTermCode();
        }
        if (StringUtils.isEmpty(termCode) && transcript.getMasterSchedule() != null) {
            termCode = transcript.getMasterSchedule().getTermView();
        }
        return termCode;
    }


    /**
     * Gets the double.
     *
     * @param value String
     * @return Double
     */
    Double getDouble(String value) {
        Double returnValue = null;
        try {
            returnValue = Double.valueOf(Double.parseDouble(value));
        } catch (NumberFormatException e) {
            // nothing to do
        }
        return returnValue;
    }

    /**
     * Initialize map of all possible Reference Codes for AP IB keyed on Code.
     */
    private void initAllApIbCodes() {
        if (m_fieldAPIBCode == null) {
            initializeFields();
        }

        DataDictionaryField ddField = getDataDictionaryField(Course.class, m_fieldAPIBCode);

        if (ddField != null && !StringUtils.isEmpty(ddField.getReferenceTableOid())) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, ddField.getReferenceTableOid());

            m_apIbCodesMap = getBroker().getMapByQuery(new QueryByCriteria(ReferenceCode.class, criteria),
                    ReferenceCode.COL_CODE, 512);
        } else {
            m_apIbCodesMap = new HashMap<String, ReferenceCode>();
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_requireFinalGrade = (Boolean) getParameter(PARAM_REQUIRE_FINAL_GRADE);
        if (m_requireFinalGrade == null) {
            m_requireFinalGrade = Boolean.FALSE;
        }

        m_fieldAPIBCode = translateAliasToJavaName(ALIAS_APIB_CODE, true);
        m_fieldCIPCode = translateAliasToJavaName(ALIAS_CIP_CODE, true);
        m_fieldContentArea = translateAliasToJavaName(ALIAS_CONTENT_AREA, true);
        m_fieldDesignationCode = translateAliasToJavaName(ALIAS_DESIGNATION_CODE, true);
        m_fieldCollegeDesignationCode = translateAliasToJavaName(ALIAS_COLLEGE_DESIGNATION_CODE, true);
        m_fieldExcludeTrn = translateAliasToJavaName(ALIAS_EXCLUDE_TRN, false);
        m_fieldExcludeGrdhst = translateAliasToJavaName(ALIAS_EXCLUDE_GRDHST, false);
        m_fieldGradeDefinitionState = translateAliasToJavaName(ALIAS_GRADE_STATE_CODE, true);
        m_fieldStateCourseCode = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
        m_fieldTransTermEndDate = translateAliasToJavaName(ALIAS_TRN_END_DATE, false);
        m_aliasSchoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldTransferCourse = translateAliasToJavaName(ALIAS_TRANSFER_COURSE, true);
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
        transcriptCriteria.addIn(Transcript.COL_STUDENT_OID, studentSubQuery);
        if (!StringUtils.isEmpty(m_fieldExcludeGrdhst)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_SCHOOL_COURSE + PATH_DELIMITER +
                    SchoolCourse.REL_COURSE + PATH_DELIMITER +
                    m_fieldExcludeGrdhst,
                    BooleanAsStringConverter.TRUE);
        }

        if (m_requireFinalGrade.booleanValue()) {
            transcriptCriteria.addNotEmpty(Transcript.COL_FINAL_GRADE, getBroker().getPersistenceKey());
        }

        BeanQuery query = new BeanQuery(Transcript.class, transcriptCriteria);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 1000);
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_aliasSchoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        Map excludedSchools = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
        // To identify and remove the transfer school, as we want to include the transcripts from
        // there.
        for (Object schoolOid : excludedSchools.entrySet()) {
            SisSchool school = (SisSchool) excludedSchools.get(schoolOid);
            if (school != null && !CODE_OUT_OF_DISTRICT.equals(school.getFieldValueByBeanPath(m_fieldSchoolCode))) {
                m_excludeSchool.put(schoolOid, school);
            }
        }
    }
}
