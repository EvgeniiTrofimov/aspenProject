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
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
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
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for WA Student Grade History.
 *
 * @author X2 Development Corporation
 */
public class WAStudentGradeHistoryMukilteo extends StateReportData {
    /**
     * Entity class for WA Student Grade History.
     *
     */
    public static class WAStudentGradeHistoryEntity extends StateReportEntity {
        /**
         * List of the reportable transcript records for the current student.
         */
        private List<Transcript> m_transcripts;

        /*
         * Full year term codes
         */
        protected static final String TERM_CODE_Q0 = "Q0";
        protected static final String TERM_CODE_T0 = "T0";
        private static final String FIELD_LETTER_GRADE = "LetterGrade";
        private static final BigDecimal BIG_DECIMAL_TWO = new BigDecimal(2);
        private static final BigDecimal BIG_DECIMAL_THREE = new BigDecimal(3);
        private static final String[] CODE_LOOKUP_TRIMESTERS = {"T1", "T2", "T3"};
        private static final String[] CODE_LOOKUP_SEMESTERS = {"S1", "S2"};

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
         * Filter expressed letter grade codes or those transcripts without a letter grade.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            FieldDefinition field = getData().getFieldDefinition(FIELD_LETTER_GRADE);
            String value = getFieldValue(FIELD_LETTER_GRADE);

            if (value.isEmpty()) {
                error = new StateReportValidationError(this, field, "Must have a grade to be reported.", "");
            }
            return error;
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
            WAStudentGradeHistoryMukilteo sghData = (WAStudentGradeHistoryMukilteo) data;

            // If the student or course are missing, skip this transcript record.
            SisStudent student = (SisStudent) getBean();

            List<Transcript> evaluateList = (List<Transcript>) sghData.m_transcriptMap.get(student.getOid());
            m_transcripts = new ArrayList<Transcript>();
            if (evaluateList != null) {
                Iterator<Transcript> iterator = evaluateList.iterator();
                while (iterator.hasNext()) {
                    boolean addToList = true;

                    Transcript transcript = iterator.next();

                    if (sghData.includeSchool(transcript.getSchoolOid())) {
                        // Check the exclude transcript indicator.
                        if (!StringUtils.isEmpty(sghData.m_fieldExcludeTrn)) {
                            Object exclude = transcript.getFieldValueByBeanPath(sghData.m_fieldExcludeTrn);
                            if ((exclude instanceof Boolean && ((Boolean) exclude).booleanValue()) ||
                                    (exclude instanceof String && "1".equals(exclude))) {
                                iterator.remove();
                                addToList = false;
                                continue;
                            }
                        }

                        // Only report transcript records for credit.
                        if (transcript.getSchoolCourse() != null) {
                            Course course = transcript.getSchoolCourse().getCourse();
                            BigDecimal credit = course.getCredit();
                            String creditOverride = transcript.getPotentialCredit();
                            if ((credit == null || credit.floatValue() == 0) &&
                                    StringUtils.isEmpty(creditOverride)) {
                                iterator.remove();
                                addToList = false;
                                continue;
                            }
                        }

                        String termCode = "";
                        Transcript newCopyTranscript;
                        termCode = getTranscriptTermCode(transcript);

                        Course course = null;
                        if (transcript.getEquivalentSchoolCourse() != null) {
                            course = transcript.getEquivalentSchoolCourse().getCourse();
                        } else {
                            if (transcript.getSchoolCourse() != null) {
                                course = transcript.getSchoolCourse().getCourse();
                            }
                        }

                        if (TERM_CODE_Q0.equals(termCode)) {
                            for (int a = 0; a < 2; a++) {
                                newCopyTranscript = (Transcript) transcript.copyBean();
                                newCopyTranscript.setTermCode(CODE_LOOKUP_SEMESTERS[a]);
                                BigDecimal creditAsBigDecimal =
                                        getCreditAsBigDecimal(transcript, CODE_LOOKUP_SEMESTERS[a]);
                                if (creditAsBigDecimal == null) {
                                    creditAsBigDecimal = course.getCredit();
                                }
                                String credit = creditAsBigDecimal.divide(BIG_DECIMAL_TWO, RoundingMode.UP).toString();
                                if ("S1".equals(CODE_LOOKUP_SEMESTERS[a])) {
                                    newCopyTranscript.setFieldA009(credit);
                                } else if ("S2".equals(CODE_LOOKUP_SEMESTERS[a])) {
                                    newCopyTranscript.setFieldA012(credit);
                                }
                                newCopyTranscript.setPotentialCredit(credit);
                                m_transcripts.add(newCopyTranscript);
                            }
                        } else if (TERM_CODE_T0.equals(termCode)) {
                            for (int a = 0; a < 3; a++) {
                                newCopyTranscript = (Transcript) transcript.copyBean();
                                newCopyTranscript.setTermCode(CODE_LOOKUP_TRIMESTERS[a]);
                                BigDecimal creditAsBigDecimal =
                                        getCreditAsBigDecimal(transcript, CODE_LOOKUP_TRIMESTERS[a]);
                                if (creditAsBigDecimal == null) {
                                    creditAsBigDecimal = course.getCredit();
                                }
                                String credit =
                                        creditAsBigDecimal.divide(BIG_DECIMAL_THREE, RoundingMode.UP).toString();
                                if ("T1".equals(CODE_LOOKUP_TRIMESTERS[a])) {
                                    newCopyTranscript.setFieldA007(credit);
                                } else if ("T2".equals(CODE_LOOKUP_TRIMESTERS[a])) {
                                    newCopyTranscript.setFieldA008(credit);
                                } else if ("T3".equals(CODE_LOOKUP_TRIMESTERS[a])) {
                                    newCopyTranscript.setFieldA009(credit);
                                }
                                newCopyTranscript.setPotentialCredit(credit);
                                m_transcripts.add(newCopyTranscript);
                            }
                        } else {
                            if (addToList) {
                                BigDecimal creditAsBigDecimal = getCreditAsBigDecimal(transcript, termCode);
                                if (creditAsBigDecimal == null) {
                                    creditAsBigDecimal = course.getCredit();
                                }
                                String credit = creditAsBigDecimal.toString();
                                if ("T1".equals(termCode)) {
                                    transcript.setFieldA007(credit);
                                } else if ("T2".equals(termCode)) {
                                    transcript.setFieldA008(credit);
                                } else if ("T3".equals(termCode) || "S1".equals(termCode)) {
                                    transcript.setFieldA009(credit);
                                } else if ("S2".equals(termCode)) {
                                    transcript.setFieldA012(credit);
                                }
                                transcript.setPotentialCredit(credit);
                                m_transcripts.add(transcript);
                            }
                        }
                    }
                }

                setRowCount(m_transcripts.size());
            } else {
                setRowCount(0);
            }
        }

        /**
         * Uses the getCredit method to get our credit value and then convert it into a BigDecimal
         * Overloaded to be able to be sent transcript object and get the appropriate credit value.
         *
         * @author X2 Development Corporation
         * @param transcript Transcript
         * @param codeLookup String
         * @return Big decimal
         */
        private BigDecimal getCreditAsBigDecimal(Transcript transcript, String codeLookup) {
            String credit = getCredit(transcript, codeLookup);
            return getCreditAsBigDecimal(transcript.getPotentialCredit(), credit);
        }

        /**
         * Uses the getCredit method to get our credit value and then convert it into a BigDecimal
         * Overloaded to be able to be sent two strings, override is taken first if it exists.
         *
         * @author X2 Development Corporation
         * @param credit String
         * @param override String
         * @return Big decimal
         */
        private BigDecimal getCreditAsBigDecimal(String credit, String override) {
            BigDecimal creditAsNumber = null;
            try {
                if (override != null) {
                    creditAsNumber = new BigDecimal(override);
                } else {
                    if (credit != null) {
                        creditAsNumber = new BigDecimal(credit);
                    }
                }
            } catch (NumberFormatException nfe) {
                // nothing. The credit is not numeric.
            }
            return creditAsNumber;
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
     * Returns the course information from the Equivalent School Course if available.
     * School Course is used otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveCourse implements FieldRetriever {
        private final String ALIAS_COURSE_CODE_OVERRIDE = "Override CrsDsg";
        private final String ALIAS_COURSE_NUMBER_OVERRIDE = "Override CrsNum";
        private final String FIELD_CREDITS_ATTEMPTED = "CreditsAttempted";
        private final String FIELD_LETTER_GRADE = "LetterGrade";
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
        private final String PARAM_SCHOOL_CODE = "SCHOOL_CODE";
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
            WAStudentGradeHistoryMukilteo sghData = (WAStudentGradeHistoryMukilteo) data;
            String parameter = (String) field.getParameter();
            SisStudent student = (SisStudent) sghEntity.getBean();
            Object value = null;
            Course course = null;
            if (transcript.getSchoolCourse() != null) {
                course = transcript.getSchoolCourse().getCourse();
            } else {
                return null;
            }

            if (PARAM_APIB_CODE.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldAPIBCode);
                value = lookupIfExists(Course.class, m_fieldAPIBCode, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_CIP_CODE.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldCIPCode);
                value = lookupIfExists(Course.class, m_fieldCIPCode, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_CONTENT_AREA.equals(parameter)) {
                String code = (String) course.getFieldValueByBeanPath(m_fieldContentArea);
                value = lookupIfExists(Course.class, m_fieldContentArea, code,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_SCHOOL_CODE.equals(parameter)) {
                StudentEnrollment enrollment = null;
                List<StudentEnrollmentSpan> spans = sghData.m_historyHelper.getStudentEnrollmentSpans(student, true);
                for (StudentEnrollmentSpan span : spans) {
                    enrollment = span.getFirstActiveEnrollment();
                }
                if (null != enrollment && enrollment.getSchoolOid().equals(transcript.getSchoolOid())) {
                    value = enrollment.getFieldValueByBeanPath(m_fieldLocationOveride);
                }
                if (value == null || StringUtils.isEmpty(value.toString())) {
                    String schoolId = transcript.getSchool().getSchoolId();
                    Object stateCode = transcript.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
                    Object transferCourse = course.getFieldValueByBeanPath(m_fieldTransferCourse);
                    if (CODE_OUT_OF_DISTRICT.equals(stateCode)
                            || BooleanAsStringConverter.TRUE.equals(transferCourse)) {
                        value = CODE_OUT_OF_DISTRICT;
                    } else {
                        value = schoolId;
                    }
                }
            } else if (PARAM_SCHOOL.equals(parameter)) {
                Object stateCode = transcript.getSchool().getFieldValueByBeanPath(m_fieldSchoolCode);
                Object transferCourse =
                        transcript.getSchoolCourse().getCourse().getFieldValueByBeanPath(m_fieldTransferCourse);
                if (BooleanAsStringConverter.TRUE.equals(transferCourse)) {
                    value = CODE_OUT_OF_DISTRICT;
                } else {
                    value = stateCode;
                }
            } else if (PARAM_CREDITS_ATTEMPTED.equals(parameter)) {
                String termCode = getTranscriptTermCode(transcript);
                String overrideCredit = getCredit(transcript, termCode);
                String credit = (overrideCredit != null) ? overrideCredit : transcript.getPotentialCredit();
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
                    value = transcript.getSchoolCourse().getCourse().getCredit();
                }
            } else if (PARAM_CREDITS_EARNED.equals(parameter)) {
                BigDecimal creditAsNumber = new BigDecimal(0);
                GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                for (GradeScaleGradeDefinition scales : scale.getGradeScaleDefinitions()) {
                    if (scales.getGradeCode().equals(entity.getFieldValue(FIELD_LETTER_GRADE))) {
                        if (scales.getCreditIndicator()) {
                            try {
                                creditAsNumber = new BigDecimal(entity.getFieldValue(FIELD_CREDITS_ATTEMPTED));
                            } catch (NumberFormatException nfe) {
                                creditAsNumber = new BigDecimal(0);
                            }
                        }
                    }
                }

                value = creditAsNumber;
            } else if (PARAM_DESIGNATION_CODE.equals(parameter)) {
                value = transcript.getFieldValueByAlias(ALIAS_COURSE_CODE_OVERRIDE);
                if (value == null) {
                    String designationCode = (String) course.getFieldValueByBeanPath(m_fieldDesignationCode);
                    value = designationCode;
                }
            } else if (PARAM_GRADE_LEVEL.equals(parameter)) {
                String gradeLevel = transcript.getGradeLevel();
                value = data.lookupReferenceCodeByBeanPath(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            } else if (PARAM_STATE_COURSE_CODE.equals(parameter)) {
                if (value == null) {
                    String code = (String) course.getFieldValueByBeanPath(m_fieldStateCourseCode);
                    value = lookupIfExists(Course.class, m_fieldStateCourseCode, code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            if (PARAM_COURSE_ID.equals(parameter)) {
                value = transcript.getFieldValueByAlias(ALIAS_COURSE_NUMBER_OVERRIDE);
                if (value == null) {
                    value = course.getNumber();
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
     * Returns the grade value for the passed term code.
     *
     * @param transcript Transcript
     * @param termCode String
     * @return String
     */
    protected String getGrade(Transcript transcript, String termCode) {
        String grade = "";

        if ("T1".equals(termCode) || "SS".equals(termCode) || "Y0".equals(termCode) || "Q1".equals(termCode)) {
            grade = transcript.getFieldA020();
        } else if ("T2".equals(termCode) || "Q2".equals(termCode)) {
            grade = transcript.getFieldA025();
        } else if ("S1".equals(termCode) || "T3".equals(termCode)) {
            grade = transcript.getFieldA030();
        } else if ("S2".equals(termCode)) {
            grade = transcript.getFieldA045();
        } else if ("Q3".equals(termCode)) {
            grade = transcript.getFieldA035();
        } else if ("Q4".equals(termCode)) {
            grade = transcript.getFieldA040();
        }

        if (grade == null) {
            grade = "";
        }

        return grade;
    }

    /**
     * Returns transcript grade from the transcript record associated with this section or class.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveLetterGrade implements FieldRetriever {

        private HashMap<String, Map<String, String>> m_gradeScaleMap = new HashMap();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentGradeHistoryEntity sghEntity = (WAStudentGradeHistoryEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            String value = null;

            String termCode = getTranscriptTermCode(transcript);

            String finalGrade = getGrade(transcript, termCode);

            if (finalGrade.isEmpty()) {
                finalGrade = transcript.getFinalGrade();
            }

            /*
             * Check ungraded course.
             */
            GradeScale scale = m_gradeScales.get(transcript.getTranscriptDefinitionOid());

            // See if the grade in the final grade column is a grade scale value (letter grade).
            if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                BigDecimal numericGradeBD = m_gradesManager.getNumericValue(finalGrade, scale,
                        transcript.getSchool(), transcript.getSchoolCourseOid());
                if (numericGradeBD != null) {
                    value = finalGrade;
                }

                if (value == null) {
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
                if (StringUtils.isEmpty(value)) {
                    value = finalGrade;
                }
            }

            if (scale != null && !StringUtils.isEmpty(value)) {
                Map<String, String> stateMap = m_gradeScaleMap.get(scale.getOid());
                if (stateMap == null) {
                    stateMap = new HashMap();
                    for (GradeScaleGradeDefinition scaleDefn : scale.getGradeScaleDefinitions()) {
                        stateMap.put(scaleDefn.getGradeCode(),
                                (String) scaleDefn.getFieldValueByBeanPath(m_fieldGradeStateCode));
                    }

                    m_gradeScaleMap.put(scale.getOid(), stateMap);
                }
                String stateCode = stateMap.get(value);
                if (!StringUtils.isEmpty(stateCode)) {
                    value = stateCode;
                }
            }

            return value;
        }
    }

    /**
     * Looks up the override values for the appropriate codes below. If there is no over ride grade,
     * then we do not return the over ride credit. Null is returned otherwise.
     *
     * @author X2 Development Corporation
     * @param transcript Transcript
     * @param termCode String
     * @return String
     */
    protected static String getCredit(Transcript transcript, String termCode) {
        String credit = null;

        if ("T1".equals(termCode) || "SS".equals(termCode) || "Y0".equals(termCode) || "Q1".equals(termCode)) {
            credit = (transcript.getFieldA020() == null) ? transcript.getFieldA007() : null;
        } else if ("T2".equals(termCode) || "Q2".equals(termCode)) {
            credit = (transcript.getFieldA025() == null) ? transcript.getFieldA008() : null;
        } else if ("S1".equals(termCode) || "T3".equals(termCode)) {
            credit = (transcript.getFieldA030() == null) ? transcript.getFieldA009() : null;
        } else if ("Q3".equals(termCode)) {
            credit = (transcript.getFieldA045() == null) ? transcript.getFieldA010() : null;
        } else if ("Q4".equals(termCode)) {
            credit = (transcript.getFieldA035() == null) ? transcript.getFieldA011() : null;
        } else if ("S2".equals(termCode)) {
            credit = (transcript.getFieldA040() == null) ? transcript.getFieldA012() : null;
        }

        return credit;
    }

    /**
     * Returns the local identifier for the primary staff from the Master Schedule if available
     * or the teacher name from the related staff (not via Master Schedule) if available.
     * This calculated value is blank if there is no Master Schedule or related Staff for the
     * student transcript.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStaffId implements FieldRetriever {
        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            WAStudentGradeHistoryEntity sghEntity = (WAStudentGradeHistoryEntity) entity;
            Transcript transcript = sghEntity.getTranscript();
            MasterSchedule schedule = transcript.getMasterSchedule();
            Object value = null;

            if (schedule != null && schedule.getPrimaryStaff() != null) {
                value = schedule.getPrimaryStaff().getStateId();
            } else if (transcript.getTeacher() != null) {
                value = transcript.getTeacher().getStateId();
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
            MasterSchedule masterSchedule = transcript.getMasterSchedule();
            Object value = null;

            String termCode;
            termCode = getTranscriptTermCode(transcript);

            if (PARAM_END_DATE.equals(parameter)) {
                if (m_fieldTransTermEndDate != null) {
                    value = getPropertyAsJavaType(transcript, m_fieldTransTermEndDate);
                }
                if (value == null) {
                    value = m_termEndDateHelper.getGradeEndDate(transcript.getDistrictContextOid(),
                            transcript.getSchoolOid(), termCode);
                }
                if (value != null) {
                    PlainDate date = (PlainDate) value;
                    return date;
                }
                int year = transcript.getDistrictContext().getSchoolYear();
                if (year == getOrganization().getCurrentContext().getSchoolYear()) {
                    value = m_schoolTermDates.get(transcript.getSchoolOid() + "-" + termCode);
                    if (value == null) {
                        if (masterSchedule != null) {
                            value = m_termEndDateHelper.getEndDateSchedule(masterSchedule);
                        } else // If the master schedule is unavailable
                        {
                            termCode = getTranscriptTermCode(transcript);
                            value = m_termEndDateHelper.getEndDateSchool(transcript.getDistrictContextOid(),
                                    transcript.getSchoolOid(), termCode);
                        }
                    }
                }
                if (value == null) {
                    // This is specific to Mukilteo, this is something that is only going to be used
                    // for the 2012-2013 school year and should not be used afterwards

                    // Q1=11/year
                    // Q2=01/year
                    // Q3=03/year
                    // Q4=06/year
                    // SS 08/year (not sure if needed-but put in anyway)
                    // T1 11/year
                    // T2 03/year
                    // T3 06/year

                    if ("S1".equals(termCode) || "Q2".equals(termCode)) {
                        value = "01/";
                    }
                    if ("Q3".equals(termCode) || "T2".equals(termCode)) {
                        value = "03/";
                    }
                    if ("S2".equals(termCode) || "Q4".equals(termCode) || "T3".equals(termCode)
                            || "Y0".equals(termCode)) {
                        value = "06/";
                    }
                    if ("SS".equals(termCode)) {
                        value = "08/";
                    }
                    if ("Q1".equals(termCode) || "T1".equals(termCode)) {
                        value = "11/";
                    }

                    value = value + "" + year;

                    DateFormat format = new SimpleDateFormat("MM/yyyy", Locale.ENGLISH);
                    Date date = null;
                    try {
                        date = format.parse((String) value);
                        value = date;
                    } catch (ParseException e) {
                        // TODO Auto-generated catch block
                    }
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
        // district context oid, school oid, term, end Date
        private Map<String, PlainDate> m_schoolEndDates = new HashMap<String, PlainDate>();
        private Map<String, PlainDate> m_scheduleEndDates = new HashMap<String, PlainDate>();

        /**
         * Find the term end date for a master section.
         *
         * @param masterSchedule MasterSchedule
         * @return PlainDate
         */
        PlainDate getEndDateSchedule(MasterSchedule masterSchedule) {
            PlainDate endDate = m_scheduleEndDates.get(masterSchedule.getScheduleTermOid());
            if (endDate == null) {
                Collection<ScheduleTermDate> scheduleTermDates =
                        masterSchedule.getScheduleTerm().getScheduleTermDates();
                if (scheduleTermDates != null) {
                    for (ScheduleTermDate scheduleTermDate : scheduleTermDates) {
                        if (scheduleTermDate.getEndDate() != null) {
                            if (endDate == null || scheduleTermDate.getEndDate().after(endDate)) {
                                endDate = scheduleTermDate.getEndDate();
                            }
                        }
                    }
                }
                m_scheduleEndDates.put(masterSchedule.getScheduleTermOid(), endDate);
            }
            return endDate;
        }


        /**
         * Gets the grade end date.
         *
         * @param districtContextOid String
         * @param schoolOid String
         * @param termCode String
         * @return Plain date
         */
        PlainDate getGradeEndDate(String districtContextOid, String schoolOid, String termCode) {
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
            return endDate;
        }

        /**
         * Return the term end date for a term code in a school.
         *
         * @param districtContextOid String
         * @param schoolOid String
         * @param termCode String
         * @return PlainDate
         */
        PlainDate getEndDateSchool(String districtContextOid, String schoolOid, String termCode) {
            String mapKey = schoolOid + "-" + termCode;
            PlainDate endDate = null;
            if (m_schoolEndDates.containsKey(mapKey)) {
                endDate = m_schoolEndDates.get(mapKey);
            } else {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER + ScheduleTerm.COL_CODE,
                        termCode);
                criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER
                        + ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER
                        + Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER
                        + SchoolScheduleContext.COL_SCHOOL_OID, schoolOid);
                criteria.addEqualTo(ScheduleTermDate.REL_SCHEDULE_TERM + PATH_DELIMITER
                        + ScheduleTerm.REL_SCHEDULE + PATH_DELIMITER
                        + Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER
                        + SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, districtContextOid);
                QueryByCriteria query = new QueryByCriteria(ScheduleTermDate.class, criteria);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);

                try {
                    while (iterator.hasNext()) {
                        ScheduleTermDate scheduleTermDate = (ScheduleTermDate) iterator.next();
                        if (endDate == null || scheduleTermDate.getEndDate().after(endDate)) {
                            endDate = scheduleTermDate.getEndDate();
                        }
                    }
                } finally {
                    iterator.close();
                }
                m_schoolEndDates.put(mapKey, endDate);
            }
            return endDate;
        }
    }

    /**
     * Validate CTE Course Equivalency Identification.
     */
    protected class ValidateCteCourse implements FieldValidator {
        private static final String VAL_ID = "VAL-CTE-COURSE";

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

            if (!StringUtils.isEmpty(cipCode) && StringUtils.isEmpty(value)) {

                errors.add(new StateReportValidationError(entity, field,
                        "If CIP Code is not NULL then CTE Course Equivalency should have a value",
                        "CIP Code = " + STYLE_BOLD + cipCode + STYLE_END +
                                ", CTE Course Equivalency = " + STYLE_BOLD + value +
                                STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate CTE Credit Equivalency Identification.
     */
    protected class ValidateCteCredit implements FieldValidator {
        private static final String VAL_ID = "VAL-CTE-CREDIT";
        private final List VALID_CTE_CRS = Arrays.asList(new String[] {"A", "B"});

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
            String cteCode = entity.getFieldValue(FIELD_CTE_COURSE);

            if (VALID_CTE_CRS.contains(cteCode) && StringUtils.isEmpty(value)) {

                errors.add(new StateReportValidationError(entity, field,
                        "If CTE Course Equivalency equals A or B, data must be provided for CTE Credit Equivalency.",
                        "CTE Course Equivalency = " + STYLE_BOLD + cteCode + STYLE_END +
                                ", CTE Credit Equivalency = " + STYLE_BOLD + value +
                                STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Gets the transcript term code.
     *
     * @param transcript Transcript
     * @return String
     */
    static String getTranscriptTermCode(Transcript transcript) {
        String termCode = null;
        if (transcript.getMasterSchedule() != null &&
                transcript.getMasterSchedule().getScheduleTerm() != null &&
                !StringUtils.isEmpty(transcript.getMasterSchedule().getScheduleTerm().getCode()) &&
                !WAStudentGradeHistoryEntity.TERM_CODE_Q0
                        .equals(transcript.getMasterSchedule().getScheduleTerm().getCode())
                &&
                !WAStudentGradeHistoryEntity.TERM_CODE_T0
                        .equals(transcript.getMasterSchedule().getScheduleTerm().getCode())) {
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
     * Constants: Aliases
     */
    protected static final String ALIAS_APIB_CODE = "DOE AP IB CODE";
    protected static final String ALIAS_CIP_CODE = "DOE CIP CODE";
    protected static final String ALIAS_CONTENT_AREA = "DOE CONTENT AREA";
    protected static final String ALIAS_CRS_CTE_EQUIV = "DOE CTE COURSE EQUIVALENCY";
    protected static final String ALIAS_CTE_CRS_CREDIT_EQUIV = "DOE CTE CREDIT EQUIVALENCY";
    protected static final String ALIAS_CTE_TRN_CREDIT_EQUIV = "DOE STD CTE CREDIT EQUIVALENCY";
    protected static final String ALIAS_DESIGNATION_CODE = "DOE CRS DESIGNATION";
    protected static final String ALIAS_EXCLUDE_GRDHST = "DOE EXCLUDE GRDHST";
    protected static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_TRN = "DOE EXCLUDE TRN";
    protected static final String ALIAS_GRADE_STATE_CODE = "DOE GRADE STATE CODE";
    protected static final String ALIAS_LOCATION_OVERRIDE = "DOE LOC OVERRIDE";
    protected static final String ALIAS_STATE_COURSE_CODE = "DOE STATE COURSE CODE";
    protected static final String ALIAS_TRN_END_DATE = "DOE TRN END DATE";
    protected static final String ALIAS_STD_ASSESSMENT_PASSER = "DOE STD ASSESSMENT PASSER";
    protected static final String ALIAS_STD_CTE_COMPLETER = "DOE STD CTE COMPLETER";
    protected static final String ALIAS_STD_STD_CERTIFICATION = "DOE STD CERTIFICATION";
    protected static final String ALIAS_STD_TECH_COMPLETER = "DOE STD TECH COMPLETER";
    protected static final String ALIAS_TRANSFER_COURSE = "DOE TRANSFER COURSE";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";

    /*
     * Export format field names
     */
    protected static final String FIELD_CIP_CODE = "CIPCode";
    protected static final String FIELD_CTE_COURSE = "CTECourseEquiv";

    /**
     * Other constants
     */
    protected static final String CODE_OUT_OF_DISTRICT = "9999";
    protected static final String VALUE_DEFAULT_CTE_EQ = "C";

    /**
     * Instance variables.
     */
    protected Map<String, ReferenceCode> m_apIbCodesMap;
    protected String m_fieldAPIBCode;
    protected String m_fieldCIPCode;
    protected String m_fieldContentArea;
    protected String m_fieldCrsCteEquivalency;
    protected String m_fieldCteCrsCreditEquivalency;
    protected String m_fieldCteTrnCreditEquivalency;
    protected String m_fieldDesignationCode;
    protected String m_fieldExcludeGrdhst;
    protected String m_fieldExcludeTrn;
    protected String m_fieldGradeStateCode;
    protected String m_fieldLocationOveride;
    protected String m_fieldSchoolCode;
    protected String m_fieldStateCourseCode;
    protected String m_fieldStateTermCode;
    protected String m_fieldTransferCourse;
    protected String m_fieldTransTermEndDate;

    protected GradesManager m_gradesManager;
    protected Map<String, GradeScale> m_gradeScales;
    protected Map<String, PlainDate> m_schoolTermDates;
    protected StudentHistoryHelper m_historyHelper;
    protected TermEndDateHelper m_termEndDateHelper;
    protected Map<String, Collection<Transcript>> m_transcriptMap;

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

        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
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

            loadSchoolTermMaps();

            m_termEndDateHelper = new TermEndDateHelper();
            m_gradesManager = new GradesManager(getBroker());

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("TRN-COURSE", new RetrieveCourse());
            calcs.put("TRN-STAFFID", new RetrieveStaffId());
            calcs.put("TRN-LETTERGRADE", new RetrieveLetterGrade());
            calcs.put("TRN-TERM", new RetrieveTerm());
            calcs.put("TRN-TRN", new RetrieveTranscriptValue());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateCteCourse.VAL_ID, new ValidateCteCourse());
            validators.put(ValidateCteCredit.VAL_ID, new ValidateCteCredit());
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
        m_fieldAPIBCode = translateAliasToJavaName(ALIAS_APIB_CODE, true);
        m_fieldCIPCode = translateAliasToJavaName(ALIAS_CIP_CODE, true);
        m_fieldContentArea = translateAliasToJavaName(ALIAS_CONTENT_AREA, true);
        m_fieldCrsCteEquivalency = translateAliasToJavaName(ALIAS_CRS_CTE_EQUIV, true);
        m_fieldCteCrsCreditEquivalency = translateAliasToJavaName(ALIAS_CTE_CRS_CREDIT_EQUIV, true);
        m_fieldCteTrnCreditEquivalency = translateAliasToJavaName(ALIAS_CTE_TRN_CREDIT_EQUIV, true);
        m_fieldDesignationCode = translateAliasToJavaName(ALIAS_DESIGNATION_CODE, true);
        // m_fieldExcludeStd = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_fieldExcludeTrn = translateAliasToJavaName(ALIAS_EXCLUDE_TRN, false);
        m_fieldExcludeGrdhst = translateAliasToJavaName(ALIAS_EXCLUDE_GRDHST, false);
        // m_fieldExcludeMst = translateAliasToJavaName(ALIAS_EXCLUDE_MST, false);
        m_fieldGradeStateCode = translateAliasToJavaName(ALIAS_GRADE_STATE_CODE, true);
        m_fieldStateCourseCode = translateAliasToJavaName(ALIAS_STATE_COURSE_CODE, true);
        m_fieldTransTermEndDate = translateAliasToJavaName(ALIAS_TRN_END_DATE, true);
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldTransferCourse = translateAliasToJavaName(ALIAS_TRANSFER_COURSE, true);
        m_fieldLocationOveride = translateAliasToJavaName(ALIAS_LOCATION_OVERRIDE, true);

        // Transcript aliases.
        // m_fieldTechCompleter = translateAliasToJavaName(ALIAS_STD_TECH_COMPLETER, true);
        // m_fieldVocCompleter = translateAliasToJavaName(ALIAS_STD_CTE_COMPLETER, true);
        // m_fieldAssessment = translateAliasToJavaName(ALIAS_STD_ASSESSMENT_PASSER, true);
        // m_fieldCertification = translateAliasToJavaName(ALIAS_STD_STD_CERTIFICATION, true);
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
     * Load school start and end dates by term.
     */
    private void loadSchoolTermMaps() {
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, new X2Criteria());
        Map schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));

        m_schoolTermDates = new HashMap<String, PlainDate>();

        PlainDate termEndDate = null;
        for (Object schoolObject : schoolMap.values()) {
            SisSchool school = (SisSchool) schoolObject;
            String schoolIdString = school.getOid();
            Schedule schoolSchedule = school.getActiveSchedule();
            if (schoolSchedule != null) {
                for (ScheduleTerm term : schoolSchedule.getScheduleTerms()) {
                    termEndDate = null;
                    for (ScheduleTermDate termDates : term.getScheduleTermDates()) {
                        termEndDate = termDates.getEndDate();
                    }
                    m_schoolTermDates.put(schoolIdString + "-" + term.getCode(), termEndDate);
                }
            }
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

        BeanQuery query = new BeanQuery(Transcript.class, transcriptCriteria);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 1000);
    }

    public Map m_excludeSchool;
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";


    /**
     * Load school exclude map.
     */
    private void loadSchoolExcludeMap() {
        String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(schoolExclude, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        Map excludedSchools = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
        // To identify and remove the transfer school, as we want to include the transcripts from
        // there.
        for (Object schoolOid : excludedSchools.entrySet()) {
            SisSchool school = (SisSchool) excludedSchools.get(schoolOid);
            if (school != null && CODE_OUT_OF_DISTRICT.equals(school.getFieldValueByBeanPath(m_fieldSchoolCode))) {
                m_excludeSchool.put(schoolOid, school);
            }
        }
    }

    /**
     * Include school.
     *
     * @param schoolOid String
     * @return true, if successful
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchool == null) || !m_excludeSchool.containsKey(schoolOid);
    }
}
