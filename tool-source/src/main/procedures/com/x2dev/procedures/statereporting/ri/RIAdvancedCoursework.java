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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradeScaleGradeDefinition;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.model.beans.TranscriptColumnDefinition;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for the TCS Student Dual and Concurrent Export.
 * This class implements the data export for the TCS Student Dual and Concurrent Export.
 *
 * @author X2 Development Corporation
 */
public class RIAdvancedCoursework extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TCS Student Dual and Concurrent Export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RIAdvancedCourseworkEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        /*
         * Cached values for retrievers to share.
         */
        RIAdvancedCoursework m_tcsData = null;
        List<CourseAndTranscriptInfo> m_crsAndTrnInfos = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public RIAdvancedCourseworkEntity() {
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
            CourseAndTranscriptInfo info = getCrsTrnInfo();
            if (info != null) {
                name += info.getProviderCourseName();
            }

            return name;
        }

        /**
         * Returns the schedule info record for the schedule record(s)
         * based in the current row value.
         *
         * @return ScheduleInfo
         */
        public CourseAndTranscriptInfo getCrsTrnInfo() {
            CourseAndTranscriptInfo info = null;
            if (m_crsAndTrnInfos != null && getCurrentRow() < m_crsAndTrnInfos.size() && getCurrentRow() >= 0) {
                info = m_crsAndTrnInfos.get(getCurrentRow());
            }
            return info;
        }

        /**
         * Initialize the entity for the student bean provided.
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

            // Sort records by course view.
            TreeSet<CourseAndTranscriptInfo> infos =
                    new TreeSet<CourseAndTranscriptInfo>(new Comparator<CourseAndTranscriptInfo>() {
                        @Override
                        public int compare(CourseAndTranscriptInfo info1, CourseAndTranscriptInfo info2) {
                            return info1.getCourseView().compareTo(info2.getCourseView());
                        }
                    });
            m_tcsData = (RIAdvancedCoursework) data;
            SisStudent student = (SisStudent) bean;

            Collection<Transcript> transcripts = m_tcsData.m_transcriptMap.get(student.getOid());
            /*
             * Remove student if withdrew from school before the current year's start date
             */
            List<StudentEnrollmentSpan> spans = m_tcsData.m_helper.getStudentEnrollmentSpans(student, true);
            if (spans.isEmpty()) {
                setRowCount(0);
                return;
            }

            // Add current schedule sections to the map.
            if (transcripts != null) {
                for (Transcript transcript : transcripts) {
                    CourseAndTranscriptInfo info = new CourseAndTranscriptInfo(transcript, m_tcsData);
                    infos.add(info);
                }
            }

            m_crsAndTrnInfos = new ArrayList<CourseAndTranscriptInfo>(infos);

            /*
             * With m_schedules populated, we now know how many schedule records for this student.
             */
            setRowCount(m_crsAndTrnInfos.size());
            setCurrentRow(0);
        }
    }

    /**
     * Returns data about Dual\Concurrent enrollment in PS institutions.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveAdvancedCourseworkData implements FieldRetriever {
        private static final String CALC_ID = "ADV-CRSW";

        private final String PARAM_CRS_EX_REASON = "CRS-EX-REASON";
        private final String PARAM_CRS_TERM = "CRS-TERM";
        private final String PARAM_CRS_NETWORK = "CRS-NETWORK";
        private final String PARAM_ENR_TYPE = "ENR-TYPE";
        private final String PARAM_LETTER_GRADE = "LETTER-GRADE";
        private final String PARAM_NUMERIC_GRADE = "NUMERIC-GRADE";
        private final String PARAM_PRVD_ID = "PRVD-ID";
        private final String PARAM_PRVD_CRS_ID = "PRVD-CRS-ID";
        private final String PARAM_PRVD_CRS_NAME = "PRVD-CRS-NAME";
        private final String PARAM_PRVD_CRS_CREDITS = "PRVD-CRS-CRED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            String param = (String) field.getParameter();
            RIAdvancedCoursework advCrsData = (RIAdvancedCoursework) data;
            CourseAndTranscriptInfo info = ((RIAdvancedCourseworkEntity) entity).getCrsTrnInfo();

            if (PARAM_CRS_TERM.equals(param)) {
                value = info.getCourseTerm();
            } else if (PARAM_ENR_TYPE.equals(param)) {
                value = info.getEnrollType();
            } else if (PARAM_CRS_EX_REASON.equals(param)) {
                String trnFinalGrade = info.getTranscript() != null ? info.getTranscript().getFinalGrade() : null;
                if (!StringUtils.isEmpty(trnFinalGrade)) {
                    value = VALUE_CRS_COMPLETED;
                } else {
                    value = info.getExitReason();
                }
            } else if (PARAM_CRS_NETWORK.equals(param)) {
                value = BooleanAsStringConverter.TRUE.equals(info.getCrsAdvNetwork()) ? "Y" : "N";
            } else if (PARAM_PRVD_ID.equals(param)) {
                value = info.getProviderID();
            } else if (PARAM_PRVD_CRS_ID.equals(param)) {
                value = info.getProviderCourseID();
            } else if (PARAM_PRVD_CRS_NAME.equals(param)) {
                value = info.getProviderCourseName();
            } else if (PARAM_PRVD_CRS_CREDITS.equals(param)) {
                try {
                    value = new BigDecimal(info.getProviderCourseCredits());
                } catch (Exception e) {
                    // String representation of number is expected.
                }
            } else if (PARAM_LETTER_GRADE.equals(param)) {
                Transcript transcript = info.getTranscript();

                String letterGradeValue = null;

                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();
                    letterGradeValue = finalGrade;

                    GradeScale scale = advCrsData.m_gradeScales.get(transcript.getTranscriptDefinitionOid());
                    String letterGrade = null;

                    if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                        BigDecimal numericGradeBD = advCrsData.m_gradesManager.getNumericValue(finalGrade, scale,
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
                                letterGrade = advCrsData.m_gradesManager.getLetterValue(gradeAsNumber, scale,
                                        transcript.getSchool(), transcript.getSchoolCourseOid());
                            }
                        }

                        if (!StringUtils.isEmpty(letterGrade)) {
                            letterGradeValue = advCrsData.lookupStateValue(GradeScaleGradeDefinition.class,
                                    GradeScaleGradeDefinition.COL_GRADE_CODE,
                                    letterGrade);
                        }
                    }
                }

                value = letterGradeValue;
            } else if (PARAM_NUMERIC_GRADE.equals(param)) {
                Transcript transcript = info.getTranscript();
                Object numericGradeValue = null;

                if (transcript != null) {
                    String finalGrade = transcript.getFinalGrade();
                    GradeScale scale = advCrsData.m_gradeScales.get(transcript.getTranscriptDefinitionOid());

                    if (!StringUtils.isEmpty(finalGrade) && scale != null) {
                        // If the final grade is a letter value, convert it to a number.
                        numericGradeValue = advCrsData.m_gradesManager.getNumericValue(finalGrade, scale,
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

                value = numericGradeValue;
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
        private static final String CALC_ID = "DISTRICT-SCHOOL";

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
            CourseAndTranscriptInfo info = ((RIAdvancedCourseworkEntity) entity).getCrsTrnInfo();

            if (PARAM_DISTRICT.equals(param)) {
                if (info.getSection() == null) {
                    String adjustedDistrCode =
                            (String) info.getTranscript().getSchool().getFieldValueByBeanPath(m_sklFieldAdjDistr);
                    value = !StringUtils.isEmpty(adjustedDistrCode) ? adjustedDistrCode
                            : info.getTranscript().getSchool().getOrganization1()
                                    .getFieldValueByBeanPath(m_districtIdField);
                } else {
                    String adjustedDistrCode = (String) info.getSection().getSchedule().getSchool()
                            .getFieldValueByBeanPath(m_sklFieldAdjDistr);
                    value = !StringUtils.isEmpty(adjustedDistrCode) ? adjustedDistrCode
                            : info.getSection().getSchedule().getSchool().getOrganization1()
                                    .getFieldValueByBeanPath(m_districtIdField);
                }
            } else if (PARAM_SCHOOL.equals(param)) {
                value = info.getSection() == null
                        ? info.getTranscript().getSchool().getFieldValueByBeanPath(m_sklIdField)
                        : info.getSection().getSchedule().getSchool().getFieldValueByBeanPath(m_sklIdField);
            }

            return value;
        }
    }

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
        private static final String CALC_ID = "SECTION";

        private final String PARAM_CREDIT = "CREDIT";
        private final String PARAM_CREDIT_TOTAL = "CREDIT-TOTAL";
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
            CourseAndTranscriptInfo info = ((RIAdvancedCourseworkEntity) entity).getCrsTrnInfo();
            MasterSchedule section = info.getSection();
            Transcript transcript = info.getTranscript();
            if (PARAM_ID.equals(param)) {
                value = section == null ? null : section.getCourseView();
            } else if (PARAM_CREDIT.equals(param)) {
                if (transcript != null) {
                    String psCreditsEarned = (String) transcript.getFieldValueByBeanPath(m_trnCreditsEarned);
                    if (psCreditsEarned != null) {
                        BigDecimal totalCredit = new BigDecimal(psCreditsEarned);
                        value = totalCredit;
                    }
                }
            } else if (PARAM_CREDIT_TOTAL.equals(param)) {
                if (transcript != null) {
                    value = transcript.getTotalCredit();
                }
            }

            return value;
        }
    }

    /**
     * A container class for record information for one Dual/Concurrent enrollment.
     *
     * @author X2 Development Corporation
     */
    protected static class CourseAndTranscriptInfo {
        private Object m_crsAdvNetwork;
        private String m_enrollmentType;
        private String m_exitReason;
        private String m_crsTerm;
        private String m_prvdCourseCredits;
        private String m_prvdCourseID;
        private String m_prvdCourseName;
        private String m_prvdID;
        private MasterSchedule m_section;
        private Transcript m_transcript;

        /**
         * Instantiates a new course and transcript info.
         *
         * @param transcript Transcript
         * @param data RIAdvancedCoursework
         */
        public CourseAndTranscriptInfo(Transcript transcript, RIAdvancedCoursework data) {
            m_transcript = transcript;
            m_section = m_transcript.getMasterSchedule();

            m_crsAdvNetwork = m_transcript.getFieldValueByBeanPath(data.m_trnCrsNetwork);

            m_enrollmentType = (String) m_transcript.getFieldValueByBeanPath(data.m_trnEnrollType);
            m_enrollmentType = data.lookupReferenceCodeByBeanPath(Transcript.class,
                    data.m_trnEnrollType, m_enrollmentType,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            m_exitReason = (String) m_transcript.getFieldValueByBeanPath(data.m_trnExReason);
            m_exitReason = data.lookupReferenceCodeByBeanPath(Transcript.class,
                    data.m_trnExReason, m_exitReason,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            m_prvdCourseCredits = (String) m_transcript.getFieldValueByBeanPath(data.m_trnInstitutionCrsCredits);
            m_prvdCourseID = (String) m_transcript.getFieldValueByBeanPath(data.m_trnProviderCrsId);
            m_prvdCourseName = (String) m_transcript.getFieldValueByBeanPath(data.m_trnProviderCrsName);

            m_prvdID = (String) m_transcript.getFieldValueByBeanPath(data.m_trnProviderId);
            m_prvdID = data.lookupReferenceCodeByBeanPath(Transcript.class,
                    data.m_trnProviderId, m_prvdID,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            m_crsTerm = (String) m_transcript.getFieldValueByBeanPath(data.m_trnCrsTerm);
            m_crsTerm = data.lookupReferenceCodeByBeanPath(Transcript.class,
                    data.m_trnCrsTerm, m_crsTerm,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

        }

        /**
         * Return the name of the section for use in sorting.
         *
         * @return String
         */
        protected String getCourseView() {
            String name = "";
            if (m_section != null) {
                name = m_section.getCourseView();
            } else {
                name += m_enrollmentType + m_prvdCourseCredits + m_prvdCourseID + m_prvdCourseName + m_prvdID;
            }
            return name;
        }

        /**
         * Gets the course term.
         *
         * @return String
         */
        protected String getCourseTerm() {
            return m_crsTerm;
        }

        /**
         * Gets the crs adv network.
         *
         * @return Object
         */
        protected Object getCrsAdvNetwork() {
            return m_crsAdvNetwork;
        }

        /**
         * Gets the enroll type.
         *
         * @return String
         */
        protected String getEnrollType() {
            return m_enrollmentType;
        }

        /**
         * Gets the exit reason.
         *
         * @return String
         */
        protected String getExitReason() {
            return m_exitReason;
        }

        /**
         * Gets the provider course credits.
         *
         * @return String
         */
        protected String getProviderCourseCredits() {
            return m_prvdCourseCredits;
        }

        /**
         * Gets the provider course ID.
         *
         * @return String
         */
        protected String getProviderCourseID() {
            return m_prvdCourseID;
        }

        /**
         * Gets the provider course name.
         *
         * @return String
         */
        protected String getProviderCourseName() {
            return m_prvdCourseName;
        }

        /**
         * Gets the provider ID.
         *
         * @return String
         */
        protected String getProviderID() {
            return m_prvdID;
        }

        /**
         * Gets the section.
         *
         * @return Master schedule
         */
        protected MasterSchedule getSection() {
            return m_section;
        }

        /**
         * Gets the transcript.
         *
         * @return Transcript
         */
        protected Transcript getTranscript() {
            return m_transcript;
        }

        /**
         * Sets the transcript.
         *
         * @param transcript void
         */
        protected void setTranscript(Transcript transcript) {
            m_transcript = transcript;
        }
    }

    /**
     * "Credit" can either be 0.00 or equal to "Institution Crs Cred"
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
            RIAdvancedCourseworkEntity dcEntity = (RIAdvancedCourseworkEntity) entity;
            String instCourseCredits = dcEntity.getCrsTrnInfo().getProviderCourseCredits();
            if (!"0.00".equals(value) &&
                    ((instCourseCredits != null && !instCourseCredits.equals(value)) ||
                            (instCourseCredits == null && value != null))) {
                errors.add(new StateReportValidationError(entity, field,
                        "Credit is not 0.00 and not equal to Institution Crs Cred",
                        "Credit = " + STYLE_BOLD + String.valueOf(value) + STYLE_END +
                                ", Institution Crs Cred = " + STYLE_BOLD +
                                String.valueOf(instCourseCredits) + STYLE_END));
            }

            return errors;
        }

    }

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

    protected static final String ALIAS_SSC_ENR_TYPE = "all-ssc-CourseEnrollmentType";
    protected static final String ALIAS_SSC_COURSE_CREDITS = "all-ssc-ProviderCourseCredits";
    protected static final String ALIAS_SSC_CRS_EXIT_RESON = "all-ssc-CourseExitReason";
    protected static final String ALIAS_SSC_CRS_NETWORK = "all-ssc-AdvancedCourseNetwork";
    protected static final String ALIAS_SSC_CRS_TERM = "all-ssc-CourseTermPSorOther";
    protected static final String ALIAS_SSC_PROVIDER_ID = "all-ssc-ProviderID";
    protected static final String ALIAS_SSC_PROVIDER_CRS_ID = "all-ssc-ProviderCourseID";
    protected static final String ALIAS_SSC_PROVIDER_CRS_NAME = "all-ssc-ProviderCourseName";

    protected static final String ALIAS_TRN_ENR_TYPE = "all-trn-CourseEnrollmentType";
    protected static final String ALIAS_TRN_COURSE_CREDITS = "all-trn-ProviderCourseCredits";
    protected static final String ALIAS_TRN_CRS_EXIT_RESON = "all-trn-CourseExitReason";
    protected static final String ALIAS_TRN_CRS_NETWORK = "all-trn-AdvancedCourseNetwork";
    protected static final String ALIAS_TRN_CRS_TERM = "all-trn-CourseTermPSorOther";
    protected static final String ALIAS_TRN_PROVIDER_ID = "all-trn-ProviderID";
    protected static final String ALIAS_TRN_PROVIDER_CR_EARNED = "all-trn-ProviderCreditEarned";
    protected static final String ALIAS_TRN_PROVIDER_CRS_ID = "all-trn-ProviderCourseID";
    protected static final String ALIAS_TRN_PROVIDER_CRS_NAME = "all-trn-ProviderCourseName";

    /**
     * Other constants
     */
    public final String VALUE_CRS_COMPLETED = "COMPLETED";
    public final String VALUE_CS_MOVED = "MOVED";
    public static final String STUDENT_STATUS_INACTIVE = "Inactive";

    /**
     * Member fields
     */
    protected String m_trnCreditsEarned;
    protected String m_trnCrsNetwork;
    protected String m_trnCrsTerm;
    protected String m_trnEnrollType;
    protected String m_trnExReason;
    protected String m_trnInstitutionCrsCredits;
    protected String m_trnProviderId;
    protected String m_trnProviderCrsId;
    protected String m_trnProviderCrsName;

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected Set<String> m_enrollmentTypes;
    protected String m_excludeCourseField;
    protected String m_excludeSectionField;
    protected String m_excludeStudentField;
    protected GradesManager m_gradesManager;
    protected Map<String, GradeScale> m_gradeScales;
    protected StudentHistoryHelper m_helper;
    protected boolean m_includeStateExclusion;
    protected String m_orgFieldStr;
    protected String m_orgOid;
    protected PlainDate m_reportDate;
    protected boolean m_sasidStudentsOnly;
    protected X2Criteria m_scheduleCriteria;
    protected Map<String, Collection<Transcript>> m_transcriptMap;

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
        if (getSetupErrors().isEmpty()) {
            initEnrollmentTypes();
        }
        if (getSetupErrors().isEmpty()) {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));

            // Load a map of student transcript records my student.
            // Must load before student criteria is generated. That depends on
            // schedule and change criteria.
            loadTranscripts();

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.getStudentCriteria().addIn(X2BaseBean.COL_OID, m_transcriptMap.keySet());

            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(RIAdvancedCourseworkEntity.class);

            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveDistrictSchool.CALC_ID, new RetrieveDistrictSchool());
            calcs.put(RetrieveSection.CALC_ID, new RetrieveSection());
            calcs.put(RetrieveAdvancedCourseworkData.CALC_ID, new RetrieveAdvancedCourseworkData());
            addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("VAL-CREDITS", new ValidateCredits());
            addValidators(validators);
        }
    }

    /**
     * Build the criteria to query transcripts.
     *
     * @return X 2 criteria
     */
    private X2Criteria getTranscriptCriteria() {
        X2Criteria transcriptCriteria = new X2Criteria();
        transcriptCriteria.addEqualTo(Transcript.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        transcriptCriteria.addIn(m_trnEnrollType, m_enrollmentTypes);

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

        // Check include exclusion flag as well as exclusion flags for student,
        // section and student schedule.
        if (!m_includeStateExclusion && !StringUtils.isEmpty(m_excludeStudentField)) {
            transcriptCriteria.addNotEqualTo(Transcript.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStudentField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with
        // non-empty state ids
        if (m_sasidStudentsOnly) {
            transcriptCriteria.addNotEmpty(Transcript.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }
        applyInputCriteria(transcriptCriteria, true, Transcript.REL_STUDENT);

        return transcriptCriteria;
    }

    /**
     * Find the set of post-secondary enrollment types.
     */
    private void initEnrollmentTypes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField enrollTypeField = dictionary.findDataDictionaryField(Transcript.class.getName(),
                m_trnEnrollType);
        if (enrollTypeField.getReferenceTable() != null) {
            m_enrollmentTypes = enrollTypeField.getReferenceTable().getCodeMap().keySet();
        } else {
            this.addSetupError("Missing Reference Table",
                    "A reference table is needed for field StudentTranscript.[" + ALIAS_TRN_ENR_TYPE + "]");
        }
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

        // Lookup potential exclude fields. Not required.
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

        m_trnCreditsEarned = translateAliasToJavaName(ALIAS_TRN_PROVIDER_CR_EARNED, true);
        m_trnCrsNetwork = translateAliasToJavaName(ALIAS_TRN_CRS_NETWORK, true);
        m_trnCrsTerm = translateAliasToJavaName(ALIAS_TRN_CRS_TERM, true);
        m_trnEnrollType = translateAliasToJavaName(ALIAS_TRN_ENR_TYPE, true);
        m_trnExReason = translateAliasToJavaName(ALIAS_TRN_CRS_EXIT_RESON, true);
        m_trnInstitutionCrsCredits = translateAliasToJavaName(ALIAS_TRN_COURSE_CREDITS, true);
        m_trnProviderId = translateAliasToJavaName(ALIAS_TRN_PROVIDER_ID, true);
        m_trnProviderCrsId = translateAliasToJavaName(ALIAS_TRN_PROVIDER_CRS_ID, true);
        m_trnProviderCrsName = translateAliasToJavaName(ALIAS_TRN_PROVIDER_CRS_NAME, true);

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

    }

    /**
     * Loads a map of sets of transcript records by studentOid for students in the export.
     * In certain condition transcripts could be duplicated in collection for a single student
     */
    private void loadTranscripts() {
        X2Criteria transcriptCriteria = getTranscriptCriteria();
        QueryByCriteria query = new QueryByCriteria(Transcript.class, transcriptCriteria);
        query.addOrderBy(Transcript.COL_STUDENT_OID, true);
        m_transcriptMap = getBroker().getGroupedCollectionByQuery(query, Transcript.COL_STUDENT_OID, 500);
    }
}
