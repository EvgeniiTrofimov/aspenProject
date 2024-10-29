/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import static com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity.FIELD_NAME_ALT_REPORT_CARD;
import static com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity.FLAG_T;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolGradeTermDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolRubricAssessment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolRubricCriterion;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolRubricDefinition;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolTranscriptRubric;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnRubricAssessmentPerformance;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnRubricCriterion;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSLPProgram;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnTranscript;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisReportCard.OnsisReportCardEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisReportCardTerm.OnsisReportCardTermEntity;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Class OnsisReportCardSubjectStrand.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisReportCardSubjectStrand extends OnsisStateReportData {


    /**
     * The Class OnsisReportCardSubjectStrandEntity.
     */
    public static class OnsisReportCardSubjectStrandEntity extends OnsisStateReportEntity {

        /**
         * The Class Strand.
         */
        private class Strand {
            private boolean m_isHeader;
            private boolean m_isSynthetic = false;
            private OnRubricAssessmentPerformance m_rap;
            private List<OnRubricAssessmentPerformance> m_rapBeans;
            private Set<String> m_schoolCourseOids;
            private String m_stateCode;

            /**
             * Instantiates a new strand.
             *
             * @param stateCode the state code
             * @param rap the rap
             * @param isHeader the is header
             */
            Strand(String stateCode, OnRubricAssessmentPerformance rap, boolean isHeader) {
                m_stateCode = stateCode;
                m_rap = rap;
                m_isHeader = isHeader;
            }

            /**
             * Instantiates a new strand.
             *
             * @param stateCode the state code
             * @param schoolCourseOids the school course oids
             */
            Strand(String stateCode, Set<String> schoolCourseOids, boolean isSynthetic) {
                m_stateCode = stateCode;
                m_schoolCourseOids = schoolCourseOids;
                m_isSynthetic = isSynthetic;
            }

            /**
             * Set to state code only so that set includes the first instance of Strand for any
             * particular state code.
             *
             * @param obj the obj
             * @return true, if successful
             * @see java.lang.Object#equals(java.lang.Object)
             */
            @Override
            public boolean equals(Object obj) {
                if (this == obj) {
                    return true;
                }
                if (obj == null) {
                    return false;
                }
                if (getClass() != obj.getClass()) {
                    return false;
                }
                Strand strand = (Strand) obj;
                return Objects.equals(getStateCode(), strand.getStateCode());
            }

            /**
             * Hash code.
             *
             * @return the int
             * @see java.lang.Object#hashCode()
             */
            @Override
            public int hashCode() {
                return getStateCode().hashCode();
            }

            /**
             * To string.
             *
             * @return the string
             * @see java.lang.Object#toString()
             */
            @Override
            public String toString() {
                StringBuilder output = new StringBuilder();
                output.append("Code: " + getStateCode());
                output.append(" isHeader: " + isHeader());
                if (m_rap != null) {
                    output.append(" OID: " + m_rap.getOid());
                    output.append(" ID: " + m_rap.getId());
                    OnRubricCriterion rubricCriterion = (OnRubricCriterion) m_rap.getRubricCriterion(getBroker());
                    if (rubricCriterion != null) {
                        output.append(" HEADER: " + rubricCriterion.getColumnHeader());
                        ToolRubricDefinition rubricDefinition = rubricCriterion.getRubricDefinition(getBroker());
                        String stateCode = rubricDefinition.getSubjectCode();
                        output.append(" SUBJECT: " + stateCode);
                        stateCode = rubricCriterion.getSubjectStrand();
                        output.append(" SUBJECT_STRAND: " + stateCode);
                    }
                    output.append("\n");
                }
                return output.toString();
            }

            /**
             * Gets the rubric assessment performances.
             *
             * @return the rubric assessment performances
             */
            Collection<OnRubricAssessmentPerformance> getRubricAssessmentPerformances() {
                if (m_rapBeans == null) {
                    m_rapBeans = m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand.stream()
                            .filter(rap -> {
                                OnRubricCriterion rubricCriterion =
                                        (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                                if (rubricCriterion == null) {
                                    return false;
                                }

                                String stateValue = rubricCriterion.getSubjectStrand();
                                ToolRubricAssessment rubricAssessment = rap.getRubricAssessment(getBroker());
                                if (rubricAssessment == null) {
                                    return false;
                                }

                                if (getStateCode().equals(stateValue)) {
                                    if (getSchoolCourseOids().isEmpty()) {
                                        return true;
                                    }

                                    // Get the SchoolCourses
                                    Set<String> schoolCourseOids =
                                            rubricAssessment.getTranscriptRubrics(getBroker()).stream()
                                                    .map(trr -> trr.getSchoolCourseOid())
                                                    .filter(Objects::nonNull).collect(Collectors.toSet());

                                    return schoolCourseOids.equals(getSchoolCourseOids());
                                }
                                return false;
                            })
                            .collect(Collectors.toList());
                }
                return m_rapBeans;
            }

            /**
             * Gets the school course oids.
             *
             * @return the school course oids
             */
            Set<String> getSchoolCourseOids() {
                if (m_schoolCourseOids == null) {
                    if (m_rap != null && VALID_SUBJECT_STRANDS.contains(getStateCode())) {
                        m_schoolCourseOids = m_rap.getRubricAssessment(getBroker()).getTranscriptRubrics(getBroker())
                                .stream()
                                .map(trr -> trr.getSchoolCourseOid())
                                .filter(Objects::nonNull).collect(Collectors.toSet());
                    } else {
                        m_schoolCourseOids = Collections.EMPTY_SET;
                    }
                }
                return m_schoolCourseOids;
            }

            /**
             * Gets the state code.
             *
             * @return the state code
             */
            String getStateCode() {
                return m_stateCode;
            }

            /**
             * Checks if is header.
             *
             * @return true, if is header
             */
            boolean isHeader() {
                return m_isHeader;
            }

            /**
             * Checks if is synthetic.
             *
             * @return true, if is synthetic
             */
            boolean isSynthetic() {
                return m_isSynthetic;
            }
        }

        public static final String ENGLISH_RELIGION_HEADER_SUBJECT = "REL-10";
        public static final String FRENCH_HEADER_SUBJECT = "FRE-10";
        public static final String FRENCH_LANGUAGE_PROGRAM = "FSL";
        public static final String FRENCH_RELIGION_HEADER_SUBJECT = "REL-20";
        public static final String LANGUAGE_HEADER_SUBJECT = "LAN-10";
        public static final String NATIVE_LANGUAGE_HEADER_SUBJECT = "NLA-10";
        public static final String NATIVE_LANGUAGE_PROGRAM = "NSL";

        private static final String RAP_RATING_ID_X = "x";

        private static final List<String> RBC_COL_HEADER_ESL_ELD = Arrays.asList("ESL/ELD", "PANA", "ALF");
        private static final List<String> RBC_COL_HEADER_EVALUATION =
                Arrays.asList("Evaluation", "Évaluation", "Ã‰valuation");
        private static final List<String> RBC_COL_HEADER_IEP = Arrays.asList("IEP", "PEI");
        private static final List<String> RBC_COL_HEADER_NA = Arrays.asList("NA", "S.O.");

        private static final List<String> REQUIRED_ENGLISH_STRAND_CODES =
                Arrays.asList("ART-20", "ART-21", "ART-22", "ART-23", "ART-24", "FRE-10", "LAN-10", "NLA-10", "OPT-30");
        private static final List<String> REQUIRED_FRENCH_STRAND_CODES =
                Arrays.asList("ANG-20", "ART-30", "ART-31", "ART-32", "ART-33", "ART-34", "NLA-20");
        private static final List<String> REQUIRED_ENGLISH_STRAND_CODES_7OR8 =
                Arrays.asList("ART-20", "ART-21", "ART-22", "ART-23", "ART-24", "FRE-10", "LAN-10", "NLA-10", "OPT-30",
                        "GEO-20", "HIS-20");
        private static final List<String> REQUIRED_FRENCH_STRAND_CODES_7OR8 =
                Arrays.asList("ANG-20", "ART-30", "ART-31", "ART-32", "ART-33", "ART-34", "NLA-20", "GEO-30", "HIS-30");
        private static final List<String> REQUIRED_STRANDS_WITHOUT_NA = Arrays.asList("ART-20", "ART-30");

        private static final List<String> VALID_FRENCH_FLAG_STRAND_CODES = Arrays.asList("ART-21", "ART-22", "ART-23",
                "ART-24", "GEO-20", "HIS-20", "HPE-21", "HPE-22", "MAT-20", "OPT-30", "SAT-20", "SOC-20");

        private static List<String> VALUE_GRADE_REQUIRE_MARK_ABOVE_49 = Arrays.asList("07", "08");
        private static List<String> VALID_RELIGION_STRANDS = Arrays.asList("REL-10", "REL-20");
        private static List<String> VALID_SUBJECT_STRANDS = Arrays.asList("OPT-30", "OPT-40");

        private Set<String> m_iepFlagStrandCodes;
        private boolean m_isFrenchSchool = false;
        private Set<String> m_naFlagStrandCodes;
        private ArrayList<Strand> m_sortedStrandCodes;
        private List<OnRubricAssessmentPerformance> m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand;

        /**
         * Gets the entity name.
         *
         * @return the entity name
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            return getSubjectStrandCode();
        }

        /**
         * Elementary rubrics currently include a criteria for French, and in Trillium it is
         * populated for each student/subject area.
         *
         * In Aspen, we'll Discontinue (disable) the French criteria on each elementary subject area
         * rubric and instead the mark report will retrieve the value from one of several places:
         *
         * Course Language of Instruction / all-crs-LanguageOfInstruction
         *
         * Schedule Master:ï¿½all-mst-LanguageOfInstruction
         *
         * Student Transcript:ï¿½all-trn-LanguageOfInstructionOverrideï¿½
         *
         * French will be flagged on the mark report if course language is French and there are no
         * overrides, or if there is an override that = French.
         *
         * @return the french flag
         */
        public Boolean getFrenchFlag() {
            Boolean result = null;
            if (!m_isFrenchSchool) {
                String strandCode = getSubjectStrandCode();
                if (VALID_FRENCH_FLAG_STRAND_CODES.contains(strandCode)
                        && !VALID_RELIGION_STRANDS.contains(strandCode)) {
                    boolean isFrench = getRubricAssessmentPerformanceForSchoolStudentTermStrand().stream()
                            .map(rap -> rap.getRubricAssessment(getBroker()))
                            .flatMap(rba -> rba.getTranscriptRubrics(getBroker()).stream()
                                    .map(trr -> trr.getTranscript(getBroker()))
                                    .map(trn -> (OnTranscript) trn))
                            .anyMatch(trn -> trn.isFrench(getBroker()));
                    if (isFrench) {
                        result = Boolean.TRUE;
                    }
                }
            }
            return result;
        }

        /**
         * IEP_FLAG - populate with the 'T' if an RAP record with rapRatingId = 'x' and
         * relRapRbcOid.rbcColumnHeader = 'IEP' exists
         *
         * @return the iep flag
         */
        public Boolean getIepFlag() {
            /*
             * S-58339 If NA_FLAG is T, don't export IEP_FLAG
             */
            Boolean isNA = getNaFlag();
            if (Boolean.TRUE.equals(isNA)) {
                return null;
            }

            /*
             * S-59639: IEP_FLAG must be null for Religion strands
             */
            String subjectStrandCode = getSubjectStrandCode();
            if (VALID_RELIGION_STRANDS.contains(subjectStrandCode)) {
                return null;
            }

            boolean isRapsWithXandIEP =
                    getRubricAssessmentPerformanceForSchoolStudentTermStrand().stream().anyMatch(rap -> {
                        if (!RAP_RATING_ID_X.equals(rap.getId())) {
                            return false;
                        }

                        ToolRubricCriterion rubricCriterion = rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        if (RBC_COL_HEADER_IEP.contains(rubricCriterion.getColumnHeader())) {
                            return true;
                        }

                        return false;
                    });

            String strandCode = getSubjectStrandCode();
            if (!isRapsWithXandIEP && m_iepFlagStrandCodes.contains(strandCode)) {
                return Boolean.FALSE;
            }

            return !isRapsWithXandIEP ? null : Boolean.TRUE;
        }

        /**
         * NA_FLAG - populate with the 'T' if an RAP record with rapRatingId = 'x' and
         * relRapRbcOid.rbcColumnHeader = 'NA' exists
         *
         * Determine NA_FLAG:
         * 1. If exists on first level (Subject) criteria, required under header/Subject only
         * 2. If exists under specific criteria, required under child strand only
         * 3. Otherwise do not send NA (does not exist in setup)
         *
         * @return Boolean
         */
        public Boolean getNaFlag() {
            if (getSubjectStrand().isSynthetic()) {
                return Boolean.TRUE;
            }

            String strandCode = getSubjectStrandCode();
            if (VALID_RELIGION_STRANDS.contains(strandCode)) {
                return null;
            }

            boolean isRapsWithHeaderNAX =
                    getRubricAssessmentPerformanceForSchoolStudentTermStrand().stream().anyMatch(rap -> {
                        if (!RAP_RATING_ID_X.equals(rap.getId())) {
                            return false;
                        }

                        ToolRubricCriterion rubricCriterion = rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        if (RBC_COL_HEADER_NA.contains(rubricCriterion.getColumnHeader())) {
                            return true;
                        }

                        return false;
                    });

            if (!isRapsWithHeaderNAX && m_naFlagStrandCodes.contains(strandCode)) {
                return Boolean.FALSE;
            }

            return !isRapsWithHeaderNAX ? null : Boolean.TRUE;
        }

        /**
         * MARK - populate with rapRatingId if a RAP record
         * with relRapRbcOid.rbcColumnHeader = 'Evaluation' exists
         *
         * @return the mark
         */
        public String getMark() {
            String subjectStrandCode = getSubjectStrandCode();
            if (ENGLISH_RELIGION_HEADER_SUBJECT.equals(subjectStrandCode)
                    || (FRENCH_RELIGION_HEADER_SUBJECT.equals(subjectStrandCode) && getSubjectStrand().isSynthetic())) {
                return "T";
            }

            Collection<OnRubricAssessmentPerformance> rapBeans =
                    getRubricAssessmentPerformanceForSchoolStudentTermStrand().stream().filter(rap -> {
                        OnRubricCriterion rubricCriterion =
                                (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        if (RBC_COL_HEADER_EVALUATION.contains(rubricCriterion.getColumnHeader())) {
                            return true;
                        }

                        return false;
                    }).collect(Collectors.toList());

            for (OnRubricAssessmentPerformance rubricAssessmentPerformance : rapBeans) {
                String markEvaluation = rubricAssessmentPerformance.getId();
                String transcriptGradeLevel = rubricAssessmentPerformance.getTranscriptGradeLevel();

                if (VALUE_GRADE_REQUIRE_MARK_ABOVE_49.contains(transcriptGradeLevel)) {
                    if (StringUtils.isNumeric(markEvaluation)) {
                        Float mark = Float.valueOf(markEvaluation);
                        if (mark.doubleValue() < 50.0) {
                            return "R";
                        }
                    }
                }

                return markEvaluation;
            }

            return EMPTY_STRING;
        }


        /**
         * Gets the report data.
         *
         * @return the report data
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisReportCardSubjectStrand getReportData() {
            return (OnsisReportCardSubjectStrand) super.getReportData();
        }

        /**
         * Gets the rubric assessment performance for school student term strand.
         *
         * @return the rubric assessment performance for school student term strand
         */
        public Collection<OnRubricAssessmentPerformance> getRubricAssessmentPerformanceForSchoolStudentTermStrand() {
            Strand keyValuePair = getSubjectStrand();

            return keyValuePair.getRubricAssessmentPerformances();
        }

        /**
         * Find student SLP record for the current school and report the [pgm-slp-code].
         *
         * @return [pgm-slp-code] from the StudentProgramParticipation
         */
        public String getSlProgramCode() {
            if ("T".equals(getFieldValue(FIELD_NA_FLAG))) {
                return null;
            }
            String filterByType = getSlpType();
            if (filterByType == null) {
                return null;
            }

            String strandCode = getSubjectStrandCode();
            if (VALID_RELIGION_STRANDS.contains(strandCode)) {
                return EMPTY_STRING;
            }

            OnsisReportCardTermEntity parentEntity = getReportData().getParentEntity();
            OnsisStudent student = parentEntity.getStudent();
            ToolGradeTermDate gradeTermDate = parentEntity.getGradeTermDate();
            Range<Date> dateRange = Range.of(gradeTermDate.getStartDate(), gradeTermDate.getEndDate());
            getReportData().log("OnsisReportCardSubjectStrandEntity.getSlProgramCode-dateRange: " + dateRange);

            OnStudentSLPProgram slpProgram = student.getSlpPrograms(getBroker())
                    .stream()
                    .filter(pgm -> {
                        String slpType = pgm.getProgramType();
                        if (!filterByType.equals(slpType)) {
                            Range<Date> pgmDateRange = Range.of(pgm.getStartDate(), pgm.getEndDate());
                            boolean retValue = dateRange.isOverlap(pgmDateRange);
                            getReportData()
                                    .log("OnsisReportCardSubjectStrandEntity.getSlProgramCode-NOT candidate dateRange:["
                                            + pgmDateRange + "] Code:[" + pgm.getProgramCode() + "] Result:[" + retValue
                                            + "]");
                            return false;
                        }
                        Range<Date> pgmDateRange = Range.of(pgm.getStartDate(), pgm.getEndDate());
                        boolean retValue = dateRange.isOverlap(pgmDateRange);
                        getReportData().log("OnsisReportCardSubjectStrandEntity.getSlProgramCode-candidate dateRange:["
                                + pgmDateRange + "] Code:[" + pgm.getProgramCode() + "] Result:[" + retValue + "]");
                        return retValue;
                    })
                    .sorted(new Comparator<OnStudentSLPProgram>() {

                        @Override
                        public int compare(OnStudentSLPProgram pgm0, OnStudentSLPProgram pgm1) {
                            int numDays0 = numDays(pgm0);
                            int numDays1 = numDays(pgm1);
                            int result = numDays1 - numDays0;
                            getReportData().log("OnsisReportCardSubjectStrandEntity.getSlProgramCode-compare numDays0:["
                                    + numDays0 + "] PGM:[" + pgm0 + "] numDays1:["
                                    + numDays1 + "] PGM:[" + pgm1 + "] Result:[" + result + "]");
                            return result;
                        }

                        private int numDays(OnStudentSLPProgram pgm) {
                            Range<Date> dates = dateRange.intersection(Range.of(pgm.getStartDate(), pgm.getEndDate()));
                            if (dates.getStart() == null || dates.getEnd() == null) {
                                return Integer.MAX_VALUE;
                            }
                            return (int) TimeUnit.DAYS.convert(dates.getEnd().getTime() - dates.getStart().getTime(),
                                    TimeUnit.MILLISECONDS);
                        }
                    }).findFirst().orElse(null);

            getReportData()
                    .log("OnsisReportCardSubjectStrandEntity.getSlProgramCode-selected program:[" + slpProgram + "]");

            return slpProgram == null ? null
                    : getDictionaryExtractor().getSifValue(slpProgram, OnStudentSLPProgram.FIELD_PROGRAM_CODE);
        }

        /**
         * SUBJECT_NAME - populate with
         * relRapRbaOid.relRbaTrrOid.relTrrTrnOid.relTrnCskOid.cskCourseNum from the RAP record
         * where relRapRbcOid.rbcColumnHeader = 'Evaluation'
         * only if SUBJECT_STRAND IN {OPT-30, OPT-40}
         *
         * @return the subject name
         */
        public String getSubjectName() {
            String strandCode = getSubjectStrandCode();
            if (!VALID_SUBJECT_STRANDS.contains(strandCode)) {
                return EMPTY_STRING;
            }

            if (VALID_RELIGION_STRANDS.contains(strandCode)) {
                return EMPTY_STRING;
            }

            Collection<OnRubricAssessmentPerformance> rapBeans =
                    getRubricAssessmentPerformanceForSchoolStudentTermStrand().stream().filter(rap -> {
                        ToolRubricCriterion rubricCriterion = rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        if (RBC_COL_HEADER_EVALUATION.contains(rubricCriterion.getColumnHeader())) {
                            return true;
                        }

                        return false;
                    }).collect(Collectors.toList());

            Set<String> subjectName = new TreeSet<>();
            for (OnRubricAssessmentPerformance rapBean : rapBeans) {
                ToolRubricAssessment rubricAssessment = rapBean.getRubricAssessment(getBroker());
                if (rubricAssessment == null) {
                    continue;
                }

                Collection<ToolTranscriptRubric> transcriptRubrics = rubricAssessment.getTranscriptRubrics(getBroker());

                transcriptRubrics.stream()
                        .map(trr -> trr.getCourseDescription())
                        .collect(Collectors.toSet());

                // Collect all the SchoolCourse Numbers
                subjectName.addAll(transcriptRubrics.stream()
                        .map(trr -> trr.getCourseDescription())
                        .collect(Collectors.toSet()));
            }

            return StringUtils.convertCollectionToDelimitedString(subjectName, ',');
        }

        /**
         * Gets the subject strand code.
         *
         * @return the subject strand code
         */
        public String getSubjectStrandCode() {
            return getSubjectStrand().getStateCode();
        }

        /**
         * Gets the subject strand.
         *
         * @return the subject strand
         */
        public Strand getSubjectStrand() {
            return m_sortedStrandCodes.get(getCurrentRow());
        }

        /**
         * SUPPORT_PROGRAM_CODE - populate with 'ESL/ELD' if a RAP record with rapRatingId = 'x'
         * and relRapRbcOid.rbcColumnHeader = 'ESL/ELD' exists
         *
         * @return the support program code
         */
        public String getSupportProgramCode() {
            String strandCode = getSubjectStrandCode();
            if (VALID_RELIGION_STRANDS.contains(strandCode)) {
                return EMPTY_STRING;
            }
            Optional<OnRubricAssessmentPerformance> isRapsWithProperHeaderX =
                    getRubricAssessmentPerformanceForSchoolStudentTermStrand().stream().filter(rap -> {
                        if (!RAP_RATING_ID_X.equals(rap.getId())) {
                            return false;
                        }

                        ToolRubricCriterion rubricCriterion = rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        if (RBC_COL_HEADER_ESL_ELD.contains(rubricCriterion.getColumnHeader())) {
                            return true;
                        }

                        return false;
                    })
                            .findAny();
            String value = EMPTY_STRING;
            if (isRapsWithProperHeaderX.isPresent()) {
                value = isRapsWithProperHeaderX.get().getRubricCriterion(getBroker()).getColumnHeader();
            }
            if ("PANA".equals(value)) {
                value = "PDF";
            }

            return value;
        }

        /**
         * Intitialize.
         *
         * @param data the data
         * @param bean the bean
         * @throws X2BaseException the x 2 base exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisReportCardSubjectStrand reportData = getReportData();
            OnsisReportCardTermEntity parentEntity = reportData.getParentEntity();
            OnsisReportCardEntity reportCardEntity = parentEntity.getReportData().getParentEntity();
            OnsisStudentEnrollmentEntity studentEnrollmentEntity = reportCardEntity.getReportData().getParentEntity();

            m_isFrenchSchool = OnsisConstants.VALUE_LANGUAGE_TYPE_FRENCH
                    .equals(reportData.getGlobalData().getSchool().getLanguageType());

            /*
             * If the TERM's Alt Report Card Flag is T,
             * then no Subject Strands or Learning Skills should publish
             */
            boolean isAltReportCard = FLAG_T.equals(parentEntity.getFieldValue(FIELD_NAME_ALT_REPORT_CARD));
            if (isAltReportCard) {
                setRowCount(0);
                return;
            }

            boolean isGrade7or8 = Arrays.asList("7", "8")
                    .contains(studentEnrollmentEntity.getFieldValue(OnsisStudentSchoolEnrollment.FIELD_GRADE_LEVEL));

            OnsisStudent student = parentEntity.getStudent();
            ToolGradeTermDate gradeTerm = parentEntity.getGradeTermDate();

            if (getGlobalData().getDebugDetail()) {
                StringBuilder debugOutput = new StringBuilder();
                student.getTranscriptRubrics(getBroker()).stream().forEach(trr -> {
                    debugOutput.append(trr + "\n");
                    debugOutput.append(trr.getTranscript(getBroker()) + "\n");
                    if (trr.getRubricAssessment(getBroker()) != null) {
                        trr.getRubricAssessment(getBroker()).getRubricAssessmentPerformances(getBroker()).stream()
                                .forEach(rap -> {
                                    debugOutput.append(rap + "\n");
                                });
                    }
                    debugOutput.append("\n");
                });
                getReportData().log("transcript detail: \n" + debugOutput.toString());
            }

            m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand =
                    student.getRubricAssessmentPerformance(getBroker(), gradeTerm.getGradeTermId()).stream()
                            .filter(rap -> {
                                OnRubricCriterion rubricCriterion =
                                        (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                                if (rubricCriterion == null) {
                                    return false;
                                }

                                String stateValue = rubricCriterion.getSubjectStrand();
                                return !StringUtils.isBlank(stateValue);
                            })
                            .map(rap -> (OnRubricAssessmentPerformance) rap)
                            .collect(Collectors.toList());

            Set<Strand> uniqueStrands = Stream.concat(
                    // Headers first
                    m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand.stream()
                            .filter(rap -> {
                                OnRubricCriterion rubricCriterion =
                                        (OnRubricCriterion) rap.getRubricCriterion(getBroker());

                                ToolRubricDefinition rubricDefinition =
                                        rubricCriterion.getRubricDefinition(getBroker());
                                String stateCode = rubricDefinition.getSubjectCode();
                                if (StringUtils.isBlank(stateCode)) {
                                    return false;
                                }
                                if (VALID_SUBJECT_STRANDS.contains(stateCode)
                                        && rap.getRubricAssessment(getBroker()) == null) {
                                    return false;
                                }
                                return true;
                            }).map(rap -> {
                                String stateCode = rap.getRubricCriterion(getBroker()).getRubricDefinition(getBroker())
                                        .getSubjectCode();
                                return new Strand(stateCode, rap, true);
                            }),
                    // Followed by criterion strands
                    m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand.stream()
                            .filter(rap -> {
                                OnRubricCriterion rubricCriterion =
                                        (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                                String stateCode = rubricCriterion.getSubjectStrand();
                                if (StringUtils.isBlank(stateCode)) {
                                    return false;
                                }
                                if (!VALID_SUBJECT_STRANDS.contains(stateCode)) {
                                    return true;
                                }
                                if (rap.getRubricAssessment(getBroker()) == null) {
                                    return false;
                                }
                                return true;
                            }).map(rap -> {
                                OnRubricCriterion rubricCriterion =
                                        (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                                String stateCode = rubricCriterion.getSubjectStrand();
                                return new Strand(stateCode, rap, false);
                            }))
                    .collect(Collectors.toSet());

            // Collections.sort(m_sortedStrandCodes);
            m_sortedStrandCodes = new ArrayList(uniqueStrands);

            /*
             * if a strand FRE-10 exists and LAN-10 does not exist generate and LAN-10 strand with
             * NA flag = true - S-59860
             */
            Set<String> existingStrandCodes =
                    m_sortedStrandCodes.stream().map(Strand::getStateCode).collect(Collectors.toSet());
            if (getGlobalData().getDebugDetail()) {
                getReportData().log("existingStrandCodes: " + existingStrandCodes.toString());
            }
            if (!existingStrandCodes.isEmpty()) {
                List<String> requiredCodes =
                        m_isFrenchSchool ? REQUIRED_FRENCH_STRAND_CODES : REQUIRED_ENGLISH_STRAND_CODES;
                if (isGrade7or8) {
                    requiredCodes =
                            m_isFrenchSchool ? REQUIRED_FRENCH_STRAND_CODES_7OR8 : REQUIRED_ENGLISH_STRAND_CODES_7OR8;
                }
                if (getGlobalData().getDebugDetail()) {
                    getReportData().log("requiredCodes: " + requiredCodes.toString());
                }
                for (String requiredCode : requiredCodes) {
                    if (!existingStrandCodes.contains(requiredCode)) {
                        m_sortedStrandCodes.add(new Strand(requiredCode, Collections.EMPTY_SET,
                                !REQUIRED_STRANDS_WITHOUT_NA.contains(requiredCode)));
                        if (getGlobalData().getDebugDetail()) {
                            getReportData().log("add synthetic " + requiredCode);
                        }
                    }
                }
            }

            Boolean isCatholic = reportData.getGlobalData().getOrganizationToolBean().isCatholic(getBroker());

            if (!existingStrandCodes.isEmpty() && isCatholic && m_isFrenchSchool
                    && !existingStrandCodes.contains(FRENCH_RELIGION_HEADER_SUBJECT)) {
                m_sortedStrandCodes
                        .add(new Strand(FRENCH_RELIGION_HEADER_SUBJECT, Collections.EMPTY_SET, false));
                if (getGlobalData().getDebugDetail()) {
                    getReportData().log("add synthetic " + FRENCH_RELIGION_HEADER_SUBJECT);
                }
            }

            if (!existingStrandCodes.isEmpty() && isCatholic && !m_isFrenchSchool
                    && !existingStrandCodes.contains(ENGLISH_RELIGION_HEADER_SUBJECT)) {
                m_sortedStrandCodes
                        .add(new Strand(ENGLISH_RELIGION_HEADER_SUBJECT, Collections.EMPTY_SET, false));
                if (getGlobalData().getDebugDetail()) {
                    getReportData().log("add synthetic " + ENGLISH_RELIGION_HEADER_SUBJECT);
                }
            }

            Collections.sort(m_sortedStrandCodes,
                    (pair1, pair2) -> pair1.getStateCode().compareTo(pair2.getStateCode()));

            if (getGlobalData().getDebugDetail()) {
                getReportData().log("m_sortedStrandCodes-sorted: " + m_sortedStrandCodes.toString());
            }

            setRowCount(m_sortedStrandCodes.size());

            if (m_sortedStrandCodes.size() > 0) {
                initializeIepFlagStrandCodes();
                initializeNaFlagStrandCodes();
            }
        }

        /**
         * Map subject header to program type.
         *
         * @return SLP Type
         */
        private String getSlpType() {
            switch (getSubjectStrandCode()) {
                case FRENCH_HEADER_SUBJECT:
                    return FRENCH_LANGUAGE_PROGRAM;
                case NATIVE_LANGUAGE_HEADER_SUBJECT:
                    return NATIVE_LANGUAGE_PROGRAM;
                default:
                    return null;
            }
        }

        /**
         * Initialize iep flag strand codes.
         */
        private void initializeIepFlagStrandCodes() {
            Stream<String> iepFlagChildStrandCodes = m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand
                    .stream()
                    .filter(rap -> {
                        OnRubricCriterion rubricCriterion =
                                (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        ToolRubricCriterion rootCriterion = rubricCriterion.getRootCriterion(getBroker());
                        if (rubricCriterion.getOid().equals(rootCriterion.getOid())) {
                            return false;
                        }

                        Collection<ToolRubricCriterion> siblingCriteria = rootCriterion.getRubricCriteria(getBroker());
                        for (ToolRubricCriterion siblingCriterion : siblingCriteria) {
                            if (RBC_COL_HEADER_IEP.contains(siblingCriterion.getColumnHeader())) {
                                return true;
                            }
                        }

                        return false;
                    })
                    .map(rap -> rap.getRubricCriterion(getBroker()))
                    .map(rbc -> (OnRubricCriterion) rbc)
                    .map(rbc -> rbc.getSubjectStrand());

            Stream<String> iepFlagHeaderStrandCodes = m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand
                    .stream()
                    .filter(rap -> {
                        OnRubricCriterion rubricCriterion =
                                (OnRubricCriterion) rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        ToolRubricDefinition rubricDefinition = rubricCriterion.getRubricDefinition(getBroker());
                        Collection<ToolRubricCriterion> rubricCriteria =
                                rubricDefinition.getRubricCriteria(getBroker());
                        for (ToolRubricCriterion childCriterion : rubricCriteria) {
                            ToolRubricCriterion rootCriterion = childCriterion.getRootCriterion(getBroker());
                            if (!childCriterion.getOid().equals(rootCriterion.getOid())) {
                                continue;
                            }

                            String stateValue = ((OnRubricCriterion) childCriterion).getSubjectStrand();

                            if (StringUtils.isBlank(stateValue)) {
                                continue;
                            }

                            if (RBC_COL_HEADER_IEP.contains(childCriterion.getColumnHeader())) {
                                return true;
                            }
                        }

                        return false;
                    })
                    .map(rap -> rap.getRubricCriterion(getBroker()))
                    .map(rbc -> (OnRubricCriterion) rbc)
                    .map(rbc -> rbc.getSubjectStrand());

            m_iepFlagStrandCodes = Stream
                    .concat(iepFlagChildStrandCodes, iepFlagHeaderStrandCodes)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
        }

        /**
         * Initialize na flag strand codes.
         */
        private void initializeNaFlagStrandCodes() {
            Set<String> naFlagChildStrandCodes = m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand
                    .stream()
                    .filter(rap -> {
                        ToolRubricCriterion rubricCriterion = rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        ToolRubricCriterion rootCriterion = rubricCriterion.getRootCriterion(getBroker());
                        if (rubricCriterion.getOid().equals(rootCriterion.getOid())) {
                            return false;
                        }

                        Collection<ToolRubricCriterion> siblingCriteria = rootCriterion.getRubricCriteria(getBroker());
                        for (ToolRubricCriterion siblingCriterion : siblingCriteria) {
                            if (RBC_COL_HEADER_NA.contains(siblingCriterion.getColumnHeader())) {
                                return true;
                            }
                        }

                        return false;
                    })
                    .map(rap -> rap.getRubricCriterion(getBroker()))
                    .map(rbc -> (OnRubricCriterion) rbc)
                    .map(rbc -> rbc.getSubjectStrand())
                    .collect(Collectors.toSet());

            Set<String> naFlagHeaderStrandCodes = m_rubricAssessmentPerformanceForStudentAndTermWithValidStrand
                    .stream()
                    .filter(rap -> {
                        ToolRubricCriterion rubricCriterion = rap.getRubricCriterion(getBroker());
                        if (rubricCriterion == null) {
                            return false;
                        }

                        ToolRubricDefinition rubricDefinition = rubricCriterion.getRubricDefinition(getBroker());
                        Collection<ToolRubricCriterion> rubricCriteria =
                                rubricDefinition.getRubricCriteria(getBroker());
                        for (ToolRubricCriterion childCriterion : rubricCriteria) {
                            ToolRubricCriterion rootCriterion = childCriterion.getRootCriterion(getBroker());
                            if (!childCriterion.getOid().equals(rootCriterion.getOid())) {
                                continue;
                            }

                            String stateValue = ((OnRubricCriterion) childCriterion).getSubjectStrand();

                            if (StringUtils.isBlank(stateValue)) {
                                continue;
                            }

                            if (RBC_COL_HEADER_NA.contains(childCriterion.getColumnHeader())) {
                                return true;
                            }
                        }

                        return false;
                    })
                    .map(rap -> rap.getRubricCriterion(getBroker()).getRubricDefinition(getBroker()))
                    .map(rbd -> rbd.getSubjectCode())
                    .collect(Collectors.toSet());

            m_naFlagStrandCodes = Stream
                    .concat(naFlagChildStrandCodes.stream(), naFlagHeaderStrandCodes.stream())
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());
        }
    }

    public static final String FIELD_NA_FLAG = "NaFlag";

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getParentEntity().getBean()));
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();

        calcs.put(OnsisRetrieverAction.CALC_ID, new OnsisRetrieverAction());

        return calcs;
    }

    /**
     * Gets the parent entity.
     *
     * @return the parent entity
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisReportCardTermEntity getParentEntity() {
        return (OnsisReportCardTermEntity) super.getParentEntity();
    }

    /**
     * Gets the parent report data.
     *
     * @return the parent report data
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentReportData()
     */
    @Override
    public OnsisReportCardTerm getParentReportData() {
        return (OnsisReportCardTerm) super.getParentReportData();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisReportCardSubjectStrandEntity.class);
    }
}
