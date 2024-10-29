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

import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchoolCalendar;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentProgramParticipation;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentELLProgram;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSLPProgram;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSLPProgramFrench;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class OnsisSecondLanguageProgram.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisSecondLanguageProgram extends OnsisStateReportData {

    /**
     * The Class OnsisSecondLanguageProgramEntity.
     */
    public static class OnsisSecondLanguageProgramEntity extends OnsisStateReportEntity {
        private List<ToolStudentProgramParticipation> m_programs;

        /**
         * Gets the minutes of instruction per day.
         *
         * @return Big decimal
         */
        public BigDecimal getMinutesOfInstructionPerDay() {
            BigDecimal value = null;

            ToolStudentProgramParticipation pgm = getProgram();
            if (pgm instanceof OnStudentELLProgram) {
                value = BigDecimal.ZERO;
            } else if (pgm instanceof OnStudentSLPProgramFrench) {
                value = ((OnStudentSLPProgramFrench) pgm).getMinutesOfInstruction();
                if (value != null && BIG_DECIMAL_ONE_HALF.compareTo(value) > 0) {
                    value = null;
                }
            } else if (pgm instanceof OnStudentSLPProgram) {
                value = ((OnStudentSLPProgram) pgm).getMinutesOfInstruction();
                if (value != null && BIG_DECIMAL_ONE_HALF.compareTo(value) > 0) {
                    value = null;
                }
            }
            return value;
        }

        /**
         * Gets the program.
         *
         * @return Student program participation
         */
        public ToolStudentProgramParticipation getProgram() {
            return m_programs.get(getCurrentRow());
        }

        /**
         * For Secondary OnSIS submissions ONLY
         * - if second language program type = Native Second Language and
         * second language code = OnSIS code (020, 021, 022, 023, 024,025, 026)
         * then SECOND_LANGUAGE_PROGRAM/TYPE must equal '027'.
         * Else, getProgram;[pgm-slp-code]#STATE@STD-PGM-SLP
         *
         * @return String
         */
        public String getType() {
            String typeCode = null;
            ToolStudentProgramParticipation pgm = getProgram();
            if (pgm instanceof OnStudentELLProgram) {
                typeCode = ((OnStudentELLProgram) pgm).getProgramCode();
            } else if (pgm instanceof OnStudentSLPProgramFrench) {
                typeCode = ((OnStudentSLPProgramFrench) pgm).getProgramType();
            } else if (pgm instanceof OnStudentSLPProgram) {
                typeCode = ((OnStudentSLPProgram) pgm).getProgramCode();
                boolean isSecondary =
                        getGlobalData().getSchoolType()
                                .equals(OnsisStateReportData.SubmissionSchoolType.PUBLIC_SECONDARY);
                if (isSecondary) {
                    if (NL_CODES.contains(typeCode)) {
                        return CODE_NATIVE_LANGUAGE;
                    }
                }
            }

            return typeCode;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            OnsisStateReportEntity parentEntity = getReportData().getParentEntity();
            OnsisAnnualSpan span = null;
            if (parentEntity instanceof OnsisStudentEnrollmentEntity) {
                span = ((OnsisStudentEnrollmentEntity) parentEntity).getSpan();
            }

            /*
             * LP should be reported in Main & Secondary Schools for public elementary submission
             * LP should be reported for Main for public secondary schools
             * LP should be reported for Secondary School for public secondary schools if
             * the student is enrolled in a course
             * with a Ministry Defined ID beginning with FS, FI, FE
             * or course language of instruction = F.
             */

            /*
             * If this IS a Secondary Submission:
             * setRowCount(0) if a StudentSchedule span at some OTHER school
             * has a Course with the French prefixes etc.
             */

            boolean isSecondarySubmission =
                    getGlobalData().getSchoolType().equals(OnsisStateReportData.SubmissionSchoolType.PUBLIC_SECONDARY);

            PlainDate countDate = getCountDate(span);
            if (parentEntity instanceof OnsisStudentEnrollmentEntity) {
                PlainDate endDate = ((OnsisStudentEnrollmentEntity) parentEntity).getOnsisEnrollmentEndDate();
                if (endDate != null && endDate.before(countDate)) {
                    countDate = DateUtils.add(endDate, -1);
                    if (span != null && span.getLastActiveInSessionDate() != null
                            && span.getLastActiveInSessionDate().before(countDate)) {
                        countDate = span.getLastActiveInSessionDate();
                    }
                }
            }

            boolean isSecondarySchool = span == null ? false : span.isSecondary();
            m_programs = Stream
                    .concat(Stream.concat(getSlpFrPrograms(isSecondarySubmission, isSecondarySchool, countDate),
                            getSlpPrograms(isSecondarySubmission, isSecondarySchool, countDate)),
                            getEllPrograms(isSecondarySubmission, isSecondarySchool, countDate))
                    .collect(Collectors.toList());
            setRowCount(m_programs.size());
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            if (debugOutput != null) {
                debugOutput.append("OnsisSecondLanguageProgramEntity.intitialize: \n");
                debugOutput.append(m_programs.stream().map(pgm -> pgm.toString()).collect(Collectors.joining("\n")));
                getReportData().log(debugOutput.toString());
            }
        }

        /**
         * Determine count date.
         *
         * Note: June Submission will return latest in session date from school calendar.
         *
         * @param span AnnualSpan
         * @return Plain date
         */
        private PlainDate getCountDate(OnsisAnnualSpan span) {
            PlainDate countDate = getGlobalData().getEndDate();
            if (span == null) {
                return countDate;
            }

            if (getGlobalData().getSubmissionType().isJuneSubmission() && span != null) {
                boolean forward = false;
                ToolSchoolCalendar schoolCalendar = span.getSchoolCalendar();
                if (schoolCalendar != null) {
                    countDate = schoolCalendar.findFirstInSessionDate(getBroker(), countDate, forward);
                }
            }

            return countDate;
        }

        /**
         * Gets the ell programs.
         *
         * @param isSecondarySubmission boolean
         * @param isSecondarySchool boolean
         * @param countDate PlainDate
         * @return Stream
         */
        private Stream<? extends ToolStudentProgramParticipation> getEllPrograms(boolean isSecondarySubmission,
                                                                                 boolean isSecondarySchool,
                                                                                 PlainDate countDate) {
            Stream<OnStudentELLProgram> stream = Stream.empty();
            OnsisStudent student = (OnsisStudent) getBean();

            if (!isSecondarySubmission || !isSecondarySchool
                    || !(getReportData().getParentEntity() instanceof OnsisStudentEnrollmentEntity)
                    || hasCourseWithPrefix(null, ELL_PREFIXES)) {
                stream = student.getEllPrograms(getBroker()).stream()
                        .filter(pgm -> !StringUtils.isEmpty(pgm.getProgramCode())
                                && pgm.getDateRange().contains(countDate))
                        .sorted(new Comparator<ToolStudentProgramParticipation>() {
                            @Override
                            public int compare(ToolStudentProgramParticipation o1, ToolStudentProgramParticipation o2) {
                                return o1.getOid().compareTo(o2.getOid());
                            }
                        });
            }
            return stream;
        }

        /**
         * Gets the slp programs.
         *
         * @param isSecondarySubmission boolean
         * @param isSecondarySchool boolean
         * @param countDate PlainDate
         * @return Stream
         */
        private Stream<? extends ToolStudentProgramParticipation> getSlpFrPrograms(boolean isSecondarySubmission,
                                                                                   boolean isSecondarySchool,
                                                                                   PlainDate countDate) {
            Stream<OnStudentSLPProgramFrench> stream = Stream.empty();
            OnsisStudent student = (OnsisStudent) getBean();

            if (!isSecondarySubmission || !isSecondarySchool
                    || !(getReportData().getParentEntity() instanceof OnsisStudentEnrollmentEntity)) {
                stream = student.getSlpFrenchPrograms(getBroker()).stream()
                        .filter(pgm -> !StringUtils.isEmpty(pgm.getProgramType())
                                && pgm.getDateRange().contains(countDate))
                        .sorted(new Comparator<ToolStudentProgramParticipation>() {
                            @Override
                            public int compare(ToolStudentProgramParticipation o1, ToolStudentProgramParticipation o2) {
                                return o1.getOid().compareTo(o2.getOid());
                            }
                        });
            }
            return stream;
        }

        /**
         * Gets the slp programs.
         *
         * @param isSecondarySubmission boolean
         * @param isSecondarySchool boolean
         * @param countDate PlainDate
         * @return Stream
         */
        private Stream<? extends ToolStudentProgramParticipation> getSlpPrograms(boolean isSecondarySubmission,
                                                                                 boolean isSecondarySchool,
                                                                                 PlainDate countDate) {
            StringBuilder debugOutput = getGlobalData().getDebugDetail() ? new StringBuilder() : null;
            if (debugOutput != null) {
                debugOutput.append("getSlpPrograms: [" + isSecondarySubmission + ", " + isSecondarySchool + ", "
                        + countDate + "]\n");
            }

            Stream<OnStudentSLPProgram> stream = Stream.empty();
            OnsisStudent student = (OnsisStudent) getBean();

            if (!isSecondarySubmission || !isSecondarySchool
                    || !(getReportData().getParentEntity() instanceof OnsisStudentEnrollmentEntity)
                    || hasSlpCourseInSchool()) {
                stream = student.getSlpPrograms(getBroker()).stream()
                        .filter(pgm -> !StringUtils.isEmpty(pgm.getProgramCode())
                                && pgm.getDateRange().contains(countDate))
                        .sorted(new Comparator<ToolStudentProgramParticipation>() {
                            @Override
                            public int compare(ToolStudentProgramParticipation o1, ToolStudentProgramParticipation o2) {
                                return o1.getOid().compareTo(o2.getOid());
                            }
                        });
            }
            if (debugOutput != null) {
                getReportData().log(debugOutput.toString());
            }
            return stream;
        }

        /**
         * Checks for course with prefix.
         *
         * @param spans List<StudentScheduleSpan>
         * @param prefixes List<String>
         * @return true, if successful
         */
        private boolean hasCourseWithPrefix(List<OnsisStudentScheduleSpan> spans, List<String> prefixes) {
            if (spans == null) {
                spans = ((OnsisStudent) getBean()).getStudentScheduleSpans(getBroker(), getGlobalData(), null);
            }
            Optional<OnsisStudentScheduleSpan> slpSpan =
                    spans.stream()
                            .filter(scheduleSpan -> {
                                String courseView = scheduleSpan.getSection().getCourseView();
                                return isCourseStartWithPrefix(courseView, prefixes);
                            }).findAny();
            return slpSpan.isPresent();
        }

        /**
         * Checks for slp course in school.
         *
         * @return true, if successful
         */
        private boolean hasSlpCourseInSchool() {
            OnsisStudent student = (OnsisStudent) getBean();
            List<OnsisStudentScheduleSpan> spans = student.getStudentScheduleSpans(getBroker(), getGlobalData(), null);

            boolean hasSlpCourse = hasCourseWithPrefix(spans, SLP_PREFIXES);

            if (!hasSlpCourse) {
                Optional<OnsisStudentScheduleSpan> frenchSegmentSpan =
                        spans.stream().filter(scheduleSpan -> {
                            OnSection testSection = (OnSection) scheduleSpan.getSection();
                            String languageType = testSection.getLanguageOfInstruction();
                            return "F".equals(languageType);
                        }).findAny();
                hasSlpCourse = frenchSegmentSpan.isPresent();
            }
            return hasSlpCourse;
        }

        /**
         * Checks if course starts with provided prefixes or no.
         *
         * @param courseNumber String
         * @param prefixes String[]
         * @return true, if is course start with preffix
         */
        private boolean isCourseStartWithPrefix(String courseNumber, Collection<String> prefixes) {
            if (!StringUtils.isEmpty(courseNumber)) {
                for (String prefix : prefixes) {
                    if (courseNumber.startsWith(prefix)) {
                        return true;
                    }
                }
            }
            return false;
        }


    }

    private static final BigDecimal BIG_DECIMAL_ONE_HALF = new BigDecimal(0.5);
    private static final String CODE_NATIVE_LANGUAGE = "027";
    private static final List<String> ELL_PREFIXES = Arrays.asList("ESL", "ELD");
    private static final String[] FRENCH_PREFIXES = new String[] {"FE", "FI", "FS"};
    private static final String[] NATIVE_PREFIXES = new String[] {"LN"};
    private static List<String> NL_CODES = Arrays.asList("020", "021", "022", "023", "024", "025", "026");
    private static final List<String> SLP_PREFIXES =
            Stream.concat(Arrays.stream(FRENCH_PREFIXES), Arrays.stream(NATIVE_PREFIXES)).collect(Collectors.toList());

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        if (getGlobalData().getSubmissionType().isOctoberSubmission()
                && getParentEntity() instanceof OnsisStudentEnrollmentEntity
                && ((OnsisStudentEnrollmentEntity) getParentEntity()).getSpan() == null) {
            setBeans(Collections.EMPTY_LIST);
        } else {
            setBeans(Arrays.asList(getParentEntity().getBean()));
        }
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
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisSecondLanguageProgramEntity.class);
    }
}
