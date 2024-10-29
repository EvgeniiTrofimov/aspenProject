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

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolClass.ClassWrapper;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The Class OnsisStudentScheduleCommon.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisStudentScheduleCommon extends OnsisStateReportData {
    /**
     * The Class OnsisStudentScheduleCommonEntity.
     */
    //
    public static class OnsisStudentScheduleCommonEntity extends OnsisStateReportEntity {
        /**
         * The Class StudentScheduleSpanSet.
         */
        protected class StudentScheduleSpanSet {
            private OnsisStudentScheduleSpan m_entrySpan;
            private OnsisStudentScheduleSpan m_submissionSpan;
            private List<OnsisStudentScheduleSpan> m_spans;

            /**
             * Instantiates a new student schedule span set.
             *
             * @param item List<StudentScheduleSpan>
             */
            public StudentScheduleSpanSet(List<OnsisStudentScheduleSpan> item) {
                m_spans = item.stream()
                        .sorted(new Comparator<StudentScheduleSpan>() {

                            @Override
                            public int compare(StudentScheduleSpan span1, StudentScheduleSpan span2) {
                                int result = span1.getEntryDate().compareTo(span2.getEntryDate());
                                if (result == 0) {
                                    result = span1.getExitDate().compareTo(span2.getExitDate());
                                }
                                return result;
                            }
                        })
                        .collect(Collectors.toList());
                GlobalData globalData = m_reportData.getGlobalData();

                // calculate entry span - the earliest span
                m_entrySpan = m_spans.iterator().next();

                // calculate submission span
                m_submissionSpan = null;
                for (OnsisStudentScheduleSpan span : m_spans) {
                    if (m_submissionSpan == null) {
                        m_submissionSpan = span;
                    } else if (!span.getEntryDate().after(globalData.getEndDate())) {
                        m_submissionSpan = span;
                    }
                }
            }

            /**
             * Gets the campus number.
             *
             * @return String
             */
            public String getCampusNumber() {
                String value = null;
                if (getGlobalData().getCurrentSchool().size() > 1) {
                    OnSchool school = null;
                    try {
                        school = (OnSchool) getSpan().getSection().getSchedule(getBroker()).getSchool(getBroker());
                    } catch (NullPointerException e) {
                        // ignore null exception
                    }
                    if (school != null) {
                        value = school.getSchoolId();
                    }
                }
                return value;
            }

            /**
             * Gets the entry span.
             *
             * @return Student schedule span
             */
            public OnsisStudentScheduleSpan getEntrySpan() {
                return m_entrySpan;
            }

            /**
             * Gets the submission span.
             *
             * @return Student schedule span
             */
            public OnsisStudentScheduleSpan getSubmissionSpan() {
                return m_submissionSpan;
            }

            /**
             * Gets the spans.
             *
             * @return List
             */
            public List<OnsisStudentScheduleSpan> getSpans() {
                return m_spans;
            }

            /**
             * Includes date.
             *
             * @param date PlainDate
             * @return true, if successful
             */
            public boolean includesDate(PlainDate date) {
                return m_spans.stream()
                        .anyMatch(span -> span.getDateRange().contains(date));
            }
        }


        protected OnsisStudent m_student;
        protected List<StudentScheduleSpanSet> m_studentScheduleSpans = new ArrayList<>();
        private OnsisStudentScheduleCommon m_reportData;

        /**
         * Gets the class entity.
         *
         * @return Class wrapper
         */
        public ClassWrapper getClassEntity() {
            return getClassWrapper(getSpan());
        }

        /**
         * Gets the entry span.
         *
         * @return Student schedule span
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getEntityBean()
         */
        public StudentScheduleSpan getEntrySpan() {
            return m_studentScheduleSpans.get(getCurrentRow()).getEntrySpan();
        }

        /**
         * Gets the span.
         *
         * @return Student schedule span
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getEntityBean()
         */
        public OnsisStudentScheduleSpan getSpan() {
            return m_studentScheduleSpans.get(getCurrentRow()).getSubmissionSpan();
        }

        /**
         * Gets the span.
         *
         * @return Student schedule span
         * @see com.follett.fsc.aspensif.framework.OnsisStateReportData.OnsisStateReportEntity#getEntityBean()
         */
        public StudentScheduleSpanSet getSpanSet() {
            return m_studentScheduleSpans.get(getCurrentRow());
        }

        /**
         * Gets the student.
         *
         * @return Sis student
         */
        public OnsisStudent getStudent() {
            return m_student;
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean ToolBean
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, ToolBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_student = (OnsisStudent) bean;
            m_reportData = (OnsisStudentScheduleCommon) data;
        }

        /**
         * Gets the class wrapper.
         *
         * @param span StudentScheduleSpan
         * @return Class wrapper
         */
        protected ClassWrapper getClassWrapper(StudentScheduleSpan span) {
            OnSection section = (OnSection) span.getSection();
            // cls == null ? section : cls
            // ScheduleClass cls = section.getSectionClass();
            return new ClassWrapper(getGlobalData(), section, section);
        }

        /**
         * Gets the key.
         *
         * 1. Ministry Defined Course Code:
         * 2. InstitutionType (DCC only): null
         * 3. LocallyDevelopedCourse (LDC only): null
         * 4. TermCode
         * 5. SchoolCourse credit: 1.00
         *
         * @param span StudentScheduleSpan
         * @return String
         */
        protected String getKey(StudentScheduleSpan span) {
            StringBuilder result = new StringBuilder();
            OnSection section = (OnSection) span.getSection();
            result.append(fixupClassCode(getGlobalData(), section.getClassCode(getBroker()), getBroker()));
            result.append(section.getMinistryDefinedCourse());
            result.append(section.getInstitutionType());
            result.append(section.getLocallyDevelopedCourse());
            // result.append(span.getSection().getTermView());
            BigDecimal credit = section.getCredit();
            if (credit == null) {
                credit = BigDecimal.valueOf(0.0);
            }
            result.append(OnsisConstants.DECIMAL_FORMAT_FTE.format(credit));
            result.append(section.getLanguageOfInstruction());
            return result.toString();
        }

    }

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
     * Gets the non credit program codes.
     *
     * @return Sets the
     */
    public Set<String> getNonCreditProgramCodes() {
        return getDictionaryExtractor()
                .getRefCodesWithStateValue(OnSection.FIELD_CONED_PROG_TYPE.getField(getDictionaryExtractor()),
                        Arrays.asList(OnSection.CONED_INDIGENOUS_LANGUAGE, OnSection.CONED_LITERACY,
                                OnSection.CONED_DEVELOPMENTALLY_DISABLED))
                .stream().map(ReferenceCode::getCode).collect(Collectors.toSet());
    }

    /**
     * Gets the parent entity.
     *
     * @return Onsis student enrollment entity
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getParentEntity()
     */
    @Override
    public OnsisStudentEnrollmentEntity getParentEntity() {
        return (OnsisStudentEnrollmentEntity) super.getParentEntity();
    }

}
