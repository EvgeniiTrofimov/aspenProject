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
import com.x2dev.procedures.statereporting.common.ToolBean.ToolGradeTermDate;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradeMatcher;
import com.x2dev.procedures.statereporting.on.revised.OnsisBeans.OnsisStudent;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.OnsisAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnsisStudentSchoolEnrollment.OnsisStudentEnrollmentEntity;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.w3c.dom.Element;

/**
 * The Class OnsisReportCard.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnsisReportCard extends OnsisStateReportData {

    /**
     * The Class OnsisReportCardEntity.
     */
    public static class OnsisReportCardEntity extends OnsisStateReportEntity {
        public boolean m_hasTerms;

        /**
         * Gets the annual span.
         *
         * @return the annual span
         */
        public OnsisAnnualSpan getAnnualSpan() {
            OnsisReportCard reportData = getReportData();

            OnsisStudentEnrollmentEntity parentEntity = reportData.getParentEntity();
            return parentEntity.getSpan();
        }

        /**
         * Gets the grade in september.
         *
         * @return the grade in september
         */
        public String getGradeInSeptember() {
            OnsisReportCard reportData = getReportData();

            OnsisStudentEnrollmentEntity parentEntity = reportData.getParentEntity();
            OnsisAnnualSpan span = parentEntity.getSpan();

            /*
             * GradeInSeptember is current span grade level + 1.
             *
             * span.getGradeType is a State Code.
             * 1. Reverse-lookup from the State Code to get the ReferenceCode record
             * 2. Get the numeric grade level from the current ReferenceCode record
             * 3. Locate the ReferenceCode record with (Numeric Level +1)
             * 4. Return the new State Code
             */
            String gradeStateCode = span.getGradeType(getGlobalData());
            GradeMatcher gradeMatcher = getGlobalData().getGradesHelper().getGradeMatcher();
            ReferenceCode refCode = gradeMatcher.getReferenceCodeReverseLookup(gradeStateCode);
            if (refCode == null) {
                return null;
            }

            Integer numericGrade = gradeMatcher.getNumericGrade(refCode);
            if (numericGrade == null) {
                return null;
            }

            ReferenceCode nextYearGradRefCode = gradeMatcher.getReferenceCodeForNumericGradeLevel(numericGrade + 1);
            if (nextYearGradRefCode == null) {
                return null;
            }

            String gradeInSeptember = nextYearGradRefCode.getStateCode();
            return gradeInSeptember;
        }


        /**
         * Gets the grade terms.
         *
         * @return the grade terms
         */
        public List<ToolGradeTermDate> getGradeTerms() {
            /*
             * Submissions prior to June:
             * Export report cards for Terms that end during this reporting period.
             *
             * For the June submissions only:
             * "Export report cards that haven't already published in prior submissions."
             *
             * This means (June only):
             * Export report cards for all terms in the school year
             * but exclude report cards for Enrolment spans
             * that ended before the Period Start Date April 1
             * because those report cards would have exported in prior submissions.
             */
            OnsisReportCard reportData = getReportData();
            SubmissionType submissionType = getGlobalData().getSubmissionType();

            OnsisStudentEnrollmentEntity parentEntity = reportData.getParentEntity();
            ToolSchool school = getAnnualSpan().getSchool();
            if (school == null) {
                return new ArrayList<>();
            }

            Collection<ToolGradeTermDate> gradeTerms = school.getGradeTermDates(getBroker())
                    .getGroup(ToolGradeTermDate.FIELD_DISTRICT_CONTEXT_OID,
                            getGlobalData().getCurrentContext().getOid())
                    .stream()
                    .filter(gta -> gta.getEndDate() != null && !gta.getEndDate().after(getGlobalData().getEndDate()))
                    .collect(Collectors.toList());
            for (ToolGradeTermDate gta : gradeTerms) {
                String gradeTermId = gta.getGradeTermId();
                PlainDate startDate = gta.getStartDate();
                PlainDate endDate = gta.getEndDate();
                getReportData().log("OnsisReportCard.getGradeTerms(GradeTermDate " + gradeTermId + ": " + startDate
                        + " to " + endDate + ".)");
            }

            /*
             * Term must overlap enrolment span
             */
            Range<Date> activeDateRange =
                    Range.of(parentEntity.getOnsisEnrollmentStartDate(), parentEntity.getOnsisEnrollmentEndDate());
            return gradeTerms.stream()
                    .filter(gtd -> activeDateRange.isOverlap(gtd.getRange()))
                    .collect(Collectors.toList());
        }

        /**
         * Gets the report data.
         *
         * @return the report data
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#getReportData()
         */
        @Override
        public OnsisReportCard getReportData() {
            return (OnsisReportCard) super.getReportData();
        }

        /**
         * Gets the student.
         *
         * @return the student
         */
        public OnsisStudent getStudent() {
            OnsisReportCard reportData = getReportData();
            OnsisStudentEnrollmentEntity parentEntity = reportData.getParentEntity();

            return parentEntity.getStudent();
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

            // must be set later by ReportCardTerm topic
            m_hasTerms = false;

            /*
             * Filter records by grade level
             */
            SubmissionSchoolType schoolType = getGlobalData().getSchoolType();
            Collection<String> reportCardGradeLevelLimiter = schoolType.getReportCardGradeLevelLimiter();
            if (reportCardGradeLevelLimiter != null) {
                String gradeLevel = getAnnualSpan() == null ? getStudent().getGradeLevel()
                        : getAnnualSpan().getGradeType(getGlobalData());
                if (!reportCardGradeLevelLimiter.contains(gradeLevel)) {
                    setRowCount(0);
                }
            }

            OnsisReportCard reportData = getReportData();
            OnsisStudentEnrollmentEntity parentEntity = reportData.getParentEntity();
            OnsisAnnualSpan span = parentEntity.getSpan();

            /*
             * If it's other than a June submission,
             * Student must have a withdrawal during the submission period.
             */
            SubmissionType submissionType = getGlobalData().getSubmissionType();
            if (!submissionType.isJuneSubmission()) {
                PlainDate withdrawalDate = parentEntity.getOnsisEnrollmentEndDate();
                if (withdrawalDate == null || !getGlobalData().getDateRange().contains(withdrawalDate)) {
                    getReportData().log("OnsisReportCard.intitialize(withdrawalDate " + withdrawalDate
                            + " not in submission period.)");
                    setRowCount(0);
                }
            }


            /*
             * In June, a Withdrawal is not required when exporting report cards,
             * but don't export report cards if this span ended before the report start date
             */
            if (submissionType.isJuneSubmission()) {
                PlainDate lastActiveDate = span.getLastActiveInSessionDate();
                if (lastActiveDate != null && lastActiveDate.before(getGlobalData().getStartDate())) {
                    getReportData().log("OnsisReportCard.intitialize(lastActiveDate " + lastActiveDate
                            + " before submission period.)");
                    setRowCount(0);
                }
            }
        }

        /**
         * Checks if is cancelable.
         *
         * @return true, if is cancelable
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isCancelable()
         */
        @Override
        protected boolean isCancelable() {
            return true;
        }

        /**
         * Checks if is row canceled.
         *
         * @param entityElement the entity element
         * @param parentElement the parent element
         * @return true, if is row canceled
         * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData.OnsisStateReportEntity#isRowCanceled(org.jdom.Element,
         *      org.jdom.Element)
         */
        @Override
        protected boolean isRowCanceled(Element entityElement, Element parentElement) {
            int termElementCount = entityElement.getElementsByTagName(ELEMENT_TERM).getLength();

            getReportData().log("OnsisReportCard.isRowCanceled(TermCount " + termElementCount + " cancel if zero)");

            /*
             * Cancel a Report Card row if there are no Terms published
             */
            return termElementCount == 0;
        }

    }

    public static final String ELEMENT_TERM = "TERM";

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
    public OnsisStudentEnrollmentEntity getParentEntity() {
        return (OnsisStudentEnrollmentEntity) super.getParentEntity();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OnsisReportCardEntity.class);
    }
}
