/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.*;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FrenchAsSecondLanguageReportData.
 */
public class FrenchAsSecondLanguageReportData extends OnReportJavaSourceNew {

    /**
     * The Enum CourseLevel.
     */
    private static enum CourseLevel {
        Intermediate, Senior;
    }



    /**
     * The Class FSLSummaryRecord.
     */
    private static class FSLRecord {
        private Map<FSLCommonFields, Object> m_commonFieldValuePairs = new HashMap<FSLCommonFields, Object>();
        private Map<FSLSummaryFieldElementary, FSLElementarySummaryContainer> m_summaryFieldElemValuesPairs =
                new HashMap<FSLSummaryFieldElementary, FSLElementarySummaryContainer>();
        private Map<FSLSummaryFieldSecondary, FSLSecondarySummaryContainer> m_summaryFieldSecValuesPairs =
                new HashMap<FSLSummaryFieldSecondary, FSLSecondarySummaryContainer>();
        private Map<FSLDetailFields, Object> m_detailFieldValuePairs = new HashMap<FSLDetailFields, Object>();

        /**
         * The Enum FSLCommonFields.
         */
        private enum FSLCommonFields {
            boardName,
            //
            boardNumber,
            //
            schoolName,
            //
            schoolNumber,
            //
            academicYears,
            //
            programName,
            //
            currentDate,
            //
            district;

            /**
             * Gets the.
             *
             * @param key String
             * @return FSLCommonFields
             */
            public static FSLCommonFields get(String key) {
                for (FSLCommonFields field : FSLCommonFields.values()) {
                    if (key.equals(field.toString())) {
                        return field;
                    }
                }

                return null;
            }
        }

        /**
         * The Enum FSLCommonFields.
         */
        private enum FSLDetailFields {
            //
            age,
            //
            arrivalStatus,
            //
            boardResidenceStatus,
            //
            grade,
            //
            gradeToSort,
            //
            gender,
            //
            oen,
            //
            studentName,
            //
            studentNameToSort,
            //
            counted,
            //
            isOver21Student,
            //
            isOtherPupil,
            //
            isSharedStudent,
            //
            isElearningStudent,
            //
            isFullyHighCredit,
            //
            numberOfMinutes,
            //
            coreInterCredits,
            //
            coreSeniorCredits,
            //
            immersionInterCredits,
            //
            immersionSeniorCredits,
            //
            otherInterCredits,
            //
            otherSeniorCredits,
            //
            secondLanguageProgramType,
            //
            gradeTypeSequenceNumber,
            //
            totalSchoolCore,
            //
            totalSchoolExtended,
            //
            totalSchoolImmersion,
            //
            totalSchoolCounted;

            /**
             * Gets the.
             *
             * @param key String
             * @return FSLDetailFields
             */
            public static FSLDetailFields get(String key) {
                for (FSLDetailFields field : FSLDetailFields.values()) {
                    if (key.equals(field.toString())) {
                        return field;
                    }
                }

                return null;
            }
        }

        /**
         * The Enum FSLSummaryField for Elementary Report.
         */
        private enum FSLSummaryFieldElementary {
            //
            jk("Junior Kindergarten"),
            //
            sk("Kindergarten"),
            //
            grade01Data("Grade 1"),
            //
            grade02Data("Grade 2"),
            //
            grade03Data("Grade 3"),
            //
            grade04Data("Grade 4"),
            //
            grade05Data("Grade 5"),
            //
            grade06Data("Grade 6"),
            //
            grade07Data("Grade 7"),
            //
            grade08Data("Grade 8"),
            //
            totalCount("Total");

            private final String key;

            /**
             * Instantiates a new FSL summary field.
             *
             * @param key String
             */
            FSLSummaryFieldElementary(String key) {
                this.key = key;
            }

            /**
             * Key.
             *
             * @return String
             */
            public String key() {
                return key;
            }
        }

        /**
         * The Enum FSLSummaryField for Secondary Report.
         */
        private enum FSLSummaryFieldSecondary {
            //
            preGrade9("Pre-Grade 9"),
            //
            grade9Data("Grade 9"),
            //
            grade10Data("Grade 10"),
            //
            grade11Data("Grade 11"),
            //
            grade12Data("Grade 12"),
            //
            student21OrOver("Students 21 or over"),
            //
            totalCount("Total");

            private final String key;

            /**
             * Instantiates a new FSL summary field.
             *
             * @param key String
             */
            FSLSummaryFieldSecondary(String key) {
                this.key = key;
            }

            /**
             * Key.
             *
             * @return String
             */
            public String key() {
                return key;
            }

            /**
             * Gets the.
             *
             * @param key String
             * @return FSLSummaryField
             */
            public static FSLSummaryFieldSecondary get(String key) {
                for (FSLSummaryFieldSecondary field : FSLSummaryFieldSecondary.values()) {
                    if (key.equals(field.toString())) {
                        return field;
                    }
                }

                return null;
            }
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(FSLCommonFields.district.toString(),
                        FSLCommonFields.boardNumber.toString(),
                        FSLCommonFields.schoolName.toString(),
                        FSLDetailFields.studentName.toString(),
                        FSLDetailFields.oen.toString());

        private static ValueByKeyResolver<FSLRecord> s_valueResolver = new ValueByKeyResolver<FSLRecord>() {
            @Override
            public Object getValue(String key, FSLRecord entity) {
                Object rerutnValue = null;
                if (FSLCommonFields.get(key) != null) {
                    rerutnValue = entity.getCommonField(FSLCommonFields.valueOf(key));
                } else if (FSLDetailFields.get(key) != null) {
                    rerutnValue = entity.getDetailField(FSLDetailFields.valueOf(key));
                } else if (FSLSummaryFieldSecondary.get(key) != null) {
                    rerutnValue = entity.getSummaryFieldSec(FSLSummaryFieldSecondary.valueOf(key));
                }

                return rerutnValue;
            }
        };

        /**
         * Gets the.
         *
         * @param key Field
         * @return Object
         */
        Object getCommonField(FSLCommonFields key) {
            return m_commonFieldValuePairs.get(key);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        void setCommonField(FSLCommonFields field, Object value) {
            m_commonFieldValuePairs.put(field, value);
        }

        /**
         * Sets the summary field for elementary.
         *
         * @param studentGroupName FSLSummaryFieldElementary
         * @param fslContainer FSLSummaryContainer
         */
        void setSummaryFieldElem(FSLSummaryFieldElementary studentGroupName,
                                 FSLElementarySummaryContainer fslContainer) {
            m_summaryFieldElemValuesPairs.put(studentGroupName, fslContainer);
        }

        /**
         * Sets value into container.
         *
         * @param studentGroupName FSLSummaryField
         * @param fslContainer FSLSummaryContainer
         */
        void setSummaryFieldSec(FSLSummaryFieldSecondary studentGroupName, FSLSecondarySummaryContainer fslContainer) {
            m_summaryFieldSecValuesPairs.put(studentGroupName, fslContainer);
        }

        /**
         * Gets the summary field elem.
         *
         * @param studentGroupName FSLSummaryFieldElementary
         * @return FSL elementary summary container
         */
        FSLElementarySummaryContainer getSummaryFieldElem(FSLSummaryFieldElementary studentGroupName) {
            return m_summaryFieldElemValuesPairs.get(studentGroupName);
        }

        /**
         * Gets value from container.
         *
         * @param studentGroupName FSLSummaryFieldSecondary
         * @return FSL summary container
         */
        FSLSecondarySummaryContainer getSummaryFieldSec(FSLSummaryFieldSecondary studentGroupName) {
            return m_summaryFieldSecValuesPairs.get(studentGroupName);
        }

        /**
         * Sets value into container.
         *
         * @param detailField FSLDetailFields
         * @param value Object
         */
        void setDetailField(FSLDetailFields detailField, Object value) {
            m_detailFieldValuePairs.put(detailField, value);
        }

        /**
         * Gets value from container.
         *
         * @param detailField FSLDetailFields
         * @return Object
         */
        Object getDetailField(FSLDetailFields detailField) {
            return m_detailFieldValuePairs.get(detailField);
        }

    }

    /**
     * Class intended for storing fields of summary report for Elementary.
     */
    public static class FSLElementarySummaryContainer {
        private String m_fslPupilCategoryName;
        private Integer m_coreTotalNumberOfStudents;
        private BigDecimal m_coreMinutes;
        private Integer m_extendedTotalNumberOfStudents;
        private BigDecimal m_extendedMinutes;
        private Integer m_immersionTotalNumberOfStudents;
        private BigDecimal m_immersionMinutes;

        /**
         * Instantiates a new FSL summary container.
         */
        public FSLElementarySummaryContainer() {

        }

        /**
         * Instantiates a new FSL summary container.
         *
         * @param categoryName String
         */
        public FSLElementarySummaryContainer(String categoryName) {
            m_fslPupilCategoryName = categoryName;
            m_coreTotalNumberOfStudents = Integer.valueOf(0);
            m_coreMinutes = new BigDecimal(0);
            m_extendedTotalNumberOfStudents = Integer.valueOf(0);
            m_extendedMinutes = new BigDecimal(0);
            m_immersionTotalNumberOfStudents = Integer.valueOf(0);
            m_immersionMinutes = new BigDecimal(0);
        }

        /**
         * Gets the fsl pupil category name.
         *
         * @return the fslPupilCategoryName
         */
        public String getFslPupilCategoryName() {
            return m_fslPupilCategoryName;
        }

        /**
         * Sets the fsl pupil category name.
         *
         * @param fslPupilCategoryName void
         */
        public void setFslPupilCategoryName(String fslPupilCategoryName) {
            m_fslPupilCategoryName = fslPupilCategoryName;
        }

        /**
         * Gets the core total number of students.
         *
         * @return the coreTotalNumberOfStudents
         */
        public Integer getCoreTotalNumberOfStudents() {
            return m_coreTotalNumberOfStudents;
        }

        /**
         * Sets the core total number of students.
         *
         * @param coreTotalNumberOfStudents void
         */
        public void setCoreTotalNumberOfStudents(Integer coreTotalNumberOfStudents) {
            m_coreTotalNumberOfStudents = coreTotalNumberOfStudents;
        }

        /**
         * Gets the extended total number of students.
         *
         * @return the m_extendedTotalNumberOfStudents
         */
        public Integer getExtendedTotalNumberOfStudents() {
            return m_extendedTotalNumberOfStudents;
        }

        /**
         * Sets the extended total number of students.
         *
         * @param extendedTotalNumberOfStudents void
         */
        public void setExtendedTotalNumberOfStudents(Integer extendedTotalNumberOfStudents) {
            m_extendedTotalNumberOfStudents = extendedTotalNumberOfStudents;
        }

        /**
         * Gets the extended minutes.
         *
         * @return the m_extendedMinutes
         */
        public BigDecimal getExtendedMinutes() {
            return m_extendedMinutes;
        }

        /**
         * Sets the extended minutes.
         *
         * @param extendedMinutes void
         */
        public void setExtendedMinutes(BigDecimal extendedMinutes) {
            m_extendedMinutes = extendedMinutes;
        }

        /**
         * Gets the immersion total number of students.
         *
         * @return the m_immersionTotalNumberOfStudents
         */
        public Integer getImmersionTotalNumberOfStudents() {
            return m_immersionTotalNumberOfStudents;
        }

        /**
         * Sets the immersion total number of students.
         *
         * @param immersionTotalNumberOfStudents void
         */
        public void setImmersionTotalNumberOfStudents(Integer immersionTotalNumberOfStudents) {
            m_immersionTotalNumberOfStudents = immersionTotalNumberOfStudents;
        }

        /**
         * Gets the immersion minutes.
         *
         * @return the m_immersionMinutes
         */
        public BigDecimal getImmersionMinutes() {
            return m_immersionMinutes;
        }

        /**
         * Sets the immersion minutes.
         *
         * @param immersionMinutes void
         */
        public void setImmersionMinutes(BigDecimal immersionMinutes) {
            m_immersionMinutes = immersionMinutes;
        }

        /**
         * Gets the core minutes.
         *
         * @return the m_coreMinutes
         */
        public BigDecimal getCoreMinutes() {
            return m_coreMinutes;
        }

        /**
         * Sets the core minutes.
         *
         * @param coreMinutes void
         */
        public void setCoreMinutes(BigDecimal coreMinutes) {
            this.m_coreMinutes = coreMinutes;
        }
    }

    /**
     * Class intended for storing fields of summary report for Secondary.
     */
    public static class FSLSecondarySummaryContainer {
        private String fslPupilCategoryName;
        private Integer coreTotalNumberOfStudents;
        private BigDecimal coreCreditsOfIntermediateStudents;
        private BigDecimal coreCreditsOfSeniorStudents;
        private Integer extendedTotalNumberOfStudents;
        private Integer immersionTotalNumberOfStudents;
        private BigDecimal frenchCourseIntermediateCredits;
        private BigDecimal frenchCourseSeniorCredits;
        private BigDecimal otherSubjectIntermediateCredits;
        private BigDecimal otherSubjectSeniorCredits;

        /**
         * Instantiates a new FSL summary container.
         */
        public FSLSecondarySummaryContainer() {

        }

        /**
         * Instantiates a new FSL summary container.
         *
         * @param categoryName String
         */
        public FSLSecondarySummaryContainer(String categoryName) {
            fslPupilCategoryName = categoryName;
        }

        /**
         * Gets the fsl pupil category name.
         *
         * @return the fslPupilCategoryName
         */
        public String getFslPupilCategoryName() {
            return fslPupilCategoryName;
        }

        /**
         * Sets the fsl pupil category name.
         *
         * @param fslPupilCategoryName void
         */
        public void setFslPupilCategoryName(String fslPupilCategoryName) {
            this.fslPupilCategoryName = fslPupilCategoryName;
        }

        /**
         * Gets the core total number of students.
         *
         * @return the coreTotalNumberOfStudents
         */
        public Integer getCoreTotalNumberOfStudents() {
            return coreTotalNumberOfStudents;
        }

        /**
         * Sets the core total number of students.
         *
         * @param coreTotalNumberOfStudents void
         */
        public void setCoreTotalNumberOfStudents(Integer coreTotalNumberOfStudents) {
            this.coreTotalNumberOfStudents = coreTotalNumberOfStudents;
        }

        /**
         * Gets the core credits of intermediate students.
         *
         * @return the coreCreditsOfIntermediateStudents
         */
        public BigDecimal getCoreCreditsOfIntermediateStudents() {
            return coreCreditsOfIntermediateStudents;
        }

        /**
         * Sets the core credits of intermediate students.
         *
         * @param coreCreditsOfIntermediateStudents void
         */
        public void setCoreCreditsOfIntermediateStudents(BigDecimal coreCreditsOfIntermediateStudents) {
            this.coreCreditsOfIntermediateStudents = coreCreditsOfIntermediateStudents;
        }

        /**
         * Gets the core credits of senior students.
         *
         * @return the coreCreditsOfSeniorStudents
         */
        public BigDecimal getCoreCreditsOfSeniorStudents() {
            return coreCreditsOfSeniorStudents;
        }

        /**
         * Sets the core credits of senior students.
         *
         * @param coreCreditsOfSeniorStudents void
         */
        public void setCoreCreditsOfSeniorStudents(BigDecimal coreCreditsOfSeniorStudents) {
            this.coreCreditsOfSeniorStudents = coreCreditsOfSeniorStudents;
        }

        /**
         * Gets the extended total number of students.
         *
         * @return the extendedTotalNumberOfStudents
         */
        public Integer getExtendedTotalNumberOfStudents() {
            return extendedTotalNumberOfStudents;
        }

        /**
         * Sets the extended total number of students.
         *
         * @param extendedTotalNumberOfStudents void
         */
        public void setExtendedTotalNumberOfStudents(Integer extendedTotalNumberOfStudents) {
            this.extendedTotalNumberOfStudents = extendedTotalNumberOfStudents;
        }

        /**
         * Gets the immersion total number of students.
         *
         * @return the immersionTotalNumberOfStudents
         */
        public Integer getImmersionTotalNumberOfStudents() {
            return immersionTotalNumberOfStudents;
        }

        /**
         * Sets the immersion total number of students.
         *
         * @param immersionTotalNumberOfStudents void
         */
        public void setImmersionTotalNumberOfStudents(Integer immersionTotalNumberOfStudents) {
            this.immersionTotalNumberOfStudents = immersionTotalNumberOfStudents;
        }

        /**
         * Gets the french course intermediate credits.
         *
         * @return the frenchCourseIntermediateCredits
         */
        public BigDecimal getFrenchCourseIntermediateCredits() {
            return frenchCourseIntermediateCredits;
        }

        /**
         * Sets the french course intermediate credits.
         *
         * @param frenchCourseIntermediateCredits void
         */
        public void setFrenchCourseIntermediateCredits(BigDecimal frenchCourseIntermediateCredits) {
            this.frenchCourseIntermediateCredits = frenchCourseIntermediateCredits;
        }

        /**
         * Gets the french course senior credits.
         *
         * @return the frenchCourseSeniorCredits
         */
        public BigDecimal getFrenchCourseSeniorCredits() {
            return frenchCourseSeniorCredits;
        }

        /**
         * Sets the french course senior credits.
         *
         * @param frenchCourseSeniorCredits void
         */
        public void setFrenchCourseSeniorCredits(BigDecimal frenchCourseSeniorCredits) {
            this.frenchCourseSeniorCredits = frenchCourseSeniorCredits;
        }

        /**
         * Gets the other subject intermediate credits.
         *
         * @return the otherSubjectIntermediateCredits
         */
        public BigDecimal getOtherSubjectIntermediateCredits() {
            return otherSubjectIntermediateCredits;
        }

        /**
         * Sets the other subject intermediate credits.
         *
         * @param otherSubjectIntermediateCredits void
         */
        public void setOtherSubjectIntermediateCredits(BigDecimal otherSubjectIntermediateCredits) {
            this.otherSubjectIntermediateCredits = otherSubjectIntermediateCredits;
        }

        /**
         * Gets the other subject senior credits.
         *
         * @return the otherSubjectSeniorCredits
         */
        public BigDecimal getOtherSubjectSeniorCredits() {
            return otherSubjectSeniorCredits;
        }

        /**
         * Sets the other subject senior credits.
         *
         * @param otherSubjectSeniorCredits void
         */
        public void setOtherSubjectSeniorCredits(BigDecimal otherSubjectSeniorCredits) {
            this.otherSubjectSeniorCredits = otherSubjectSeniorCredits;
        }

    }

    /**
     * Class intended for storing total count data.
     */
    private static class FSLTotalsContainer {

        private Integer totalSchoolCore;
        private Integer totalSchoolExtended;
        private Integer totalSchoolImmersion;

        /**
         * Gets the total school core.
         *
         * @return the totalSchoolCore
         */
        public Integer getTotalSchoolCore() {
            return totalSchoolCore;
        }

        /**
         * Sets the total school core.
         *
         * @param totalSchoolCore void
         */
        public void setTotalSchoolCore(Integer totalSchoolCore) {
            this.totalSchoolCore = totalSchoolCore;
        }

        /**
         * Gets the total school extended.
         *
         * @return the totalSchoolExtended
         */
        public Integer getTotalSchoolExtended() {
            return totalSchoolExtended;
        }

        /**
         * Sets the total school extended.
         *
         * @param totalSchoolExtended void
         */
        public void setTotalSchoolExtended(Integer totalSchoolExtended) {
            this.totalSchoolExtended = totalSchoolExtended;
        }

        /**
         * Gets the total school immersion.
         *
         * @return the totalSchoolImmersion
         */
        public Integer getTotalSchoolImmersion() {
            return totalSchoolImmersion;
        }

        /**
         * Sets the total school immersion.
         *
         * @param totalSchoolImmersion void
         */
        public void setTotalSchoolImmersion(Integer totalSchoolImmersion) {
            this.totalSchoolImmersion = totalSchoolImmersion;
        }
    }

    /**
     * The Enum PGMType.
     */
    private static enum PGMType {
        CORE, EXTENDED_IMMERSION, OTHER;
    }

    /**
     * The Enum ReportType.
     */
    private static enum ReportType {
        DETAIL, SUMMARY;
    }

    /**
     * The Class SchoolDateRangeProvider.
     */
    class SchoolDateRangeProvider implements OnSchoolDateRangeProvider {

        private DistrictSchoolYearContext m_ctxByDate;

        /**
         * Instantiates a new school date range provider.
         *
         * @param school the school
         */
        public SchoolDateRangeProvider(OnSchool school) {
            m_school = school;
        }

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisBrokerProvider#getBroker()
         */
        @Override
        public X2Broker getBroker() {
            return FrenchAsSecondLanguageReportData.this.getBroker();
        }

        /**
         * Gets the context.
         *
         * @return the context
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnCTXProvider#getContext()
         */
        @Override
        public DistrictSchoolYearContext getContext() {
            return getCurrentContext();
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return the dictionary extractor
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDictionaryExtractorProvider#getDictionaryExtractor()
         */
        @Override
        public DictionaryExtractor getDictionaryExtractor() {
            return getDictExtractor();
        }


        /**
         * Gets the context by date.
         *
         * @param date PlainDate
         * @return District school year context
         */
        public DistrictSchoolYearContext getContextByDate(PlainDate date) {
            if (m_ctxByDate == null) {
                QueryByCriteria query = new QueryByCriteria(DistrictSchoolYearContext.class);
                query.addOrderByDescending(DistrictSchoolYearContext.COL_START_DATE);
                Collection<DistrictSchoolYearContext> contexts = getBroker().getCollectionByQuery(query);
                /*
                 * Walk context years in descending order until one of them starts on/before date
                 */
                DistrictSchoolYearContext found = null;
                Iterator<DistrictSchoolYearContext> iter = contexts.iterator();
                while (found == null && iter.hasNext()) {
                    DistrictSchoolYearContext ctx = iter.next();

                    // Check that context starts before date.
                    if (date.before(ctx.getStartDate())) {
                        continue;
                    }

                    /*
                     * Check date is within 370 days of startDate
                     * to detect if we've gone too far (ie no context contains the date)
                     *
                     * Don't compare vs Context End Date
                     * because contexts might not extend until next context start date
                     */
                    PlainDate extendedContextEnd = DateUtils.add(ctx.getStartDate(), 370);
                    if (date.after(extendedContextEnd)) {
                        break;
                    }

                    found = ctx;
                }
                m_ctxByDate = found != null ? found : getContext();
            }

            return m_ctxByDate;
        }

        /**
         * Gets the end date.
         *
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDateRangeProvider#getEndDate()
         */
        @Override
        public PlainDate getEndDate() {
            return getContextByDate(getStartDate()).getEndDate();
        }

        /**
         * Gets the school.
         *
         * @return School
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnOnSchoolProvider#getSchool()
         */
        @Override
        public OnSchool getSchool() {
            return m_school;
        }

        /**
         * Gets the start date.
         *
         * @return Plain date
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDateRangeProvider#getStartDate()
         */
        @Override
        public PlainDate getStartDate() {
            return isElementaryReport() ? getReportDate() : getReportDateForSecDetail();
        }
    }

    /**
     * The Class ReportSection.
     */
    public static class ReportSection extends OnSection {

        /**
         * Instantiates a new onsis section.
         *
         * @param columns
         * @param data
         */
        public ReportSection(ToolBeanDefinition columns, Object[] data) {
            super(columns, data);
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection#getLanguageOfInstruction()
         */
        @Override
        public String getLanguageOfInstruction() {
            String languageOfInstruction = getValueReferenceState(FIELD_LANGUAGE_OF_INSTRUCTION_CSK);
            if (StringUtils.isEmpty(languageOfInstruction)
                    || !FRENCH_LANGUAGE_STATE.equals(languageOfInstruction)) {
                languageOfInstruction = getValueReferenceState(FIELD_LANGUAGE_OF_INSTRUCTION_CRS);
            }
            if (StringUtils.isEmpty(languageOfInstruction)
                    || !FRENCH_LANGUAGE_STATE.equals(languageOfInstruction)) {
                languageOfInstruction = getValueReferenceState(FIELD_LANGUAGE_OF_INSTRUCTION);
            }
            return languageOfInstruction;
        }

        /**
         * @see com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection#getMinistryDefinedCourse()
         */
        @Override
        public String getMinistryDefinedCourse() {
            return this.getValueString(FIELD_MINISTRY_COURSE_CODE);
        }

    }

    /**
     * Constants
     */

    private static final String CHAR_B = "B";
    private static final String CHAR_C = "C";
    private static final String CHAR_D = "D";
    private static final String CHAR_E = "E";

    private static final String COURSE_PREFFIX_FE = "FE";
    private static final String COURSE_PREFFIX_FI = "FI";
    private static final String COURSE_PREFFIX_FS = "FS";
    private static final String COURSE_PREFFIX_LN = "LN";
    private static final String COURSE_PREFFIX_LV = "LV";
    private static final String COURSE_PREFFIX_L = "L";

    private static final String DECIMAL_MATCHER = "^[0-9]*\\.?[0-9]*";

    private static final String FIELD_PRINT_DATE_HEADER = "printDateHeader";
    private static final String FIELD_PRINT_GRADE_HEADER = "printGradeHeader";

    private static final String FRENCH_LANGUAGE_STATE = "F";

    private static final String GRADE_LEVEL_JK = "JK";
    private static final String GRADE_LEVEL_SK = "K";
    private static final String GRADE_LEVEL_01 = "1";
    private static final String GRADE_LEVEL_02 = "2";
    private static final String GRADE_LEVEL_03 = "3";
    private static final String GRADE_LEVEL_04 = "4";
    private static final String GRADE_LEVEL_05 = "5";
    private static final String GRADE_LEVEL_06 = "6";
    private static final String GRADE_LEVEL_07 = "7";
    private static final String GRADE_LEVEL_08 = "8";
    private static final String GRADE_LEVEL_P9 = "P9";
    private static final String GRADE_LEVEL_09 = "9";
    private static final String GRADE_LEVEL_10 = "10";
    private static final String GRADE_LEVEL_11 = "11";
    private static final String GRADE_LEVEL_12 = "12";

    private static final String NUM_1 = "1";
    private static final String NUM_2 = "2";
    private static final String NUM_3 = "3";
    private static final String NUM_4 = "4";

    private static final String PGM_CODE_SLP = "SLP";
    private static final String PGM_FSL_CORE = "Core";
    private static final String PGM_FSL_IMMERSION = "Immersion";
    private static final String PGM_FSL_EXTENDED = "Extended";
    private static final String PGM_FSL_ICFP = "ICFP";
    private static final String PGM_TYPE_FSL = "FSL";

    private static final String RP_GRAND_TOTAL_CORE = "grandTotalCore";
    private static final String RP_GRAND_TOTAL_COUNTED = "grandTotalCounted";
    private static final String RP_GRAND_TOTAL_EXTENDED = "grandTotalExtended";
    private static final String RP_GRAND_TOTAL_IMMERSION = "grandTotalImmersion";

    /**
     * Report Fields Secondary.
     */
    private static final String REPORT_FIELD_CAT_NAME = "categoryName";
    private static final String REPORT_FIELD_CORE_CREDIT_INTERMEDIATE = "coreCreditIntermediate";
    private static final String REPORT_FIELD_CORE_CREDIT_SENIOR = "coreCreditSenior";
    private static final String REPORT_FIELD_CREDIT_FRENCH_INTERMEDIATE = "creditFrenchIntermediate";
    private static final String REPORT_FIELD_CREDIT_FRENCH_SENIOR = "creditFrenchSenior";
    private static final String REPORT_FIELD_CREDIT_OTHER_INTERMEDIATE = "creditOtherIntermediate";
    private static final String REPORT_FIELD_CREDIT_OTHER_SENIOR = "creditOtherSenior";
    private static final String REPORT_FIELD_TOTAL_STD_NUM = "totalStdNum";
    private static final String REPORT_FIELD_TOTAL_STD_NUM_EXTENDED = "totalStdNumExtended";
    private static final String REPORT_FIELD_TOTAL_STD_NUM_IMMERSION = "totalStdNumImmersion";

    /**
     * Report Fields Elementary.
     */
    private static final String RF_ELEM_MINUTES_CORE = "coreMinutes";
    private static final String RF_ELEM_MINUTES_EXTENDED = "extendedMinutes";
    private static final String RF_ELEM_MINUTES_IMMERSION = "immersionMinutes";
    private static final String RF_ELEM_TOTAL_STD_NUM_CORE = "totalStdNumCore";
    private static final String RF_ELEM_TOTAL_STD_NUM_EXTENDED = "totalStdNumExtended";
    private static final String RF_ELEM_TOTAL_STD_NUM_IMMERSION = "totalStdNumImmersion";

    /**
     * Other constants
     */
    private static final String REPORT_ID_CSV = "ON-VRF-FSL-SEC-DETAIL-CSV";
    private static final String REPORT_ID_PDF = "ON-VRF-FSL-SEC-DETAIL";
    private static final String ZERO_DECIMAL = "0.00";
    private static final String ZERO_INT = "0";

    /**
     * Class members.
     */
    private Integer m_coreProgramsCount = 0;
    private Map<String, BigDecimal> m_creditsMap = new HashMap<>();
    private DictionaryExtractor m_dictExtractor;
    private EnrollmentSpanCriteria m_spanCriteria;
    private Integer m_extendedProgramsCount = 0;
    private GradesHelper m_gradesHelper;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private Integer m_immersionProgramsCount = 0;
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private PlainDate m_reportDateForSecDetail;
    private Map<String, FSLTotalsContainer> m_schoolFSLTotalData = new HashMap<>();
    private Map<String, ReportStudentSchool> m_sskByStdOid = new HashMap<>();
    private SchoolDateRangeProvider m_sklDateRangeProvider;

    /**
     * Gets the fte monthly record.
     *
     * @param broker X2Broker
     * @param school the school
     * @param student the submission period code
     * @return FteMonthly
     */
    public FteMonthly getFteMonthlyRecord(X2Broker broker, OnSchool school, ReportStudent student) {
        List<FteMonthly> records = student.getFteMonthlyRecords(broker);
        return records.stream()
                .filter(fteMonthly -> school.getOid().equals(fteMonthly.getSchoolOid()))
                .findFirst()
                .orElse(null);
    }

    @Override
    public Collection<ReportStudent> getStudents(EnrollmentSpanCriteria spanCriteria,
                                                 boolean isBreakOnYog,
                                                 boolean isBreakOnStatus,
                                                 Predicate<AnnualSpan> spanFilter) {
        ToolBean.clearAllCachedToolBeans(ReportStudent.class);
        ToolBean.clearAllCachedToolBeans(ToolEnrollment.class);
        ToolBean.clearAllCachedToolBeans(ReportStudentSchool.class);

        ToolBean.resetCriteria(getBroker(), ReportStudent.class);
        ToolBean.resetCriteria(getBroker(), ToolEnrollment.class);
        ToolBean.resetCriteria(getBroker(), ReportStudentSchool.class);

        X2Criteria candidateCriteria = CriteriaHelper.getStudentCandidateCriteria(spanCriteria,
                getBroker());
        candidateCriteria.addNotEmpty(SisBeanPaths.STUDENT.stateId().getPath(), getBroker().getPersistenceKey());

        // load students with filterable
        FilterableFactory.create(getBroker(), getDictExtractor(), ReportStudent.class,
                candidateCriteria, null);

        // load enrollments and student school
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);

        X2Criteria secondaryCriteria = new X2Criteria();
        secondaryCriteria.addIn(ToolStudentSchool.FIELD_SCHOOL_OID.resolve(getDictExtractor()),
                spanCriteria.getSchoolOids());
        ToolBean.addAndCriteria(getBroker(), ReportStudentSchool.class, secondaryCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);

        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));

        Set<String> allStudentsList = new HashSet();
        ToolBean.getCachedToolBeans(ReportStudent.class).stream()
                .map(student -> student.getEnrollmentSpans(getBroker(), isBreakOnYog, isBreakOnStatus))
                .flatMap(List::stream)
                .filter(span -> {
                    // test secondary spans
                    if (!spanCriteria.isIncludeSecondarySpans() && span.isSecondary()) {
                        return false;
                    }
                    // test school
                    if (spanCriteria.getSchoolOids() != null
                            && !spanCriteria.getSchoolOids().contains(span.getSchool().getOid())) {
                        return false;
                    }
                    ToolEnrollment lastEnrolment =
                            span.getAllEnrollmentsDescend().isEmpty() ? null : span.getAllEnrollmentsDescend().get(0);
                    if (lastEnrolment == null || lastEnrolment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)
                            && getStartDate().after(lastEnrolment.getEnrollmentDate())) {
                        return false;
                    }
                    if (spanFilter != null && !spanFilter.test(span)) {
                        return false;
                    }
                    return true;
                })
                .forEach(span -> {
                    allStudentsList.add(span.getStudent().getOid());
                });

        // remove unused students
        ToolBean.filterCachedToolBeans(ReportStudent.class,
                student -> allStudentsList.contains(student.getOid()));

        return ToolBean.getCachedToolBeans(ReportStudent.class);
    }

    /**
     * Gets the fte record.
     *
     * @param fteRecords
     * @param sklToLookForFTE
     * @return
     */
    public FteRecord findFteRecord(List<FteRecord> fteRecords, OnSchool sklToLookForFTE) {
        return (FteRecord) FilterableFactory.createFilterableToolBeans(fteRecords)
                .filter(new Filter<FteRecord>() {
                    @Override
                    public boolean isFiltered(FteRecord toFilter) {
                        if (!toFilter.getSchoolOid().equals(sklToLookForFTE.getOid())) {
                            return false;
                        }
                        PlainDate fteDate = toFilter.getFteDate();
                        if (fteDate == null) {
                            return false;
                        }
                        return fteDate.compareTo(m_sklDateRangeProvider.getStartDate()) <= 0;
                    }
                })
                .extractSorted(Arrays.asList(FteRecord.FIELD_FTE_DATE.resolve(getDictExtractor())), false)
                .stream()
                .findFirst()
                .orElse(null);
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        Filterable<OnSchool> filterableSkls = getSchoolsFromInput();
        if (filterableSkls != null) {
            Collection<String> sklOidsToReport =
                    filterableSkls.extract().stream().map(skl -> skl.getOid()).collect(Collectors.toList());
            Filterable<FSLRecord> fteRecordsFilterable = filterableSkls.map(new Mapper<OnSchool, FSLRecord>() {
                @Override
                public Filterable<FSLRecord> map(OnSchool school) {
                    initializeHelpersForSchool(school);
                    m_coreProgramsCount = 0;
                    m_extendedProgramsCount = 0;
                    m_immersionProgramsCount = 0;
                    Filterable<ReportStudent> students = FilterableFactory
                            .createFilterableToolBeans(getStudents(m_spanCriteria, true, true, m_annualSpanFilter));
                    preloadStudentsData();
                    students = students.filter(new Filter<ReportStudent>() {
                        @Override
                        public boolean isFiltered(ReportStudent student) {
                            OnStudentSLPProgram slpPgm = student.getSlpPrograms(getBroker()).stream()
                                    .filter(pgmToFilter -> {
                                        return school.getOid().equals(
                                                pgmToFilter.getSchoolOid())
                                                && PGM_TYPE_FSL.equalsIgnoreCase(pgmToFilter.getProgramType())
                                                && PGM_CODE_SLP
                                                        .equalsIgnoreCase(pgmToFilter.getProgramCodeBeanValue())
                                                && !m_sklDateRangeProvider.getStartDate()
                                                        .before(pgmToFilter.getStartDate())
                                                && (pgmToFilter.getEndDate() == null
                                                        || !pgmToFilter.getEndDate()
                                                                .before(m_sklDateRangeProvider.getStartDate()));
                                    })
                                    .findAny()
                                    .orElse(null);
                            return slpPgm != null;
                        }
                    });
                    return getFrechAsSecondLanguageRecord(school, students, sklOidsToReport);
                }
            });
            List<FSLRecord> sortedRecords = fteRecordsFilterable.extractSorted(Arrays.asList(
                    FSLRecord.FSLCommonFields.district.toString(),
                    FSLRecord.FSLCommonFields.boardNumber.toString(),
                    FSLRecord.FSLCommonFields.schoolName.toString(),
                    FSLRecord.FSLDetailFields.gradeToSort.toString(),
                    FSLRecord.FSLDetailFields.secondLanguageProgramType.toString(),
                    FSLRecord.FSLDetailFields.studentNameToSort.toString(),
                    FSLRecord.FSLDetailFields.oen.toString()), true);

            ReportType reportType = getReportType();
            if (reportType == ReportType.DETAIL) {
                setDetailReportData(grid, sortedRecords);
            } else if (isElementaryReport()) {
                setSummaryElementaryReportData(grid, sortedRecords);
            } else {
                setSummarySecondaryReportData(grid, sortedRecords);
            }
            grid.beforeTop();
            if (getErrorsLog().length() > 0) {
                if (grid.isEmpty()) {
                    grid.append();
                    grid.beforeTop();
                }
                addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
            }
            addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
            addParameter(REPORT_PARAM_AS_OF_DATE,
                    s_asOfDateFormat.format(getStartDate()));
            if (reportType == ReportType.DETAIL) {
                int grandTotalCore = m_schoolFSLTotalData.values().stream()
                        .mapToInt(totalContainer -> totalContainer.getTotalSchoolCore()).sum();
                int grandTotalExtended = m_schoolFSLTotalData.values().stream()
                        .mapToInt(totalContainer -> totalContainer.getTotalSchoolExtended()).sum();
                int grandTotalimmersion = m_schoolFSLTotalData.values().stream()
                        .mapToInt(totalContainer -> totalContainer.getTotalSchoolImmersion()).sum();
                addParameter(RP_GRAND_TOTAL_CORE, String.valueOf(grandTotalCore));
                addParameter(RP_GRAND_TOTAL_EXTENDED, String.valueOf(grandTotalExtended));
                addParameter(RP_GRAND_TOTAL_IMMERSION, String.valueOf(grandTotalimmersion));
                addParameter(RP_GRAND_TOTAL_COUNTED,
                        String.valueOf(grandTotalimmersion + grandTotalCore + grandTotalExtended));
            }
        }

        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        ReportType reportType = getReportType();
        if (reportType == ReportType.DETAIL && !isElementaryReport()) {
            int reportFormat = getJob().getInput().getFormat();
            switch (reportFormat) {
                case ToolInput.CSV_FORMAT:
                    this.setFormatId(REPORT_ID_CSV);
                    break;
                case ToolInput.HTML_FORMAT:
                    this.setFormatId(REPORT_ID_PDF);
                    break;
                case ToolInput.PDF_FORMAT:
                    this.setFormatId(REPORT_ID_PDF);
                    break;
                case ToolInput.XLS_FORMAT:
                    this.setFormatId(REPORT_ID_CSV);
                    break;
            }
        }

        registerToolBeans();
        m_dictExtractor = new DictionaryExtractor(getBroker());
        initGradesMap();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Increases count of appropriate program.
     *
     * @param fslProgramType String
     */
    private void addCountOfFSLProgram(String fslProgramType) {
        if (fslProgramType != null) {
            switch (fslProgramType) {
                case PGM_FSL_CORE:
                    m_coreProgramsCount++;
                    break;
                case PGM_FSL_ICFP:
                    m_coreProgramsCount++;
                    break;
                case PGM_FSL_EXTENDED:
                    m_extendedProgramsCount++;
                    break;
                case PGM_FSL_IMMERSION:
                    m_immersionProgramsCount++;
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Adds values of 'gradeSummaryData' to total values(totalData).
     *
     * @param totalData FSLSummaryContainer
     * @param gradeSummaryData FSLSummaryContainer
     */
    private void addElemantaryDataToTotal(FSLElementarySummaryContainer totalData,
                                          FSLElementarySummaryContainer gradeSummaryData) {
        totalData.setCoreTotalNumberOfStudents(computeSummOfTwoIntegers(totalData.getCoreTotalNumberOfStudents(),
                gradeSummaryData.getCoreTotalNumberOfStudents()));
        totalData.setCoreMinutes(computeSummOfTwoBigDecimals(totalData.getCoreMinutes(),
                gradeSummaryData.getCoreMinutes()
                        .multiply(new BigDecimal(gradeSummaryData.getCoreTotalNumberOfStudents()))));

        totalData
                .setExtendedTotalNumberOfStudents(computeSummOfTwoIntegers(totalData.getExtendedTotalNumberOfStudents(),
                        gradeSummaryData.getExtendedTotalNumberOfStudents()));
        totalData.setExtendedMinutes(
                computeSummOfTwoBigDecimals(totalData.getExtendedMinutes(),
                        gradeSummaryData.getExtendedMinutes()
                                .multiply(new BigDecimal(gradeSummaryData.getExtendedTotalNumberOfStudents()))));

        totalData.setImmersionTotalNumberOfStudents(
                computeSummOfTwoIntegers(totalData.getImmersionTotalNumberOfStudents(),
                        gradeSummaryData.getImmersionTotalNumberOfStudents()));
        totalData.setImmersionMinutes(
                computeSummOfTwoBigDecimals(totalData.getImmersionMinutes(),
                        gradeSummaryData.getImmersionMinutes()
                                .multiply(new BigDecimal(gradeSummaryData.getImmersionTotalNumberOfStudents()))));

    }

    /**
     * Adds values of 'gradeSummaryData' to total values(totalData).
     *
     * @param totalData FSLSummaryContainer
     * @param gradeSummaryData FSLSummaryContainer
     */
    private void addSecondaryDataToTotal(FSLSecondarySummaryContainer totalData,
                                         FSLSecondarySummaryContainer gradeSummaryData) {

        totalData.setCoreTotalNumberOfStudents(computeSummOfTwoIntegers(totalData.getCoreTotalNumberOfStudents(),
                gradeSummaryData.getCoreTotalNumberOfStudents()));

        totalData.setCoreCreditsOfIntermediateStudents(
                computeSummOfTwoBigDecimals(totalData.getCoreCreditsOfIntermediateStudents(),
                        gradeSummaryData.getCoreCreditsOfIntermediateStudents()));

        totalData.setCoreCreditsOfSeniorStudents(computeSummOfTwoBigDecimals(totalData.getCoreCreditsOfSeniorStudents(),
                gradeSummaryData.getCoreCreditsOfSeniorStudents()));

        totalData.setExtendedTotalNumberOfStudents(
                computeSummOfTwoIntegers(totalData.getExtendedTotalNumberOfStudents(),
                        gradeSummaryData.getExtendedTotalNumberOfStudents()));

        totalData.setImmersionTotalNumberOfStudents(
                computeSummOfTwoIntegers(totalData.getImmersionTotalNumberOfStudents(),
                        gradeSummaryData.getImmersionTotalNumberOfStudents()));

        totalData.setFrenchCourseIntermediateCredits(computeSummOfTwoBigDecimals(
                totalData.getFrenchCourseIntermediateCredits(), gradeSummaryData.getFrenchCourseIntermediateCredits()));

        totalData.setFrenchCourseSeniorCredits(computeSummOfTwoBigDecimals(totalData.getFrenchCourseSeniorCredits(),
                gradeSummaryData.getFrenchCourseSeniorCredits()));

        totalData.setOtherSubjectIntermediateCredits(computeSummOfTwoBigDecimals(
                totalData.getOtherSubjectIntermediateCredits(), gradeSummaryData.getOtherSubjectIntermediateCredits()));

        totalData.setOtherSubjectSeniorCredits(computeSummOfTwoBigDecimals(totalData.getOtherSubjectSeniorCredits(),
                gradeSummaryData.getOtherSubjectSeniorCredits()));
    }

    /**
     * Adds total values of current school to report.
     *
     * @param schoolName String
     * @param grid ReportDataGrid
     */
    private void addTotalFieldsToGrid(String schoolName, ReportDataGrid grid) {
        FSLTotalsContainer totalValues = m_schoolFSLTotalData.get(schoolName);
        if (totalValues != null) {
            grid.set(FSLRecord.FSLDetailFields.totalSchoolCore.toString(), totalValues.getTotalSchoolCore());
            grid.set(FSLRecord.FSLDetailFields.totalSchoolExtended.toString(),
                    totalValues.getTotalSchoolExtended());
            grid.set(FSLRecord.FSLDetailFields.totalSchoolImmersion.toString(),
                    totalValues.getTotalSchoolImmersion());
            grid.set(FSLRecord.FSLDetailFields.totalSchoolCounted.toString(),
                    Integer.valueOf(totalValues.getTotalSchoolImmersion() + totalValues.getTotalSchoolCore()
                            + totalValues.getTotalSchoolExtended()));
        }
    }

    /**
     * Computes credit value of provided 'StudentScheduleSpan' list(for
     * ''core/'extended'/'immersion'
     * courses).
     *
     * @param scheduleSpans List<StudentScheduleSpan>
     * @param courseLevel CourseLevel
     * @param preffixes String[]
     * @return BigDecimal
     */
    private BigDecimal computeCoreExtendedImmersionFrechSubjectCredits(List<StudentScheduleSpan> scheduleSpans,
                                                                       CourseLevel courseLevel,
                                                                       String... preffixes) {
        BigDecimal frenchSubjectCredits = null;
        for (StudentScheduleSpan scheduleSpan : scheduleSpans) {
            if (doComputeSpan(scheduleSpan)) {
                ReportSection section = (ReportSection) scheduleSpan.getSection();
                ToolCourse course = scheduleSpan.getSection().getCourse(getBroker());
                String courseNumber = section.getMinistryDefinedCourse();
                if (isCourseStartWithPreffix(courseNumber, preffixes)) {
                    boolean addCredits = false;
                    switch (courseLevel) {
                        case Intermediate:
                            if (NUM_1.equals(Character.toString(courseNumber.charAt(3)))
                                    || NUM_2.equals(Character.toString(courseNumber.charAt(3)))) {
                                addCredits = true;
                            }
                            break;
                        case Senior:
                            if (NUM_3.equals(Character.toString(courseNumber.charAt(3)))
                                    || NUM_4.equals(Character.toString(courseNumber.charAt(3)))) {
                                addCredits = true;
                            }
                            break;
                        default:
                            break;
                    }
                    if (addCredits) {
                        frenchSubjectCredits =
                                computeSummOfTwoBigDecimals(frenchSubjectCredits, course.getCourseCredit());
                    }
                }
            }
        }

        return frenchSubjectCredits;
    }

    /**
     * Computes credits of French courses.
     *
     * @param scheduleSpans List<StudentScheduleSpan>
     * @param courseLevel CourseLevel
     * @param pgmType PGMType
     * @return BigDecimal
     */
    private BigDecimal computeFrenchSubjectCredits(List<StudentScheduleSpan> scheduleSpans,
                                                   CourseLevel courseLevel,
                                                   PGMType pgmType) {
        BigDecimal frenchSubjectCredits = null;

        switch (pgmType) {
            case CORE:
                frenchSubjectCredits =
                        computeCoreExtendedImmersionFrechSubjectCredits(scheduleSpans, courseLevel,
                                COURSE_PREFFIX_FS);
                break;
            case EXTENDED_IMMERSION:
                frenchSubjectCredits =
                        computeCoreExtendedImmersionFrechSubjectCredits(scheduleSpans, courseLevel,
                                COURSE_PREFFIX_FE, COURSE_PREFFIX_FI);
                break;
            case OTHER:
                frenchSubjectCredits = computeOtherFrechSubjectCredits(scheduleSpans, courseLevel);
                break;
            default:
                break;
        }

        return frenchSubjectCredits != null ? frenchSubjectCredits : BigDecimal.ZERO;
    }

    /**
     * Computes count of French-subject credits.
     *
     * @param school OnSchool
     * @param enrollments Filterable<StudentEnrollment>
     * @param courseLevel CourseLevel
     * @param pgmType PGMType
     * @return BigDecimal
     */
    private BigDecimal computeFrenchSubjectCreditsByCourseLevelAndPGMType(OnSchool school,
                                                                          Filterable<OnEnrollment> enrollments,
                                                                          CourseLevel courseLevel,
                                                                          PGMType pgmType) {
        BigDecimal frenchSubjectCredits = null;
        BigDecimal creditsForStd = BigDecimal.ZERO;
        for (OnEnrollment enrollment : enrollments.extract()) {
            ReportStudent student = (ReportStudent) enrollment.getStudent(getBroker());
            List<StudentScheduleSpan> scheduleSpans = student.getStudentScheduleSpans(getBroker());
            creditsForStd = computeFrenchSubjectCredits(scheduleSpans, courseLevel, pgmType);
            if (creditsForStd != null && creditsForStd.compareTo(BigDecimal.ZERO) > 0) {
                if (m_creditsMap.containsKey(student.getOid())) {
                    BigDecimal existingCredits = m_creditsMap.get(student.getOid());
                    existingCredits = computeSummOfTwoBigDecimals(existingCredits, creditsForStd);
                } else {
                    m_creditsMap.put(student.getOid(), creditsForStd);
                }
            }
            frenchSubjectCredits = computeSummOfTwoBigDecimals(frenchSubjectCredits,
                    computeFrenchSubjectCredits(scheduleSpans, courseLevel, pgmType));
        }
        return frenchSubjectCredits;
    }

    /**
     * Computes count of French-subject credits.
     *
     * @param school OnSchool
     * @param enrollment StudentEnrollment
     * @param courseLevel CourseLevel
     * @param pgmType PGMType
     * @return BigDecimal
     */
    private BigDecimal computeFrenchSubjectCreditsByCourseLevelAndPGMType(OnSchool school,
                                                                          OnEnrollment enrollment,
                                                                          CourseLevel courseLevel,
                                                                          PGMType pgmType) {
        BigDecimal frenchSubjectCredits = BigDecimal.ZERO;
        ToolStudent student = enrollment.getStudent(getBroker());
        List<StudentScheduleSpan> scheduleSpans = student.getStudentScheduleSpans(getBroker());
        frenchSubjectCredits = computeFrenchSubjectCredits(scheduleSpans, courseLevel, pgmType);
        return frenchSubjectCredits;
    }


    /**
     * Computes credit value of provided 'StudentScheduleSpan' list(for 'other french' courses).
     *
     * @param scheduleSpans List<StudentScheduleSpan>
     * @param courseLevel CourseLevel
     * @return BigDecimal
     */
    private BigDecimal computeOtherFrechSubjectCredits(List<StudentScheduleSpan> scheduleSpans,
                                                       CourseLevel courseLevel) {
        BigDecimal frenchSubjectCredits = null;
        for (StudentScheduleSpan scheduleSpan : scheduleSpans) {
            if (doComputeSpan(scheduleSpan)) {
                ReportSection section = (ReportSection) scheduleSpan.getSection();
                ToolCourse course = section.getCourse(getBroker());
                String courseNumber = section.getMinistryDefinedCourse();
                String languageOfInstruction = section.getLanguageOfInstruction();
                if (!StringUtils.isEmpty(courseNumber) && !isCourseStartWithPreffix(courseNumber, COURSE_PREFFIX_FS,
                        COURSE_PREFFIX_FE, COURSE_PREFFIX_FI) && FRENCH_LANGUAGE_STATE.equals(languageOfInstruction)) {
                    CourseLevel currentCourseLevel = retrieveOtherCourseType(section);
                    if (currentCourseLevel == courseLevel) {
                        BigDecimal crsCredit = course.getCourseCredit();
                        if (scheduleSpan.getExitDate() != null
                                && scheduleSpan.getExitDate().before(getSubmissionType().getPeriodEndDate())) {
                            crsCredit = BigDecimal.ZERO;
                        }
                        frenchSubjectCredits = computeSummOfTwoBigDecimals(frenchSubjectCredits, crsCredit);
                    }
                }
            }
        }

        return frenchSubjectCredits;
    }

    /**
     * Computes sum of two BigDecimal values(include null cases).
     *
     * @param num1 BigDecimal
     * @param num2 BigDecimal
     * @return BigDecimal
     */
    private BigDecimal computeSummOfTwoBigDecimals(BigDecimal num1, BigDecimal num2) {
        if (num1 == null) {
            return num2;
        }
        if (num2 == null) {
            return num1;
        }

        return num1.add(num2);
    }

    /**
     * Computes sum of two integer values(include null cases).
     *
     * @param num1 Integer
     * @param num2 Integer
     * @return Integer
     */
    private Integer computeSummOfTwoIntegers(Integer num1, Integer num2) {
        if (num1 == null) {
            return num2;
        }
        if (num2 == null) {
            return num1;
        }

        return num1 + num2;
    }

    /**
     * Do compute span.
     *
     * @param sscSpan StudentScheduleSpan
     * @return true, if successful
     */
    private boolean doComputeSpan(StudentScheduleSpan sscSpan) {
        boolean toCompute = false;
        ReportSection section = (ReportSection) sscSpan.getSection();
        String courseNumber = section.getMinistryDefinedCourse();
        String languageOfInstruction = section.getLanguageOfInstruction();
        if (!StringUtils.isEmpty(courseNumber)
                && isCourseStartDateValid(sscSpan.getEntryDate(), sscSpan.getExitDate())
                && (isCourseStartWithPreffix(courseNumber, COURSE_PREFFIX_FS,
                        COURSE_PREFFIX_FE, COURSE_PREFFIX_FI)
                        || (!StringUtils.isEmpty(languageOfInstruction)
                                && FRENCH_LANGUAGE_STATE.equals(languageOfInstruction)))) {
            toCompute = true;
        }
        return toCompute;
    }

    /**
     * Format string decimal number.
     *
     * @param number String
     * @return String
     */
    private String formatStringDecimal(String number) {
        return !StringUtils.isEmpty(number) && number.matches(DECIMAL_MATCHER)
                ? new BigDecimal(number).setScale(2, BigDecimal.ROUND_HALF_UP).toString()
                : "0.00";
    }

    /**
     * Gets the arrival status.
     *
     * @param enrollment StudentEnrollment
     * @param ssk OnStudentSchool
     * @return String
     */
    private String getArrivalStatus(OnEnrollment enrollment, OnStudentSchool ssk) {
        String result = enrollment == null ? "" : enrollment.getArrivalStatus();
        if (StringUtils.isEmpty(result) && ssk != null) {
            result = ssk.getArrivalStatus();
        }
        return result;
    }

    /**
     * Gets the board residence status code for enrollment.
     *
     * @param enrollment StudentEnrollment
     * @return String
     */
    private String getBoardResStatus(OnEnrollment enrollment) {
        DataDictionaryField dictField = OnEnrollment.FIELD_BRD_RES_STAT_TYPE.getField(getDictExtractor());
        Map<String, ReferenceCode> refCodes = getDictExtractor().getReferenceCodes(dictField.getReferenceTableOid());
        if (refCodes == null) {
            throw new RuntimeException("Cannot find reference table for field [" + dictField.getJavaName() + "]");
        }
        ReferenceCode refCode = refCodes.get(enrollment.getBoardResidentStatusPlain());
        if (refCode == null) {
            ReportStudent student = (ReportStudent) enrollment.getStudent(getBroker());
            String errorMessage = "Cannot determine reference code for enrollment of student "
                    + getStudentNameFirstMiddleLast(student)
                    + " "
                    + student.getOenRaw()
                    + " for field " + dictField.getUserLongName();
            throw new DataErrorException(errorMessage);
        }
        return refCode.getCode();
    }

    /**
     * Gets the fsl records.
     *
     * @param school OnSchool
     * @param students Filterable<ReportStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<FSLRecord> getFrechAsSecondLanguageRecord(final OnSchool school,
                                                                 Filterable<ReportStudent> students,
                                                                 Collection<String> sklOidsToReport) {
        List<OnEnrollment> enrollments = new ArrayList<>();
        ReportType reportType = getReportType();
        for (ReportStudent student : students.extract()) {
            OnEnrollment recentEnrolment = null;
            List<AnnualSpan> annualSpans =
                    student.getEnrollmentSpans(getBroker(), true, true);
            Collections.sort(annualSpans, new Comparator<AnnualSpan>() {
                @Override
                public int compare(AnnualSpan span1, AnnualSpan span2) {
                    PlainDate date1 = span1.getFirstActiveInSessionDate();
                    PlainDate date2 = span2.getFirstActiveInSessionDate();
                    return date1.compareTo(date2);
                }
            });

            if (annualSpans != null && !annualSpans.isEmpty()) {
                OnAnnualSpan firstAnnualSpan =
                        (OnAnnualSpan) ToolsSharedContainer.reverse(annualSpans.stream()).findFirst().get();
                if (firstAnnualSpan != null) {
                    recentEnrolment = (OnEnrollment) firstAnnualSpan.getRecentEnrollmentESY();
                    if (recentEnrolment == null && firstAnnualSpan.isSecondary()
                            && sklOidsToReport.contains(firstAnnualSpan.getSecondary().getSchool(getBroker()).getOid())
                            && firstAnnualSpan.getBestPrimarySpanFor(getBroker()) != null) {
                        recentEnrolment = (OnEnrollment) firstAnnualSpan.getBestPrimarySpanFor(getBroker())
                                .getRecentEnrollmentESY();
                        m_sskByStdOid.put(student.getOid(), (ReportStudentSchool) firstAnnualSpan.getSecondary());
                    }
                }
            }
            if (recentEnrolment != null && (reportType == ReportType.DETAIL ||
                    !isFullyHighCredit(student, school))) {
                enrollments.add(recentEnrolment);
            }
        }
        Filterable<OnEnrollment> filtrableEnrollments = FilterableFactory.createFilterableToolBeans(enrollments).filter(
                new Filter<OnEnrollment>() {
                    @Override
                    public boolean isFiltered(OnEnrollment toFilter) {
                        try {
                            String residenceStatus = getBoardResStatus(toFilter);
                            return !StringUtils.isEmpty(residenceStatus) && (reportType == ReportType.DETAIL ||
                                    BOARD_RES_STATUS_PUPIL_OF_THE_BOARD.equals(residenceStatus));
                        } catch (DataErrorException e) {
                            logError(e.getMessage());
                        }
                        return false;
                    }
                });
        Filterable<OnEnrollment> enrollmentsWithPGMCodes =
                filtrableEnrollments.filter(getStudentEnrollmentsWithNonEmptyPGMType(reportType));
        List<FSLRecord> frenchAsSecondLanguageRecords = new ArrayList<FSLRecord>();
        try {
            if (reportType == ReportType.DETAIL) {
                for (OnEnrollment enrollment : enrollmentsWithPGMCodes.extract()) {
                    ReportStudent std = (ReportStudent) enrollment.getStudent(getBroker());
                    OnStudentSLPProgram studentProgramParticipation = std.getSlpPrograms(getBroker()).stream()
                            .filter(pgmToFilter -> school.getOid().equals(
                                    pgmToFilter.getSchoolOid())
                                    && PGM_TYPE_FSL.equalsIgnoreCase(pgmToFilter.getProgramType())
                                    && PGM_CODE_SLP.equalsIgnoreCase(pgmToFilter.getProgramCodeBeanValue())
                                    && !getStartDate().before(pgmToFilter.getStartDate())
                                    && (pgmToFilter.getEndDate() == null
                                            || !pgmToFilter.getEndDate()
                                                    .before(getStartDate())))
                            .findFirst()
                            .orElse(null);
                    String minutesPerInstDay = null;
                    String programCode = null;
                    if (studentProgramParticipation != null) {
                        minutesPerInstDay = studentProgramParticipation.getMinutesOfInstruction().toString();
                        programCode = studentProgramParticipation.getProgramCodeSLP();
                    }
                    boolean toReportRecord = studentProgramParticipation != null;
                    if (toReportRecord) {
                        FSLRecord frenchAsSecondLanguageRecord = new FSLRecord();
                        initFSLRecordCommonData(school, frenchAsSecondLanguageRecord);
                        initFSLRecordDetailData(school, frenchAsSecondLanguageRecord, enrollment, minutesPerInstDay,
                                programCode);
                        frenchAsSecondLanguageRecords.add(frenchAsSecondLanguageRecord);
                    }
                }
                FSLTotalsContainer totalValues = new FSLTotalsContainer();
                totalValues.setTotalSchoolCore(m_coreProgramsCount);
                totalValues.setTotalSchoolExtended(m_extendedProgramsCount);
                totalValues.setTotalSchoolImmersion(m_immersionProgramsCount);
                m_schoolFSLTotalData.put(school.getName(), totalValues);
            } else if (isElementaryReport()) {
                FSLRecord frenchAsSecondLanguageRecord = new FSLRecord();
                initFSLRecordCommonData(school, frenchAsSecondLanguageRecord);
                initFSLRecordElementarySummaryData(school, frenchAsSecondLanguageRecord, enrollmentsWithPGMCodes);
                frenchAsSecondLanguageRecords.add(frenchAsSecondLanguageRecord);
            } else {
                FSLRecord frenchAsSecondLanguageRecord = new FSLRecord();
                initFSLRecordCommonData(school, frenchAsSecondLanguageRecord);
                initFSLRecordSecondarySummaryData(school, frenchAsSecondLanguageRecord, enrollmentsWithPGMCodes);
                frenchAsSecondLanguageRecords.add(frenchAsSecondLanguageRecord);
            }
        } catch (DataErrorException e) {
            logError(e.getMessage());
        }
        return FilterableFactory.create(frenchAsSecondLanguageRecords, FSLRecord.s_uniqueFields,
                FSLRecord.s_valueResolver);
    }

    /**
     * Gets the report date for sec detail.
     *
     * @return Plain date
     */
    private PlainDate getReportDateForSecDetail() {
        if (m_reportDateForSecDetail == null) {
            m_reportDateForSecDetail = getSubmissionType().getPeriodEndDate();
        }
        return m_reportDateForSecDetail;
    }

    /**
     * Retrieve running report type.
     *
     * @return ReportType
     */
    private ReportType getReportType() {
        return getJob().getTool().getId().contains(ReportType.DETAIL.toString()) ? ReportType.DETAIL
                : ReportType.SUMMARY;
    }

    /**
     * Gets the students for minutes for program elementary.
     *
     * @param enrollments Filterable<StudentEnrollment>
     * @return Key value pair
     */
    private KeyValuePair<BigDecimal, Integer> getStudentsFoMinutesForProgramElementary(Filterable<OnEnrollment> enrollments) {
        Map<BigDecimal, Integer> stdForMinutesMap = new HashMap<BigDecimal, Integer>();
        for (OnEnrollment enrollment : enrollments.extract()) {
            ReportStudent std = (ReportStudent) enrollment.getStudent(getBroker());
            OnStudentSLPProgram studentProgramParticipation = std.getSlpPrograms(getBroker()).stream()
                    .filter(pgmToFilter -> enrollment.getSchoolOid().equals(
                            pgmToFilter.getSchoolOid())
                            && PGM_TYPE_FSL.equalsIgnoreCase(pgmToFilter.getProgramType())
                            && PGM_CODE_SLP.equalsIgnoreCase(pgmToFilter.getProgramCodeBeanValue())
                            && !m_sklDateRangeProvider.getStartDate().before(pgmToFilter.getStartDate())
                            && (pgmToFilter.getEndDate() == null
                                    || !pgmToFilter.getEndDate()
                                            .before(m_sklDateRangeProvider.getStartDate())))
                    .findFirst().orElse(null);
            BigDecimal minutesPerInstDay = null;
            if (studentProgramParticipation != null) {
                minutesPerInstDay = studentProgramParticipation.getMinutesOfInstruction();
            }
            if (minutesPerInstDay != null) {
                BigDecimal minutesKey = minutesPerInstDay;
                Integer stdCount = stdForMinutesMap.get(minutesKey);
                if (stdCount == null) {
                    stdCount = Integer.valueOf(1);
                    stdForMinutesMap.put(minutesKey, stdCount);
                } else {
                    stdForMinutesMap.put(minutesKey, Integer.valueOf(stdCount.intValue() + 1));
                }
            }
        }
        int candidateAmount = 0;
        BigDecimal candidateMinutes = BigDecimal.ZERO;
        if (!stdForMinutesMap.isEmpty()) {
            for (BigDecimal minutesKey : stdForMinutesMap.keySet()) {
                int stdAmountForKey = stdForMinutesMap.get(minutesKey);
                candidateAmount += stdAmountForKey;
                candidateMinutes = computeSummOfTwoBigDecimals(candidateMinutes,
                        new BigDecimal(minutesKey.doubleValue() * stdAmountForKey));
            }
        }
        if (candidateAmount != 0) {
            candidateMinutes = candidateMinutes.divide(
                    new BigDecimal(candidateAmount), 2, RoundingMode.HALF_UP);

        } else {
            candidateMinutes = BigDecimal.ZERO;
        }
        return new KeyValuePair<BigDecimal, Integer>(candidateMinutes.setScale(2), Integer.valueOf(candidateAmount));
    }

    /**
     * Returns size of filterable records(null, if size is 0).
     *
     * @param enrollments Filterable<StudentEnrollment>
     * @return Integer
     */
    private Integer getSizeOfFiltrable(Filterable<OnEnrollment> enrollments) {
        return enrollments.extract().size() == 0 ? Integer.valueOf(0) : enrollments.extract().size();
    }

    /**
     * Gets the start date.
     *
     * @return Plain date
     */
    private PlainDate getStartDate() {
        return isElementaryReport() ? getReportDate() : getReportDateForSecDetail();
    }

    /**
     * Creates filter to retrieve data with appropriate
     * grade level and age.
     *
     * @param gradeLevels List<String>
     * @param is21Plus boolean
     * @return Filter
     */
    private Filter<OnEnrollment> getStudnetEnrollmentsByGradeLevels(List<String> gradeLevels,
                                                                    boolean is21Plus) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                ReportStudent student = (ReportStudent) toFilter.getStudent(getBroker());
                int yog = getYog(student, toFilter);
                String grade = m_gradesHelper.getGradeLevel(yog);
                if (is21Plus) {
                    return is21plus(student) && gradeLevels.contains(grade) && isCountedStudent(toFilter);
                }
                return gradeLevels.contains(grade) && isCountedStudent(toFilter);
            }
        };
    }

    /**
     * Creates filter to retrieve data with appropriate program types.
     *
     * @param pgmTypes List<String>
     * @param school OnSchool
     * @return Filter
     */
    private Filter<OnEnrollment> getStudnetEnrollmentsByPGMType(List<String> pgmTypes, OnSchool school) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    ReportStudent student = (ReportStudent) toFilter.getStudent(getBroker());
                    OnStudentSLPProgram studentProgramParticipation = student.getSlpPrograms(getBroker()).stream()
                            .filter(pgmToFilter -> school.getOid().equals(
                                    pgmToFilter.getSchoolOid())
                                    && PGM_TYPE_FSL.equalsIgnoreCase(pgmToFilter.getProgramType())
                                    && PGM_CODE_SLP
                                            .equalsIgnoreCase(pgmToFilter.getProgramCodeBeanValue())
                                    && pgmTypes.contains(pgmToFilter.getProgramCodeSLP())
                                    && !m_sklDateRangeProvider.getStartDate().before(pgmToFilter.getStartDate())
                                    && (pgmToFilter.getEndDate() == null
                                            || !pgmToFilter.getEndDate()
                                                    .before(m_sklDateRangeProvider.getStartDate())))
                            .findFirst().orElse(null);
                    if (studentProgramParticipation == null) {
                        return false;
                    }
                    return isCountedStudent(toFilter);
                } catch (Exception e) {
                    return false;
                }
            }
        };
    }

    /**
     * Creates filter to retrieve data with non empty PGM code.
     *
     * @param reportType ReportType
     * @return Filter
     */
    private Filter<OnEnrollment> getStudentEnrollmentsWithNonEmptyPGMType(ReportType reportType) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    ReportStudent student = (ReportStudent) toFilter.getStudent(getBroker());
                    OnStudentSLPProgram studentProgramParticipation = student.getSlpPrograms(getBroker()).stream()
                            .filter(pgmToFilter -> PGM_TYPE_FSL.equalsIgnoreCase(pgmToFilter.getProgramType())
                                    && PGM_CODE_SLP.equalsIgnoreCase(pgmToFilter.getProgramCodeBeanValue())
                                    && !m_sklDateRangeProvider.getStartDate()
                                            .before(pgmToFilter.getStartDate())
                                    &&
                                    (pgmToFilter.getEndDate() == null
                                            || !pgmToFilter.getEndDate()
                                                    .before(m_sklDateRangeProvider.getStartDate())))
                            .findFirst().orElse(null);
                    if (reportType == ReportType.DETAIL) {
                        return studentProgramParticipation != null;
                    }
                    return studentProgramParticipation != null && isCountedStudent(toFilter);
                } catch (Exception e) {
                    return false;
                }
            }
        };
    }

    /**
     * Initializes title/header data of FSL report.
     *
     * @param school OnSchool
     * @param frenchAsSecondLanguageRecord FSLRecord
     */
    private void initFSLRecordCommonData(OnSchool school, FSLRecord frenchAsSecondLanguageRecord) {
        try {
            String district = getOrganization().getName();
            String schoolName = school.getName();
            String schoolNumber = school.getBsid();
            ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
            String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
            String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
            String academicYears = getCurrentContext().getContextId();
            String programName = "Aspen";
            String currentDate = m_formatter
                    .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.district, district);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.schoolName, schoolName);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.schoolNumber, schoolNumber);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.boardNumber, boardNumber);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.boardName, boardName);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.academicYears, academicYears);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.programName, programName);
            frenchAsSecondLanguageRecord.setCommonField(FSLRecord.FSLCommonFields.currentDate, currentDate);
        } catch (DataErrorException e) {
            logError(e.getMessage());
        }
    }

    /**
     * Initializes data for FSL detail report.
     *
     * @param school OnSchool
     * @param frenchAsSecondLanguageRecord FSLRecord
     * @param enrollment StudentEnrollment
     * @param minutesPerInstDay String
     * @param programCode String
     */
    private void initFSLRecordDetailData(OnSchool school,
                                         FSLRecord frenchAsSecondLanguageRecord,
                                         OnEnrollment enrollment,
                                         String minutesPerInstDay,
                                         String programCode) {
        try {
            ReportStudent student = (ReportStudent) enrollment.getStudent(getBroker());
            String residenceStatusCode = getBoardResStatus(enrollment);
            boolean isCountedByCredits = true;
            if (isElementaryReport()) {
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.numberOfMinutes,
                        formatStringDecimal(minutesPerInstDay));
            } else {
                BigDecimal creditsSum = BigDecimal.ZERO;
                BigDecimal value = computeFrenchSubjectCreditsByCourseLevelAndPGMType(school, enrollment,
                        CourseLevel.Intermediate, PGMType.CORE);
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.coreInterCredits,
                        value.setScale(2, BigDecimal.ROUND_HALF_UP).toString());
                value = computeFrenchSubjectCreditsByCourseLevelAndPGMType(school, enrollment,
                        CourseLevel.Senior, PGMType.CORE);
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.coreSeniorCredits,
                        value.setScale(2, BigDecimal.ROUND_HALF_UP).toString());
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                value = computeFrenchSubjectCreditsByCourseLevelAndPGMType(school, enrollment,
                        CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION);
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.immersionInterCredits,
                        value.setScale(2, BigDecimal.ROUND_HALF_UP).toString());
                value = computeFrenchSubjectCreditsByCourseLevelAndPGMType(school, enrollment,
                        CourseLevel.Senior, PGMType.EXTENDED_IMMERSION);
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.immersionSeniorCredits,
                        value.setScale(2, BigDecimal.ROUND_HALF_UP).toString());
                value = computeFrenchSubjectCreditsByCourseLevelAndPGMType(school, enrollment,
                        CourseLevel.Intermediate, PGMType.OTHER);
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.otherInterCredits,
                        value.setScale(2, BigDecimal.ROUND_HALF_UP).toString());
                value = computeFrenchSubjectCreditsByCourseLevelAndPGMType(school, enrollment,
                        CourseLevel.Senior, PGMType.OTHER);
                creditsSum = computeSummOfTwoBigDecimals(creditsSum, value);
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.otherSeniorCredits,
                        value.setScale(2, BigDecimal.ROUND_HALF_UP).toString());
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.isOver21Student,
                        Boolean.valueOf(is21plus(student)));
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.isElearningStudent,
                        Boolean.valueOf(BOARD_RES_STATUS_ELEARNING.equals(residenceStatusCode)));
                frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.isFullyHighCredit,
                        isFullyHighCredit(student, school));
                isCountedByCredits = creditsSum.compareTo(BigDecimal.ZERO) > 0;
            }
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.secondLanguageProgramType,
                    programCode);
            boolean isShared = BOARD_RES_STATUS_SHARED.equals(residenceStatusCode) ||
                    (m_sskByStdOid.containsKey(student.getOid())
                            && m_sskByStdOid.get(student.getOid()).getSchool(getBroker()).getOid()
                                    .equals(school.getOid()));
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.isSharedStudent,
                    Boolean.valueOf(isShared));
            boolean isCounted = isCountedStudent(enrollment) && !isShared && isCountedByCredits;
            boolean isOther = !BOARD_RES_STATUS_SHARED.equals(residenceStatusCode)
                    && !BOARD_RES_STATUS_PUPIL_OF_THE_BOARD.equals(residenceStatusCode)
                    && !BOARD_RES_STATUS_ELEARNING.equals(residenceStatusCode);
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.isOtherPupil, isOther);
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.counted, Boolean.valueOf(isCounted));
            if (isCounted) {
                addCountOfFSLProgram(programCode);
            }
            int yog = getYog(student, enrollment);
            String grade =
                    m_gradesHelper.getGradeLevel(yog) != null ? m_gradesHelper.getGradeLevel(yog) : "dummyContent";
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.grade, grade);
            if (m_gradesMap.containsKey(grade)) {
                String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                        ALIAS_DDX_NUM_GRADE, m_dictExtractor.getDictionary(DDX_ID_GRADES));
                if (!StringUtils.isEmpty(numGradeLevel)) {
                    frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.gradeToSort,
                            Integer.valueOf(numGradeLevel));
                } else {
                    frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.gradeToSort,
                            Integer.valueOf(-99));
                }
            }
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.studentName,
                    getStudentNameFirstMiddleLast(student));
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.studentNameToSort,
                    getStudentNameLastFirstMiddle(student));
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.oen,
                    student.getOenRaw());
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.gender,
                    student.getGenderType());
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.age,
                    String.valueOf(student.getAgeAsOfDate(m_sklDateRangeProvider.getStartDate())));
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.boardResidenceStatus,
                    getDescription(enrollment, OnEnrollment.FIELD_BRD_RES_STAT_TYPE));
            String arrivalStatus =
                    enrollment == null ? "" : getArrivalStatus(enrollment, m_sskByStdOid.get(student.getOid()));
            frenchAsSecondLanguageRecord.setDetailField(FSLRecord.FSLDetailFields.arrivalStatus, arrivalStatus);
        } catch (DataErrorException e) {
            logError(e.getMessage());
        }
    }

    /**
     * Initializes Summary data of FSL report for elementary schools.
     *
     * @param school OnSchool
     * @param frenchAsSecondLanguageRecord FSLRecord
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     */
    private void initFSLRecordElementarySummaryData(OnSchool school,
                                                    FSLRecord frenchAsSecondLanguageRecord,
                                                    Filterable<OnEnrollment> filtrableEnrollments) {
        try {
            FSLElementarySummaryContainer totalContainer =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.totalCount.key());
            FSLElementarySummaryContainer gradeJKData =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.jk.key());
            retrieveElementaryGradeData(school, gradeJKData, GRADE_LEVEL_JK, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer gradeSKData =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.sk.key());
            retrieveElementaryGradeData(school, gradeSKData, GRADE_LEVEL_SK, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade01Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade01Data.key());
            retrieveElementaryGradeData(school, grade01Data, GRADE_LEVEL_01, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade02Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade02Data.key());
            retrieveElementaryGradeData(school, grade02Data, GRADE_LEVEL_02, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade03Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade03Data.key());
            retrieveElementaryGradeData(school, grade03Data, GRADE_LEVEL_03, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade04Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade04Data.key());
            retrieveElementaryGradeData(school, grade04Data, GRADE_LEVEL_04, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade05Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade05Data.key());
            retrieveElementaryGradeData(school, grade05Data, GRADE_LEVEL_05, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade06Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade06Data.key());
            retrieveElementaryGradeData(school, grade06Data, GRADE_LEVEL_06, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade07Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade07Data.key());
            retrieveElementaryGradeData(school, grade07Data, GRADE_LEVEL_07, filtrableEnrollments, totalContainer);
            FSLElementarySummaryContainer grade08Data =
                    new FSLElementarySummaryContainer(FSLRecord.FSLSummaryFieldElementary.grade08Data.key());
            retrieveElementaryGradeData(school, grade08Data, GRADE_LEVEL_08, filtrableEnrollments, totalContainer);
            if (totalContainer.getCoreTotalNumberOfStudents() != null
                    && 0 != totalContainer.getCoreTotalNumberOfStudents().intValue()) {
                totalContainer.setCoreMinutes(totalContainer.getCoreMinutes().divide(
                        new BigDecimal(totalContainer.getCoreTotalNumberOfStudents()), 2, RoundingMode.HALF_UP));
            }
            if (totalContainer.getExtendedTotalNumberOfStudents() != null
                    && 0 != totalContainer.getExtendedTotalNumberOfStudents().intValue()) {
                totalContainer.setExtendedMinutes(totalContainer.getExtendedMinutes().divide(
                        new BigDecimal(totalContainer.getExtendedTotalNumberOfStudents()), 2, RoundingMode.HALF_UP));
            }
            if (totalContainer.getImmersionTotalNumberOfStudents() != null
                    && 0 != totalContainer.getImmersionTotalNumberOfStudents().intValue()) {
                totalContainer.setImmersionMinutes(totalContainer.getImmersionMinutes().divide(
                        new BigDecimal(totalContainer.getImmersionTotalNumberOfStudents()), 2, RoundingMode.HALF_UP));
            }

            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.jk, gradeJKData);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.sk, gradeSKData);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade01Data,
                    grade01Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade02Data,
                    grade02Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade03Data,
                    grade03Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade04Data,
                    grade04Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade05Data,
                    grade05Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade06Data,
                    grade06Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade07Data,
                    grade07Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.grade08Data,
                    grade08Data);
            frenchAsSecondLanguageRecord.setSummaryFieldElem(FSLRecord.FSLSummaryFieldElementary.totalCount,
                    totalContainer);
        } catch (DataErrorException e) {
            logError(e.getMessage());
        }
    }

    /**
     * Initializes Summary data of FSL report for secondary schools.
     *
     * @param school OnSchool
     * @param frenchAsSecondLanguageRecord FSLRecord
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     */
    private void initFSLRecordSecondarySummaryData(OnSchool school,
                                                   FSLRecord frenchAsSecondLanguageRecord,
                                                   Filterable<OnEnrollment> filtrableEnrollments) {
        try {
            FSLSecondarySummaryContainer totalContainer =
                    new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.totalCount.key());
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.preGrade9,
                    retrievePreGrade9Data(school, filtrableEnrollments, totalContainer));
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.grade9Data,
                    retrieveGrade9Data(school, filtrableEnrollments, totalContainer));
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.grade10Data,
                    retrieveGrade10Data(school, filtrableEnrollments, totalContainer));
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.grade11Data,
                    retrieveGrade11Data(school, filtrableEnrollments, totalContainer));
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.grade12Data,
                    retrieveGrade12Data(school, filtrableEnrollments, totalContainer));
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.student21OrOver,
                    retrieve21PlusData(school, filtrableEnrollments));
            frenchAsSecondLanguageRecord.setSummaryFieldSec(FSLRecord.FSLSummaryFieldSecondary.totalCount,
                    totalContainer);
        } catch (DataErrorException e) {
            logError(e.getMessage());
        }
    }

    /**
     * Inits the grades map.
     */
    private void initGradesMap() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        DataDictionaryField dictionaryField =
                m_dictExtractor.getDataDictionaryField(ReportStudent.class, SisStudent.COL_GRADE_LEVEL);
        String referenceTableOid = null;
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            referenceTableOid = dictionaryField.getDataFieldConfig().getReferenceTableOid();
            X2Criteria refTablecriteria = new X2Criteria();
            refTablecriteria.addEqualTo(X2BaseBean.COL_OID, referenceTableOid);
            ReferenceTable gradeRefTable =
                    getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, refTablecriteria));
            if (gradeRefTable != null) {
                Map<String, ReferenceCode> gradesMap = gradeRefTable.getCodeMap();
                for (Entry<String, ReferenceCode> entry : gradesMap.entrySet()) {
                    String stateGradeCode = entry.getValue().getStateCode();
                    if (!StringUtils.isEmpty(stateGradeCode)) {
                        m_gradesMap.put(stateGradeCode, entry.getValue());
                    }
                }
            }
        }
    }

    /**
     * Initialize helpers.
     *
     * @param school OnSchool
     */
    private void initializeHelpersForSchool(OnSchool school) {
        m_sklDateRangeProvider = new SchoolDateRangeProvider(school);
        m_gradesHelper = new GradesHelper(m_sklDateRangeProvider);

        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true);

        if (!isElementaryReport()) {
            m_annualSpanFilter = new Predicate<AnnualSpan>() {
                @Override
                public boolean test(AnnualSpan span) {
                    PlainDate spanStartDate = span.getFirstActiveInSessionDate();
                    PlainDate reportStartDate = getSubmissionType().getPeriodEndDate();
                    return !reportStartDate.before(spanStartDate);
                }
            };
        }
    }

    /**
     * Checks if is counted student.
     *
     * @param toFilter StudentEnrollment
     * @return true, if is counted student
     */
    private boolean isCountedStudent(OnEnrollment toFilter) {
        boolean isCountedBySpan = false;
        if (!isElementaryReport()) {

            List<StudentScheduleSpan> scheduleSpans =
                    toFilter.getStudent(getBroker()).getStudentScheduleSpans(getBroker());
            if (scheduleSpans != null && !scheduleSpans.isEmpty()) {
                for (StudentScheduleSpan sscSpan : scheduleSpans) {
                    if (sscSpan.getEntryDate() != null && DateUtils.isBetween(sscSpan.getEntryDate(),
                            getSubmissionType().getPeriodStartDate(), getSubmissionType().getPeriodEndDate())) {
                        isCountedBySpan = true;
                        break;
                    }
                }
            }
        }
        boolean isCountedByStatus = BOARD_RES_STATUS_PUPIL_OF_THE_BOARD.equals(getBoardResStatus(toFilter));
        return isElementaryReport() ? isCountedByStatus : isCountedByStatus && isCountedBySpan;
    }

    /**
     * Checks if course date is valid or no.
     *
     * @param entryDate PlainDate
     * @param exitDate PlainDate
     * @return true, if is course start date valid
     */
    private boolean isCourseStartDateValid(PlainDate entryDate, PlainDate exitDate) {
        if (entryDate != null) {
            boolean isStartDateValid = !entryDate.after(m_sklDateRangeProvider.getEndDate())
                    && !entryDate.after(m_sklDateRangeProvider.getStartDate());
            boolean isEndDateValid = false;
            if (isElementaryReport()) {
                isEndDateValid = exitDate == null || !exitDate.before(m_sklDateRangeProvider.getStartDate());
            } else {
                isEndDateValid = exitDate == null || !exitDate.before(getSubmissionType().getPeriodStartDate());
            }
            if (isStartDateValid && isEndDateValid) {
                return true;
            }
            return false;
        }
        return true;
    }

    /**
     * Checks if course starts with provided prefixes or no.
     *
     * @param courseNumber String
     * @param prefixes String[]
     * @return true, if is course start with preffix
     */
    private boolean isCourseStartWithPreffix(String courseNumber, String... prefixes) {
        if (!StringUtils.isEmpty(courseNumber)) {
            for (String prefix : prefixes) {
                if (courseNumber.startsWith(prefix)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if is fully high credit.
     *
     * @param student ReportStudent
     * @param school OnSchool
     * @return true, if is fully high credit
     */
    private boolean isFullyHighCredit(ReportStudent student, OnSchool school) {
        boolean isFullyHC = false;
        Double highCreditFte = Double.valueOf(0.0d);
        FteMonthly montlyFteRecordFoStd = getFteMonthlyRecord(getBroker(), school, student);
        if (montlyFteRecordFoStd != null) {
            highCreditFte = montlyFteRecordFoStd.getFteHc().doubleValue();
        } else {
            FteRecord fteRecordC = findFteRecord(student.getFteRecords(getBroker()), school);
            if (fteRecordC != null) {
                highCreditFte = fteRecordC.getFteHc().doubleValue();
            }
        }
        if (highCreditFte.compareTo(0.0d) > 0) {
            isFullyHC = true;
        }
        return isFullyHC;
    }

    /**
     * Preload student data for school
     */
    private void preloadStudentsData() {
        ToolBean.clearAllCachedToolBeans(FteMonthly.class);
        ToolBean.clearAllCachedToolBeans(FteRecord.class);
        ToolBean.clearAllCachedToolBeans(OnStudentSLPProgram.class);
        ToolBean.clearAllCachedToolBeans(ToolStudentSchedule.class);
        ToolBean.clearAllCachedToolBeans(ToolStudentScheduleChange.class);
        ToolBean.clearAllCachedToolBeans(ToolTranscript.class);

        ToolBean.resetCriteria(getBroker(), FteMonthly.class);
        ToolBean.resetCriteria(getBroker(), FteRecord.class);
        ToolBean.resetCriteria(getBroker(), OnStudentSLPProgram.class);
        ToolBean.resetCriteria(getBroker(), ToolStudentSchedule.class);
        ToolBean.resetCriteria(getBroker(), ToolStudentScheduleChange.class);
        ToolBean.resetCriteria(getBroker(), ToolTranscript.class);

        EnrollmentSpanCriteria spanCriteria = new EnrollmentSpanCriteria()
                .setExcludeSection(OnSection.FIELD_MST_EXCLUDE_FROM_ONSIS)
                .setCurrentContext(m_sklDateRangeProvider.getContextByDate(m_sklDateRangeProvider.getStartDate()));

        // preload schedules
        ToolBean.addAndCriteria(getBroker(), ToolStudentSchedule.class,
                CriteriaHelper.buildStudentScheduleCriteria(spanCriteria));
        ToolBean.addAndCriteria(getBroker(), ToolStudentScheduleChange.class,
                CriteriaHelper.buildStudentScheduleChangeCriteria(spanCriteria));
        ToolBean.addAndCriteria(getBroker(), ToolTranscript.class,
                CriteriaHelper.buildStudentTranscriptCriteria(spanCriteria, getBroker()));

        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentSchedule.FIELD_STUDENT_OID, ToolStudentSchedule.FIELD_SECTION_OID),
                ToolStudentSchedule.PARENT_STUDENT);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentScheduleChange.FIELD_STUDENT_OID, ToolStudentScheduleChange.FIELD_SECTION_OID,
                        ToolStudentScheduleChange.FIELD_EFFECTIVE_DATE, ToolStudentScheduleChange.FIELD_TIMESTAMP,
                        ToolStudentScheduleChange.FIELD_CHANGE_TYPE_CODE),
                ToolStudentScheduleChange.PARENT_STUDENT);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolTranscript.FIELD_STUDENT_OID, ToolTranscript.FIELD_SECTION_OID),
                ToolTranscript.PARENT_STUDENT);

        Set<String> sectionOids = Stream.concat(Stream.concat(
                ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream().map(ssc -> ssc.getSectionOid()),
                ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream()
                        .map(scc -> scc.getSectionOid())),
                ToolBean.getCachedToolBeans(ToolTranscript.class).stream().map(trn -> trn.getSectionOid()))
                .collect(Collectors.toSet());
        ToolBean.loadByOid(getBroker(), getDictExtractor(), ReportSection.class, sectionOids);

        // preload fte
        ToolOrganization organization =
                ToolBean.getBeanByOid(getBroker(), ToolOrganization.class, getOrganization().getOid(),
                        false);
        X2Criteria fteMontlyCriteria = new X2Criteria();
        fteMontlyCriteria.addEqualTo(FteMonthly.FIELD_REPORT_DATE.resolve(getDictExtractor()),
                s_aliasDateFormat.format(m_sklDateRangeProvider.getStartDate()));
        ToolBean.addAndCriteria(getBroker(), FteMonthly.class, fteMontlyCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(), Arrays.asList(ToolBean.FIELD_OID),
                OnStudent.CHILD_FTE_MONTHLY_RECORDS);

        X2Criteria fteCriteria = new X2Criteria();
        ToolDistrictContext context = organization.getSchoolYearContext(getBroker(),
                m_sklDateRangeProvider.getStartDate());
        String contextId = context == null ? getCurrentContext().getContextId() : context.getContextId();
        fteCriteria.addEqualTo(FteRecord.FIELD_SCHOOL_YEAR.resolve(getDictExtractor()),
                contextId);
        ToolBean.addAndCriteria(getBroker(), FteRecord.class, fteCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(), Arrays.asList(ToolBean.FIELD_OID),
                OnStudent.CHILD_FTE_RECORDS);

        ToolBean.preload(getBroker(), getDictExtractor(), Arrays.asList(OnStudentSLPProgram.FIELD_DATE_DESC),
                OnStudent.CHILD_SLP_PROGRAMS);
    }

    /**
     * Register tool beans.
     */
    private void registerToolBeans() {
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(ReportStudent.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(FteMonthly.class);
        ToolBean.registerClass(FteRecord.class);
        ToolBean.registerClass(OnStudentSLPProgram.class);
        ToolBean.registerClass(ReportSection.class);
        ToolBean.setBroker(getBroker());

        m_dictExtractor = new DictionaryExtractor(getBroker());
        ToolBean.setDictionaryExtractor(m_dictExtractor);
    }

    /**
     * Computes FSL summary data for students with age 21+.
     *
     * @param school OnSchool
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @return FSLSecondarySummaryContainer
     */
    private FSLSecondarySummaryContainer retrieve21PlusData(OnSchool school,
                                                            Filterable<OnEnrollment> filtrableEnrollments) {
        FSLSecondarySummaryContainer plus21StudentData =
                new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.student21OrOver.key());

        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(
                        Arrays.asList(GRADE_LEVEL_P9, GRADE_LEVEL_09, GRADE_LEVEL_10, GRADE_LEVEL_11, GRADE_LEVEL_12),
                        true));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        m_creditsMap = new HashMap<>();
        Filterable<OnEnrollment> coreEnrollmentsGrade09and10 =
                coreEnrollments.filter(getStudnetEnrollmentsByGradeLevels(
                        Arrays.asList(GRADE_LEVEL_09, GRADE_LEVEL_10), true));
        Filterable<OnEnrollment> coreEnrollmentsGrade11and12 =
                coreEnrollments.filter(getStudnetEnrollmentsByGradeLevels(
                        Arrays.asList(GRADE_LEVEL_11, GRADE_LEVEL_12), true));
        plus21StudentData
                .setCoreCreditsOfIntermediateStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                        coreEnrollmentsGrade09and10, CourseLevel.Intermediate, PGMType.CORE));
        plus21StudentData.setCoreCreditsOfSeniorStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollmentsGrade11and12, CourseLevel.Senior, PGMType.CORE));
        Filterable<OnEnrollment> enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        plus21StudentData.setCoreTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));


        Filterable<OnEnrollment> extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        Filterable<OnEnrollment> immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedAndImmersionEnrollmentsGrade09and10 =
                extendedAndImmersionStudentEnrollments.filter(getStudnetEnrollmentsByGradeLevels(
                        Arrays.asList(GRADE_LEVEL_09, GRADE_LEVEL_10), true));
        Filterable<OnEnrollment> extendedAndImmersionEnrollmentsGrade11and12 =
                extendedAndImmersionStudentEnrollments.filter(getStudnetEnrollmentsByGradeLevels(
                        Arrays.asList(GRADE_LEVEL_11, GRADE_LEVEL_12), true));

        m_creditsMap = new HashMap<>();
        plus21StudentData.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionEnrollmentsGrade09and10, CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION));
        plus21StudentData.setFrenchCourseSeniorCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionEnrollmentsGrade11and12, CourseLevel.Senior, PGMType.EXTENDED_IMMERSION));

        plus21StudentData.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionEnrollmentsGrade09and10, CourseLevel.Intermediate, PGMType.OTHER));
        plus21StudentData.setFrenchCourseSeniorCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionEnrollmentsGrade11and12, CourseLevel.Senior, PGMType.OTHER));

        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        plus21StudentData.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        plus21StudentData.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        plus21StudentData.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        return plus21StudentData;
    }

    /**
     * Retrieve elementary grade data.
     *
     * @param school OnSchool
     * @param gradeData FSLSummaryContainer
     * @param grade String
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @param totalContainer FSLSummaryContainer
     */
    private void retrieveElementaryGradeData(OnSchool school,
                                             FSLElementarySummaryContainer gradeData,
                                             String grade,
                                             Filterable<OnEnrollment> filtrableEnrollments,
                                             FSLElementarySummaryContainer totalContainer) {
        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(Arrays.asList(grade), false));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        KeyValuePair<BigDecimal, Integer> minutesAndStdsCore =
                getStudentsFoMinutesForProgramElementary(coreEnrollments);
        gradeData.setCoreTotalNumberOfStudents(minutesAndStdsCore.getValue());
        gradeData.setCoreMinutes(minutesAndStdsCore.getKey());

        Filterable<OnEnrollment> extendedProgramEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        KeyValuePair<BigDecimal, Integer> minutesAndStdsExtended =
                getStudentsFoMinutesForProgramElementary(extendedProgramEnrollments);
        gradeData.setExtendedTotalNumberOfStudents(minutesAndStdsExtended.getValue());
        gradeData.setExtendedMinutes(minutesAndStdsExtended.getKey());

        Filterable<OnEnrollment> immersionProgramEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        KeyValuePair<BigDecimal, Integer> minutesAndStdsImmersion =
                getStudentsFoMinutesForProgramElementary(immersionProgramEnrollments);
        gradeData.setImmersionTotalNumberOfStudents(minutesAndStdsImmersion.getValue());
        gradeData.setImmersionMinutes(minutesAndStdsImmersion.getKey());
        addElemantaryDataToTotal(totalContainer, gradeData);
    }

    /**
     * Computes FSL summary data for grade-9 students(Grade level:09).
     *
     * @param school OnSchool
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @param totalContainer FSLSummaryContainer
     * @return FSLSecondarySummaryContainer
     */
    private FSLSecondarySummaryContainer retrieveGrade9Data(OnSchool school,
                                                            Filterable<OnEnrollment> filtrableEnrollments,
                                                            FSLSecondarySummaryContainer totalContainer) {
        FSLSecondarySummaryContainer grade9Data =
                new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.grade9Data.key());
        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(Arrays.asList(GRADE_LEVEL_09), false));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        m_creditsMap = new HashMap<>();
        grade9Data.setCoreCreditsOfIntermediateStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Intermediate, PGMType.CORE));
        Filterable<OnEnrollment> enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade9Data.setCoreTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        Filterable<OnEnrollment> extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        Filterable<OnEnrollment> immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        m_creditsMap = new HashMap<>();
        grade9Data.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION));
        grade9Data.setOtherSubjectIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade9Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade9Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        addSecondaryDataToTotal(totalContainer, grade9Data);
        return grade9Data;
    }


    /**
     * Computes FSL summary data for grade-10 students(Grade level:10).
     *
     * @param school OnSchool
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @param totalContainer FSLSummaryContainer
     * @return FSLSecondarySummaryContainer
     */
    private FSLSecondarySummaryContainer retrieveGrade10Data(OnSchool school,
                                                             Filterable<OnEnrollment> filtrableEnrollments,
                                                             FSLSecondarySummaryContainer totalContainer) {
        FSLSecondarySummaryContainer grade10Data =
                new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.grade10Data.key());
        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(Arrays.asList(GRADE_LEVEL_10), false));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        m_creditsMap = new HashMap<>();
        grade10Data.setCoreCreditsOfIntermediateStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Intermediate, PGMType.CORE));
        Filterable<OnEnrollment> enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade10Data.setCoreTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        Filterable<OnEnrollment> extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        Filterable<OnEnrollment> immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        m_creditsMap = new HashMap<>();
        grade10Data.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION));
        grade10Data.setOtherSubjectIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade10Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade10Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        addSecondaryDataToTotal(totalContainer, grade10Data);
        return grade10Data;
    }

    /**
     * Computes FSL summary data for grade-11 students(Grade level:11).
     *
     * @param school OnSchool
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @param totalContainer FSLSummaryContainer
     * @return FSLSecondarySummaryContainer
     */
    private FSLSecondarySummaryContainer retrieveGrade11Data(OnSchool school,
                                                             Filterable<OnEnrollment> filtrableEnrollments,
                                                             FSLSecondarySummaryContainer totalContainer) {
        FSLSecondarySummaryContainer grade11Data =
                new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.grade11Data.key());
        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(Arrays.asList(GRADE_LEVEL_11), false));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        m_creditsMap = new HashMap<>();
        grade11Data.setCoreCreditsOfIntermediateStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Intermediate, PGMType.CORE));
        Filterable<OnEnrollment> enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        int totalCoreIntermid = getSizeOfFiltrable(enrollmentsWithCredits).intValue();
        Filterable<OnEnrollment> extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        Filterable<OnEnrollment> immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        m_creditsMap = new HashMap<>();
        grade11Data.setCoreCreditsOfSeniorStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Senior, PGMType.CORE));
        enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        int totalCoreSen = getSizeOfFiltrable(enrollmentsWithCredits).intValue();
        grade11Data.setCoreTotalNumberOfStudents(Integer.valueOf(totalCoreSen + totalCoreIntermid));
        extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));

        extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        m_creditsMap = new HashMap<>();
        grade11Data.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION));
        grade11Data.setOtherSubjectIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade11Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade11Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        m_creditsMap = new HashMap<>();
        grade11Data.setFrenchCourseSeniorCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Senior, PGMType.EXTENDED_IMMERSION));
        grade11Data.setOtherSubjectSeniorCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Senior, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade11Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade11Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        addSecondaryDataToTotal(totalContainer, grade11Data);
        return grade11Data;
    }

    /**
     * Computes FSL summary data for grade-12 students(Grade level:12).
     *
     * @param school OnSchool
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @param totalContainer FSLSummaryContainer
     * @return FSLSecondarySummaryContainer
     */
    private FSLSecondarySummaryContainer retrieveGrade12Data(OnSchool school,
                                                             Filterable<OnEnrollment> filtrableEnrollments,
                                                             FSLSecondarySummaryContainer totalContainer) {
        FSLSecondarySummaryContainer grade12Data =
                new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.grade12Data.key());
        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(Arrays.asList(GRADE_LEVEL_12), false));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        m_creditsMap = new HashMap<>();
        grade12Data.setCoreCreditsOfIntermediateStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Intermediate, PGMType.CORE));
        grade12Data.setCoreCreditsOfSeniorStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Senior, PGMType.CORE));
        Filterable<OnEnrollment> enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade12Data.setCoreTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        Filterable<OnEnrollment> extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        Filterable<OnEnrollment> immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));

        m_creditsMap = new HashMap<>();
        grade12Data.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION));
        grade12Data.setOtherSubjectIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade12Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade12Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        m_creditsMap = new HashMap<>();
        grade12Data.setFrenchCourseSeniorCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Senior, PGMType.EXTENDED_IMMERSION));
        grade12Data.setOtherSubjectSeniorCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Senior, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade12Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        grade12Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        addSecondaryDataToTotal(totalContainer, grade12Data);
        return grade12Data;
    }

    /**
     * Computes course type("Other Subjects Intermediate Credits" or
     * "Other Subjects Senior Credits).
     *
     * @param course Course
     * @return CourseLevel
     */
    private CourseLevel retrieveOtherCourseType(ReportSection section) {
        String courseNumber = section.getCourseNumber();
        String fourthChar = Character.toString(courseNumber.charAt(4));
        if (courseNumber.startsWith(COURSE_PREFFIX_LV)) {
            if (CHAR_B.equals(fourthChar) || CHAR_C.equals(fourthChar)) {
                return CourseLevel.Senior;
            }
            return CourseLevel.Intermediate;
        }

        if (courseNumber.startsWith(COURSE_PREFFIX_LN)) {
            if (CHAR_E.equals(fourthChar) || CHAR_D.equals(fourthChar)) {
                return CourseLevel.Senior;
            }
            return CourseLevel.Intermediate;
        }

        if (courseNumber.startsWith(COURSE_PREFFIX_L) && (!courseNumber.startsWith(COURSE_PREFFIX_LN) &&
                courseNumber.startsWith(COURSE_PREFFIX_LV))) {
            if (CHAR_C.equals(fourthChar) || CHAR_D.equals(fourthChar)) {
                return CourseLevel.Senior;
            }
            return CourseLevel.Intermediate;
        }

        if (!courseNumber.startsWith(COURSE_PREFFIX_L) && !courseNumber.startsWith(COURSE_PREFFIX_FS)
                && !courseNumber.startsWith(COURSE_PREFFIX_FE) && !courseNumber.startsWith(COURSE_PREFFIX_FI)) {
            if (NUM_3.equals(fourthChar) || NUM_4.equals(fourthChar)) {
                return CourseLevel.Senior;
            }
            return CourseLevel.Intermediate;
        }

        return null;
    }

    /**
     * Computes FSL summary data for preGrade9 students(Grade level:p9).
     *
     * @param school OnSchool
     * @param filtrableEnrollments Filterable<StudentEnrollment>
     * @param totalContainer FSLSummaryContainer
     * @return FSLSummaryContainer
     */
    private FSLSecondarySummaryContainer retrievePreGrade9Data(OnSchool school,
                                                               Filterable<OnEnrollment> filtrableEnrollments,
                                                               FSLSecondarySummaryContainer totalContainer) {
        FSLSecondarySummaryContainer gradeP9Data =
                new FSLSecondarySummaryContainer(FSLRecord.FSLSummaryFieldSecondary.preGrade9.key());
        Filterable<OnEnrollment> enrollments = filtrableEnrollments.filter(
                getStudnetEnrollmentsByGradeLevels(Arrays.asList(GRADE_LEVEL_P9), false));
        Filterable<OnEnrollment> coreEnrollments = enrollments
                .filter(getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_CORE, PGM_FSL_ICFP), school));
        m_creditsMap = new HashMap<>();
        gradeP9Data.setCoreCreditsOfIntermediateStudents(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                coreEnrollments, CourseLevel.Intermediate, PGMType.CORE));
        Filterable<OnEnrollment> enrollmentsWithCredits =
                coreEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        gradeP9Data.setCoreTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));

        Filterable<OnEnrollment> extendedAndImmersionStudentEnrollments = enrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED, PGM_FSL_IMMERSION), school));
        Filterable<OnEnrollment> extendedProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_EXTENDED), school));
        Filterable<OnEnrollment> immersionProgramEnrollments = extendedAndImmersionStudentEnrollments.filter(
                getStudnetEnrollmentsByPGMType(Arrays.asList(PGM_FSL_IMMERSION), school));
        m_creditsMap = new HashMap<>();
        gradeP9Data.setFrenchCourseIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.EXTENDED_IMMERSION));
        gradeP9Data.setOtherSubjectIntermediateCredits(computeFrenchSubjectCreditsByCourseLevelAndPGMType(school,
                extendedAndImmersionStudentEnrollments, CourseLevel.Intermediate, PGMType.OTHER));
        enrollmentsWithCredits =
                immersionProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        gradeP9Data.setImmersionTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        enrollmentsWithCredits =
                extendedProgramEnrollments.filter(enr -> m_creditsMap.keySet().contains(enr.getStudentOid()));
        gradeP9Data.setExtendedTotalNumberOfStudents(getSizeOfFiltrable(enrollmentsWithCredits));
        addSecondaryDataToTotal(totalContainer, gradeP9Data);
        return gradeP9Data;
    }

    /**
     * Sets computed data to detail report.
     *
     * @param grid ReportDataGrid
     * @param fslRecords List<FSLRecord>
     */
    private void setDetailReportData(ReportDataGrid grid, List<FSLRecord> fslRecords) {
        if (!fslRecords.isEmpty()) {
            String currentGrade = "dummyContent";
            String currentProgramType = "dummyContent";
            String schoolName = null;
            for (int i = 0; i < fslRecords.size(); i++) {
                FSLRecord record = fslRecords.get(i);
                grid.append();
                grid.set(FIELD_PRINT_GRADE_HEADER, Boolean.TRUE);
                schoolName = (String) record.m_commonFieldValuePairs.get(FSLRecord.FSLCommonFields.schoolName);
                grid.set(FIELD_PRINT_DATE_HEADER, schoolName.equals(record.m_commonFieldValuePairs.get(
                        FSLRecord.FSLCommonFields.schoolName)));
                if (currentGrade.equals(record.m_detailFieldValuePairs.get(FSLRecord.FSLDetailFields.grade))
                        && currentProgramType.equals(record.m_detailFieldValuePairs
                                .get(FSLRecord.FSLDetailFields.secondLanguageProgramType))) {
                    grid.set(FIELD_PRINT_GRADE_HEADER, Boolean.FALSE);
                } else {
                    currentGrade = (String) record.m_detailFieldValuePairs.get(FSLRecord.FSLDetailFields.grade);
                    currentProgramType = (String) record.m_detailFieldValuePairs
                            .get(FSLRecord.FSLDetailFields.secondLanguageProgramType);
                    grid.set(FIELD_PRINT_GRADE_HEADER, Boolean.TRUE);
                }

                for (FSLRecord.FSLCommonFields field : record.m_commonFieldValuePairs.keySet()) {
                    grid.set(field.toString(), record.m_commonFieldValuePairs.get(field));
                }
                for (FSLRecord.FSLDetailFields field : record.m_detailFieldValuePairs.keySet()) {
                    grid.set(field.toString(), record.m_detailFieldValuePairs.get(field));
                }

                addTotalFieldsToGrid(schoolName, grid);
            }
        }
    }

    /**
     * Sets the summary elementary report data.
     *
     * @param grid ReportDataGrid
     * @param fslRecords List<FSLRecord>
     */
    private void setSummaryElementaryReportData(ReportDataGrid grid, List<FSLRecord> fslRecords) {
        if (!fslRecords.isEmpty()) {
            for (FSLRecord record : fslRecords) {
                for (FSLRecord.FSLSummaryFieldElementary value : FSLRecord.FSLSummaryFieldElementary.values()) {
                    grid.append();
                    for (FSLRecord.FSLCommonFields field : record.m_commonFieldValuePairs.keySet()) {
                        grid.set(field.toString(), record.m_commonFieldValuePairs.get(field));
                    }
                    FSLElementarySummaryContainer container = record.getSummaryFieldElem(value);
                    if (container != null && grid != null) {
                        grid.set(REPORT_FIELD_CAT_NAME, container.getFslPupilCategoryName());
                        grid.set(RF_ELEM_TOTAL_STD_NUM_CORE, container.getCoreTotalNumberOfStudents() == null ? ZERO_INT
                                : container.getCoreTotalNumberOfStudents().toString());
                        grid.set(RF_ELEM_TOTAL_STD_NUM_EXTENDED,
                                container.getExtendedTotalNumberOfStudents() == null ? ZERO_INT
                                        : container.getExtendedTotalNumberOfStudents().toString());
                        grid.set(RF_ELEM_TOTAL_STD_NUM_IMMERSION,
                                container.getImmersionTotalNumberOfStudents() == null ? ZERO_INT
                                        : container.getImmersionTotalNumberOfStudents().toString());
                        grid.set(RF_ELEM_MINUTES_CORE, container.getCoreMinutes() == null ? ZERO_DECIMAL
                                : container.getCoreMinutes().toString());
                        grid.set(RF_ELEM_MINUTES_EXTENDED, container.getExtendedMinutes() == null ? ZERO_DECIMAL
                                : container.getExtendedMinutes().toString());
                        grid.set(RF_ELEM_MINUTES_IMMERSION, container.getImmersionMinutes() == null ? ZERO_DECIMAL
                                : container.getImmersionMinutes().toString());
                    }
                }
            }
        }
    }

    /**
     * Sets the summary secondary report data.
     *
     * @param grid ReportDataGrid
     * @param fslRecords List<FSLRecord>
     */
    private void setSummarySecondaryReportData(ReportDataGrid grid, List<FSLRecord> fslRecords) {
        if (!fslRecords.isEmpty()) {
            for (FSLRecord record : fslRecords) {
                for (FSLRecord.FSLSummaryFieldSecondary value : FSLRecord.FSLSummaryFieldSecondary.values()) {
                    grid.append();
                    for (FSLRecord.FSLCommonFields field : record.m_commonFieldValuePairs.keySet()) {
                        grid.set(field.toString(), record.m_commonFieldValuePairs.get(field));
                    }
                    FSLSecondarySummaryContainer container = record.getSummaryFieldSec(value);
                    if (container != null && grid != null) {
                        grid.set(REPORT_FIELD_CAT_NAME, container.getFslPupilCategoryName());
                        grid.set(REPORT_FIELD_TOTAL_STD_NUM, container.getCoreTotalNumberOfStudents() == null ? ZERO_INT
                                : container.getCoreTotalNumberOfStudents().toString());
                        grid.set(REPORT_FIELD_CORE_CREDIT_INTERMEDIATE,
                                container.getCoreCreditsOfIntermediateStudents() == null ? ZERO_DECIMAL
                                        : formatStringDecimal(
                                                container.getCoreCreditsOfIntermediateStudents().toString()));
                        grid.set(REPORT_FIELD_CORE_CREDIT_SENIOR,
                                container.getCoreCreditsOfSeniorStudents() == null ? ZERO_DECIMAL
                                        : formatStringDecimal(container.getCoreCreditsOfSeniorStudents().toString()));
                        grid.set(REPORT_FIELD_TOTAL_STD_NUM_EXTENDED,
                                container.getExtendedTotalNumberOfStudents() == null ? ZERO_INT
                                        : container.getExtendedTotalNumberOfStudents().toString());
                        grid.set(REPORT_FIELD_TOTAL_STD_NUM_IMMERSION,
                                container.getImmersionTotalNumberOfStudents() == null ? ZERO_INT
                                        : container.getImmersionTotalNumberOfStudents().toString());
                        grid.set(REPORT_FIELD_CREDIT_FRENCH_INTERMEDIATE,
                                container.getFrenchCourseIntermediateCredits() == null ? ZERO_DECIMAL
                                        : formatStringDecimal(
                                                container.getFrenchCourseIntermediateCredits().toString()));
                        grid.set(REPORT_FIELD_CREDIT_FRENCH_SENIOR,
                                container.getFrenchCourseSeniorCredits() == null ? ZERO_DECIMAL
                                        : formatStringDecimal(container.getFrenchCourseSeniorCredits().toString()));
                        grid.set(REPORT_FIELD_CREDIT_OTHER_INTERMEDIATE,
                                container.getOtherSubjectIntermediateCredits() == null ? ZERO_DECIMAL
                                        : formatStringDecimal(
                                                container.getOtherSubjectIntermediateCredits().toString()));
                        grid.set(REPORT_FIELD_CREDIT_OTHER_SENIOR,
                                container.getOtherSubjectSeniorCredits() == null ? ZERO_DECIMAL
                                        : formatStringDecimal(container.getOtherSubjectSeniorCredits().toString()));
                    }
                }
            }
        }
    }
}
