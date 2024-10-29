/*
 * ====================================================================
 *
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

package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolOrganization;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentProgramParticipation;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteMonthly;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteRecord;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSalep;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class EnrByAttendanceTypeSecSumReportData.
 */
public class EnrByAttendanceTypeSecSumReportData extends OnReportJavaSourceNew {

    private static final long serialVersionUID = 1L;

    private static final String ATT_TYPE_NOT_SET =
            "FTE was not found Or values by aliases udc-fte-register or udd-fte-register are not set!!!";
    private static final String BOARD_RES_STATUS_01 = "01";
    private static final String BOARD_RES_STATUS_02 = "02";
    private static final String BOARD_RES_STATUS_03 = "03";
    private static final String BOARD_RES_STATUS_05 = "05";
    private static final String BOARD_RES_STATUS_07 = "07";
    private static final String ENR_ATT_TYPE_FT = "FT";
    private static final String ENR_ATT_TYPE_PT = "PT";
    private static List<String> ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE =
            Arrays.asList("ftU21", "ptU21", "ftO21", "ptO21");
    private static final Integer INT_9 = 9;

    /**
     * Class members
     */
    private DecimalFormat m_decimalFormat = new DecimalFormat("#.##");
    private DictionaryExtractor m_dictExtractor;
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private Map<String, ToolBean> m_fteMap = new HashMap<>();
    private Collection<String> m_sklBsidCodes = new HashSet<>();
    private transient Map<String, SchoolDateRangeProvider> m_schoolDateRangeProvider;
    private EnrollmentSpanCriteria m_spanCriteria;
    private Map<String, ToolStudentSchool> m_sskByStdOid = new HashMap<>();

    /**
     * The Class EnrByAttSummRecord.
     */
    private static class EnrByAttSummRecord {
        private Map<Field, Object> m_fieldValuePairs = new HashMap<Field, Object>();

        /**
         * The Enum Field.
         */
        private enum Field {
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
            pupilsOfBoardSecondaryOnly,
            //
            pupilsOfBoardJuniorKindergarten,
            //
            pupilsOfBoardKindergarten,
            //
            nativeEducationAuthority,
            //
            governmentOfCanada,
            //
            studyPermitTempResident,
            //
            otherStudents,
            //
            summaryValues
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardNumber.toString(),
                        Field.schoolName.toString(),
                        Field.schoolNumber.toString());

        private static ValueByKeyResolver<EnrByAttSummRecord> s_valueResolver =
                new ValueByKeyResolver<EnrByAttSummRecord>() {
                    @Override
                    public Object getValue(String key, EnrByAttSummRecord entity) {
                        return entity.get(Field.valueOf(key));
                    }
                };

        /**
         * Gets the.
         *
         * @param key Field
         * @return Object
         */
        Object get(Field key) {
            return m_fieldValuePairs.get(key);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        void set(Field field, Object value) {
            m_fieldValuePairs.put(field, value);
        }
    }

    /**
     * The Class EnrByAttSummaryContainer.
     */
    public static class EnrByAttSummaryContainer {
        private Integer fullTimeU21NumberOfStudents = 0;
        private BigDecimal fullTimeU21RegularFTE = BigDecimal.ZERO;
        private BigDecimal fullTimeU21HighCreditFTE = BigDecimal.ZERO;
        private Integer partTimeU21NumberOfStudents = 0;
        private BigDecimal partTimeU21RegularFTE = BigDecimal.ZERO;
        private BigDecimal partTimeU21HighCreditFTE = BigDecimal.ZERO;
        private Integer fullTimeO21NumberOfStudents = 0;
        private Integer partTimeO21NumberOfStudents = 0;
        private BigDecimal partTimeO21RegularFTE = BigDecimal.ZERO;

        /**
         * Gets the full time U 21 number of students.
         *
         * @return the fullTimeU21NumberOfStudents
         */
        public Integer getFullTimeU21NumberOfStudents() {
            return fullTimeU21NumberOfStudents;
        }

        /**
         * Sets the full time U 21 number of students.
         *
         * @param fullTimeU21NumberOfStudents void
         */
        public void setFullTimeU21NumberOfStudents(Integer fullTimeU21NumberOfStudents) {
            this.fullTimeU21NumberOfStudents = fullTimeU21NumberOfStudents;
        }

        /**
         * Gets the full time U 21 regular FTE.
         *
         * @return the fullTimeU21RegularFTE
         */
        public BigDecimal getFullTimeU21RegularFTE() {
            return fullTimeU21RegularFTE;
        }

        /**
         * Sets the full time U 21 regular FTE.
         *
         * @param fullTimeU21RegularFTE void
         */
        public void setFullTimeU21RegularFTE(BigDecimal fullTimeU21RegularFTE) {
            this.fullTimeU21RegularFTE = fullTimeU21RegularFTE;
        }

        /**
         * Gets the full time U 21 high credit FTE.
         *
         * @return the fullTimeU21HighCreditFTE
         */
        public BigDecimal getFullTimeU21HighCreditFTE() {
            return fullTimeU21HighCreditFTE;
        }

        /**
         * Sets the full time U 21 high credit FTE.
         *
         * @param fullTimeU21HighCreditFTE void
         */
        public void setFullTimeU21HighCreditFTE(BigDecimal fullTimeU21HighCreditFTE) {
            this.fullTimeU21HighCreditFTE = fullTimeU21HighCreditFTE;
        }

        /**
         * Gets the part time U 21 number of students.
         *
         * @return the partTimeU21NumberOfStudents
         */
        public Integer getPartTimeU21NumberOfStudents() {
            return partTimeU21NumberOfStudents;
        }

        /**
         * Sets the part time U 21 number of students.
         *
         * @param partTimeU21NumberOfStudents void
         */
        public void setPartTimeU21NumberOfStudents(Integer partTimeU21NumberOfStudents) {
            this.partTimeU21NumberOfStudents = partTimeU21NumberOfStudents;
        }

        /**
         * Gets the part time U 21 regular FTE.
         *
         * @return the partTimeU21RegularFTE
         */
        public BigDecimal getPartTimeU21RegularFTE() {
            return partTimeU21RegularFTE;
        }

        /**
         * Sets the part time U 21 regular FTE.
         *
         * @param partTimeU21RegularFTE void
         */
        public void setPartTimeU21RegularFTE(BigDecimal partTimeU21RegularFTE) {
            this.partTimeU21RegularFTE = partTimeU21RegularFTE;
        }

        /**
         * Gets the part time U 21 high credit FTE.
         *
         * @return the partTimeU21HighCreditFTE
         */
        public BigDecimal getPartTimeU21HighCreditFTE() {
            return partTimeU21HighCreditFTE;
        }

        /**
         * Sets the part time U 21 high credit FTE.
         *
         * @param partTimeU21HighCreditFTE void
         */
        public void setPartTimeU21HighCreditFTE(BigDecimal partTimeU21HighCreditFTE) {
            this.partTimeU21HighCreditFTE = partTimeU21HighCreditFTE;
        }

        /**
         * Gets the full time O 21 number of students.
         *
         * @return the fullTimeO21NumberOfStudents
         */
        public Integer getFullTimeO21NumberOfStudents() {
            return fullTimeO21NumberOfStudents;
        }

        /**
         * Sets the full time O 21 number of students.
         *
         * @param fullTimeO21NumberOfStudents void
         */
        public void setFullTimeO21NumberOfStudents(Integer fullTimeO21NumberOfStudents) {
            this.fullTimeO21NumberOfStudents = fullTimeO21NumberOfStudents;
        }

        /**
         * Gets the part time O 21 number of students.
         *
         * @return the partTimeO21NumberOfStudents
         */
        public Integer getPartTimeO21NumberOfStudents() {
            return partTimeO21NumberOfStudents;
        }

        /**
         * Sets the part time O 21 number of students.
         *
         * @param partTimeO21NumberOfStudents void
         */
        public void setPartTimeO21NumberOfStudents(Integer partTimeO21NumberOfStudents) {
            this.partTimeO21NumberOfStudents = partTimeO21NumberOfStudents;
        }

        /**
         * Gets the part time O 21 regular FTE.
         *
         * @return the partTimeO21RegularFTE
         */
        public BigDecimal getPartTimeO21RegularFTE() {
            return partTimeO21RegularFTE;
        }

        /**
         * Sets the part time O 21 regular FTE.
         *
         * @param partTimeO21RegularFTE void
         */
        public void setPartTimeO21RegularFTE(BigDecimal partTimeO21RegularFTE) {
            this.partTimeO21RegularFTE = partTimeO21RegularFTE;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "EnrollmentByAttendanceSummaryInstance [fullTimeU21NumberOfStudents=" + fullTimeU21NumberOfStudents
                    + ", fullTimeU21RegularFTE=" + fullTimeU21RegularFTE + ", fullTimeU21HighCreditFTE="
                    + fullTimeU21HighCreditFTE + ", partTimeU21NumberOfStudents=" + partTimeU21NumberOfStudents
                    + ", partTimeU21RegularFTE=" + partTimeU21RegularFTE + ", partTimeU21HighCreditFTE="
                    + partTimeU21HighCreditFTE + ", fullTimeO21NumberOfStudents=" + fullTimeO21NumberOfStudents
                    + ", partTimeO21NumberOfStudents=" + partTimeO21NumberOfStudents + ", partTimeO21RegularFTE="
                    + partTimeO21RegularFTE + "]";
        }

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
            return EnrByAttendanceTypeSecSumReportData.this.getBroker();
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
            return getContextByDate(getReportDate()).getEndDate();
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
            return getReportDate();
        }

        /**
         * Sets the school.
         *
         * @param school void
         */
        public void setSchool(OnSchool school) {
            m_school = school;
        }
    }

    /**
     * @see com.x2dev.reports.statereporting.on.OnReportJavaSourceNew#getStudents(com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria,
     *      boolean, boolean, java.util.function.Predicate)
     */
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
                            span.getAllEnrollmentsDescend().isEmpty() ? null
                                    : span.getAllEnrollmentsDescend().get(0);
                    if (!span.isSecondary() && (lastEnrolment == null
                            || lastEnrolment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL))) {
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
            Filterable<EnrByAttSummRecord> fteSummarryRecords = filterableSkls
                    .map(new Mapper<OnSchool, EnrByAttSummRecord>() {
                        @Override
                        public Filterable<EnrByAttSummRecord> map(OnSchool school) {
                            initializeHelpersForSchool(school);
                            Collection<ReportStudent> students =
                                    getStudents(m_spanCriteria, false, false, m_annualSpanFilter)
                                            .stream().collect(Collectors.toList());
                            Collection<String> existStdOid =
                                    students.stream().map(std -> std.getOid()).collect(Collectors.toList());
                            students.addAll(getAdditionalStudents(existStdOid, school));
                            preloadStudentsData();
                            return getRecords(school, students, sklOidsToReport);
                        }
                    });
            for (EnrByAttSummRecord record : fteSummarryRecords.extract()) {
                grid.append();
                EnrByAttSummaryContainer summaryValues = new EnrByAttSummaryContainer();
                for (EnrByAttSummRecord.Field field : record.m_fieldValuePairs.keySet()) {
                    if (record.m_fieldValuePairs.get(field) instanceof EnrByAttSummaryContainer) {
                        addDataToSummary((EnrByAttSummaryContainer) record.m_fieldValuePairs.get(field),
                                summaryValues);
                        EnrByAttSummaryContainer container =
                                (EnrByAttSummaryContainer) record.m_fieldValuePairs.get(field);
                        populateGridWithValues(grid, container, field.toString());
                    } else if (!(record.m_fieldValuePairs.get(field) instanceof EnrByAttSummaryContainer)) {
                        grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                    }
                }
                populateGridWithValues(grid, summaryValues, EnrByAttSummRecord.Field.summaryValues.toString());
            }
            if (getErrorsLog().length() > 0) {
                if (grid.isEmpty()) {
                    grid.append();
                }
                addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
            }
            grid.beforeTop();
            addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
            addParameter(REPORT_PARAM_AS_OF_DATE, s_asOfDateFormat.format(getReportDate()));
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

        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(FteMonthly.class);
        ToolBean.registerClass(FteRecord.class);

        initSklBSIDCodes();
    }

    /**
     * Computes summary values.
     *
     * @param enrollmentByAttendanceValues EnrByAttSummaryContainer
     * @param summaryValues EnrByAttSummaryContainer
     */
    private void addDataToSummary(EnrByAttSummaryContainer enrollmentByAttendanceValues,
                                  EnrByAttSummaryContainer summaryValues) {
        summaryValues.setFullTimeU21NumberOfStudents(summaryValues.getFullTimeU21NumberOfStudents()
                + enrollmentByAttendanceValues.getFullTimeU21NumberOfStudents());

        summaryValues.setFullTimeU21RegularFTE(
                summaryValues.getFullTimeU21RegularFTE().add(enrollmentByAttendanceValues.getFullTimeU21RegularFTE()));

        summaryValues.setFullTimeU21HighCreditFTE(summaryValues.getFullTimeU21HighCreditFTE()
                .add(enrollmentByAttendanceValues.getFullTimeU21HighCreditFTE()));
        summaryValues.setPartTimeU21NumberOfStudents(summaryValues.getPartTimeU21NumberOfStudents()
                + enrollmentByAttendanceValues.getPartTimeU21NumberOfStudents());
        summaryValues.setPartTimeU21RegularFTE(
                summaryValues.getPartTimeU21RegularFTE().add(enrollmentByAttendanceValues.getPartTimeU21RegularFTE()));
        summaryValues.setPartTimeU21HighCreditFTE(summaryValues.getPartTimeU21HighCreditFTE()
                .add(enrollmentByAttendanceValues.getPartTimeU21HighCreditFTE()));
        summaryValues.setFullTimeO21NumberOfStudents(summaryValues.getFullTimeO21NumberOfStudents()
                + enrollmentByAttendanceValues.getFullTimeO21NumberOfStudents());
        summaryValues.setPartTimeO21NumberOfStudents(summaryValues.getPartTimeO21NumberOfStudents()
                + enrollmentByAttendanceValues.getPartTimeO21NumberOfStudents());
        summaryValues.setPartTimeO21RegularFTE(summaryValues.getPartTimeO21RegularFTE()
                .add(enrollmentByAttendanceValues.getPartTimeO21RegularFTE()));
    }

    /**
     * Creates filter to retrieve data by board res. status
     *
     * @param attendanceType String
     * @param isPlus21 boolean
     * @param boardResStatus String
     * @return Filter
     */
    private Filter createFileterByBoardResStatus(String attendanceType,
                                                 boolean isPlus21,
                                                 String boardResStatus) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {

                    return attendanceType.equals(getAttendanceTypeByFTE(toFilter))
                            && isPlus21 == is21plus((ReportStudent) toFilter.getStudent(getBroker()))
                            &&
                            boardResStatus.equals(toFilter.getBoardResidentStatus());
                } catch (Exception e) {
                    // e.printStackTrace();
                    return false;
                }
            }
        };
    }

    /**
     * Creates filter to retrieve data, which satisfied to
     * ' Pupils of the board - Secondary only' condition:
     * (STUDENT_SCHOOL_ENROLMENT.GRADE_TYPE_CODE >= 9
     * STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="01")
     *
     * @param attendanceType String
     * @param isPlus21 boolean
     * @param gradeLevel Integer
     * @param boardResStatus String
     * @return Filter
     */
    private Filter createFilterForPupilOfBoardSecondary(String attendanceType,
                                                        boolean isPlus21,
                                                        Integer gradeLevel,
                                                        String boardResStatus) {
        return new Filter<OnEnrollment>() {
            @Override
            public boolean isFiltered(OnEnrollment toFilter) {
                try {
                    Integer gradeLevelInt = Integer.valueOf(toFilter.getStudent(getBroker()).getGradeLevel());
                    return attendanceType.equals(getAttendanceTypeByFTE(toFilter))
                            && isPlus21 == is21plus((ReportStudent) toFilter.getStudent(getBroker()))
                            && gradeLevelInt >= gradeLevel
                            && boardResStatus.equals(toFilter.getBoardResidentStatus())
                            && !m_sskByStdOid.containsKey(toFilter.getStudentOid());

                } catch (Exception e) {
                    // e.printStackTrace();
                    return false;
                }

            }
        };
    }

    /**
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
                        return fteDate.compareTo(getReportDate()) <= 0;
                    }
                })
                .extractSorted(Arrays.asList(FteRecord.FIELD_FTE_DATE.resolve(getDictExtractor())), false)
                .stream()
                .findFirst()
                .orElse(null);
    }

    /**
     * Gets the additional student with FTE.
     *
     * @param stdOidsToExclude Collection<String>
     * @param skl SisSchool
     * @return Collection
     */
    private Collection<ReportStudent> getAdditionalStudents(Collection<String> stdOidsToExclude, OnSchool skl) {
        X2Criteria fteCriteria = new X2Criteria();
        fteCriteria.addEqualTo(FteMonthly.FIELD_REPORT_DATE.resolve(getDictExtractor()),
                s_aliasDateFormat.format(getReportDate()));
        fteCriteria.addEqualTo(FteMonthly.FIELD_SCHOOL_OID.resolve(getDictExtractor()), skl.getOid());
        fteCriteria.addNotIn(FteMonthly.FIELD_STUDENT_OID.resolve(getDictExtractor()), stdOidsToExclude);
        SubQuery stdWithFteOidsSubquery =
                new SubQuery(FteMonthly.getX2BaseClass(), FteMonthly.FIELD_STUDENT_OID.resolve(getDictExtractor()),
                        fteCriteria);
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addNotIn(ToolBean.FIELD_OID.resolve(getDictExtractor()), stdOidsToExclude);
        stdCriteria.addIn(ToolBean.FIELD_OID.resolve(getDictExtractor()), stdWithFteOidsSubquery);
        return FilterableFactory.create(getBroker(), getDictExtractor(), ReportStudent.class, stdCriteria,
                Arrays.asList(ToolBean.FIELD_OID)).extract();
    }

    /**
     * Gets the attendance type by FTE.
     *
     * @param stdEnr StudentEnrollment
     * @return String
     */
    private String getAttendanceTypeByFTE(OnEnrollment stdEnr) {
        FteMonthly montlyFteRecordFoStd = null;
        FteRecord fteRecordC = null;
        String attType = ATT_TYPE_NOT_SET;
        Object fteRecord = m_fteMap.get(stdEnr.getOid());
        if (fteRecord != null) {
            if (fteRecord instanceof FteMonthly) {
                montlyFteRecordFoStd = (FteMonthly) fteRecord;
            } else if (fteRecord instanceof FteRecord) {
                fteRecordC = (FteRecord) fteRecord;
            }
        }
        if (montlyFteRecordFoStd != null) {
            attType = montlyFteRecordFoStd.getRegister();
        } else if (fteRecordC != null) {
            attType = fteRecordC.getAttendanceType();
        }
        return attType;
    }

    /**
     * Gets the fte monthly record.
     *
     * @param broker X2Broker
     * @param school the school
     * @param student the submission period code
     * @return FteMonthly
     */
    private FteMonthly getFteMonthlyRecord(OnSchool school, ReportStudent student) {
        List<FteMonthly> records = student.getFteMonthlyRecords(getBroker());
        return records.stream()
                .filter(fteMonthly -> school.getOid().equals(fteMonthly.getSchoolOid()))
                .findFirst()
                .orElse(null);
    }

    /**
     * Gets the fte record.
     *
     * @param stdEnr StudentEnrollment
     * @return Object
     */
    private ToolBean getFteRecord(OnEnrollment stdEnr, OnSchool skl) {
        ReportStudent student = (ReportStudent) stdEnr.getStudent(getBroker());
        FteMonthly montlyFteRecordFoStd = getFteMonthlyRecord(skl, student);
        FteRecord fteRecordC = null;
        if (montlyFteRecordFoStd != null) {
            return montlyFteRecordFoStd;
        }
        List<FteRecord> fteRecordsFoStd = student.getFteRecords(getBroker());
        if (fteRecordsFoStd != null) {
            fteRecordC = findFteRecord(fteRecordsFoStd, skl);
            if (fteRecordC == null && !m_sskByStdOid.containsKey(student.getOid())) {
                String errorMessage = "Can't find FTE record for student: "
                        + getStudentNameFirstMiddleLast(student)
                        + ", OEN: "
                        + student.getOenRaw()
                        + " for District Context: "
                        + getSchoolDateRangeProvider(skl)
                                .getContextByDate(getReportDate()).getContextId();
                logError(errorMessage);
            }
        }
        return fteRecordC;
    }

    /**
     * Gets the fte records.
     *
     * @param students Filterable<OnStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<EnrByAttSummRecord> getRecords(OnSchool skl,
                                                      Collection<ReportStudent> students,
                                                      Collection<String> sklOidsToReport) {
        String schoolName = skl.getName();
        String schoolNumber = skl.getBsid();
        ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
        String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
        String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
        String academicYears = getCurrentContext().getContextId();
        String programName = "Aspen";
        String currentDate = m_formatter
                .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
        List<OnEnrollment> enrollments = new ArrayList<>();
        Map<String, OnSchool> fteSklsByEnrId = new HashMap<String, OnSchool>();
        for (OnStudent student : students) {
            List<AnnualSpan> annualSpans = student.getEnrollmentSpans(getBroker(), false, false).stream()
                    .filter(annualSpan -> annualSpan.getSchool().getOid().equals(skl.getOid()))
                    .filter(annualSpan -> annualSpan.getContext().getOid()
                            .equals(getSchoolDateRangeProvider(skl).getContextByDate(getReportDate()).getOid()))
                    .collect(Collectors.toList());
            OnEnrollment recentEnrolment = null;
            if (annualSpans != null && !annualSpans.isEmpty()) {
                OnAnnualSpan firstAnnualSpan =
                        (OnAnnualSpan) ToolsSharedContainer.reverse(annualSpans.stream()).findFirst().get();
                if (firstAnnualSpan != null) {
                    recentEnrolment = (OnEnrollment) firstAnnualSpan.getRecentEnrollmentESY();
                    if (recentEnrolment == null && firstAnnualSpan.isSecondary()
                            && sklOidsToReport
                                    .contains(firstAnnualSpan.getSecondary().getSchool(getBroker()).getOid())
                            && firstAnnualSpan
                                    .getBestPrimarySpanFor(getBroker()) != null) {
                        recentEnrolment = (OnEnrollment) firstAnnualSpan.getBestPrimarySpanFor(getBroker())
                                .getRecentEnrollmentESY();
                        m_sskByStdOid.put(student.getOid(),
                                firstAnnualSpan.getSecondary());
                    }
                }
            }
            OnSchool sklToLookForFTE = skl;
            if (m_sskByStdOid.containsKey(student.getOid())) {
                ToolStudentSchool ssk = m_sskByStdOid.get(student.getOid());
                if (ssk != null && ssk.getEndDate() != null && !ssk.getEndDate().after(getReportDate())) {
                    continue;
                }
                sklToLookForFTE =
                        recentEnrolment != null ? (OnSchool) recentEnrolment.getSchool(getBroker())
                                : (OnSchool) student.getSchool(getBroker());
            }
            if (recentEnrolment != null) {
                String primarySklBsid = ((OnSchool) recentEnrolment.getSchool(getBroker())).getBsid();
                if (!m_sskByStdOid.containsKey(student.getOid()) && (StringUtils.isEmpty(primarySklBsid) ||
                        !m_sklBsidCodes.contains(primarySklBsid))) {
                    continue;
                }
                enrollments.add(recentEnrolment);
                if (sklToLookForFTE != null) {
                    fteSklsByEnrId.put(recentEnrolment.getOid(), sklToLookForFTE);
                }
            }
        }
        List<EnrByAttSummRecord> enrByAttSummRecords = new ArrayList<EnrByAttSummRecord>();
        Filterable<OnEnrollment> filtrableEnrollments = new Filterable<OnEnrollment>() {
            @Override
            public Map<String, OnEnrollment> initializeEntities() {
                Map<String, OnEnrollment> filtrableValues = new HashMap<>();
                for (OnEnrollment enrollment : enrollments) {
                    filtrableValues.put(enrollment.getOid(), enrollment);
                    ToolBean fteRecord = getFteRecord(enrollment, fteSklsByEnrId.get(enrollment.getOid()));
                    if (fteRecord != null) {
                        m_fteMap.put(enrollment.getOid(), fteRecord);
                    }
                }
                return filtrableValues;
            }

            @Override
            public PredefinedResolver initializeValueResolver() {
                return PredefinedResolver.RPT_BEAN;
            }

            @Override
            public List<String> initializeUniqueKeys() {
                return Arrays.asList(ToolBean.FIELD_OID.resolve(null));
            }
        };
        EnrByAttSummRecord enrByAttSummRecord = new EnrByAttSummRecord();
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.schoolName, schoolName);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.schoolNumber, schoolNumber);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.boardNumber, boardNumber);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.boardName, boardName);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.academicYears, academicYears);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.programName, programName);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.currentDate, currentDate);
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.pupilsOfBoardSecondaryOnly,
                retrievePupilOfBoardSecondaryOnlyData(filtrableEnrollments));
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.nativeEducationAuthority,
                retrieveNativeEducationAuthorityData(filtrableEnrollments));
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.governmentOfCanada,
                retrieveGovernmentOfCanadaData(filtrableEnrollments));
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.studyPermitTempResident,
                retrieveTempResidentData(filtrableEnrollments));
        enrByAttSummRecord.set(EnrByAttSummRecord.Field.otherStudents,
                retrieveOtherStudentsData(filtrableEnrollments));
        enrByAttSummRecords.add(enrByAttSummRecord);
        return FilterableFactory.create(enrByAttSummRecords, EnrByAttSummRecord.s_uniqueFields,
                EnrByAttSummRecord.s_valueResolver);
    }

    /**
     * Gets the school date range provider.
     *
     * @param school the school
     * @return the school date range provider
     */
    private SchoolDateRangeProvider getSchoolDateRangeProvider(OnSchool school) {
        String key = school == null ? "Default" : school.getOid();
        if (m_schoolDateRangeProvider == null) {
            m_schoolDateRangeProvider = new HashMap();
        }
        if (!m_schoolDateRangeProvider.containsKey(key)) {
            m_schoolDateRangeProvider.put(key, new SchoolDateRangeProvider(school));
        }
        return m_schoolDateRangeProvider.get(key);
    }

    /**
     * Inits the skl BSID codes.
     */
    private void initSklBSIDCodes() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        DataDictionaryField dictionaryField =
                m_dictExtractor.getFieldByAlias(ALIAS_SKL_BSID_NUMBER);
        if (dictionaryField != null) {
            if (dictionaryField.hasReferenceTable()) {
                m_sklBsidCodes = dictionaryField.getReferenceTable().getCodeMap().keySet();
            } else {
                logError("The reference table for field with alias = " + ALIAS_SKL_BSID_NUMBER
                        + " for School table is not set.");
            }
        } else {
            logError("The field with alias = " + ALIAS_SKL_BSID_NUMBER + " for School table is not found.");
        }
    }

    /**
     * Initialize helpers.
     */
    private void initializeHelpersForSchool(OnSchool school) {
        SchoolDateRangeProvider sklDateRangeProvider = getSchoolDateRangeProvider(school);
        m_spanCriteria = getSpanConfiguration(sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true)
                .setIncludeAllSecondarySpans(true);
    }

    /**
     * Populates grid witn needed data.
     *
     * @param grid ReportDataGrid
     * @param container EnrByAttSummaryContainer
     * @param fieldName String
     */
    private void populateGridWithValues(ReportDataGrid grid,
                                        EnrByAttSummaryContainer container,
                                        String fieldName) {
        LinkedList<String> listOfValuesToSet = new LinkedList();
        listOfValuesToSet.add(0, container.getFullTimeU21NumberOfStudents().toString());
        listOfValuesToSet.add(1, m_decimalFormat.format(container.getFullTimeU21RegularFTE().doubleValue()));
        listOfValuesToSet.add(2, m_decimalFormat.format(container.getFullTimeU21HighCreditFTE().doubleValue()));
        listOfValuesToSet.add(3, container.getPartTimeU21NumberOfStudents().toString());
        listOfValuesToSet.add(4, m_decimalFormat.format(container.getPartTimeU21RegularFTE().doubleValue()));
        listOfValuesToSet.add(5, m_decimalFormat.format(container.getPartTimeU21HighCreditFTE().doubleValue()));
        listOfValuesToSet.add(6, container.getFullTimeO21NumberOfStudents().toString());
        listOfValuesToSet.add(7, container.getPartTimeO21NumberOfStudents().toString());
        listOfValuesToSet.add(8, m_decimalFormat.format(container.getPartTimeO21RegularFTE().doubleValue()));
        grid.set(fieldName, listOfValuesToSet);
    }

    /**
     * Preload students data.
     *
     */
    private void preloadStudentsData() {
        ToolBean.clearAllCachedToolBeans(FteMonthly.class);
        ToolBean.clearAllCachedToolBeans(FteRecord.class);
        ToolBean.clearAllCachedToolBeans(OnStudentSalep.class);

        ToolBean.resetCriteria(getBroker(), FteMonthly.class);
        ToolBean.resetCriteria(getBroker(), FteRecord.class);
        ToolBean.resetCriteria(getBroker(), OnStudentSalep.class);

        // preload fte
        ToolOrganization organization =
                ToolBean.getBeanByOid(getBroker(), ToolOrganization.class, getOrganization().getOid(),
                        false);

        X2Criteria fteMontlyCriteria = new X2Criteria();
        fteMontlyCriteria.addEqualTo(FteMonthly.FIELD_REPORT_DATE.resolve(getDictExtractor()),
                s_aliasDateFormat.format(getReportDate()));
        ToolBean.addAndCriteria(getBroker(), FteMonthly.class, fteMontlyCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(), Arrays.asList(ToolBean.FIELD_OID),
                OnStudent.CHILD_FTE_MONTHLY_RECORDS);

        X2Criteria fteCriteria = new X2Criteria();
        ToolDistrictContext context = organization.getSchoolYearContext(getBroker(),
                getReportDate());
        String contextId = context == null ? getCurrentContext().getContextId() : context.getContextId();
        fteCriteria.addEqualTo(FteRecord.FIELD_SCHOOL_YEAR.resolve(getDictExtractor()),
                contextId);
        ToolBean.addAndCriteria(getBroker(), FteRecord.class, fteCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(), Arrays.asList(ToolBean.FIELD_OID),
                OnStudent.CHILD_FTE_RECORDS);

        // preload salep
        ToolBean.addAndCriteria(getBroker(), OnStudentSalep.class, ToolStudentProgramParticipation
                .getDateRangeCriteria(getBroker(), getReportDate(), getCurrentContext().getEndDate()));
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE), OnStudent.CHILD_SALEP_PROGRAMS);

    }

    /**
     * Computes data of summary report by filtering data from enrollments
     * according to provided filters.
     *
     * @param studentEnrollments Filterable<StudentEnrollment>
     * @param enrollmentFilters Map<String,Filter>
     * @param computeHighCreditFte boolean
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveEnrollmentReportData(Filterable<OnEnrollment> studentEnrollments,
                                                                  Map<String, Filter> enrollmentFilters,
                                                                  boolean computeHighCreditFte) {
        EnrByAttSummaryContainer enrByAttendanceSumRecord =
                new EnrByAttSummaryContainer();
        Collection<ToolEnrollment> ftU21Enrollments = studentEnrollments
                .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0))).extract();

        enrByAttendanceSumRecord
                .setFullTimeU21NumberOfStudents(ftU21Enrollments.size());
        List<BigDecimal> fteValues = new ArrayList<>();
        setFTEValues(ftU21Enrollments, fteValues, ENR_ATT_TYPE_FT, computeHighCreditFte);
        enrByAttendanceSumRecord.setFullTimeU21RegularFTE(fteValues.get(0));
        enrByAttendanceSumRecord.setFullTimeU21HighCreditFTE(fteValues.get(1));

        Collection<ToolEnrollment> ptU21Enrollments = studentEnrollments
                .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1))).extract();
        enrByAttendanceSumRecord
                .setPartTimeU21NumberOfStudents(ptU21Enrollments.size());
        fteValues = new ArrayList<>();
        setFTEValues(ptU21Enrollments, fteValues, ENR_ATT_TYPE_PT, computeHighCreditFte);
        enrByAttendanceSumRecord.setPartTimeU21RegularFTE(fteValues.get(0));
        enrByAttendanceSumRecord.setPartTimeU21HighCreditFTE(fteValues.get(1));
        Collection<StudentEnrollment> ftO21Enrollments = studentEnrollments
                .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2))).extract();
        enrByAttendanceSumRecord
                .setFullTimeO21NumberOfStudents(ftO21Enrollments == null ? 0 : ftO21Enrollments.size());

        Collection<ToolEnrollment> ptO21Enrollments = studentEnrollments
                .filter(enrollmentFilters.get(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3))).extract();
        enrByAttendanceSumRecord
                .setPartTimeO21NumberOfStudents(ptO21Enrollments.size());
        fteValues = new ArrayList<>();
        setFTEValues(ptO21Enrollments, fteValues, ENR_ATT_TYPE_PT, false);
        enrByAttendanceSumRecord.setPartTimeO21RegularFTE(fteValues.get(0));
        return enrByAttendanceSumRecord;
    }

    /**
     * Computes data of 'Government of Canada' records.
     *
     * @param studentEnrollments Filterable<StudentEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveGovernmentOfCanadaData(Filterable<OnEnrollment> studentEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT",
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="03"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, false, BOARD_RES_STATUS_03));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT"+
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="03"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, false, BOARD_RES_STATUS_03));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT",
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="03"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, true, BOARD_RES_STATUS_03));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT",
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="03"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, true, BOARD_RES_STATUS_03));

        return retrieveEnrollmentReportData(studentEnrollments, enrollmentFilters, true);
    }

    /**
     * Computes data of 'Native students under tuition agreement with a Native
     * education authority' records.
     *
     * @param studentEnrollments Filterable<StudentEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveNativeEducationAuthorityData(Filterable<OnEnrollment> studentEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT"+
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="02"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, false, BOARD_RES_STATUS_02));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT"+
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="02"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, false, BOARD_RES_STATUS_02));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT"+
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="02"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, true, BOARD_RES_STATUS_02));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT"+
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="02"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, true, BOARD_RES_STATUS_02));

        return retrieveEnrollmentReportData(studentEnrollments, enrollmentFilters, true);
    }

    /**
     * Computes data of 'Other Students' records.
     *
     * @param studentEnrollments Filterable<StudentEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveOtherStudentsData(Filterable<OnEnrollment> studentEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT",
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="07"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, false, BOARD_RES_STATUS_07));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT"+
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="07"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, false, BOARD_RES_STATUS_07));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT",
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="07"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, true, BOARD_RES_STATUS_07));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT",
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="07"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, true, BOARD_RES_STATUS_07));

        return retrieveEnrollmentReportData(studentEnrollments, enrollmentFilters, true);
    }

    /**
     * Inits the pupil of board secondary only data.
     *
     * @param studentEnrollments Filterable<StudentEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrievePupilOfBoardSecondaryOnlyData(Filterable<OnEnrollment> studentEnrollments) {

        Map<String, Filter> enrollmentFilters = new HashMap<>();
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT", GRADE_TYPE_CODE > 9,
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="01"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFilterForPupilOfBoardSecondary(ENR_ATT_TYPE_FT, false, INT_9,
                        BOARD_RES_STATUS_01));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT", GRADE_TYPE_CODE > 9,
        // STUDENT.STUDENT_DOB < 21, STU_BRD_RES_STAT_TYPE_ID="01"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFilterForPupilOfBoardSecondary(ENR_ATT_TYPE_PT, false, INT_9,
                        BOARD_RES_STATUS_01));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT", GRADE_TYPE_CODE > 9,
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="01"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFilterForPupilOfBoardSecondary(ENR_ATT_TYPE_FT, true, INT_9,
                        BOARD_RES_STATUS_01));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT", GRADE_TYPE_CODE > 9,
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="01"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFilterForPupilOfBoardSecondary(ENR_ATT_TYPE_PT, true, INT_9,
                        BOARD_RES_STATUS_01));
        return retrieveEnrollmentReportData(studentEnrollments, enrollmentFilters, true);
    }

    /**
     * Computes data of 'Study Permit/Temp Resident' records.
     *
     * @param studentEnrollments Filterable<StudentEnrollment>
     * @return EnrByAttSummaryContainer
     */
    private EnrByAttSummaryContainer retrieveTempResidentData(Filterable<OnEnrollment> studentEnrollments) {
        Map<String, Filter> enrollmentFilters = new HashMap<>();
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT",
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="05"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(0),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, false, BOARD_RES_STATUS_05));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT"+
        // STUDENT.STUDENT_DOB < 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="05"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(1),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, false, BOARD_RES_STATUS_05));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "FT",
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="05"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(2),
                createFileterByBoardResStatus(ENR_ATT_TYPE_FT, true, BOARD_RES_STATUS_05));
        // records, where STUDENT_SCHOOL_ENROLMENT.ATTENDANCE_TYPE_ID = "PT",
        // STUDENT.STUDENT_DOB > 21, STUDENT_SCHOOL_ENROLMENT.STU_BRD_RES_STAT_TYPE_ID="05"
        enrollmentFilters.put(ENR_RECORDS_KEYS_BY_AGE_AND_ATTENDANCE.get(3),
                createFileterByBoardResStatus(ENR_ATT_TYPE_PT, true, BOARD_RES_STATUS_05));

        return retrieveEnrollmentReportData(studentEnrollments, enrollmentFilters, true);
    }

    /**
     * Initializes/computes FTE values.
     *
     * @param stdEnrollments Collection<StudentEnrollment>
     * @param fteContainer List<BigDecimal>
     * @param attendanceType String
     * @param computeHighCreditFTE boolean
     */
    private void setFTEValues(Collection<ToolEnrollment> stdEnrollments,
                              List<BigDecimal> fteContainer,
                              String attendanceType,
                              boolean computeHighCreditFTE) {
        BigDecimal sumFTE = BigDecimal.ZERO;
        BigDecimal sumHighCreditFTE = BigDecimal.ZERO;
        for (ToolEnrollment stdEnr : stdEnrollments) {
            OnStudent student = (OnStudent) stdEnr.getStudent(getBroker());
            BigDecimal fte = BigDecimal.ZERO;
            BigDecimal highCreditFte = BigDecimal.ZERO;
            BigDecimal finalFte = BigDecimal.ZERO;
            BigDecimal finalFteHighCredit = BigDecimal.ZERO;
            FteMonthly montlyFteRecordFoStd = null;
            FteRecord fteRecordC = null;
            String attType = ATT_TYPE_NOT_SET;
            Object fteRecord = m_fteMap.get(stdEnr.getOid());
            if (fteRecord != null) {
                if (fteRecord instanceof FteMonthly) {
                    montlyFteRecordFoStd = (FteMonthly) fteRecord;
                } else if (fteRecord instanceof FteRecord) {
                    fteRecordC = (FteRecord) fteRecord;
                }
            }
            if (montlyFteRecordFoStd != null) {
                fte = montlyFteRecordFoStd.getFte();
                highCreditFte = montlyFteRecordFoStd.getFteHc();
                attType = montlyFteRecordFoStd.getRegister();
            } else if (fteRecordC != null) {
                fte = fteRecordC.getFte();
                highCreditFte = fteRecordC.getFteHc();
                attType = fteRecordC.getAttendanceType();
            }

            if (m_sskByStdOid.containsKey(student.getOid()) && m_sskByStdOid.get(student.getOid())
                    .getSchool(getBroker()).getOid()
                    .equals(getSchoolDateRangeProvider((OnSchool) stdEnr.getSchool(getBroker())).getSchool()
                            .getOid())) {
                continue;
            }
            if (attendanceType.equals(attType)) {
                finalFte = fte.compareTo(FTE_FT_FINAL_THRESHOLD) >= 0 ? BigDecimal.ONE : fte;
            }
            if (computeHighCreditFTE) {
                if (attendanceType.equals(attType)) {
                    if (highCreditFte.compareTo(BigDecimal.ZERO) > 0) {
                        finalFteHighCredit = fte.compareTo(FTE_FT_FINAL_THRESHOLD) >= 0
                                ? BigDecimal.ZERO
                                : fte.compareTo(BigDecimal.ZERO) == 0
                                        ? highCreditFte
                                        : BigDecimal.ONE.subtract(fte);
                    }
                }
                sumHighCreditFTE = sumHighCreditFTE.add(finalFteHighCredit);
            }
            sumFTE = sumFTE.add(finalFte);
        }
        fteContainer.add(sumFTE);
        fteContainer.add(sumHighCreditFTE);
    }
}
