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

import com.follett.fsc.core.framework.persistence.SubQuery;
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
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSchool;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class EnrByAttendanceTypeElemDetailReportData.
 */
public class EnrByAttendanceTypeSecDetailReportData extends OnReportJavaSourceNew {


    /**
     * The Class FteRecord.
     */
    private static class Record implements RecordInterface {
        private Map<FieldInterface, Object> m_fieldValuePairs = new HashMap<FieldInterface, Object>();

        /**
         * The Enum Field.
         */
        private enum Field implements FieldInterface {
            age,
            //
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
            boardResidenceStatus,
            //
            attendanceType,
            //
            gender,
            //
            grade,
            //
            gradeToSort,
            //
            oen,
            //
            studentName,
            //
            studentNameToSort,
            //
            is21plus,
            //
            isSal,
            //
            fteAsReportedRegular,
            //
            fteAsReportedHighCredit,
            //
            finalFteRegular,
            //
            finalFteHighCredit,
            //
            counted,
            //
            totalGradeGender,
            //
            totalStudentsByBoardResidenceStatus,
            //
            totalSalForSchool,
            //
            totalStudentsForSchool,
            //
            totalStudentsForBoard,
            //
            yog
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardNumber.toString(),
                        Field.schoolNumber.toString(),
                        Field.oen.toString());

        private static ValueByKeyResolver<Record> s_valueResolver = new ValueByKeyResolver<Record>() {
            @Override
            public Object getValue(String key, Record entity) {
                return entity.get(Field.valueOf(key));
            }
        };

        /**
         * Gets the.
         *
         * @param key Field
         * @return Object
         */
        @Override
        public Object get(FieldInterface key) {
            return m_fieldValuePairs.get(key);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        @Override
        public void set(FieldInterface field, Object value) {
            m_fieldValuePairs.put(field, value);
        }
    }

    /**
     * The Class SchoolDateRangeProvider.
     */
    private class SchoolDateRangeProvider implements OnSchoolDateRangeProvider {

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
            return EnrByAttendanceTypeSecDetailReportData.this.getBroker();
        }

        /**
         * Gets the district dictionary.
         *
         * @return Data dictionary
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDictionaryProvider#getDistrictDictionaryProvider()
         */
        @Override
        public DictionaryExtractor getDictionaryExtractor() {
            return getDictExtractor();
        }

        /**
         * Gets the context.
         *
         * @return District school year context
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisCTXProvider#getDistrictSchoolYearContext()
         */
        @Override
        public DistrictSchoolYearContext getContext() {
            return getCurrentContext();
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
         * @return the school
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolProvider#getSchool()
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
    }

    private static final long serialVersionUID = 1L;
    private final List<String> s_isCounted = Arrays.asList(BOARD_RES_STATUS_PUPIL_OF_THE_BOARD);
    /**
     * Constants
     */
    private static final String ATT_TYPE_NOT_SET =
            "FTE was not found Or values by aliases udc-fte-register or udd-fte-register are not set!!!";
    private static final String ENR_ATT_TYPE_PT = "PT";
    private static final String REPORT_ID_CSV = "ON-VRF-EBA-SEC-DET-CSV";
    private static final String REPORT_ID_PDF = "ON-VRF-EBA-SEC-DETAIL";

    /**
     * Class members
     */
    private DictionaryExtractor m_dictExtractor;
    private GradesHelper m_gradesHelper;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private transient Map<String, SchoolDateRangeProvider> m_schoolDateRangeProvider;
    private Collection<String> m_sklBsidCodes = new HashSet();
    private EnrollmentSpanCriteria m_spanCriteria;
    private Map<String, ReportStudentSchool> m_sskByStdOid = new HashMap<>();

    /**
     * Gets the school date range provider.
     *
     * @param school the school
     * @return the school date range provider
     */
    public SchoolDateRangeProvider getSchoolDateRangeProvider(OnSchool school) {
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
            Collection<OnSchool> schools = filterableSkls.extract().stream().collect(Collectors.toList());
            Collections.sort((List<OnSchool>) schools, (s1, s2) -> s1.getOid().compareTo(s2.getOid()));
            Filterable<Record> fteRecordsFilterable = FilterableFactory.createFilterableToolBeans(schools)
                    .map(new Mapper<OnSchool, Record>() {
                        @Override
                        public Filterable<Record> map(OnSchool school) {
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
            List<Record> sortedRecords = fteRecordsFilterable.extractSorted(Arrays.asList(
                    Record.Field.boardNumber.toString(),
                    Record.Field.schoolNumber.toString(),
                    Record.Field.boardResidenceStatus.toString(),
                    Record.Field.attendanceType.toString(),
                    Record.Field.gradeToSort.toString(),
                    Record.Field.gender.toString(),
                    Record.Field.studentNameToSort.toString(),
                    Record.Field.oen.toString()), true);
            for (Record record : sortedRecords) {
                assignTotals(record, fteRecordsFilterable);
                grid.append();
                for (FieldInterface field : record.m_fieldValuePairs.keySet()) {
                    grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                }
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

        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(FteMonthly.class);
        ToolBean.registerClass(FteRecord.class);
        ToolBean.registerClass(OnStudentSalep.class);

        m_dictExtractor = new DictionaryExtractor(getBroker());
        initGradesMap();
        initSklBSIDCodes();
    }

    /**
     * Assign totals.
     *
     * @param record FteRecord
     * @param filterable Filterable<FteRecord>
     * @return FteRecord
     */
    private Record assignTotals(Record record, Filterable<Record> filterable) {
        {
            int totalStudentsForBoard = filterByFields(Arrays.asList(
                    Record.Field.boardNumber), record, filterable).count();
            record.set(Record.Field.totalStudentsForBoard, String.valueOf(totalStudentsForBoard));
        }

        {
            int totalGradeGender = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber,
                    Record.Field.boardResidenceStatus,
                    Record.Field.attendanceType,
                    Record.Field.gradeToSort,
                    Record.Field.gender), record, filterable).count();
            record.set(Record.Field.totalGradeGender, String.valueOf(totalGradeGender));
        }

        {
            List<List<Record>> recordsByResidenceStatus = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable)
                            .groupBy(Arrays.asList(Record.Field.boardResidenceStatus.toString()));
            Collections.sort(recordsByResidenceStatus, new Comparator<List<Record>>() {
                @Override
                public int compare(List<Record> o1, List<Record> o2) {
                    return ((String) o1.get(0).get(Record.Field.boardResidenceStatus))
                            .compareTo(((String) o2.get(0).get(Record.Field.boardResidenceStatus)));
                }
            });
            StringBuilder totalByResidenceStatusBuilder = new StringBuilder();
            for (List<Record> currentRecords : recordsByResidenceStatus) {
                if (totalByResidenceStatusBuilder.length() != 0) {
                    totalByResidenceStatusBuilder.append("\n");
                }
                String currentBoardResidenceStatus =
                        (String) currentRecords.get(0).get(Record.Field.boardResidenceStatus);
                if (currentBoardResidenceStatus.startsWith("0")) {
                    currentBoardResidenceStatus = currentBoardResidenceStatus.replaceFirst("0", "");
                }
                totalByResidenceStatusBuilder
                        .append("Total Students (" + currentBoardResidenceStatus + ") : " + currentRecords.size());
            }
            record.set(Record.Field.totalStudentsByBoardResidenceStatus, totalByResidenceStatusBuilder.toString());
        }

        {
            int totalStudentsForSchool = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable).count();
            record.set(Record.Field.totalStudentsForSchool, String.valueOf(totalStudentsForSchool));
        }

        {
            int totalSalForSchool = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable)
                            .filter(Record.Field.isSal.toString(), Boolean.TRUE).count();
            record.set(Record.Field.totalSalForSchool, String.valueOf(totalSalForSchool));
        }

        return record;
    }

    /**
     * Find fte record.
     *
     * @param fteRecords List<FteRecord>
     * @param sklToLookForFTE OnSchool
     * @return FteRecord
     */
    private FteRecord findFteRecord(List<FteRecord> fteRecords, OnSchool sklToLookForFTE) {
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
     * Gets the fte monthly record.
     *
     * @param broker X2Broker
     * @param school the school
     * @param student the submission period code
     * @return FteMonthly
     */
    private FteMonthly getFteMonthlyRecord(X2Broker broker, OnSchool school, ReportStudent student) {
        List<FteMonthly> records = student.getFteMonthlyRecords(broker);
        return records.stream()
                .filter(fteMonthly -> school.getOid().equals(fteMonthly.getSchoolOid()))
                .findFirst()
                .orElse(null);
    }

    /**
     * Gets the fte records.
     *
     * @param skl OnSchool
     * @param students Filterable<ReportStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<Record> getRecords(OnSchool skl,
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
        List<Record> fteRecords = new ArrayList<Record>();
        List<OnEnrollment> enrollments = new ArrayList<>();
        for (ReportStudent student : students) {
            try {
                OnEnrollment recentEnrolment = null;
                List<AnnualSpan> annualSpans = student.getEnrollmentSpans(getBroker(), false, false).stream()
                        .filter(annualSpan -> annualSpan.getSchool().getOid().equals(skl.getOid())
                                || annualSpan.isSecondary())
                        .filter(annualSpan -> annualSpan.getContext().getOid()
                                .equals(getSchoolDateRangeProvider(skl).getContextByDate(getReportDate()).getOid()))
                        .collect(Collectors.toList());
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
                                    (ReportStudentSchool) firstAnnualSpan.getSecondary());
                        }
                    }
                }
                OnSchool sklToLookForFTE = skl;
                if (m_sskByStdOid.containsKey(student.getOid())) {
                    ReportStudentSchool ssk = m_sskByStdOid.get(student.getOid());
                    if (ssk != null && ssk.getEndDate() != null && !ssk.getEndDate().after(getReportDate())) {
                        continue;
                    }
                    sklToLookForFTE =
                            recentEnrolment != null ? (OnSchool) recentEnrolment.getSchool(getBroker())
                                    : (OnSchool) student.getSchool(getBroker());
                }
                FteMonthly montlyFteRecordFoStd = getFteMonthlyRecord(getBroker(), sklToLookForFTE, student);
                if (recentEnrolment != null) {
                    String primarySklBsid = ((OnSchool) recentEnrolment.getSchool(getBroker())).getBsid();
                    if (!m_sskByStdOid.containsKey(student.getOid()) && (StringUtils.isEmpty(primarySklBsid) ||
                            !m_sklBsidCodes.contains(primarySklBsid))) {
                        continue;
                    }
                    Record fteRecord = new Record();
                    fteRecord.set(Record.Field.schoolName, schoolName);
                    fteRecord.set(Record.Field.schoolNumber, schoolNumber);
                    fteRecord.set(Record.Field.boardNumber, boardNumber);
                    fteRecord.set(Record.Field.boardName, boardName);
                    fteRecord.set(Record.Field.academicYears, academicYears);
                    fteRecord.set(Record.Field.programName, programName);
                    fteRecord.set(Record.Field.currentDate, currentDate);
                    fteRecord.set(Record.Field.is21plus, is21plus(student));
                    fteRecord.set(Record.Field.isSal, isSalep(student));
                    fteRecord.set(Record.Field.age, String.valueOf(student.getAgeAsOfDate(getReportDate())));

                    enrollments.add(recentEnrolment);
                    int yog = getYog(student, recentEnrolment);
                    fteRecord.set(Record.Field.yog, yog);
                    String resStatus = "___dummy___";
                    boolean isSecondaryRecord = false;
                    if (m_sskByStdOid.containsKey(student.getOid()) && m_sskByStdOid.get(student.getOid())
                            .getSchool(getBroker()).getOid().equals(skl.getOid())) {
                        isSecondaryRecord = true;
                        ReportStudentSchool sskForStd = m_sskByStdOid.get(student.getOid());
                        String sskResStatus = sskForStd.getBoardResidenceStatus();
                        DataDictionaryField sskResStatusField =
                                OnStudentSchool.FIELD_BRD_RES_STAT_TYPE.getField(getDictExtractor());
                        if (!StringUtils.isEmpty(sskResStatus)) {
                            ReferenceCode rcdStatus = sskForStd.getBoardResidenceStatusReferenceCode();
                            if (rcdStatus == null) {
                                String errorMessage = "Cannot determine reference code for enrollment of student "
                                        + getStudentNameFirstMiddleLast(student)
                                        + " "
                                        + student.getOenRaw()
                                        + " for field " + sskResStatusField.getUserLongName();
                                throw new DataErrorException(errorMessage);
                            }
                            resStatus = rcdStatus.getDescription();
                            if ("Pupil of the Board".equalsIgnoreCase(resStatus)) {
                                resStatus = "0" + resStatus;
                            }
                        } else {
                            ReferenceTable refTable = sskResStatusField.getReferenceTable();
                            if (refTable == null) {
                                throw new RuntimeException("Cannot find reference table for field ["
                                        + sskResStatusField.getJavaName() + "]");
                            }
                            resStatus = refTable.getCodeMap().get(BOARD_RES_STATUS_ELEARNING).getDescription();
                        }
                    } else {
                        resStatus = getDescription(recentEnrolment, OnEnrollment.FIELD_BRD_RES_STAT_TYPE);
                        if ("Pupil of the Board".equalsIgnoreCase(resStatus)) {
                            resStatus = "0" + resStatus;
                        }
                    }

                    fteRecord.set(Record.Field.boardResidenceStatus, resStatus); // 0pupil
                    boolean isCounted = (recentEnrolment == null && montlyFteRecordFoStd != null)
                            || isCounted(recentEnrolment.getBoardResidentStatus());
                    fteRecord.set(Record.Field.counted, isCounted && !isSecondaryRecord);
                    BigDecimal fte = BigDecimal.ZERO;
                    BigDecimal highCreditFte = BigDecimal.ZERO;
                    String attType = ATT_TYPE_NOT_SET;
                    if (montlyFteRecordFoStd != null) {
                        fte = montlyFteRecordFoStd.getFte();
                        highCreditFte = montlyFteRecordFoStd.getFteHc();
                        attType = montlyFteRecordFoStd.getRegister();
                    } else {
                        FteRecord fteRecordC = findFteRecord(student.getFteRecords(getBroker()), sklToLookForFTE);
                        if (fteRecordC != null) {
                            fte = fteRecordC.getFte();
                            highCreditFte = fteRecordC.getFteHc();
                            attType = fteRecordC.getAttendanceType();
                        }
                        if (fteRecordC == null && !m_sskByStdOid.containsKey(student.getOid())) {
                            String errorMessage = "Can't find FTE record for student: "
                                    + getStudentNameFirstMiddleLast(student)
                                    + ", OEN: "
                                    + student.getOenRaw()
                                    + ", School Name: "
                                    + student.getSchool(getBroker()).getName()
                                    + " for District Context: "
                                    + getSchoolDateRangeProvider(skl).getContextByDate(getReportDate())
                                            .getContextId();
                            logError(errorMessage);
                        }
                    }
                    BigDecimal finalFte = fte.compareTo(FTE_FT_FINAL_THRESHOLD) >= 0
                            ? BigDecimal.ONE
                            : fte;
                    BigDecimal finalFteHighCredit = null;
                    if (highCreditFte.compareTo(BigDecimal.ZERO) > 0) {
                        finalFteHighCredit = fte.compareTo(FTE_FT_FINAL_THRESHOLD) >= 0
                                ? BigDecimal.ZERO
                                : fte.compareTo(BigDecimal.ZERO) == 0
                                        ? highCreditFte
                                        : BigDecimal.ONE.subtract(fte);
                    }
                    if (isSecondaryRecord) {
                        attType = ENR_ATT_TYPE_PT;
                        fte = BigDecimal.ZERO;
                        highCreditFte = BigDecimal.ZERO;
                        finalFte = BigDecimal.ZERO;
                        finalFteHighCredit = BigDecimal.ZERO;
                    }
                    fteRecord.set(Record.Field.attendanceType, attType);
                    fteRecord.set(Record.Field.fteAsReportedRegular,
                            fte == null ? BigDecimal.ZERO : fte.setScale(2, RoundingMode.HALF_UP));
                    fteRecord.set(Record.Field.fteAsReportedHighCredit,
                            highCreditFte == null ? BigDecimal.ZERO : highCreditFte.setScale(2, RoundingMode.HALF_UP));
                    fteRecord.set(Record.Field.finalFteRegular, finalFte == null ? BigDecimal.ZERO
                            : finalFte.setScale(2, RoundingMode.HALF_UP));
                    fteRecord.set(Record.Field.finalFteHighCredit, finalFteHighCredit == null ? BigDecimal.ZERO
                            : finalFteHighCredit.setScale(2, RoundingMode.HALF_UP));
                    String grade =
                            m_gradesHelper.getGradeLevel(yog) != null ? m_gradesHelper.getGradeLevel(yog)
                                    : "dummyContent";
                    fteRecord.set(Record.Field.grade, grade);
                    if (m_gradesMap.containsKey(grade)) {
                        String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                                ALIAS_DDX_NUM_GRADE, m_dictExtractor.getDictionary(DDX_ID_GRADES));
                        if (!StringUtils.isEmpty(numGradeLevel)) {
                            fteRecord.set(Record.Field.gradeToSort, new Integer(numGradeLevel));
                        } else {
                            fteRecord.set(Record.Field.gradeToSort, new Integer(-99));
                        }
                    }
                    fteRecord.set(Record.Field.gender, student.getGenderType());
                    fteRecord.set(Record.Field.oen, student.getOenRaw());
                    fteRecord.set(Record.Field.studentName, getStudentNameFirstMiddleLast(student));
                    fteRecord.set(Record.Field.studentNameToSort, getStudentNameLastFirstMiddle(student));
                    fteRecords.add(fteRecord);
                }
            } catch (DataErrorException e) {
                logError(e.getMessage());
            }
        }
        return FilterableFactory.create(fteRecords, Record.s_uniqueFields, Record.s_valueResolver);
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
     *
     * @param school OnSchool
     */
    private void initializeHelpersForSchool(OnSchool school) {
        SchoolDateRangeProvider sklDateRangeProvider = getSchoolDateRangeProvider(school);
        m_gradesHelper = new GradesHelper(sklDateRangeProvider);
        m_spanCriteria = getSpanConfiguration(sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true)
                .setIncludeAllSecondarySpans(true);
    }

    /**
     * Checks if is counted.
     *
     * @param boardResStatus String
     * @return true, if is counted
     */
    private boolean isCounted(String boardResStatus) {
        return s_isCounted.contains(boardResStatus);
    }

    /**
     * Checks if is salep.
     *
     * @param student ReportStudent
     * @return true, if is salep
     */
    private boolean isSalep(ReportStudent student) {
        return student.getSalepPrograms(getBroker())
                .stream()
                .anyMatch(pgm -> {
                    boolean isStartDateValid = pgm.getStartDate() != null
                            && !pgm.getStartDate().after(getReportDate());
                    boolean isEndDateValid = pgm.getEndDate() == null
                            || !pgm.getEndDate().before(getReportDate());
                    return isStartDateValid && isEndDateValid;
                });
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
}
