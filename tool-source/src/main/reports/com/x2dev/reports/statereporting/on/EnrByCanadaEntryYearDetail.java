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
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolOrganization;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteMonthly;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteRecord;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class EnrByAttendanceTypeDetail.
 */
public class EnrByCanadaEntryYearDetail extends OnReportJavaSourceNew {

    /**
     * The Class Range.
     */
    private class Range {
        private final static String DATE_FORMAT = "MMM d, yyyy";
        private final SimpleDateFormat s_dateFormat = new SimpleDateFormat(DATE_FORMAT);

        private PlainDate m_endDate;
        private PlainDate m_startDate;

        /**
         * Instantiates a new range.
         *
         * @param startDate PlainDate
         * @param endDate PlainDate
         */
        public Range(PlainDate startDate, PlainDate endDate) {
            m_startDate = startDate;
            m_endDate = endDate;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            try {
                return s_dateFormat.format(m_startDate) + " to " + s_dateFormat.format(m_endDate);
            } catch (Exception e) {
                return "Cannot parse dates: " + m_startDate + "/" + m_endDate;
            }
        }
    }

    /**
     * The Class Record.
     */
    private static class Record implements RecordInterface {
        private Map<FieldInterface, Object> m_fieldValuePairs = new HashMap<FieldInterface, Object>();

        /**
         * The Enum Field.
         */
        private enum Field implements FieldInterface {
            age,
            //
            academicYears,
            //
            arrivalStatus,
            //
            arrivalDate,
            //
            arrivalYearToSort,
            //
            birthCountry,
            //
            boardName,
            //
            boardNumber,
            //
            isCounted,
            //
            currentDate,
            //
            grade,
            //
            gradeNumeric,
            //
            gradeToDisplay,
            //
            is21plus,
            //
            isELearning,
            //
            isFullyHighCredit,
            //
            isPupilOfTheBoard,
            //
            isSharedStudent,
            //
            oen,
            //
            printDetail,
            //
            programName,
            //
            schoolName,
            //
            schoolNumber,
            //
            studentName,
            //
            studentNameToSort,
            //
            totalAllStudents,
            //
            totalCountedStudents,
            //
            totalCountedStudents21AndOver,
            //
            totalCountedStudentsUnder21,
            //
            totalGradeGender,
            //
            boardResidenceCode
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardNumber.toString(),
                        Field.schoolNumber.toString(),
                        Field.studentName.toString(),
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
    class SchoolDateRangeProvider implements OnSchoolDateRangeProvider {

        X2Broker m_broker;
        DistrictSchoolYearContext m_ctxByDate;
        DataDictionary m_dictionary;

        /**
         * Instantiates a new school date range provider.
         *
         * @param school OnSchool
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
            return EnrByCanadaEntryYearDetail.this.getBroker();
        }

        /**
         * Gets the district dictionary.
         *
         * @return Data dictionary
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisDictionaryProvider#getDistrictDictionaryProvider()
         */
        public DataDictionary getDistrictDictionary() {
            if (m_dictionary == null) {
                m_dictionary = getDictionary();
            }
            return m_dictionary;
        }

        /**
         * Gets the dictionary extractor.
         *
         * @return Dictionary extractor
         * @see com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnDictionaryExtractorProvider#getDictionaryExtractor()
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
            return getContextByDate(getReportDate());
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
                m_ctxByDate = found;
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
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisSchoolProvider#getSchool()
         */
        public OnSchool getOnSchool() {
            return m_school;
        }

        /**
         * Gets the school.
         *
         * @return On school
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

        /**
         * Sets the broker.
         *
         * @param broker void
         */
        public void setBroker(X2Broker broker) {
            m_broker = broker;
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


    private static final long serialVersionUID = 1L;

    private static final String ARRIVAL_DATE_FORMAT = "yyyy/MM";
    private static final List COUNTRY_STATE_CODES_NOT_INCLUDED = Arrays.asList("CAN");
    private static final List COUNTRY_STATE_CODES_NOT_COUNTED_BOTH =
            Arrays.asList("034", "304", "439", "661", "664", "655", "658", "667", "665", "988");
    private static final List COUNTRY_STATE_CODES_NOT_COUNTED_FR =
            Arrays.asList("202", "256", "376", "208", "508", "544", "211", "430", "212", "695");
    private static final String ORG_CODE_ONSIS_LANG_ENG = "E";
    private static final String ORG_CODE_ONSIS_LANG_FR = "F";
    private static final String VALUE_BIRTH_COUNTRY_DEFAULT = "";
    private static final String REPORT_ID_CSV = "ON-VRF-EBCEY-CSV";
    private static final String REPORT_ID_PDF = "ON-VRF-EBCEY-ELEM-DETAIL";

    /**
     * Class members.
     */
    private DictionaryExtractor m_dictExtractor;
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private GradesHelper m_gradesHelper;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private PlainDate m_range1EndDate;
    private Map<String, ToolEnrollment> m_recentEnrByStdOid = new HashMap<>();
    private PlainDate m_september1back4years;
    private transient Map<String, SchoolDateRangeProvider> m_schoolDateRangeProvider;
    private SchoolDateRangeProvider m_sklDateRangeProvider;
    private EnrollmentSpanCriteria m_spanCriteria;
    private final SimpleDateFormat s_arrivalDateFormat = new SimpleDateFormat(ARRIVAL_DATE_FORMAT);
    private final List<String> s_boardResidenceCounted =
            Arrays.asList(BOARD_RES_STATUS_PUPIL_OF_THE_BOARD, BOARD_RES_STATUS_INDEPENDENT_STUDY);
    private final List<String> s_boardResidenceTypesSecondary = Arrays.asList(BOARD_RES_STATUS_INDEPENDENT_STUDY,
            BOARD_RES_STATUS_ELEARNING, BOARD_RES_STATUS_INDEPENDENT);
    private final List<String> s_boardResidenceTypesAll = Arrays.asList(BOARD_RES_STATUS_PUPIL_OF_THE_BOARD,
            BOARD_RES_STATUS_SHARED, BOARD_RES_STATUS_SHARED);

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
        final List<String> reportableCodes = getReportableResidenceCodes();
        Filterable<OnSchool> filterableSkls = getSchoolsFromInput();
        if (filterableSkls != null) {
            Collection<String> sklOidsToReport =
                    filterableSkls.extract().stream().map(skl -> skl.getOid()).collect(Collectors.toList());
            Filterable<Record> recordsFilterable = filterableSkls.map(new Mapper<OnSchool, Record>() {
                @Override
                public Filterable<Record> map(OnSchool school) {
                    initializeHelpersForSchool(school);
                    Filterable<ReportStudent> students = FilterableFactory
                            .createFilterableToolBeans(getStudents(m_spanCriteria, false, false, m_annualSpanFilter))
                            .filter(new Filter<ReportStudent>() {
                                @Override
                                public boolean isFiltered(ReportStudent toFilter) {
                                    OnEnrollment recentEnrolment = null;
                                    List<AnnualSpan> annualSpans = toFilter
                                            .getEnrollmentSpans(getBroker(), false, false).stream()
                                            .filter(annualSpan -> annualSpan.getSchool().getOid()
                                                    .equals(school.getOid())
                                                    || annualSpan.isSecondary())
                                            .filter(annualSpan -> annualSpan.getContext().getOid()
                                                    .equals(getSchoolDateRangeProvider(school)
                                                            .getContextByDate(getReportDate()).getOid()))
                                            .collect(Collectors.toList());
                                    if (annualSpans != null && !annualSpans.isEmpty()) {
                                        OnAnnualSpan firstAnnualSpan =
                                                (OnAnnualSpan) ToolsSharedContainer.reverse(annualSpans.stream())
                                                        .findFirst().get();
                                        if (firstAnnualSpan != null) {
                                            recentEnrolment = (OnEnrollment) firstAnnualSpan.getRecentEnrollmentESY();
                                            if (recentEnrolment == null && firstAnnualSpan.isSecondary()
                                                    && sklOidsToReport
                                                            .contains(firstAnnualSpan.getSecondary()
                                                                    .getSchool(getBroker()).getOid())
                                                    && firstAnnualSpan
                                                            .getBestPrimarySpanFor(getBroker()) != null) {
                                                recentEnrolment = (OnEnrollment) firstAnnualSpan
                                                        .getBestPrimarySpanFor(getBroker())
                                                        .getRecentEnrollmentESY();
                                            }
                                        }
                                    }
                                    String boardResidenceCode = null;
                                    if (recentEnrolment != null) {
                                        boardResidenceCode = recentEnrolment.getBoardResidentStatus();
                                        m_recentEnrByStdOid.put(toFilter.getOid(), recentEnrolment);
                                    }
                                    boolean filtered = !StringUtils.isEmpty(boardResidenceCode)
                                            && reportableCodes.contains(boardResidenceCode)
                                            && isCanadaEntryInLast4Years(toFilter) && !COUNTRY_STATE_CODES_NOT_INCLUDED
                                                    .contains(getBirthCountry(toFilter, true));
                                    return filtered;

                                }
                            });
                    preloadStudentsData();
                    return getRecords(students, sklOidsToReport);
                }
            });
            List<Record> sortedRecords = recordsFilterable.extractSorted(Arrays.asList(
                    Record.Field.boardNumber.toString(),
                    Record.Field.schoolNumber.toString(),
                    Record.Field.gradeNumeric.toString(),
                    Record.Field.arrivalYearToSort.toString(),
                    Record.Field.studentNameToSort.toString(),
                    Record.Field.oen.toString()), true);

            for (Record record : sortedRecords) {
                assignTotals(record, recordsFilterable);
                grid.append();
                for (FieldInterface field : record.m_fieldValuePairs.keySet()) {
                    grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                }
            }
        }
        grid.beforeTop();
        if (getErrorsLog().length() > 0) {
            addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
        }
        addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
        addParameter(REPORT_PARAM_AS_OF_DATE, s_asOfDateFormat.format(getReportDate()));
        return grid;
    }

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

        m_dictExtractor = new DictionaryExtractor(getBroker());
        initGradesMap();
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
            int totalGradeGender = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber,
                    Record.Field.grade), record, filterable).count();
            record.set(Record.Field.totalGradeGender, totalGradeGender);
        }

        {
            int totalCountedStudents = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable)
                            .filter(Record.Field.isCounted.toString(), Boolean.TRUE)
                            .count();
            record.set(Record.Field.totalCountedStudents, totalCountedStudents);
        }

        {
            int totalCountedUnder21 = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable)
                            .filter(Arrays.asList(Record.Field.isCounted.toString(), Record.Field.is21plus.toString()),
                                    Arrays.asList(Boolean.TRUE, Boolean.FALSE))
                            .count();
            record.set(Record.Field.totalCountedStudentsUnder21, totalCountedUnder21);
        }

        {
            int totalCounted21Plus = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable)
                            .filter(Arrays.asList(Record.Field.isCounted.toString(), Record.Field.is21plus.toString()),
                                    Arrays.asList(Boolean.TRUE, Boolean.TRUE))
                            .count();
            record.set(Record.Field.totalCountedStudents21AndOver, totalCounted21Plus);
        }

        {
            int totalAllStudents = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable).count();
            record.set(Record.Field.totalAllStudents, totalAllStudents);
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
     * Gets the arrival date.
     *
     * @param student ReportStudent
     * @return String
     */
    private String getArrivalDate(ReportStudent student) {
        PlainDate arrivalDate = student.getArrivalDateCanada();
        if (arrivalDate != null) {
            return s_arrivalDateFormat.format(arrivalDate);
        }
        return null;
    }

    /**
     * Gets the arrival year.
     *
     * @param student ReportStudent
     * @return String
     */
    private Long getArrivalTimestamp(ReportStudent student) {
        PlainDate arrivalDate = student.getArrivalDateCanada();
        if (arrivalDate != null) {
            return arrivalDate.getTime();
        }
        return null;
    }

    /**
     * Gets the birth country.
     *
     * @param student ReportStudent
     * @param stateValue boolean
     * @return String
     */
    private String getBirthCountry(ReportStudent student, boolean stateValue) {
        String value = stateValue ? student.getBirthCountry() : student.getBirthCountryDescription();
        return value == null ? VALUE_BIRTH_COUNTRY_DEFAULT : value;
    }

    /**
     * Gets the fte monthly record.
     *
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
     * Gets the previous year range.
     *
     * @param year int
     * @return Range
     */
    private Range getPreviousYearRange(int year) {
        Calendar calendar = Calendar.getInstance();

        calendar.set(Calendar.YEAR, year);
        calendar.set(Calendar.MONTH, Calendar.AUGUST);
        calendar.set(Calendar.DATE, 31);

        PlainDate rangeEndDate = new PlainDate(calendar.getTime());

        calendar.set(Calendar.YEAR, year - 1);
        calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
        calendar.set(Calendar.DATE, 1);

        PlainDate rangeStartDate = new PlainDate(calendar.getTime());

        return new Range(rangeStartDate, rangeEndDate);
    }

    /**
     * Gets the ranges.
     *
     * @return List
     */
    private List<Range> getRanges() {
        Calendar calendar = Calendar.getInstance();
        List<Range> ranges = new ArrayList<>();
        int sklYearByDate = m_sklDateRangeProvider.getContextByDate(getReportDate()).getSchoolYear();
        calendar.set(Calendar.YEAR, sklYearByDate - 1);
        calendar.set(Calendar.MONTH, Calendar.OCTOBER);
        calendar.set(Calendar.DATE, 31);

        PlainDate range1EndDate = new PlainDate(calendar.getTime());

        calendar.set(Calendar.YEAR, sklYearByDate - 2);
        calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
        calendar.set(Calendar.DATE, 1);

        PlainDate range1StartDate = new PlainDate(calendar.getTime());

        ranges.add(new Range(range1StartDate, range1EndDate));

        ranges.add(getPreviousYearRange(sklYearByDate - 2));
        ranges.add(getPreviousYearRange(sklYearByDate - 3));
        ranges.add(getPreviousYearRange(sklYearByDate - 4));

        return ranges;
    }

    /**
     * Gets the records.
     *
     * @param students Filterable<ReportStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<Record> getRecords(Filterable<ReportStudent> students,
                                          Collection<String> sklOidsToReport) {
        List<Record> records = new ArrayList<Record>();
        try {
            String schoolName = m_sklDateRangeProvider.getSchool().getName();
            String schoolNumber = m_sklDateRangeProvider.getSchool().getBsid();
            if (StringUtils.isEmpty(schoolNumber)) {
                throw new RuntimeException(
                        "BSID is not defined for school " + m_sklDateRangeProvider.getSchool().getName());
            }
            ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
            String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
            String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
            String academicYears = getCurrentContext().getContextId();
            String programName = "Aspen";
            String currentDate = m_formatter
                    .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
            for (ReportStudent student : students.extract()) {
                Record record = new Record();
                record.set(Record.Field.schoolName, schoolName);
                record.set(Record.Field.schoolNumber, schoolNumber);
                record.set(Record.Field.printDetail, "Y");
                record.set(Record.Field.boardNumber, boardNumber);
                record.set(Record.Field.boardName, boardName);
                record.set(Record.Field.academicYears, academicYears);
                record.set(Record.Field.programName, programName);
                record.set(Record.Field.currentDate, currentDate);
                record.set(Record.Field.oen, student.getOenRaw());
                record.set(Record.Field.studentName, getStudentNameFirstMiddleLast(student));
                record.set(Record.Field.studentNameToSort, getStudentNameLastFirstMiddle(student));
                record.set(Record.Field.is21plus, is21plus(student));
                record.set(Record.Field.age, String.valueOf(student.getAgeAsOfDate(getReportDate())));
                OnEnrollment recentEnrolment = (OnEnrollment) m_recentEnrByStdOid.get(student.getOid());
                if (recentEnrolment != null) {
                    record.set(Record.Field.isFullyHighCredit, isFullyHighCredit(record, recentEnrolment));
                    record.set(Record.Field.boardResidenceCode, recentEnrolment.getBoardResidentStatus());
                    int yog = getYog(student, recentEnrolment);
                    String grade = m_gradesHelper.getGradeLevel(yog);
                    if (StringUtils.isEmpty(grade)) {
                        grade = student.getGradeLevel();
                    }
                    String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                            ALIAS_DDX_NUM_GRADE, m_dictExtractor.getDictionary(DDX_ID_GRADES));
                    if (!StringUtils.isEmpty(numGradeLevel)) {
                        record.set(Record.Field.gradeNumeric, new Integer(numGradeLevel));
                    }
                    record.set(Record.Field.gradeToDisplay, m_gradesMap.get(grade).getDescription());
                    record.set(Record.Field.grade, m_gradesMap.get(grade).getStateCode());
                    record.set(Record.Field.arrivalStatus, recentEnrolment.getArrivalStatus());
                }
                record.set(Record.Field.isCounted, isCounted(record, student));
                record.set(Record.Field.arrivalDate, getArrivalDate(student));
                Long arrivalYear = getArrivalTimestamp(student);
                arrivalYear = arrivalYear == null ? null : (-arrivalYear); // we want DESC on
                                                                           // arrivalYear field
                record.set(Record.Field.arrivalYearToSort, arrivalYear);
                record.set(Record.Field.birthCountry, getBirthCountry(student, false));
                record.set(Record.Field.isELearning,
                        BOARD_RES_STATUS_ELEARNING.equals(record.get(Record.Field.boardResidenceCode)));
                record.set(Record.Field.isPupilOfTheBoard,
                        BOARD_RES_STATUS_PUPIL_OF_THE_BOARD.equals(record.get(Record.Field.boardResidenceCode)));
                record.set(Record.Field.isSharedStudent,
                        BOARD_RES_STATUS_SHARED.equals(record.get(Record.Field.boardResidenceCode)));
                records.add(record);
            }

            if (records.isEmpty()) {
                Record record = new Record();
                record.set(Record.Field.academicYears, academicYears);
                record.set(Record.Field.boardName, boardName);
                record.set(Record.Field.boardNumber, boardNumber);
                record.set(Record.Field.currentDate, currentDate);
                record.set(Record.Field.programName, programName);
                record.set(Record.Field.schoolName, schoolName);
                record.set(Record.Field.schoolNumber, schoolNumber);
                record.set(Record.Field.printDetail, "N");
                records.add(record);
            }

        } catch (Exception e) {
            if (records.isEmpty()) {
                records.add(new Record());
            }
            logError(e.getMessage());
            e.printStackTrace();
        }
        return FilterableFactory.create(records, Record.s_uniqueFields, Record.s_valueResolver);
    }

    /**
     * Gets the reportable residence codes.
     *
     * @return List
     */
    private List<String> getReportableResidenceCodes() {
        return !isElementaryReport()
                ? Stream.concat(s_boardResidenceTypesAll.stream(), s_boardResidenceTypesSecondary.stream())
                        .collect(Collectors.toList())
                : s_boardResidenceTypesAll;
    }

    /**
     * Inits the grades map.
     */
    private void initGradesMap() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        DataDictionaryField dictionaryField = ToolStudent.FIELD_GRADE_LEVEL.getField(getDictExtractor());
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            Map<String, ReferenceCode> refCodes =
                    getDictExtractor().getReferenceCodes(dictionaryField.getReferenceTableOid());
            for (Entry<String, ReferenceCode> entry : refCodes.entrySet()) {
                String stateGradeCode = entry.getValue().getStateCode();
                if (!StringUtils.isEmpty(stateGradeCode)) {
                    m_gradesMap.put(stateGradeCode, entry.getValue());
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
        m_sklDateRangeProvider.setBroker(getBroker());
        m_sklDateRangeProvider.setSchool(school);
        m_gradesHelper = new GradesHelper(m_sklDateRangeProvider);
        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true)
                .setIncludeAllSecondarySpans(true);

        List<Range> ranges = getRanges();
        if (!ranges.isEmpty()) {
            m_september1back4years = ranges.get(ranges.size() - 1).m_startDate;
            m_range1EndDate = ranges.get(0).m_endDate;
        }
    }

    /**
     * Checks if is canada entry in last 4 years.
     *
     * @param student ReportStudent
     * @return true, if is canada entry in last 4 years
     */
    private boolean isCanadaEntryInLast4Years(ReportStudent student) {
        Date entryDate = student.getArrivalDateCanada();
        if (entryDate != null) {
            return !entryDate.before(m_september1back4years) && !entryDate.after(m_range1EndDate);
        }
        return false;
    }

    /**
     * Checks if is counted.
     *
     * @param record Record
     * @param student ReportStudent
     * @return true, if is counted
     */
    private boolean isCounted(Record record, ReportStudent student) {
        Date arrivalDate = student.getArrivalDateCanada();
        boolean isCountedBYCountryAndLang = true;
        String languageState = getOrganizationToolBean().getLanguageStateValue();
        if (ORG_CODE_ONSIS_LANG_ENG.equals(languageState)) {
            isCountedBYCountryAndLang = !COUNTRY_STATE_CODES_NOT_COUNTED_BOTH.contains(getBirthCountry(student, true));
        }
        if (ORG_CODE_ONSIS_LANG_FR.equals(languageState)) {
            isCountedBYCountryAndLang = !COUNTRY_STATE_CODES_NOT_COUNTED_BOTH.contains(getBirthCountry(student, true))
                    && !COUNTRY_STATE_CODES_NOT_COUNTED_FR.contains(getBirthCountry(student, true));
        }
        return s_boardResidenceCounted.contains(record.get(Record.Field.boardResidenceCode))
                && isCanadaEntryInLast4Years(student)
                && !((Boolean) record.get(Record.Field.is21plus))
                && !((Boolean) record.get(Record.Field.isFullyHighCredit))
                && arrivalDate != null && !arrivalDate.after(m_range1EndDate)
                && isCountedBYCountryAndLang;
    }

    /**
     * Checks if is fully high credit.
     *
     * @param record Record
     * @param stdEnr StudentEnrollment
     * @return true, if is fully high credit
     */
    private boolean isFullyHighCredit(Record record, ToolEnrollment stdEnr) {
        if (!isElementaryReport()) {
            BigDecimal highCreditFte = BigDecimal.ZERO;
            ReportStudent student = (ReportStudent) stdEnr.getStudent(getBroker());
            FteMonthly montlyFteRecordFoStd = getFteMonthlyRecord((OnSchool) stdEnr.getSchool(getBroker()), student);
            FteRecord fteRecordC = null;
            if (montlyFteRecordFoStd != null) {
                highCreditFte = montlyFteRecordFoStd.getFteHc();
            } else {
                List<FteRecord> fteRecordsFoStd = student.getFteRecords(getBroker());
                if (fteRecordsFoStd != null) {
                    fteRecordC = findFteRecord(fteRecordsFoStd, (OnSchool) stdEnr.getSchool(getBroker()));
                    if (fteRecordC != null) {
                        highCreditFte = fteRecordC.getFteHc();
                    }
                }
                if (fteRecordC == null) {
                    ReportStudent std = (ReportStudent) stdEnr.getStudent(getBroker());
                    String errorMessage = "Can't find FTE record for student: "
                            + getStudentNameFirstMiddleLast(std)
                            + ", OEN: "
                            + std.getOenRaw()
                            + ", School Name: "
                            + std.getSchool(getBroker()).getName()
                            + " for District Context: "
                            + m_sklDateRangeProvider.getContextByDate(getReportDate()).getContextId();
                    throw new DataErrorException(errorMessage);
                }
            }
            if (highCreditFte.compareTo(BigDecimal.ZERO) > 0) {
                return true;
            }
            if (BOARD_RES_STATUS_INDEPENDENT.equals(record.get(Record.Field.boardResidenceCode))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Preload students data.
     */
    private void preloadStudentsData() {
        ToolBean.clearAllCachedToolBeans(FteMonthly.class);
        ToolBean.clearAllCachedToolBeans(FteRecord.class);

        ToolBean.resetCriteria(getBroker(), FteMonthly.class);
        ToolBean.resetCriteria(getBroker(), FteRecord.class);

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
    }
}
