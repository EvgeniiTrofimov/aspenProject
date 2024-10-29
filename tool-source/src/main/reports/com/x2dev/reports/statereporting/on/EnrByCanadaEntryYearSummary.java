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
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filter;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.PredefinedResolver;
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
import java.text.DateFormat;
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
public class EnrByCanadaEntryYearSummary extends OnReportJavaSourceNew {

    /**
     * The Class Range.
     */
    private class Range {
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
         * @param formatter the formatter
         * @return the string
         */
        public String toString(DateFormat formatter) {
            try {
                return formatter.format(m_startDate) + " "
                        + getUserMessageResources().getMessage(getUserMessageKeyPrefix() + "rpt.to") + " "
                        + formatter.format(m_endDate);
            } catch (Exception e) {
                return "Cannot parse dates: " + m_startDate + "/" + m_endDate;
            }
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
            return EnrByCanadaEntryYearSummary.this.getBroker();
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
    }

    /**
     * The Class Record.
     */
    private static class StudentRecord {
        private Map<Field, Object> m_fieldValuePairs = new HashMap<Field, Object>();

        /**
         * The Enum Field.
         */
        private enum Field {
            academicYears,
            //
            arrivalDate,
            //
            birthCountry,
            //
            gender,
            //
            grade,
            //
            is21plus,
            //
            isCounted,
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
            studentName
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.studentName.toString(),
                        Field.oen.toString());

        private static ValueByKeyResolver<StudentRecord> s_valueResolver = new ValueByKeyResolver<StudentRecord>() {
            @Override
            public Object getValue(String key, StudentRecord entity) {
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
     * The Class Record.
     */
    private static class SchoolGradeRecord {
        private Map<Field, Object> m_fieldValuePairs = new HashMap<Field, Object>();

        /**
         * The Enum Field.
         */
        private enum Field {
            academicYears,
            //
            boardName,
            //
            boardNumber,
            //
            currentDate,
            //
            grade,
            //
            gradeNumeric,
            //
            gradeToDisplay,
            //
            printDetail,
            //
            programName,
            //
            range1,
            //
            range1GradeTotal,
            //
            range1Total,
            //
            range2,
            //
            range2GradeTotal,
            //
            range2Total,
            //
            range3,
            //
            range3GradeTotal,
            //
            range3Total,
            //
            range4,
            //
            range4GradeTotal,
            //
            range4Total,
            //
            schoolName,
            //
            schoolNumber
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardNumber.toString(),
                        Field.schoolNumber.toString(),
                        Field.grade.toString());

        private static ValueByKeyResolver<SchoolGradeRecord> s_valueResolver =
                new ValueByKeyResolver<SchoolGradeRecord>() {
                    @Override
                    public Object getValue(String key, SchoolGradeRecord entity) {
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

    private static final long serialVersionUID = 1L;

    /**
     * Other constants
     */
    private static final List COUNTRY_STATE_CODES_NOT_INCLUDED = Arrays.asList("CAN");
    private static final List COUNTRY_STATE_CODES_NOT_COUNTED_BOTH =
            Arrays.asList("034", "304", "439", "661", "664", "655", "658", "667", "665", "988");
    private static final List COUNTRY_STATE_CODES_NOT_COUNTED_FR =
            Arrays.asList("202", "256", "376", "208", "508", "544", "211", "430", "212", "695");
    private final static String DATE_FORMAT = "MMM d, yyyy";
    private static final String ORG_CODE_ONSIS_LANG_ENG = "E";
    private static final String ORG_CODE_ONSIS_LANG_FR = "F";
    private static final String VALUE_BIRTH_COUNTRY_DEFAULT = "";

    /**
     * Class members
     */
    private SimpleDateFormat m_dateFormat;
    private DictionaryExtractor m_dictExtractor;
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private GradesHelper m_gradesHelper;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private PlainDate m_range1EndDate;
    List<Range> m_ranges = new ArrayList<>();
    private PlainDate m_september1back4years;
    private final List<String> s_boardResidenceTypesSecondary = Arrays.asList(BOARD_RES_STATUS_INDEPENDENT_STUDY);
    private final List<String> s_boardResidenceTypesAll = Arrays.asList(BOARD_RES_STATUS_PUPIL_OF_THE_BOARD);
    private transient Map<String, SchoolDateRangeProvider> m_schoolDateRangeProvider;
    private SchoolDateRangeProvider m_sklDateRangeProvider;
    private EnrollmentSpanCriteria m_spanCriteria;

    private final List<String> s_boardResidenceCounted =
            Arrays.asList(BOARD_RES_STATUS_PUPIL_OF_THE_BOARD, BOARD_RES_STATUS_INDEPENDENT_STUDY);

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
            Filterable<SchoolGradeRecord> recordsFilterable = filterableSkls
                    .map(new Mapper<OnSchool, SchoolGradeRecord>() {
                        @Override
                        public Filterable<SchoolGradeRecord> map(OnSchool school) {
                            initializeHelpersForSchool(school);
                            Filterable<StudentRecord> students = getStudentRecords(school, sklOidsToReport);
                            return getRecords(students);
                        }
                    });
            List<SchoolGradeRecord> sortedRecords = recordsFilterable.extractSorted(Arrays.asList(
                    SchoolGradeRecord.Field.boardNumber.toString(),
                    SchoolGradeRecord.Field.schoolNumber.toString(),
                    SchoolGradeRecord.Field.gradeNumeric.toString()), true);

            if (sortedRecords != null && !sortedRecords.isEmpty()) {
                for (SchoolGradeRecord record : sortedRecords) {
                    grid.append();
                    for (SchoolGradeRecord.Field field : record.m_fieldValuePairs.keySet()) {
                        grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                    }
                }
            }
            grid.beforeTop();
            if (getErrorsLog().length() > 0) {
                addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
            }
        }
        addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
        addParameter(REPORT_PARAM_AS_OF_DATE, s_asOfDateFormat.format(getReportDate()));
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

        m_dateFormat = new SimpleDateFormat(DATE_FORMAT, getUserLocale());
        m_dictExtractor = new DictionaryExtractor(getBroker());
        initGradesMap();
    }

    /**
     *
     * @param fteRecords
     * @param sklToLookForFTE
     * @return
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
    private PlainDate getArrivalDate(ReportStudent student) {
        return student.getArrivalDateCanada();
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
     * @return Filterable
     */
    private Filterable<SchoolGradeRecord> getRecords(Filterable<StudentRecord> students) {
        List<SchoolGradeRecord> records = new ArrayList<SchoolGradeRecord>();
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
            List<String> grades = students.extract(StudentRecord.Field.grade.toString());
            for (Object gradeObj : grades) {
                if (gradeObj != null) {
                    SchoolGradeRecord record = new SchoolGradeRecord();
                    records.add(record);
                    record.set(SchoolGradeRecord.Field.academicYears, academicYears);
                    record.set(SchoolGradeRecord.Field.boardName, boardName);
                    record.set(SchoolGradeRecord.Field.boardNumber, boardNumber);
                    record.set(SchoolGradeRecord.Field.currentDate, currentDate);
                    record.set(SchoolGradeRecord.Field.programName, programName);
                    record.set(SchoolGradeRecord.Field.schoolName, schoolName);
                    record.set(SchoolGradeRecord.Field.schoolNumber, schoolNumber);
                    record.set(SchoolGradeRecord.Field.printDetail, "Y");
                    String grade = (String) gradeObj;
                    record.set(SchoolGradeRecord.Field.grade, m_gradesMap.get(grade).getStateCode());
                    String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                            ALIAS_DDX_NUM_GRADE, m_dictExtractor.getDictionary(DDX_ID_GRADES));
                    if (!StringUtils.isEmpty(numGradeLevel)) {
                        record.set(SchoolGradeRecord.Field.gradeNumeric, new Integer(numGradeLevel));
                    }
                    record.set(SchoolGradeRecord.Field.gradeToDisplay, m_gradesMap.get(grade).getDescription());

                    for (int rangeIndex = 0; rangeIndex < m_ranges.size(); rangeIndex++) {
                        Range range = m_ranges.get(rangeIndex);
                        int rangeFieldNumber = rangeIndex + 1;

                        Filterable<StudentRecord> rangeRecords = students.filter(new Filter<StudentRecord>() {
                            @Override
                            public boolean isFiltered(StudentRecord toFilter) {
                                PlainDate arrivalDate =
                                        (PlainDate) toFilter.get(StudentRecord.Field.arrivalDate);
                                return ToolsSharedContainer.Range.of(arrivalDate, arrivalDate)
                                        .isOverlap(ToolsSharedContainer.Range.of(range.m_startDate, range.m_endDate));
                            }

                        });
                        SchoolGradeRecord.Field rangeField =
                                SchoolGradeRecord.Field.valueOf("range" + rangeFieldNumber);
                        SchoolGradeRecord.Field rangeGradeTotalField =
                                SchoolGradeRecord.Field.valueOf(rangeField.toString() + "GradeTotal");
                        record.set(rangeField, range.toString(m_dateFormat));
                        record.set(rangeGradeTotalField, rangeRecords.count(
                                StudentRecord.Field.grade.toString(),
                                grade));
                    }
                }
            }
            if (records.isEmpty()) {
                SchoolGradeRecord record = new SchoolGradeRecord();
                record.set(SchoolGradeRecord.Field.academicYears, academicYears);
                record.set(SchoolGradeRecord.Field.boardName, boardName);
                record.set(SchoolGradeRecord.Field.boardNumber, boardNumber);
                record.set(SchoolGradeRecord.Field.currentDate, currentDate);
                record.set(SchoolGradeRecord.Field.programName, programName);
                record.set(SchoolGradeRecord.Field.schoolName, schoolName);
                record.set(SchoolGradeRecord.Field.schoolNumber, schoolNumber);
                record.set(SchoolGradeRecord.Field.printDetail, "N");
                records.add(record);
            }
        } catch (Exception e) {
            if (records.isEmpty()) {
                records.add(new SchoolGradeRecord());
            }
            logError(e.getMessage());
            e.printStackTrace();
        }
        return FilterableFactory.create(records, SchoolGradeRecord.s_uniqueFields, SchoolGradeRecord.s_valueResolver);
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
     * Gets the students.
     *
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<StudentRecord> getStudentRecords(OnSchool school, Collection<String> sklOidsToReport) {
        Map<String, ToolEnrollment> recentEnrByStdOid = new HashMap<>();
        Collection<ReportStudent> students = getStudents(m_spanCriteria, false, false, m_annualSpanFilter);
        final List<String> reportableCodes = getReportableResidenceCodes();
        Collection<ReportStudent> studentsToReport = FilterableFactory
                .create(students, Arrays.asList(ToolBean.FIELD_OID.resolve(null)), PredefinedResolver.RPT_BEAN)
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
                            recentEnrByStdOid.put(toFilter.getOid(), recentEnrolment);
                        }
                        return !StringUtils.isEmpty(boardResidenceCode)
                                && reportableCodes.contains(boardResidenceCode)
                                && isCanadaEntryInLast4Years(toFilter)
                                && !COUNTRY_STATE_CODES_NOT_INCLUDED
                                        .contains(getBirthCountry(toFilter, true));
                    }
                }).extract();
        preloadStudentsData();
        List<StudentRecord> studentRecords = studentsToReport.stream().map(student -> {
            StudentRecord record = new StudentRecord();
            record.set(StudentRecord.Field.oen, student.getOenRaw());
            record.set(StudentRecord.Field.studentName, getStudentNameLastFirstMiddle(student));
            record.set(StudentRecord.Field.gender, student.getGenderType());
            record.set(StudentRecord.Field.is21plus, is21plus(student));
            record.set(StudentRecord.Field.arrivalDate, getArrivalDate(student));
            ToolEnrollment recentEnrolment = recentEnrByStdOid.get(student.getOid());
            boolean isCounted = false;
            if (recentEnrolment != null) {
                record.set(StudentRecord.Field.isFullyHighCredit, isFullyHighCredit(recentEnrolment));
                record.set(StudentRecord.Field.birthCountry, getBirthCountry(student, false));
                int yog = getYog(student, (OnEnrollment) recentEnrolment);
                String grade = m_gradesHelper.getGradeLevel(yog);
                record.set(StudentRecord.Field.grade, grade);
                isCounted = isCounted(record, student, recentEnrolment);
            }
            return isCounted ? record : new StudentRecord();
        }).collect(Collectors.toList());
        return FilterableFactory.create(studentRecords, StudentRecord.s_uniqueFields, StudentRecord.s_valueResolver);
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
        m_gradesHelper = new GradesHelper(m_sklDateRangeProvider);
        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true)
                .setIncludeAllSecondarySpans(true);

        m_ranges = getRanges();
        if (!m_ranges.isEmpty()) {
            m_september1back4years = m_ranges.get(m_ranges.size() - 1).m_startDate;
            m_range1EndDate = m_ranges.get(0).m_endDate;
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
     * @param sklOidsToReport Collection<String>
     * @return true, if is counted
     */
    private boolean isCounted(StudentRecord record,
                              ReportStudent student,
                              ToolEnrollment recentEnrolment) {
        Date arrivalDate = student.getArrivalDateCanada();
        boolean isCountedBYCountryAndLang = true;
        if (Arrays.asList(ORG_CODE_ONSIS_LANG_ENG, ORG_CODE_ONSIS_LANG_FR)
                .contains(getOrganizationToolBean().getLanguageStateValue())) {
            isCountedBYCountryAndLang = !COUNTRY_STATE_CODES_NOT_COUNTED_BOTH.contains(getBirthCountry(student, true))
                    && !COUNTRY_STATE_CODES_NOT_COUNTED_FR.contains(getBirthCountry(student, true));
        }
        String boardResStatus = ((OnEnrollment) recentEnrolment).getBoardResidentStatus();
        return s_boardResidenceCounted.contains(boardResStatus)
                && isCanadaEntryInLast4Years(student) && !((Boolean) record.get(StudentRecord.Field.is21plus))
                && !((Boolean) record.get(StudentRecord.Field.isFullyHighCredit))
                && arrivalDate != null && !arrivalDate.after(m_range1EndDate)
                && isCountedBYCountryAndLang;
    }

    /**
     * Checks if is fully high credit.
     *
     * @param stdEnr StudentEnrollment
     * @return true, if is fully high credit
     */
    private boolean isFullyHighCredit(ToolEnrollment stdEnr) {
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
                            + getStudentNameLastFirstMiddle(std)
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
            if (BOARD_RES_STATUS_INDEPENDENT.equals(((OnEnrollment) stdEnr).getBoardResidentStatus())) {
                return true;
            }
        }
        return false;
    }

    /**
     *
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
