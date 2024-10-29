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
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable.Mapper;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolDistrictContext;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolOrganization;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentProgramParticipation;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteMonthly;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.FteRecord;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSalep;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class EnrByAttendanceTypeElemDetailReportData.
 */
public class EnrByAttendanceTypeElemDetailReportData extends OnReportJavaSourceNew {

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
            finalFteRegular,
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
            return EnrByAttendanceTypeElemDetailReportData.this.getBroker();
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
     * The Enum EnrolmentRegister.
     */
    private enum EnrolmentRegister {
        FT, HT, LT, PT;
    }

    private static final long serialVersionUID = 1L;
    private final List<String> s_isCounted = Arrays.asList(BOARD_RES_STATUS_PUPIL_OF_THE_BOARD);

    /**
     * Constants
     */
    private static final String ATT_TYPE_NOT_SET = "Set enr.[all-enr-EnrolmentRegister]";
    private static final String ZERO = "0.00";
    private static final String REPORT_ID_CSV = "ON-VRF-EBA-ELEM-DET-CSV";
    private static final String REPORT_ID_PDF = "ON-VRF-EBA-ELEM-DETAIL";

    /**
     * Class members
     */
    private DictionaryExtractor m_dictExtractor;
    private Map<String, ReferenceCode> m_gradesMap = new HashMap<>();
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private SchoolDateRangeProvider m_sklDateRangeProvider;
    private EnrollmentSpanCriteria m_spanCriteria;

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
            Filterable<Record> fteRecordsFilterable = filterableSkls
                    .map(new Mapper<OnSchool, Record>() {
                        @Override
                        public Filterable<Record> map(OnSchool school) {
                            initializeHelpersForSchool(school);
                            Filterable<ReportStudent> students = FilterableFactory.createFilterableToolBeans(
                                    getStudents(m_spanCriteria, false, false, m_annualSpanFilter));
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
            grid.beforeTop();
            if (getErrorsLog().length() > 0) {
                addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
            }
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

        ToolBean.registerClass(FteMonthly.class);
        ToolBean.registerClass(FteRecord.class);
        ToolBean.registerClass(OnStudentSalep.class);

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
                        .append("Total Students by " + currentBoardResidenceStatus + ": " + currentRecords.size());
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
     * Format string number.
     *
     * @param number String
     * @return String
     */
    private String formatStringNumber(BigDecimal number) {
        return number.setScale(2, BigDecimal.ROUND_HALF_UP).toString();
    }

    /**
     * Gets the records.
     *
     * @param school SisSchool
     * @param students Filterable<OnStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     */
    private Filterable<Record> getRecords(final OnSchool school,
                                          Filterable<ReportStudent> students,
                                          Collection<String> sklOidsToReport) {
        String schoolName = school.getName();
        String schoolNumber = school.getBsid();
        ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
        String boardNumber = board == null ? "Cannot determine board by school number" : board.getCode();
        String boardName = board == null ? "Cannot determine board by school number" : board.getDescription();
        String academicYears = getCurrentContext().getContextId();
        String programName = "Aspen";
        String currentDate = m_formatter
                .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
        List<Record> fteRecords = new ArrayList<Record>();
        List<OnEnrollment> enrollments = new ArrayList<>();
        for (ReportStudent student : students.extract()) {
            try {
                Record fteRecord = new Record();
                fteRecord.set(Record.Field.schoolName, schoolName);
                fteRecord.set(Record.Field.schoolNumber, schoolNumber);
                fteRecord.set(Record.Field.boardNumber, boardNumber);
                fteRecord.set(Record.Field.boardName, boardName);
                fteRecord.set(Record.Field.academicYears, academicYears);
                fteRecord.set(Record.Field.programName, programName);
                fteRecord.set(Record.Field.currentDate, currentDate);
                fteRecord.set(Record.Field.gender, student.getGenderType());
                fteRecord.set(Record.Field.is21plus, is21plus(student));
                fteRecord.set(Record.Field.isSal, isSalep(student));
                fteRecord.set(Record.Field.age, String.valueOf(student.getAgeAsOfDate(getReportDate())));
                OnEnrollment recentEnrolment = null;
                List<AnnualSpan> annualSpans = student.getEnrollmentSpans(getBroker(), true, true).stream()
                        .filter(annualSpan -> annualSpan.getContext().getOid()
                                .equals(m_sklDateRangeProvider.getContextByDate(getReportDate()).getOid()))
                        .collect(Collectors.toList());
                if (annualSpans != null && !annualSpans.isEmpty()) {
                    OnAnnualSpan firstAnnualSpan =
                            (OnAnnualSpan) ToolsSharedContainer.reverse(annualSpans.stream()).findFirst().get();
                    if (firstAnnualSpan != null) {
                        recentEnrolment = (OnEnrollment) firstAnnualSpan.getRecentEnrollmentESY();
                        if (recentEnrolment == null && firstAnnualSpan.isSecondary()
                                && sklOidsToReport
                                        .contains(firstAnnualSpan.getSecondary().getSchool(getBroker()).getOid())
                                && firstAnnualSpan.getBestPrimarySpanFor(getBroker(), null) != null) {
                            recentEnrolment = (OnEnrollment) firstAnnualSpan.getBestPrimarySpanFor(getBroker(), null)
                                    .getRecentEnrollmentESY();
                        }
                    }
                }
                if (recentEnrolment != null) {
                    enrollments.add(recentEnrolment);
                    boolean isCounted = isCounted(recentEnrolment.getBoardResidentStatus());
                    fteRecord.set(Record.Field.counted, isCounted);
                    fteRecord.set(Record.Field.oen, student.getOenRaw());
                    fteRecord.set(Record.Field.studentName, getStudentNameFirstMiddleLast(student));
                    fteRecord.set(Record.Field.studentNameToSort, getStudentNameLastFirstMiddle(student));
                    int yog = getYog(student, recentEnrolment);
                    fteRecord.set(Record.Field.yog, yog);
                    String grade = student.getGradeLevel();
                    if (!StringUtils.isEmpty(grade) && m_gradesMap.containsKey(grade)) {
                        fteRecord.set(Record.Field.grade, m_gradesMap.get(grade).getStateCode());
                        String numGradeLevel = (String) m_gradesMap.get(grade).getFieldValueByAliasExtended(
                                ALIAS_DDX_NUM_GRADE, m_dictExtractor.getDictionary(DDX_ID_GRADES));
                        if (!StringUtils.isEmpty(numGradeLevel)) {
                            fteRecord.set(Record.Field.gradeToSort, new Integer(numGradeLevel));
                        } else {
                            fteRecord.set(Record.Field.gradeToSort, new Integer(-99));
                        }
                    }
                    String resStatus = getDescription(recentEnrolment, OnEnrollment.FIELD_BRD_RES_STAT_TYPE);
                    if ("Pupil of the Board".equalsIgnoreCase(resStatus)) {
                        resStatus = "0" + resStatus;
                    }
                    fteRecord.set(Record.Field.boardResidenceStatus, resStatus);
                    String attendanceType = recentEnrolment.getRegister();
                    fteRecord.set(Record.Field.attendanceType,
                            !StringUtils.isEmpty(attendanceType) ? attendanceType : ATT_TYPE_NOT_SET);

                    BigDecimal fte = recentEnrolment.getFte();
                    fte = fte == null ? BigDecimal.ZERO : fte;
                    BigDecimal finalFte = BigDecimal.ZERO;
                    if (EnrolmentRegister.FT.toString().equals(attendanceType)) {
                        finalFte = FTE_FT_FINAL_THRESHOLD.compareTo(fte) > 0 ? fte : BigDecimal.ONE;
                    }
                    fteRecord.set(Record.Field.fteAsReportedRegular, fte == null ? ZERO : formatStringNumber(fte));
                    fteRecord.set(Record.Field.finalFteRegular, finalFte == null ? BigDecimal.ZERO
                            : finalFte.setScale(2, BigDecimal.ROUND_HALF_UP));
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
                m_dictExtractor.getDataDictionaryField(OnStudent.class, SisStudent.COL_GRADE_LEVEL);
        String referenceTableOid = null;
        if (dictionaryField != null && dictionaryField.hasReferenceTable()) {
            referenceTableOid = dictionaryField.getDataFieldConfig().getReferenceTableOid();
            X2Criteria refTablecriteria = new X2Criteria();
            refTablecriteria.addEqualTo(X2BaseBean.COL_OID, referenceTableOid);
            ReferenceTable gradeRefTable =
                    getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, refTablecriteria));
            if (gradeRefTable != null) {
                m_gradesMap = gradeRefTable.getCodeMap();
            }
        }
    }

    /**
     * Initialize helpers.
     */
    private void initializeHelpersForSchool(OnSchool school) {
        m_sklDateRangeProvider = new SchoolDateRangeProvider(school);

        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true);
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
     * @param schoolOid String
     * @param studentOid String
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
