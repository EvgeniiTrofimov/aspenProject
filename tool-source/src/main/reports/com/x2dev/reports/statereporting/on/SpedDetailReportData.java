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
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentProgramParticipation;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.AnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnAnnualSpan;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnEnrollment;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudent;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSped;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnStudentSpedDetail;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class SpedDetailReportData.
 */
public class SpedDetailReportData extends OnReportJavaSourceNew {
    /**
     * The Class Record.
     */
    private static class Record implements RecordInterface {
        private Map<FieldInterface, Object> m_fieldValuePairs = new HashMap<FieldInterface, Object>();

        /**
         * The Enum Field.
         */
        private enum Field implements FieldInterface {
            academicYears,
            //
            age,
            //
            area,
            //
            arrivalStatus,
            //
            boardName,
            //
            boardNumber,
            //
            boardResStatus,
            //
            currentDate,
            //
            errorsLog,
            //
            exceptionality,
            //
            grade,
            //
            is21plus,
            //
            isCounted,
            //
            isELearning,
            //
            isIep,
            //
            isMainExceptionality,
            //
            isSharedStudent,
            //
            oen,
            //
            programName,
            //
            schoolName,
            //
            schoolNumber,
            //
            setting,
            //
            settingToOrder,
            //
            studentName,
            //
            totalCountedStudents,
            //
            totalExceptionality,
            //
            totalSetting,
            //
            totalStudents
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.boardNumber.toString(),
                        Field.schoolNumber.toString(),
                        Field.area.toString(),
                        Field.exceptionality.toString(),
                        Field.setting.toString(),
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

        DistrictSchoolYearContext m_ctxByDate;

        /**
         * Gets the broker.
         *
         * @return X 2 broker
         * @see com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.OnsisBrokerProvider#getBroker()
         */
        @Override
        public X2Broker getBroker() {
            return SpedDetailReportData.this.getBroker();
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
         * Gets the school year by date.
         *
         * @param date PlainDate
         * @return Integer
         */
        public Integer getYear(PlainDate date) {
            return getContextByDate(date).getSchoolYear();
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
     * Area (parent descriptor) table keyed by Exceptionality State Code.
     */
    private static final Map<String, String> AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE;
    static {
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE = new HashMap<String, String>();
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("1", "BE"); // Behavioural
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("2", "CE"); // Autism
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("3", "CE"); // Deaf and Hard of Hearing
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("5", "CE"); // Learning Disability
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("6", "CE"); // Speech Impairment
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("7", "CE"); // Language Impairment
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("8", "IE"); // Giftedness
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("9", "IE"); // Mild Intellectual Disability
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("10", "IE"); // Developmental Disability
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("11", "PD"); // Physical Disability
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("12", "PD"); // Blind and Low Vision
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("14", "ME"); // Multiple Exceptionalities
        AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.put("15", "CE"); // Deaf/Hard of Hearing PRESCHOOL
    }

    private static final long serialVersionUID = 1L;

    /**
     * Constants
     */
    private static final String NO_EXCEPTIONALITY_KEY = "rpt.No.Exceptionality";
    private static final String REPORT_ID_CSV = "ON-VRF-SPED-DETAIL-CSV";
    private static final String REPORT_ID_PDF_ELEM = "ON-VRF-SPED-ELEM-DETAIL";
    private static final String REPORT_ID_PDF_SEC = "ON-VRF-SPED-SEC-DETAIL";
    private static final String VALUE_NOT_SELECTED_KEY = "rpt.Not.Selected";

    /**
     * Class members
     */
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private GradesHelper m_gradesHelper;
    private SchoolDateRangeProvider m_sklDateRangeProvider;
    private EnrollmentSpanCriteria m_spanCriteria;
    private Map<String, ToolStudentSchool> m_sskByStdOid = new HashMap<>();

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
            Collection<OnSchool> skls = filterableSkls.extract();
            Collection<String> sklOidsToReport = skls.stream().map(skl -> skl.getOid()).collect(Collectors.toList());
            for (OnSchool skl : skls) {
                initializeHelpersForSchool(skl);
                Filterable<ReportStudent> students = FilterableFactory
                        .createFilterableToolBeans(getStudents(m_spanCriteria, true, true, m_annualSpanFilter));
                preloadStudentsData();
                Filterable<Record> recordsFilterable = getRecords(students, sklOidsToReport);
                if (recordsFilterable != null) {
                    List<Record> sortedRecords = recordsFilterable.extractSorted(Arrays.asList(
                            Record.Field.boardNumber.toString(),
                            Record.Field.schoolNumber.toString(),
                            Record.Field.area.toString(),
                            Record.Field.exceptionality.toString(),
                            Record.Field.settingToOrder.toString(),
                            Record.Field.studentName.toString(),
                            Record.Field.oen.toString()), true);
                    for (Record record : sortedRecords) {
                        assignTotals(record, recordsFilterable);
                        grid.append();
                        for (FieldInterface field : record.m_fieldValuePairs.keySet()) {
                            grid.set(field.toString(), record.m_fieldValuePairs.get(field));
                        }
                    }
                    if (getErrorsLog().length() > 0) {
                        addParameter(Record.Field.errorsLog.toString(), getErrorsLog().toString());
                    }
                }
            }
        }
        if (grid.isEmpty()) {
            grid.append();
        }
        if (getErrorsLog().length() > 0) {
            addParameter(PARAMETER_NAME_ERRORS_LOG, getErrorsLog().toString());
        }
        grid.beforeTop();
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
        int reportFormat = getJob().getInput().getFormat();
        String pdfReportID = isElementaryReport() ? REPORT_ID_PDF_ELEM : REPORT_ID_PDF_SEC;
        switch (reportFormat) {
            case ToolInput.CSV_FORMAT:
                this.setFormatId(REPORT_ID_CSV);
                break;
            case ToolInput.HTML_FORMAT:
                this.setFormatId(pdfReportID);
                break;
            case ToolInput.PDF_FORMAT:
                this.setFormatId(pdfReportID);
                break;
            case ToolInput.XLS_FORMAT:
                this.setFormatId(REPORT_ID_CSV);
                break;
        }
        ToolBean.registerClass(OnSchool.class);
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
            int totalCountedStudents = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber,
                    Record.Field.area), record, filterable)
                            .filter(Record.Field.isCounted.toString(), Boolean.TRUE)
                            .count();
            record.set(Record.Field.totalCountedStudents, totalCountedStudents);
        }

        {
            int totalExceptionality = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber,
                    Record.Field.area,
                    Record.Field.exceptionality), record, filterable).count();
            record.set(Record.Field.totalExceptionality, totalExceptionality);
        }

        {
            int totalSetting = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber,
                    Record.Field.area,
                    Record.Field.exceptionality,
                    Record.Field.setting), record, filterable).count();
            record.set(Record.Field.totalSetting, totalSetting);
        }

        {
            int totalStudents = filterByFields(Arrays.asList(
                    Record.Field.boardNumber,
                    Record.Field.schoolNumber), record, filterable).count();
            record.set(Record.Field.totalStudents, totalStudents);
        }

        return record;
    }

    /**
     * Gets the area by exceptionality state code.
     *
     * @param stateCode String
     * @return String
     */
    private String getAreaByExceptionalityStateCode(String stateCode) {
        return !StringUtils.isEmpty(stateCode) && AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.containsKey(stateCode)
                ? AREA_BY_MAP_EXCEPTIONALITY_STATE_CODE.get(stateCode)
                : "No Area";
    }

    /**
     * Gets the sped iep flag.
     *
     * @param spedPgm OnStudentSped
     * @return Boolean
     */
    private Boolean getSpedIepFlag(OnStudentSped spedPgm) {
        OnStudentSpedDetail detail = spedPgm.getProgramDetails(getBroker()).stream()
                .filter(pgd -> OnStudentSpedDetail.TYPE_PLACEMENT.equals(pgd.getType())
                        && pgd.getStartDate() != null
                        && DateUtils.isBetween(pgd.getStartDate(),
                                m_sklDateRangeProvider.getContextByDate(getReportDate()).getStartDate(),
                                m_sklDateRangeProvider.getEndDate())
                        && (pgd.getEndDate() == null || DateUtils.isBetween(pgd.getEndDate(),
                                m_sklDateRangeProvider.getContextByDate(getReportDate()).getStartDate(),
                                m_sklDateRangeProvider.getEndDate())))
                .collect(Collectors.maxBy(new Comparator<OnStudentSpedDetail>() {
                    // Latest Start Date
                    @Override
                    public int compare(OnStudentSpedDetail pgd1, OnStudentSpedDetail pgd2) {
                        return pgd1.getStartDate().compareTo(pgd2.getStartDate());
                    }
                })).orElse(null);
        return detail == null ? null : detail.getIepRequiredIndicator();
    }

    /**
     * Gets the placement type.
     *
     * @param spedPgm OnStudentSped
     * @return String
     */
    private String getSpedStatePlacementType(OnStudentSped spedPgm) {
        OnStudentSpedDetail detail = spedPgm.getProgramDetails(getBroker()).stream()
                .filter(pgd -> OnStudentSpedDetail.TYPE_PLACEMENT.equals(pgd.getType())
                        && pgd.getStartDate() != null
                        && DateUtils.isBetween(pgd.getStartDate(),
                                m_sklDateRangeProvider.getContextByDate(getReportDate()).getStartDate(),
                                m_sklDateRangeProvider.getEndDate())
                        && (pgd.getEndDate() == null || DateUtils.isBetween(pgd.getEndDate(),
                                m_sklDateRangeProvider.getContextByDate(getReportDate()).getStartDate(),
                                m_sklDateRangeProvider.getEndDate())))
                .collect(Collectors.maxBy(new Comparator<OnStudentSpedDetail>() {
                    // Latest Start Date
                    @Override
                    public int compare(OnStudentSpedDetail pgd1, OnStudentSpedDetail pgd2) {
                        return pgd1.getStartDate().compareTo(pgd2.getStartDate());
                    }
                })).orElse(null);
        return detail == null ? null : detail.getPlacementTypeState();
    }

    /**
     * Gets the records.
     *
     * @param students Filterable<ReportStudent>
     * @param sklOidsToReport Collection<String>
     * @return Filterable
     * @throws X2BaseException exception
     */
    private Filterable<Record> getRecords(Filterable<ReportStudent> students, Collection<String> sklOidsToReport)
            throws X2BaseException {
        List<Record> records = new ArrayList<Record>();
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
        Map<String, ReferenceCode> pgmExceptionalityMap = getDictExtractor().getReferenceCodes(
                OnStudentSped.FIELD_EXCEPTIONALITY.getField(getDictExtractor()).getReferenceTableOid());
        Map<String, ReferenceCode> pgmSettingMapByState = new HashMap<>();
        try {
            pgmSettingMapByState.putAll(getDictExtractor()
                    .getReferenceCodes(OnStudentSpedDetail.FIELD_SPECED_PLACEMENT_TYPE.getField(getDictExtractor())
                            .getReferenceTableOid())
                    .values()
                    .stream()
                    .filter(rcd -> !StringUtils.isBlank(rcd.getStateCode()))
                    .distinct()
                    .collect(Collectors.toMap(ReferenceCode::getStateCode, code -> code)));

        } catch (Exception e) {
            logError("OnSIS Code duplicated (invalid) in Reference Table: "
                    + OnStudentSpedDetail.FIELD_SPECED_PLACEMENT_TYPE.getField(getDictExtractor()).getReferenceTable()
                            .getUserName());
            return null;
        }
        for (ReportStudent student : students.extract()) {
            List<OnStudentSped> spedPgms = student.getSpedPrograms(getBroker()).stream()
                    .filter(pgm -> !getReportDate().before(pgm.getStartDate()))
                    .collect(Collectors.toList());
            String stdOEN = student.getOenRaw();
            if (spedPgms != null && !spedPgms.isEmpty()) {
                for (OnStudentSped spedPgm : spedPgms) {
                    String exceptCode = spedPgm.getExceptionalityPlain();
                    String exceptToDisplay = null;
                    String exceptStateCode = null;
                    if (!StringUtils.isEmpty(exceptCode) && pgmExceptionalityMap.containsKey(exceptCode)) {
                        exceptStateCode = pgmExceptionalityMap.get(exceptCode).getStateCode();
                    } else {
                        logError("Exceptionality is not set for Student with OEN = " + stdOEN);
                    }
                    if (StringUtils.isEmpty(exceptStateCode) && !StringUtils.isEmpty(exceptCode)) {
                        exceptToDisplay =
                                getUserMessageResources().getMessage(getUserMessageKeyPrefix() + NO_EXCEPTIONALITY_KEY);
                    }
                    if (pgmExceptionalityMap.containsKey(exceptCode)) {
                        exceptToDisplay = pgmExceptionalityMap.get(exceptCode).getDescription();
                    }
                    Record record = new Record();
                    record.set(Record.Field.exceptionality, exceptToDisplay);
                    String area = getAreaByExceptionalityStateCode(exceptStateCode);
                    if (getUserMessageResources().getMessage(getUserMessageKeyPrefix() + NO_EXCEPTIONALITY_KEY)
                            .equals(exceptToDisplay)) {
                        area = "0";
                        exceptToDisplay = " " + exceptToDisplay;
                    }
                    record.set(Record.Field.area, area);
                    record.set(Record.Field.isIep, getSpedIepFlag(spedPgm));
                    Boolean mainExpectionality = getMainExceptFlag(spedPgm, student);
                    record.set(Record.Field.isMainExceptionality, mainExpectionality);
                    String settingState = getSpedStatePlacementType(spedPgm);
                    String setting =
                            !StringUtils.isEmpty(settingState) && pgmSettingMapByState.containsKey(settingState)
                                    ? pgmSettingMapByState.get(settingState).getDescription()
                                    : getUserMessageResources()
                                            .getMessage(getUserMessageKeyPrefix() + VALUE_NOT_SELECTED_KEY);
                    String settingToOrder = "";
                    if (!StringUtils.isEmpty(settingState)) {
                        settingToOrder = getSettingSequenceByState(settingState);
                    }
                    record.set(Record.Field.setting, setting);
                    record.set(Record.Field.settingToOrder, settingToOrder);
                    record.set(Record.Field.schoolName, schoolName);
                    record.set(Record.Field.schoolNumber, schoolNumber);
                    record.set(Record.Field.boardNumber, boardNumber);
                    record.set(Record.Field.boardName, boardName);
                    record.set(Record.Field.academicYears, academicYears);
                    record.set(Record.Field.programName, programName);
                    record.set(Record.Field.currentDate, currentDate);
                    record.set(Record.Field.oen, stdOEN);
                    record.set(Record.Field.studentName, getStudentNameLastFirstMiddle(student));
                    record.set(Record.Field.is21plus, is21plus(student));
                    OnEnrollment recentEnrolment = null;
                    List<AnnualSpan> annualSpans = student.getEnrollmentSpans(getBroker(), false, false).stream()
                            .filter(annualSpan -> annualSpan.getSchool().getOid()
                                    .equals(m_sklDateRangeProvider.getSchool().getOid())
                                    || annualSpan.isSecondary())
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
                                    && firstAnnualSpan
                                            .getBestPrimarySpanFor(getBroker()) != null) {
                                recentEnrolment = (OnEnrollment) firstAnnualSpan.getBestPrimarySpanFor(getBroker())
                                        .getRecentEnrollmentESY();
                                m_sskByStdOid.put(student.getOid(),
                                        firstAnnualSpan.getSecondary());
                            }
                        }
                    }
                    boolean isSharedStudent = false;
                    boolean isELearning = false;
                    boolean isCounted = false;
                    if (recentEnrolment != null) {
                        String boardResidenceCode = recentEnrolment.getBoardResidentStatusPlain();
                        boolean isSecondaryRecord = false;
                        if (m_sskByStdOid.containsKey(student.getOid()) && m_sskByStdOid.get(student.getOid())
                                .getSchool(getBroker()).getOid().equals(m_sklDateRangeProvider.getSchool().getOid())) {
                            isSecondaryRecord = true;
                        }
                        isSharedStudent = BOARD_RES_STATUS_SHARED.equals(boardResidenceCode);
                        isELearning = BOARD_RES_STATUS_ELEARNING.equals(boardResidenceCode);
                        isCounted = !isSecondaryRecord && (mainExpectionality.booleanValue()
                                || (!StringUtils.isEmpty(exceptToDisplay) && exceptToDisplay.startsWith(" ")));
                        String resStatusDescr = getDescription(recentEnrolment, OnEnrollment.FIELD_BRD_RES_STAT_TYPE);
                        if ("Pupil of the Board".equalsIgnoreCase(resStatusDescr)) {
                            resStatusDescr = "0" + resStatusDescr;
                        }
                        record.set(Record.Field.boardResStatus, resStatusDescr);
                        record.set(Record.Field.arrivalStatus, recentEnrolment.getArrivalStatus());
                    }
                    record.set(Record.Field.isSharedStudent, isSharedStudent);
                    record.set(Record.Field.isELearning, isELearning);
                    record.set(Record.Field.isCounted, isCounted);
                    int yog = getYog(student, recentEnrolment);
                    String grade = m_gradesHelper.getGradeLevel(yog);
                    record.set(Record.Field.grade, grade);
                    record.set(Record.Field.age, String.valueOf(student.getAgeAsOfDate(getReportDate())));
                    records.add(record);
                }
            }
        }
        return FilterableFactory.create(records, Record.s_uniqueFields, Record.s_valueResolver);
    }

    /**
     * Gets the setting sequence by state.
     *
     * @param settingStateCode String
     * @return String
     */
    private String getSettingSequenceByState(String settingStateCode) {
        String sequence = "";
        switch (settingStateCode) {
            case "F":
                sequence = "1";
                break;
            case "P":
                sequence = "2";
                break;
            case "W":
                sequence = "3";
                break;
            case "R":
                sequence = "4";
                break;
            case "I":
                sequence = "5";
                break;

            default:
                break;
        }
        return sequence;
    }

    /**
     * Initialize helpers.
     *
     * @param school OnSchool
     */
    private void initializeHelpersForSchool(OnSchool school) {
        m_sklDateRangeProvider = new SchoolDateRangeProvider();
        m_sklDateRangeProvider.setSchool(school);
        m_gradesHelper = new GradesHelper(m_sklDateRangeProvider);
        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true);
    }

    /**
     * Preload students data.
     */
    private void preloadStudentsData() {
        ToolBean.clearAllCachedToolBeans(OnStudentSped.class);
        ToolBean.resetCriteria(getBroker(), OnStudentSped.class);

        // preload special ed
        X2Criteria spedCriteria = ToolStudentProgramParticipation.getDateRangeCriteria(getBroker(),
                m_sklDateRangeProvider.getContextByDate(getReportDate()).getStartDate(),
                m_sklDateRangeProvider.getEndDate());
        spedCriteria.addEqualTo(OnStudentSped.FIELD_SPED_REPORT_INDICATOR.resolve(getDictExtractor()), Boolean.TRUE);
        ToolBean.addAndCriteria(getBroker(), OnStudentSped.class, spedCriteria);
        ToolBean.preload(getBroker(), getDictExtractor(),
                Arrays.asList(ToolStudentProgramParticipation.FIELD_START_DATE, ToolBean.FIELD_OID),
                OnStudent.CHILD_SPED_PROGRAMS);
    }



}
