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
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.CriteriaHelper.EnrollmentSpanCriteria;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
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
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.OnSchoolDateRangeProvider;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
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
 * The Class SpedSummaryReportData.
 */
public class SpedSummaryReportData extends OnReportJavaSourceNew {
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
            boardName,
            //
            boardNumber,
            //
            currentDate,
            //
            errorsLog,
            //
            area,
            //
            exceptionalityCode,
            //
            schoolName,
            //
            schoolNumber,
            //
            iepEC,
            //
            iepNI,
            //
            fullySelfContainedEC,
            //
            fullySelfContainedNI,
            //
            partiallyIntegratedEC,
            //
            partiallyIntegratedNI,
            //
            withdrawalAssistanceEC,
            //
            withdrawalAssistanceNI,
            //
            resourceAssistanceEC,
            //
            resourceAssistanceNI,
            //
            indirectServiceEC,
            //
            indirectServiceNI
        }

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
            return SpedSummaryReportData.this.getBroker();
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

    private static final long serialVersionUID = 1L;

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

    public static final String EXCEPT_CODE_NOT_INDENT = "No Exceptionality";
    public static final String EXCEPT_CODE_SUB_TOTAL = "SUB TOTAL";
    public static final String EXCEPT_CODE_GRAND_TOTAL = "GRAND TOTAL";
    public static final String RF_PAGE_RESET = "pageResetKey";
    private static final String VALUE_EXCEPTIONALITY_EXCLUDED_CODE_SECONDARY = "15";

    /**
     * Class members
     */
    private SimpleDateFormat m_formatter = new SimpleDateFormat("yyyy/MM/dd hh:mm:ss");
    private Map<String, ReferenceCode> m_pgmExceptionalityByCodeMap = new HashMap<>();
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
            grid.append();
            grid.beforeTop();
            if (getErrorsLog().length() > 0) {
                addParameter(Record.Field.errorsLog.toString(), getErrorsLog().toString());
            }

            addParameter(REPORT_PARAM_VERSION, getJob().getTool().getComment());
            addParameter(REPORT_PARAM_AS_OF_DATE, s_asOfDateFormat.format(getReportDate()));
            return grid;
        }
        Filterable<OnSchool> filterableSkls = getSchoolsFromInput();
        if (filterableSkls != null) {
            Collection<OnSchool> skls = filterableSkls.extract();
            Collection<String> sklOidsToReport = skls.stream().map(skl -> skl.getOid()).collect(Collectors.toList());
            for (OnSchool skl : skls) {
                initializeHelpersForSchool(skl);
                Filterable<ReportStudent> students = FilterableFactory
                        .createFilterableToolBeans(getStudents(m_spanCriteria, true, true, m_annualSpanFilter));
                preloadStudentsData();
                Map<String, Record> recordsMap = getSummaryMapForSchool(students, sklOidsToReport);
                populateGridForSchool(grid, recordsMap, skl);
            }
            grid.beforeTop();
            if (getErrorsLog().length() > 0) {
                addParameter(Record.Field.errorsLog.toString(), getErrorsLog().toString());
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
     * Gets the placement type.
     *
     * @param spedPgm OnStudentSped
     * @return String
     */
    private String getSpedPlacementType(OnStudentSped spedPgm) {
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
        return detail == null ? Boolean.FALSE : detail.getIepRequiredIndicator();
    }

    /**
     * Populate summary map.
     *
     * @param students Filterable<ReportStudent>
     * @param sklOidsToReport Collection<String>
     * @return Map
     * @throws X2BaseException exception
     */
    private Map<String, Record> getSummaryMapForSchool(Filterable<ReportStudent> students,
                                                       Collection<String> sklOidsToReport)
            throws X2BaseException {
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
        String currentDate = m_formatter
                .format(new Date(DateUtils.currentTimeMillis(OrganizationManager.getTimeZone(getOrganization()))));
        Map<String, Record> recordsByExceptCode = new HashMap<>();

        Record recordSubTotal = new Record();
        recordSubTotal.set(Record.Field.schoolName, schoolName);
        recordSubTotal.set(Record.Field.schoolNumber, schoolNumber);
        recordSubTotal.set(Record.Field.boardNumber, boardNumber);
        recordSubTotal.set(Record.Field.boardName, boardName);
        recordSubTotal.set(Record.Field.academicYears, academicYears);
        recordSubTotal.set(Record.Field.currentDate, currentDate);
        recordSubTotal.set(Record.Field.iepEC, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.iepNI, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.fullySelfContainedEC, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.fullySelfContainedNI, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.partiallyIntegratedEC, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.partiallyIntegratedNI, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.withdrawalAssistanceEC, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.withdrawalAssistanceNI, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.resourceAssistanceEC, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.resourceAssistanceNI, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.indirectServiceEC, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.indirectServiceNI, Integer.valueOf(0));
        recordSubTotal.set(Record.Field.exceptionalityCode, EXCEPT_CODE_SUB_TOTAL);
        recordsByExceptCode.put(EXCEPT_CODE_SUB_TOTAL, recordSubTotal);

        Record recordGrandTotal = new Record();
        recordGrandTotal.set(Record.Field.schoolName, schoolName);
        recordGrandTotal.set(Record.Field.schoolNumber, schoolNumber);
        recordGrandTotal.set(Record.Field.boardNumber, boardNumber);
        recordGrandTotal.set(Record.Field.boardName, boardName);
        recordGrandTotal.set(Record.Field.academicYears, academicYears);
        recordGrandTotal.set(Record.Field.currentDate, currentDate);
        recordGrandTotal.set(Record.Field.iepEC, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.iepNI, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.fullySelfContainedEC, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.fullySelfContainedNI, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.partiallyIntegratedEC, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.partiallyIntegratedNI, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.withdrawalAssistanceEC, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.withdrawalAssistanceNI, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.resourceAssistanceEC, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.resourceAssistanceNI, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.indirectServiceEC, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.indirectServiceNI, Integer.valueOf(0));
        recordGrandTotal.set(Record.Field.exceptionalityCode, EXCEPT_CODE_GRAND_TOTAL);
        recordsByExceptCode.put(EXCEPT_CODE_GRAND_TOTAL, recordGrandTotal);

        m_pgmExceptionalityByCodeMap = getDictExtractor().getReferenceCodes(
                OnStudentSped.FIELD_EXCEPTIONALITY.getField(getDictExtractor()).getReferenceTableOid()).entrySet()
                .stream()
                .filter(entry -> {
                    ReferenceCode code = entry.getValue();
                    if (!isElementaryReport()
                            && VALUE_EXCEPTIONALITY_EXCLUDED_CODE_SECONDARY.equals(code.getStateCode())) {
                        return false;
                    }
                    if (code.getDisabledIndicator()) {
                        return false;
                    }
                    return true;
                }).collect(Collectors.toMap(entry -> entry.getKey(), entry -> entry.getValue()));
        for (String exceptCode : m_pgmExceptionalityByCodeMap.keySet()) {
            Record recordToPut = new Record();
            recordToPut.set(Record.Field.schoolName, schoolName);
            recordToPut.set(Record.Field.schoolNumber, schoolNumber);
            recordToPut.set(Record.Field.boardNumber, boardNumber);
            recordToPut.set(Record.Field.boardName, boardName);
            recordToPut.set(Record.Field.academicYears, academicYears);
            recordToPut.set(Record.Field.currentDate, currentDate);
            recordToPut.set(Record.Field.iepEC, Integer.valueOf(0));
            recordToPut.set(Record.Field.iepNI, Integer.valueOf(0));
            recordToPut.set(Record.Field.fullySelfContainedEC, Integer.valueOf(0));
            recordToPut.set(Record.Field.fullySelfContainedNI, Integer.valueOf(0));
            recordToPut.set(Record.Field.partiallyIntegratedEC, Integer.valueOf(0));
            recordToPut.set(Record.Field.partiallyIntegratedNI, Integer.valueOf(0));
            recordToPut.set(Record.Field.withdrawalAssistanceEC, Integer.valueOf(0));
            recordToPut.set(Record.Field.withdrawalAssistanceNI, Integer.valueOf(0));
            recordToPut.set(Record.Field.resourceAssistanceEC, Integer.valueOf(0));
            recordToPut.set(Record.Field.resourceAssistanceNI, Integer.valueOf(0));
            recordToPut.set(Record.Field.indirectServiceEC, Integer.valueOf(0));
            recordToPut.set(Record.Field.indirectServiceNI, Integer.valueOf(0));
            String exceptCodeToDisplay = m_pgmExceptionalityByCodeMap.get(exceptCode).getDescription();
            recordToPut.set(Record.Field.exceptionalityCode, exceptCodeToDisplay);
            String exceptStateCode = m_pgmExceptionalityByCodeMap.get(exceptCode).getStateCode();
            recordToPut.set(Record.Field.area, StringUtils.isEmpty(exceptStateCode) ? ""
                    : getAreaByExceptionalityStateCode(exceptStateCode));
            recordsByExceptCode.put(exceptCode, recordToPut);
        }
        for (ReportStudent student : students.extract()) {
            String stdOEN = student.getOenRaw();
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
            List<OnStudentSped> spedPgms = student.getSpedPrograms(getBroker());
            if (spedPgms != null && !spedPgms.isEmpty()) {
                for (OnStudentSped spedPgm : spedPgms) {
                    String exceptCode = spedPgm.getExceptionalityPlain();
                    String exceptToDisplay = null;
                    String exceptStateCode = null;
                    if (!StringUtils.isEmpty(exceptCode)
                            && m_pgmExceptionalityByCodeMap.containsKey(exceptCode)) {
                        exceptStateCode = m_pgmExceptionalityByCodeMap.get(exceptCode).getStateCode();
                    } else {
                        logError("Exceptionality is not set for Student with OEN = " + stdOEN);
                    }
                    if (StringUtils.isEmpty(exceptStateCode) && !StringUtils.isEmpty(exceptCode)) {
                        exceptToDisplay = EXCEPT_CODE_NOT_INDENT;
                    }
                    if (m_pgmExceptionalityByCodeMap.containsKey(exceptCode)) {
                        exceptToDisplay = m_pgmExceptionalityByCodeMap.get(exceptCode).getDescription();
                    }
                    Record recordToIncrement = recordsByExceptCode.get(exceptCode);
                    if (recordToIncrement == null) {
                        recordToIncrement = new Record();
                        recordToIncrement.set(Record.Field.schoolName, schoolName);
                        recordToIncrement.set(Record.Field.schoolNumber, schoolNumber);
                        recordToIncrement.set(Record.Field.boardNumber, boardNumber);
                        recordToIncrement.set(Record.Field.boardName, boardName);
                        recordToIncrement.set(Record.Field.academicYears, academicYears);
                        recordToIncrement.set(Record.Field.currentDate, currentDate);
                        recordToIncrement.set(Record.Field.iepEC, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.iepNI, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.fullySelfContainedEC, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.fullySelfContainedNI, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.partiallyIntegratedEC, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.partiallyIntegratedNI, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.withdrawalAssistanceEC, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.withdrawalAssistanceNI, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.resourceAssistanceEC, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.resourceAssistanceNI, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.indirectServiceEC, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.indirectServiceNI, Integer.valueOf(0));
                        recordToIncrement.set(Record.Field.exceptionalityCode, exceptToDisplay);
                        recordToIncrement.set(Record.Field.area, EXCEPT_CODE_NOT_INDENT.equals(exceptToDisplay) ? ""
                                : getAreaByExceptionalityStateCode(exceptStateCode));
                        recordsByExceptCode.put(exceptCode, recordToIncrement);
                    }
                    Boolean mainExpectionality = getMainExceptFlag(spedPgm, student);
                    boolean isSecondaryRecord = false;
                    if (m_sskByStdOid.containsKey(student.getOid()) && m_sskByStdOid.get(student.getOid())
                            .getSchool(getBroker()).getOid().equals(m_sklDateRangeProvider.getSchool().getOid())) {
                        isSecondaryRecord = true;
                    }
                    boolean isCounted = !isSecondaryRecord && (mainExpectionality.booleanValue()
                            || (!StringUtils.isEmpty(exceptToDisplay)
                                    && EXCEPT_CODE_NOT_INDENT.equals(exceptToDisplay)));
                    if (isCounted) {
                        incrementRecord(spedPgm, recordToIncrement, recordSubTotal, recordGrandTotal);
                    }
                }
            }
        }
        return recordsByExceptCode;
    }

    /**
     * Increment record.
     *
     * @param student ReportStudent
     * @param pgmToEvaluate StudentProgramParticipation
     * @param recordToIncrement Record
     * @param recordSubTotal Record
     * @param recordGrandTotal Record
     * @throws X2BaseException exception
     */
    private void incrementRecord(OnStudentSped pgmToEvaluate,
                                 Record recordToIncrement,
                                 Record recordSubTotal,
                                 Record recordGrandTotal)
            throws X2BaseException {
        boolean isIep = getSpedIepFlag(pgmToEvaluate);
        String exceptRecordCode = (String) recordToIncrement.get(Record.Field.exceptionalityCode);
        String placementType = getSpedPlacementType(pgmToEvaluate);
        if (EXCEPT_CODE_NOT_INDENT.equalsIgnoreCase(exceptRecordCode)) {
            if (isIep) {
                recordToIncrement.set(Record.Field.iepNI,
                        Integer.valueOf((Integer) recordToIncrement.get(Record.Field.iepNI) + 1));
                recordGrandTotal.set(Record.Field.iepEC,
                        Integer.valueOf((Integer) recordGrandTotal.get(Record.Field.iepEC) + 1));

            }
            if (!StringUtils.isEmpty(placementType)) {
                switch (placementType) {
                    case "F":
                        recordToIncrement.set(Record.Field.fullySelfContainedNI,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.fullySelfContainedNI) + 1));
                        recordGrandTotal.set(Record.Field.fullySelfContainedEC,
                                Integer.valueOf((Integer) recordGrandTotal.get(Record.Field.fullySelfContainedEC) + 1));
                        break;
                    case "P":
                        recordToIncrement.set(Record.Field.partiallyIntegratedNI,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.partiallyIntegratedNI) + 1));
                        recordGrandTotal.set(Record.Field.partiallyIntegratedEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.partiallyIntegratedEC) + 1));
                        break;
                    case "W":
                        recordToIncrement.set(Record.Field.withdrawalAssistanceNI,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.withdrawalAssistanceNI) + 1));
                        recordGrandTotal.set(Record.Field.withdrawalAssistanceEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.withdrawalAssistanceEC) + 1));
                        break;
                    case "R":
                        recordToIncrement.set(Record.Field.resourceAssistanceNI,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.resourceAssistanceNI) + 1));
                        recordGrandTotal.set(Record.Field.resourceAssistanceEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.resourceAssistanceEC) + 1));
                        break;
                    case "I":
                        recordToIncrement.set(Record.Field.indirectServiceNI,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.indirectServiceNI) + 1));
                        recordGrandTotal.set(Record.Field.indirectServiceEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.indirectServiceEC) + 1));
                        break;
                    default:
                        break;
                }
            }
        } else {
            if (isIep) {
                recordToIncrement.set(Record.Field.iepEC,
                        Integer.valueOf((Integer) recordToIncrement.get(Record.Field.iepEC) + 1));
                recordSubTotal.set(Record.Field.iepEC,
                        Integer.valueOf((Integer) recordSubTotal.get(Record.Field.iepEC) + 1));
                recordGrandTotal.set(Record.Field.iepEC,
                        Integer.valueOf((Integer) recordGrandTotal.get(Record.Field.iepEC) + 1));
            }
            if (!StringUtils.isEmpty(placementType)) {
                switch (placementType) {
                    case "F":
                        recordToIncrement.set(Record.Field.fullySelfContainedEC,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.fullySelfContainedEC) + 1));
                        recordSubTotal.set(Record.Field.fullySelfContainedEC,
                                Integer.valueOf((Integer) recordSubTotal.get(Record.Field.fullySelfContainedEC) + 1));
                        recordGrandTotal.set(Record.Field.fullySelfContainedEC,
                                Integer.valueOf((Integer) recordGrandTotal.get(Record.Field.fullySelfContainedEC) + 1));
                        break;
                    case "P":
                        recordToIncrement.set(Record.Field.partiallyIntegratedEC,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.partiallyIntegratedEC) + 1));
                        recordSubTotal.set(Record.Field.partiallyIntegratedEC,
                                Integer.valueOf((Integer) recordSubTotal.get(Record.Field.partiallyIntegratedEC) + 1));
                        recordGrandTotal.set(Record.Field.partiallyIntegratedEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.partiallyIntegratedEC) + 1));
                        break;
                    case "W":
                        recordToIncrement.set(Record.Field.withdrawalAssistanceEC,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.withdrawalAssistanceEC) + 1));
                        recordSubTotal.set(Record.Field.withdrawalAssistanceEC,
                                Integer.valueOf((Integer) recordSubTotal.get(Record.Field.withdrawalAssistanceEC) + 1));
                        recordGrandTotal.set(Record.Field.withdrawalAssistanceEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.withdrawalAssistanceEC) + 1));
                        break;
                    case "R":
                        recordToIncrement.set(Record.Field.resourceAssistanceEC,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.resourceAssistanceEC) + 1));
                        recordSubTotal.set(Record.Field.resourceAssistanceEC,
                                Integer.valueOf((Integer) recordSubTotal.get(Record.Field.resourceAssistanceEC) + 1));
                        recordGrandTotal.set(Record.Field.resourceAssistanceEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.resourceAssistanceEC) + 1));
                        break;
                    case "I":
                        recordToIncrement.set(Record.Field.indirectServiceEC,
                                Integer.valueOf(
                                        (Integer) recordToIncrement.get(Record.Field.indirectServiceEC) + 1));
                        recordSubTotal.set(Record.Field.indirectServiceEC,
                                Integer.valueOf((Integer) recordSubTotal.get(Record.Field.indirectServiceEC) + 1));
                        recordGrandTotal.set(Record.Field.indirectServiceEC,
                                Integer.valueOf(
                                        (Integer) recordGrandTotal.get(Record.Field.indirectServiceEC) + 1));
                        break;
                    default:
                        break;
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
        m_sklDateRangeProvider = new SchoolDateRangeProvider();
        m_sklDateRangeProvider.setSchool(school);
        m_spanCriteria = getSpanConfiguration(m_sklDateRangeProvider.getStartDate())
                .setSchoolOids(Arrays.asList(school.getOid()))
                .setIncludeSecondarySpans(true);
    }

    /**
     * Populate grid for school.
     *
     * @param mainGrid ReportDataGrid
     * @param recordsMap Map<String,Record>
     * @param school OnSchool
     */
    private void populateGridForSchool(ReportDataGrid mainGrid, Map<String, Record> recordsMap, OnSchool school) {
        Record recordSubTotal = null;
        Record recordGrandTotal = null;
        Record recordNotIndent = null;
        ReportDataGrid sortedGridToAdd = new ReportDataGrid();
        if (recordsMap != null) {
            for (String expectCode : recordsMap.keySet()) {
                Record record = recordsMap.get(expectCode);
                String calculatedExceptCode = m_pgmExceptionalityByCodeMap.containsKey(expectCode)
                        ? m_pgmExceptionalityByCodeMap.get(expectCode).getDescription()
                        : "";
                if (!Arrays.asList(EXCEPT_CODE_NOT_INDENT, EXCEPT_CODE_GRAND_TOTAL, EXCEPT_CODE_SUB_TOTAL)
                        .contains(expectCode) && !EXCEPT_CODE_NOT_INDENT.equals(calculatedExceptCode)) {
                    sortedGridToAdd.append();
                    sortedGridToAdd.set(RF_PAGE_RESET, school.getOid());
                    for (FieldInterface field : record.m_fieldValuePairs.keySet()) {
                        sortedGridToAdd.set(field.toString(), record.m_fieldValuePairs.get(field));
                    }
                } else if (EXCEPT_CODE_NOT_INDENT.equals(expectCode)
                        || EXCEPT_CODE_NOT_INDENT.equals(calculatedExceptCode)) {
                    recordNotIndent = record;
                } else if (EXCEPT_CODE_SUB_TOTAL.equals(expectCode)) {
                    recordSubTotal = record;
                } else if (EXCEPT_CODE_GRAND_TOTAL.equals(expectCode)) {
                    recordGrandTotal = record;
                }
            }
            sortedGridToAdd.sort(
                    Arrays.asList(Record.Field.area.toString(), Record.Field.exceptionalityCode.toString()), true);
            if (recordSubTotal != null) {
                sortedGridToAdd.append();
                for (FieldInterface field : recordSubTotal.m_fieldValuePairs.keySet()) {
                    sortedGridToAdd.set(RF_PAGE_RESET, school.getOid());
                    sortedGridToAdd.set(field.toString(), recordSubTotal.m_fieldValuePairs.get(field));
                }
            }
            if (recordNotIndent != null) {
                sortedGridToAdd.append();
                for (FieldInterface field : recordNotIndent.m_fieldValuePairs.keySet()) {
                    sortedGridToAdd.set(RF_PAGE_RESET, school.getOid());
                    sortedGridToAdd.set(field.toString(), recordNotIndent.m_fieldValuePairs.get(field));
                }
            }
            if (recordGrandTotal != null) {
                sortedGridToAdd.append();
                for (FieldInterface field : recordGrandTotal.m_fieldValuePairs.keySet()) {
                    sortedGridToAdd.set(RF_PAGE_RESET, school.getOid());
                    sortedGridToAdd.set(field.toString(), recordGrandTotal.m_fieldValuePairs.get(field));
                }
            }
            sortedGridToAdd.beforeTop();
            mainGrid.append(sortedGridToAdd);
        }
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
