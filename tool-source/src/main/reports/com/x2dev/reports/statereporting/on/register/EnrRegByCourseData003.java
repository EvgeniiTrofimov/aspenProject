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
package com.x2dev.reports.statereporting.on.register;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.JasperEngine5MultyReportFiller;
import com.x2dev.procedures.statereporting.common.PublishReportsManagerMultyPerBeanJe5;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolBean.DistrictManager;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolEnrollment;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolSection;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudent;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchedule;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentScheduleChange;
import com.x2dev.procedures.statereporting.common.ToolBean.ToolStudentSchool;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Pair;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.Range;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpan;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.*;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSection.ConedRegisterInfo;
import com.x2dev.procedures.statereporting.on.revised.OnHelpersContainer.GradesHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisConstants;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;

import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.x2dev.reports.statereporting.on.register.EnrRegByCourseDataConstants.*;

/**
 * The Class EnrRegByCourseData003.
 *
 * @author Follett Software Company
 * @copyright 2022
 */
public class EnrRegByCourseData003 extends EnrRegByCourseDataHelper {
    private transient GradesHelper m_gradesHelper;

    private ReportDataGrid m_grid;

    private ReportDataGrid gridForMstForPage2;

    private ReportDataGrid gridForMstForPage3;

    private ReportDataGrid gridForMstForPage4;

    private ReportDataGrid gridForMstForPage5;

    private ReportDataGrid gridForMstForPage6;

    private String m_rptIdPart1;

    private String m_rptIdPart2;

    private int m_stdCnt;

    /**
     * Gather data.
     *
     * @return the object
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        // build course criteria
        loadSections();

        buildOutputGrid();

        // publishResults uses m_grid directly
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException the x 2 base exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // Initialize variables
        m_grid = new ReportDataGrid();

        // get report ids (for ireport parts)
        m_rptIdPart1 = (String) getParameter(PARAM_RPT_ID_PART1);
        m_rptIdPart2 = (String) getParameter(PARAM_RPT_ID_PART2);


        // Setting locale to English as localization not used yet
        // get prefixes for parts of both reports and add to separate parameters
        // cannot use the same prefix as they are different reports
        addParameter(REPORT_PREFIX1, getUserMessageKeyPrefix(getReportOidRptId(m_rptIdPart1)));
        addParameter(REPORT_PREFIX2, getUserMessageKeyPrefix(getReportOidRptId(m_rptIdPart2)));

        DistrictManager.setOrganization(getOrganization());
        DistrictManager.setDefaultCalendarIds(Arrays.asList(OnsisConstants.DEFAULT_ONSIS_CALENDAR_IDS));
        DistrictManager.setAnnualSpanFactory(new OnAnnualSpanFactory(getBroker()));
        DistrictManager.setStudentScheduleSpanFactory(new StudentScheduleSpanFactory() {
            /**
             * @see com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StudentScheduleSpanFactory#instantiateSpan(com.x2dev.procedures.statereporting.common.ToolBean.ToolSection)
             */
            @Override
            public StudentScheduleSpan instantiateSpan(ToolSection section) {
                return new RptScheduleSpan(section);
            }
        });
        // TODO: verify classes registered
        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictionaryExtractor());
        ToolBean.registerClass(RptStudent.class);
        ToolBean.registerClass(RptSection.class);
        ToolBean.registerClass(FteMonthly.class);
        ToolBean.registerClass(OnEnrollment.class);
        ToolBean.registerClass(OnSchool.class);
        ToolBean.registerClass(OnStaff.class);
        ToolBean.registerClass(OnStudentSchool.class);
    }

    /**
     * Publish results.
     *
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     *
     *      Override to combine portrait and landscape reports
     */
    @Override
    protected void publishResults() throws Exception {
        // if grid is empty run the super publishResults and return
        if (m_grid.isEmpty()) {
            super.publishResults();
        }

        else {
            // assumes jasper reports 5, if different version then add and call another
            // filler
            // method from ri MultipleGrid.. report
            JasperEngine5MultyReportFiller filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(),
                    getBroker());
            boolean publishRpt = false;
            // publishing will be done for combined report using customized publishing
            // manager
            PublishReportsManagerMultyPerBeanJe5 publishManager = null;
            /*
             * If the job has been aborted then don't bother filling the format or exporting
             * the results.
             */
            if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
                if (PublishReportsManager.isPublishing(getJob(), m_grid, this)) {
                    publishRpt = true;

                    // data source, format are null as report filler is not created/populated in
                    // publishing manager
                    publishManager = new PublishReportsManagerMultyPerBeanJe5(getJob(), null, getParameters(),
                            null, this, getSchool(), getOrganization(), getBroker());
                }

                // get report formats for both portrait and landscape reports
                InputStream rptFmtPart1 = null;
                if (!StringUtils.isEmpty(m_rptIdPart1)) {
                    rptFmtPart1 = getFormatRptId(m_rptIdPart1);
                }
                InputStream rptFmtPart2 = null;
                if (!StringUtils.isEmpty(m_rptIdPart2)) {
                    rptFmtPart2 = getFormatRptId(m_rptIdPart2);
                }

                // loop through grid and sends records for each course to report 1 (portrait)
                // and
                // report 2 (landscape)
                ToolBean mstPrev = null;
                String mstOidPrev = OnsisConstants.CONST_EMPTY;
                ReportDataGrid mstGrid = new ReportDataGrid();
                m_grid.beforeTop();

                while (m_grid.next()) {
                    ToolBean mst = (ToolBean) m_grid.get(FIELD_MASTER_SCHEDULE);
                    String mstOid = mst.getOid();

                    // if new MasterSchedule send grid records for previous master schedule to
                    // reports
                    if ((!StringUtils.isEmpty(mstOidPrev)) && (!mstOidPrev.equals(mstOid))) {
                        mstGrid.beforeTop();
                        filler5.addReport(rptFmtPart1, getParameters(), mstGrid);
                        mstGrid.beforeTop();
                        filler5.addReport(rptFmtPart2, getParameters(), mstGrid);

                        // create publisher and publish mst report (one per master schedule) if
                        // publishRpt
                        if (publishRpt) {
                            // publish report for master schedule
                            publishManager.publishReportEachBean(filler5, mstPrev);

                            // reset jasper report filler (as publish is only for one master
                            // schedule)
                            filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(), getBroker());
                        }

                        // reset formats
                        rptFmtPart1.reset();
                        rptFmtPart2.reset();

                        // initialize master schedule grid
                        mstGrid = new ReportDataGrid();
                    }
                    mstPrev = mst;
                    mstOidPrev = mstOid;

                    // add current grid record to master schedule grid
                    mstGrid.append(m_grid.getCurrentRow());
                }

                // process last master schedule
                mstGrid.beforeTop();
                filler5.addReport(rptFmtPart1, getParameters(), mstGrid);
                mstGrid.beforeTop();
                filler5.addReport(rptFmtPart2, getParameters(), mstGrid);

                // create publisher and publish mst report (last master schedule) if publishRpt
                if (publishRpt) {
                    // publish report for master scheddule
                    publishManager.publishReportEachBean(filler5, mstPrev);

                    // reset jasper report filler (as publish is only for one master schedule)
                    filler5 = new JasperEngine5MultyReportFiller(getJob(), getResultHandler(), getBroker());
                } else {
                    // else write concatenated reports to output stream
                    filler5.fillReport();
                }
            }
        }
    }

    /**
     * Builds the output grid.
     */
    private void buildOutputGrid() {
        String courseSort = (String) getParameter(PARAM_COURSE_SORT);
        ToolBean.getCachedToolBeans(RptSection.class).stream()
                .sorted(new Comparator<RptSection>() {

                    @Override
                    public int compare(RptSection mst1, RptSection mst2) {
                        int value = 0;
                        if (courseSort.equals(CONST_SORT_CRS_NUM_SECTION)) {
                            value = mst1.getCourseView().compareTo(mst2.getCourseView());
                        } else if (courseSort.equals(CONST_SORT_CRS_DESC_SECTION)) {
                            value = mst1.getDescription().compareTo(mst2.getDescription());
                            if (value == 0) {
                                value = mst1.getSectionNumber().compareTo(mst2.getSectionNumber());
                            }
                        }
                        return value;
                    }
                })
                .forEach(section -> {
                    m_stdCnt = 0;
                    // write detail records twice if days met go over 45
                    gridForMstForPage2 = new ReportDataGrid();
                    // write detail records thrice if days met go over 120
                    gridForMstForPage3 = new ReportDataGrid();
                    // write detail records thrice if days met go over 195
                    gridForMstForPage4 = new ReportDataGrid();
                    // write detail records thrice if days met go over 270
                    gridForMstForPage5 = new ReportDataGrid();
                    // write detail records thrice if days met go over 345
                    gridForMstForPage6 = new ReportDataGrid();
                    Range<Date> dateRange = section.getDateRange(getBroker());
                    section.getStudentsWithScheduleSpans(getBroker(), dateRange, false).stream()
                            .sorted(Comparator.comparing(Pair::getLeft,
                                    (s1, s2) -> s1.getNameView().compareTo(s2.getNameView())))
                            .forEach(pair -> {
                                buildPair(section, pair);
                            });
                    gridForMstForPage2.beforeTop();
                    m_grid.append(gridForMstForPage2);
                    gridForMstForPage3.beforeTop();
                    m_grid.append(gridForMstForPage3);
                    gridForMstForPage4.beforeTop();
                    m_grid.append(gridForMstForPage4);
                    gridForMstForPage5.beforeTop();
                    m_grid.append(gridForMstForPage5);
                    gridForMstForPage6.beforeTop();
                    m_grid.append(gridForMstForPage6);
                });
    }

    /**
     * Builds the pair.
     *
     * @param section the section
     * @param pair the pair
     */
    private void buildPair(RptSection section, Pair<ToolStudent, List<StudentScheduleSpan>> pair) {
        RptStudent student = (RptStudent) pair.getLeft();
        OnSchool school = (OnSchool) section.getSchool(getBroker());
        List<StudentScheduleSpan> spans = pair.getRight();

        Range<Date> scheduleSpanRange = Range.of(
                spans.stream()
                        .map(StudentScheduleSpan::getEntryDate)
                        .min(Comparator.naturalOrder())
                        .get(), spans.stream()
                        .map(StudentScheduleSpan::getExitDate)
                        .max(Comparator.naturalOrder())
                        .get());

        student.setAnnualSpans(new ArrayList<>(student.getEnrollmentSpans(getBroker(),
                section.getSchedule(getBroker()).getDistrictContextOid(),
                Collections.singletonList(school.getOid()), scheduleSpanRange)));

        if (student.getEnrollmentSpans().isEmpty()) {
            return;
        }

        ConedRegisterInfo info = section.getConedRegisterInfo(student,
                spans.stream()
                        .map(span -> (RptScheduleSpan) span)
                        .flatMap(span -> span.getDateRanges(student.getEnrollmentSpans()).stream())
                        .collect(Collectors.toList()));

        student.setConedRegisterInfo(info);
        section.addConedRegisterInfo(info);

        List<String> attList = info.getAttendanceList(isFrenchLanguage());

        int attFundedCtNonOp = student.isOtherPupil() ? 0 : info.getTotalDays();

        m_stdCnt++;

        boolean isConedSchool = student.isConedSchool(getBroker(), getDictionaryExtractor());

        m_grid.append();
        m_grid.set(FIELD_MASTER_SCHEDULE, section);
        m_grid.set(FIELD_STUDENT, student);
        m_grid.set(FIELD_OP_IND, student.isOtherPupil() ? "OP" : "");
        m_grid.set(FIELD_REG_TYPE_OCT, isConedSchool ? "CE" : student.getFteOct(section, getGradesHelper(), getBroker()));
        m_grid.set(FIELD_REG_TYPE_MAR, isConedSchool ? "CE" : student.getFteMar(section, getGradesHelper(), getBroker()));
        m_grid.set(FIELD_TOTAL_DAYS_FUNDED_REG, student.getTotalDaysFundedReg());
        m_grid.set(FIELD_TOTAL_DAYS_FUNDED_SUMMER, student.getTotalDaysFundedSummer());
        m_grid.set(FIELD_CLASS_DTL_PAGE, 1);
        m_grid.set(FIELD_CLASS_DTL_PAGE_START, 0);
        m_grid.set(FIELD_CLASS_ATT_LIST, attList);
        m_grid.set(FIELD_ATT_FUNDED_CT_NON_OP, attFundedCtNonOp);
        m_grid.set(FIELD_STUDENT_NUMBER, m_stdCnt);
        m_grid.set(FIELD_REASON, String.join("\n", info.getReasons()));

        if (attList.size() > CONST_CLASS_DTL_PAGE1_CT) {
            Map<String, Object> gridForMstCurrRowClone = new HashMap<>(m_grid.getCurrentRow());
            gridForMstForPage2.append(gridForMstCurrRowClone);
            gridForMstForPage2.set(FIELD_STUDENT_NUMBER, m_stdCnt);
            gridForMstForPage2.set(FIELD_CLASS_DTL_PAGE, 2);
            gridForMstForPage2.set(FIELD_CLASS_DTL_PAGE_START, CONST_CLASS_DTL_PAGE1_CT);
        }
        if (attList.size() > (CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT)) {
            Map<String, Object> gridForMstCurrRowClone = new HashMap<>(m_grid.getCurrentRow());
            gridForMstForPage3.append(gridForMstCurrRowClone);
            gridForMstForPage3.set(FIELD_STUDENT_NUMBER, m_stdCnt);
            gridForMstForPage3.set(FIELD_CLASS_DTL_PAGE, 3);
            gridForMstForPage3.set(FIELD_CLASS_DTL_PAGE_START,
                    CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT);
        }
        if (attList.size() > (CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 2)) {
            Map<String, Object> gridForMstCurrRowClone = new HashMap<>(m_grid.getCurrentRow());
            gridForMstForPage4.append(gridForMstCurrRowClone);
            gridForMstForPage4.set(FIELD_STUDENT_NUMBER, m_stdCnt);
            gridForMstForPage4.set(FIELD_CLASS_DTL_PAGE, 4);
            gridForMstForPage4.set(FIELD_CLASS_DTL_PAGE_START,
                    CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 2);
        }
        if (attList.size() > (CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 3)) {
            Map<String, Object> gridForMstCurrRowClone = new HashMap<>(m_grid.getCurrentRow());
            gridForMstForPage5.append(gridForMstCurrRowClone);
            gridForMstForPage5.set(FIELD_STUDENT_NUMBER, m_stdCnt);
            gridForMstForPage5.set(FIELD_CLASS_DTL_PAGE, 5);
            gridForMstForPage5.set(FIELD_CLASS_DTL_PAGE_START,
                    CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 3);
        }
        if (attList.size() > (CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 4)) {
            Map<String, Object> gridForMstCurrRowClone = new HashMap<>(m_grid.getCurrentRow());
            gridForMstForPage6.append(gridForMstCurrRowClone);
            gridForMstForPage6.set(FIELD_STUDENT_NUMBER, m_stdCnt);
            gridForMstForPage6.set(FIELD_CLASS_DTL_PAGE, 6);
            gridForMstForPage6.set(FIELD_CLASS_DTL_PAGE_START,
                    CONST_CLASS_DTL_PAGE1_CT + CONST_CLASS_DTL_PAGE2_CT * 4);
        }
    }

    /**
     * Gets the grades helper.
     *
     * @return the grades helper
     */
    private GradesHelper getGradesHelper() {
        if (m_gradesHelper == null) {
            m_gradesHelper = new GradesHelper(getCurrentContext(), getDictionaryExtractor(), getBroker());
        }

        return m_gradesHelper;
    }

    /**
     * Load sections.
     */
    private void loadSections() {
        // Get master schedule oids based on input selection
        Collection<String> mstOidsInput = new ArrayList<>();

        if (getParameter(PARAM_MST_OIDS) != null) {
            mstOidsInput.addAll(StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS), ","));
        }

        if (getParameter(PARAM_MST_OIDS_STAFF_VIEW) != null) {
            mstOidsInput.addAll(
                    StringUtils.convertDelimitedStringToList((String) getParameter(PARAM_MST_OIDS_STAFF_VIEW), ","));
        }

        if (mstOidsInput.isEmpty()) {
            Collection<String> cskConEdProgramTypesInput = new ArrayList<>();

            if (getParameter(PARAM_CSK_CON_ED_PROGRAM_TYPES) != null) {
                cskConEdProgramTypesInput.addAll(StringUtils
                        .convertDelimitedStringToList((String) getParameter(PARAM_CSK_CON_ED_PROGRAM_TYPES), ";"));
            }

            // create master schedule criteria and add sections to selection object
            X2Criteria mstCriteria = new X2Criteria();

            // add school condition
            mstCriteria.addEqualTo(ToolSection.FIELD_SCHOOL_OID.resolve(getDictionaryExtractor()),
                    getSchool().getOid());

            // add school year condition
            mstCriteria.addEqualTo(ToolSection.FIELD_DISTRICT_CONTEXT_OID.resolve(getDictionaryExtractor()),
                    getCurrentContext().getOid());

            String conedProgTypePath = OnSection.FIELD_CONED_PROG_TYPE.resolve(getDictionaryExtractor());
            if (!StringUtils.isEmpty(conedProgTypePath) && !cskConEdProgramTypesInput.isEmpty()) {
                List<String> nonCreditCodes = getDictionaryExtractor()
                        .getRefCodesWithStateValue(OnSection.FIELD_CONED_PROG_TYPE.getField(getDictionaryExtractor()),
                                cskConEdProgramTypesInput).stream()
                        .map(ReferenceCode::getCode)
                        .collect(Collectors.toList());

                mstCriteria.addIn(OnSection.FIELD_CONED_PROG_TYPE.resolve(getDictionaryExtractor()), nonCreditCodes);
            }
            FilterableFactory.create(getBroker(), getDictionaryExtractor(), RptSection.class, mstCriteria, null);
        } else {
            ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), RptSection.class, mstOidsInput);
        }

        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                ToolStudentSchedule.PARENT_SECTION);
        ToolBean.preload(getBroker(), getDictionaryExtractor(), null,
                ToolStudentScheduleChange.PARENT_SECTION);

        Set<String> studentOids = Stream.concat(
                ToolBean.getCachedToolBeans(ToolStudentSchedule.class).stream()
                        .map(ToolStudentSchedule::getStudentOid),
                ToolBean.getCachedToolBeans(ToolStudentScheduleChange.class).stream()
                        .map(ToolStudentScheduleChange::getStudentOid))
                .collect(Collectors.toSet());

        ToolBean.loadByOid(getBroker(), getDictionaryExtractor(), RptStudent.class, studentOids);

        // load enrollments and student school
        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolEnrollment.FIELD_DATE_DESC, ToolEnrollment.FIELD_TIMESTAMP_DESC),
                ToolStudent.CHILD_STUDENT_ENROLLMENTS);
        ToolBean.preload(getBroker(), getDictionaryExtractor(),
                Arrays.asList(ToolStudentSchool.FIELD_START_DATE_DESC, ToolBean.FIELD_OID),
                ToolStudent.CHILD_STUDENT_SCHOOLS);
    }
}
