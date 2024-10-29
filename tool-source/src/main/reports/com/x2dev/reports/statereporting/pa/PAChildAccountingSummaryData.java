/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.pa;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.PAChildCommonData;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.PAChildWorker;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.SplitSpan;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpanPAChild;
import com.x2dev.procedures.statereporting.pa.SubSpanHelper.WorkerSubSpanFabric;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * Data source for "PA Child Accounting Summary" report.
 *
 * @author X2 Development Corporation
 */
public class PAChildAccountingSummaryData extends ReportJavaSourceNet {

    /**
     * State report data helper to manage StudentHistoryHelper.
     */
    class PAChildAccountingSummaryReportData extends StateReportData {
        private StudentHistoryHelper m_helper;

        /**
         * Return the current student criteria.
         *
         * @return Student history helper
         */
        public StudentHistoryHelper getStudentHelper() {
            return m_helper;
        }

        /**
         * Initialize the export. Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.FALSE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
        }
    }

    /**
     * fabric for create PAChildWorker and SubSpanPAChild.
     *
     * @author Follett Software Company
     */
    public class PAChildWorkerFabric implements WorkerSubSpanFabric<PAChildWorker, SubSpanPAChild> {
        PAChildCommonData m_commonData = null;
        SubSpanHelper m_helperWF;
        PAChildWorker m_worker = null;

        /**
         * Instantiates a new PA child worker fabric.
         *
         * @param commonData PAChildCommonData
         * @param helper SubSpanHelper
         */
        public PAChildWorkerFabric(PAChildCommonData commonData, SubSpanHelper helper) {
            m_helperWF = helper;
            m_commonData = commonData;
        }

        /**
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.WorkerSubSpanFabric#createSubSpan(com.x2dev.procedures.statereporting.pa.SubSpanHelper.SubSpan)
         */
        @Override
        public SubSpanPAChild createSubSpan(SubSpanPAChild previousSpan) {
            return m_helperWF.getSubSpanPAChild(previousSpan, createWorker());
        }

        /**
         * @see com.x2dev.procedures.statereporting.pa.SubSpanHelper.WorkerSubSpanFabric#createWorker()
         */
        @Override
        public PAChildWorker createWorker() {
            m_worker = m_helperWF.getPaChildWorker();
            m_worker.setCommonData(m_commonData);
            return m_worker;
        }
    }

    /**
     * Helper class to count totals.
     *
     * @author Follett Software Company
     */
    class TotalCounter implements Comparable<TotalCounter> {
        double m_ada;
        double m_adm;
        String m_grade;
        int m_stdCount;
        String m_stdType;

        /**
         * Instantiates a new total counter.
         *
         * @param stdType String
         * @param grade String
         */
        public TotalCounter(String stdType, String grade) {
            this.m_grade = grade;
            this.m_stdType = stdType;
        }

        /**
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(TotalCounter other) {
            int value = m_grade.compareTo(other.m_grade);

            if (value == 0) {
                value = m_stdType.compareTo(other.m_stdType);
            }

            return value;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return compareTo((TotalCounter) obj) == 0 ? true : false;
        }

        /**
         * Gets the ada.
         *
         * @return the m_ada
         */
        public double getAda() {
            return m_ada;
        }

        /**
         * Gets the adm.
         *
         * @return the m_adm
         */
        public double getAdm() {
            return m_adm;
        }

        /**
         * Gets the grade.
         *
         * @return the m_grade
         */
        public String getGrade() {
            return m_grade;
        }

        /**
         * Gets the std count.
         *
         * @return the m_stdCount
         */
        public int getStdCount() {
            return m_stdCount;
        }

        /**
         * Gets the std type.
         *
         * @return the m_stdType
         */
        public String getStdType() {
            return m_stdType;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_stdType.hashCode() + m_grade.hashCode() + m_stdType.hashCode();
        }

        /**
         * Increment member variables of the TotalCounter item.
         *
         * @param ada double
         * @param adm double
         */
        public void increment(double ada, double adm) {
            this.m_ada += ada;
            this.m_adm += adm;
            this.m_stdCount += 1;
        }

    }

    /**
     * The Class TotalCounterContainer.
     */
    class TotalCounterContainer {
        TotalCounter m_allStudentsAll;
        List<TotalCounter> m_list = new ArrayList<PAChildAccountingSummaryData.TotalCounter>();
        TotalCounter m_nonResAll;
        TotalCounter m_resAll;

        /**
         * Instantiates a new total counter container.
         */
        TotalCounterContainer() {
            m_allStudentsAll = new TotalCounter(ALL_STD_TYPE, GRADE_ALL);
            m_resAll = new TotalCounter(RES_STD_TYPE, GRADE_ALL);
            m_nonResAll = new TotalCounter(NON_RES_STD_TYPE, GRADE_ALL);
            m_list.add(m_allStudentsAll);
            m_list.add(m_resAll);
            m_list.add(m_nonResAll);
        }

        /**
         * Gets the all students.
         *
         * @return Total counter
         */
        public TotalCounter getAllStudents() {
            return m_allStudentsAll;
        }

        /**
         * Gets the list.
         *
         * @return List
         */
        public List<TotalCounter> getList() {
            return m_list;
        }

        /**
         * Gets the non res all.
         *
         * @return Total counter
         */
        public TotalCounter getNonResAll() {
            return m_nonResAll;
        }

        /**
         * Gets the res all.
         *
         * @return Total counter
         */
        public TotalCounter getResAll() {
            return m_resAll;
        }

    }

    /**
     * Aliases
     */
    private static final String ALIAS_DISTRICT_RESIDENCE = "DOE DISTRICT RESIDENCE";

    private static final String ALL_STD_TYPE = "Students";

    /**
     * Columns for accounting sub report
     */
    private static final String COLUMN_ADA = "adaForPeriod";
    private static final String COLUMN_ADM = "admForPeriod";
    private static final String COLUMN_CAS = "calendar";
    private static final String COLUMN_CAS_ID = "calendarId";
    private static final String COLUMN_DAYS_PRESENT = "daysPresentForPeriod";
    private static final String COLUMN_ENR_PERCENTAGE = "enrPercentage";
    private static final String COLUMN_ENROLLMENT_DATE = "enrollmentDate";
    private static final String COLUMN_WITHDRAWAL_DATE = "withdrawalDate";
    private static final String COLUMN_EXIT_CODE = "exitCode";
    private static final String COLUMN_ENTRY_ENR = "entryEnr";
    private static final String COLUMN_FIRST_ENTRY_DATE = "firstEntryDate";
    private static final String COLUMN_GRADE = "grade";
    private static final String COLUMN_IS_RESIDENT = "isResident";
    private static final String COLUMN_LAST_ENTRY_DATE = "lastEntryDate";
    private static final String COLUMN_MEMBERSHIP_DAYS = "membershipDays";
    private static final String COLUMN_NON_RES_DAYS_MEMBERSHIP = "nonResDaysForPeriod";
    private static final String COLUMN_RACE = "race";
    private static final String COLUMN_RES_DAYS_MEMBERSHIP = "resDaysForPeriod";
    private static final String COLUMN_RESIDENT_DISTRICT = "residentDistrict";
    private static final String COLUMN_SCHOOL = "school";
    private static final String COLUMN_SCHOOL_ABBR = "schoolAbbr";
    private static final String COLUMN_SORT_STD_LAST_NAME = "sortByStdLastName";
    private static final String COLUMN_STD_ATT = "stdAttendance";
    private static final String COLUMN_STUDENT = "student";
    private static final String COLUMN_TOTAL_ENROLLMENT = "totalEnrollment";
    private static final String COLUMN_WITHDRAWAL_ENR = "withdrawalEnr";

    /**
     * Columns for summary sub report
     */
    private static final String COL_SUMMARY_ADA = "ADA";
    private static final String COL_SUMMARY_ADM = "ADM";
    private static final String COL_SUMMARY_SCHOOL = "school";
    private static final String COL_SUMMARY_SCHOOL_NAME = "schoolName";
    private static final String COL_SUMMARY_STD_COUNT = "stdCount";
    private static final String COL_SUMMARY_STD_GRADE = "grade";
    private static final String COL_SUMMARY_STD_TYPE = "stdType";
    private static final String COL_SUMMARY_STD_TYPE_SORT = "stdTypeSort";

    private static final String GRADE_ALL = "ALL";

    /**
     * Input parameters
     */
    private static final String INPUT_PARAM_ALL_CALENDARS = "allCalendars";
    private static final String INPUT_PARAM_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_PARAM_CAS_ID = "calendarId";
    private static final String INPUT_PARAM_END_CYCLE = "endCycle";
    private static final String INPUT_PARAM_IS_SCL_VERSION = "schoolVersion";
    private static final String INPUT_PARAM_SCHOOL_OIDS = "schoolOids";
    private static final String INPUT_PARAM_START_CYCLE = "startCycle";

    private static final String NON_RES_STD_TYPE = "Non-Resident students";

    /**
     * Report format variable
     */
    private static final String REPORT_FORMAT_SUMMARY = "reportFormat2";
    private static final String SUBREPORT_COL_SUMMARY = "reportSummary";

    /**
     * Report parameters
     */
    private static final String REPORT_PARAM_CAS_ID = "casId";
    private static final String REPORT_PARAM_END_CYCLE = "paramEndCycle";
    private static final String REPORT_PARAM_LEA_NAME = "leaName";
    private static final String REPORT_PARAM_START_CYCLE = "paramStartCycle";

    private static final String RES_STD_TYPE = "Resident students";

    /**
     * Sub-report IDs
     */
    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String SUBREPORT_SUMMARY = "PACAS_SUMMARY";

    /**
     * Fields
     */
    PlainDate m_endDate;
    PlainDate m_startDate;

    String m_calendarId;

    Map<String, Collection<SchoolCalendar>> m_casMap;
    /**
     * Custom objects
     */
    private PAChildAccountingSummaryReportData m_data;
    private List<DistrictCalendar> m_distCalendars;
    private String m_endCycle;
    Boolean m_isAllCalendars;
    protected Map<String, String> m_mostCommonCalendars;
    private Map<String, ReferenceCode> m_referenceRacesCodeMap;
    Collection<School> m_schools;
    private String m_startCycle;
    private X2Criteria m_studentCriteria;
    @SuppressWarnings("unused")
    private Map<String, Collection<StudentProgramParticipation>> m_studentProgramMap;
    private Map<SisSchool, TotalCounterContainer> m_summaryMap;
    private boolean m_useDetail;
    private static final String STUDENT_RESIDENCY_STATUS_CHANGED = "W21";



    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid pdfGrid = new ReportDataGrid();
        ReportDataGrid detailGrid = new ReportDataGrid();

        if (buildAccountingPart(pdfGrid, detailGrid)) {
            pdfGrid.set(SUBREPORT_COL_SUMMARY, buildSummaryPart());
            detailGrid.beforeTop();
            pdfGrid.beforeTop();
        }

        return m_useDetail ? detailGrid : pdfGrid;

    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        initReportsFormat();

        m_isAllCalendars = (Boolean) getParameter(INPUT_PARAM_ALL_CALENDARS);

        collectReportEntities();

        String startRefCodeOId = (String) getParameter(INPUT_PARAM_START_CYCLE);
        String endRefCodeOId = (String) getParameter(INPUT_PARAM_END_CYCLE);
        String casOid = (String) getParameter(INPUT_PARAM_CAS_ID);

        if (!StringUtils.isEmpty(casOid)) {
            m_calendarId = ((SchoolCalendar) getBroker().getBeanByOid(SchoolCalendar.class, casOid)).getCalendarId();
        }

        if (startRefCodeOId != null && endRefCodeOId != null) {
            m_startCycle = ((ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, startRefCodeOId)).getCode();
            m_endCycle = ((ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, endRefCodeOId)).getCode();

            if (Integer.parseInt(m_startCycle) > Integer.parseInt(m_endCycle)) {
                m_endCycle = m_startCycle;
            }

            setCycleDays(m_startCycle, m_endCycle);

            initializeSchools();

            loadRacesCodes();
            addParameter(REPORT_PARAM_LEA_NAME, getOrganization().getName());
            addParameter(REPORT_PARAM_CAS_ID, m_calendarId);

        }

        m_data = new PAChildAccountingSummaryReportData();
        m_data.setBroker(getBroker());
        m_data.setOrganization(getOrganization());
        m_data.setPrivilegeSet(getPrivilegeSet());
        if (m_schools.size() == 1) {
            m_data.setSchoolContext(true);
            m_data.setSchool(m_schools.iterator().next());
        } else {
            m_data.setSchoolContext(false);
        }
        m_data.setParameters(getParameters());
        m_data.setUser(getUser());
        m_data.initializeExport();
    }


    /**
     * Build a accounting part of report.
     *
     * @param pdfGrid ReportDataGrid
     * @param detailGrid ReportDataGrid
     * @return data grid for current sub-report
     * @throws Exception exception
     */
    private boolean buildAccountingPart(ReportDataGrid pdfGrid, ReportDataGrid detailGrid) throws Exception {
        m_summaryMap = new LinkedHashMap<SisSchool, TotalCounterContainer>();

        SubSpanHelper subSpanHelper = new SubSpanHelper(m_data.getStudentHelper(), getOrganization(), getBroker());
        StudentHistoryHelper studentHelper = m_data.getStudentHelper();

        PAChildCommonData commonData = subSpanHelper.getPAChildCommonData(m_schools, m_isAllCalendars, m_calendarId);
        WorkerSubSpanFabric fabric = new PAChildWorkerFabric(commonData, subSpanHelper);

        SplitSpan<PAChildWorker, SubSpanPAChild> splitSpan =
                subSpanHelper.getSplitSPanDefaultBehavior(fabric,
                        subSpanHelper.getRules(SubSpanHelper.SPLIT_RULES_PA_CHILD));


        Collection<SisStudent> students = getBroker().getCollectionByQuery(studentHelper.getStudentQuery(false));

        for (SisStudent student : students) {

            List<SubSpan<PAChildWorker>> spans =
                    splitSpan.splitSpan(studentHelper.getStudentEnrollments(student), false);

            StudentEnrollment entryEnr = null;

            double ada = 0;
            double adm = 0;

            double totalDaysMemdershipForPeriod = 0;
            for (SubSpan<PAChildWorker> span : spans) {
                if (!span.isSkipRecord()) {
                    StudentEnrollment operatedEnr = span.getFirstInSpanEnr();
                    PAChildWorker spanWorker = span.getWorker();
                    if (operatedEnr != null) {
                        totalDaysMemdershipForPeriod += spanWorker.getMemberShipDaysInPeriod().size();
                    }
                }
            }

            for (SubSpan<PAChildWorker> span : spans) {

                if (!span.isSkipRecord()) {

                    StudentEnrollment operatedEnr = span.getFirstInSpanEnr();
                    PAChildWorker spanWorker = span.getWorker();

                    if (operatedEnr != null) {
                        SisSchool operatedSchool = operatedEnr.getSchool();
                        TotalCounterContainer totalcontainer = m_summaryMap.get(operatedSchool);

                        if (totalcontainer == null) {
                            totalcontainer = new TotalCounterContainer();
                            m_summaryMap.put(operatedSchool, totalcontainer);
                        }
                        List<TotalCounter> totalsForSchool = totalcontainer.getList();
                        TotalCounter allStudentsAll = totalcontainer.getAllStudents();
                        TotalCounter resAll = totalcontainer.getResAll();
                        TotalCounter nonResAll = totalcontainer.getNonResAll();

                        String residentStatus = spanWorker.getResidentStatus();
                        boolean isResStudent =
                                ("A".equals(residentStatus) || StringUtils.isEmpty(residentStatus)) ? true : false;

                        pdfGrid.append();
                        detailGrid.append();

                        SchoolCalendar cas = spanWorker.getCalendar();

                        String casId = spanWorker.getCalendarId();
                        pdfGrid.set(COLUMN_CAS_ID, casId);
                        detailGrid.set(COLUMN_CAS_ID, casId);

                        String percentage = spanWorker.getPercentageForCalendar();

                        pdfGrid.set(COLUMN_ENR_PERCENTAGE, percentage);
                        detailGrid.set(COLUMN_ENR_PERCENTAGE, percentage);

                        double pctMultiplier = 1.0;
                        try {
                            pctMultiplier = Double.parseDouble(percentage);
                            pctMultiplier = pctMultiplier / 100.0;
                        } catch (Exception e) {
                            // use default of exception
                        }

                        //
                        Pair<PlainDate, PlainDate> pair = spanWorker.getFstLstCalendarDatesLimitedByRD();


                        pdfGrid.set(COLUMN_FIRST_ENTRY_DATE, pair.getLeft());
                        detailGrid.set(COLUMN_FIRST_ENTRY_DATE, pair.getLeft());

                        pdfGrid.set(COLUMN_LAST_ENTRY_DATE, pair.getRight());
                        detailGrid.set(COLUMN_LAST_ENTRY_DATE, pair.getRight());

                        pdfGrid.set(COLUMN_CAS, cas);
                        detailGrid.set(COLUMN_CAS, cas);

                        pdfGrid.set(COLUMN_SCHOOL, operatedSchool);
                        detailGrid.set(COLUMN_SCHOOL, operatedSchool);

                        String sklAbbreviation = getSchoolAbbreviation(operatedSchool);
                        pdfGrid.set(COLUMN_SCHOOL_ABBR, sklAbbreviation);
                        detailGrid.set(COLUMN_SCHOOL_ABBR, sklAbbreviation);

                        pdfGrid.set(COLUMN_STUDENT, student);
                        detailGrid.set(COLUMN_STUDENT, student);

                        pdfGrid.set(COLUMN_SORT_STD_LAST_NAME, student.getPerson().getLastName());
                        detailGrid.set(COLUMN_SORT_STD_LAST_NAME, student.getPerson().getLastName());

                        ReferenceCode raceCode = m_referenceRacesCodeMap.get(student.getPerson().getRaceView());
                        String race = "";
                        if (raceCode != null) {
                            race = raceCode.getStateCode();
                        }

                        pdfGrid.set(COLUMN_RACE, race);
                        detailGrid.set(COLUMN_RACE, race);

                        pdfGrid.set(COLUMN_GRADE, spanWorker.getGradeLevel());
                        detailGrid.set(COLUMN_GRADE, spanWorker.getGradeLevel());

                        entryEnr = span.getFirstInSpanEnr();
                        StudentEnrollment withdrawalEnr = span.getLastInSpanEnr();

                        // reive it to change like first in span
                        pdfGrid.set(COLUMN_ENTRY_ENR, entryEnr);
                        detailGrid.set(COLUMN_ENTRY_ENR, entryEnr);

                        // check and change name to last in span enr
                        pdfGrid.set(COLUMN_WITHDRAWAL_ENR, withdrawalEnr);
                        detailGrid.set(COLUMN_WITHDRAWAL_ENR, withdrawalEnr);

                        PlainDate exitDate = null;
                        if (withdrawalEnr != null) {

                            exitDate = spanWorker.getLstInSessionDateLimitedByCSY();
                        }
                        pdfGrid.set(COLUMN_WITHDRAWAL_DATE, exitDate);
                        detailGrid.set(COLUMN_WITHDRAWAL_DATE, exitDate);

                        pdfGrid.set(COLUMN_EXIT_CODE, getWithdrawalExitCode(withdrawalEnr));
                        detailGrid.set(COLUMN_EXIT_CODE, getWithdrawalExitCode(withdrawalEnr));
                        double stdAtt = spanWorker.getAbsentDays();

                        double daysMemdershipForPeriod = spanWorker.getMemberShipDaysInPeriod().size();
                        pdfGrid.set(COLUMN_TOTAL_ENROLLMENT, Integer.valueOf((int) totalDaysMemdershipForPeriod));
                        detailGrid.set(COLUMN_TOTAL_ENROLLMENT, Integer.valueOf((int) totalDaysMemdershipForPeriod));

                        pdfGrid.set(COLUMN_MEMBERSHIP_DAYS, Integer.valueOf((int) daysMemdershipForPeriod));
                        detailGrid.set(COLUMN_MEMBERSHIP_DAYS, Integer.valueOf((int) daysMemdershipForPeriod));

                        pdfGrid.set(COLUMN_ENROLLMENT_DATE, spanWorker.getFstInSessionDateLimitByCSY());
                        detailGrid.set(COLUMN_ENROLLMENT_DATE, spanWorker.getFstInSessionDateLimitByCSY());
                        if (isResStudent) {
                            pdfGrid.set(COLUMN_IS_RESIDENT, "Y");
                            detailGrid.set(COLUMN_IS_RESIDENT, "Y");

                            pdfGrid.set(COLUMN_RESIDENT_DISTRICT, operatedSchool.getOrganization1().getId());
                            detailGrid.set(COLUMN_RESIDENT_DISTRICT, operatedSchool.getOrganization1().getId());

                            pdfGrid.set(COLUMN_RES_DAYS_MEMBERSHIP, Integer.valueOf((int) daysMemdershipForPeriod));
                            detailGrid.set(COLUMN_RES_DAYS_MEMBERSHIP, Integer.valueOf((int) daysMemdershipForPeriod));

                            pdfGrid.set(COLUMN_NON_RES_DAYS_MEMBERSHIP, null);
                            detailGrid.set(COLUMN_NON_RES_DAYS_MEMBERSHIP, null);
                        } else {
                            pdfGrid.set(COLUMN_IS_RESIDENT, "N");
                            detailGrid.set(COLUMN_IS_RESIDENT, "N");

                            String residentDistrict = (String) student.getFieldValueByAlias(ALIAS_DISTRICT_RESIDENCE);
                            pdfGrid.set(COLUMN_RESIDENT_DISTRICT, residentDistrict);
                            detailGrid.set(COLUMN_RESIDENT_DISTRICT, residentDistrict);

                            pdfGrid.set(COLUMN_RES_DAYS_MEMBERSHIP, null);
                            detailGrid.set(COLUMN_RES_DAYS_MEMBERSHIP, null);

                            pdfGrid.set(COLUMN_NON_RES_DAYS_MEMBERSHIP, Integer.valueOf((int) daysMemdershipForPeriod));
                            detailGrid.set(COLUMN_NON_RES_DAYS_MEMBERSHIP,
                                    Integer.valueOf((int) daysMemdershipForPeriod));
                        }

                        pdfGrid.set(COLUMN_STD_ATT, Double.valueOf(stdAtt));
                        detailGrid.set(COLUMN_STD_ATT, Double.valueOf(stdAtt));

                        Double daysPresentForPeriod = Double.valueOf(daysMemdershipForPeriod - stdAtt);

                        pdfGrid.set(COLUMN_DAYS_PRESENT, daysPresentForPeriod);
                        detailGrid.set(COLUMN_DAYS_PRESENT, daysPresentForPeriod);

                        if (daysPresentForPeriod != null && cas != null && cas.getDaysInSession() > 0) {
                            ada = (((double) daysPresentForPeriod.intValue()) / (cas.getDaysInSession())) *
                                    pctMultiplier;
                        }

                        pdfGrid.set(COLUMN_ADA, Double.valueOf(ada));
                        detailGrid.set(COLUMN_ADA, Double.valueOf(ada));

                        if (daysPresentForPeriod != null && daysMemdershipForPeriod != 0 && cas != null
                                && cas.getDaysInSession() > 0) {
                            if (totalDaysMemdershipForPeriod > cas.getDaysInSession()) {
                                adm = ((daysMemdershipForPeriod) / (totalDaysMemdershipForPeriod)) * pctMultiplier;
                            } else {
                                adm = ((daysMemdershipForPeriod) / (cas.getDaysInSession())) * pctMultiplier;
                            }
                        }

                        pdfGrid.set(COLUMN_ADM, Double.valueOf(adm));
                        detailGrid.set(COLUMN_ADM, Double.valueOf(adm));

                        TotalCounter allStudents = new TotalCounter(ALL_STD_TYPE, student.getGradeLevel());
                        TotalCounter totalToAddRes = new TotalCounter(RES_STD_TYPE, student.getGradeLevel());
                        TotalCounter totalToAddNonRes = new TotalCounter(NON_RES_STD_TYPE,
                                student.getGradeLevel());

                        if (isResStudent) {
                            totalsForSchool.get(totalsForSchool.indexOf(resAll)).increment(ada, adm);

                            if (totalsForSchool.contains(totalToAddRes)) {
                                totalsForSchool.get(totalsForSchool.indexOf(totalToAddRes)).increment(ada, adm);
                            } else {
                                totalToAddRes.increment(ada, adm);
                                totalsForSchool.add(totalToAddRes);

                            }

                        } else {
                            totalsForSchool.get(totalsForSchool.indexOf(nonResAll)).increment(ada, adm);

                            if (totalsForSchool.contains(totalToAddNonRes)) {
                                totalsForSchool.get(totalsForSchool.indexOf(totalToAddNonRes)).increment(ada,
                                        adm);
                            } else {
                                totalToAddNonRes.increment(ada, adm);
                                totalsForSchool.add(totalToAddNonRes);

                            }

                        }

                        totalsForSchool.get(totalsForSchool.indexOf(allStudentsAll)).increment(ada, adm);

                        if (totalsForSchool.contains(allStudents)) {
                            totalsForSchool.get(totalsForSchool.indexOf(allStudents)).increment(ada, adm);
                        } else {
                            allStudents.increment(ada, adm);
                            totalsForSchool.add(allStudents);
                        }
                    }
                }
            }
        }

        pdfGrid.sort(Arrays.asList(COLUMN_SCHOOL_ABBR, COLUMN_CAS_ID, COL_SUMMARY_STD_GRADE, COLUMN_SORT_STD_LAST_NAME),
                true);
        detailGrid.sort(
                Arrays.asList(COLUMN_SCHOOL_ABBR, COLUMN_CAS_ID, COL_SUMMARY_STD_GRADE, COLUMN_SORT_STD_LAST_NAME),
                true);

        return !pdfGrid.isEmpty();
    }


    /**
     * Build a summary part of report.
     *
     * @return data grid for current sub-report
     */
    private ReportDataGrid buildSummaryPart() {
        ReportDataGrid grid = new ReportDataGrid();

        for (Map.Entry<SisSchool, TotalCounterContainer> entry : m_summaryMap.entrySet()) {
            SisSchool school = entry.getKey();
            for (TotalCounter totalCounter : entry.getValue().getList()) {
                grid.append();
                grid.set(COL_SUMMARY_SCHOOL, school);
                grid.set(COL_SUMMARY_SCHOOL_NAME, school.getName());
                grid.set(COL_SUMMARY_STD_TYPE, totalCounter.getStdType());
                grid.set(
                        COL_SUMMARY_STD_TYPE_SORT,
                        ALL_STD_TYPE.equals(totalCounter.getStdType()) ? "1"
                                : RES_STD_TYPE.equals(totalCounter
                                        .getStdType()) ? "2" : "3");
                grid.set(COL_SUMMARY_STD_GRADE, totalCounter.getGrade());
                grid.set(COL_SUMMARY_STD_COUNT, Integer.valueOf(totalCounter.getStdCount()));
                grid.set(COL_SUMMARY_ADA, Double.valueOf(totalCounter.getAda()));
                grid.set(COL_SUMMARY_ADM, Double.valueOf(totalCounter.getAdm()));
            }
        }

        grid.sort(Arrays.asList(COL_SUMMARY_SCHOOL_NAME, COL_SUMMARY_STD_TYPE_SORT, COL_SUMMARY_STD_GRADE), true);
        grid.beforeTop();
        return grid;
    }

    /**
     * Loading appropriate sub-report format.
     */
    private void collectReportEntities() {
        Report report2 = ReportUtils.getReport(SUBREPORT_SUMMARY, getBroker());
        // Send them to report parameters as byte-streams
        addParameter(REPORT_FORMAT_SUMMARY, new ByteArrayInputStream(report2.getCompiledFormat()));
    }

    /**
     * Gets the school abbreviation.
     *
     * @param school SisSchool
     * @return abbreviation of school name
     */
    private String getSchoolAbbreviation(SisSchool school) {
        String sklAbbr = "";

        if (school != null) {
            String sklName = school.getName();
            if (sklName != null) {
                ArrayList<String> delimeteredName = StringUtils.convertDelimitedStringToList(sklName, ' ', true);
                for (String word : delimeteredName) {
                    char firstChar = word.charAt(0);
                    if (Character.isLetter(firstChar)) {
                        sklAbbr += firstChar;
                    }
                }
            }
        }

        return sklAbbr;
    }



    /**
     * Gets the withdrawal exit code.
     *
     * @param withdrawalEnr StudentEnrollment
     * @return String
     */
    private String getWithdrawalExitCode(StudentEnrollment withdrawalEnr) {

        String wCode = null;
        if (withdrawalEnr != null) {
            if (StudentEnrollment.STATUS_CHANGE.equals(withdrawalEnr.getEnrollmentType())) {
                wCode = STUDENT_RESIDENCY_STATUS_CHANGED;
            } else if (StudentEnrollment.WITHDRAWAL.equals(withdrawalEnr.getEnrollmentType())) {
                wCode = withdrawalEnr.getEnrollmentCode();
            }
        }
        return wCode;
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        String formatPDF = (String) getParameter(INPUT_REPORT_ID_PDF);
        String formatCSV = (String) getParameter(INPUT_REPORT_ID_CSV);
        ToolJob job = this.getJob();
        m_useDetail = false;
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                m_useDetail = true;
                this.setFormatId(formatCSV);
                break;
            case ToolInput.HTML_FORMAT:
                this.setFormatId(formatPDF);
                break;
            case ToolInput.PDF_FORMAT:
                this.setFormatId(formatPDF);
                break;
            case ToolInput.XLS_FORMAT:
                this.setFormatId(formatPDF);
                break;
        }
    }

    /**
     * Loads the schools used in the export.
     *
     * @return Collection<SisSchool>
     */
    private void initializeSchools() {
        X2Criteria criteria = new X2Criteria();

        String sklOid = (String) getParameter(INPUT_PARAM_SCHOOL_OIDS);

        Boolean isAllSchools = (Boolean) getParameter(INPUT_PARAM_ALL_SCHOOLS);

        Boolean isSchoolVersion = (Boolean) getParameter(INPUT_PARAM_IS_SCL_VERSION);

        if (isSchoolVersion != null && isSchoolVersion.booleanValue()) {
            m_schools = new ArrayList<School>();
            m_schools.add(getSchool());
        } else {
            if (isAllSchools.booleanValue()) {
                criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
                criteria.addAndCriteria(getOrganizationCriteria(SisSchool.class));
            } else {
                criteria.addEqualTo(X2BaseBean.COL_OID, sklOid);
            }

            m_schools = getBroker().getCollectionByQuery(new QueryByCriteria(SisSchool.class, criteria));
        }
    }

    /**
     * Load student programs for special education for entire year.
     */
    @SuppressWarnings("unused")
    private void initializeStudentPrograms() {
        m_studentCriteria = m_data.getStudentHelper().getStudentCriteria();

        if (m_schools == null) {
            initializeSchools();
        }

        m_studentCriteria.addIn(SisStudent.COL_SCHOOL_OID, m_schools);

        X2Criteria programCriteria = new X2Criteria();
        programCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, getOrganization()
                .getCurrentContext().getEndDate());
        X2Criteria endDate1Criteria = new X2Criteria();
        endDate1Criteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());

        X2Criteria endDate2Criteria = new X2Criteria();
        endDate2Criteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, getOrganization()
                .getCurrentContext().getStartDate());

        endDate1Criteria.addOrCriteria(endDate2Criteria);
        programCriteria.addAndCriteria(endDate1Criteria);

        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_studentCriteria);
        programCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);

        /**
         * TODO set up proper program code
         *
         * uncomment after getting info about what program code to set
         */
        // programCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE,
        // getProgramCodes(SPECIAL_ED_CODE));

        QueryByCriteria programQuery = new QueryByCriteria(StudentProgramParticipation.class, programCriteria);
        programQuery.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        programQuery.addOrderBy(StudentProgramParticipation.COL_START_DATE, true);
        m_studentProgramMap = getBroker().getGroupedCollectionByQuery(programQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 1024);
    }

    /**
     * Load races codes.
     */
    private void loadRacesCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop = new ModelProperty(SisPerson.class, SisPerson.COL_RACE_VIEW, getBroker()
                .getPersistenceKey());
        DataDictionaryField field = null;
        ReferenceTable referenceTable = null;

        if (prop != null) {
            field = dictionary.findDataDictionaryField(prop.getFieldId());
        }

        if (field != null) {
            referenceTable = field.getReferenceTable();
        }

        if (referenceTable != null) {
            m_referenceRacesCodeMap = referenceTable.getCodeMap();
        } else {
            m_referenceRacesCodeMap = new HashMap<String, ReferenceCode>();
        }
    }

    /**
     * Calculate days of the given attendance period.
     *
     * @param startCycle String
     * @param endCycle String
     */
    private void setCycleDays(String startCycle, String endCycle) {

        addParameter(REPORT_PARAM_START_CYCLE, startCycle);
        addParameter(REPORT_PARAM_END_CYCLE, endCycle);
        ArrayList<String> cycles = new ArrayList<String>();
        if (startCycle != null && endCycle != null) {
            cycles = new ArrayList<String>();
            int start = Integer.parseInt(startCycle);
            int end = Integer.parseInt(endCycle);
            for (int i = start; i <= end; i++) {
                String cycleToAdd = String.valueOf(i);
                if (cycleToAdd.length() < 2) {
                    cycleToAdd = "0".concat(cycleToAdd);
                }
                cycles.add(cycleToAdd);
            }
        }

        if (cycles.isEmpty()) {
            cycles.add(startCycle);
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());

        criteria.addIn(DistrictCalendar.COL_CYCLE, cycles);
        QueryByCriteria query = new QueryByCriteria(DistrictCalendar.class, criteria);
        query.addOrderBy(DistrictCalendar.COL_DATE, true);

        m_distCalendars = (List<DistrictCalendar>) getBroker().getCollectionByQuery(query);
        if (!m_distCalendars.isEmpty()) {
            m_startDate = m_distCalendars.get(0).getDate();
            m_endDate = m_distCalendars.get(m_distCalendars.size() - 1).getDate();
        } else {
            String errorMessage = "Report can not be generated. Found zero days for period.";
            AppGlobals.getLog().log(Level.SEVERE, errorMessage);
            throw new IllegalArgumentException(errorMessage);
        }

    }
}

