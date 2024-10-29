/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentHistoryHelper;
import com.x2dev.procedures.statereporting.tn.TNStateReportData;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.*;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source of Data sources for the "TN Discipline Report" report and sub reports.
 *
 * @author X2 Development Corporation
 */
public class ConductActionData extends ReportJavaSourceNet {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class SpanHelper.
     */
    class SpanHelper extends TNStateReportData {
        private TNEnrollmentHelper m_tnEnrHelper;
        private TNStudentHistoryHelper m_tnStudentHelper;

        /**
         * Returns grade level reference code that had student for date range.
         *
         * @param student SisStudent
         * @param startDate PlainDate
         * @param endDate PlainDate
         * @return Reference code
         */
        public ReferenceCode getGradeLevelByDates(SisStudent student, PlainDate startDate, PlainDate endDate) {
            return m_tnStudentHelper.getGradeLevelByDates(student, startDate, endDate);
        }

        /**
         * Returns a list of student enrollment spans that represent all of the students enrollment
         * activity and segments.
         *
         * @param student Student
         * @param limit boolean
         * @return List<StudentEnrollmentSpan>
         */
        public List<TNStudentEnrollmentSpan> getTNStudentEnrollmentSpans(Student student, boolean limit) {
            return m_tnStudentHelper.getTNStudentEnrollmentSpans(student, limit);
        }

        /**
         * Initialize the export.
         * Set up the student history helper.
         *
         * @throws X2BaseException exception
         */
        @Override
        public void initialize() throws X2BaseException {
            super.initialize();
            m_tnEnrHelper = new TNEnrollmentHelper(this);
            m_tnStudentHelper = m_tnEnrHelper.getStudentHistoryHelper();

            m_tnStudentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SCHEDULE_SPANS, Boolean.TRUE);

            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG, Boolean.TRUE);
            m_tnStudentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS, Boolean.TRUE);
        }
    }

    /**
     * It's helper for counts.
     */
    class IncrementableTable {

        protected Set<String> m_columns;
        protected String m_fieldNameForRowHeader;
        protected Set<String> m_rows;
        protected Map<String, Map<String, Integer>> m_table = null;

        /**
         * Instantiates a new incrementable table.
         *
         * @param rows Set
         * @param columns Set
         * @param fieldNameForRowHeader String
         */
        public IncrementableTable(Set rows, Set columns, String fieldNameForRowHeader) {
            m_fieldNameForRowHeader = fieldNameForRowHeader;
            m_rows = rows;
            m_columns = columns;
            resetCounts();
        }

        /**
         * Generate dataGrid with values.
         *
         * @return Report data grid
         */
        public ReportDataGrid getGrid() {
            ReportDataGrid grid = new ReportDataGrid();

            for (String rowKey : m_rows) {
                Map row = m_table.get(rowKey);
                grid.append(row);
                grid.set(m_fieldNameForRowHeader, rowKey);

            }
            grid.beforeTop();
            return grid;
        }

        /**
         * Increment cell value.
         *
         * @param rowKey String
         * @param columnKey String
         * @return true, if successful
         */
        public boolean incValue(String rowKey, String columnKey) {
            Map row = m_table.get(rowKey);
            if (row == null) {
                return false;
            }
            Integer value = (Integer) row.get(columnKey);
            if (value == null) {
                return false;
            }
            row.put(columnKey, Integer.valueOf(value.intValue() + 1));

            return true;
        }

        /**
         * Fills table by zero values.
         */
        public void resetCounts() {
            m_table = new HashMap();
            for (String rowKey : m_rows) {
                Map row = new HashMap();
                for (String columnKey : m_columns) {
                    row.put(columnKey, Integer.valueOf(0));
                }
                m_table.put(rowKey, row);
            }
        }

        /**
         * Sum.
         *
         * @param value1 Integer
         * @param value2 int
         * @return Integer
         */
        public Integer sum(Integer value1, int value2) {
            return Integer.valueOf(value1.intValue() + value2);
        }

    }

    protected PlainDate m_endDate;
    protected PlainDate m_startDate;

    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_EIS_STATE_ID = "DOE EIS STATE ID";

    private static final String DETAIL_REPORT_FIELD_ACTION_CODE = "actionCode";
    private static final String DETAIL_REPORT_FIELD_ACTION_END_DATE = "actionEndDate";
    private static final String DETAIL_REPORT_FIELD_ACTION_START_DATE = "actionStartDate";
    private static final String DETAIL_REPORT_FIELD_CAUSE_KEY = "causeKey";
    private static final String DETAIL_REPORT_FIELD_GENDER = "gender";
    private static final String DETAIL_REPORT_FIELD_GRADE_LEVEL = "gradeLevel";
    private static final String DETAIL_REPORT_FIELD_HISPANIC_INDICATOR = "hispanicIndicator";
    private static final String DETAIL_REPORT_FIELD_INCIDENT_DATE = "incidentDate";
    private static final String DETAIL_REPORT_FIELD_INCIDENT_ID = "incidentId";
    private static final String DETAIL_REPORT_FIELD_LOCAL_ID = "localId";
    private static final String DETAIL_REPORT_FIELD_RACE_VIEW = "raceView";
    private static final String DETAIL_REPORT_FIELD_SCHOOL_NAME = "schoolName";
    private static final String DETAIL_REPORT_FIELD_STATE_ACTION_CODE = "stateActionCode";
    private static final String DETAIL_REPORT_FIELD_STATE_ID = "stateId";
    private static final String DETAIL_REPORT_FIELD_STUDENT_NAME = "studentName";

    private static final int INITIAL_ACTION_SIZE = 4;
    private static final int INITIAL_CAUSE_SIZE = 22;

    private static final String INPUT_ALL_SCHOOLS = "allSchools";
    private static final String INPUT_ACTION_REF_TABLE_OID = "actionRefTableOid";
    private static final String INPUT_CAUSE_CRITERIA = "causeCriteria";
    private static final String INPUT_CAUSE_REF_TABLE_OID = "causeRefTableOid";
    private static final String INPUT_DISTRICT_SUMMARY = "includeDistrictSummary";
    private static final String INPUT_EXPLUSION_CODES = "explusionCodes";
    private static final String INPUT_FEMALE_CODE = "femaleCode";
    private static final String INPUT_GRADE_CRITERIA = "gradeCriteria";
    private static final String INPUT_IN_SCHOOL_SUSPENSIONS_CODES = "inSchoolSuspensionsCodes";
    private static final String INPUT_KEY_DELIMITER = "keyDelimiter";
    private static final String INPUT_LIST_DELIMITER = "listDelimiter";
    private static final String INPUT_LONG_TERM_CHANGES_CODES = "longTermChangesCodes";
    private static final String INPUT_MALE_CODE = "maleCode";
    private static final String INPUT_PERIOD = "period";
    private static final String INPUT_RACE_REF_TABLE_OID = "raceRefTableOid";
    private static final String INPUT_REPORT_ID_CSV = "subreportIdCSVVersion";
    private static final String INPUT_REPORT_ID_PDF = "subreportIdPDFVersion";
    private static final String INPUT_REPORT_MODE = "reportMode";
    private static final String INPUT_REPORT_PERIOD = "reportPeriod";
    private static final String INPUT_SCHOOLS = "schoolOids";
    private static final String INPUT_SUBREPORT_ID_SECTION1 = "subreportIdSection1";
    private static final String INPUT_SUBREPORT_ID_SECTION2 = "subreportIdSection2";
    private static final String INPUT_SUBREPORT_ID_SECTION3 = "subreportIdSection3";
    private static final String INPUT_SUMMARY_ONLY = "summaryOnly";
    private static final String INPUT_SUSPENSIONS_CODES = "suspensionsCodes";

    private static final String RACE_HISPANIC = "hispanic";
    private static final String RACE_TWO_OR_MORE = "TWO_OR_MORE_RACES";

    private static final String REF_TABLE_OID_CONDUCT_ACTION = "rtbCndAction";

    private static final String ROOT_FIELD_ACTION_TYPE = "actionType";
    private static final String ROOT_FIELD_DATA_SOURCE = "dataSourse";
    private static final String ROOT_FIELD_PERIOD_MONTH = "periodMonth";
    private static final String ROOT_FIELD_PERIOD_START = "periodStartDate";
    private static final String ROOT_FIELD_PERIOD_END = "periodEndDate";
    private static final String ROOT_FIELD_REPORT_FORMAT = "reportFormat";
    private static final String ROOT_FIELD_SCHOOL = "school";
    private static final String ROOT_PARAM_SCHOOL_YEAR = "schoolYear";
    private static final String ROOT_PARAM_ORGANIZATION = "organization";

    private static final String STAFF_SCHOOL_CODE = "9999";

    private static final String SUB_FIELD_ROW_NAME = "rowName";

    private Map<String, ReferenceCode> m_actionCodeMap;

    private Map m_criteriaActionInverse;
    private Set m_criteriaActionOrder;

    private Map m_criteriaCauseInverse;
    private Set m_criteriaCauseOrder;

    private Map m_criteriaGradeInverse;
    private Set m_criteriaGradeOrder;

    private SpanHelper m_data;

    private DataDictionary m_dictionary;

    private Map m_districtSection1Tables;
    private Map m_districtSection2Tables;
    private IncrementableTable m_districtSection3Table;

    private Boolean m_includeDistrictSummary;

    private String m_month = null;

    private TNReportingPeriodHelper m_periodHelper;

    private Integer m_reportMode;

    private Map m_schoolSection1Tables;
    private Map m_schoolSection2Tables;
    private IncrementableTable m_schoolSection3Table;

    private DistrictSchoolYearContext m_context;

    private Report m_subreport1;
    private Report m_subreport2;
    private Report m_subreport3;

    private Boolean m_summaryOnly;

    private boolean m_useDetail;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    /**
     * @throws X2BaseException
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        ReportDataGrid detailGrid = new ReportDataGrid();

        X2Criteria actCriteria = new X2Criteria();
        actCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_endDate);

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_END_DATE, m_startDate);
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIsNull(ConductAction.COL_ACTION_END_DATE);
        endDateCriteria.addOrCriteria(orCriteria);
        actCriteria.addAndCriteria(endDateCriteria);

        // Do not include data for incidents for a prior year offense.
        actCriteria.addGreaterOrEqualThan(
                ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE, m_startDate);
        actCriteria.addLessOrEqualThan(ConductAction.REL_INCIDENT + PATH_DELIMITER + ConductIncident.COL_INCIDENT_DATE,
                m_endDate);

        Collection<SisSchool> schools = getSchools();
        if (schools.size() == 0) {
            return grid;
        }
        Collection<String> schoolOids = new LinkedList<String>();
        // Get selected school oids to join by selected schools
        for (SisSchool school : schools) {
            schoolOids.add(school.getOid());
        }

        actCriteria.addIn(ConductAction.COL_SCHOOL_OID, schoolOids);
        ReportQueryByCriteria actQuery = new ReportQueryByCriteria(ConductAction.class, actCriteria);
        actQuery.addOrderByAscending(ConductAction.COL_SCHOOL_OID);
        actQuery.addOrderByAscending(ConductAction.COL_INCIDENT_OID);
        actQuery.addOrderByAscending(ConductAction.COL_STUDENT_OID);

        Map<String, Collection<ConductAction>> schoolActions =
                getBroker().getGroupedCollectionByQuery(actQuery, ConductAction.COL_SCHOOL_OID, 40);

        PlainDate startDate = null;
        PlainDate endDate = null;

        for (SisSchool school : schools) {
            m_data = new SpanHelper();
            m_data.setBroker(getBroker());
            m_data.setOrganization(getOrganization());
            m_data.setPrivilegeSet(getPrivilegeSet());
            m_data.setSchoolContext(true);
            m_data.setSchool(school);
            m_data.setParameters(getParameters());
            m_data.setUser(getUser());
            m_data.initializeExport();
            //
            Collection<ConductAction> actions = schoolActions.get(school.getOid());

            if (actions == null) {
                actions = new ArrayList<ConductAction>();
            }
            ConductAction tempConductAction = null;

            if (m_reportMode.intValue() == 1 && m_periodHelper != null) {
                startDate = m_periodHelper.getDateBegin(school.getOid());
                endDate = m_periodHelper.getDateEnd(school.getOid());
                if (startDate == null || endDate == null) {
                    String message = "For school " + school.getName() + " is not defined report period with code: " +
                            m_periodHelper.getMonth().getCode();
                    AppGlobals.getLog().log(Level.WARNING, message);
                    continue;
                }
            }
            // Index to determine last action.
            int index = 0;
            for (ConductAction action : actions) {
                index++;
                // If report mode is Period mode, get start date and end date of period for current
                // school.
                // If action start date is outside period range, do not include the action
                // - then do not include that action
                if (m_reportMode.intValue() == 1 && m_periodHelper != null) {
                    if (action.getActionStartDate().after(endDate) || action.getActionStartDate().before(startDate)) {
                        // If current action is skipped, but previous action was appropriate, then
                        // add previous
                        // action.
                        tempConductAction = addAllSectionsToCounts(tempConductAction, null, detailGrid);
                        continue;
                    }
                }
                tempConductAction = addAllSectionsToCounts(tempConductAction, action, detailGrid);
                // Add last action of school.
                if (actions.size() == index) {
                    tempConductAction = addAllSectionsToCounts(tempConductAction, null, detailGrid);
                }
            }
            if (!m_summaryOnly.booleanValue()) {
                fillGrid(grid, school, m_schoolSection1Tables, m_schoolSection2Tables, m_schoolSection3Table,
                        m_month, startDate, endDate);
            }
            resetCounts();
        }

        if (m_includeDistrictSummary.booleanValue()) {
            fillGrid(grid, null, m_districtSection1Tables, m_districtSection2Tables, m_districtSection3Table,
                    m_month, startDate, endDate);
        }

        grid.beforeTop();
        detailGrid.beforeTop();
        return m_useDetail ? detailGrid : grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        loadActionCodes();
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        m_includeDistrictSummary = (Boolean) getParameter(INPUT_DISTRICT_SUMMARY);

        if (m_includeDistrictSummary == null || isSchoolContext()) {
            m_includeDistrictSummary = Boolean.FALSE;
        }

        m_summaryOnly = (Boolean) getParameter(INPUT_SUMMARY_ONLY);
        if (m_summaryOnly == null || isSchoolContext()) {
            m_summaryOnly = Boolean.FALSE;
        }

        m_context = getCurrentContext();

        m_reportMode = getParameter(INPUT_REPORT_MODE) != null ? (Integer) getParameter(INPUT_REPORT_MODE)
                : Integer.valueOf(0);

        m_startDate = m_context.getStartDate();
        m_endDate = m_context.getEndDate();

        // Annual Mode - used by default.
        switch (m_reportMode.intValue()) {
            // Report Period Mode
            case 1:
                ReferenceCode reportPeriod = null;
                String reportPeriodOid = (String) getParameter(INPUT_REPORT_PERIOD);
                if (!StringUtils.isEmpty(reportPeriodOid)) {
                    reportPeriod = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, reportPeriodOid);
                } else {
                    String errorMessage = "Report period must be specified";
                    AppGlobals.getLog().log(Level.SEVERE, errorMessage);
                    throw new IllegalArgumentException(errorMessage);
                }
                m_periodHelper = new TNReportingPeriodHelper(getOrganization(), m_context, reportPeriod, getBroker());
                m_month = m_periodHelper.getMonth().getCode();
                addParameter(INPUT_PERIOD, reportPeriod.getCode());
                break;
        }

        addParameter(ROOT_PARAM_SCHOOL_YEAR, m_context);
        addParameter(ROOT_PARAM_ORGANIZATION, getOrganization());

        String keyDelimiter = ((String) getParameter(INPUT_KEY_DELIMITER)).substring(0, 1);
        char listDelimiter = ((String) getParameter(INPUT_LIST_DELIMITER)).charAt(0);

        initGradeCriteria(keyDelimiter, listDelimiter);
        initCauseCriteria(keyDelimiter);
        initActionCriteria();
        initCounts();
        initReportsFormat();
    }

    /**
     * Increment count for all section on the table.
     *
     * Note: we only want to add the action that has the biggest sequence number for each incident
     * (instead of counting all actions).
     *
     * @param oldTempAction ConductAction
     * @param newAction return oldTempAction the temporary ConductAction
     * @param detailGrid ReportDataGrid
     * @return ConductAction
     */
    private ConductAction addAllSectionsToCounts(ConductAction oldTempAction,
                                                 ConductAction newAction,
                                                 ReportDataGrid detailGrid) {
        // We only want to add action with the biggest sequence number for each incident (instead of
        // counting all actions).
        if (oldTempAction != null && newAction != null && !StringUtils.isEmpty(oldTempAction.getIncidentOid())
                && oldTempAction.getIncidentOid().equals(newAction.getIncidentOid())) {
            // Check which has the bigger sequence number
            ReferenceCode newCode = m_actionCodeMap.get(newAction.getActionCode());
            ReferenceCode oldCode = m_actionCodeMap.get(oldTempAction.getActionCode());
            if (newCode.getSequenceNumber() > oldCode.getSequenceNumber()) {
                oldTempAction = newAction;
            }
        } else {
            // When the incident is changed (or the last one on the list), then add the action to
            // the counts
            if (oldTempAction != null && oldTempAction.getStudent() != null) {

                SisStudent student = oldTempAction.getStudent();
                SisPerson person = student.getPerson();

                String columnKey = getColumnKey(person);
                String actionKey = getActionKey(oldTempAction);
                // If student counts at least for one section (condition (columnKey != null &&
                // actionKey != null) for
                // "section3Table"), show him in the detail report.
                if (columnKey != null && actionKey != null) {
                    detailGrid.append();
                    detailGrid.set(DETAIL_REPORT_FIELD_ACTION_CODE, oldTempAction.getActionCode());
                    detailGrid.set(DETAIL_REPORT_FIELD_ACTION_END_DATE, oldTempAction.getActionEndDate());
                    detailGrid.set(DETAIL_REPORT_FIELD_ACTION_START_DATE, oldTempAction.getActionStartDate());
                    detailGrid.set(DETAIL_REPORT_FIELD_CAUSE_KEY, getCauseKey(oldTempAction));
                    detailGrid.set(DETAIL_REPORT_FIELD_GENDER, oldTempAction.getStudent().getPerson().getGenderCode());

                    PlainDate actEndDate =
                            oldTempAction.getActionEndDate() != null ? oldTempAction.getActionEndDate() : m_endDate;
                    detailGrid.set(DETAIL_REPORT_FIELD_GRADE_LEVEL, m_data
                            .getGradeLevelByDates(student, oldTempAction.getActionStartDate(), actEndDate).getCode());
                    detailGrid.set(DETAIL_REPORT_FIELD_HISPANIC_INDICATOR,
                            Boolean.valueOf(oldTempAction.getStudent().getPerson().getHispanicLatinoIndicator()));
                    detailGrid.set(DETAIL_REPORT_FIELD_INCIDENT_DATE, oldTempAction.getIncident().getIncidentDate());
                    detailGrid.set(DETAIL_REPORT_FIELD_INCIDENT_ID, oldTempAction.getIncident().getIncidentId());
                    detailGrid.set(DETAIL_REPORT_FIELD_RACE_VIEW, oldTempAction.getStudent().getPerson().getRaceView());
                    detailGrid.set(DETAIL_REPORT_FIELD_SCHOOL_NAME, oldTempAction.getSchool().getName());
                    detailGrid.set(DETAIL_REPORT_FIELD_STATE_ACTION_CODE,
                            m_criteriaActionInverse.get(oldTempAction.getActionCode()));
                    detailGrid.set(DETAIL_REPORT_FIELD_STUDENT_NAME, oldTempAction.getStudent().getNameView());
                    detailGrid.set(DETAIL_REPORT_FIELD_LOCAL_ID, oldTempAction.getStudent().getLocalId());
                    detailGrid.set(DETAIL_REPORT_FIELD_STATE_ID,
                            oldTempAction.getStudent().getFieldValueByAlias(ALIAS_EIS_STATE_ID));
                }

                addToCounts(oldTempAction, m_schoolSection1Tables, m_schoolSection2Tables, m_schoolSection3Table);

                if (m_includeDistrictSummary.booleanValue()) {
                    addToCounts(oldTempAction, m_districtSection1Tables, m_districtSection2Tables,
                            m_districtSection3Table);
                }
            }

            oldTempAction = newAction;
        }

        return oldTempAction;
    }

    /**
     * Increment count for suitable table.
     *
     * @param action ConductAction
     * @param section1Tables Map
     * @param section2Tables Map
     * @param section3Table IncrementableTable
     */
    private void addToCounts(ConductAction action,
                             Map section1Tables,
                             Map section2Tables,
                             IncrementableTable section3Table) {
        SisStudent student = action.getStudent();
        SisPerson person = student.getPerson();

        String columnKey = getColumnKey(person);
        String actionKey = getActionKey(action);
        if (columnKey == null || actionKey == null) {
            return;
        }
        section3Table.incValue(actionKey, columnKey);

        PlainDate actEndDate = action.getActionEndDate() != null ? action.getActionEndDate() : m_endDate;
        String gradeKey =
                getGradeKey(m_data.getGradeLevelByDates(student, action.getActionStartDate(), actEndDate).getCode());
        if (gradeKey == null) {
            return;
        }
        IncrementableTable section1Table = (IncrementableTable) section1Tables.get(actionKey);
        section1Table.incValue(gradeKey, columnKey);

        String causeKey = getCauseKey(action);
        if (causeKey == null) {
            return;
        }
        IncrementableTable section2Table = (IncrementableTable) section2Tables.get(actionKey);
        section2Table.incValue(causeKey, columnKey);


    }

    /**
     * Fill the Grid by subreport parameters, subreport fields and report formats.
     *
     * @param grid ReportDataGrid
     * @param currentSchool SisSchool
     * @param orgSection1Tables Map
     * @param orgSection2Tables Map
     * @param orgSection3Table IncrementableTable
     * @param month String
     * @param periodBeginDate PlainDate
     * @param periodEndDate PlainDate
     */
    private void fillGrid(ReportDataGrid grid,
                          SisSchool currentSchool,
                          Map orgSection1Tables,
                          Map orgSection2Tables,
                          IncrementableTable orgSection3Table,
                          String month,
                          PlainDate periodBeginDate,
                          PlainDate periodEndDate) {
        // Section 1 tables
        for (String key : (Set<String>) m_criteriaActionOrder) {
            ReportDataGrid table = ((IncrementableTable) orgSection1Tables.get(key)).getGrid();
            fillGridRow(grid, currentSchool, key, table, m_subreport1, month, periodBeginDate, periodEndDate);
        }
        // Section 2 tables
        for (String key : (Set<String>) m_criteriaActionOrder) {
            ReportDataGrid table = ((IncrementableTable) orgSection2Tables.get(key)).getGrid();
            fillGridRow(grid, currentSchool, key, table, m_subreport2, month, periodBeginDate, periodEndDate);
        }
        // Section 3 table
        ReportDataGrid table = orgSection3Table.getGrid();
        fillGridRow(grid, currentSchool, "", table, m_subreport3, month, periodBeginDate, periodEndDate);
    }

    /**
     * Fill grid row.
     *
     * @param grid ReportDataGrid
     * @param currentSchool SisSchool
     * @param key String
     * @param table ReportDataGrid
     * @param subreport Report
     * @param month String
     * @param periodStartDate PlainDate
     * @param periodEndDate PlainDate
     */
    private void fillGridRow(ReportDataGrid grid,
                             SisSchool currentSchool,
                             String key,
                             ReportDataGrid table,
                             Report subreport,
                             String month,
                             PlainDate periodStartDate,
                             PlainDate periodEndDate) {
        grid.append();
        grid.set(ROOT_FIELD_SCHOOL, currentSchool);
        grid.set(ROOT_FIELD_ACTION_TYPE, key);
        grid.set(ROOT_FIELD_DATA_SOURCE, table);
        grid.set(ROOT_FIELD_PERIOD_MONTH, month);
        grid.set(ROOT_FIELD_PERIOD_START, periodStartDate);
        grid.set(ROOT_FIELD_PERIOD_END, periodEndDate);

        ByteArrayInputStream reportFormat = new ByteArrayInputStream(subreport.getCompiledFormat());
        grid.set(ROOT_FIELD_REPORT_FORMAT, reportFormat);
    }

    /**
     * Generate all possible combinations for column key.
     *
     * @return Set
     */
    private Set generateColumns() {
        String rtbOid = (String) getParameter(INPUT_RACE_REF_TABLE_OID);
        Criteria raceCriteria = new X2Criteria();
        raceCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        ReportQueryByCriteria raceQuery = new ReportQueryByCriteria(ReferenceCode.class, raceCriteria);
        Collection raceRefCodes = getBroker().getCollectionByQuery(raceQuery);

        List raceCodes = new ArrayList();
        raceCodes.add(RACE_HISPANIC);
        raceCodes.addAll(CollectionUtils.getPropertyCollection(raceRefCodes, ReferenceCode.COL_CODE));
        raceCodes.add(RACE_TWO_OR_MORE);

        Set columns = new HashSet();
        String maleCode = (String) getParameter(INPUT_MALE_CODE);
        String femaleCode = (String) getParameter(INPUT_FEMALE_CODE);

        for (String genderCode : new String[] {maleCode, femaleCode}) {
            for (String raceCode : (List<String>) raceCodes) {
                columns.add(getColumnKey(genderCode, raceCode));
            }
        }

        return columns;
    }

    /**
     * Gets the action key.
     *
     * @param action ConductAction
     * @return String
     */
    private String getActionKey(ConductAction action) {
        String actionCode = action.getActionCode();
        return (String) m_criteriaActionInverse.get(actionCode);
    }

    /**
     * Generate cause row-key for given action.
     *
     * @param action ConductAction
     * @return String
     */
    private String getCauseKey(ConductAction action) {
        Collection<ConductOffense> offenses = action.getIncident().getConductOffenses();

        // If the primary code is not null, always use that one. Otherwise, try to use the
        // additional code
        if (action.getIncident().getIncidentCode() != null) {
            return (String) m_criteriaCauseInverse.get(action.getIncident().getIncidentCode());
        }

        // Otherwise, look up all key for cnoIncident and select the earlier key
        Collection causeKeys = new ArrayList(offenses.size());
        for (ConductOffense offense : offenses) {
            String causeKey = (String) m_criteriaCauseInverse.get(offense.getIncidentCode());
            causeKeys.add(causeKey);
        }

        // select earlier key
        for (String key : (Set<String>) m_criteriaCauseOrder) {
            if (causeKeys.contains(key)) {
                return key;
            }
        }

        return null;
    }

    /**
     * Generate column key for given person.
     *
     * @param person SisPerson
     * @return String
     */
    private String getColumnKey(SisPerson person) {
        if (person == null || person.getGenderCode() == null) {
            return null;
        }
        String genderCode = person.getGenderCode();

        if (person.getHispanicLatinoIndicator()) {
            return getColumnKey(genderCode, RACE_HISPANIC);
        }
        Collection<Race> races = person.getRaces();
        if (races != null && races.size() == 1) {
            String raceCode = races.iterator().next().getRaceCode();
            return getColumnKey(genderCode, raceCode);
        } else if (races != null && races.size() > 1) {
            return getColumnKey(genderCode, RACE_TWO_OR_MORE);
        }
        return null;
    }

    /**
     * Rule to calculate column parameter.
     *
     * @param genderCode String
     * @param raceCode String
     * @return String
     */
    private String getColumnKey(String genderCode, String raceCode) {
        return genderCode + "_" + raceCode;
    }

    /**
     * Gets the grade key.
     *
     * @param gradeLevel String
     * @return String
     */
    private String getGradeKey(String gradeLevel) {
        if (gradeLevel == null) {
            return null;
        }
        return (String) m_criteriaGradeInverse.get(gradeLevel);
    }


    /**
     * Return collection of schools.
     *
     * @return Collection<SisSchool> collection of schools
     */
    private Collection<SisSchool> getSchools() {
        Collection<SisSchool> schools = null;
        Object objIsAllSchools = getParameter(INPUT_ALL_SCHOOLS);
        boolean isAllSchools = objIsAllSchools == null ? false : ((Boolean) objIsAllSchools).booleanValue();
        if (isAllSchools) {
            X2Criteria schoolCriteria = new X2Criteria();

            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            DataDictionaryField aliasSklStateIDField =
                    m_dictionary.findDataDictionaryFieldByAlias(ALIAS_SKL_STATE_ID);
            schoolCriteria.addNotEmpty(aliasSklStateIDField.getJavaName(), getBroker().getPersistenceKey());
            schoolCriteria.addNotEqualTo(aliasSklStateIDField.getJavaName(), STAFF_SCHOOL_CODE);

            QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
            schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
            schools = getBroker().getCollectionByQuery(schoolQuery);
        } else {
            schools = new LinkedList();
            Object objSchools = getParameter(INPUT_SCHOOLS);
            String schoolOids = objSchools == null ? "" : (String) objSchools;
            if (!StringUtils.isEmpty(schoolOids)) {
                List<String> oids = Arrays.asList(schoolOids.split(","));
                X2Criteria schoolCriteria = new X2Criteria();

                schoolCriteria.addIn(X2BaseBean.COL_OID, oids);

                QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);
                schoolQuery.addOrderByAscending(SisSchool.COL_NAME);
                schools = getBroker().getCollectionByQuery(schoolQuery);
            }
        }

        return schools;
    }

    /**
     * Load the action codes from the 'rtbCndAction' reference Table.
     */
    private void loadActionCodes() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_TABLE_OID_CONDUCT_ACTION);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_actionCodeMap = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, INITIAL_ACTION_SIZE);
    }

    /**
     * Init m_criteriaActionOrder and m_criteriaActionInverse.
     */
    private void initActionCriteria() {
        m_criteriaActionOrder = new LinkedHashSet();
        m_criteriaActionOrder.add(getParameter(INPUT_SUSPENSIONS_CODES));
        m_criteriaActionOrder.add(getParameter(INPUT_EXPLUSION_CODES));
        m_criteriaActionOrder.add(getParameter(INPUT_LONG_TERM_CHANGES_CODES));
        m_criteriaActionOrder.add(getParameter(INPUT_IN_SCHOOL_SUSPENSIONS_CODES));

        Criteria actionCriteria = new X2Criteria();
        String rtbOid = (String) getParameter(INPUT_ACTION_REF_TABLE_OID);
        actionCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        actionCriteria.addIn(ReferenceCode.COL_STATE_CODE, m_criteriaActionOrder);

        ReportQueryByCriteria actionQuery = new ReportQueryByCriteria(ReferenceCode.class, actionCriteria);
        Map actionRefCodesByStateCode =
                getBroker().getGroupedCollectionByQuery(actionQuery, ReferenceCode.COL_STATE_CODE,
                        INITIAL_ACTION_SIZE);
        m_criteriaActionInverse = new LinkedHashMap();

        for (String stateCode : (Set<String>) m_criteriaActionOrder) {
            Collection refCodes = (Collection) actionRefCodesByStateCode.get(stateCode);
            Collection codes = CollectionUtils.getPropertyCollection(refCodes, ReferenceCode.COL_CODE);

            for (String code : (Collection<String>) codes) {
                m_criteriaActionInverse.put(code, stateCode);
            }
        }
    }

    /**
     * Initialize m_criteriaCauseInverse and m_criteriaCauseOrder.
     *
     * @param keyDelimiter String
     */
    private void initCauseCriteria(String keyDelimiter) {
        Criteria causeCriteria = new X2Criteria();
        String rtbOid = (String) getParameter(INPUT_CAUSE_REF_TABLE_OID);
        causeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);

        ReportQueryByCriteria causeQuery = new ReportQueryByCriteria(ReferenceCode.class, causeCriteria);
        Map causeRefCodes = getBroker().getGroupedCollectionByQuery(causeQuery, ReferenceCode.COL_STATE_CODE,
                INITIAL_CAUSE_SIZE);
        m_criteriaCauseInverse = new LinkedHashMap();
        m_criteriaCauseOrder = new LinkedHashSet();
        int i = 1;
        String causeInput = null;
        while ((causeInput = (String) getParameter(INPUT_CAUSE_CRITERIA + Integer.toString(i++))) != null) {
            int pos = causeInput.indexOf(keyDelimiter);
            if (pos < 1) {
                continue;
            }
            String key = causeInput.substring(0, pos);
            String codeState = causeInput.substring(pos + 1);

            m_criteriaCauseOrder.add(key);
            Collection refCodes = (Collection) causeRefCodes.get(codeState);
            if (refCodes != null) {
                Set<String> codes = CollectionUtils.getPropertySet(refCodes, ReferenceCode.COL_CODE);
                for (String code : codes) {
                    m_criteriaCauseInverse.put(code, key);
                }
            }

        }
    }

    /**
     * Initializes tables for calculation.
     */
    private void initCounts() {
        m_schoolSection1Tables = new LinkedHashMap();
        m_schoolSection2Tables = new LinkedHashMap();
        Set columns = generateColumns();
        for (String key : (Set<String>) m_criteriaActionOrder) {
            m_schoolSection1Tables.put(key, new IncrementableTable(m_criteriaGradeOrder, columns, SUB_FIELD_ROW_NAME));
            m_schoolSection2Tables.put(key, new IncrementableTable(m_criteriaCauseOrder, columns, SUB_FIELD_ROW_NAME));
        }
        m_schoolSection3Table = new IncrementableTable(m_criteriaActionOrder, columns, SUB_FIELD_ROW_NAME);

        if (m_includeDistrictSummary.booleanValue()) {
            m_districtSection1Tables = new LinkedHashMap();
            m_districtSection2Tables = new LinkedHashMap();
            for (String key : (Set<String>) m_criteriaActionOrder) {
                m_districtSection1Tables.put(key,
                        new IncrementableTable(m_criteriaGradeOrder, columns, SUB_FIELD_ROW_NAME));
                m_districtSection2Tables.put(key,
                        new IncrementableTable(m_criteriaCauseOrder, columns, SUB_FIELD_ROW_NAME));
            }
            m_districtSection3Table = new IncrementableTable(m_criteriaActionOrder, columns, SUB_FIELD_ROW_NAME);
        }
    }

    /**
     * Initializes m_criteriaGradeInverse and m_criteriaGradeOrder.
     *
     * @param keyDelimiter String
     * @param listDelimiter char
     */
    private void initGradeCriteria(String keyDelimiter, char listDelimiter) {
        m_criteriaGradeInverse = new LinkedHashMap();
        m_criteriaGradeOrder = new LinkedHashSet();
        int i = 1;
        String gradeCriteriaInput = null;
        while ((gradeCriteriaInput = (String) getParameter(INPUT_GRADE_CRITERIA + Integer.toString(i++))) != null) {
            int pos = gradeCriteriaInput.indexOf(keyDelimiter);
            if (pos > 0) {
                String key = gradeCriteriaInput.substring(0, pos);
                String undelimitedCodes = gradeCriteriaInput.substring(pos + 1);

                List codes = StringUtils.convertDelimitedStringToList(undelimitedCodes, listDelimiter, true);

                m_criteriaGradeOrder.add(key);
                for (Object code : (List<String>) codes) {
                    m_criteriaGradeInverse.put(code, key);
                }
            }
        }
    }

    /**
     * Initialize report formats.
     */
    private void initReportsFormat() {
        m_subreport1 = ReportUtils.getReport((String) getParameter(INPUT_SUBREPORT_ID_SECTION1), getBroker());
        m_subreport2 = ReportUtils.getReport((String) getParameter(INPUT_SUBREPORT_ID_SECTION2), getBroker());
        m_subreport3 = ReportUtils.getReport((String) getParameter(INPUT_SUBREPORT_ID_SECTION3), getBroker());

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
     * Resets counts for all tables.
     */
    private void resetCounts() {
        for (String key : (Set<String>) m_criteriaActionOrder) {
            ((IncrementableTable) m_schoolSection1Tables.get(key)).resetCounts();
            ((IncrementableTable) m_schoolSection2Tables.get(key)).resetCounts();
        }
        m_schoolSection3Table.resetCounts();
    }

}
