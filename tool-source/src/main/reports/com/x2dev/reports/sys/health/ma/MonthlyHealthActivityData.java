/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.sys.health.ma;

import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STAFF_ACTIVE_CODE;
import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Contact;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.HealthInventoryTransaction;
import com.x2dev.sis.model.beans.HealthJob;
import com.x2dev.sis.model.beans.HealthLog;
import com.x2dev.sis.model.beans.HealthLogAction;
import com.x2dev.sis.model.beans.HealthLogComplaint;
import com.x2dev.sis.model.beans.HealthLogTreatment;
import com.x2dev.sis.model.beans.HealthMedicationOrder;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the "Monthly Health Activity" report.
 *
 * @author X2 Development Corporation
 */
public class MonthlyHealthActivityData extends ReportJavaSourceNet {

    /**
     * State Report Data class for gathering data, using StudentHistoryHelper, calculating
     * enrollment history.
     * This export should report a row for each student/school combination.
     * It should report positive attendance, and report positive codes for each day in membership.
     * Days not in membership should be reported as empty.
     *
     * @author Follett Development Corporation
     */
    class AttendanceStatistics extends StateReportData {

        /*
         * Instance variables.
         */
        private StudentHistoryHelper m_helper;

        /**
         * Initialize the export.
         * Set up the student history helper.
         */
        @Override
        public void initialize() {
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_SECONDARY, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_dateStart);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_dateEnd);
            if (isSchoolContext()) {
                m_helper.getStudentCriteria().addEqualTo(SisStudent.COL_SCHOOL_OID, getSchool().getOid());
            }
            setQuery(m_helper.getStudentQuery(true));
        }
    }

    // Input parameters
    private static final String ACTIVE_STUDENTS_ONLY_PARAM = "activeOnly";
    private static final String END_DATE_PARAM = "endDate";
    private static final String PARAM_SCHOOL = "school";
    private static final String QUERY_BY_PARAM = "queryBy";
    private static final String QUERY_STRING_PARAM = "queryString";
    private static final String STAFF_QUERY_BY_PARAM = "staffQueryBy";
    private static final String START_DATE_PARAM = "startDate";

    // Report parameters
    private static final String DISTRICT_CONTEXT_PARAM = "context";
    private static final String SKL_NAME_PARAM = "sklName";
    private static final String USER_PARAM = "user";

    // Subreport constants
    private static final String FIELD_DATA = "data";
    private static final String FIELD_FORMAT = "format";
    private static final String SUBREPORT_ID_PREFIX = "SYS-HTH-MA-003-SUB";
    private static final String FIELD_ENCOUNTERS_INPERSON = "encounters-inperson";
    private static final String FIELD_ENCOUNTERS_REMOTE = "encounters-remote";

    // Input parameter value constants
    private static final int STAFF_ACTIVE_VALUE = 2;
    private static final int STAFF_CURRENT_VALUE = 1;
    private static final int STAFF_NONE_VALUE = 3;

    private static final int STUDENT_CURRENT_VALUE = 1;
    private static final int STUDENT_HOMEROOM_VALUE = 3;
    private static final int STUDENT_NONE_VALUE = 4;
    private static final int STUDENT_YOG_VALUE = 2;

    // Type constants
    private static final String CONTACT_SUFFIX = ".cnt";
    private static final String STAFF_SUFFIX = ".stf";
    private static final int STAFF_TYPE = 1;
    private static final String STUDENT_SUFFIX = ".std";
    private static final int STUDENT_TYPE = 0;
    private static final String VISIT_TYPE_VIRTUAL = "Virtual";
    private static final String DISPOSITION_PREFIX = "8.";

    // Other constants
    private static final int PAGES = 4;
    private static final String REPORT_LOCATION = "report-location";
    private static final String SUFFIX_PRN_ADMIN_SCH = "_prn_admin_sch";
    private static final String SUFFIX_PRN_ADMIN_SCH_NON = "_prn_admin_sch_non";
    private static final String SUFFIX_PRN_SCH = "_prn_sch";
    private static final String SUFFIX_PRN_SCH_NON = "_prn_sch_non";

    /**
     * Members
     */
    private Map<String, List<String>> m_codesByLocation;
    private Map<String, Object> m_counts;
    private PlainDate m_dateEnd;
    private PlainDate m_dateStart;
    private Map<String, List<String>> m_locationsByCode;
    private Collection<String> m_referenceTableOids;
    private AttendanceStatistics m_statistics;
    private Collection<String> m_usedCodes;
    private Collection<String> m_usedActionCodes;
    private int m_virtualCount = 0;

    /**
     * Criteria
     */
    private X2Criteria m_actionCriteria;
    private X2Criteria m_complaintCriteria;
    private X2Criteria m_studentCriteria;
    private X2Criteria m_treatmentCriteria;
    private X2Criteria m_visitCriteria;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_dateStart = (PlainDate) getParameter(START_DATE_PARAM);
        m_dateEnd = (PlainDate) getParameter(END_DATE_PARAM);
        /*
         * Build the health criteria based on user input
         */
        if (m_dateStart != null && m_dateEnd != null) {
            int queryStudentByParam = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            if (queryStudentByParam != STUDENT_NONE_VALUE) {
                resetCriteria();
                addGeneralCriteria(m_dateStart, m_dateEnd, STUDENT_TYPE);
                addStudentCriteria(queryStudentByParam);
                executeQueries();
                tallyCodeUsage(STUDENT_TYPE);
            }

            int queryStaffByParam = ((Integer) getParameter(STAFF_QUERY_BY_PARAM)).intValue();
            if (queryStaffByParam != STAFF_NONE_VALUE) {
                resetCriteria();
                addGeneralCriteria(m_dateStart, m_dateEnd, STAFF_TYPE);
                addStaffCriteria(queryStaffByParam);
                executeQueries();
                tallyCodeUsage(STAFF_TYPE);
            }
            resetCriteria();
            addGeneralCriteria(m_dateStart, m_dateEnd, -1);
            addContactCriteria();
            executeQueries();
            tallyCodeUsage(-1);
        }
        /*
         * Append the counts to the grid and set the grid for each page.
         */
        ReportDataGrid grid = new ReportDataGrid(100);
        grid.append(m_counts);
        populateMedicationAdminGrid(grid);
        populateMedicationOrderGrid(grid);
        grid.beforeTop();

        ReportDataGrid outerGrid = new ReportDataGrid();
        for (int i = 1; i <= PAGES; i++) {
            outerGrid.append();
            outerGrid.set(FIELD_DATA, grid);
            Report report = ReportUtils.getReport(SUBREPORT_ID_PREFIX + String.valueOf(i), getBroker());
            if (report != null) {
                outerGrid.set(FIELD_FORMAT, new ByteArrayInputStream(report.getCompiledFormat()));
            }
        }
        CalendarManager manager = new CalendarManager(getBroker());
        addParameter(DISTRICT_CONTEXT_PARAM, manager.getDistrictContext(m_dateEnd, getOrganization().getOid()));
        addParameter(USER_PARAM, getUser());
        if (isSchoolContext()) {
            addParameter(SKL_NAME_PARAM, getSchool().getName());
        }
        outerGrid.beforeTop();
        return outerGrid;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        /*
         * Initialize the codes and counts maps.
         */
        m_codesByLocation = new HashMap<String, List<String>>(4096);
        m_locationsByCode = new HashMap<String, List<String>>(4096);
        m_counts = new HashMap<String, Object>(4096);
        /*
         * Load the reference table OIDs into a collection for easy access by the Reference Code
         * query.
         */
        m_referenceTableOids = new ArrayList<String>(5);
        m_referenceTableOids.add("rtbHthActCds");
        m_referenceTableOids.add("rtbHthCompCds");
        m_referenceTableOids.add("rtbHthTreatCds");
        m_referenceTableOids.add("rtbVisitTypes");
        m_referenceTableOids.add("rtbHthMedType");

        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField hlgPrimActionCode =
                dataDictionary.findDataDictionaryField(HealthLog.class.getName(), HealthLog.COL_PRIMARY_ACTION_CODE);
        if (hlgPrimActionCode != null && hlgPrimActionCode.hasReferenceTable()) {
            m_referenceTableOids.add(hlgPrimActionCode.getReferenceTableOid());
        }
        loadReferenceCodes();
        loadStudentAttendanceStatistics();
    }

    /**
     * Adds general criteria to all defined queries, including person type, date constraints, and
     * school context
     *
     * @param startDate the PlainDate start date constraint specified by the user
     * @param endDate the PlainDate end date constraint specified by the user
     * @param personType the person type indicator (STUDENT_TYPE or STAFF_TYPE)
     */
    private void addGeneralCriteria(PlainDate startDate, PlainDate endDate, int personType) {
        if (personType == STAFF_TYPE) {
            addPersonIndicatorCriteria(Person.COL_STAFF_INDICATOR);
        } else if (personType == STUDENT_TYPE) {
            addPersonIndicatorCriteria(Person.COL_STUDENT_INDICATOR);
        } else {
            addPersonIndicatorCriteria(Person.COL_CONTACT_INDICATOR);
        }
        m_actionCriteria.addGreaterOrEqualThan(HealthLogAction.REL_HEALTH_LOG + PATH_DELIMITER + HealthLog.COL_DATE,
                startDate);
        m_actionCriteria.addLessOrEqualThan(HealthLogAction.REL_HEALTH_LOG + PATH_DELIMITER + HealthLog.COL_DATE,
                endDate);
        m_actionCriteria.addNotEmpty(HealthLogAction.COL_ACTION_CODE, getBroker().getPersistenceKey());
        m_actionCriteria.addEqualTo(HealthLogAction.COL_PRIMARY_INDICATOR, Boolean.TRUE);

        m_complaintCriteria.addGreaterOrEqualThan(
                HealthLogComplaint.REL_HEALTH_LOG + PATH_DELIMITER + HealthLog.COL_DATE, startDate);
        m_complaintCriteria.addLessOrEqualThan(HealthLogComplaint.REL_HEALTH_LOG + PATH_DELIMITER + HealthLog.COL_DATE,
                endDate);

        m_treatmentCriteria.addGreaterOrEqualThan(
                HealthLogTreatment.REL_HEALTH_LOG + PATH_DELIMITER + HealthLog.COL_DATE, startDate);
        m_treatmentCriteria.addLessOrEqualThan(HealthLogTreatment.REL_HEALTH_LOG + PATH_DELIMITER + HealthLog.COL_DATE,
                endDate);

        m_visitCriteria.addGreaterOrEqualThan(HealthLog.COL_DATE, startDate);
        m_visitCriteria.addLessOrEqualThan(HealthLog.COL_DATE, endDate);

        if (isSchoolContext()) {
            m_actionCriteria.addEqualTo(HealthLogAction.REL_HEALTH_LOG + PATH_DELIMITER +
                    HealthLog.COL_SCHOOL_OID, getSchool().getOid());
            m_complaintCriteria.addEqualTo(HealthLogComplaint.REL_HEALTH_LOG + PATH_DELIMITER +
                    HealthLog.COL_SCHOOL_OID, getSchool().getOid());
            m_treatmentCriteria.addEqualTo(HealthLogTreatment.REL_HEALTH_LOG + PATH_DELIMITER +
                    HealthLog.COL_SCHOOL_OID, getSchool().getOid());
            m_visitCriteria.addEqualTo(HealthLog.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            m_actionCriteria.addAndCriteria(getOrganizationCriteria(HealthLogAction.class));
            m_complaintCriteria.addAndCriteria(getOrganizationCriteria(HealthLogComplaint.class));
            m_treatmentCriteria.addAndCriteria(getOrganizationCriteria(HealthLogTreatment.class));
            m_visitCriteria.addAndCriteria(getOrganizationCriteria(HealthLog.class));
        }
    }

    /**
     * Adds the person type to the criteria for actions, complaints, treatments, and visit type
     * queries.
     *
     * @param personIndicatorAttribute the attribute on the Person table indicating the person's
     *        type
     */
    private void addPersonIndicatorCriteria(String personIndicatorAttribute) {
        m_actionCriteria.addEqualTo(HealthLogAction.REL_HEALTH_LOG + PATH_DELIMITER +
                HealthLog.REL_PERSON + PATH_DELIMITER + personIndicatorAttribute, Boolean.TRUE);
        m_complaintCriteria.addEqualTo(HealthLogComplaint.REL_HEALTH_LOG + PATH_DELIMITER +
                HealthLog.REL_PERSON + PATH_DELIMITER + personIndicatorAttribute, Boolean.TRUE);
        m_treatmentCriteria.addEqualTo(HealthLogTreatment.REL_HEALTH_LOG + PATH_DELIMITER +
                HealthLog.REL_PERSON + PATH_DELIMITER + personIndicatorAttribute, Boolean.TRUE);
        m_visitCriteria.addEqualTo(HealthLog.REL_PERSON + PATH_DELIMITER +
                personIndicatorAttribute, Boolean.TRUE);
    }

    /**
     * Adds a person-constrained subquery to all existing queries (medication query gets set only
     * if the person type is student).
     *
     * @param subQuery the SubQuery for which to add person constraints
     * @param personType the person type indicator (STUDENT_TYPE or STAFF_TYPE)
     */
    private void addPersonOidCriteria(SubQuery subQuery) {
        m_actionCriteria.addIn(HealthLogAction.REL_HEALTH_LOG + PATH_DELIMITER +
                HealthLog.COL_PERSON_OID, subQuery);
        m_complaintCriteria.addIn(HealthLogComplaint.REL_HEALTH_LOG + PATH_DELIMITER +
                HealthLog.COL_PERSON_OID, subQuery);
        m_treatmentCriteria.addIn(HealthLogTreatment.REL_HEALTH_LOG + PATH_DELIMITER +
                HealthLog.COL_PERSON_OID, subQuery);
        m_visitCriteria.addIn(HealthLog.COL_PERSON_OID, subQuery);
    }

    /**
     * Adds staff-specific query constraints.
     *
     * @param queryStaffByParam the staff type specified by the user in the input
     */
    private void addContactCriteria() {
        Criteria cntCriteria = new Criteria();
        SubQuery cntSubQuery = new SubQuery(Contact.class, Contact.COL_PERSON_OID, cntCriteria);
        addPersonOidCriteria(cntSubQuery);
    }

    /**
     * Adds staff-specific query constraints.
     *
     * @param queryStaffByParam the staff type specified by the user in the input
     */
    private void addStaffCriteria(int queryStaffByParam) {
        switch (queryStaffByParam) {
            case STAFF_CURRENT_VALUE: // Current Selection
                SubQuery subQuery = new SubQuery(HealthLog.class, X2BaseBean.COL_OID, getCurrentCriteria());
                m_actionCriteria.addIn(HealthLogAction.COL_HEALTH_LOG_OID, subQuery);
                m_complaintCriteria.addIn(HealthLogComplaint.COL_HEALTH_LOG_OID, subQuery);
                m_treatmentCriteria.addIn(HealthLogTreatment.COL_HEALTH_LOG_OID, subQuery);
                m_visitCriteria.addIn(X2BaseBean.COL_OID, subQuery);
                break;

            case STAFF_ACTIVE_VALUE: // All Active Staff
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STAFF_ACTIVE_CODE);

                Criteria activeCriteria = new Criteria();
                activeCriteria.addEqualTo(SisStaff.COL_STATUS, activeCode);

                SubQuery activeSubQuery = new SubQuery(SisStaff.class, SisStaff.COL_PERSON_OID, activeCriteria);
                addPersonOidCriteria(activeSubQuery);
                break;

            case STAFF_NONE_VALUE: // None (should never get to here)
                m_actionCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_complaintCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_treatmentCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_visitCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                break;

            default: // All Staff
                break;
        }
    }

    /**
     * Adds student-specific query constraints. Also sets the active-only constraint, if specified
     * by the user.
     *
     * @param queryByParam the student-specific indicator specified by the user in the input
     */
    private void addStudentCriteria(int queryByParam) {
        m_studentCriteria = m_statistics.m_helper.getStudentCriteria();
        if (((Boolean) getParameter(ACTIVE_STUDENTS_ONLY_PARAM)).booleanValue()) {
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STUDENT_ACTIVE_CODE);

            Criteria activeCriteria = new Criteria();
            activeCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
            m_studentCriteria.addAndCriteria(activeCriteria);

            SubQuery activeSubQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, activeCriteria);
            addPersonOidCriteria(activeSubQuery);
        }
        switch (queryByParam) {
            case STUDENT_CURRENT_VALUE: // Current Selection
                X2Criteria hlgCriteria = getCurrentCriteria();
                QueryByCriteria hlgQuery = new QueryByCriteria(HealthLog.class, hlgCriteria);
                m_studentCriteria.addIn(SisStudent.COL_PERSON_OID,
                        getBroker().getGroupedCollectionByQuery(hlgQuery, HealthLog.COL_PERSON_OID).keySet());
                SubQuery subQuery = new SubQuery(HealthLog.class, X2BaseBean.COL_OID, hlgCriteria);
                m_actionCriteria.addIn(HealthLogAction.COL_HEALTH_LOG_OID, subQuery);
                m_complaintCriteria.addIn(HealthLogComplaint.COL_HEALTH_LOG_OID, subQuery);
                m_treatmentCriteria.addIn(HealthLogTreatment.COL_HEALTH_LOG_OID, subQuery);
                m_visitCriteria.addIn(X2BaseBean.COL_OID, subQuery);
                break;
            case STUDENT_YOG_VALUE: // YOG
                Criteria yogCriteria = new Criteria();
                yogCriteria.addEqualTo(Student.COL_YOG, getParameter(QUERY_STRING_PARAM));
                m_studentCriteria.addAndCriteria(yogCriteria);
                SubQuery yogSubQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, yogCriteria);
                addPersonOidCriteria(yogSubQuery);
                break;
            case STUDENT_HOMEROOM_VALUE: // Homeroom
                Criteria homeroomCriteria = new Criteria();
                homeroomCriteria.addEqualTo(SisStudent.COL_HOMEROOM, getParameter(QUERY_STRING_PARAM));
                m_studentCriteria.addAndCriteria(homeroomCriteria);
                SubQuery homeroomSubQuery = new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, homeroomCriteria);
                addPersonOidCriteria(homeroomSubQuery);
                break;
            case STUDENT_NONE_VALUE: // None (should never get to here)
                m_studentCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_actionCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_complaintCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_treatmentCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                m_visitCriteria.addEmpty(X2BaseBean.COL_OID, getUser().getPersistenceKey());
                break;
            default: // All Students
                break;
        }
    }

    /**
     * Executes all defined queries
     *
     */
    private void executeQueries() {
        /*
         * Execute the Health Action sub query
         */
        SubQuery actionSubQuery =
                new SubQuery(HealthLogAction.class, HealthLogAction.COL_ACTION_CODE, m_actionCriteria);
        m_usedActionCodes.addAll(getBroker().getSubQueryCollectionByQuery(actionSubQuery));

        m_usedCodes.addAll(getBroker().getSubQueryCollectionByQuery(actionSubQuery));

        /*
         * Execute the Health Complaint sub query
         */
        SubQuery complaintSubQuery =
                new SubQuery(HealthLogComplaint.class, HealthLogComplaint.COL_COMPLAINT_CODE, m_complaintCriteria);
        m_usedCodes.addAll(getBroker().getSubQueryCollectionByQuery(complaintSubQuery));

        /*
         * Execute the Health Treatment sub query
         */
        SubQuery treatmentSubQuery =
                new SubQuery(HealthLogTreatment.class, HealthLogTreatment.COL_TREATMENT_CODE, m_treatmentCriteria);
        m_usedCodes.addAll(getBroker().getSubQueryCollectionByQuery(treatmentSubQuery));

        /*
         * Execute the Visit sub query
         */
        SubQuery visitSubQuery = new SubQuery(HealthLog.class, HealthLog.COL_VISIT_TYPE, m_visitCriteria);
        m_usedCodes.addAll(getBroker().getSubQueryCollectionByQuery(visitSubQuery));

        /*
         * Get the count of virtual health logs
         */
        m_visitCriteria.addEqualTo(HealthLog.COL_VISIT_TYPE, VISIT_TYPE_VIRTUAL);
        SubQuery virtualSubQuery = new SubQuery(HealthLog.class, HealthLog.COL_VISIT_TYPE, m_visitCriteria);
        m_virtualCount = getBroker().getCount(virtualSubQuery);
    }

    /**
     * Loads the reference codes into a map keyed to the codes' report location.
     */
    private void loadReferenceCodes() {
        DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField reportLocationField = dataDictionary.findDataDictionaryFieldByAlias(REPORT_LOCATION);
        if (reportLocationField != null) {
            String reportLocationAttribute = reportLocationField.getJavaName();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.FALSE);
            criteria.addIn(ReferenceCode.COL_REFERENCE_TABLE_OID, m_referenceTableOids);
            criteria.addNotEmpty(reportLocationAttribute, getBroker().getPersistenceKey());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ReferenceCode referenceCode = (ReferenceCode) iterator.next();

                    String code = referenceCode.getCode();
                    List<String> reportLocations = StringUtils.convertDelimitedStringToList(
                            (String) referenceCode.getFieldValueByBeanPath(reportLocationAttribute), ',');

                    for (String location : reportLocations) {
                        location = location.trim();

                        /*
                         * Load a map of location codes linked to a list of reference codes.
                         */
                        if (m_codesByLocation.keySet().contains(location)) {
                            List<String> codes = m_codesByLocation.get(location);
                            if (!codes.contains(code)) {
                                codes.add(code);
                                m_codesByLocation.put(location, codes);
                            }
                        } else {
                            List<String> codes = new ArrayList<String>();
                            codes.add(code);

                            m_codesByLocation.put(location, codes);
                        }

                        /*
                         * Load a map of reference codes linked to a list of location codes.
                         */
                        if (m_locationsByCode.keySet().contains(code)) {
                            List<String> locations = m_locationsByCode.get(code);
                            if (!locations.contains(location)) {
                                locations.add(location);
                                m_locationsByCode.put(code, locations);
                            }
                        } else {
                            List<String> locations = new ArrayList<String>();
                            locations.add(location);

                            m_locationsByCode.put(code, locations);
                        }
                    }
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Load Student Attendance Statistics.
     */
    private void loadStudentAttendanceStatistics() {
        Map<String, Object> parametersMap = new HashMap<String, Object>();

        m_statistics = new AttendanceStatistics();
        m_statistics.setBroker(getBroker());
        CalendarManager manager = new CalendarManager(getBroker());
        m_statistics.setCurrentContext(manager.getDistrictContext(m_dateEnd, getOrganization().getOid()));
        m_statistics.setPrivilegeSet(getPrivilegeSet());
        m_statistics.setOrganization(OrganizationManager.getRootOrganization(getOrganization()));
        Object school = getParameter(PARAM_SCHOOL);
        boolean isSchoolContext = false;
        if (school instanceof School) {

            isSchoolContext = true;
            m_statistics.setSchool((School) school);
        }
        m_statistics.setSchoolContext(isSchoolContext);


        m_statistics.setParameters(parametersMap);
        try {
            m_statistics.initializeExport();
        } catch (X2BaseException e) {
            e.printStackTrace();
        }
        m_statistics.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                m_dateStart);
        m_statistics.m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                m_dateEnd);
    }

    /**
     * Counts the medication orders administered.
     *
     * @param grid ReportDataGrid
     */
    private void populateMedicationAdminGrid(ReportDataGrid grid) {
        X2Criteria medicationAdminCriteria = new X2Criteria();
        medicationAdminCriteria.addGreaterOrEqualThan(
                HealthInventoryTransaction.REL_HEALTH_JOB + PATH_DELIMITER + HealthJob.COL_DATE,
                m_dateStart);
        medicationAdminCriteria.addLessOrEqualThan(
                HealthInventoryTransaction.REL_HEALTH_JOB + PATH_DELIMITER + HealthJob.COL_DATE,
                m_dateEnd);
        SubQuery personOidSubQuery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_studentCriteria);
        medicationAdminCriteria.addIn(HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                HealthMedicationOrder.REL_STUDENT + PATH_DELIMITER + Student.COL_PERSON_OID, personOidSubQuery);
        Map<String, Integer> adminPrnMap = new HashMap<>();
        String[] columns = {HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                HealthMedicationOrder.COL_MEDICATION_TYPE,
                HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                        HealthMedicationOrder.COL_AS_NEEDED_INDICATOR,
                HealthInventoryTransaction.REL_MEDICATION_ORDER + PATH_DELIMITER +
                        HealthMedicationOrder.COL_HMO_OID};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(HealthInventoryTransaction.class, columns, medicationAdminCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                boolean asNeededIndicator = "1".equals(record[1]);
                String suffix = null;
                if (asNeededIndicator) {
                    suffix = SUFFIX_PRN_ADMIN_SCH_NON;
                } else {
                    suffix = SUFFIX_PRN_ADMIN_SCH;
                }
                List<String> locations = m_locationsByCode.get(code);
                if (!CollectionUtils.isEmpty(locations)) {
                    for (String location : locations) {
                        String key = location + suffix;
                        int usage = adminPrnMap.containsKey(key) ? adminPrnMap.get(key).intValue() : 0;
                        adminPrnMap.put(key, new Integer(usage + 1));
                    }
                }
            }
        } finally {
            iterator.close();
        }
        if (!adminPrnMap.isEmpty()) {
            int totalAdminSch = 0;
            int totalAdminSchNon = 0;
            for (String type : adminPrnMap.keySet()) {
                if (type.endsWith(SUFFIX_PRN_ADMIN_SCH)) {
                    totalAdminSch += adminPrnMap.get(type).intValue();
                } else if (type.endsWith(SUFFIX_PRN_ADMIN_SCH_NON)) {
                    totalAdminSchNon += adminPrnMap.get(type).intValue();
                }
                grid.set(type, adminPrnMap.get(type));
            }
            grid.set("total" + SUFFIX_PRN_ADMIN_SCH, Integer.valueOf(totalAdminSch));
            grid.set("total" + SUFFIX_PRN_ADMIN_SCH_NON, Integer.valueOf(totalAdminSchNon));
        }
    }

    /**
     * Counts the medication order that are open.
     *
     * @param grid ReportDataGrid
     */
    private void populateMedicationOrderGrid(ReportDataGrid grid) {
        X2Criteria medicationCriteria = new X2Criteria();
        medicationCriteria.addLessOrEqualThan(HealthMedicationOrder.COL_START_DATE, m_dateEnd);
        medicationCriteria.addGreaterOrEqualThan(HealthMedicationOrder.COL_STOP_DATE,
                m_dateStart);
        medicationCriteria.addEmpty(HealthMedicationOrder.COL_HMO_OID, getBroker().getPersistenceKey());
        SubQuery personOidSubQuery =
                new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, m_studentCriteria);
        medicationCriteria.addIn(HealthMedicationOrder.REL_STUDENT + PATH_DELIMITER +
                SisStudent.COL_PERSON_OID, personOidSubQuery);
        Map<String, Integer> prnMap = new HashMap<>();

        String[] columns = {HealthMedicationOrder.COL_MEDICATION_TYPE,
                HealthMedicationOrder.COL_AS_NEEDED_INDICATOR};
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(HealthMedicationOrder.class, columns, medicationCriteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] record = (Object[]) iterator.next();
                String code = (String) record[0];
                boolean asNeededIndicator = "1".equals(record[1]);
                List<String> locations = m_locationsByCode.get(code);
                if (!CollectionUtils.isEmpty(locations)) {
                    for (String location : locations) {
                        String scheduledSuffix =
                                asNeededIndicator ? SUFFIX_PRN_SCH_NON : SUFFIX_PRN_SCH;
                        String key = location + scheduledSuffix;
                        int usage = prnMap.containsKey(key) ? prnMap.get(key).intValue() : 0;
                        prnMap.put(key, new Integer(usage + 1));
                    }
                }
            }
        } finally {
            iterator.close();
        }
        if (!prnMap.isEmpty()) {
            int totalSch = 0;
            int totalSchNon = 0;
            for (String type : prnMap.keySet()) {
                if (type.endsWith(SUFFIX_PRN_SCH)) {
                    totalSch += prnMap.get(type).intValue();
                } else if (type.endsWith(SUFFIX_PRN_SCH_NON)) {
                    totalSchNon += prnMap.get(type).intValue();
                }
                grid.set(type, prnMap.get(type));
            }
            grid.set("total" + SUFFIX_PRN_SCH, Integer.valueOf(totalSch));
            grid.set("total" + SUFFIX_PRN_SCH_NON, Integer.valueOf(totalSchNon));
        }
    }

    /**
     * Resets all query constraints.
     */
    private void resetCriteria() {
        m_actionCriteria = new X2Criteria();
        m_complaintCriteria = new X2Criteria();
        m_treatmentCriteria = new X2Criteria();
        m_visitCriteria = new X2Criteria();
        m_usedCodes = new ArrayList<String>();
        m_usedActionCodes = new ArrayList<String>();
    }

    /**
     * Tallies the codes encountered after executing the queries and sets the respective iReport
     * field values.
     *
     * @param personType the person type indicator (STUDENT_TYPE or STAFF_TYPE)
     */
    private void tallyCodeUsage(int personType) {
        /*
         * Determine the usage for each code
         */
        Map<String, Integer> codeUsage = tallyAllCodes(m_usedCodes);
        Map<String, Integer> actionCodeUsage = tallyAllCodes(m_usedActionCodes);

        /*
         * Perform count
         */
        String suffix = null;
        if (personType == STUDENT_TYPE) {
            suffix = STUDENT_SUFFIX;
        } else if (personType == STAFF_TYPE) {
            suffix = STAFF_SUFFIX;
        } else {
            suffix = CONTACT_SUFFIX;
        }
        int totalDisp = 0;
        for (String location : m_codesByLocation.keySet()) {
            int usage = 0;
            List<String> codes = m_codesByLocation.get(location);
            for (String code : codes) {
                if (location.startsWith(DISPOSITION_PREFIX)) {
                    if (actionCodeUsage.keySet().contains(code)) {
                        usage += actionCodeUsage.get(code).intValue();
                        totalDisp += actionCodeUsage.get(code).intValue();
                    }
                } else {
                    if (codeUsage.keySet().contains(code)) {
                        usage += codeUsage.get(code).intValue();
                    }
                }
            }
            int existingUsage = (m_counts.containsKey(location + suffix))
                    ? ((Integer) m_counts.get(location + suffix)).intValue()
                    : 0;
            m_counts.put(location + suffix, Integer.valueOf(usage + existingUsage));
        }
        m_counts.put(FIELD_ENCOUNTERS_REMOTE + suffix, Integer.valueOf(m_virtualCount));
        m_counts.put(FIELD_ENCOUNTERS_INPERSON + suffix, Integer.valueOf(totalDisp - m_virtualCount));
    }

    /**
     * Tally all codes.
     *
     * @param usedCodes Collection<String>
     * @return Map
     */
    private Map<String, Integer> tallyAllCodes(Collection<String> usedCodes) {
        Map<String, Integer> codeUsage = new HashMap<String, Integer>(1024);
        for (String code : usedCodes) {
            if (code != null) {
                if (codeUsage.keySet().contains(code)) {
                    int usage = codeUsage.get(code).intValue();
                    codeUsage.put(code, Integer.valueOf(usage + 1));
                } else {
                    codeUsage.put(code, Integer.valueOf(1));
                }
            }
        }
        return codeUsage;
    }
}
