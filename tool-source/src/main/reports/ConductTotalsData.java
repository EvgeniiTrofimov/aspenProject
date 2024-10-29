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

import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ReferenceDescriptionLookup;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Prepares the data for the Conduct Totals report. This report lists the total number of each
 * incident or action that occurred within a specified date range.
 *
 * @author X2 Development Corporation
 */
public class ConductTotalsData extends SecondaryStudentDataSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report parameter name for the end of the date range. This value is a PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the "group by" report input parameter. This value is an Integer.
     */
    public static final String GROUP_BY_PARAM = "groupBy";

    /**
     * Name for the "sort by totals" report input parameter. This value is a Boolean.
     */
    public static final String ORDER_BY_TOTALS_PARAM = "orderByTotals";

    /**
     * Name for the "query by" report input parameter. This value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" report input parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Report parameter name for the start of the date range. This value is a PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /*
     * Grid fields
     */
    private static final String FIELD_CODE = "code";
    private static final String FIELD_COUNT = "count";
    private static final String FIELD_DESCRIPTION = "description";
    private static final String FIELD_GROUP = "group";
    private static final String FIELD_SCHOOL = "school";

    /*
     * Report Parameters
     */
    private static final String PARAMETER_TITLE = "title";

    private static final String SQL_COUNT = "count(*)";
    private static final String TITLE_ACTION = "Action Totals";
    private static final String TITLE_INCIDENT = "Incident Totals";
    private static final String TITLE_LOCATION_GROUP = " by Location";
    private static final String TITLE_STAFF_GROUP = " by Referral Staff";
    private static final String TITLE_STUDENT_GROUP = " by Student";

    private DataDictionary m_dictionary;
    private PlainDate m_endDate;
    private int m_groupBy;
    private boolean m_orderByTotals;
    private PlainDate m_startDate;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_startDate = (PlainDate) getParameter(START_DATE_PARAM);
        m_endDate = (PlainDate) getParameter(END_DATE_PARAM);

        m_dictionary = DataDictionary.getDistrictDictionary(getUser().getPersistenceKey());

        m_groupBy = ((Integer) getParameter(GROUP_BY_PARAM)).intValue();
        m_orderByTotals = ((Boolean) getParameter(ORDER_BY_TOTALS_PARAM)).booleanValue();

        String title = "";

        Criteria conductCriteria = new Criteria();
        String tableOid;
        String extraColumn;
        QueryByCriteria conductQuery;

        switch (m_groupBy) {
            case 0: // Actions
                conductCriteria = getActionCriteria();
                extraColumn = null;
                conductQuery = getActionQuery(conductCriteria, extraColumn);

                tableOid = getReferenceTableOid(ConductAction.class.getName(),
                        ConductAction.COL_ACTION_CODE);

                title = TITLE_ACTION;
                break;

            case 1: // Actions by student
                conductCriteria = getActionCriteria();
                extraColumn = ConductAction.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW;
                conductQuery = getActionQuery(conductCriteria, extraColumn);

                tableOid = getReferenceTableOid(ConductAction.class.getName(),
                        ConductAction.COL_ACTION_CODE);

                title = TITLE_ACTION + TITLE_STUDENT_GROUP;
                break;

            case 2: // Incidents
                conductCriteria = getIncidentCriteria();
                extraColumn = null;
                conductQuery = getIncidentQuery(conductCriteria, extraColumn);

                tableOid = getReferenceTableOid(ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);

                title = TITLE_INCIDENT;
                break;

            case 3: // Incidents by student
                conductCriteria = getIncidentCriteria();
                extraColumn = ConductIncident.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW;
                conductQuery = getIncidentQuery(conductCriteria, extraColumn);

                tableOid = getReferenceTableOid(ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);

                title = TITLE_INCIDENT + TITLE_STUDENT_GROUP;
                break;

            case 4: // Incidents by staff
                conductCriteria = getIncidentCriteria();
                extraColumn = ConductIncident.REL_REFERRAL_STAFF + "." + SisStaff.COL_NAME_VIEW;
                conductQuery = getIncidentQuery(conductCriteria, extraColumn);

                tableOid = getReferenceTableOid(ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);

                title = TITLE_INCIDENT + TITLE_STAFF_GROUP;
                break;

            default: // Incidents by location
                conductCriteria = getIncidentCriteria();
                extraColumn = ConductIncident.COL_INCIDENT_LOCATION;
                conductQuery = getIncidentQuery(conductCriteria, extraColumn);

                tableOid = getReferenceTableOid(ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);

                title = TITLE_INCIDENT + TITLE_LOCATION_GROUP;
                break;
        }


        ReportDataGrid grid = new ReportDataGrid(1000, 5);
        SisSchool currentSchool = null;
        String lastSchoolOid = null;

        ReferenceDescriptionLookup referenceLookup =
                (ReferenceDescriptionLookup) getParameter(REFERENCE_LOOKUP_KEY);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(conductQuery);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                String code = (String) row[0];
                String schoolOid = (String) row[1];
                String countString = row[2].toString();
                Long count = Long.valueOf(countString);

                String extraField = null;
                if (row.length > 3) {
                    extraField = (String) row[3];

                    if (StringUtils.isEmpty(extraField)) {
                        extraField = "BLANK";
                    }
                }

                if (lastSchoolOid == null || !schoolOid.equals(lastSchoolOid)) {
                    currentSchool = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                }

                grid.append();
                grid.set(FIELD_SCHOOL, currentSchool);
                grid.set(FIELD_COUNT, count);
                grid.set(FIELD_CODE, code);
                grid.set(FIELD_GROUP, extraField);

                /*
                 * Display description from reference table - if exists.
                 */
                String codeDescription = referenceLookup.getDescriptionForSchool(tableOid, schoolOid, code);
                if (StringUtils.isEmpty(codeDescription)) {
                    codeDescription = code;
                }
                grid.set(FIELD_DESCRIPTION, codeDescription);

                lastSchoolOid = schoolOid;
            }
        } finally {
            iterator.close();
        }

        /*
         * Add report parameters.
         */
        addParameter(START_DATE_PARAM, m_startDate);
        addParameter(END_DATE_PARAM, m_endDate);
        addParameter(PARAMETER_TITLE, title);

        grid.beforeTop();
        return grid;
    }

    /**
     * Returns a general criteria for ConductActions.
     *
     * @return Criteria
     */
    private Criteria getActionCriteria() {
        Criteria actionCriteria = new Criteria();

        actionCriteria.addGreaterOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_startDate);
        actionCriteria.addLessOrEqualThan(ConductAction.COL_ACTION_START_DATE, m_endDate);

        if (isSchoolContext()) {
            if (hasSpecifiedCriteria()) {
                actionCriteria.addAndCriteria(getStudentObjectCriteria(ConductAction.COL_STUDENT_OID,
                        ConductAction.REL_STUDENT,
                        ConductAction.COL_SCHOOL_OID));
            } else {
                actionCriteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getSchool().getOid());
            }
        } else {
            actionCriteria.addAndCriteria(getOrganizationCriteria(ConductAction.class));
        }

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                actionCriteria.addEqualTo(ConductAction.REL_STUDENT + "." + SisStudent.COL_YOG,
                        getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // Record Set
                actionCriteria.addIn(ConductAction.COL_STUDENT_OID,
                        ReportUtils.getRecordSetSubQuery((String) getParameter(QUERY_STRING_PARAM),
                                getUser(), getSchool()));
                break;

            default: // All
                break;
        }

        return actionCriteria;
    }

    /**
     * Builds the query on ConductAction table.
     *
     * @param conductCriteria Criteria
     * @param extraColumn String
     * @return QueryByCriteria
     */
    private QueryByCriteria getActionQuery(Criteria conductCriteria, String extraColumn) {
        boolean useExtraColumn = !StringUtils.isEmpty(extraColumn);
        String[] columns;

        if (useExtraColumn) {
            columns = new String[] {ConductAction.COL_ACTION_CODE,
                    ConductAction.COL_SCHOOL_OID, SQL_COUNT, extraColumn};
        } else {
            columns = new String[] {ConductAction.COL_ACTION_CODE,
                    ConductAction.COL_SCHOOL_OID, SQL_COUNT};
        }

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(ConductAction.class, columns, conductCriteria);

        query.addGroupBy(ConductAction.COL_ACTION_CODE);
        query.addGroupBy(ConductAction.COL_SCHOOL_OID);

        if (useExtraColumn) {
            query.addGroupBy(extraColumn);
        }

        query.addOrderByAscending(ConductAction.COL_SCHOOL_OID);

        if (useExtraColumn) {
            query.addOrderByAscending(extraColumn);
        }

        if (m_orderByTotals) {
            query.addOrderByAscending(SQL_COUNT);
        }

        query.addOrderByAscending(ConductAction.COL_ACTION_CODE);

        return query;
    }

    /**
     * Returns a general criteria for ConductIncidents.
     *
     * @return Criteria
     */
    private Criteria getIncidentCriteria() {
        Criteria incidentCriteria = new Criteria();

        incidentCriteria.addGreaterOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_startDate);
        incidentCriteria.addLessOrEqualThan(ConductIncident.COL_INCIDENT_DATE, m_endDate);

        if (isSchoolContext()) {
            if (hasSpecifiedCriteria()) {
                incidentCriteria.addAndCriteria(getStudentObjectCriteria(ConductIncident.COL_STUDENT_OID,
                        ConductIncident.REL_STUDENT,
                        ConductIncident.COL_SCHOOL_OID));
            } else {
                incidentCriteria.addEqualTo(ConductIncident.COL_SCHOOL_OID, getSchool().getOid());
            }
        } else {
            incidentCriteria.addAndCriteria(getOrganizationCriteria(ConductIncident.class));
        }

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                incidentCriteria.addEqualTo(ConductIncident.REL_STUDENT + "." + SisStudent.COL_YOG,
                        getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // Record Set
                incidentCriteria.addIn(ConductIncident.COL_STUDENT_OID,
                        ReportUtils.getRecordSetSubQuery((String) getParameter(QUERY_STRING_PARAM),
                                getUser(), getSchool()));
                break;

            default: // All
                break;
        }

        return incidentCriteria;
    }

    /**
     * Builds the query on ConductIncident table.
     *
     * @param conductCriteria Criteria
     * @param extraColumn String
     * @return QueryByCriteria
     */
    private QueryByCriteria getIncidentQuery(Criteria conductCriteria, String extraColumn) {
        boolean useExtraColumn = !StringUtils.isEmpty(extraColumn);
        String[] columns;

        if (useExtraColumn) {
            columns = new String[] {ConductIncident.COL_INCIDENT_CODE,
                    ConductIncident.COL_SCHOOL_OID, SQL_COUNT, extraColumn};
        } else {
            columns = new String[] {ConductIncident.COL_INCIDENT_CODE,
                    ConductIncident.COL_SCHOOL_OID, SQL_COUNT};
        }

        ReportQueryByCriteria query =
                new ReportQueryByCriteria(ConductIncident.class, columns, conductCriteria);

        query.addGroupBy(ConductIncident.COL_INCIDENT_CODE);
        query.addGroupBy(ConductIncident.COL_SCHOOL_OID);

        if (useExtraColumn) {
            query.addGroupBy(extraColumn);
        }

        query.addOrderByAscending(ConductIncident.COL_SCHOOL_OID);

        if (useExtraColumn) {
            query.addOrderByAscending(extraColumn);
        }

        if (m_orderByTotals) {
            query.addOrderByAscending(SQL_COUNT);
        }

        query.addOrderByAscending(ConductIncident.COL_INCIDENT_CODE);

        return query;
    }

    /**
     * Returns the reference table oid for the passed classname and field. Returns null if table
     * does not exist.
     *
     * @param className String
     * @param fieldId String
     * @return String
     */
    private String getReferenceTableOid(String className, String fieldId) {
        String oid = null;

        ReferenceTable table = m_dictionary.findDataDictionaryField(className, fieldId).getReferenceTable();

        if (table != null) {
            oid = table.getOid();
        }

        return oid;
    }
}
