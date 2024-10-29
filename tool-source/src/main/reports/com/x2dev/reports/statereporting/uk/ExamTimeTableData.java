/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.uk;

import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.ExamComponent;
import com.x2dev.sis.model.beans.ExamSeason;
import com.x2dev.utils.StringUtils;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports3.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares data for the "Exam Timetable" report.
 *
 * @author X2 Development Corporation
 */
public class ExamTimeTableData extends ReportJavaSourceNet {
    /*
     * Input parameters
     */
    private static final String PARAM_EXAM_SEASON = "examSeason";
    private static final String PARAM_EXCLUDE_EMPTY_DATES = "excludeEmptyDates";
    private static final String PARAM_SORT_BY = "sortBy";

    /*
     * Report parameters
     */
    private static final String REPORT_PARAM_SEASON = "Season";

    /*
     * Grid fields
     */
    private static final String FIELD_CANDIDATES = "Candidates";
    private static final String FIELD_DATE = "Date";
    private static final String FIELD_EXAM_NAME = "Exam name";
    private static final String FIELD_EXAM_CODE = "Exam code";
    private static final String FIELD_MINS_ALLOWED = "Mins allowed";
    private static final String FIELD_TIMETABLE_SESSION = "Timetable session";
    private static final String FIELD_EXAM_TYPE_QUALIFICATION = "Exam type qualification";

    private ExamSeason m_season;
    private Map<String, String> m_refDescriptionLookup;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;

        try {
            statement = connection.prepareStatement(buildSql());
            statement.setString(1, m_season.getOid());

            results = statement.executeQuery();
            while (results.next()) {
                grid.append();
                grid.set(FIELD_DATE, results.getDate(1));
                grid.set(FIELD_EXAM_NAME, results.getString(2));
                grid.set(FIELD_EXAM_CODE, results.getString(3));
                grid.set(FIELD_MINS_ALLOWED, Integer.valueOf(results.getInt(4)));
                grid.set(FIELD_TIMETABLE_SESSION, resolveDescription(results.getString(5)));
                grid.set(FIELD_EXAM_TYPE_QUALIFICATION, results.getString(6));
                grid.set(FIELD_CANDIDATES, Integer.valueOf(results.getInt(7)));
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().log(Level.WARNING, sqle.getMessage(), sqle);
        } finally {
            try {
                if (results != null) {
                    results.close();
                }

                if (statement != null) {
                    statement.close();
                }
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.WARNING, e.getMessage(), e);
            }

            getBroker().returnConnection();
        }

        addParameter(REPORT_PARAM_SEASON, m_season.getName());

        grid.beforeTop();
        return grid;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#initialize()
     */
    @Override
    protected void initialize() {
        String seasonOid = (String) getParameter(PARAM_EXAM_SEASON);
        m_season = (ExamSeason) getBroker().getBeanByOid(ExamSeason.class, seasonOid);

        m_refDescriptionLookup = loadRefDescriptionLookup();
    }

    /**
     * Builds the sql query.
     * 
     * @return String
     */
    private String buildSql() {
        StringBuilder sql = new StringBuilder(500);

        sql.append("SELECT COM_TIMETABLE_DATE, OPT_TITLE, OPC_OPTION_ENTRY_CODE, COM_TIME_ALLOWED, ");
        sql.append("       COM_TIMETABLE_SESSION_CODE, OPT_EXAM_TYPE_QUAL_CERT, ENT_NUM_CANDIDATES ");
        sql.append("  FROM EXAM_SEASON ");
        sql.append(" INNER JOIN EXAM_SERIES ON SER_SES_OID = SES_OID ");
        sql.append(" INNER JOIN EXAM_COMPONENT ON COM_SER_OID = SER_OID ");
        sql.append(" INNER JOIN EXAM_OPTION_COMPONENT ON OPC_COM_OID = COM_OID ");
        sql.append(" INNER JOIN EXAM_OPTION  ON OPC_OPT_OID = OPT_OID ");
        sql.append("  LEFT OUTER JOIN (SELECT ENT_OPT_OID ENTRY_OPT_OID, COUNT(*) ENT_NUM_CANDIDATES ");
        sql.append("                     FROM EXAM_ENTRY ");
        sql.append("                   GROUP BY ENT_OPT_OID) AS A1 ON ENTRY_OPT_OID = OPT_OID ");
        sql.append(" WHERE SES_OID = ?");

        Boolean excludeEmptyDates = (Boolean) getParameter(PARAM_EXCLUDE_EMPTY_DATES);
        if (excludeEmptyDates.booleanValue()) {
            sql.append(" AND COM_TIMETABLE_DATE IS NOT NULL");
        }

        sql.append(" ORDER BY ");
        sql.append((String) getParameter(PARAM_SORT_BY));

        return sql.toString();
    }

    /**
     * Returns a map of reference code descriptions keyed on codes for the reference table
     * linked to 'Timetable Session' field on ExamComponent.
     * 
     * @return Map
     */
    private Map<String, String> loadRefDescriptionLookup() {
        Map<String, String> refDescLookup = new HashMap<String, String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField timetableSession =
                dictionary.findDataDictionaryField(ExamComponent.class.getName(),
                        ExamComponent.COL_TIMETABLE_SESSION_CODE);
        if (timetableSession != null && timetableSession.hasReferenceTable()) {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, timetableSession.getReferenceTableOid());

            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    ReferenceCode refCode = (ReferenceCode) iterator.next();
                    refDescLookup.put(refCode.getCode(), refCode.getDescription());
                }
            } finally {
                iterator.close();
            }
        }

        return refDescLookup;
    }

    /**
     * Returns the description for the passed reference code.
     *
     *
     * @param code String
     * @return String
     */
    private String resolveDescription(String code) {
        String description = m_refDescriptionLookup.get(code);

        if (StringUtils.isEmpty(description)) {
            description = code;
        }

        return description;
    }
}
