/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Prepares the data for the "Credit Summary" report. This report shows each student and their
 * total credits earned over the given grade level range.
 *
 * @author X2 Development Corporation
 */
public class CreditSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" input parameter. This value is a Boolean.
     */
    public static final String ACTIVE_ONLY = "activeOnly";

    /**
     * Name for the "end grade" input parameter. This value is a String.
     */
    public static final String END_GRADE_PARAM = "endGrade";

    /**
     * Name for the "max credits" input parameter. This value is a BigDecimal.
     */
    public static final String MAXIMUM_CREDIT_PARAM = "maxCredits";

    /**
     * Name for the "min credits" input parameter. This value is a BigDecimal.
     */
    public static final String MINIMUM_CREDIT_PARAM = "minCredits";

    /**
     * Name for the "query by" input parameter. This value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" input parameter. This value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the "sort" input parameter. This value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Name for the "active only" input parameter. This value is a Boolean.
     */
    public static final String START_GRADE_PARAM = "startGrade";

    // Grid fields
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_TOTAL = "totalCredits";

    // Report parameters
    private static final String CONTEXT_MAP_PARAM = "contextMap";

    private SisStudent m_currentStudent;
    private BigDecimal m_maxCredit;
    private BigDecimal m_minCredit;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid resultsGrid = new ReportDataGrid(20);

        try {
            Connection connection = null;
            Statement statement = null;
            ResultSet results = null;
            try {
                connection = getBroker().borrowConnection();
                statement = connection.createStatement(ResultSet.TYPE_FORWARD_ONLY,
                        ResultSet.CONCUR_READ_ONLY);
                results = statement.executeQuery(getSql());

                int currentYear = getCurrentContext().getSchoolYear();

                String lastStudentOid = null;

                while (results.next()) {
                    String studentOid = results.getString("_STD_OID");

                    SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, studentOid);

                    int schoolYear = results.getInt("CTX_SCHOOL_YEAR");
                    String yearOffset = Integer.toString(currentYear - schoolYear);

                    if (lastStudentOid == null || !studentOid.equals(lastStudentOid)) {
                        resultsGrid.append();
                        resultsGrid.set(FIELD_STUDENT, student);
                        resultsGrid.set(FIELD_TOTAL, new BigDecimal(0));
                    }

                    BigDecimal credits = new BigDecimal(String.valueOf(results.getDouble("_CREDIT")));
                    BigDecimal totalCredits = (BigDecimal) resultsGrid.get(FIELD_TOTAL);

                    totalCredits = totalCredits.add(credits);

                    resultsGrid.set(yearOffset, credits);
                    resultsGrid.set(FIELD_TOTAL, totalCredits);

                    lastStudentOid = studentOid;
                }
            } finally {
                if (results != null) {
                    try {
                        results.close();
                    } catch (SQLException e) {
                        // ignore
                    }
                }

                if (statement != null) {
                    try {
                        statement.close();
                    } catch (SQLException e) {
                        // ignore
                    }
                }

                getBroker().returnConnection();
            }
        } catch (SQLException sqle) {
            AppGlobals.getLog().severe(sqle.getMessage());
        }

        if (m_minCredit != null || m_maxCredit != null) {
            resultsGrid.beforeTop();
            cleanGrid(resultsGrid);
        }

        // Add report parameters
        addParameter(CONTEXT_MAP_PARAM, getContextMap());

        resultsGrid.beforeTop();
        return resultsGrid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        /*
         * If we're in the context of a single student, print the report for just that student
         */
        m_currentStudent = userData.getCurrentRecord(SisStudent.class);
        m_minCredit = (BigDecimal) getParameter(MINIMUM_CREDIT_PARAM);
        m_maxCredit = (BigDecimal) getParameter(MAXIMUM_CREDIT_PARAM);
    }

    /**
     * Iterate over grid and remove students if they do not meet the credit requirements.
     *
     * @param grid ReportDataGrid
     */
    private void cleanGrid(ReportDataGrid grid) {
        while (grid.next()) {
            BigDecimal total = (BigDecimal) grid.get(FIELD_TOTAL);
            boolean delete = false;

            if (m_minCredit != null && total.compareTo(m_minCredit) == -1) {
                delete = true;
            }

            if (!delete && m_maxCredit != null && total.compareTo(m_maxCredit) == 1) {
                delete = true;
            }

            if (delete) {
                grid.deleteRow();
            }
        }
    }

    /**
     * Loads the years into a map keyed to offsets of current year.
     *
     * @return Map
     */
    private Map getContextMap() {
        HashMap contextMap = new HashMap(10);
        int currentYear = getCurrentContext().getSchoolYear();
        for (int i = 0; i < 6; i++) {
            contextMap.put(Integer.valueOf(i).toString(), Integer.valueOf(currentYear - i));
        }

        return contextMap;
    }

    /**
     * Returns a comma delimited, single quoted list of grade levels for which credits should be
     * considered. If all grade levels should be considered, null is returned.
     *
     * @return String
     */
    private String getGradeLevelList() {
        String gradeLevelList = null;

        String startGrade = (String) getParameter(START_GRADE_PARAM);
        String endGrade = (String) getParameter(END_GRADE_PARAM);

        if (startGrade != null || endGrade != null) {
            List gradeLevels = StudentManager.getGradeLevelRange(startGrade, endGrade, getBroker());
            gradeLevelList = StringUtils.convertCollectionToDelimitedString(gradeLevels, ",", "'");
        }

        return gradeLevelList;
    }

    /**
     * Returns the SQL for the report's main query that that calculates total credits from the
     * student transcript and credit adjustment tables.
     * <p>
     * The structure of the query is as follows:
     *
     * <pre>
     *  SELECT [student fields], SUM(credit)
     *    FROM (SELECT [transcript credit]
     *          UNION
     *          SELECT [adjusted credit))
     *   INNER JOIN STUDENT...
     *   INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT...
     *   WHERE [where clause based on report params]
     *   GROUP BY [student, year]
     *   ORDER BY [sort fields based on report params]
     * </pre>
     *
     * @return String
     */
    private String getSql() {
        String gradeLevelList = getGradeLevelList();

        String sql = "SELECT _STD_OID, _CTX_OID, SUM(_CREDIT) as _CREDIT, " +
                "MAX(STD_NAME_VIEW) as STD_NAME_VIEW, MAX(STD_YOG) as STD_YOG, MAX(STD_HOMEROOM) as STD_HOMEROOM, MAX(CTX_SCHOOL_YEAR) as CTX_SCHOOL_YEAR "
                +
                "FROM (SELECT TRN_STD_OID AS _STD_OID, TRN_CTX_OID AS _CTX_OID, TRN_TOTAL_CREDIT AS _CREDIT " +
                "FROM STUDENT_TRANSCRIPT ";

        if (gradeLevelList != null) {
            sql += "WHERE TRN_GRADE_LEVEL IN (" + gradeLevelList + ") ";
        }

        sql += "UNION ALL " +
                "SELECT CRA_STD_OID AS _STD_OID, CRA_CTX_OID AS _CTX_OID, CRA_CREDIT AS _CREDIT " +
                "FROM STUDENT_CREDIT_ADJUSTMENT ";

        if (gradeLevelList != null) {
            sql += "WHERE CRA_GRADE_LEVEL IN (" + gradeLevelList + ") ";
        }

        sql += ") a " +
                "INNER JOIN STUDENT " +
                "ON STD_OID = _STD_OID " +
                "INNER JOIN DISTRICT_SCHOOL_YEAR_CONTEXT " +
                "ON CTX_OID = _CTX_OID ";

        ArrayList where = new ArrayList(5);
        if (m_currentStudent != null) {
            where.add("STD_OID = '" + m_currentStudent.getOid() + "'");
        } else {
            int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
            switch (queryBy) {
                case 2: // YOG
                    where.add("STD_YOG = " + getParameter(QUERY_STRING_PARAM));
                    break;

                case 3: // Homeroom
                    where.add("STD_HOMEROOM = '" + getParameter(QUERY_STRING_PARAM) + "'");
                    break;

                default: // All
                    break;
            }

            boolean activeOnly = ((Boolean) getParameter(ACTIVE_ONLY)).booleanValue();
            if (activeOnly) {
                where.add(StudentManager.getActiveStudentDirectSQL(getOrganization(), "STD_ENROLLMENT_STATUS"));
            }


            if (isSchoolContext()) {
                where.add("STD_SKL_OID = '" + getSchool().getOid() + "'");
            } else {
                int level = getOrganization().getOrganizationDefinition().getLevel();
                where.add(" AND STD_ORG_OID_");
                where.add(Integer.valueOf(level + 1));
                where.add(" = '");
                where.add(getOrganization().getOid());
                where.add("'");
            }

        }

        // Apply the where clause
        for (int i = 0; i < where.size(); i++) {
            sql += i == 0 ? " WHERE " : " AND ";
            sql += where.get(i);
        }

        // Apply the group by clause
        sql += " GROUP BY _STD_OID, _CTX_OID";

        // Apply the order by clause
        sql += " ORDER BY ";

        int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
        switch (sort) {
            case 1: // YOG
                sql += "STD_YOG, STD_NAME_VIEW,";
                break;

            case 2: // Homeroom
                sql += "STD_HOMEROOM, STD_NAME_VIEW,";
                break;

            default: // Name view (0)
                sql += "STD_NAME_VIEW,";
                break;
        }

        sql += " _STD_OID, CTX_SCHOOL_YEAR, _CTX_OID ";

        return sql;
    }
}
