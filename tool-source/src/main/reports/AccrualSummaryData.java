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
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Accrual Summary" report, which displays the accrual account balances
 * for one or more staff members.
 *
 * @author X2 Development Corporation
 */
public class AccrualSummaryData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "active only" report parameter. The value is a Boolean.
     */
    public static final String ACTIVE_ONLY_PARAM = "activeOnly";

    /**
     * Name for the "date of calculation" report parameter. The value is a java.sql.Date.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Name for the enumerated "selection" report parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    // Data grid columns
    private static final String COL_BALANCE = "balance";
    private static final String COL_STAFF = "staff";
    private static final String COL_TYPE = "type";

    private boolean m_activeOnly;
    private PlainDate m_asOfDate;
    private SisStaff m_currentStaff;
    private PlainDate m_cutoverDate;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        m_asOfDate = (PlainDate) getParameter(DATE_PARAM);

        m_cutoverDate =
                PreferenceManager.getPreferenceValueAsDate(getOrganization(), SisPreferenceConstants.SFA_CUTOVER_DATE);

        // Instantiate a HashMap into which balances will be calculated
        HashMap<String, Map<String, BigDecimal>> staffMap =
                new HashMap<String, Map<String, BigDecimal>>(m_currentStaff != null ? 1 : 1000);

        ReportDataGrid grid = new ReportDataGrid(m_currentStaff != null ? 1 : 1000, 5);
        QueryIterator staffIterator = null;
        try {
            // Calculate the unadjusted balances
            calculateInitialBalances(staffMap);

            // Update with attendance
            StaffAttendanceManager.calculateAttendance(staffMap,
                    m_asOfDate,
                    m_cutoverDate,
                    m_currentStaff != null ? m_currentStaff.getOid() : null,
                    getBroker());

            // Update with leaves
            calculateLeaves(staffMap);

            // Load final calculations into the ReportDataGrid for the selected staff
            Criteria staffCriteria = buildStaffCriteria();
            QueryByCriteria query = createQueryByCriteria(SisStaff.class, staffCriteria);
            applyUserSort(query, ((String) getParameter(SORT_PARAM)));

            staffIterator = getBroker().getIteratorByQuery(query);
            while (staffIterator.hasNext()) {
                SisStaff staff = (SisStaff) staffIterator.next();
                Map<String, BigDecimal> balances = staffMap.get(staff.getOid());

                if (balances != null) {
                    Iterator typeIterator = balances.keySet().iterator();
                    while (typeIterator.hasNext()) {
                        String accrualType = (String) typeIterator.next();
                        if (accrualType != null && accrualType.length() > 0) {
                            grid.append();
                            grid.set(COL_STAFF, staff);
                            grid.set(COL_TYPE, accrualType);
                            grid.set(COL_BALANCE, balances.get(accrualType));
                        }
                    }
                }
            }
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        } finally {
            if (staffIterator != null) {
                staffIterator.close();
            }
        }

        grid.beforeTop();

        return grid;
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
         * If we're in the context of a single staff member, print the report for just that member
         */
        m_currentStaff = userData.getCurrentRecord(SisStaff.class);
        m_activeOnly = ((Boolean) getParameter(ACTIVE_ONLY_PARAM)).booleanValue();
    }


    /**
     * Returns the staff criteria for this report based on the input parameters. If the current
     * staff is not null then the criteria will only return that staff member.
     *
     * @return Criteria
     */
    private Criteria buildStaffCriteria() {
        Criteria criteria = new Criteria();

        if (m_currentStaff != null) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentStaff.getOid());
        } else {
            addUserCriteria(criteria,
                    (String) getParameter(QUERY_BY_PARAM),
                    (String) getParameter(QUERY_STRING_PARAM),
                    null,
                    null);
            if (isSchoolContext()) {
                criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addAndCriteria(getOrganizationCriteria(SisStaff.class));
            }

            if (m_activeOnly) {
                String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                        SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
                criteria.addEqualTo(SisStaff.COL_STATUS, activeCode);
            }
        }

        return criteria;
    }

    /**
     * Populates the passed HashMap with the unadjusted accrual balances for each staff member. The
     * passed HashMap will be populated with HashMaps keyed on accrual type containing the balances
     * for each staff member and type.
     *
     * @param staffMap A Map of accrual-balance TreeMap objects keyed on Staff OIDs (String
     *        objects); each TreeMap maps an accrual type (String) to a balance
     *        (BigDecimal)
     * @throws SQLException exception
     */
    private void calculateInitialBalances(HashMap staffMap)
            throws SQLException {
        String sql = "SELECT SFR_STF_OID, SFR_TYPE, SUM(SFR_TOTAL_UNITS) as UNITS" +
                " FROM STAFF_ACCRUAL " +
                " WHERE SFR_DATE <= ?" +
                " AND SFR_DATE >= ?" +
                (m_currentStaff != null ? " AND SFR_STF_OID = ?" : "") +
                " GROUP BY SFR_STF_OID, SFR_TYPE " +
                " ORDER BY SFR_STF_OID ";

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = connection.prepareStatement(sql);

        statement.setDate(1, m_asOfDate);
        statement.setDate(2, m_cutoverDate);
        if (m_currentStaff != null) {
            statement.setString(3, m_currentStaff.getOid());
        }

        ResultSet results = null;
        try {
            results = statement.executeQuery();

            Map balanceMap = null;

            String lastOid = null;
            while (results.next()) {
                String staffOid = results.getString("SFR_STF_OID");
                String type = results.getString("SFR_TYPE");
                BigDecimal balance = results.getBigDecimal("UNITS");

                if (balance != null) {
                    if (lastOid == null || !lastOid.equals(staffOid)) {
                        if (lastOid != null) {
                            staffMap.put(lastOid, balanceMap);
                        }
                        balanceMap = new TreeMap();
                        lastOid = staffOid;
                    }

                    balanceMap.put(type, balance);
                }
            }

            staffMap.put(lastOid, balanceMap);
        } finally {
            if (results != null) {
                try {
                    results.close();
                } catch (SQLException e) {
                    // ignore
                }
            }

            try {
                statement.close();
            } catch (SQLException e) {
                // ignore
            }

            getBroker().returnConnection();
        }
    }

    /**
     * Populates the passed HashMap with leave data for each staff member.
     *
     * @param staffMap HashMap
     */
    private void calculateLeaves(HashMap staffMap) {
        Boolean includeSaturday = null;
        Boolean includeSunday = null;

        SisSchool school = null;
        String staffOid = null;
        if (m_currentStaff != null) {
            staffOid = m_currentStaff.getOid();
            school = m_currentStaff.getSchool();
        } else {
            // Set the school as the selected value on the input.
            school = (SisSchool) getSchool();
        }

        if (school != null) {
            includeSaturday =
                    Boolean.valueOf(PreferenceManager.getPreferenceValue(school,
                            SisPreferenceConstants.SFA_SATURDAY_INDICATOR));
            includeSunday =
                    Boolean.valueOf(PreferenceManager.getPreferenceValue(school,
                            SisPreferenceConstants.SFA_SUNDAY_INDICATOR));
        } else {
            includeSaturday =
                    Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                            SisPreferenceConstants.SFA_SATURDAY_INDICATOR));
            includeSunday =
                    Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                            SisPreferenceConstants.SFA_SUNDAY_INDICATOR));
        }

        StaffAttendanceManager saManager = AttendanceManagerFactory.getStaffAttendanceManager(getBroker(),
                includeSaturday.booleanValue(), includeSunday.booleanValue());

        Criteria leaveCriteria = StaffAttendanceManager.getLeaveCriteria(m_cutoverDate,
                m_asOfDate,
                null,
                getOrganization().getOid(),
                staffOid);

        QueryByCriteria leaveQuery = new QueryByCriteria(StaffLeave.class, leaveCriteria);
        QueryIterator leaves = null;
        try {
            leaves = getBroker().getIteratorByQuery(leaveQuery);
            while (leaves.hasNext()) {
                StaffLeave leave = (StaffLeave) leaves.next();

                if (leave.getAccrualType() != null) {
                    Map balanceMap = (Map) staffMap.get(leave.getStaffOid());

                    if (balanceMap == null) {
                        balanceMap = new TreeMap();
                    }

                    BigDecimal balance = (BigDecimal) balanceMap.get(leave.getAccrualType());
                    if (balance == null) {
                        balance = new BigDecimal(0);
                    }

                    BigDecimal netLeaveDays =
                            new BigDecimal(saManager.getNetLeaveDays(leave, new PlainDate(0), m_asOfDate));

                    balance = balance.subtract(netLeaveDays);

                    balanceMap.put(leave.getAccrualType(), balance);
                    staffMap.put(leave.getStaffOid(), balanceMap);
                }
            }
        } finally {
            if (leaves != null) {
                leaves.close();
            }
        }
    }
}
