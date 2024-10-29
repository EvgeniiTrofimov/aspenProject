/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2008 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import static com.follett.fsc.core.k12.beans.SystemPreferenceDefinition.STAFF_ACTIVE_CODE;
import static com.x2dev.sis.model.beans.SisPreferenceConstants.SFA_CUTOVER_DATE;
import static com.x2dev.sis.model.beans.SisPreferenceConstants.SFA_SATURDAY_INDICATOR;
import static com.x2dev.sis.model.beans.SisPreferenceConstants.SFA_SUNDAY_INDICATOR;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.StaffAccrual;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Date;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to adjust staff accrual totals based on their accumulation, maximum carryover, maximum
 * balance and the awarded credits.
 * <p>
 * This procedure uses a variation of the methods:
 * <ul>
 * <li><code>calculateAttendance(HashMap)</code>
 * <li><code>calculateInitialBalances(HashMap)</code>
 * <li><code>calculateLeaves(HashMap)</code>
 * </ul>
 * that are used to calculate accruals in the <b>Accrual Summary</b> report and other reports of
 * that kind.
 * <p>
 * TODO: Those methods should be moved to a manager class since they are duplicated in at least 4
 * different places (see ticket T20004573).
 *
 * @author X2 Development Corporation
 */
public class CreditAccrualsAdvanced extends ProcedureJavaSource {
    // Input parameters
    private static final String BARGAINING_UNIT_PARAM = "bargainingUnit";
    private static final String CREDIT_PARAM = "credit";
    private static final String DATE_PARAM = "date";
    private static final String MAX_BALANCE_PARAM = "maxBalance";
    private static final String MAX_CARRYOVER_PARAM = "maxCarryover";
    private static final String MAX_YEARS_PARAM = "maxYears";
    private static final String MIN_YEARS_PARAM = "minYears";
    private static final String REASON_PARAM = "reason";
    private static final String RESET_BALANCES_PARAM = "resetBalances";
    private static final String STAFF_ID_PARAM = "staffId";
    private static final String TYPE_PARAM = "type";
    private static final String USE_SENIORITY_PARAM = "useSeniority";

    // Other constants
    private static final String COLLECTION_DELIMITER = "','";

    private String m_accrualType;
    private PlainDate m_asOfDate;
    private PlainDate m_cutoverDate;
    private Map<String, PlainDate> m_staffHireDates;
    private Collection<String> m_staffOids;
    private boolean m_useSeniority;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // Instantiate a HashMap into which balances will be calculated
        HashMap staffMap = new HashMap(1000);

        try {
            // Calculate the unadjusted balances
            calculateInitialBalances(staffMap);

            // Update with attendance
            calculateAttendance(staffMap);

            // Update with leaves
            calculateLeaves(staffMap);

            // Load staff hire dates if seniority is used
            if (m_useSeniority) {
                m_staffHireDates = new HashMap<String, PlainDate>(512);
                loadStaffHireDates();
            }

            // Adjust the accruals considering the max carryover, max balance and credit
            adjustAccruals(staffMap);
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        }
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        SubQuery query = new SubQuery(Staff.class, X2BaseBean.COL_OID, buildStaffCriteria());
        m_staffOids = getBroker().getSubQueryCollectionByQuery(query);

        m_accrualType = (String) getParameter(TYPE_PARAM);
        m_asOfDate = (PlainDate) getParameter(DATE_PARAM);

        m_cutoverDate = PreferenceManager.getPreferenceValueAsDate(getOrganization(), SFA_CUTOVER_DATE);

        m_useSeniority = ((Boolean) getParameter(USE_SENIORITY_PARAM)).booleanValue();
    }

    /**
     * Creates an accrual record for every staff member to adjust the remaining accrual totals at
     * the
     * end of the school year.
     * This calculation takes into consideration the maximum number of units a staff member can
     * carry over to the next school year.
     *
     * @param staffMap Map<String,BigDecimal>
     */
    private void adjustAccruals(Map<String, BigDecimal> staffMap) {
        double credit = ((BigDecimal) getParameter(CREDIT_PARAM)).doubleValue();
        double maxBalance = ((BigDecimal) getParameter(MAX_BALANCE_PARAM)).doubleValue();
        double maxCarryover = ((BigDecimal) getParameter(MAX_CARRYOVER_PARAM)).doubleValue();
        String reason = (String) getParameter(REASON_PARAM);
        boolean resetBalances = ((Boolean) getParameter(RESET_BALANCES_PARAM)).booleanValue();

        int maxYears = 0;
        int minYears = 0;

        if (m_useSeniority) {
            maxYears = ((Integer) getParameter(MAX_YEARS_PARAM)).intValue();
            minYears = ((Integer) getParameter(MIN_YEARS_PARAM)).intValue();
        }

        ModelBroker modelBroker = new ModelBroker(getPrivilegeSet());

        Collection<String> unadjustedStaffOids = new ArrayList<String>();

        int totalAdjustments = 0;
        int totalStaff = 0;

        for (String staffOid : m_staffOids) {
            int yearsEmployed = 0;

            if (m_useSeniority) {
                yearsEmployed = Person.getAgeAsOfDate(m_staffHireDates.get(staffOid), m_asOfDate);
            }

            if (!m_useSeniority || (m_useSeniority && yearsEmployed >= minYears && yearsEmployed < maxYears)) {
                double originalBalance = 0.0;
                double resetValue = 0.0;

                if (staffMap.containsKey(staffOid)) {
                    originalBalance = staffMap.get(staffOid).doubleValue();
                }

                if (resetBalances && originalBalance < 0.0) {
                    resetValue = originalBalance;
                    originalBalance = 0.0;
                }

                double newBalance = originalBalance;

                if (newBalance > maxCarryover) {
                    newBalance = maxCarryover;
                }

                newBalance += credit;

                if (newBalance > maxBalance) {
                    newBalance = maxBalance;
                }

                double adjustment = newBalance - originalBalance - resetValue;
                if (adjustment != 0) {
                    StaffAccrual accrual = new StaffAccrual(getBroker().getPersistenceKey());
                    accrual.setStaffOid(staffOid);
                    accrual.setDate(m_asOfDate);
                    accrual.setType(m_accrualType);
                    accrual.setReasonCode(reason);
                    accrual.setTotalUnits(new BigDecimal(adjustment));

                    modelBroker.saveBeanForced(accrual);
                    totalAdjustments++;
                } else {
                    unadjustedStaffOids.add(staffOid);
                }

                totalStaff++;
            }
        }

        logMessage("Checked " + totalStaff + " staff member(s), adjusted balances for " + totalAdjustments + ".");

        if (unadjustedStaffOids.size() > 0) {
            logMessage("Balances were not adjusted for: ");

            Criteria criteria = new Criteria();
            criteria.addIn(X2BaseBean.COL_OID, unadjustedStaffOids);

            QueryByCriteria query = new QueryByCriteria(Staff.class, criteria);

            QueryIterator iterator = getBroker().getIteratorByQuery(query);
            try {
                while (iterator.hasNext()) {
                    Staff staff = (Staff) iterator.next();

                    logMessage("Local ID: " + staff.getLocalId() + " - " + staff.getNameView());
                }
            } finally {
                iterator.close();
            }
        }
    }

    /**
     * Returns the staff criteria for this procedure based on the input parameters.
     *
     * @return Criteria
     */
    private Criteria buildStaffCriteria() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(Staff.COL_ORGANIZATION1_OID, getOrganization().getOid());

        String activeCode = PreferenceManager.getPreferenceValue(getOrganization(), STAFF_ACTIVE_CODE);
        criteria.addEqualTo(Staff.COL_STATUS, activeCode);

        String staffId = (String) getParameter(STAFF_ID_PARAM);
        if (!StringUtils.isEmpty(staffId)) {
            criteria.addEqualTo(Staff.COL_LOCAL_ID, staffId);
        }

        String bargainingUnit = (String) getParameter(BARGAINING_UNIT_PARAM);
        if (!StringUtils.isEmpty(bargainingUnit)) {
            criteria.addEqualTo(Staff.COL_BARGAINING_UNIT, bargainingUnit);
        }

        return criteria;
    }

    /**
     * Populates the passed HashMap with attendance data for each staff member.
     *
     * @param staffMap HashMap
     * @throws SQLException exception
     */
    private void calculateAttendance(HashMap staffMap) throws SQLException {
        String sql = "SELECT SFA_STF_OID, SUM(SFA_TIME_ABSENT) as UNITS" +
                "  FROM STAFF_ATTENDANCE" +
                " WHERE SFA_DATE <= ?" +
                "   AND SFA_DATE >= ?" +
                "   AND SFA_ACCRUAL_TYPE_CODE = ?" +
                "   AND SFA_STF_OID IN ('" + StringUtils.convertCollectionToDelimitedString(m_staffOids,
                        COLLECTION_DELIMITER)
                + "') " +
                " GROUP BY SFA_STF_OID ";

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = connection.prepareStatement(sql);

        statement.setDate(1, m_asOfDate);
        statement.setDate(2, m_cutoverDate);
        statement.setString(3, m_accrualType);

        ResultSet results = null;
        try {
            results = statement.executeQuery();

            while (results.next()) {
                String staffOid = results.getString("SFA_STF_OID");
                BigDecimal units = results.getBigDecimal("UNITS");

                if (units != null) {
                    BigDecimal balance = (BigDecimal) staffMap.get(staffOid);
                    if (balance == null) {
                        balance = new BigDecimal(0);
                    }

                    balance = balance.subtract(units);

                    staffMap.put(staffOid, balance);
                }
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
    }

    /**
     * Populates the passed HashMap with the unadjusted accrual balances for each staff member.
     *
     * @param staffMap A Map of accrual-balance BigDecimal objects keyed on Staff OIDs (String
     *        objects)
     * @throws SQLException exception
     */
    private void calculateInitialBalances(HashMap staffMap) throws SQLException {
        String sql = "SELECT SFR_STF_OID, SUM(SFR_TOTAL_UNITS) as UNITS" +
                "  FROM STAFF_ACCRUAL " +
                " WHERE SFR_STF_OID IN ('" + StringUtils.convertCollectionToDelimitedString(m_staffOids,
                        COLLECTION_DELIMITER)
                + "') " +
                "   AND SFR_DATE <= ?" +
                "   AND SFR_DATE >= ?" +
                "   AND SFR_TYPE = ?" +
                " GROUP BY SFR_STF_OID " +
                " ORDER BY SFR_STF_OID ";

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = connection.prepareStatement(sql);

        statement.setDate(1, m_asOfDate);
        statement.setDate(2, m_cutoverDate);
        statement.setString(3, m_accrualType);

        ResultSet results = null;
        try {
            results = statement.executeQuery();
            while (results.next()) {
                String staffOid = results.getString("SFR_STF_OID");
                BigDecimal balance = results.getBigDecimal("UNITS");

                if (balance != null) {
                    staffMap.put(staffOid, balance);
                }
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
    }

    /**
     * Populates the passed HashMap with leave data for each staff member.
     *
     * @param staffMap HashMap
     */
    private void calculateLeaves(HashMap staffMap) {
        Boolean includeSaturday = null;
        Boolean includeSunday = null;

        includeSaturday =
                Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(), SFA_SATURDAY_INDICATOR));
        includeSunday =
                Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(), SFA_SUNDAY_INDICATOR));

        StaffAttendanceManager saManager = AttendanceManagerFactory.getStaffAttendanceManager(getBroker(),
                includeSaturday.booleanValue(), includeSunday.booleanValue());

        Criteria criteria = StaffAttendanceManager.getLeaveCriteria(m_cutoverDate,
                m_asOfDate,
                null,
                getOrganization().getOid(),
                null);
        criteria.addEqualTo(StaffLeave.COL_ACCRUAL_TYPE, m_accrualType);
        criteria.addIn(StaffLeave.COL_STAFF_OID, m_staffOids);

        QueryByCriteria leaveQuery = new QueryByCriteria(StaffLeave.class, criteria);
        QueryIterator leaves = null;
        try {
            leaves = getBroker().getIteratorByQuery(leaveQuery);
            while (leaves.hasNext()) {
                StaffLeave leave = (StaffLeave) leaves.next();

                if (leave.getAccrualType() != null) {
                    BigDecimal balance = (BigDecimal) staffMap.get(leave.getStaffOid());
                    if (balance == null) {
                        balance = new BigDecimal(0);
                    }

                    BigDecimal netLeaveDays =
                            new BigDecimal(saManager.getNetLeaveDays(leave, new PlainDate(0), m_asOfDate));

                    balance = balance.subtract(netLeaveDays);

                    staffMap.put(leave.getStaffOid(), balance);
                }
            }
        } finally {
            if (leaves != null) {
                leaves.close();
            }
        }
    }

    /**
     * Loads the staff hire dates in a map keyed to the staff OID.
     *
     * @throws SQLException exception
     */
    private void loadStaffHireDates() throws SQLException {
        String sql = "SELECT STF_OID, STF_HIRE_DATE " +
                "  FROM STAFF " +
                " WHERE STF_OID IN ('" + StringUtils.convertCollectionToDelimitedString(m_staffOids,
                        COLLECTION_DELIMITER)
                + "') ";

        Connection connection = getBroker().borrowConnection();
        PreparedStatement statement = null;
        ResultSet results = null;
        try {
            statement = connection.prepareStatement(sql);
            results = statement.executeQuery();
            while (results.next()) {
                String staffOid = results.getString("STF_OID");
                Date hireDate = results.getDate("STF_HIRE_DATE");

                m_staffHireDates.put(staffOid, hireDate != null ? new PlainDate(hireDate) : m_asOfDate);
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
    }
}
