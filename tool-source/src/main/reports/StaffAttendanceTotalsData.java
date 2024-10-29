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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Prepares the data for the "Accrual Balances" report, which displays the accrual account balances
 * in a consolidated grid. This report allows the results to be filtered based on criteria like
 * "personal balance greater than 5" or "sick balance less than or equal to 0".
 *
 * @author X2 Development Corporation
 */
public class StaffAttendanceTotalsData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Prefix for the names of the criteria operator parameters. The values are Integer objects.
     */
    public static final String CRITERIA_OPERATOR_PARAM_PREFIX = "operator_";

    /**
     * Prefix for the names of the criteria type parameters. The values are String objects.
     */
    public static final String CRITERIA_TYPE_PARAM_PREFIX = "type_";

    /**
     * Prefix for the names of the criteria value parameters. The values are java.math.BigDecimal
     * objects.
     */
    public static final String CRITERIA_VALUE_PARAM_PREFIX = "value_";

    /**
     * Name for the "date of calculation" report parameter. The value is a PlainDate.
     */
    public static final String START_DATE_PARAM = "startDate";
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Name for the enumerated "selection" report parameter. The value is a String.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" report parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" report parameter. The value is a String.
     */
    public static final String SORT_PARAM = "sort";

    // Data grid column for staff object (other columns are named by accrual type)
    private static final String COL_STAFF = "staff";

    // The number of OPERATOR-TYPE-VALUE input criteria
    private static final int NUMBER_OF_CRITERIA = 3;

    private PlainDate m_startDate;
    private PlainDate m_endDate;
    private List m_criteria;

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

        // Instantiate a HashMap into which balances will be calculated
        HashMap<String, Map<String, BigDecimal>> staffMap = new HashMap<String, Map<String, BigDecimal>>(1000);

        ReportDataGrid grid = new ReportDataGrid(1000, 10);
        QueryIterator staffIterator = null;
        try {
            // Build the list of accrual criteria
            buildCriteriaList();

            // Update with attendance
            StaffAttendanceManager.calculateAttendance(staffMap, m_startDate, m_endDate, null, getBroker());

            // Update with leaves
            calculateLeaves(staffMap);

            // Load final calculations into the ReportDataGrid for the selected staff
            Criteria staffCriteria = buildStaffCriteria();
            QueryByCriteria query = new QueryByCriteria(SisStaff.class, staffCriteria);
            applyUserSort(query, (String) getParameter(SORT_PARAM));

            staffIterator = getBroker().getIteratorByQuery(query);
            while (staffIterator.hasNext()) {
                SisStaff staff = (SisStaff) staffIterator.next();

                Map<String, BigDecimal> totals = staffMap.get(staff.getOid());
                if (totals != null && checkBalances(totals)) {
                    grid.append();
                    grid.set(COL_STAFF, staff);

                    Iterator typeIterator = totals.keySet().iterator();
                    while (typeIterator.hasNext()) {
                        String accrualType = (String) typeIterator.next();
                        if (accrualType != null && accrualType.length() > 0) {
                            grid.set(accrualType, totals.get(accrualType));
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
     * Builds the list of criteria that was entered by the user.
     */
    private void buildCriteriaList() {
        m_criteria = new LinkedList();

        for (int i = 0; i < NUMBER_OF_CRITERIA; i++) {
            String type = (String) getParameter(CRITERIA_TYPE_PARAM_PREFIX + i);
            if (type != null && type.length() > 0) {
                int operator =
                        ((Integer) getParameter(CRITERIA_OPERATOR_PARAM_PREFIX + i)).intValue();
                BigDecimal value = (BigDecimal) getParameter(CRITERIA_VALUE_PARAM_PREFIX + i);

                AccrualCriteria criteria = new AccrualCriteria(type, operator, value);
                m_criteria.add(criteria);
            }
        }
    }

    /**
     * Returns the staff criteria for this report based on the input parameters. If the current
     * staff is not null then the criteria will only return the current staff.
     *
     * @return Criteria
     */
    private Criteria buildStaffCriteria() {
        Criteria criteria = new Criteria();

        addUserCriteria(criteria, (String) getParameter(QUERY_BY_PARAM),
                (String) getParameter(QUERY_STRING_PARAM), null, X2BaseBean.COL_OID);

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(SisStaff.COL_ORGANIZATION1_OID, getOrganization().getOid());
        }

        return criteria;
    }

    /**
     * Populates the passed HashMap with the unadjusted accrual balances for each staff member.
     *
     * @param staffMap A Map of accrual-balance TreeMap objects keyed on Staff OIDs (String
     *        objects); each TreeMap maps an accrual type (String) to a balance
     *        (BigDecimal)
     */
    private void calculateLeaves(HashMap staffMap) {
        Boolean includeSaturday =
                Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                        SisPreferenceConstants.SFA_SATURDAY_INDICATOR));
        Boolean includeSunday =
                Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                        SisPreferenceConstants.SFA_SUNDAY_INDICATOR));

        StaffAttendanceManager saManager = AttendanceManagerFactory.getStaffAttendanceManager(getBroker(),
                includeSaturday.booleanValue(), includeSunday.booleanValue());

        Criteria leaveCriteria = StaffAttendanceManager.getLeaveCriteria(m_startDate,
                m_endDate,
                null,
                getOrganization().getOid(),
                null);

        QueryByCriteria leaveQuery = new QueryByCriteria(StaffLeave.class, leaveCriteria);
        QueryIterator leaves = null;
        try {
            leaves = getBroker().getIteratorByQuery(leaveQuery);
            while (leaves.hasNext()) {
                StaffLeave leave = (StaffLeave) leaves.next();

                if (leave.getAccrualType() != null) {
                    Map totalMap = (Map) staffMap.get(leave.getStaffOid());

                    if (totalMap == null) {
                        totalMap = new TreeMap();
                    }

                    BigDecimal total = (BigDecimal) totalMap.get(leave.getAccrualType());
                    if (total == null) {
                        total = new BigDecimal(0);
                    }

                    BigDecimal netLeaveDays =
                            new BigDecimal(saManager.getNetLeaveDays(leave, m_startDate, m_endDate));

                    total = total.add(netLeaveDays);

                    totalMap.put(leave.getAccrualType(), total);
                    staffMap.put(leave.getStaffOid(), totalMap);
                }
            }
        } finally {
            if (leaves != null) {
                leaves.close();
            }
        }
    }

    /**
     * Returns true as long as the accrual balances matches at least on criteria, false if no
     * criteria are matched.
     *
     * @param balances Map
     * @return boolean
     */
    private boolean checkBalances(Map balances) {
        // If there are no criteria then the balances always pass!
        boolean success = m_criteria.isEmpty();

        Iterator criteria = m_criteria.iterator();
        while (criteria.hasNext()) {
            AccrualCriteria accrualCriteria = (AccrualCriteria) criteria.next();
            if (accrualCriteria.checkBalances(balances)) {
                success = true;
                break;
            }
        }

        return success;
    }

    /**
     * Simple class for evaluating accrual balances against a criteria like "sick bank > 5 days".
     */
    private class AccrualCriteria {
        private int m_operator;
        private String m_type;
        private BigDecimal m_value;

        /**
         * Constructs an AccrualCriteria.
         *
         * @param type String
         * @param operator int
         * @param value BigDecimal
         */
        public AccrualCriteria(String type, int operator, BigDecimal value) {
            m_operator = operator;
            m_type = type;
            m_value = value;
        }

        /**
         * Returns true if the accruals pass this criteria, false otherwise.
         *
         * @param accruals a Map of accrual types (Strings) to balances (BigDecimals)
         *
         * @return boolean
         */
        public boolean checkBalances(Map accruals) {
            boolean pass = false;

            if (accruals.containsKey(m_type)) {
                BigDecimal balance = (BigDecimal) accruals.get(m_type);

                switch (m_operator) {
                    case 0: // Equals
                        pass = balance.compareTo(m_value) == 0;
                        break;

                    case 1: // Greater than
                        pass = balance.compareTo(m_value) > 0;
                        break;

                    case 2: // Greater than or equal to
                        pass = balance.compareTo(m_value) >= 0;
                        break;

                    case 3: // Less than
                        pass = balance.compareTo(m_value) < 0;
                        break;

                    case 4: // Less than or equal to
                        pass = balance.compareTo(m_value) <= 0;
                        break;
                }
            }

            return pass;
        }
    }
}
