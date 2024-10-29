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

import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.RefAttendanceStaff;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.admin.SisReferenceManager;
import com.x2dev.sis.model.business.attendance.AttendanceManagerFactory;
import com.x2dev.sis.model.business.attendance.StaffAttendanceHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the Staff Absence Totals report, which counts district-wide staff absences broken
 * down by accrual type, reason, and bargaining unit.
 *
 * @author X2 Development Corporation
 */
public class StaffAbsenceTotalsData extends ReportJavaSourceNet {
    /** Last date in the report date range */
    public static final String END_DATE_PARAM = "endDate";

    /** First date in the report date range */
    public static final String START_DATE_PARAM = "startDate";

    // Report field constants
    private static final String COUNT = "count";
    private static final String COUNT_LABEL = "countLabel";
    private static final String COUNT_TYPE_FIELD = "countType"; // Bargaining unit, reason, or
                                                                // accrual type
    private static final String SUBCOUNT_TYPE_FIELD = "subcountType";

    // Number of fields
    private static final int FIELD_COUNT = 4;

    // Count type constants
    private static final Integer ACCRUAL_TYPE = Integer.valueOf(0);
    private static final Integer BARGAINING_UNIT_TYPE = Integer.valueOf(1);
    private static final Integer REASON_TYPE = Integer.valueOf(2);

    private static final long serialVersionUID = 1L;

    /**
     * Returns a data source with the following fields:
     * <ul>
     * <li>countLabel: String; an accrual type, bargaining unit, or reason code
     * <li>countType: Integer; 0 for accrual type, 1 for bargaining unit, 2 for reason code
     * <li>count: Integer; the count value
     * </ul>
     * .
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);

        Boolean includeSaturday =
                Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                        SisPreferenceConstants.SFA_SATURDAY_INDICATOR));

        Boolean includeSunday =
                Boolean.valueOf(PreferenceManager.getPreferenceValue(getOrganization(),
                        SisPreferenceConstants.SFA_SUNDAY_INDICATOR));

        StaffAttendanceManager saManager =
                AttendanceManagerFactory.getStaffAttendanceManager(getBroker(), includeSaturday.booleanValue(),
                        includeSunday.booleanValue());

        HashSet codeOids = getAbsenceOids();

        // Query for all the attendance in the specified date range with absent codes
        ArrayList sortOrder = new ArrayList();
        sortOrder.add(StaffAttendanceHelper.REL_STAFF + "." + SisStaff.COL_SCHOOL_OID);

        Map accrualCounts = new TreeMap();
        Map bunitCounts = new TreeMap();
        Map reasonCounts = new TreeMap();

        Map reasonBunitCounts = new HashMap();

        try {
            Collection attendance = saManager.getAttendance(startDate, endDate, sortOrder);
            Iterator attendanceIterator = attendance.iterator();
            while (attendanceIterator.hasNext()) {
                StaffAttendanceHelper helper = (StaffAttendanceHelper) attendanceIterator.next();

                // Only include absences
                if ((helper.isLeave() || codeOids.contains(helper.getAttendance().getReferenceAttendanceOid()))
                        && helper.getStaff() != null) {
                    /*
                     * Update the accrual counts
                     */
                    String accrualKey = helper.getAccrualType() != null ? helper.getAccrualType() : "";
                    Integer accrualCount = (Integer) accrualCounts.get(accrualKey);
                    if (accrualCount == null) {
                        accrualCount = Integer.valueOf(0);
                    }
                    accrualCount = Integer.valueOf(accrualCount.intValue() + 1);
                    accrualCounts.put(accrualKey, accrualCount);

                    /*
                     * Update the bargaining unit counts
                     */
                    String bunitKey =
                            helper.getStaff().getBargainingUnit() != null ? helper.getStaff().getBargainingUnit() : "";

                    Integer bunitCount = (Integer) bunitCounts.get(bunitKey);
                    if (bunitCount == null) {
                        bunitCount = Integer.valueOf(0);
                    }
                    bunitCount = Integer.valueOf(bunitCount.intValue() + 1);
                    bunitCounts.put(bunitKey, bunitCount);

                    /*
                     * Update the reason counts, which are further broken down by bargaining unit.
                     * These sub-counts are stored in the resonBunitCounts HashMap.
                     */
                    String reasonKey = helper.getReason() != null ? helper.getReason() : "";
                    Integer reasonCount = (Integer) reasonCounts.get(reasonKey);
                    if (reasonCount == null) {
                        reasonCount = Integer.valueOf(0);
                    }
                    reasonCount = Integer.valueOf(reasonCount.intValue() + 1);
                    reasonCounts.put(reasonKey, reasonCount);

                    TreeMap bunitSubCounts = (TreeMap) reasonBunitCounts.get(reasonKey);
                    if (bunitSubCounts == null) {
                        bunitSubCounts = new TreeMap();
                    }
                    Integer bunitSubCount = (Integer) bunitSubCounts.get(bunitKey);
                    if (bunitSubCount == null) {
                        bunitSubCount = Integer.valueOf(0);
                    }
                    bunitSubCount = Integer.valueOf(bunitSubCount.intValue() + 1);
                    bunitSubCounts.put(bunitKey, bunitSubCount);
                    reasonBunitCounts.put(reasonKey, bunitSubCounts);
                }
            }
        } catch (X2BaseException xbe) {
            AppGlobals.getLog().log(Level.WARNING, xbe.getMessage(), xbe);
        }

        ReportDataGrid reportData = new ReportDataGrid(50, FIELD_COUNT);

        // Populate the data grid
        Iterator accrualIterator = accrualCounts.keySet().iterator();
        while (accrualIterator.hasNext()) {
            Object accrualType = accrualIterator.next();

            reportData.append();
            reportData.set(COUNT_TYPE_FIELD, ACCRUAL_TYPE);
            reportData.set(SUBCOUNT_TYPE_FIELD, Integer.valueOf(-1));
            reportData.set(COUNT_LABEL, accrualType);
            reportData.set(COUNT, accrualCounts.get(accrualType));
        }

        Iterator bunitIterator = bunitCounts.keySet().iterator();
        while (bunitIterator.hasNext()) {
            Object bargainingUnit = bunitIterator.next();

            reportData.append();
            reportData.set(COUNT_TYPE_FIELD, BARGAINING_UNIT_TYPE);
            reportData.set(SUBCOUNT_TYPE_FIELD, Integer.valueOf(-1));
            reportData.set(COUNT_LABEL, bargainingUnit);
            reportData.set(COUNT, bunitCounts.get(bargainingUnit));
        }

        Iterator reasonIterator = reasonCounts.keySet().iterator();
        while (reasonIterator.hasNext()) {
            Object reason = reasonIterator.next();

            reportData.append();
            reportData.set(COUNT_TYPE_FIELD, REASON_TYPE);
            reportData.set(SUBCOUNT_TYPE_FIELD, Integer.valueOf(-1));
            reportData.set(COUNT_LABEL, reason);
            reportData.set(COUNT, reasonCounts.get(reason));

            TreeMap bunitSubCounts = (TreeMap) reasonBunitCounts.get(reason);
            bunitIterator = bunitSubCounts.keySet().iterator();
            while (bunitIterator.hasNext()) {
                Object bargainingUnit = bunitIterator.next();

                reportData.append();
                reportData.set(COUNT_TYPE_FIELD, REASON_TYPE);
                reportData.set(SUBCOUNT_TYPE_FIELD, BARGAINING_UNIT_TYPE);
                reportData.set(COUNT_LABEL, bargainingUnit);
                reportData.set(COUNT, bunitSubCounts.get(bargainingUnit));
            }
        }

        reportData.beforeTop();
        return reportData;
    }

    /**
     * Returns a HashSet of RefAttendanceStaff OIDs corresponding to absence codes.
     * 
     * @return HashSet
     */
    private HashSet getAbsenceOids() {
        // Create a subquery to get all the codes OIDs that indicate an absence
        String dictionaryOid = getExtendedDictionary() != null ? getExtendedDictionary().getOid() : null;
        Criteria codeCriteria =
                SisReferenceManager.getStaffAttendanceCodesCriteria(getOwnableCriteria(),
                        dictionaryOid,
                        getBroker().getPersistenceKey());

        codeCriteria.addEqualTo(RefAttendanceStaff.COL_ABSENT_INDICATOR, Boolean.TRUE);

        ReportQueryByCriteria codeQuery =
                new ReportQueryByCriteria(RefAttendanceStaff.class,
                        new String[] {X2BaseBean.COL_OID},
                        codeCriteria);

        HashSet codeOids = new HashSet(10);
        ReportQueryIterator codeOidIterator = null;
        try {
            codeOidIterator = getBroker().getReportQueryIteratorByQuery(codeQuery);
            while (codeOidIterator.hasNext()) {
                String codeOid = (String) ((Object[]) codeOidIterator.next())[0];
                if (codeOid != null) {
                    codeOids.add(codeOid.trim());
                }
            }
        } finally {
            if (codeOidIterator != null) {
                codeOidIterator.close();
            }
        }

        return codeOids;
    }
}
