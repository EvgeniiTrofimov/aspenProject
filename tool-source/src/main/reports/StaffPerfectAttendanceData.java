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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.reports.QueryIteratorDataSource;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.RefAttendanceStaff;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.sis.model.business.StaffAttendanceManager;
import com.x2dev.sis.model.business.admin.SisReferenceManager;
import com.x2dev.utils.types.PlainDate;
import net.sf.jasperreports.engine.JRDataSource;
import net.sf.jasperreports.engine.JREmptyDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Class that prepares the data for the Staff Perfect Attendance report, which generates a list of
 * staff members with perfect attendance during a given date range.
 *
 * @author X2 Development Corporation
 */
public class StaffPerfectAttendanceData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Report input parameter for the end date in the attendance date range. The value is a
     * PlainDate object.
     */
    public static final String END_DATE_PARAM = "endDate";

    /**
     * Report input parameter for the start date in the attendance date range. The value is a
     * PlainDate object.
     */
    public static final String START_DATE_PARAM = "startDate";

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        JRDataSource staff = null;

        PlainDate startDate = (PlainDate) getParameter(START_DATE_PARAM);
        PlainDate endDate = (PlainDate) getParameter(END_DATE_PARAM);
        if (startDate != null && endDate != null) {
            // Create the attendance subquery
            Criteria attendanceCriteria =
                    StaffAttendanceManager.getAttendanceCriteria(startDate, endDate, null, getOrganization().getOid(),
                            null);

            attendanceCriteria.addEqualToField(StaffAttendance.COL_STAFF_OID,
                    Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);

            // Filter by reference codes which are absent, not excused, and matching the current
            // context
            attendanceCriteria.addIn(StaffAttendance.COL_REFERENCE_ATTENDANCE_OID, getStaffAttendanceCodesSubQuery());

            QueryByCriteria attendanceQuery =
                    new ReportQueryByCriteria(StaffAttendance.class,
                            new String[] {X2BaseBean.COL_OID}, attendanceCriteria);

            // Create the leave subquery
            Criteria leaveCriteria =
                    StaffAttendanceManager.getLeaveCriteria(startDate, endDate, null, getOrganization().getOid(), null);
            leaveCriteria.addEqualToField(StaffAttendance.COL_STAFF_OID,
                    Criteria.PARENT_QUERY_PREFIX + X2BaseBean.COL_OID);

            QueryByCriteria leaveQuery =
                    new ReportQueryByCriteria(StaffLeave.class,
                            new String[] {X2BaseBean.COL_OID}, leaveCriteria);

            // Assemble the overall query
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STAFF_ACTIVE_CODE);

            Criteria staffCriteria = new Criteria();
            staffCriteria.addEqualTo(SisStaff.COL_STATUS, activeCode);
            staffCriteria.addNotExists(attendanceQuery);
            staffCriteria.addNotExists(leaveQuery);

            if (isSchoolContext()) {
                staffCriteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
            }

            // Run the query
            QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);

            if (!isSchoolContext()) {
                staffQuery.setPathOuterJoin(SisStaff.REL_SCHOOL);

                staffQuery.addOrderByAscending(SisStaff.REL_SCHOOL + "." + SisSchool.COL_NAME);
                staffQuery.addOrderByAscending(SisStaff.COL_SCHOOL_OID);
            }

            staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);

            staff = new QueryIteratorDataSource(getBroker().getIteratorByQuery(staffQuery));
        } else {
            staff = new JREmptyDataSource();
        }

        return staff;
    }

    /**
     * Returns a subquery to retrieve RefAttendanceStaff OIDs for codes
     * which are absent and not excused.
     *
     * @return SubQuery
     */
    private SubQuery getStaffAttendanceCodesSubQuery() {
        Criteria codeCriteria =
                SisReferenceManager.getStaffAttendanceCodesCriteria(getOwnableCriteria(),
                        getExtendedDictionary() != null ? getExtendedDictionary().getOid() : null,
                        getBroker().getPersistenceKey());

        codeCriteria.addEqualTo(RefAttendanceStaff.COL_ABSENT_INDICATOR, Boolean.TRUE);
        codeCriteria.addNotEqualTo(RefAttendanceStaff.COL_EXCUSED_INDICATOR, Boolean.TRUE);

        return new SubQuery(RefAttendanceStaff.class, X2BaseBean.COL_OID, codeCriteria);
    }
}
