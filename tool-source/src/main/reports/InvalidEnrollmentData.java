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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Data source for the "Invalid Enrollments" report. This report lists students whose enrollment
 * records are inconsistent with their current school and/or enrollment status.
 * <p>
 * Enrollments are considered inconsistent if any of the following apply:
 * <ul>
 * <li>The latest record is an entry, and the student's enrollment status is not active
 * <li>The latest record is an entry, and the student's school is different than the school on the
 * entry record
 * <li>The latest record is an entry, and the student is in the archive school
 * <li>The latest record is a withdrawal, and the student' enrollment status is active
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class InvalidEnrollmentData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    // Input parameters
    public static final String INACTIVE_MEMBER = "inactiveMember";
    public static final String ACTIVE_NONMEMBER = "activeNonmember";
    public static final String SCHOOL_DISCREPANCY = "schoolDiscrepancy";
    public static final String ARCHIVED_MEMBER = "archivedMember";
    public static final String INCLUDE_ARCHIVE_SCHOOL = "includeArchive";

    // Grid field constants
    public static final String FIELD_STUDENT = "student";
    public static final String FIELD_REASON_1 = "reason1";
    public static final String FIELD_REASON_2 = "reason2";
    public static final String FIELD_REASON_3 = "reason3";

    // Reason constants
    public static final int REASON_INACTIVE_MEMBER = 100;
    public static final int REASON_SCHOOL_MEMBERSHIP_DISCREPANCY = 200;
    public static final int REASON_ARCHIVED_MEMBER = 300;
    public static final int REASON_ACTIVE_NONMEMBER = 400;

    private HashMap m_studentStatusLookup;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        Boolean archivedMember = (Boolean) getParameter(ARCHIVED_MEMBER);
        Boolean activeNonmember = (Boolean) getParameter(ACTIVE_NONMEMBER);
        Boolean inactiveMember = (Boolean) getParameter(INACTIVE_MEMBER);
        Boolean schoolDiscrepancy = (Boolean) getParameter(SCHOOL_DISCREPANCY);
        Boolean includeArchiveSchool = (Boolean) getParameter(INCLUDE_ARCHIVE_SCHOOL);

        String archiveSchoolOid = getArchiveSchoolOid();

        ReportDataGrid grid = new ReportDataGrid(500, 5);

        loadStudentStatusLookup();

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE,
                StudentEnrollment.ENTRY);
        enrollmentCriteria.addOrEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE,
                StudentEnrollment.WITHDRAWAL);

        if (!includeArchiveSchool.booleanValue()) {
            enrollmentCriteria.addEqualTo(
                    StudentEnrollment.REL_STUDENT + "." + SisStudent.REL_SCHOOL + "." + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class,
                enrollmentCriteria);
        enrollmentQuery.addOrderByAscending(
                StudentEnrollment.REL_STUDENT + "." + SisStudent.REL_SCHOOL + "." + SisSchool.COL_NAME);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
        enrollmentQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);

        String lastStudentOid = null;
        QueryIterator enrollmentIterator = null;
        try {
            enrollmentIterator = getBroker().getIteratorByQuery(enrollmentQuery);
            while (enrollmentIterator.hasNext()) {
                StudentEnrollment enrollment = (StudentEnrollment) enrollmentIterator.next();
                if (lastStudentOid == null || !lastStudentOid.equals(enrollment.getStudentOid())) {
                    StudentStatusInfo statusInfo =
                            (StudentStatusInfo) m_studentStatusLookup.get(enrollment.getStudentOid());

                    boolean errorFound = false;
                    List reasons = new ArrayList(3);

                    if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                        if (!StudentManager.isActiveStudent(getOrganization(), statusInfo.enrollmentStatus)
                                && inactiveMember.booleanValue()) {
                            errorFound = true;
                            reasons.add(Integer.valueOf(REASON_INACTIVE_MEMBER));
                        }

                        if (enrollment.getSchoolOid() != null &&
                                !enrollment.getSchoolOid().equals(statusInfo.schoolOid) &&
                                schoolDiscrepancy.booleanValue()) {
                            errorFound = true;
                            reasons.add(Integer.valueOf(REASON_SCHOOL_MEMBERSHIP_DISCREPANCY));
                        }

                        if (statusInfo.schoolOid.equals(archiveSchoolOid) &&
                                archivedMember.booleanValue()) {
                            errorFound = true;
                            reasons.add(Integer.valueOf(REASON_ARCHIVED_MEMBER));
                        }
                    } else // Withdrawal
                    {
                        if (StudentManager.isActiveStudent(getOrganization(), statusInfo.enrollmentStatus)
                                && activeNonmember.booleanValue()) {
                            errorFound = true;
                            reasons.add(Integer.valueOf(REASON_ACTIVE_NONMEMBER));
                        }
                    }

                    if (errorFound) {
                        grid.append();
                        grid.set(FIELD_STUDENT, enrollment.getStudent());
                        grid.set(FIELD_REASON_1, reasons.get(0));
                        if (reasons.size() >= 2) {
                            grid.set(FIELD_REASON_2, reasons.get(1));
                        }
                        if (reasons.size() >= 3) {
                            grid.set(FIELD_REASON_3, reasons.get(2));
                        }
                    }
                }

                lastStudentOid = enrollment.getStudentOid();
            }
        } finally {
            if (enrollmentIterator != null) {
                enrollmentIterator.close();
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Release resources.
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#releaseResources()
     */
    @Override
    protected void releaseResources() {
        super.releaseResources();

        m_studentStatusLookup.clear();
        m_studentStatusLookup = null;
    }

    /**
     * Returns the OID of the archive school for the current district, if one exists.
     *
     * @return String
     */
    private String getArchiveSchoolOid() {
        String oid = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchool.COL_ORGANIZATION1_OID, getOrganization().getOid());
        criteria.addEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        QueryByCriteria query = new QueryByCriteria(SisSchool.class, criteria);
        SisSchool archiveSchool = (SisSchool) getBroker().getBeanByQuery(query);

        if (archiveSchool != null) {
            oid = archiveSchool.getOid();
        }

        return oid;
    }

    /**
     * Loads the student status lookup map. Each element of the map is populated with an instance
     * of StudentStatusInfo.
     */
    private void loadStudentStatusLookup() {
        m_studentStatusLookup = new HashMap(5000);

        /*
         * Use a report query to retrieve this info -- less overhead than iterating over all student
         * beans in the system
         */
        ReportQueryByCriteria query = new ReportQueryByCriteria(SisStudent.class,
                new String[] {X2BaseBean.COL_OID,
                        SisStudent.COL_ENROLLMENT_STATUS,
                        SisStudent.COL_SCHOOL_OID},
                new Criteria());

        ReportQueryIterator iterator = null;
        try {
            iterator = getBroker().getReportQueryIteratorByQuery(query);
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                StudentStatusInfo statusInfo = new StudentStatusInfo();
                statusInfo.enrollmentStatus = (String) row[1];
                statusInfo.schoolOid = (String) row[2];

                m_studentStatusLookup.put(row[0], statusInfo);
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }
    }

    /**
     * Helper type that holds student status information.
     *
     * @author X2 Development Corporation
     */
    private class StudentStatusInfo {
        public String enrollmentStatus;
        public String schoolOid;

        /**
         * Constructs a StudentStatusInfo.
         */
        StudentStatusInfo() {
            // Increase the visibility of the constructor to improve performance.
        }
    }
}
