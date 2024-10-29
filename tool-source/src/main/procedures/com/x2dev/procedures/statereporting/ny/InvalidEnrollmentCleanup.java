/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.ObjectUtils;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2004 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
// import static com.x2dev.sis.model.business.ModelProperty.PATH_DELIMITER;
// import static com.x2dev.sis.model.beans.SystemPreferenceDefinition.STUDENT_ACTIVE_CODE;
import java.util.Collection;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to go through all student's enrollment records and make sure they do not have any
 * invalid enrollment.
 * <p>
 * This will perform the following 3 tasks:
 * <ul>
 * <li>Add withdrawals for all schools that is not the student's primary school if none exist
 * <li>Add entries for the student's primary school if none exist
 * <li>Add withdrawal for the student's primary if they are not active and none exist
 *
 * @author X2 Development Corporation
 */
public class InvalidEnrollmentCleanup extends ProcedureJavaSource {
    /**
     * Name for the enumerated "query by" input parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "query string" input parameter. The value is a String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    private static final String ENROLLMENT_REASON = "Data cleanup";
    private static final String ENTRY_CODE = "X2E";
    private static final String WITHDRAWAL_CODE = "X2W";

    private String m_activeCode;
    private CalendarManager m_calendarManager;
    private int m_insertCount = 0;

    private Map<String, Collection<StudentEnrollment>> m_enrollmentMap;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
        m_calendarManager = new CalendarManager(getBroker());

        loadEnrollment();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(Student.COL_ORGANIZATION1_OID, getOrganization().getOid());

        if (isSchoolContext()) {
            criteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        }

        int queryBy = (Integer.valueOf((String) getParameter(QUERY_BY_PARAM))).intValue();
        switch (queryBy) {
            case 1: // YOG
                criteria.addEqualTo(Student.COL_YOG, getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // LASID
                criteria.addEqualTo(Student.COL_LOCAL_ID, getParameter(QUERY_STRING_PARAM));
                break;

            default: // None
                break;
        }

        QueryByCriteria query = new QueryByCriteria(Student.class, criteria);

        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            StudentEnrollment entry = null;
            StudentEnrollment withdrawal = null;

            while (iterator.hasNext()) {
                Student student = (Student) iterator.next();
                School primarySchool = student.getSchool();

                Collection<StudentEnrollment> enrollments = m_enrollmentMap.get(student.getOid());
                if (CollectionUtils.isEmpty(enrollments) &&
                        primarySchool != null && !primarySchool.getArchiveIndicator()
                        && !primarySchool.getInactiveIndicator()) {
                    // Create entry records
                    createEnrollment(student,
                            primarySchool,
                            ENTRY_CODE,
                            getCurrentContext().getStartDate(),
                            StudentEnrollment.ENTRY,
                            m_activeCode,
                            getCurrentContext().getStartDate().getTime(),
                            student.getYog());

                    // If student is not active, create withdrawal record
                    if (!StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus())) {
                        createEnrollment(student,
                                primarySchool,
                                WITHDRAWAL_CODE,
                                getCurrentContext().getStartDate(),
                                StudentEnrollment.WITHDRAWAL,
                                student.getEnrollmentStatus(),
                                getCurrentContext().getStartDate().getTime() + 1,
                                student.getYog());
                    }
                } else if (!CollectionUtils.isEmpty(enrollments)) {
                    School lastSchool = null;
                    for (StudentEnrollment enrollment : enrollments) {
                        School school = enrollment.getSchool();

                        if (!ObjectUtils.match(school, lastSchool)) {
                            if (lastSchool != null) {
                                assessEnrollments(student, lastSchool, entry, withdrawal, enrollment);
                            }

                            entry = null;
                            withdrawal = null;
                        }

                        if (StudentEnrollment.ENTRY.equals(enrollment.getEnrollmentType())) {
                            entry = enrollment;
                        } else if (StudentEnrollment.WITHDRAWAL.equals(enrollment.getEnrollmentType())) {
                            withdrawal = enrollment;
                        }

                        lastSchool = school;
                    }

                    // assess records for the last enrollment
                    assessEnrollments(student, lastSchool, entry, withdrawal, null);
                }
            }
        } finally {
            iterator.close();
        }

        logMessage("Created " + m_insertCount + " Student Enrollment records");
    }

    /**
     * Look at the given information and determines what new enrollment records to create.
     *
     * @param student Student
     * @param school School
     * @param entry StudentEnrollment
     * @param withdrawal StudentEnrollment
     * @param next StudentEnrollment
     */
    private void assessEnrollments(Student student,
                                   School school,
                                   StudentEnrollment entry,
                                   StudentEnrollment withdrawal,
                                   StudentEnrollment next) {
        if (!student.getSchool().equals(school)) {
            if (entry != null && withdrawal == null) {
                PlainDate date = null;
                long timeStamp = 0;

                if (next != null && StudentEnrollment.ENTRY.equals(next.getEnrollmentType())) {
                    date = next.getEnrollmentDate();
                    timeStamp = next.getTimestamp() - 1;
                } else {
                    DistrictSchoolYearContext context =
                            m_calendarManager.getDistrictContext(entry.getEnrollmentDate(), getOrganization().getOid());
                    if (context != null) {
                        date = context.getEndDate();
                        timeStamp = date.getTime();
                    }
                }

                if (date != null) {
                    createEnrollment(student,
                            school,
                            WITHDRAWAL_CODE,
                            date,
                            StudentEnrollment.WITHDRAWAL,
                            "Inactive",
                            timeStamp,
                            entry.getYog());
                }
            } else if (withdrawal != null && entry == null) {
                DistrictSchoolYearContext context = m_calendarManager.getDistrictContext(withdrawal.getEnrollmentDate(),
                        getOrganization().getOid());
                if (context != null) {
                    PlainDate date = context.getStartDate();
                    long timeStamp = date.getTime();

                    createEnrollment(student,
                            school,
                            ENTRY_CODE,
                            date,
                            StudentEnrollment.ENTRY,
                            m_activeCode,
                            timeStamp,
                            withdrawal.getYog());
                }
            } else if (withdrawal != null && entry != null && !isBefore(entry, withdrawal)) {
                PlainDate date = entry.getEnrollmentDate();
                long timestamp = entry.getTimestamp() + 1;

                createEnrollment(student,
                        school,
                        WITHDRAWAL_CODE,
                        date,
                        StudentEnrollment.WITHDRAWAL,
                        "Inactive",
                        timestamp,
                        student.getYog());
            }

            if (next == null && !student.getSchool().getArchiveIndicator()
                    && !student.getSchool().getInactiveIndicator()) {
                PlainDate date = null;
                PlainDate contextStart = getCurrentContext().getStartDate();
                PlainDate lastDate = null;
                long timeStamp = 0;

                if (withdrawal != null) {
                    lastDate = withdrawal.getEnrollmentDate();
                    timeStamp = withdrawal.getTimestamp() + 1;
                } else if (entry != null) {
                    DistrictSchoolYearContext context =
                            m_calendarManager.getDistrictContext(entry.getEnrollmentDate(), getOrganization().getOid());
                    if (context != null) {
                        lastDate = context.getEndDate();
                        timeStamp = lastDate.getTime();
                    }
                }

                if (lastDate == null || contextStart.after(lastDate)) {
                    date = contextStart;
                } else {
                    date = lastDate;
                }

                createEnrollment(student,
                        student.getSchool(),
                        ENTRY_CODE,
                        date,
                        StudentEnrollment.ENTRY,
                        m_activeCode,
                        timeStamp,
                        student.getYog());

                // If student is not active, also create withdrawal
                if (!StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus())) {
                    createEnrollment(student,
                            student.getSchool(),
                            WITHDRAWAL_CODE,
                            date,
                            StudentEnrollment.WITHDRAWAL,
                            student.getEnrollmentStatus(),
                            timeStamp + 2,
                            student.getYog());
                }
            }
        } else if (!school.getArchiveIndicator() && !school.getInactiveIndicator()) {
            if (entry == null && withdrawal != null) {
                DistrictSchoolYearContext context = m_calendarManager.getDistrictContext(withdrawal.getEnrollmentDate(),
                        getOrganization().getOid());
                if (context != null) {
                    PlainDate date = context.getStartDate();
                    long timeStamp = date.getTime();

                    createEnrollment(student,
                            school,
                            ENTRY_CODE,
                            date,
                            StudentEnrollment.ENTRY,
                            m_activeCode,
                            timeStamp,
                            withdrawal.getYog());
                }
            } else if (StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus()) &&
                    entry != null &&
                    withdrawal != null &&
                    entry.getEnrollmentDate().before(withdrawal.getEnrollmentDate())) {
                PlainDate date = withdrawal.getEnrollmentDate();
                long timeStamp = withdrawal.getTimestamp() + 1;

                createEnrollment(student,
                        school,
                        ENTRY_CODE,
                        date,
                        StudentEnrollment.ENTRY,
                        m_activeCode,
                        timeStamp,
                        withdrawal.getYog());
            }

            if (!StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus()) && withdrawal == null
                    && entry != null) {
                DistrictSchoolYearContext context =
                        m_calendarManager.getDistrictContext(entry.getEnrollmentDate(), getOrganization().getOid());
                if (context != null) {
                    PlainDate date = context.getEndDate();
                    long timeStamp = date.getTime();

                    createEnrollment(student,
                            school,
                            WITHDRAWAL_CODE,
                            date,
                            StudentEnrollment.WITHDRAWAL,
                            student.getEnrollmentStatus(),
                            timeStamp,
                            entry.getYog());
                }
            } else if (StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus()) && next != null
                    && entry != null) {
                PlainDate date = next.getEnrollmentDate();
                long timeStamp = next.getTimestamp() - 1;

                createEnrollment(student,
                        school,
                        WITHDRAWAL_CODE,
                        date,
                        StudentEnrollment.WITHDRAWAL,
                        "Inactive",
                        timeStamp,
                        entry.getYog());
            }
        }
    }

    /**
     * Creates and saves a StudentEnrollment bean based on the given input.
     *
     * @param student Student
     * @param school School
     * @param enrollmentCode String
     * @param date PlainDate
     * @param type String
     * @param status String
     * @param timestamp long
     * @param yog int
     */
    private void createEnrollment(Student student,
                                  School school,
                                  String enrollmentCode,
                                  PlainDate date,
                                  String type,
                                  String status,
                                  long timestamp,
                                  int yog) {
        StudentEnrollment enrollment = new StudentEnrollment(getBroker().getPersistenceKey());
        enrollment.setSchoolOid(school.getOid());
        enrollment.setStudentOid(student.getOid());

        enrollment.setEnrollmentCode(enrollmentCode);
        enrollment.setEnrollmentDate(date);
        enrollment.setEnrollmentType(type);

        enrollment.setReasonCode(ENROLLMENT_REASON);
        enrollment.setStatusCode(status);
        enrollment.setTimestamp(timestamp);
        enrollment.setYog(yog);

        getBroker().saveBeanForced(enrollment);
        m_insertCount++;
    }

    /**
     * Compares 2 enrollment records to see if the first occurred before the second. It first checks
     * the enrollment date, then if necessary, the timestamp.
     *
     * @param enrollment1 StudentEnrollment
     * @param enrollment2 StudentEnrollment
     * @return boolean
     */
    private boolean isBefore(StudentEnrollment enrollment1, StudentEnrollment enrollment2) {
        boolean isBefore = false;

        PlainDate date1 = enrollment1.getEnrollmentDate();
        PlainDate date2 = enrollment2.getEnrollmentDate();

        if (date1.before(date2)) {
            isBefore = true;
        } else if (date1.equals(date2)) {
            long timestamp1 = enrollment1.getTimestamp();
            long timestamp2 = enrollment2.getTimestamp();

            if (timestamp1 < timestamp2) {
                isBefore = true;
            }
        }

        return isBefore;
    }

    /**
     * Load the enrollment records into a map keyed to the student OID.
     */
    private void loadEnrollment() {
        Criteria criteria = new Criteria();


        if (isSchoolContext()) {
            criteria.addEqualTo(StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    Student.COL_SCHOOL_OID, getSchool().getOid());
        }

        int queryBy = (Integer.valueOf((String) getParameter(QUERY_BY_PARAM))).intValue();
        switch (queryBy) {
            case 1: // YOG
                criteria.addEqualTo(StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                        Student.COL_YOG, getParameter(QUERY_STRING_PARAM));
                break;

            case 2: // LASID
                criteria.addEqualTo(StudentEnrollment.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                        Student.COL_LOCAL_ID, getParameter(QUERY_STRING_PARAM));
                break;

            default: // None
                break;
        }

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        query.addOrderByAscending(StudentEnrollment.COL_ENROLLMENT_DATE);
        query.addOrderByAscending(StudentEnrollment.COL_TIMESTAMP);
        query.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_TYPE);

        m_enrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 10000);
    }
}
