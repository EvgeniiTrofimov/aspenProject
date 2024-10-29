/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This procedure will process all active students with no enrollment activity over the summer.
 * It will and a withdrawal and entry for end of school year and first day of next school year.
 *
 * @author X2 Development Corporation
 */
public class StudentExitEntry extends ProcedureJavaSource {
    private static final String ALIAS_BOY_ENTRY_EXIT = "BOY_ENTRY_EXIT";
    private static final String ALIAS_DISTRICT_HOME = "DOE DISTRICT HOME";
    private static final String ALIAS_SCHOOL_HOME = "DOE SCHOOL HOME";

    private static final String PARAM_REPORT_TYPE = "reportType";

    private static final String REPORT_TYPE_VALUE_ADD = "add";
    // private static final String REPORT_TYPE_VALUE_MISSING = "show";

    private String m_fieldEntryExitInd;
    private String m_fieldDistrictHome;
    private String m_fieldSchoolHome;
    private String m_reportType;
    private Map<String, List<StudentEnrollment>> m_enrollmentMap;
    private Map<String, List<StudentEnrollment>> m_allEnrollmentMap;
    private PlainDate m_entryDate;
    private PlainDate m_withdrawalDate;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        int addCount = 0;
        int missingCount = 0;
        java.util.Date now = new java.util.Date();

        initializeFields();

        // Get student active code.
        String activeStatus =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        // Load enrollments for all active students.
        loadEnrollments();

        // Select all active students.
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        QueryByCriteria query = new QueryByCriteria(SisStudent.class, criteria);
        query.addOrderBy(SisStudent.COL_NAME_VIEW, true);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                SisStudent student = (SisStudent) iterator.next();
                StudentEnrollment preYogOnly = isOnylYog(student);
                StudentEnrollment presummer = lastRecord(student);
                List<StudentEnrollment> enrollments = m_enrollmentMap.get(student.getOid());
                if (enrollments == null && REPORT_TYPE_VALUE_ADD.equals(m_reportType)) {
                    // Student is active and has no enrollment records in the summer.
                    StudentEnrollment exitEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, getBroker().getPersistenceKey());
                    StudentEnrollment entryEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, getBroker().getPersistenceKey());

                    exitEnrollment.setStudentOid(student.getOid());
                    exitEnrollment.setSchoolOid(student.getSchoolOid());
                    exitEnrollment.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
                    exitEnrollment.setEnrollmentCode("W201");
                    exitEnrollment.setEnrollmentDate(m_withdrawalDate);
                    exitEnrollment.setTimestamp(now.getTime());
                    exitEnrollment.setStatusCode("Inactive");
                    exitEnrollment.setYog(student.getYog());
                    exitEnrollment.setFieldValueByBeanPath(m_fieldEntryExitInd, BooleanAsStringConverter.TRUE);

                    entryEnrollment.setStudentOid(student.getOid());
                    entryEnrollment.setSchoolOid(student.getSchoolOid());
                    entryEnrollment.setEnrollmentType(StudentEnrollment.ENTRY);
                    entryEnrollment.setEnrollmentCode("E119");
                    entryEnrollment.setEnrollmentDate(m_entryDate);
                    entryEnrollment.setTimestamp(now.getTime());
                    entryEnrollment.setStatusCode(activeStatus);
                    entryEnrollment.setYog(student.getYog());
                    entryEnrollment.setFieldValueByBeanPath(m_fieldEntryExitInd, BooleanAsStringConverter.TRUE);
                    if (presummer != null) {
                        entryEnrollment.setFieldValueByBeanPath(m_fieldDistrictHome,
                                presummer.getFieldValueByBeanPath(m_fieldDistrictHome));
                        entryEnrollment.setFieldValueByBeanPath(m_fieldSchoolHome,
                                presummer.getFieldValueByBeanPath(m_fieldSchoolHome));
                    }
                    getBroker().saveBeanForced(exitEnrollment);
                    getBroker().saveBeanForced(entryEnrollment);
                    addCount++;
                } else if (preYogOnly != null && REPORT_TYPE_VALUE_ADD.equals(m_reportType)) {
                    // Student has only YOG in the summer.
                    StudentEnrollment exitEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, getBroker().getPersistenceKey());
                    StudentEnrollment entryEnrollment =
                            X2BaseBean.newInstance(StudentEnrollment.class, getBroker().getPersistenceKey());

                    exitEnrollment.setStudentOid(student.getOid());
                    exitEnrollment.setSchoolOid(student.getSchoolOid());
                    exitEnrollment.setEnrollmentType(StudentEnrollment.WITHDRAWAL);
                    exitEnrollment.setEnrollmentCode("W201");
                    exitEnrollment.setEnrollmentDate(m_withdrawalDate);
                    exitEnrollment.setStatusCode("Inactive");
                    exitEnrollment.setYog(preYogOnly.getYog());
                    exitEnrollment.setFieldValueByBeanPath(m_fieldEntryExitInd, BooleanAsStringConverter.TRUE);

                    entryEnrollment.setStudentOid(student.getOid());
                    entryEnrollment.setSchoolOid(student.getSchoolOid());
                    entryEnrollment.setEnrollmentType(StudentEnrollment.ENTRY);
                    entryEnrollment.setEnrollmentCode("E119");
                    entryEnrollment.setEnrollmentDate(m_entryDate);
                    entryEnrollment.setStatusCode(activeStatus);
                    entryEnrollment.setYog(student.getYog());
                    entryEnrollment.setFieldValueByBeanPath(m_fieldEntryExitInd, BooleanAsStringConverter.TRUE);
                    if (presummer != null) {
                        entryEnrollment.setFieldValueByBeanPath(m_fieldDistrictHome,
                                presummer.getFieldValueByBeanPath(m_fieldDistrictHome));
                        entryEnrollment.setFieldValueByBeanPath(m_fieldSchoolHome,
                                presummer.getFieldValueByBeanPath(m_fieldSchoolHome));
                    }
                    getBroker().saveBeanForced(exitEnrollment);
                    getBroker().saveBeanForced(entryEnrollment);
                    addCount++;
                } else {
                    StudentEnrollment enrollment = findBOYEntry(student);
                    if (enrollment == null) {
                        logMessage("No entry record : " + student.getLocalId() + "  " + student.getNameView());
                        missingCount++;
                    }
                }
            }
        } finally {
            iterator.close();
        }

        logMessage("");
        logMessage("Students missing entry records : " + Integer.toString(missingCount));

        // Print totals
        if (REPORT_TYPE_VALUE_ADD.equals(m_reportType)) {
            logMessage("");
            logMessage("Students adding enrollment records : " + Integer.toString(addCount));
        }
    }

    /**
     * Find an entry record on the start of school date.
     *
     * @param student SisStudent
     * @return StudentEnrollment
     */
    private StudentEnrollment findBOYEntry(SisStudent student) {
        StudentEnrollment enrollment = null;
        List<StudentEnrollment> enrollments = m_enrollmentMap.get(student.getOid());
        if (enrollments != null) {
            for (StudentEnrollment enr : enrollments) {
                if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType()) &&
                        !m_entryDate.after(enr.getEnrollmentDate())) {
                    enrollment = enr;
                    break;
                }

            }
        }

        return enrollment;
    }

    /**
     * Determine if only YOG records exist in the summer.
     * If so, return the last regular record prior to the end of school year.
     * the parent can create withdrawal using the YOG from that record and the new Entry based on
     * SisStudent YOG.
     * 
     *
     * @param student SisStudent
     * @return StudentEnrollment
     */
    private StudentEnrollment isOnylYog(SisStudent student) {
        StudentEnrollment enrollment = null;
        boolean onlyYog = true;
        List<StudentEnrollment> enrollments = m_allEnrollmentMap.get(student.getOid());
        for (StudentEnrollment enr : enrollments) {
            // Check if a non-YOG record exists in the summer.
            if (!StudentEnrollment.YOG_CHANGE.equals(enr.getEnrollmentType()) &&
                    m_withdrawalDate.before(enr.getEnrollmentDate())) {
                onlyYog = false;
                break;
            }
            // Find the first non-summer enrollment record.
            if (!m_withdrawalDate.before(enr.getEnrollmentDate())) {
                enrollment = enr;
                break;
            }
        }
        if (!onlyYog) {
            enrollment = null;
        }
        return enrollment;
    }

    /**
     * Returns the last ENTRY record before the withdrawal date.
     * Used to get home district and school to put on the new Entry.
     *
     * @param student SisStudent
     * @return StudentEnrollment
     */
    private StudentEnrollment lastRecord(SisStudent student) {
        StudentEnrollment enrollment = null;
        List<StudentEnrollment> enrollments = m_allEnrollmentMap.get(student.getOid());
        for (StudentEnrollment enr : enrollments) {
            // Find the first non-summer enrollment record.
            if (StudentEnrollment.ENTRY.equals(enr.getEnrollmentType()) &&
                    !m_withdrawalDate.before(enr.getEnrollmentDate())) {
                enrollment = enr;
                break;
            }
        }
        return enrollment;
    }

    /**
     * Load a map of student enrollments by student Oid.
     *
     * @return Map<String, StudentEnrollment>
     */
    private void loadEnrollments() {
        // List<String> enrollmentTypes = Arrays.asList(StudentEnrollment.ENTRY,
        // StudentEnrollment.WITHDRAWAL,
        // StudentEnrollment.YOG_CHANGE);
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                        SisStudent.COL_ENROLLMENT_STATUS));
        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        m_allEnrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 2048);

        criteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_withdrawalDate);
        // criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, enrollmentTypes);
        query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_STUDENT_OID, true);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, false);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, false);
        m_enrollmentMap = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 2048);

    }

    /**
     * Initialize parameters and instance variables.
     */
    private void initializeFields() {
        // Calculate dates for entry and withdrawal.
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.YEAR, 2012);
        calendar.set(Calendar.MONTH, Calendar.JUNE);
        calendar.set(Calendar.DAY_OF_MONTH, 15);
        m_withdrawalDate = new PlainDate(calendar.getTimeInMillis());
        calendar.set(Calendar.MONTH, Calendar.SEPTEMBER);
        calendar.set(Calendar.DAY_OF_MONTH, 4);
        m_entryDate = new PlainDate(calendar.getTimeInMillis());

        // Report parameters
        m_reportType = (String) getParameter(PARAM_REPORT_TYPE);

        // Lookup aliases
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_BOY_ENTRY_EXIT);
        if (field != null) {
            m_fieldEntryExitInd = field.getJavaName();
        }

        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_DISTRICT_HOME);
        if (field != null) {
            m_fieldDistrictHome = field.getJavaName();
        }

        field = dictionary.findDataDictionaryFieldByAlias(ALIAS_SCHOOL_HOME);
        if (field != null) {
            m_fieldSchoolHome = field.getJavaName();
        }

    }
}

