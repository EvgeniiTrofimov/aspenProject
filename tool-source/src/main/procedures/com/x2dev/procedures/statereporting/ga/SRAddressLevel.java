/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ga;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * This class implements the data export for Student Record Student Address Level export.
 *
 * @author X2 Development Corporation
 */
public class SRAddressLevel extends StateReportData {
    /**
     * Entity class for Student Record Student Address Level export.
     *
     * @author X2 Development Corporation
     */
    public static class SRAddressLevelEntity extends StateReportEntity {
        /**
         * private entity variables.
         */
        private SRAddressLevel m_addressLevelData = null;
        private List<SisSchool> m_schools = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SRAddressLevelEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();

            String name = student.getNameView() +
                    " [Local ID: " + student.getLocalId() +
                    ", State ID: " + student.getStateId() +
                    "]";
            return name;
        }

        /**
         * Returns the current reporting school for this student.
         *
         * @return School
         */
        public SisSchool getSchool() {
            SisSchool school = null;
            if (getCurrentRow() >= 0 && getCurrentRow() < m_schools.size()) {
                school = m_schools.get(getCurrentRow());
            }
            return school;
        }

        /**
         * Initialize.
         * Get a list of all schools the student was active in during the year.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_addressLevelData = (SRAddressLevel) data;
            Student student = (Student) bean;

            Set<SisSchool> schoolSet = new HashSet<SisSchool>();

            Collection<StudentEnrollment> enrollments = m_addressLevelData.m_studentEnrollements.get(student.getOid());

            // If the student is active, add current school.
            if (m_addressLevelData.m_activeCode.equals(student.getEnrollmentStatus())) {
                schoolSet.add((SisSchool) student.getSchool());
            }

            // Any enrollment activity, add the school.
            if (enrollments != null) {
                for (StudentEnrollment enrollment : enrollments) {
                    schoolSet.add(enrollment.getSchool());
                }
            }

            m_schools = new ArrayList<SisSchool>(schoolSet);
            setRowCount(m_schools.size());
        }
    }

    /**
     * Constants for reporting information.
     */
    private static final String DOE_EXCLUDE_STD_ALIAS = "DOE EXCLUDE STD";
    private static final String DOE_OVERRIDE_SCHOOL = "DOE Override School Code";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";
    private static final String PARAM_SORT = "sort";
    private static final String SCHOOL_CODE_ALIAS = "DOE School";

    /**
     * Local variables for reporting information.
     */
    protected String m_activeCode;
    protected Map<String, Collection<StudentEnrollment>> m_studentEnrollements;
    protected PlainDate m_reportDate;
    protected Collection<String> m_retained;
    protected String m_schoolCodeField;
    protected String m_overrideSchoolCodeField;

    /**
     * Retrieve the school code from the school on the row. A student
     * can generate multiple schools, so retrieve the code from the current row school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveSchoolCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String code = null;
            School school = ((SRAddressLevelEntity) entity).getSchool();
            SisStudent student = (SisStudent) entity.getBean();
            if (null != student) {
                code = (String) student.getFieldValueByBeanPath(m_overrideSchoolCodeField);
            }
            if (StringUtils.isEmpty(code) && school != null) {
                code = (String) school.getFieldValueByBeanPath(m_schoolCodeField);
            }

            return code;
        }
    }

    /**
     * Initialize the data module.
     */
    @Override
    protected void initialize() {
        // Set the report date.
        m_reportDate = new PlainDate(); // (PlainDate) getParameter(REPORT_DATE_PARAM);

        // Lookup aliases
        m_schoolCodeField = translateAliasToJavaName(SCHOOL_CODE_ALIAS, true);
        m_overrideSchoolCodeField = translateAliasToJavaName(DOE_OVERRIDE_SCHOOL, true);
        m_activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        Criteria studentCriteria = getStudentCriteria();
        QueryByCriteria studentQuery = new QueryByCriteria(Student.class, studentCriteria);
        int sort = ((Integer) getParameter(PARAM_SORT)).intValue();
        switch (sort) {
            case 0: // Name
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 1: // YOG
                studentQuery.addOrderByAscending(Student.COL_YOG);
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 2: // School
                studentQuery.addOrderByAscending(Student.REL_SCHOOL + ModelProperty.PATH_DELIMITER + School.COL_NAME);
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;

            case 3: // LASID
                studentQuery.addOrderByAscending(Student.COL_LOCAL_ID);
                break;

            case 4: // SASID
                studentQuery.addOrderByAscending(Student.COL_STATE_ID);
                break;

            default:
                studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);
                break;
        }

        // Set the query to be used for student selection.
        setQuery(studentQuery);
        setEntityClass(SRAddressLevelEntity.class);

        // Load associated maps for student data.
        loadEnrollmentData(studentCriteria);

        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("GA-SR-SCHOOL-CODE", new RetrieveSchoolCode());
        super.addCalcs(calcs);

    }

    /**
     * Adds criteria to filter the export selection by record set.
     *
     * @param criteria Criteria
     * @param recordSetName String
     */
    private void addRecordSetCriteria(Criteria criteria, String recordSetName) {
        Criteria recordSetCriteria = new Criteria();
        recordSetCriteria.addEqualTo(RecordSetKey.REL_RECORD_SET + "." + RecordSet.COL_NAME,
                recordSetName);

        criteria.addIn(X2BaseBean.COL_OID, new SubQuery(RecordSetKey.class,
                RecordSetKey.COL_OBJECT_OID, recordSetCriteria));
    }

    /**
     * Builds a criteria for the students that should be reported (not considering user input
     * parameters).
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        /*
         * Students to include.
         *
         * 1. The student is active and in an active school.
         * or
         * 2. The student has (E,W) enrollment records within the school year.
         *
         */

        // Select students with enrollment activity (E,W) in the school this year.
        X2Criteria enrollCriteria = new X2Criteria();
        enrollCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        X2Criteria enrollCriteria2 = new X2Criteria();
        enrollCriteria2.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.ENTRY);
        enrollCriteria2.addOrCriteria(enrollCriteria);

        X2Criteria activityCriteria = new X2Criteria();
        PlainDate startDate = getOrganization().getCurrentContext().getStartDate();
        activityCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startDate);
        activityCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_reportDate);
        activityCriteria.addAndCriteria(enrollCriteria2);
        activityCriteria.addEqualTo(StudentEnrollment.COL_STATUS_CODE, m_activeCode);

        if (isSchoolContext()) {
            activityCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.FALSE);
            activityCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.FALSE);
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, activityCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        enrollCriteria = new X2Criteria();
        enrollCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        X2Criteria primaryCriteria = new X2Criteria();
        primaryCriteria.addEqualTo(Student.COL_ENROLLMENT_STATUS, m_activeCode);

        primaryCriteria.addOrCriteria(enrollCriteria);

        return primaryCriteria;
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria userCriteria = new X2Criteria();

        String fieldExcludeStd = translateAliasToJavaName(DOE_EXCLUDE_STD_ALIAS, false);
        if (!StringUtils.isEmpty(fieldExcludeStd)) {
            userCriteria.addNotEqualTo(fieldExcludeStd, BooleanAsStringConverter.TRUE);
        }

        /*
         * Check school selection user input parameter.
         */
        if (isSchoolContext()) {
            userCriteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            userCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR, Boolean.FALSE);
            userCriteria.addEqualTo(Student.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR, Boolean.FALSE);
        }

        /*
         * Check student selection criteria user input.
         */
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        /*
         * Then combine the user criteria with the general reporting criteria
         */
        Criteria studentCriteria = new Criteria();
        studentCriteria.addAndCriteria(userCriteria);
        studentCriteria.addAndCriteria(getReportingCriteria());

        return studentCriteria;
    }

    /**
     * Loads the enrollment data required by this export.
     *
     * @param studentCriteria Criteria
     */
    private void loadEnrollmentData(Criteria studentCriteria) {
        ArrayList typeCodes = new ArrayList(2);
        typeCodes.add(StudentEnrollment.ENTRY);
        typeCodes.add(StudentEnrollment.WITHDRAWAL);
        typeCodes.add(StudentEnrollment.YOG_CHANGE);

        // subQuery - Students.
        SubQuery studentsSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        // Main report query - students and Absence.
        Criteria criteria = new Criteria();
        criteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentsSubQuery);
        if (isSchoolContext()) {
            criteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        }
        criteria.addIn(StudentEnrollment.COL_ENROLLMENT_TYPE, typeCodes);
        criteria.addGreaterOrEqualThanField(StudentEnrollment.COL_ENROLLMENT_DATE,
                StudentEnrollment.REL_SCHOOL + PATH_DELIMITER +
                        SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                        SchoolScheduleContext.REL_ACTIVE_SCHEDULE + PATH_DELIMITER +
                        Schedule.COL_START_DATE);

        QueryByCriteria query = new QueryByCriteria(StudentEnrollment.class, criteria);
        query.addOrderBy(StudentEnrollment.COL_ENROLLMENT_DATE, true);
        query.addOrderBy(StudentEnrollment.COL_TIMESTAMP, true);

        m_studentEnrollements = getBroker().getGroupedCollectionByQuery(query, StudentEnrollment.COL_STUDENT_OID, 1000);
    }
}
