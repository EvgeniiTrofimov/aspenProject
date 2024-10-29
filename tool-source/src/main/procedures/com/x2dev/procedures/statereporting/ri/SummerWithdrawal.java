/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * RI state report for Summer Withdrawals export. This class implements the data
 * export for the RI Summer Withdrawals export.
 *
 * @author X2 Development Corporation
 */
public class SummerWithdrawal extends RIStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the Summer Withdrawals export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SummerWithdrawalEntity extends StateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        SummerWithdrawal m_summerWithdrawal = null;
        SisStudent m_student = null;
        String m_stateExitTypeValue = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public SummerWithdrawalEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            return name;
        }

        /**
         * Gets the state exit type.
         *
         * @return String
         */
        public String getStateExitType() {
            return m_stateExitTypeValue;
        }

        /**
         * Sets the state exit type.
         *
         * @param exitType void
         */
        protected void setStateExitType(String exitType) {
            m_stateExitTypeValue = exitType;
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method finds the student schedule and student schedule change records for the
         * student
         * and generates a list of reportable schedule items.
         * The entity can produce multiple rows from these results.
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

            m_summerWithdrawal = (SummerWithdrawal) data;
            m_student = (SisStudent) bean;

            List<StudentEnrollment> enrollments = m_summerWithdrawal.m_enrollmentManager.getOrderedEnrollment(
                    m_student, m_summerWithdrawal.m_summerStartDate, m_summerWithdrawal.m_summerEndDate, null, false);

            String stateExitType = null;

            StudentEnrollment lastEnrollmentIsWithdrawal = null;
            // check the most recent enrollment is a withdrawal, then set it to the exit type for
            // this student
            for (StudentEnrollment enrollment : enrollments) {
                if (enrollment.getEnrollmentType().equals(StudentEnrollment.WITHDRAWAL)) {
                    stateExitType = (String) enrollment.getFieldValueByBeanPath(m_summerWithdrawal.m_stateExitType);
                    if (!"01".equals(stateExitType)) {
                        setStateExitType(stateExitType);
                        lastEnrollmentIsWithdrawal = enrollment;
                    }
                }
                break;
            }

            // If the most recent is W and is summer, keep it.
            if (lastEnrollmentIsWithdrawal != null) {
                setRowCount(1);

                // If a record set is being used to track the summer withdrawals found, add to it
                // here.
                RecordSet recordSet = ((SummerWithdrawal) data).m_recordSet;
                if (recordSet != null) {
                    RecordSetKey recordSetKey =
                            X2BaseBean.newInstance(RecordSetKey.class, data.getBroker().getPersistenceKey());
                    recordSetKey.setRecordSetOid(recordSet.getOid());
                    recordSetKey.setObjectOid(m_student.getOid());
                    data.getBroker().saveBeanForced(recordSetKey);

                }
            } else {
                setRowCount(0);

            }
        }
    }

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String QUERY_STRING_PARAM = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";

    /**
     * Input parameter: Select organization level.
     */
    public static final String ORGANIZATION_PARAM = "orgOid";

    /**
     * Summer start date parameter
     */
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";

    /**
     * Summer end date parameter
     */
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";

    /**
     * Optional input parameter containing the name of a snapshot to create of the students included
     * in the export
     */
    public static final String SNAPSHOT_NAME_PARAM = "snapshotName";

    /**
     * Name for the active students only parameter. The value is a boolean.
     */
    public static final String SASID_STUDENTS_ONLY_PARAM = "sasidStudentsOnly";

    public static final String STATE_EXIT_TYPE = "RI Exit Type";

    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";

    protected PlainDate m_summerStartDate;
    protected PlainDate m_summerEndDate;
    protected EnrollmentManager m_enrollmentManager;
    protected String m_stateExitType;
    protected String m_excludeStdField;
    private String m_orgFieldStr = null;
    private String m_orgOid = null;
    protected RecordSet m_recordSet;
    protected boolean m_sasidStudentsOnly;

    /**
     * Retrieve the exit type from the student withdrawal.
     * 
     * @author X2 Development Corporation
     */
    protected class RetrieveExitType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SummerWithdrawalEntity summerWithdrawal = (SummerWithdrawalEntity) entity;
            return summerWithdrawal.getStateExitType();
        }
    }

    /**
     * Initialize the export.
     *
     * @throws X2BaseException exception
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_summerStartDate = (PlainDate) getParameter(SUMMER_START_DATE_PARAM);
        m_summerEndDate = (PlainDate) getParameter(SUMMER_END_DATE_PARAM);

        m_stateExitType = translateAliasToJavaName(STATE_EXIT_TYPE, true);
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);

        m_sasidStudentsOnly = true;
        Boolean sasidStudentsOnly = (Boolean) getParameter(SASID_STUDENTS_ONLY_PARAM);
        if (sasidStudentsOnly != null) {
            m_sasidStudentsOnly = sasidStudentsOnly.booleanValue();
        }

        m_enrollmentManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());

        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        Organization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (Organization) getBroker().getBeanByOid(Organization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        String snapshotName = (String) getParameter(SNAPSHOT_NAME_PARAM);

        if (!StringUtils.isEmpty(snapshotName)) {
            createRecordSet(snapshotName);
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            Criteria studentCriteria = getStudentCriteria();
            QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class, studentCriteria);

            int sort = ((Integer) getParameter(SORT_PARAM)).intValue();
            switch (sort) {
                case 0: // Name
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 1: // YOG
                    studentQuery.addOrderByAscending(SisStudent.COL_YOG);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 2: // School
                    studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL + PATH_DELIMITER + School.COL_NAME);
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;

                case 3: // LASID
                    studentQuery.addOrderByAscending(SisStudent.COL_LOCAL_ID);
                    break;

                case 4: // SASID
                    studentQuery.addOrderByAscending(SisStudent.COL_STATE_ID);
                    break;

                default:
                    studentQuery.addOrderByAscending(SisStudent.COL_NAME_VIEW);
                    break;
            }

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(SummerWithdrawalEntity.class);

            // Load attendance maps.


            // Build maps of retriever functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("SUM-ExitType", new RetrieveExitType());

            super.addCalcs(calcs);
        }
    }

    /**
     * Creates a record set in m_recordSet with the passed name, owned by the current user. If a
     * user owned snapshot with
     * that name already exists, it is deleted first.
     *
     * @param snapshotName String
     */
    private void createRecordSet(String snapshotName) {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryTable table = dictionary.findDataDictionaryTableByPrefix(Student.OBJECT_PREFIX);

        X2Criteria snapshotCriteria = new X2Criteria();
        snapshotCriteria.addEqualTo(RecordSet.COL_NAME, snapshotName);
        snapshotCriteria.addEqualTo(RecordSet.COL_OWNER_OID, getUser().getOid());

        QueryByCriteria query = new QueryByCriteria(RecordSet.class, snapshotCriteria);
        m_recordSet = (RecordSet) getBroker().getBeanByQuery(query);

        if (m_recordSet != null) {
            getBroker().deleteBean(m_recordSet); // Assumes RSN-->RSK cascading delete!
        }

        m_recordSet = X2BaseBean.newInstance(RecordSet.class, getBroker().getPersistenceKey());
        m_recordSet.setDataTableOid(table.getDataTableConfig().getDataTableOid());
        m_recordSet.setName(snapshotName);
        m_recordSet.setOwnerOid(getUser().getOid());
        m_recordSet.setOwnerType(Ownable.OWNER_TYPE_USER);
        getBroker().saveBeanForced(m_recordSet);
    }

    /**
     * Returns the criteria that retrieves all students that should be included in the export.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        /*
         * First build the criteria based on the user's input
         */
        X2Criteria userCriteria = new X2Criteria();
        userCriteria.addAndCriteria(getReportingCriteria());

        String queryString = (String) getParameter(QUERY_STRING_PARAM);
        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 1: // YOG
                userCriteria.addEqualTo(SisStudent.COL_YOG, queryString);
                break;

            case 2: // LASID
                userCriteria.addEqualTo(SisStudent.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                userCriteria.addEqualTo(SisStudent.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(userCriteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }

        return userCriteria;
    }

    /**
     * Gets the reporting criteria.
     *
     * @return Criteria
     */
    private Criteria getReportingCriteria() {
        // Students with a EnrollmentType
        X2Criteria reportingCriteria = new X2Criteria();

        // this is the first session date, not the first date of the calendar year
        reportingCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);

        reportingCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_summerStartDate);
        reportingCriteria.addLessOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, m_summerEndDate);

        if (isSchoolContext()) {
            reportingCriteria.addEqualTo(StudentEnrollment.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                reportingCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + m_orgFieldStr,
                        m_orgOid);
            }
            reportingCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            reportingCriteria.addNotEqualTo(
                    StudentEnrollment.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        }
        // Check exclude student from state reporting flag if present.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            reportingCriteria.addNotEqualTo(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }

        // If "Student must have SASID" is checked, get only students with non-empty state ids
        if (m_sasidStudentsOnly) {
            reportingCriteria.addNotEmpty(StudentEnrollment.REL_STUDENT + PATH_DELIMITER +
                    SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
        }

        SubQuery enrQuery = new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, reportingCriteria);

        // Get the students current primary school, Or in students with enrollment activity.
        reportingCriteria = new X2Criteria();
        reportingCriteria.addIn(X2BaseBean.COL_OID, enrQuery);

        return reportingCriteria;
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
}
