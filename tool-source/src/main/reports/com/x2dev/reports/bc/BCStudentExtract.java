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
package com.x2dev.reports.bc;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class to produce a list of all students who are enrolled in both Aspen production schools and
 * BCeSIS in the current school year.
 */
public class BCStudentExtract extends StateReportData {

    int m_count = 0;
    private StudentLocatorDAO m_dao;
    private Logger m_logger;

    /**
     * Default constructor.
     */
    public BCStudentExtract() {
        super();
        m_logger = AppGlobals.getLog();
    }

    /**
     * Returns the logger for this extract.
     *
     * @return Logger
     */
    public Logger getLogger() {
        return m_logger;
    }

    /**
     * Returns the StudentLocatorDAO.
     *
     * @return StudentLocatorDAO
     */
    public StudentLocatorDAO getStudentLocatorDAO() {
        return m_dao;
    }

    /**
     * Initialize the data module and necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        String dbServerName = (String) getParameter(StudentLocatorDAO.DB_SERVER_KEY);
        if (dbServerName == null) {
            addSetupError("BC eSIS linked Oracle Database",
                    "There is no host name provided in the input definition for the Common Student Extract");
            throw new X2BaseException((String) null,
                    "No host name found for linked Oracle Database. Contact your system administrator");
        }

        // Create a StudentLocator DAO for use by the StudentExtractEntity to find eSIS course
        // records
        m_dao = new StudentLocatorDAO(getBroker(), dbServerName);
        initializeFields();
        setEntityClass(StudentExtractEntity.class);

        // If no errors so far, continue with query.
        if (getSetupErrors().size() == 0) {
            // Build query object that will be used to retrieve export students.
            QueryByCriteria query = createActiveStudentQuery(m_dao);
            setQuery(query);
        }
    }

    /**
     * creates a query to find students with an enrollment status of either'Active' or Active no
     * primary'
     * and whose state id is also found in the eSIS database as a Student PEN, but only those
     * students
     * whose school is a "PROD" (production) school and whose privacy indicator is not true.
     *
     * @param dao StudentLocatorDAO
     * @return QueryByCriteria
     */
    private QueryByCriteria createActiveStudentQuery(StudentLocatorDAO dao) {
        PlainDate currentYearStartDate = getCurrentContext().getStartDate();

        Criteria criteria = new Criteria();
        criteria.addSql("STD_ID_STATE IN (" + dao.getQueryForPEN() + ") ");
        criteria.addNotEqualTo(Student.REL_PERSON + "." + Person.COL_PRIVATE_INDICATOR, Boolean.TRUE);

        Criteria primarySchoolCriteria = new Criteria();
        primarySchoolCriteria.addEqualTo(Student.REL_SCHOOL + "." + School.COL_FIELD_A003, "PROD");
        primarySchoolCriteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        addOrgCriteria(primarySchoolCriteria, Student.REL_SCHOOL);

        Criteria withdrawnStudentCriteria = createWithdrawnStudentCriteria(currentYearStartDate);

        Criteria secondarySchoolCriteria = createSecondarySchoolCriteria(currentYearStartDate);

        Criteria studentCriteria = new Criteria();
        studentCriteria.addOrCriteria(primarySchoolCriteria);
        studentCriteria.addOrCriteria(secondarySchoolCriteria);
        studentCriteria.addOrCriteria(withdrawnStudentCriteria);

        criteria.addAndCriteria(studentCriteria);
        applyInputCriteria(criteria, false, null); // Apply school scoping from user input.

        QueryByCriteria sortedCriteria = new QueryByCriteria(Student.class, criteria);
        applyInputSort(sortedCriteria, null);

        return sortedCriteria;
    }

    /**
     * adds the organization constraint to a school or enrollment criteria. This signature
     * permits the caller to pass in an explicit prefix appropriate to the target type of\
     * the caller.
     *
     * @param criteria Criteria
     * @param prefix String
     */
    private void addOrgCriteria(Criteria criteria, String prefix) {
        if (isSchoolContext()) {
            criteria.addEqualTo(Student.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            int level = OrganizationManager.getOrganizationLevel(getOrganization(), getBroker().getPersistenceKey());
            StringBuffer relPath = new StringBuffer(prefix).append(ModelProperty.PATH_DELIMITER);
            criteria.addEqualTo(relPath.append(OrganizationManager.getOrganizationColumnForLevel(level)).toString(),
                    getOrganization().getOid());
        }
    }

    /**
     * Build a criteria to find secondary schools that a student may be cross-enrolled in while
     * enrolled in a BC eSis primary school.
     *
     * @param currentYearStartDate PlainDate
     * @return Criteria
     */
    private Criteria createSecondarySchoolCriteria(PlainDate currentYearStartDate) {
        X2Criteria secondaryCriteria = new X2Criteria();
        StudentManager.buildSecondaryStudentDateCriteria(Student.REL_STUDENT_SCHOOLS + ".",
                secondaryCriteria,
                currentYearStartDate,
                new PlainDate(),
                getBroker().getPersistenceKey());

        addOrgCriteria(secondaryCriteria, Student.REL_STUDENT_SCHOOLS + "." + StudentSchool.REL_SCHOOL);
        Criteria secondaryCriteria2 = new Criteria();
        secondaryCriteria2.addEqualTo(
                Student.REL_STUDENT_SCHOOLS + "." + StudentSchool.REL_SCHOOL + "." + School.COL_FIELD_A003, "PROD");
        secondaryCriteria2.addAndCriteria(secondaryCriteria);

        return secondaryCriteria2;
    }

    /**
     * Builds a criteria to find withdrawn students.
     *
     * @param currentYearStartDate PlainDate
     * @return Criteria
     */
    private Criteria createWithdrawnStudentCriteria(PlainDate currentYearStartDate) {
        Criteria withdrawalCriteria = new Criteria();
        withdrawalCriteria.addEqualTo(StudentEnrollment.REL_SCHOOL + "." + School.COL_FIELD_A003, "PROD");
        withdrawalCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, currentYearStartDate);
        withdrawalCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);
        addOrgCriteria(withdrawalCriteria, StudentEnrollment.REL_SCHOOL);

        SubQuery withdrawalSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, withdrawalCriteria);

        Criteria withdrawnStudentCriteria = new Criteria();
        withdrawnStudentCriteria.addIn(X2BaseBean.COL_OID, withdrawalSubQuery);

        return withdrawnStudentCriteria;
    }

    /**
     * Add Field Retrievers that will be used to obtain or derive values for this export.
     */
    private void initializeFields() {
        // Add any retrievers or validators.
        HashMap calcs = new HashMap<String, FieldRetriever>();

        calcs.put("BC-ESIS-DATA", new RetrieveStudentData());
        super.addCalcs(calcs);
    }

    /**
     * Implementation of StateReportEntity to be used by the BC Common Student Extract export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     */
    public static class StudentExtractEntity extends StateReportEntity {
        private BCStudentExtract m_bcStudentExtract;
        private List<Map> m_esisStudentRecs;
        private String m_schoolName;
        private String m_schoolMinistryId;
        private SisStudent m_student;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Student student = (Student) getBean();
            String name = "Student: " + student.getPerson().getNameView() +
                    " [Primary School: " + student.getSchool().getName() + "]";

            return name;
        }

        /**
         * Returns the list of Esis student extract.
         * 
         * @return List<Map>
         */
        public List<Map> getEsisStudentExtract() {
            return m_esisStudentRecs;
        }

        /**
         * Get students and staff for the course. Gather statistics on student gender, race and
         * programs.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_bcStudentExtract = (BCStudentExtract) data;
            m_student = (SisStudent) bean;
            m_bcStudentExtract.m_count++;

            School aspenSchool = (m_student.getSchool() != null && "PROD".equals(m_student.getSchool().getFieldA003()))
                    ? m_student.getSchool()
                    : getProductionSecondarySchool(m_student.getStudentSchools(m_bcStudentExtract.getBroker()));

            if (aspenSchool != null) {
                m_schoolName = aspenSchool.getName();
                m_schoolMinistryId = aspenSchool.getSchoolId();
            }

            String studentId = m_student.getStateId();
            m_esisStudentRecs = m_bcStudentExtract.getStudentLocatorDAO().getStudentCourseData(studentId);
            for (Map record : m_esisStudentRecs) {
                record.put("ASPEN_SCHOOL_NAME", m_schoolName);
                record.put("ASPEN_SCHOOL_ID", m_schoolMinistryId);
                try {
                    String esisPrimarySchool = record.get("IS_PRIMARY").toString();
                    record.put("HOME_SCHOOL", ("Y".equals(esisPrimarySchool)) ? "Y" : "N");
                } catch (Exception ex) {
                    record.put("HOME_SCHOOL", "N");
                }
            }
            setRowCount(m_esisStudentRecs == null ? 0 : m_esisStudentRecs.size());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }

        /**
         * Find the first secondary school with a date range that includes today and that is marked
         * "PROD".
         *
         * @param studentSchools Collection<StudentSchool>
         * @return School
         */
        private School getProductionSecondarySchool(Collection<StudentSchool> studentSchools) {
            School school = null;
            PlainDate today = new PlainDate();

            if (studentSchools != null) {
                for (StudentSchool stdSchool : studentSchools) {
                    if (stdSchool.getSchool() != null && "PROD".equals(stdSchool.getSchool().getFieldA003())
                            && !today.before(stdSchool.getStartDate())
                            && (stdSchool.getEndDate() == null || !today.after(stdSchool.getEndDate()))
                            && stdSchool.getType() == StudentSchool.SECONDARY) {
                        school = stdSchool.getSchool();
                        break;
                    }
                }
            }

            return school;
        }
    }

    /**
     * Returns a column from a student record from the BC eSIS database.
     * The current strategy is to cache the complete student table from the database
     * in a Map with the key for each student record being the StudentID (PEN).
     */
    protected class RetrieveStudentData implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData
         *      data, StateReportEntity entity, FieldDefinition field)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;

            if (entity instanceof StudentExtractEntity) {
                StudentExtractEntity bcStudentEntity = (StudentExtractEntity) entity;
                int rowNum = bcStudentEntity.getCurrentRow();
                String esisFieldKey = (String) field.getParameter();
                Map studentRecord = bcStudentEntity.getEsisStudentExtract().get(rowNum);

                value = studentRecord.get(esisFieldKey);
            } else {
                getLogger().log(Level.WARNING, "Looking for a StudentExtractEntity but found a "
                        + entity.getClass().getCanonicalName() + " instead.");
            }

            return value != null ? value.toString() : "";
        }
    }
}
