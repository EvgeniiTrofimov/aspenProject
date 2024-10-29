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
package com.x2dev.procedures.statereporting.uk;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class VericoolCashlessStudentExport.
 */
public class VericoolCashlessStudentExport extends StateReportData {
    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    private static final String SORT_PARAM = "sort";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";

    /**
     * The Class VericoolCashlessStudentEntity.
     */
    public static class VericoolCashlessStudentEntity extends StateReportEntity {
        VericoolCashlessStudentExport m_vericoolCashlessStudentExport;
        SisStudent m_student;
        String m_forename;
        String m_surname;
        String m_dob;
        String m_uniqueID;
        String m_yearGroup;
        String m_regGroup;
        String m_admissionNumber;
        String m_fsmStatus;
        String m_address;
        String m_rollStatus;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_vericoolCashlessStudentExport = (VericoolCashlessStudentExport) data;
            m_student = (SisStudent) bean;

            Person person = m_student.getPerson();

            if (person != null) {
                m_forename = person.getFirstName();
                m_surname = person.getLastName();
                m_uniqueID = m_student.getStateId();
                m_yearGroup = m_student.getGradeLevel();
                m_regGroup = m_student.getHomeroom();
                m_admissionNumber = m_student.getLocalId(); //
                m_fsmStatus = m_student.getFieldA095();
                m_rollStatus = m_student.getEnrollmentStatus();
                if (person.getDob() != null) {
                    m_dob = person.getDob().toString();
                }
                Address address = person.getPhysicalAddress();
                if (address != null) {
                    m_address = "\"" + address.getAddressLine01() + " "
                    // + address.getAddressLine02() + " "
                    // + address.getAddressLine03() + ", "
                            + address.getCity() + " "
                            + address.getPostalCode() + "\"";
                }
            }

        }

        /**
         * Gets the forename.
         *
         * @return the m_forename
         */
        public String getForename() {
            return m_forename;
        }

        /**
         * Gets the surname.
         *
         * @return the m_surname
         */
        public String getSurname() {
            return m_surname;
        }

        /**
         * Gets the dob.
         *
         * @return the m_dob
         */
        public String getDob() {
            return m_dob;
        }

        /**
         * Gets the unique ID.
         *
         * @return the m_uniqueID
         */
        public String getUniqueID() {
            return m_uniqueID;
        }

        /**
         * Gets the year group.
         *
         * @return the m_yearGroup
         */
        public String getYearGroup() {
            return m_yearGroup;
        }

        /**
         * Gets the reg group.
         *
         * @return the m_regGroup
         */
        public String getRegGroup() {
            return m_regGroup;
        }

        /**
         * Gets the admission number.
         *
         * @return the m_admissionNumber
         */
        public String getAdmissionNumber() {
            return m_admissionNumber;
        }

        /**
         * Gets the fsm status.
         *
         * @return the m_fsmStatus
         */
        public String getFsmStatus() {
            return m_fsmStatus;
        }

        /**
         * Gets the address.
         *
         * @return the m_Address
         */
        public String getAddress() {
            return m_address;
        }

        /**
         * Gets the roll status.
         *
         * @return the m_rollStatus
         */
        public String getRollStatus() {
            return m_rollStatus;
        }
    }

    /**
     * The Class RetrieveStudentForename.
     */
    protected class RetrieveStudentForename implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getForename();
        }
    }

    /**
     * The Class RetrieveStudentSurname.
     */
    protected class RetrieveStudentSurname implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getSurname();
        }
    }

    /**
     * The Class RetrieveStudentDOB.
     */
    protected class RetrieveStudentDOB implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getDob();
        }
    }

    /**
     * The Class RetrieveStudentUniqueID.
     */
    protected class RetrieveStudentUniqueID implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getUniqueID();
        }
    }

    /**
     * The Class RetrieveStudentYearGroup.
     */
    protected class RetrieveStudentYearGroup implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getYearGroup();
        }
    }

    /**
     * The Class RetrieveStudentRegGroup.
     */
    protected class RetrieveStudentRegGroup implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getRegGroup();
        }
    }

    /**
     * The Class RetrieveStudentAdmissionNumber.
     */
    protected class RetrieveStudentAdmissionNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getAdmissionNumber();
        }
    }

    /**
     * The Class RetrieveStudentFsmStatus.
     */
    protected class RetrieveStudentFsmStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getFsmStatus();
        }
    }

    /**
     * The Class RetrieveStudentAddress.
     */
    protected class RetrieveStudentAddress implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getAddress();
        }
    }

    /**
     * The Class RetrieveStudentRollStatus.
     */
    protected class RetrieveStudentRollStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStudentEntity vericoolCashlessStudentEntity = ((VericoolCashlessStudentEntity) entity);
            return vericoolCashlessStudentEntity.getRollStatus();
        }
    }


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {

        Criteria studentCriteria = getStudentCriteria();
        QueryByCriteria studentQuery = new QueryByCriteria(SisStudent.class,
                studentCriteria);



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
                studentQuery.addOrderByAscending(SisStudent.REL_SCHOOL
                        + PATH_DELIMITER + SisSchool.COL_NAME);
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
        setEntityClass(VericoolCashlessStudentEntity.class);

        // set fieldRetrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();

        calcs.put("VCSE-Forename", new RetrieveStudentForename());
        calcs.put("VCSE-Surname", new RetrieveStudentSurname());
        calcs.put("VCSE-DOB", new RetrieveStudentDOB());
        calcs.put("VCSE-UniqueID", new RetrieveStudentUniqueID());
        calcs.put("VCSE-YearGroup", new RetrieveStudentYearGroup());
        calcs.put("VCSE-RegGroup", new RetrieveStudentRegGroup());
        calcs.put("VCSE-AdmissionNumber", new RetrieveStudentAdmissionNumber());
        calcs.put("VCSE-FSMStatus", new RetrieveStudentFsmStatus());
        calcs.put("VCSE-Address", new RetrieveStudentAddress());
        calcs.put("VCSE-RollStatus", new RetrieveStudentRollStatus());

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
     * Gets the student criteria.
     *
     * @return Criteria
     */
    private Criteria getStudentCriteria() {
        X2Criteria criteria = new X2Criteria();
        // criteria.addColumnEqualTo(Student.COL_ENROLLMENT_STATUS, "Active");
        // TODO get Active Parameter

        /*
         * Check student selection criteria user input.
         */
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                criteria.addEqualTo(Student.COL_YOG, queryString);
                break;

            case 2: // LASID
                criteria.addEqualTo(Student.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                criteria.addEqualTo(Student.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(criteria, queryString);
                break;

            default:
                // Take all students in the district
                break;
        }
        return criteria;
    }

}
