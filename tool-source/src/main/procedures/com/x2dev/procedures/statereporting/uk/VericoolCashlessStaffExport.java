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
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.RecordSet;
import com.follett.fsc.core.k12.beans.RecordSetKey;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class VericoolCashlessStaffExport.
 */
public class VericoolCashlessStaffExport extends StateReportData {
    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String SORT_PARAM = "sort";
    private static final String PARAM_QUERY_BY = "queryBy";
    private static final String PARAM_QUERY_STRING = "queryString";

    /**
     * The Class VericoolCashlessStaffEntity.
     */
    public static class VericoolCashlessStaffEntity extends StateReportEntity {
        VericoolCashlessStaffExport m_vericoolCashlessStaffExport;
        SisStaff m_staff;
        String m_forename;
        String m_surname;
        PlainDate m_dob;
        String m_contractStatus;
        String m_uniqueID;

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);

            m_vericoolCashlessStaffExport = (VericoolCashlessStaffExport) data;
            m_staff = (SisStaff) bean;

            Person person = m_staff.getPerson();

            if (person != null) {
                m_forename = person.getFirstName();
                m_surname = person.getLastName();
                m_dob = person.getDob();
                m_contractStatus = m_staff.getStatus();
                m_uniqueID = m_staff.getStateId();
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
        public PlainDate getDob() {
            return m_dob;
        }

        /**
         * Gets the contract status.
         *
         * @return the m_contractStatus
         */
        public String getContractStatus() {
            return m_contractStatus;
        }

        /**
         * Gets the unique ID.
         *
         * @return the m_uniqueID
         */
        public String getUniqueID() {
            return m_uniqueID;
        }
    }

    /**
     * The Class RetrieveStaffForename.
     */
    protected class RetrieveStaffForename implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStaffEntity vericoolCashlessStaffEntity = ((VericoolCashlessStaffEntity) entity);
            return vericoolCashlessStaffEntity.getForename();
        }
    }

    /**
     * The Class RetrieveStaffSurname.
     */
    protected class RetrieveStaffSurname implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStaffEntity vericoolCashlessStaffEntity = ((VericoolCashlessStaffEntity) entity);
            return vericoolCashlessStaffEntity.getSurname();
        }
    }

    /**
     * The Class RetrieveStaffDOB.
     */
    protected class RetrieveStaffDOB implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStaffEntity vericoolCashlessStaffEntity = ((VericoolCashlessStaffEntity) entity);
            return vericoolCashlessStaffEntity.getDob();
        }
    }

    /**
     * The Class RetrieveStaffContractStatus.
     */
    protected class RetrieveStaffContractStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStaffEntity vericoolCashlessStaffEntity = ((VericoolCashlessStaffEntity) entity);
            return vericoolCashlessStaffEntity.getContractStatus();
        }
    }

    /**
     * The Class RetrieveStaffUniqueID.
     */
    protected class RetrieveStaffUniqueID implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            VericoolCashlessStaffEntity vericoolCashlessStaffEntity = ((VericoolCashlessStaffEntity) entity);
            return vericoolCashlessStaffEntity.getUniqueID();
        }
    }


    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {

        Criteria staffCriteria = getStaffCriteria();
        QueryByCriteria studentQuery = new QueryByCriteria(SisStaff.class,
                staffCriteria);



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
        setEntityClass(VericoolCashlessStaffEntity.class);

        // set fieldRetrievers
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();

        calcs.put("VCSfE-Forename", new RetrieveStaffForename());
        calcs.put("VCSfE-Surname", new RetrieveStaffSurname());
        calcs.put("VCSfE-DOB", new RetrieveStaffDOB());
        calcs.put("VCSfE-YearGroup", new RetrieveStaffContractStatus());
        calcs.put("VCSfE-UniqueID", new RetrieveStaffUniqueID());

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
     * Gets the staff criteria.
     *
     * @return Criteria
     */
    private Criteria getStaffCriteria() {
        X2Criteria criteria = new X2Criteria();
        // criteria.addColumnEqualTo(SisStaff.COL_STATUS, "Active");
        // TODO get Active Parameter

        /*
         * Check staff selection criteria user input.
         */
        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // YOG
                // criteria.addEqualTo(Staff.COL_YOG, queryString);
                break;

            case 2: // LASID
                criteria.addEqualTo(Staff.COL_LOCAL_ID, queryString);
                break;

            case 3: // SASID
                criteria.addEqualTo(Staff.COL_STATE_ID, queryString);
                break;

            case 4: // Snapshot
                addRecordSetCriteria(criteria, queryString);
                break;

            default:
                // Take all staff in the district
                break;
        }
        return criteria;
    }

}
