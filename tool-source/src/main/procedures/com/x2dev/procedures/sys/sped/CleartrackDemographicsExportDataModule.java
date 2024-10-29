/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;

/**
 * Data module for the Cleartrack Demographics export. This export includes all active students as
 * well as
 * inactive students who withdrew from the district at some point within the past 3 years. Support
 * for
 * Race, Exit Date (for inactive students), and Address retrievers is provided as well.
 *
 * @author mmastrangelo
 */
public class CleartrackDemographicsExportDataModule extends StateReportData {
    private static final int ARCHIVE_YEARS = 3;

    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected HashMap<String, StudentEnrollment> m_withdrawalRecords;

    /**
     * Builds a criteria that finds all withdrawal records with a date falling within the last 3
     * school years (excluding
     * the current year).
     * 
     * @return X2Criteria
     */
    protected X2Criteria getWithdrawalCriteria() {
        X2Criteria startYearCriteria = new X2Criteria();
        startYearCriteria.addEqualTo(DistrictSchoolYearContext.COL_SCHOOL_YEAR,
                Integer.valueOf(getCurrentContext().getSchoolYear() - ARCHIVE_YEARS));

        BeanQuery startYearQuery = new BeanQuery(DistrictSchoolYearContext.class, startYearCriteria);

        DistrictSchoolYearContext startYear = (DistrictSchoolYearContext) getBroker().getBeanByQuery(startYearQuery);

        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addGreaterOrEqualThan(StudentEnrollment.COL_ENROLLMENT_DATE, startYear.getStartDate());
        enrollmentCriteria.addEqualTo(StudentEnrollment.COL_ENROLLMENT_TYPE, StudentEnrollment.WITHDRAWAL);

        return enrollmentCriteria;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addIn(X2BaseBean.COL_OID,
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, getWithdrawalCriteria()));

        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), Student.COL_ENROLLMENT_STATUS));
        criteria.addOrCriteria(orCriteria);

        BeanQuery studentQuery = new BeanQuery(Student.class, criteria);
        studentQuery.addOrderByAscending(Student.COL_NAME_VIEW);

        setQuery(studentQuery);

        loadWithdrawalRecords();
        loadRaceInfo(criteria);

        addCustomCalcs();
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        calcRetrievers.put("cleartrack-Race", new RaceRetriever());
        calcRetrievers.put("cleartrack-Withdraw", new WithdrawalRetriever());
        calcRetrievers.put("cleartrack-Address", new AddressRetriever());

        addCalcs(calcRetrievers);
    }

    /**
     * Loads the race code map and race reference table OID.
     *
     * @param reportCriteria Criteria
     */
    private void loadRaceInfo(Criteria reportCriteria) {
        Criteria raceCriteria = new Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID,
                new SubQuery(Student.class, Student.COL_PERSON_OID, reportCriteria));

        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);
        m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 100);
    }

    /**
     * Populates m_withdrawalRecords with the most recent withdrawal record for each student.
     */
    private void loadWithdrawalRecords() {
        m_withdrawalRecords = new HashMap<String, StudentEnrollment>(1024);

        BeanQuery withdrawalQuery = new BeanQuery(StudentEnrollment.class, getWithdrawalCriteria());
        withdrawalQuery.addOrderByAscending(StudentEnrollment.COL_STUDENT_OID);
        withdrawalQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);

        QueryIterator withdrawals = getBroker().getIteratorByQuery(withdrawalQuery);
        try {
            String lastStudentOid = null;
            while (withdrawals.hasNext()) {
                StudentEnrollment withdrawal = (StudentEnrollment) withdrawals.next();
                if (lastStudentOid == null || !withdrawal.getStudentOid().equals(lastStudentOid)) {
                    m_withdrawalRecords.put(withdrawal.getStudentOid(), withdrawal);
                }

                lastStudentOid = withdrawal.getStudentOid();
            }
        } finally {
            withdrawals.close();
        }
    }

    /**
     * Returns the address field specified by the calculation parameter. The mailing address is used
     * if the
     * student has a mailing address; otherwise the physical address is used.
     *
     * @author mmastrangelo
     */
    protected class AddressRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object fieldValue = null;

            Address address = null;
            Student student = (Student) entity.getBean();
            Person person = student.getPerson();
            if (person != null) {
                address = person.getMailingAddress();
                if (address == null) {
                    address = person.getPhysicalAddress();
                }
            }

            if (address != null) {
                fieldValue = address.getFieldValueByBeanPath(field.getParameter().toString());
            }

            return fieldValue;
        }
    }

    /**
     * Returns the enrollment field specified by the calculation parameter from the student's most
     * recent
     * withdrawal record.
     *
     * @author mmastrangelo
     */
    protected class WithdrawalRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object fieldValue = null;

            Student student = (Student) entity.getBean();

            if (!StudentManager.isActiveStudent(getOrganization(), student.getEnrollmentStatus())) {
                StudentEnrollment withdrawal = m_withdrawalRecords.get(student.getOid());

                if (withdrawal != null) {
                    fieldValue = withdrawal.getFieldValueByBeanPath(field.getParameter().toString());
                }
            }

            return fieldValue;
        }
    }

    /**
     * Returns the nth race code from the student's collection of races. The number to retrieve is
     * read from the calculation parameter.
     *
     * @author mmastrangelo
     */
    protected class RaceRetriever implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String raceCode = null;

            Student student = (Student) entity.getBean();
            Collection<Race> races = m_raceCodeMap.get(student.getPersonOid());

            if (races != null) {
                int count = 0;
                for (Race race : races) {
                    if (++count == Integer.parseInt(field.getParameter().toString())) {
                        raceCode = race.getRaceCode();
                        break;
                    }
                }
            }

            return raceCode;
        }
    }
}
