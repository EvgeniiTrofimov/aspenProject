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
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;

/**
 * Data module for the Cleartrack Adults and Addresses export. This export includes contacts for
 * all active students as well as inactive students who withdrew from the district at some point
 * within
 * the past 3 years. Support for the Primary Guardian retriever is provided as well.
 *
 * @author mmastrangelo
 */
public class CleartrackAdultAddressExportDataModule extends StateReportData {
    private static final int ARCHIVE_YEARS = 3;


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
        orCriteria.addIn(StudentContact.COL_STUDENT_OID,
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, getWithdrawalCriteria()));

        X2Criteria criteria = new X2Criteria();
        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                StudentContact.REL_STUDENT + "." + Student.COL_ENROLLMENT_STATUS));
        criteria.addOrCriteria(orCriteria);

        BeanQuery studentContactQuery = new BeanQuery(StudentContact.class, criteria);
        studentContactQuery.addOrderByAscending(StudentContact.REL_STUDENT + "." + Student.COL_NAME_VIEW);
        studentContactQuery.addOrderByAscending(StudentContact.COL_STUDENT_OID);
        studentContactQuery.addOrderByAscending(StudentContact.COL_EMERGENCY_PRIORITY);

        setQuery(studentContactQuery);

        addCustomCalcs();
    }

    /**
     * Add custom field retrievers.
     */
    private void addCustomCalcs() {
        HashMap<String, FieldRetriever> calcRetrievers = new HashMap<String, FieldRetriever>();

        calcRetrievers.put("cleartrack-PrimGuard", new PrimaryGuardianRetriever());

        addCalcs(calcRetrievers);
    }

    /**
     * Returns true if the contact's emergency priority is 1.
     *
     * @author mmastrangelo
     */
    protected class PrimaryGuardianRetriever implements FieldRetriever {

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
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            StudentContact contact = (StudentContact) entity.getBean();
            return 1 == contact.getEmergencyPriority() ? "Y" : "N";
        }
    }
}
