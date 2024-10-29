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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.utils.X2BaseException;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New Jersey state procedure for SMID Submission export.
 *
 * @author X2 Development Corporation
 */
public class SMID_Submission extends StateReportData {

    /**
     * Entity class for Staff level export.
     *
     */
    public static class SMID_SubmissionEntity extends StateReportEntity {
        String m_Status;

        /**
         * Public no argument constructor for dynamic instantiation.
         */

        public SMID_SubmissionEntity() {
            // no argument constructor
        }


        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStaff staff = (SisStaff) getBean();
            String name = staff.getNameView() +
                    " [LOCAL ID: " + staff.getLocalId() +
                    ", STATE ID: " + staff.getStateId() +
                    "] ";

            return name;
        }

    }

    /**
     * Alias
     */
    protected static final String ALIAS_STAFF_EXIT_DATE = "DOE STAFF EXIT DATE";

    /**
     * Other Constants
     */
    protected static final String STAFF_STATUS_ACTIVE = "Active";

    /**
     * instance variables
     */
    protected String m_staffExitDate;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_staffExitDate = translateAliasToJavaName(ALIAS_STAFF_EXIT_DATE, false);
        // create staff Query
        Criteria criteria = getStaffCriteria();
        QueryByCriteria query = new QueryByCriteria(Staff.class, criteria);
        applyInputSort(query, null);
        // applyInputCriteria
        setQuery(query);
        setEntityClass(SMID_SubmissionEntity.class);
        HashMap calcs = new HashMap<String, FieldRetriever>();
        super.addCalcs(calcs);

        HashMap validators = new HashMap<String, FieldRetriever>();
        super.addValidators(validators);
    }

    /**
     * Returns the criteria that retrieves all staff that should be included in the export.
     *
     * @return Criteria
     */

    private Criteria getStaffCriteria() {
        Criteria reportingCriteria = new Criteria();

        Criteria reportingCriteria1 = new Criteria();
        Criteria reportingCriteria2 = new Criteria();

        applyInputCriteria(reportingCriteria1, true, null);
        applyInputCriteria(reportingCriteria2, true, null);

        // Include staff who ended their employment in the current school year.
        reportingCriteria1.addGreaterOrEqualThan(m_staffExitDate, getCurrentContext().getStartDate());

        // Or include staff who are Active.
        reportingCriteria2.addEqualTo(Staff.COL_STATUS, STAFF_STATUS_ACTIVE);

        reportingCriteria.addOrCriteria(reportingCriteria1);
        reportingCriteria.addOrCriteria(reportingCriteria2);

        if (!isSchoolContext()) {
            reportingCriteria.addNotEqualTo(
                    Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            reportingCriteria.addNotEqualTo(
                    Staff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        }
        return reportingCriteria;
    }

}
