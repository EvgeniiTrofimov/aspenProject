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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.HashMap;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class NYStudentContactFact.
 */
public class NYStudentContactFact extends StateReportData {

    /**
     * Entity class for Student Contact Fact export.
     *
     * @author X2 Development Corporation
     */
    public static class NYStudentContactFactEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        protected StudentContact m_studentContact;
        protected NYStudentContactFact m_scfData;


        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NYStudentContactFactEntity() {
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
            m_studentContact = (StudentContact) bean;
            m_scfData = (NYStudentContactFact) data;
        }

        /**
         * Returns attendance code.
         *
         * @return Student contact
         */
        public StudentContact getStudentContact() {
            return m_studentContact;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            if (m_studentContact.getPerson() != null) {
                return m_studentContact.getPerson().getNameView();
            }
            return "No person for oid " + super.getEntityName() + "!";
        }


        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {

            StateReportValidationError error = null;

            String stateValue = m_scfData.lookupStateValue(StudentContact.class, StudentContact.COL_RELATIONSHIP_CODE,
                    m_studentContact.getRelationshipCode());

            // if (stateValue == null || stateValue.length() == 0)
            if (StringUtils.isEmpty(stateValue)) {
                error = new StateReportValidationError(this.toString(), StudentContact.COL_RELATIONSHIP_CODE,
                        "Relationship code not present", "Relationship Code should not be empty");
            }
            return error;
        }
    }

    /**
     * Params
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Local variables for reporting information.
     */
    protected PlainDate m_reportDate;
    protected StudentHistoryHelper m_helper;

    public static final String PARAM_REMOVE_HEADER = "removeHeader";

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        if (((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue()) {
            return null;
        }
        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        Criteria studentCriteria = m_helper.getStudentCriteria();
        SubQuery studentSubQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);

        Criteria studentContactCriteria = getStudentContactCriteria();
        studentContactCriteria.addIn(StudentContact.REL_STUDENT + "." + X2BaseBean.COL_OID, studentSubQuery);

        // Create StudentContact Query
        QueryByCriteria query = new QueryByCriteria(StudentContact.class, studentContactCriteria);
        applyInputSort(query, null);
        setQuery(query);

        setEntityClass(NYStudentContactFactEntity.class);

        HashMap calcs = new HashMap<String, FieldRetriever>();
        HashMap validators = new HashMap<String, FieldRetriever>();

        super.addCalcs(calcs);
        super.addValidators(validators);
    }

    /**
     * Gathers contact criteria for students at this school and applies input critera.
     *
     * @return Criteria
     */
    private Criteria getStudentContactCriteria() {
        X2Criteria studentContactCriteria = new X2Criteria();
        applyInputCriteria(studentContactCriteria, false, StudentContact.REL_STUDENT);
        // Look up school or organization level enrollment records.
        if (isSchoolContext()) {
            studentContactCriteria.addEqualTo(StudentContact.REL_STUDENT + "." + Student.COL_SCHOOL_OID,
                    getSchool().getOid());
        }


        return studentContactCriteria;
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }
    }
}
