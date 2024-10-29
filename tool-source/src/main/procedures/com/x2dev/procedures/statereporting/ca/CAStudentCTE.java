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

package com.x2dev.procedures.statereporting.ca;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for CA Student CTE.
 *
 * @author X2 Development Corporation
 */

public class CAStudentCTE extends StateReportData {

    /**
     * Entity class for CA Student CTE export.
     *
     */
    public static class CAStudentCTEEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        List<StudentEnrollment> m_enrollments;
        CAStudentCTE m_sdData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public CAStudentCTEEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return StudentEnrollmentSpan
         */
        public StudentEnrollment getEnrollment() {
            return m_enrollments.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    "] ";
            StudentEnrollment enrollment = getEnrollment();
            if (enrollment != null) {
                name += enrollment.getSchool().getName();
            }

            return name;
        }

        /**
         * Intitialize.
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

            m_sdData = (CAStudentCTE) data;
            m_enrollments = new ArrayList<StudentEnrollment>();
            List<StudentEnrollmentSpan> spans = m_sdData.m_helper.getStudentEnrollmentSpans((Student) bean, true);

            for (StudentEnrollmentSpan span : spans) {
                List<StudentEnrollment> enrollments = span.getEnrollments();

                for (StudentEnrollment enrollment : enrollments) {
                    if (!StringUtils
                            .isEmpty((String) enrollment.getFieldValueByBeanPath(m_sdData.m_fieldCTECompleteYear))) {
                        m_enrollments.add(enrollment);
                    }
                }
            }

            setRowCount(m_enrollments.size());
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
    }

    /**
     * Retrieve data from the student enrollment record by parameter:
     *
     * <LI>COMPLETION_YEAR - CTE Pathway Completion Academic Year ID
     * <LI>OID - Oid of the current student enrollment record
     * <LI>PATHWAY_CODE - CTE Pathway Code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private final String PARAM_COMPLETION_YEAR = "COMPLETION_YEAR";
        private final String PARAM_OID = "OID";
        private final String PARAM_PATHWAY_CODE = "PATHWAY_CODE";
        private final String PARAM_SCHOOL_ID = "SCHOOL_ID";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever
         *      #getFieldValue(StateReportData, StateReportEntity, FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            StudentEnrollment enrollment = ((CAStudentCTEEntity) entity).getEnrollment();
            Object value = null;

            if (PARAM_COMPLETION_YEAR.equals(parameter)) {
                value = enrollment.getFieldValueByBeanPath(m_fieldCTECompleteYear);
            } else if (PARAM_OID.equals(parameter)) {
                value = enrollment.getOid();
            } else if (PARAM_PATHWAY_CODE.equals(parameter)) {
                String code = (String) enrollment.getFieldValueByBeanPath(m_fieldCTEPathway);
                if (code != null) {
                    value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldCTEPathway, code,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            } else if (PARAM_SCHOOL_ID.equals(parameter)) {
                if (enrollment.getSchool() == null) {
                    value = null;
                } else {
                    value = enrollment.getSchool().getSchoolId();
                }
            }

            return value;
        }
    }

    /**
     * The Class ValidatePathwayCode.
     */
    protected class ValidatePathwayCode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator
         *      #getFieldValidation(StateReportData, StateReportEntity, FieldDefinition, String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            String code = (String) entity.getBean().getFieldValueByBeanPath(m_fieldCTEPathway);
            if (StringUtils.isEmpty(code)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required", "Value is required"));
                return errors;
            }

            String stateCode = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldCTEPathway, code,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (StringUtils.isEmpty(stateCode)) {
                errors.add(new StateReportValidationError(entity, field, "Value is required",
                        "State code is missing for " + STYLE_BOLD + stateCode +
                                STYLE_END));
            }

            return errors;
        }

    }

    /*
     * Aliases for fields to look up.
     */
    protected static final String ALIAS_CTE_COMPLETE_YEAR = "DOE CTE COMPLETE YEAR";
    protected static final String ALIAS_CTE_PATHWAY = "DOE CTE PATHWAY";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldCTECompleteYear;
    protected String m_fieldCTEPathway;
    protected StudentHistoryHelper m_helper;

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
        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        PlainDate currentDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        if (getCurrentContext().getEndDate().before(currentDate)) {
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE,
                    getCurrentContext().getEndDate());
        }

        // create enrollment criteria
        X2Criteria enrollmentCriteria = new X2Criteria();
        enrollmentCriteria.addNotEmpty(m_fieldCTECompleteYear, getBroker().getPersistenceKey());
        enrollmentCriteria.addEqualTo(m_fieldCTECompleteYear, getCurrentContext().getContextId());
        SubQuery enrollmentSubQuery =
                new SubQuery(StudentEnrollment.class, StudentEnrollment.COL_STUDENT_OID, enrollmentCriteria);

        // get student criteria
        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        studentCriteria.addIn(X2BaseBean.COL_OID, enrollmentSubQuery);

        // Set the query to be used for student selection.
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        setQuery(studentQuery);

        setEntityClass(CAStudentCTEEntity.class);

        // Build a map of calculations/retrievers
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put("CTE-ENROLLMENT", new RetrieveEnrollment());
        super.addCalcs(calcs);

        // Build a map of validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("CTE-PATHWAY-VAL", new ValidatePathwayCode());
        super.addValidators(validators);

    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldCTECompleteYear = translateAliasToJavaName(ALIAS_CTE_COMPLETE_YEAR, true);
        m_fieldCTEPathway = translateAliasToJavaName(ALIAS_CTE_PATHWAY, true);
    }
}
