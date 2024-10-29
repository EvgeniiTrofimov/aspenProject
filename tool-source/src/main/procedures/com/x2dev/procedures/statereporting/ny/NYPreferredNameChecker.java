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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

/**
 * New York state procedure for StudentLite export.
 *
 * @author X2 Development Corporation
 */

public class NYPreferredNameChecker extends StateReportData {
    /**
     * Entity class for Student Lite export.
     *
     * @author X2 Development Corporation
     */

    public static class StudentNameEntity extends StateReportEntity {


        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentNameEntity() {
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
    }

    /**
     * Retrieves the Enrollment data fields.
     */
    protected class ValidateName implements FieldValidator {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            Person student = (Person) entity.getBean();
            if (student.getNameView().toLowerCase().contains(" and ")) {
                errors.add(new StateReportValidationError(entity.toString(), "Invalid Name",
                        "'And' found", "Invalid name: " + student.getNameView()));
            }
            return errors;
        }

    }

    protected StudentHistoryHelper m_helper;


    /**
     * Initialize the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * Build helper object.
         */

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {

            X2Criteria personCriteria = new X2Criteria();
            BeanQuery query = new BeanQuery(Person.class, personCriteria);

            // Set the query to be used for student selection.
            setQuery(query);
            setEntityClass(StudentNameEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put("PREFERRED-NAME", new ValidateName());
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Initialize fields.
     */
    public void initializeFields() {
        // Empty
    }

}
