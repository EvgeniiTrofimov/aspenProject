/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import java.util.Collection;
import java.util.List;

/**
 * The Class FLStateReportEntity.
 */
public class FLStateReportEntity extends StateReportEntity {

    /**
     * The Interface WdisEntity.
     */
    public interface WdisEntity {

        /**
         * Gets the program.
         *
         * @return Student program participation
         */
        public StudentProgramParticipation getProgram();
    }

    private static final int FIELD_POSITION_RECORD_ID = 0;

    /**
     * Filter entity if any entity with the same unique key has been already reported.
     *
     * @return StateReportValidationError
     */
    @Override
    public StateReportValidationError filterEntity() {
        StateReportValidationError error = super.filterEntity();
        if (error == null) {
            FLStateReportData flData = (FLStateReportData) getData();
            List<String> paths = flData.getKeysBeanPaths();

            StringBuilder beanKey = new StringBuilder();
            for (String keyPath : paths) {
                beanKey.append(this.getFieldValue(keyPath));
            }

            String uniqueKey = beanKey.toString();

            if (!flData.isUniqueKey(uniqueKey)) {
                error = new StateReportValidationError(this, getData().getFieldDefinition(FIELD_POSITION_RECORD_ID),
                        "Entity with duplicate key was not reported", "Duplicate row with key = " + uniqueKey);
            }
        }

        return error;
    }

    /**
     * Add the filter entity error to the fields collection for validation report.
     *
     * @param index int
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValidation(int)
     */
    @Override
    public Collection<StateReportValidationError> getFieldValidation(int index) {
        Collection<StateReportValidationError> errors = super.getFieldValidation(index);
        if (index == 0) {
            StateReportValidationError err = filterEntity();
            if (err != null) {
                errors.add(err);
            }
        }
        return errors;
    }

}
