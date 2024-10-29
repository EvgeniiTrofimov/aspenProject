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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import java.util.Collection;
import java.util.List;

/**
 * The Class TNStateReportEntity.
 */
public class TNStateReportEntity extends StateReportEntity {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 

    private static final String EXPORT_TYPE_PREFIX = "V";

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
            TNStateReportData tnData = (TNStateReportData) getData();
            List<String> paths = tnData.getKeysBeanPaths();

            StringBuilder beanKey = new StringBuilder();
            for (String keyPath : paths) {
                beanKey.append(this.getFieldValue(keyPath));
            }

            String uniqueKey = beanKey.toString();

            if (!tnData.isUniqueKey(uniqueKey)) {
                error = new StateReportValidationError(this, getData().getFieldDefinition(FIELD_POSITION_RECORD_ID),
                        "Entity with duplicate key was not reported", "Duplicate row with key = " + uniqueKey);
                // keep counters in actual state
                tnData.addEntityRowsCount(-1);
            }
        }

        return error;
    }

    /**
     * Use the version number from TNStateReportData to generate the definition id.
     * If TNStateReportData.getExportVersion() == 0 then return null indicating that the original
     * format is used.
     *
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getCurrentFormatDefinitionId()
     */
    @Override
    public String getCurrentFormatDefinitionId() {
        String value = null;
        if (getData() instanceof TNStateReportData) {
            TNStateReportData tnData = (TNStateReportData) getData();
            if (tnData.getExportVersion() > 0) {
                return EXPORT_TYPE_PREFIX + tnData.getExportVersion();
            }
        }
        return value;
    }

    /**
     * Add the filter entity error to the fields collection.
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

    /**
     * Gets the field value.
     *
     * @param index int
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getFieldValue()
     */
    @Override
    public String getFieldValue(int index) {
        String value = super.getFieldValue(index);
        if (value != null) {
            value = value.toUpperCase();
        }
        return value;
    }

}
