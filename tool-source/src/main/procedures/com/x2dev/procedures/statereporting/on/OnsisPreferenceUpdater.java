/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;

/**
 * View and update Aspen preferences
 *
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnsisPreferenceUpdater extends ProcedureJavaSource {
    public static final String PARAM_PRD_KEY = "prdKey";
    public static final String PARAM_PRF_VALUE = "prfValue";

    /**
    *
    */
    private static final long serialVersionUID = 1L;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String prdKey = (String) getParameter(PARAM_PRD_KEY);
        Boolean prfValue = (Boolean) getParameter(PARAM_PRF_VALUE);

        String originalValue =
                PreferenceManager.getRootPreferenceValueNonBlocking(getBroker().getPersistenceKey(), prdKey);

        boolean currentStored = Boolean.parseBoolean(originalValue);
        if (Boolean.valueOf(currentStored).equals(prfValue)) {
            logMessage("System Preference [" + prdKey + "] nothing to do: " + originalValue);
            return;
        }

        logMessage("System Preference [" + prdKey + "] OLD value: " + originalValue);

        PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), prdKey, prfValue.toString());

        logMessage("System Preference [" + prdKey + "] setting value: " + prfValue);

        String storedValue =
                PreferenceManager.getRootPreferenceValueNonBlocking(getBroker().getPersistenceKey(), prdKey);

        logMessage("System Preference [" + prdKey + "] CONFIRMED/STORED value: " + storedValue);
    }
}
