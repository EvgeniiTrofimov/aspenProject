/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.List;

/**
 * RI IEP should take effect 10 days after the final IEP Meeting.
 * The workflow may not automatically activate this, but the dates are set in the IEP.
 *
 * The workflow will now set the IEP to PENDING_APPROVAL and leave the Student Special Ed status
 * alone.
 *
 * After the IEP record start date has passed, we should chnage the IEP status to Active, and
 * adjust the previous IEP and Student Special Ed Status for the new IEP.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class ActivateIEPProcedure extends ProcedureJavaSource {

    protected static final String PREFERENCE_KEY = "sys.sped.ri.delayActivation";
    protected static final String PARAM_SET_PREFERENCE = "setPreference";
    protected static final String VALUE_SET_TRUE = "true";
    protected static final String VALUE_SET_FALSE = "false";
    protected static final String VALUE_SET_SHOW = "show";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (checkPreference()) {
            logMessage("Running IEP Activation for " + (new PlainDate()).toString());
            for (IepData iep : getPendingIEPs()) {
                activateIep(iep);
            }
        }
    }

    /**
     * Activate the IEP, also set previous IEP and set student special ed status if needed.
     *
     * @param iep
     */
    private void activateIep(IepData iep) {
        SisStudent student = iep.getStudent();
        IepData activeIep = student.getActiveIep(getBroker());

        // retire the previous IEP.
        if (activeIep != null) {
            activeIep.setStatusCode(IepData.StatusCode.PREVIOUS.ordinal());
            getBroker().saveBeanForced(activeIep);
        }

        // Activate the pending IEP.
        iep.setStatusCode(IepData.StatusCode.ACTIVE.ordinal());
        getBroker().saveBeanForced(iep);

        // Set student special ed status to Active.
        String studentActiveCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.SPED_ACTIVE_CODE);
        if (!studentActiveCode.equals(student.getSpedStatusCode())) {
            student.setSpedStatusCode(studentActiveCode);
            getBroker().saveBeanForced(student);
        }

        String message = "Activating IEP for " + student.getNameView() + "  " + student.getLocalId();
        logMessage(message);
    }

    /**
     * Check:
     * Is the input parameter set to set the preference? If so, just set the parameter and return
     * false.
     * If the input parameter is not assigning the preference, is it set to do Activation.
     *
     * @return true if not setting preference value, and value is true.
     */
    private boolean checkPreference() {
        String param = (String) getParameter(PARAM_SET_PREFERENCE);

        // See if the preference exists, and create it.
        SystemPreferenceDefinition sysDef =
                PreferenceManager.getPreferenceDefinition(PREFERENCE_KEY, getBroker().getPersistenceKey());
        if (VALUE_SET_SHOW.equals(param) && sysDef == null) {
            logMessage("Delayed IEP Activation is not enabled. "
                    + PREFERENCE_KEY);
            return false;
        }

        if (sysDef == null) {
            try {
                sysDef = PreferenceManager.addNewPreferenceDefinition(PREFERENCE_KEY, "RI IEP Activation", "Special Ed",
                        "RI IEP Activation", 1, getBroker(), true);
                logMessage("Creating the new preference " + PREFERENCE_KEY);
                PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), PREFERENCE_KEY, "false");
            } catch (InvalidPreferenceException e) {
                logMessage("Error setting preference value for " + PREFERENCE_KEY + ": " + e.getMessage());
                return false;
            }
        }

        if (VALUE_SET_TRUE.equals(param)) {
            try {
                PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), PREFERENCE_KEY, "true");
            } catch (InvalidPreferenceException e) {
                logMessage("Error setting preference value for " + PREFERENCE_KEY + ": " + e.getMessage());
                return false;
            }
            // message the status change, and return false so the procedure does not continue.
            logMessage("Delayed activation is now enabled.");
            return false;
        } else if (VALUE_SET_FALSE.equals(param)) {
            try {
                PreferenceManager.setPreferenceValue(getOrganization(), getBroker(), PREFERENCE_KEY, "false");
            } catch (InvalidPreferenceException e) {
                logMessage("Error setting preference value for " + PREFERENCE_KEY + ": " + e.getMessage());
                return false;
            }
            // message the status change, and return false so the procedure does not continue.
            logMessage("Set preference for " + PREFERENCE_KEY + " to 'false'.");
            return false;
        } else if (VALUE_SET_SHOW.equals(param)) {
            String currentValue = PreferenceManager.getPreferenceValue(getOrganization(), PREFERENCE_KEY);
            logMessage("Delayed activation is currently set to " + currentValue);
            return false;
        } else {
            String currentValue = PreferenceManager.getPreferenceValue(getOrganization(), PREFERENCE_KEY);
            logMessage("Delayed activation is currently set to " + currentValue);
            // if the current preference is false, return false so the procedure does not continue.
            if (!Boolean.parseBoolean(currentValue)) {
                logMessage("To run the activation procedure, first enable.");
                return false;
            }
            return true;
        }
    }

    /**
     * Query for Pending IEP that are on or before today.
     *
     * @return List<IepData>
     */
    private List<IepData> getPendingIEPs() {
        PlainDate today = new PlainDate();
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(IepData.COL_STATUS_CODE, IepData.StatusCode.PENDING_APPROVAL.ordinal());
        criteria.addLessOrEqualThan(IepData.COL_START_DATE, today);
        BeanQuery query = new BeanQuery(IepData.class, criteria);
        return (List<IepData>) getBroker().getCollectionByQuery(query);
    }
}
