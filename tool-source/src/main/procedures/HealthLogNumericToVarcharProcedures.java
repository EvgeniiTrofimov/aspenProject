/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * Build Version: @@@version@@@
 *
 * ====================================================================
 */

import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.HealthLog;

/**
 * Procedure to update four health log fields from zero to null.
 */
public class HealthLogNumericToVarcharProcedures extends ProcedureJavaSource {
    /**
     * Health Log Fields to change
     */
    private static final String HLG_TEMPERATURE = "HLG_TEMPERATURE";
    private static final String HLG_BP_SYSTOLIC = "HLG_BP_SYSTOLIC";
    private static final String HLG_BP_DIASTOLIC = "HLG_BP_DIASTOLIC";
    private static final String HLG_PULSE = "HLG_PULSE";


    /**
     * Perform updates on four health log fields
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        logMessage("Number of Health Log Temperatures updated = "
                + updateHealthLog(HLG_TEMPERATURE, HealthLog.COL_BODY_TEMPERATURE, "0.00"));
        logMessage("Number of Health Log Systolics updated = "
                + updateHealthLog(HLG_BP_SYSTOLIC, HealthLog.COL_SYSTOLIC_B_P, null));
        logMessage("Number of Health Log Diastolics updated = "
                + updateHealthLog(HLG_BP_DIASTOLIC, HealthLog.COL_DIASTOLIC_B_P, null));
        logMessage("Number of Health Log Pulses updated = " + updateHealthLog(HLG_PULSE, HealthLog.COL_PULSE, null));

    }

    /**
     * Updates the various health logs with zero values to null
     *
     * @param attribute String
     * @param colName String
     * @param zeroValue String
     * @return number of results updated
     */
    private int updateHealthLog(String attribute, String colName, String zeroValue) {
        X2Criteria attributeCriteria = new X2Criteria();
        int zero = 0;
        if (zeroValue != null) {
            attributeCriteria.addEqualTo(attribute, zeroValue);
        } else {
            attributeCriteria.addEqualTo(attribute, zero);
        }

        UpdateQuery updateQuery = new UpdateQuery(HealthLog.class, attributeCriteria,
                colName, null);
        return getBroker().executeUpdateQuery(updateQuery);
    }

}
