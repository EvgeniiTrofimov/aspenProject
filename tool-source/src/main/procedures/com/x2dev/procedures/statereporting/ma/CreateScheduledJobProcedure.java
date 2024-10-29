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
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.JobEntry;
import com.follett.fsc.core.k12.beans.JobEntry.DeliveryTypeCode;
import com.follett.fsc.core.k12.beans.JobEntry.SchedDayTypeCode;
import com.follett.fsc.core.k12.beans.JobEntry.ToolTypeCode;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.JobEntryManager;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;

/**
 * Procedure that creates a scheduled job that runs a procedure once at a later time and date.
 *
 * @author Follett Software Company
 */
public class CreateScheduledJobProcedure extends ProcedureJavaSource {
    private static final String PROCEDURE_ID = "SIF-PUB-ALL-MULTI";

    private ToolInput m_toolInput = null;
    private Procedure m_procedure = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (m_toolInput != null) {
            ProcedureRunTime runTime = getProcedureRunTime();
            if (runTime == null) {
                logMessage("Error - unable to determine a run time for deployment "
                        + getBroker().getPersistenceKey().getDeploymentId());
            } else {
                JobEntry job = X2BaseBean.newInstance(JobEntry.class, getBroker().getPersistenceKey());

                job.setOrganization1Oid(getOrganization().getOid());
                job.setUserOid(getUser().getOid());
                job.setStatus(true); // Job is enabled
                job.setDeliveryTypeEnum(DeliveryTypeCode.NONE);
                job.setName(m_procedure.getName());
                job.setToolOid(m_procedure.getOid());
                job.setToolTypeEnum(ToolTypeCode.PROCEDURE);
                job.setDayTypeEnum(SchedDayTypeCode.ONCE); // Runs only once
                job.setInputDefaults(true); // Use default values from the input definition

                Calendar calendar = Calendar.getInstance();
                calendar.setTimeInMillis(0);
                calendar.set(Calendar.HOUR_OF_DAY, runTime.runHour);
                calendar.set(Calendar.MINUTE, runTime.runMinute);
                job.setStartTime(new PlainTime(calendar.getTimeInMillis()));

                job.setStartDate(runTime.runDate);

                /*
                 * Get tool input data, convert to XML for storage.
                 */
                job.setToolInput(JobEntryManager.prepareToolInput(m_toolInput, getLocale()));

                getBroker().saveBeanForced(job);

                if (job.getNextRunDate() == 0) {
                    logMessage("Error - the job did not schedule.");
                } else {
                    logMessage("Success - procedure scheduled to run on "
                            + DateFormat.getDateTimeInstance().format(new Date(job.getNextRunDate())));
                }
            }
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_procedure = findProcedure();

        if (m_procedure == null) {
            logMessage("Error - unable to find procedure to schedule " + PROCEDURE_ID);
        } else {
            // Create the tool input here since it requires userData
            m_toolInput = new ToolInput(m_procedure, new HashMap<String, String>(), userData, getLocale());
        }
    }

    /**
     * Returns the procedure being scheduled.
     * 
     * @return Procedure
     */
    private Procedure findProcedure() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Procedure.COL_ID, PROCEDURE_ID);

        BeanQuery beanQuery = new BeanQuery(Procedure.class, criteria);

        return (Procedure) getBroker().getBeanByQuery(beanQuery);
    }

    /**
     * Returns the run time for the current deployment.
     * 
     * @return ProcedureRunTime
     */
    private ProcedureRunTime getProcedureRunTime() {
        String deploymentId = getBroker().getPersistenceKey().getDeploymentId();

        PlainDate today = new PlainDate();

        Calendar calendar = Calendar.getInstance();
        calendar.setTimeInMillis(System.currentTimeMillis());
        calendar.add(Calendar.DAY_OF_YEAR, 1);

        PlainDate tomorrow = new PlainDate(calendar.getTimeInMillis());

        ProcedureRunTime runTime = null;

        if ("ma-fitchburg".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-framingham".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-chelsea".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-saugus".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-amesbury".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-plymouth".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-abington".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-easton".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-everett".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-billerica".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-duxbury".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-rockland".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-dighton-rehoboth".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 2, 0);
        } else if ("ma-holbrook".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 2, 0);
        } else if ("ma-braintree".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-lowell".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-naschools".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-nashoba".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-newburyport".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-walpole".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-ludlow".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-melrose".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-montytech".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-franklin".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-innovation".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-westwood".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-wilmington".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-concord".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-glts".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-middleborough".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-nmrsd".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-scituate".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-swampscott".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-triton".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-wrentham".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-marlborough".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-npsk".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-westbridgewater".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-leominster".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-milford".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 16, 0);
        } else if ("ma-chelmsford".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-millbury".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-blackstone-millville".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-dartmouth".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        } else if ("ma-bluehills".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-bptech".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-fairhaven".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-minuteman".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 22, 0);
        } else if ("ma-hingham".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-lexington".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-nauset".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 2, 0);
        } else if ("ma-norwell".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 2, 0);
        } else if ("ma-hanover".equals(deploymentId)) {
            runTime = new ProcedureRunTime(tomorrow, 0, 0);
        } else if ("ma-dover-sherborn".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 18, 0);
        } else if ("ma-barnstable".equals(deploymentId)) {
            runTime = new ProcedureRunTime(today, 20, 0);
        }

        return runTime;
    }

    /**
     * Simple structure wrapping a run date, hour, and minute.
     * 
     * @author Follett Software Company
     */
    static class ProcedureRunTime {
        PlainDate runDate;
        int runHour;
        int runMinute;

        /**
         * Instantiates a new procedure run time.
         *
         * @param runDate PlainDate
         * @param runHour int
         * @param runMinute int
         */
        ProcedureRunTime(PlainDate runDate, int runHour, int runMinute) {
            this.runDate = runDate;
            this.runHour = runHour;
            this.runMinute = runMinute;
        }
    }
}
