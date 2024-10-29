/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2017 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.statereporting.pa;

import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * abstract class for add and report initial errors.
 *
 * @author Follett Software Company
 */
abstract public class PAPrintInitialErrorsReport extends ReportJavaSourceNet {
    private static final String ERROR_ID_ALIAS = "MISSING ALIAS";
    private static final String PARAM_REPORT_TITLE = "reportTitle";
    private static final String REPORT_ID_PA_INITIAL_ERRORS = "PA_INITIAL_ERRORS";


    /**
     * Format grid field keys.
     */
    private static final String FIELD_ERROR_ID = "errorId";
    private static final String FIELD_MESSAGE = "message";

    private Map<String, List<String>> m_initErrors = new HashMap<String, List<String>>();

    /**
     * add initial error.
     *
     * @param errorID any string which identify similar errors (for example MISSING ALIAS), report
     *        will group messages by error id
     * @param errorMeassage String
     */
    public void addInitialError(String errorID, String errorMeassage) {
        if (!StringUtils.isEmpty(errorID) && !StringUtils.isEmpty(errorMeassage)) {
            List<String> errorMessages = m_initErrors.get(errorID);
            if (errorMessages == null) {
                errorMessages = new ArrayList<String>();
                m_initErrors.put(errorID, errorMessages);
            }
            errorMessages.add(errorMeassage);
        }
    }

    /**
     * adding MISSING ALIAS error if alias doesn't exist .
     *
     * @param alias String
     */
    public void requiredAlias(String alias) {
        DataDictionary ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        requiredAlias(alias, ddx);
    }

    /**
     * adding MISSING ALIAS error if alias doesn't exist.
     *
     * @param alias String
     * @param ddx DataDictionary
     */
    public void requiredAlias(String alias, DataDictionary ddx) {
        if (!StringUtils.isEmpty(alias)) {
            DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                addInitialError(ERROR_ID_ALIAS, alias);
            }
        }

    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected final Object gatherData() throws Exception {
        Object returnObject = null;
        if (m_initErrors.size() > 0) {
            setFormatId(REPORT_ID_PA_INITIAL_ERRORS);
            Criteria criteria = new Criteria();
            criteria.addEqualTo(Report.COL_ID, REPORT_ID_PA_INITIAL_ERRORS);
            QueryByCriteria query = new QueryByCriteria(Report.class, criteria);
            Report report = (Report) getBroker().getBeanByQuery(query);

            String reportVersion = report.getEngineVersion();
            ((Report) getJob().getTool()).setEngineVersion(reportVersion);

            ReportDataGrid grid = new ReportDataGrid(300, 4);

            for (Entry<String, List<String>> entry : m_initErrors.entrySet()) {
                String errorId = entry.getKey();

                for (String errorMessage : entry.getValue()) {
                    grid.append();
                    grid.set(FIELD_ERROR_ID, errorId);
                    grid.set(FIELD_MESSAGE, errorMessage);
                }
            }
            addParameter(PARAM_REPORT_TITLE, ((Report) getJob().getTool()).getName());
            grid.beforeTop();
            returnObject = grid;
        } else {
            returnObject = reportGatherData();
        }

        return returnObject;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected final void initialize() throws X2BaseException {
        super.initialize();
        initialRequiredResources();
        if (m_initErrors.size() == 0) {
            reportInitialize();
        }
    }

    /**
     * method called before {@link #initialize()}. Need for check all required resources and add
     * error if required resources doesn't exist<br>
     * errors will reported for show user what should be configured.
     * for add error use {@link #addInitialError(String, String)} method or
     * {@link #requiredAlias(String)} and another common method/s, if somebody add them
     */
    protected abstract void initialRequiredResources();

    /**
     * method instead {@link #gatherData()}, because gatherData used and can not be override.
     *
     * @return Object
     * @throws Exception exception
     */
    protected abstract Object reportGatherData() throws Exception;

    /**
     * method instead {@link #initialize()}, because initialize used and can not be override.
     *
     * @throws X2BaseException exception
     */
    protected abstract void reportInitialize() throws X2BaseException;

}
