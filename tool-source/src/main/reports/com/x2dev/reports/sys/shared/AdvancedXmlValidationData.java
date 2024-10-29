/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.shared;

import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.business.ToolManager;
import com.follett.fsc.core.k12.business.exports.ExportFactory;
import com.follett.fsc.core.k12.business.exports.ExportInterface;
import com.follett.fsc.core.k12.business.exports.ExportValidationError;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.ToolQueue;
import com.follett.fsc.core.k12.tools.ToolQueueFactory;
import com.follett.fsc.core.k12.tools.exports.AdvancedXmlDefinitionExport;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;
import java.util.List;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * This is a report class that performs standardized error and validation reporting for
 * the advanced XML exports.
 * this class will identify a procedure that contains a state report definition.
 * It will use that definition to final all configuration errors, data load errors
 * and validation errors.
 *
 * @author X2 Development Corporation
 */
public class AdvancedXmlValidationData extends ReportJavaSourceNet {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Grid column that contains the bean name and attributes.
     */
    public static final String COLUMN_BEAN_NAME = "beanName";

    /**
     * Grid column that contains the field that failed validation.
     */
    public static final String COLUMN_FIELD_NAME = "fieldName";

    /**
     * Grid column that contains the message that describes the fail in validation.
     */
    public static final String COLUMN_MESSAGE = "message";

    /**
     * Grid column that contains the value for the field that failed validation.
     */
    public static final String COLUMN_VALUE = "values";

    /**
     * Grid column that contains the value for the field that failed validation.
     */
    public static final String EXPORT_ID = "exportId";

    private static final String PARAM_EXPORT_TITLE = "exportTitle";

    private ImportExportDefinition m_importExport;
    private AdvancedXmlDefinitionExport m_xmlExport;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid grid = new ReportDataGrid(1000, 4);

        // Set report parameters.
        addParameter(PARAM_EXPORT_TITLE, m_importExport.getName());

        try {
            ExportInterface exportInterface = ExportFactory.getExport(m_importExport,
                    m_xmlExport,
                    getParameters(),
                    getBroker(),
                    getLocale());

            exportInterface.run();

            List<ExportValidationError> errors = exportInterface.getValidationErrors();

            for (ExportValidationError e : errors) {
                grid.append();
                grid.set(COLUMN_BEAN_NAME, e.getBeanName());
                grid.set(COLUMN_FIELD_NAME, e.getFieldName());
                grid.set(COLUMN_MESSAGE, e.getMessage());
                grid.set(COLUMN_VALUE, e.getValue());
            }
        } catch (Exception e) {
            String message = (e.getMessage() == null) ? "" : e.getMessage();
            AppGlobals.getLog().log(Level.WARNING, message, e);
        }

        grid.beforeTop();
        return grid;
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
        m_importExport = (ImportExportDefinition) ToolManager.getToolForId(Tool.TYPE_IMPORT_EXPORT,
                (String) getParameter(EXPORT_ID), getBroker());

        ToolQueue queue = ToolQueueFactory.getToolQueue();
        try {
            ToolJob toolJob = ToolJob.createJob(m_importExport, userData, userData.getUserTempFolder(),
                    queue.isRemote(), getLocale());
            m_xmlExport = (AdvancedXmlDefinitionExport) toolJob.getToolJavaSource();

            m_xmlExport.initialize();
            m_xmlExport.initializeTransients();
        } catch (Exception e) {
            throw new X2BaseException(e);
        }
    }
}
