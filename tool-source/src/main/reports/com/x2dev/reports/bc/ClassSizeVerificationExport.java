/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2014 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.reports.bc;

import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.x2dev.reports.bc.ClassSizeVerificationData.ClassSizeVerificationDataGrid;
import com.x2dev.reports.bc.ClassSizeVerificationData.EXPORT_FIELDS;
import com.x2dev.reports.bc.ClassSizeVerificationData.EXPORT_TYPE;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * Data source for Class Size Verification export. Any changes to the various enums and the
 * ClassSizeVerificationDataGrid
 * needs to be applied to ClassSizeVerificationExport.
 *
 * @author X2 Development Corporation
 */
public class ClassSizeVerificationExport extends ExportJavaSource {
    /*
     * Export-specific input parameters
     */
    private static final String PARAM_REPORT_EXPORT_TYPE = "exportType";

    private EXPORT_TYPE m_exportType;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        ClassSizeVerificationData reportData = new ClassSizeVerificationData();
        ClassSizeVerificationDataGrid classSizeGrid =
                reportData.new ClassSizeVerificationDataGrid(getBroker(), getParameters());

        classSizeGrid.evaluateGrid();
        classSizeGrid.deleteColumn(EXPORT_FIELDS.FIELD_SCHOOL.getFieldId());
        classSizeGrid.beforeTop();

        return classSizeGrid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        List<String> columnNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            if (field.getFieldType().contains(m_exportType)) {
                columnNames.add(field.getFieldId());
            }
        }

        return columnNames;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        List<String> columnUserNames = new ArrayList(EXPORT_FIELDS.values().length);

        for (EXPORT_FIELDS field : EXPORT_FIELDS.values()) {
            if (field.getFieldType().contains(m_exportType)) {
                columnUserNames.add(field.getFieldName());
            }
        }

        return columnUserNames;
    }

    /**
     * Gets the custom file name.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        return StringUtils.leftPad(getSchool().getSchoolId(), 8, "0") + "_class_size_"
                + String.valueOf(getSchool().getCurrentContext().getSchoolYear()) + ".csv";
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        setUseEscapes(false);

        Integer exportTypeValue = (Integer) getParameter(PARAM_REPORT_EXPORT_TYPE);
        m_exportType = EXPORT_TYPE.getByOrdinal(exportTypeValue.intValue());

        /*
         * Ministry export does not use a header row and wraps ALL values in quotes
         */
        if (EXPORT_TYPE.MINISTRY_EXPORT == m_exportType) {
            setIncludeHeaderRow(false);
        } else {
            setUseValueWrappers(false);
        }
    }
}
