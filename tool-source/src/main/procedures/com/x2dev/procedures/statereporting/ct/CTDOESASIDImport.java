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

package com.x2dev.procedures.statereporting.ct;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.File;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure to import IEP data.
 *
 * @author Follett Software Company
 */
public class CTDOESASIDImport extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String INPUT_INDEX_DISTR_ID = "distritId";
    private static final String INPUT_INDEX_SASID = "sasid";
    private static final String INPUT_PARAM_SKIP_ROWS = "skipRows";

    /**
     * Fields
     */
    private int m_indexSASID;
    private int m_indexDistrId;
    private List<String> m_messages = new LinkedList();
    private int m_skipRows;
    private Map<String, SisStudent> m_studentMap;
    private UserDataContainer m_userData = null;

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 0;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        super.importData(sourceFile);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        if (lineNumber > m_skipRows) {
            String sasid = record.get(m_indexSASID);
            String districtId = record.get(m_indexDistrId);
            if (!StringUtils.isEmpty(districtId) && !StringUtils.isEmpty(sasid)) {
                SisStudent std = m_studentMap.get(districtId);
                if (std != null) {
                    String currentSasid = std.getStateId();
                    if (!sasid.equals(currentSasid)) {
                        std.setStateId(sasid);
                        getBroker().saveBeanForced(std);
                        incrementUpdateCount();
                    } else {
                        incrementMatchCount();
                        logInvalidRecord(lineNumber, "SASID for STD with LocalId = " + std.getLocalId()
                                + " matches SASID from import file.");
                    }
                } else {
                    incrementSkipCount();
                    logInvalidRecord(lineNumber, "No student was found with LASID = " + districtId + ".");
                }

            } else {
                incrementSkipCount();
                logInvalidRecord(lineNumber, "Not values are in file for District Student ID and SASID.");
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_skipRows = ((Integer) getParameter(INPUT_PARAM_SKIP_ROWS)).intValue();
        initializeIndexes();
        loadStudentMap();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_userData = userData;
    }

    /**
     * Initialize column numbers in CSV file.
     */
    private void initializeIndexes() {
        m_indexSASID = ((Integer) getParameter(INPUT_INDEX_SASID)).intValue();
        m_indexDistrId = ((Integer) getParameter(INPUT_INDEX_DISTR_ID)).intValue();
    }

    /**
     * Load student map.
     */
    private void loadStudentMap() {
        m_studentMap = getBroker().getMapByQuery(new QueryByCriteria(SisStudent.class, new X2Criteria()),
                SisStudent.COL_LOCAL_ID, 1024);
    }
}
