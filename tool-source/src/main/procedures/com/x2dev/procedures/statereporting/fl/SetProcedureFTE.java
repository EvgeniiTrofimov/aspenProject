/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;

/**
 * The Class SetProcedureSSC.
 */
public class SetProcedureFTE extends SetProcedure {

    protected static final String ALIAS_SCHOOL_ID = "all-skl-StateId";

    private DataDictionaryField m_fieldSchoolId;

    private FLStudentHelper m_studentHelper;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        // for (int i = 0; i < 3; i++) {
        // super.execute();
        runAndValidateExports();
        // }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize(FLExportConfiguration.FL_EXPORT.FTE, SisStudent.class);

        m_studentHelper = getFLReportData().getStudentHelper();

        m_fieldSchoolId = getFLReportData().getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_SCHOOL_ID);
    }
}
