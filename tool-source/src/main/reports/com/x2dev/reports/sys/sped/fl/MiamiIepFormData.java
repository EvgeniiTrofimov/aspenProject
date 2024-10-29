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
package com.x2dev.reports.sys.sped.fl;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;

/**
 * Java source for the Miami IEP form.
 *
 * @author X2 Development Corporation
 */
public class MiamiIepFormData extends BaseFormReportJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private IepData m_currentIep = null;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        IepData iep = getIep();

        return new SimpleFormDataSource(iep, getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Returns the current IEP. If a blank form is being printed, a new (unsaved) IEP is created and
     * returned.
     *
     * @return IepData
     */
    private IepData getIep() {
        IepData iep = null;

        if (isBlank()) {
            IepData ownerIep = (IepData) getFormOwner();

            iep = new IepData(getBroker().getPersistenceKey());
            iep.setStudentOid(ownerIep.getStudentOid());
            iep.setStaffOid(ownerIep.getStaffOid());
        } else {
            iep = (IepData) getFormStorage();
        }

        return iep;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        if (getFormInstance() != null || getFormDefinition() != null) {
            super.initialize();
        } else if (m_currentIep != null) {
            setFormOwner(m_currentIep);
            setFormStorage(m_currentIep);
            setDictionary(DataDictionary.getDistrictDictionary(m_currentIep.getExtendedDataDictionary(),
                    getBroker().getPersistenceKey()));

            addFormParameters();
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @see
     *      com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#saveState(com.follett.fsc.
     *      core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        super.saveState(userData);

        m_currentIep = userData.getCurrentRecord(IepData.class);
    }
}
