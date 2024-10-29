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
package com.x2dev.procedures.sys.setup.ri;

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;

/**
 * The Class UpdateAliasNames.
 */
public class UpdateAliasNames extends ProcedureJavaSource {
    private ModelBroker m_broker;
    private DataDictionary m_dictionary;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        renameAlias("all.ssc.PSEnrollmentType", "all-ssc-PSEnrollmentType");
        renameAlias("all.ssc.PSInstitutionCourseCredit", "all-ssc-PSInstitutionCourseCredit");
        renameAlias("all.ssc.PSInstitutionCourseId", "all-ssc-PSInstitutionCourseId");
        renameAlias("all.ssc.PSInstitutionCourseName", "all-ssc-PSInstitutionCourseName");
        renameAlias("all.ssc.PSInstitutionId", "all-ssc-PSInstitutionId");

        renameAlias("all.trn.PSEnrollmentType", "all-trn-PSEnrollmentType");
        renameAlias("all.trn.PSInstitutionCourseCredit", "all-trn-PSInstitutionCourseCredit");
        renameAlias("all.trn.PSInstitutionCourseId", "all-trn-PSInstitutionCourseId");
        renameAlias("all.trn.PSInstitutionCourseName", "all-trn-PSInstitutionCourseName");
        renameAlias("all.trn.PSInstitutionId", "all-trn-PSInstitutionId");

        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_broker = new ModelBroker(getPrivilegeSet());
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Some difficulties were experienced running this on the report server.
     * Normal results were observed using application server.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        this.runOnApplicationServer();
        super.saveState(userData);
    }

    /**
     * Rename alias.
     *
     * @param oldName String
     * @param newName String
     */
    private void renameAlias(String oldName, String newName) {
        DataDictionaryField dictionaryField = m_dictionary.findDataDictionaryFieldByAlias(oldName);
        if (dictionaryField != null) {
            DataFieldConfig field = dictionaryField.getDataFieldConfig();
            String alias = field.getAlias();
            if (alias.contains(oldName)) {
                alias = alias.replace(oldName, newName);
                field.setAlias(alias);
                m_broker.saveBeanForced(field);
                logMessage("Alias " + oldName + " updated to " + newName);
            } else {
                logMessage("Alias " + oldName + " not found in " + alias);
            }
        } else {
            logMessage("Alias " + oldName + " not found in dictionary");
        }
    }

}
