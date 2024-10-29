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

package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;

/**
 * The Class UpdatingAliasesForSscAndTrnProcedure.
 *
 * @author Follett Software Company
 *
 *         Procedure which will be looking for the fields with old aliases and add new aliases.
 *         This procedure is intended for one-time use to set the new aliases
 */
public class UpdatingAliasesForSscAndTrnProcedure extends ProcedureJavaSource {
    /**
     * Alias fields
     */
    protected static final String[] SSC_NEW_ALIASES = new String[] {"all-ssc-CourseEnrollmentType",
            "all-ssc-ProviderID",
            "all-ssc-ProviderCourseID",
            "all-ssc-ProviderCourseName",
            "all-ssc-ProviderCourseCredits"};

    protected static final String[] SSC_OLD_ALIASES = new String[] {"all-ssc-PSEnrollmentType",
            "all-ssc-PSInstitutionId",
            "all-ssc-PSInstitutionCourseId",
            "all-ssc-PSInstitutionCourseName",
            "all-ssc-PSInstitutionCourseCredit"};

    protected static final String[] TRN_NEW_ALIASES = new String[] {"all-trn-CourseEnrollmentType",
            "all-trn-ProviderID",
            "all-trn-ProviderCourseID",
            "all-trn-ProviderCourseName",
            "all-trn-ProviderCourseCredits",
            "all-trn-ProviderCreditEarned"};

    protected static final String[] TRN_OLD_ALIASES = new String[] {"all-trn-PSEnrollmentType",
            "all-trn-PSInstitutionId",
            "all-trn-PSInstitutionCourseId",
            "all-trn-PSInstitutionCourseName",
            "all-trn-PSInstitutionCourseCredit",
            "all-trn-PSCredtsEarned"};

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    private DataDictionary m_dictionary;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (SSC_NEW_ALIASES.length == SSC_NEW_ALIASES.length) {
            for (int i = 0; i < SSC_NEW_ALIASES.length; i++) {
                updateAlias(SSC_OLD_ALIASES[i], SSC_NEW_ALIASES[i]);
            }
        }

        if (TRN_NEW_ALIASES.length == TRN_NEW_ALIASES.length) {
            for (int i = 0; i < TRN_NEW_ALIASES.length; i++) {
                updateAlias(TRN_OLD_ALIASES[i], TRN_NEW_ALIASES[i]);
            }
        }

        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);
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

        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
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
    }

    /**
     * Looking for the fields with old aliases and set new aliases after old separated by comma.
     *
     * @param oldAlias String
     * @param newAlias String
     */
    public void updateAlias(String oldAlias, String newAlias) {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(newAlias);
        if (field == null) {
            field = m_dictionary.findDataDictionaryFieldByAlias(oldAlias);
            DataFieldConfig dataFieldConf = null;

            if (field != null && (dataFieldConf = field.getDataFieldConfig()) != null) {
                dataFieldConf.setAlias(oldAlias + "," + newAlias);
                getBroker().saveBeanForced(dataFieldConf);
                logMessage("Alias = " + oldAlias + " updated to alias = " + oldAlias + "," + newAlias + ".");

            } else {
                logMessage("No field found for alias = " + oldAlias + ".");
            }
        } else {
            logMessage("Alias for " + newAlias + " already exists on "
                    + field.getDataFieldConfig().getDataField().getOid());
        }
    }
}
