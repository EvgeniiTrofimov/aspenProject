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

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.X2BaseException;

/**
 * @author Follett Software Company
 * 
 * Procedure to update old aliases using in CATE exports.
 */
public class UpdateCateAliasesProcedure extends ProcedureJavaSource
{

    public enum ALIASES
    {
        ALIAS_1500("i4see 1500", "all-pgm-SendingSchoolSAU"),
        ALIAS_1510("i4see 1510", "all-pgm-SendingDistrict"),
        ALIAS_1520("i4see 1520", "all-pgm-SendingSchool"),
        ALIAS_1600("i4see 1600", "all-pgm-ReceivingSchoolSAU"),
        ALIAS_1610("i4see 1610", "all-pgm-ReceivingDistrict"),
        ALIAS_1620("i4see 1620", "all-pgm-ReceivingSchool"),
        ALIAS_1700("i4see 1700", "all-std-CATEEnrollmentStatus"),
        ALIAS_230("i4see 230 CATE", "all-pgm-EntryDate"),
        ALIAS_240("i4see 240 CATE", "all-pgm-EntryCode"),
        ALIAS_250("i4see 250 CATE", "all-pgm-ExitDate"),
        ALIAS_260("i4see 260 CATE", "all-pgm-ExitCode"),
        ALIAS_1720("i4see 1720", "all-pgm-ProgramCompleter"),
        ALIAS_141("i4see 141", "all-std-work"),
        ALIAS_1730("i4see 1730", "all-pgm-TransportationMode"),
        ALIAS_160("i4see 160", "all-std-DisplacedHomemaker"),
        ALIAS_161("i4see 161", "all-std-SingleParent"),
        ALIAS_162("i4see 162", "all-pgm-PrimaryProgramIndicator"),
        ALIAS_163("i4see 163", "all-pgm-TSA");

        private String m_oldAlias;
        private String m_newAlias;

        public String getOldAlias()
        {
            return m_oldAlias;
        }

        public String getNewAlias()
        {
            return m_newAlias;
        }

        private ALIASES(String oldAlias, String newAlias)
        {
            m_oldAlias = oldAlias;
            m_newAlias = newAlias;
        }

    }

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    private DataDictionary m_dictionary;

    @Override
    protected void execute() throws Exception
    {
        for (ALIASES aliasPair : ALIASES.values())
        {
            updateAlias(aliasPair.getOldAlias(), aliasPair.getNewAlias());
        }

        DataDictionaryCache.clearDictionaries(getUser().getPersistenceKey(), true);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException
    {
        super.initialize();

        initializeFields();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException
    {
        super.saveState(userData);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields()
    {
        m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Looking for the fields with old aliases and set new aliases after old separated by comma.
     *
     * @param oldAlias
     * @param newAlias
     *
     */
    public void updateAlias(String oldAlias, String newAlias)
    {
        DataDictionaryField field = m_dictionary.findDataDictionaryFieldByAlias(newAlias);
        if (field == null)
        {
            field = m_dictionary.findDataDictionaryFieldByAlias(oldAlias);
            DataFieldConfig dataFieldConf = null;

            if (field != null && (dataFieldConf = field.getDataFieldConfig()) != null)
            {
                dataFieldConf.setAlias(newAlias + "," + oldAlias);
                getBroker().saveBeanForced(dataFieldConf);
                logMessage("Alias = " + oldAlias + " updated to alias = " + field.getAlias() + ", field = " + dataFieldConf.getDataField().getOid());

            }
            else
            {
                logMessage("No field found for alias = " + oldAlias + ".");
            }
        }
        else
        {
            logMessage("Alias for " + newAlias + " already exists on " + field.getDataFieldConfig().getDataField().getOid());
        }
    }
}
