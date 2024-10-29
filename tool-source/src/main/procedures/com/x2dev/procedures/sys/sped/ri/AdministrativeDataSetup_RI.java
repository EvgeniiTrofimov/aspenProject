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
package com.x2dev.procedures.sys.sped.ri;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.CoreDataType;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Procedure for setting up four user fields needed in the Rhode Island
 * Administrative Data form (ADM1).
 *
 * @author X2 Development Corporation
 */
public class AdministrativeDataSetup_RI extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    public static final String CTJ_LANGUAGE_ALIAS = "contact-language";
    public static final String CTJ_LANGUAGE_NAME_LONG = "Home Native Language";
    public static final String CTJ_LANGUAGE_NAME_SHORT = "Home Lang";

    public static final String CTJ_INTERPRETER_ALIAS = "contact-interpreter";
    public static final String CTJ_INTERPRETER_NAME_LONG = "Interpreter Language (if needed)";
    public static final String CTJ_INTERPRETER_NAME_SHORT = "Interpreter";

    public static final String DETAIL_CONTROL_PICKLIST = "Picklist";

    public static final String ELL_ALIAS = "english-language-learner";
    public static final String ELL_NAME_LONG = "English Language Learner?";
    public static final String ELL_NAME_SHORT = "ELL?";

    public static final String HOME_SCHOOL_ALIAS = "home-school";
    public static final String HOME_SCHOOL_NAME = "Home School";

    public static final String LANGUAGE_REF_TABLE = "rtbLanguage";

    public static final String STUDENT_CONTACT_TABLE_OID = "tblStdContact";
    public static final String STUDENT_TABLE_OID = "tblStudent";

    private X2Broker m_broker;
    private DataFieldConfig m_ctjInterpreter;
    private DataFieldConfig m_ctjLanguage;
    private DataFieldConfig m_stdEll;
    private DataFieldConfig m_stdHomeSchool;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        if (loadFields()) {
            setFieldData();
            saveFields();
        } else {
            logMessage("Procedure skipped. Not enough available user fields.");
        }
    }

    /**
     * Loads the next available user field into each of the student and student contact member
     * variables. Returns true if there are available user fields for all 4 fields needed.
     * 
     * @return boolean
     */
    private boolean loadFields() {
        m_broker = getBroker();

        boolean fieldsAvailableInd = true;
        try {
            Iterator<DataFieldConfig> contactFields = DataDictionary.getAvailableFields(
                    STUDENT_CONTACT_TABLE_OID, 25, null, m_broker, Boolean.FALSE).iterator();
            m_ctjLanguage = contactFields.next();
            m_ctjInterpreter = contactFields.next();
            m_stdHomeSchool = DataDictionary.getAvailableField(STUDENT_TABLE_OID, 50, m_broker);
            m_stdEll = DataDictionary.getAvailableField(STUDENT_TABLE_OID, 1, m_broker);

            if (m_stdHomeSchool != null && m_stdHomeSchool.equals(m_stdEll)) {
                Iterator<DataFieldConfig> studentFieldsIt = DataDictionary.getAvailableFields(
                        STUDENT_TABLE_OID, 1, null, m_broker, Boolean.FALSE).iterator();
                while (m_stdHomeSchool.equals(m_stdEll)) {
                    m_stdEll = studentFieldsIt.next();
                }
            }
        } catch (NoSuchElementException e) {
            fieldsAvailableInd = false;
        }

        return fieldsAvailableInd &&
                m_ctjLanguage != null && m_ctjInterpreter != null &&
                m_stdHomeSchool != null && m_stdEll != null;
    }

    /**
     * Saves the 4 user fields in the database.
     */
    private void saveFields() {
        m_broker.saveBeanForced(m_ctjLanguage);
        m_broker.saveBeanForced(m_ctjInterpreter);
        m_broker.saveBeanForced(m_stdHomeSchool);
        m_broker.saveBeanForced(m_stdEll);
    }

    /**
     * Sets up the data for the 4 required user fields.
     */
    private void setFieldData() {
        m_ctjLanguage.setAlias(CTJ_LANGUAGE_ALIAS);
        m_ctjLanguage.setUserLongName(CTJ_LANGUAGE_NAME_LONG);
        m_ctjLanguage.setUserShortName(CTJ_LANGUAGE_NAME_SHORT);
        m_ctjLanguage.setUserType(CoreDataType.CHARACTER.getName());
        m_ctjLanguage.setReferenceTableOid(LANGUAGE_REF_TABLE);
        m_ctjLanguage.setUserLength(25);
        m_ctjLanguage.setUserDecimal(0);
        m_ctjLanguage.setDetailControl(DETAIL_CONTROL_PICKLIST);
        m_ctjLanguage.setEnabledIndicator(true);

        m_ctjInterpreter.setAlias(CTJ_INTERPRETER_ALIAS);
        m_ctjInterpreter.setUserLongName(CTJ_INTERPRETER_NAME_LONG);
        m_ctjInterpreter.setUserShortName(CTJ_INTERPRETER_NAME_SHORT);
        m_ctjInterpreter.setUserType(CoreDataType.CHARACTER.getName());
        m_ctjInterpreter.setReferenceTableOid(LANGUAGE_REF_TABLE);
        m_ctjInterpreter.setUserLength(25);
        m_ctjInterpreter.setUserDecimal(0);
        m_ctjInterpreter.setDetailControl(DETAIL_CONTROL_PICKLIST);
        m_ctjInterpreter.setEnabledIndicator(true);

        m_stdHomeSchool.setAlias(HOME_SCHOOL_ALIAS);
        m_stdHomeSchool.setUserLongName(HOME_SCHOOL_NAME);
        m_stdHomeSchool.setUserShortName(HOME_SCHOOL_NAME);
        m_stdHomeSchool.setUserType(CoreDataType.CHARACTER.getName());
        m_stdHomeSchool.setUserLength(50);
        m_stdHomeSchool.setUserDecimal(0);
        m_stdHomeSchool.setEnabledIndicator(true);

        m_stdEll.setAlias(ELL_ALIAS);
        m_stdEll.setUserLongName(ELL_NAME_LONG);
        m_stdEll.setUserShortName(ELL_NAME_SHORT);
        m_stdEll.setUserType(CoreDataType.LOGICAL.getName());
        m_stdEll.setUserLength(1);
        m_stdEll.setUserDecimal(0);
        m_stdEll.setEnabledIndicator(true);
    }
}
