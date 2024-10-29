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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SystemPreference;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.utils.StringUtils;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Allows school level preferences to be set for multiple schools
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class SystemPreferenceTool extends ProcedureJavaSource {
    private Map<String, SystemPreference> m_existingPreferences;
    private StringBuilder m_message;
    private String m_preferenceKey;
    private SystemPreferenceDefinition m_preferenceKeyInfo;
    private String m_preferenceValue;
    private Map<String, String> m_schoolNames;
    private Collection<String> m_selectedSchoolOids;

    private static final String MULTI_LINE_VALUE = "multiLineValue";
    private static final String PREFERENCE_KEY = "preferenceKey";
    private static final String PREFERENCE_VALUE = "preferenceValue";
    private static final String PREFERENCE_VALUE_MUTLI_LINE = "preferenceValueMutliLine";
    private static final String SCHOOL_OIDS = "schoolOids";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        setPreferenceValues();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_message = new StringBuilder();

        loadSchoolNames();
        loadParameters();
        loadExistingPreferences();
    }

    /**
     * Loads the existing preferences to memory
     */
    private void loadExistingPreferences() {
        m_existingPreferences = new HashMap<String, SystemPreference>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(SystemPreference.COL_OWNER_TYPE, Integer.valueOf(Ownable.OWNER_TYPE_SCHOOL));
        criteria.addEqualTo(
                SystemPreference.REL_PREFERENCE_DEFINITION + PATH_DELIMITER + X2BaseBean.COL_OID,
                m_preferenceKey);
        criteria.addIn(SystemPreference.COL_OWNER_OID, m_selectedSchoolOids);
        BeanQuery query = new BeanQuery(SystemPreference.class, criteria);

        try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                SystemPreference preference = (SystemPreference) iterator.next();

                String ownerOid = preference.getOwnerOid();

                m_existingPreferences.put(ownerOid, preference);
            }
        }
    }

    /**
     * Loads the input definition parameters to memory
     */
    private void loadParameters() {
        m_preferenceKey = (String) getParameter(PREFERENCE_KEY);
        m_preferenceValue = getPreferenceValue();
        m_selectedSchoolOids = StringUtils.convertDelimitedStringToList((String) getParameter(SCHOOL_OIDS), ",");
        m_preferenceKeyInfo = (SystemPreferenceDefinition) getBroker().getBeanByOid(SystemPreferenceDefinition.class,
                m_preferenceKey);
    }

    /**
     * Returns the correct preference (single line or multi-line) based upon user's selection
     *
     * @return String The preference value
     */
    private String getPreferenceValue() {
        boolean multiLineValue = ((Boolean) getParameter(MULTI_LINE_VALUE)).booleanValue();
        String preferenceValueMultiLine = (String) getParameter(PREFERENCE_VALUE_MUTLI_LINE);
        String preverenceValueSingleLine = (String) getParameter(PREFERENCE_VALUE);

        return multiLineValue ? preferenceValueMultiLine : preverenceValueSingleLine;
    }

    /**
     * Loads the school names to memory.
     */
    private void loadSchoolNames() {
        m_schoolNames = new HashMap<String, String>();
        X2Criteria criteria = new X2Criteria();
        String[] columns = {X2BaseBean.COL_OID, SisSchool.COL_NAME};

        ColumnQuery query = new ColumnQuery(SisSchool.class, columns, criteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query)) {
            while (iterator.hasNext()) {
                Object[] rows = (Object[]) iterator.next();

                String schoolOid = (String) rows[0];
                String schoolName = (String) rows[1];
                m_schoolNames.put(schoolOid, schoolName);
            }
        }
    }

    /**
     * Saves the change to the system preference and displays and logs an error message if there are
     * any issues
     *
     * @param preference The SystemPreference
     * @param schoolOid The school oid
     */
    private void savePreference(SystemPreference preference, String schoolOid) {

        List<ValidationError> errors = getBroker().saveBean(preference);

        if (!errors.isEmpty()) {
            StringBuilder errorMessage = new StringBuilder();
            errorMessage.append("Error saving value for school : ");
            errorMessage.append(m_schoolNames.get(schoolOid));
            errorMessage.append(" value: ");
            errorMessage.append(m_preferenceValue);
            errorMessage.append(" error message(s):");

            for (ValidationError error : errors) {
                errorMessage.append(" ");
                errorMessage.append(error.toString());
            }
        } else {
            String category = m_preferenceKeyInfo.getCategory();
            String description = m_preferenceKeyInfo.getDefaultValue();
            String key = m_preferenceKeyInfo.getKey();
            String name = m_preferenceKeyInfo.getName();

            m_message.append("Preference value of " + m_preferenceValue + " set for Category: ");
            m_message.append(category);
            m_message.append(" Key: ");
            m_message.append(key);
            m_message.append(" Name: ");
            m_message.append(name);
            m_message.append(" Description: ");
            m_message.append(description != null ? description : "");
            m_message.append(" for School: ");
            m_message.append(m_schoolNames.get(schoolOid));
            m_message.append(System.getProperty("line.separator"));
        }
    }

    /**
     * Sets the preference values
     */
    private void setPreferenceValues() {

        for (String schoolOid : m_selectedSchoolOids) {
            SystemPreference existingPreference = m_existingPreferences.get(schoolOid);

            if (existingPreference != null) {
                if (!existingPreference.getValue().equals(m_preferenceValue)) {
                    existingPreference.setValue(m_preferenceValue);

                    savePreference(existingPreference, schoolOid);
                } else {

                    m_message.append("No changes made for school: ");
                    m_message.append(m_schoolNames.get(schoolOid));
                    m_message.append(System.getProperty("line.separator"));
                }
            } else {
                SystemPreference newPreference =
                        X2BaseBean.newInstance(SystemPreference.class, getBroker().getPersistenceKey());
                newPreference.setPreferenceDefinitionOid(m_preferenceKey);
                newPreference.setValue(m_preferenceValue);
                newPreference.setOwnerType(Ownable.OWNER_TYPE_SCHOOL);
                newPreference.setOwnerOid(schoolOid);
                newPreference.setPersistenceKey(getBroker().getPersistenceKey());

                savePreference(newPreference, schoolOid);
            }
        }

        logMessage(m_message.toString());

        // Clears the object cache so that the user isn't confused by old preference values after an
        // update
        ModelBroker modelBroker = new ModelBroker(getPrivilegeSet());
        modelBroker.clearCache();
    }
}
