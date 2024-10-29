/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2008 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryTable;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.StringUtils;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.ResourceBundle;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Exports RSM data for off-line translation. The export includes the following fields:
 * <ol>
 * <li>Entity OID
 * <li>Locale
 * <li>RSM key
 * <li>Primary locale's value
 * <li>Translated value
 * </ol>
 *
 * @author X2 Development Corporation
 */
public class RsmExportData extends ExportJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * report parameters
     */
    public static final String PARAM_ENTITY = "entity";
    public static final String PARAM_ORG_LOCALE_OID = "olcOID";

    /**
     * entity type for Message Resources
     */
    private static final String ENTITY_RESOURCE = "RSM";

    // Grid fields
    private static final String FIELD_OID = "OID";
    private static final String FIELD_LOCALE = "Locale";
    private static final String FIELD_RSM_KEY = "RSM Key";
    private static final String FIELD_PRIMARY_VALUE = "Primary Language Value";
    private static final String FIELD_TRANSLATED_VALUE = "Translated Value";

    private List<String> m_columns;

    /**
     * Gather data.
     *
     * @return DataGrid
     * @throws Exception exception
     * @see com.x2dev.sis.tools.reports.ReportJavaSourceDori#gatherData()
     */
    @Override
    protected DataGrid gatherData() throws Exception {
        DataGrid grid = new DataGrid(m_columns.size());

        String entityType = (String) getParameter(PARAM_ENTITY);
        String olcOid = (String) getParameter(PARAM_ORG_LOCALE_OID);
        OrganizationLocale orgLocale = (OrganizationLocale) getBroker().getBeanByOid(OrganizationLocale.class, olcOid);
        String locale = orgLocale.getLocale();
        String primaryLocale = LocalizationCache.getPrimaryLocale(getBroker().getPersistenceKey()).getLocale();

        if (ENTITY_RESOURCE.equals(entityType)) {
            addResources(grid, primaryLocale, locale);
        } else {
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryTable table = dictionary.findDataDictionaryTableByPrefix(entityType);
            if (table != null) {
                addLocalizableFields(grid, table.getBeanClass(), primaryLocale, locale);
            }
        }

        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the column names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnNames()
     */
    @Override
    protected List getColumnNames() {
        return m_columns;
    }

    /**
     * Gets the column user names.
     *
     * @return List
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getColumnUserNames()
     */
    @Override
    protected List getColumnUserNames() {
        return m_columns;
    }

    /**
     * Gets the comment.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getComment()
     */
    @Override
    protected String getComment() {
        return null;
    }

    /**
     * Gets the header.
     *
     * @return String
     * @see com.x2dev.sis.tools.exports.ExportJavaSource#getHeader()
     */
    @Override
    protected String getHeader() {
        return null;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        setIncludeHeaderRow(true);
        setUseValueWrappers(false);
        setValueDelimiter(Character.valueOf('\t'));

        /*
         * Initialize the columns (since the header row is not included we can use a single list for
         * both the columnNames and columnUserNames properties).
         */
        m_columns = new ArrayList<String>(5);

        m_columns.add(FIELD_OID);
        m_columns.add(FIELD_LOCALE);
        m_columns.add(FIELD_RSM_KEY);
        m_columns.add(FIELD_PRIMARY_VALUE);
        m_columns.add(FIELD_TRANSLATED_VALUE);
    }

    /**
     * Adds the localizable fields.
     *
     * @param grid DataGrid
     * @param cls Class
     * @param primaryLocale String
     * @param locale String
     */
    private void addLocalizableFields(DataGrid grid, Class cls, String primaryLocale, String locale) {
        PersistenceKey persistenceKey = getBroker().getPersistenceKey();
        QueryByCriteria query = new QueryByCriteria(cls, null);
        QueryIterator iterator = getBroker().getIteratorByQuery(query);
        try {
            List<DataDictionaryField> fields = null;
            while (iterator.hasNext()) {
                X2BaseBean bean = (X2BaseBean) iterator.next();
                if (fields == null) {
                    fields = LocalizationUtils.getLocalizableFields(persistenceKey, bean);
                }
                if (fields != null && fields.size() > 0) {
                    for (DataDictionaryField field : fields) {
                        String key = LocalizationUtils.generateKey(bean.getOid(), field.getId());
                        String primaryValue =
                                LocalizationCache.getMessage(persistenceKey.getDeploymentId(), primaryLocale, key);
                        if (StringUtils.isEmpty(primaryValue)) {
                            try {
                                primaryValue = (String) PropertyUtils.getProperty(bean, field.getJavaName());
                            } catch (Exception ex) {
                                // TODO: Handle or comment
                            }
                        }
                        String translatedValue =
                                LocalizationCache.getMessage(persistenceKey.getDeploymentId(), locale, key);
                        addToGrid(grid, bean.getOid(), locale, key, primaryValue, translatedValue);
                    }
                }
            }
        } finally {
            iterator.close();
        }
    }

    /**
     * Adds the resources.
     *
     * @param grid DataGrid
     * @param primaryLocale String
     * @param localeString String
     */
    private void addResources(DataGrid grid, String primaryLocale, String localeString) {
        MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(), localeString, true);

        ResourceBundle resourceBundle = AppGlobals.getMessagesBundle(primaryLocale);
        Enumeration<String> bundleEnumeration = resourceBundle.getKeys();

        while (bundleEnumeration.hasMoreElements()) {
            String resourceKey = bundleEnumeration.nextElement();
            String primaryValue = resourceBundle.getString(resourceKey);
            String translatedValue = "";
            try {
                translatedValue = messages.getMessage(resourceKey);
            } catch (Exception ex) {
                // TODO: Handle or comment.
            }
            addToGrid(grid, null/* object id always NULL due to criteria */, localeString, resourceKey,
                    primaryValue, translatedValue);
        }
    }

    /**
     * Appends a row to the grid and sets the columns based on values from the attendance record.
     *
     * @param grid DataGrid
     * @param oid String
     * @param locale String
     * @param key String
     * @param primaryValue String
     * @param translatedValue String
     */
    private void addToGrid(DataGrid grid,
                           String oid,
                           String locale,
                           String key,
                           String primaryValue,
                           String translatedValue) {
        grid.append();
        grid.set(m_columns.get(0), oid);
        grid.set(m_columns.get(1), locale);
        grid.set(m_columns.get(2), key);
        grid.set(m_columns.get(3), primaryValue);
        grid.set(m_columns.get(4), translatedValue);
    }
}
