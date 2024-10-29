/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.on;

import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class OnsisErrorsReport.
 *
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnsisErrorsReport extends ReportJavaSourceNet {

    /**
     * The Class ReportDataSource.
     */
    static public class ReportDataSource {
        private X2Broker m_broker;
        private Class m_clazz;
        private List<String> m_columnNames = new LinkedList();
        private List<String> m_columnPaths = new LinkedList();
        private X2Criteria m_criteria;

        /**
         * Instantiates a new report data source.
         *
         * @param broker X2Broker
         * @param clazz Class
         */
        public ReportDataSource(X2Broker broker, Class clazz) {
            m_broker = broker;
            m_clazz = clazz;
        }

        /**
         * Adds the column.
         *
         * @param columnName String
         * @param columnPath String
         */
        public void addColumn(String columnName, String columnPath) {
            m_columnNames.add(columnName);
            m_columnPaths.add(columnPath);
        }

        /**
         * Adds the criteria.
         *
         * @param criteria X2Criteria
         */
        public void addCriteria(X2Criteria criteria) {
            m_criteria = criteria;
        }

        /**
         * Populate.
         *
         * @param grid ReportDataGrid
         */
        public void populate(ReportDataGrid grid) {
            ColumnQuery query = new ColumnQuery(m_clazz, m_columnPaths.toArray(new String[0]), m_criteria);
            setOuterJoinPaths(query);
            try (QueryIterator iterator = m_broker.getReportQueryIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    Object[] row = (Object[]) iterator.next();
                    grid.append();
                    for (int index = 0; index < m_columnNames.size(); ++index) {
                        grid.set(m_columnNames.get(index), row[index]);
                    }
                }
            }
        }

        /**
         * Sets the outer join paths.
         *
         * @param query void
         */
        private void setOuterJoinPaths(ColumnQuery query) {
            Set<String> paths = new HashSet();
            m_columnPaths.stream().forEach(path -> {
                String[] values = path.split("\\.");
                if (values.length > 1) {
                    paths.add(values[0]);
                }
            });
            paths.stream().forEach(path -> query.setPathOuterJoin(path));
        }

    }

    private static final String INPUT_PARAM_FORMAT_ID_CSV = "formatCsv";
    private static final String INPUT_PARAM_FORMAT_ID_PDF = "formatPdf";
    private static final String INPUT_PARAM_RESULT_OID = "resultOid";
    private static final String PATH_UDB = UserDefinedTableA.REL_USER_DEFINED_TABLE_B + ModelProperty.PATH_DELIMITER;
    private static final String PATH_MST =
            PATH_UDB + UserDefinedTableA.REL_MASTER_SCHEDULE + ModelProperty.PATH_DELIMITER;
    private static final String PATH_SKL = PATH_UDB + UserDefinedTableA.REL_SCHOOL + ModelProperty.PATH_DELIMITER;
    private static final String PATH_STD = PATH_UDB + UserDefinedTableA.REL_STUDENT + ModelProperty.PATH_DELIMITER;
    private static final String PATH_STF = PATH_UDB + UserDefinedTableA.REL_STAFF + ModelProperty.PATH_DELIMITER;

    private Map<String, DataDictionary> m_dictionariesById = new HashMap<>();
    private Map<String, Map<String, DataDictionaryField>> m_fieldsByAlias = new HashMap<>();

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private Locale m_user_locale;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    /**
     * Gets the user message key prefix.
     *
     * @return the user message key prefix
     */
    public String getUserMessageKeyPrefix() {
        return CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".";
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        String resultOid = (String) getParameter(INPUT_PARAM_RESULT_OID);
        if (!StringUtils.isEmpty(resultOid)) {
            ReportDataSource dataSource = new ReportDataSource(getBroker(), UserDefinedTableA.class);
            dataSource.addColumn("oid", X2BaseBean.COL_OID);
            dataSource.addColumn("schoolOid", PATH_UDB + UserDefinedTableB.COL_SCHOOL_OID);
            dataSource.addColumn("school", PATH_SKL + SisSchool.COL_NAME);
            dataSource.addColumn("staffOid", PATH_UDB + UserDefinedTableB.COL_STAFF_OID);
            dataSource.addColumn("staff", PATH_STF + SisStaff.COL_NAME_VIEW);
            dataSource.addColumn("sectionOid", PATH_UDB + UserDefinedTableB.COL_MASTER_SCHEDULE_OID);
            dataSource.addColumn("section", PATH_MST + MasterSchedule.COL_COURSE_VIEW);
            dataSource.addColumn("studentOid", PATH_UDB + UserDefinedTableB.COL_STUDENT_OID);
            dataSource.addColumn("studentName", PATH_STD + SisStudent.COL_NAME_VIEW);

            dataSource.addColumn("pathToElement", PATH_UDB + translateAlias("path-to-element", "ON-SIS-ERROR"));
            dataSource.addColumn("action", PATH_UDB + translateAlias("action", "ON-SIS-ERROR"));
            dataSource.addColumn("keyFields", PATH_UDB + translateAlias("key-fields", "ON-SIS-ERROR"));
            dataSource.addColumn("keyFieldsValues", PATH_UDB + translateAlias("key-fields-values", "ON-SIS-ERROR"));
            dataSource.addColumn("description", PATH_UDB + translateAlias("description", "ON-SIS-ERROR"));
            dataSource.addColumn("status", PATH_UDB + translateAlias("status", "ON-SIS-ERROR"));
            dataSource.addColumn("fieldName", translateAlias("FIELD_NAME", "ON-SIS-ERROR"));
            dataSource.addColumn("fieldValue", translateAlias("FIELD_VALUE", "ON-SIS-ERROR"));
            dataSource.addColumn("messageCode", translateAlias("MESSAGE_CODE", "ON-SIS-ERROR"));
            if (Locale.FRANCE.equals(m_user_locale)) {
                dataSource.addColumn("englishMessage", translateAlias("F_MESSAGE", "ON-SIS-ERROR"));
            } else {
                dataSource.addColumn("englishMessage", translateAlias("E_MESSAGE", "ON-SIS-ERROR"));
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(UserDefinedTableA.REL_USER_DEFINED_TABLE_B + ModelProperty.PATH_DELIMITER
                    + UserDefinedTableB.COL_USER_DEFINED_TABLE_A_OID, resultOid);
            dataSource.addCriteria(criteria);
            dataSource.populate(grid);
        }
        grid.sort(Arrays.asList("school", "schoolOid", "staff", "staffOid", "studentName",
                "studentOid", "pathToElement", "section", "sectionOid", "englishMessage"), false);
        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        int reportFormat = getJob().getInput().getFormat();
        switch (reportFormat) {
            case ToolInput.CSV_FORMAT:
                this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_ID_CSV));
                break;
            case ToolInput.HTML_FORMAT:
                this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_ID_PDF));
                break;
            case ToolInput.PDF_FORMAT:
                this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_ID_PDF));
                break;
            case ToolInput.XLS_FORMAT:
                this.setFormatId((String) getParameter(INPUT_PARAM_FORMAT_ID_CSV));
                break;
        }

        initializeLocalized();
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        try {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), userData.getLocale());
            m_user_locale = userData.getLocale();
        } catch (Exception e) {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
            m_user_locale = Locale.US;
        }

    }

    /**
     * Gets the alias field.
     *
     * @param alias String
     * @param ddxId String
     * @return Data dictionary field
     */
    private DataDictionaryField getAliasField(String alias, String ddxId) {
        Map<String, DataDictionaryField> dictionaryFields = m_fieldsByAlias.get(ddxId);
        if (dictionaryFields == null) {
            dictionaryFields = new HashMap<>();
            m_fieldsByAlias.put(ddxId, dictionaryFields);
        }
        DataDictionaryField field = dictionaryFields.get(alias);
        if (field == null) {
            DataDictionary dictionary = getDictionary(ddxId);
            field = dictionary.findDataDictionaryFieldByAlias(alias);
            if (field == null) {
                throw new X2RuntimeException(
                        new IllegalStateException("The alias [" + alias + "] could not be found."));
            }
            if (ddxId == null) {
                dictionaryFields.put(alias, field);
            }
        }
        return field;
    }

    /**
     * Gets the dictionary.
     *
     * @param ddxId String
     * @return Data dictionary
     */
    private DataDictionary getDictionary(String ddxId) {
        DataDictionary dictionary = m_dictionariesById.get(ddxId);
        if (dictionary == null) {
            if (ddxId == null) {
                dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                m_dictionariesById.put(null, dictionary);
            } else {
                X2Criteria ddxCriteria = new X2Criteria();
                ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
                QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
                ExtendedDataDictionary ddx = getBroker().getBeanByQuery(ddxQuery);
                if (ddx == null) {
                    throw new X2RuntimeException(
                            new IllegalStateException(
                                    "The extended dictionary with id [" + ddxId + "] could not be found."));
                }
                dictionary = DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
                m_dictionariesById.put(ddxId, dictionary);
            }
        }
        return dictionary;
    }

    /**
     * Translate alias.
     *
     * @param alias String
     * @param ddxId String
     * @return String
     */
    private String translateAlias(String alias, String ddxId) {
        return getAliasField(alias, ddxId).getJavaName();
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();

        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {

                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        loc.getLocale());
                // save the messages for that language
                resources.put(loc.getLocale(), messages);

                // populate the map of valid locales
                m_validLocales.put(loc.getName(), loc.getLocale());
                if (loc.getPrimaryIndicator()) {
                    m_defaultLocale = loc.getLocale();
                }
            }
        }

        if (m_defaultLocale == null) {
            m_defaultLocale = CONST_AMERICAN_ENGLISH_LOCALE;
        }
        addParameter(PARAM_PREFIX, getUserMessageKeyPrefix());
        addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
        // Additional hooks for enhanced implementations
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
        // Comment line below if your numeric notation, currencies and others don't display as
        // expected
        // Only tested for jasper version 5
        addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_user_locale);
    }
}
