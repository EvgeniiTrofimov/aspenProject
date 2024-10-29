/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.text.WordUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * @author Follett Software Company
 * @copyright 2019
 */
public class RcdDescriptionImport extends XlsImportJavaSource {

    /**
     * Input file headers
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private enum Header {
        // not used
        RELEASE,
        // table name
        TABLE,
        // code
        CODE,
        // not used
        PARENT,
        // short code description in English
        DESCRIPTION_SHORT_EN,
        // long code description in English
        DESCRIPTION_LONG_EN,
        // short code description in French
        DESCRIPTION_SHORT_FR,
        // long code description in French
        DESCRIPTION_LONG_FR;
    }

    /**
     * Class to process input file record
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private class Record {
        private String m_code;
        private String m_descriptionShortEn;
        private String m_descriptionShortFr;
        private int m_lineNumber;
        private Result m_result;
        private String m_table;

        /**
         * Create record object from lineNumber and list of field values
         *
         * @param record
         * @param lineNumber
         */
        public Record(List<String> record, int lineNumber) {
            m_lineNumber = lineNumber;
            m_result = Result.SKIPPED;

            m_code = getFieldValue(record, Header.CODE);
            m_descriptionShortEn = getFieldValue(record, Header.DESCRIPTION_SHORT_EN);
            m_descriptionShortFr = getFieldValue(record, Header.DESCRIPTION_SHORT_FR);
            m_table = parseTableName(getFieldValue(record, Header.TABLE));
        }

        /**
         * Is new record inserted
         *
         * @return boolean flag
         */
        public boolean isInserted() {
            return Result.INSERTED == m_result;
        }

        /**
         * Is record processing skipped
         *
         * @return boolean flag
         */
        public boolean isSkipped() {
            return Result.SKIPPED == m_result;
        }

        /**
         * Is existing record updated
         *
         * @return boolean flag
         */
        public boolean isUpdated() {
            return Result.UPDATED == m_result;
        }

        /**
         * Process a row from import file
         */
        public void process() {
            m_result = Result.SKIPPED;
            if (!validate()) {
                return;
            }
            X2BaseBean refTbl = findReferenceTable();
            if (refTbl == null) {
                return;
            }
            Map<String, LinkedList<ReferenceCode>> codes = m_existingCodes.get(refTbl.getOid());
            ReferenceCode code = null;
            if (codes != null && codes.containsKey(m_code)) {
                code = codes.get(m_code).getFirst();
            }
            if (code != null) {
                if (updateCode(code)) {
                    m_result = Result.UPDATED;
                }
            } else {
                if (insertCode(refTbl)) {
                    m_result = Result.INSERTED;
                }
            }
        }

        /**
         * Create new reference table
         *
         * @return reference table
         */
        private ReferenceTable createReferenceTableByName() {
            ReferenceTable refTbl = X2BaseBean.newInstance(ReferenceTable.class, getBroker().getPersistenceKey());
            refTbl.setCategory(TABLE_CATEGORY);
            refTbl.setDataTableOid(ReferenceCode.DICTIONARY_ID);
            refTbl.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            refTbl.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            refTbl.setUserName(m_table);
            refTbl.setCodeLength(CODE_LENGTH);
            return saveBean(refTbl) ? refTbl : null;
        }

        /**
         * Find or create reference table
         *
         * @return reference table
         */
        private ReferenceTable findReferenceTable() {
            if (!m_referenceTableByName.containsKey(m_table)) {
                m_referenceTableByName.put(m_table, createReferenceTableByName());
            }
            return m_referenceTableByName.get(m_table);
        }

        /**
         * Extract input field value
         *
         * @param record
         * @param field
         * @return field value
         */
        private String getFieldValue(List<String> record, Header field) {
            return record.get(field.ordinal()).trim();
        }

        /**
         * Insert new reference code
         *
         * @param reference table bean
         * @return success flag
         */
        private boolean insertCode(X2BaseBean refTbl) {
            ReferenceCode refCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            refCode.setReferenceTableOid(refTbl.getOid());
            refCode.setCode(m_code);
            refCode.setStateCode(m_code);
            refCode.setDescription(m_descriptionShortEn);
            refCode.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            refCode.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            return saveBean(refCode) && saveResource(refCode);
        }

        /**
         * Log processing error message
         *
         * @param error
         */
        private void logError(String error) {
            m_errors.add(String.format("line %d. %s", m_lineNumber, error));
        }

        /**
         * Prepare table name string
         *
         * @param table
         * @return table name
         */
        private String parseTableName(String table) {
            String name = table.replace(CHAR_UNDERSCORE, CHAR_SPACE);
            return TABLE_NAME_PREFIX + WordUtils.capitalizeFully(name);
        }

        /**
         * Save bean.
         *
         * @param bean X2BaseBean
         * @return true, if successful
         */
        private boolean saveBean(X2BaseBean bean) {
            boolean ok = true;
            if (m_commitChanges) {
                List<ValidationError> errors = getBroker().saveBean(bean);
                if (errors != null && !errors.isEmpty()) {
                    for (ValidationError error : errors) {
                        logError("Validation error: " + error.toString());
                    }
                    ok = false;
                }
            }
            return ok;
        }

        /**
         * Save localization resource for created/updated reference code
         *
         * @param refCode
         * @return success flag
         */
        private boolean saveResource(ReferenceCode refCode) {
            if (m_commitChanges) {
                LocalizationResource resource = new LocalizationResource(LOCALE_CANADA_FRENCH,
                        LocalizationUtils.generateKey(refCode.getOid(), m_beanPath), refCode.getOid(),
                        m_descriptionShortFr);
                LocalizationCache.setLocalizedResource(getBroker(), resource);
            }
            return true;
        }

        /**
         * Update existing reference code
         *
         * @param Reference Code bean
         * @return success flag
         */
        private boolean updateCode(ReferenceCode refCode) {
            refCode.setCode(m_code);
            refCode.setDescription(m_descriptionShortEn);
            if (refCode.isDirty()) {
                return saveBean(refCode) && saveResource(refCode);
            }
            return saveResource(refCode);
        }

        /**
         * Validate fields values
         *
         * @return true, if all the fields are valid
         */
        private boolean validate() {
            boolean valid = false;
            if (StringUtils.isEmpty(m_table)) {
                logError("Table name is empty");
            } else if (StringUtils.isEmpty(m_code)) {
                logError("Code is empty");
            } else if (StringUtils.isEmpty(m_descriptionShortEn)) {
                logError("Short description EN is empty");
            } else if (m_descriptionShortEn.length() > MAX_CODE_DESCRIPTION_LENGTH) {
                logError("Short description EN is too long, maximum length is " + MAX_CODE_DESCRIPTION_LENGTH);
            } else if (StringUtils.isEmpty(m_descriptionShortFr)) {
                logError("Short description FR is empty");
            } else {
                valid = true;
            }
            return valid;
        }
    }

    /**
     * Import results
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private enum Result {
        // new row inserted
        INSERTED,
        // row processing skipped due to some error
        SKIPPED,
        // existing row updated
        UPDATED
    }

    private static final char CHAR_SPACE = ' ';
    private static final char CHAR_UNDERSCORE = '_';
    private static final int CODE_LENGTH = 25;

    private static final int INITIAL_MAP_SIZE = 1024;

    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String LOCALE_CANADA_FRENCH = Locale.CANADA_FRENCH.toString();
    private static final int MAX_CODE_DESCRIPTION_LENGTH = 100;
    private static final String TABLE_CATEGORY = "System";
    private static final String TABLE_NAME_PREFIX = "OnSIS ";

    private String m_beanPath;
    private ModelBroker m_broker;
    private boolean m_commitChanges;
    private List<String> m_errors = new LinkedList<>();
    private Map<String, Map<String, LinkedList<ReferenceCode>>> m_existingCodes = new HashMap<>();
    private Map<String, ReferenceTable> m_referenceTableByName = new HashMap<>();

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.x2dev.sis.tools.ToolJavaSource#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Display only errors, if any, and exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder results = new StringBuilder();
        if (!m_errors.isEmpty()) {
            exportErrors(results);
        } else {
            results.append(m_commitChanges ? "Commit Mode." : "Review Mode.");
            results.append("\n\n");
            results.append(getImportStatistics().toString());
        }
        try {
            export(results.toString());
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return Header.values().length;
    }


    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> fields, int lineNumber) throws Exception {
        if (withData(fields)) {
            Record record = new Record(fields, lineNumber);
            record.process();
            if (record.isUpdated()) {
                incrementUpdateCount();
            } else if (record.isInserted()) {
                incrementInsertCount();
            } else {
                incrementSkipCount();
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
        Boolean commit = (Boolean) getParameter(INPUT_PARAM_COMMIT);
        m_commitChanges = commit == null || commit.booleanValue();

        loadExistingReferenceTables();
        loadExistingReferenceCodes();
        loadBeanPath();
    }

    /**
     * Export results string.
     *
     * @param str String
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void export(String str) throws IOException {
        ByteArrayInputStream inputStream = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8));
        try {
            StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
        } finally {
            inputStream.close();
        }
    }

    /**
     * Output errors to the results string
     *
     * @param results
     */
    private void exportErrors(StringBuilder results) {
        for (String error : m_errors) {
            results.append(error);
            results.append("\n");
        }
    }

    /**
     * Find the Reference Code description bean path
     */
    private void loadBeanPath() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty property = new ModelProperty(ReferenceCode.class, ReferenceCode.COL_DESCRIPTION, dictionary);
        m_beanPath = property.getDictionaryPath();
    }

    /**
     * Find existing reference codes
     */
    private void loadExistingReferenceCodes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addBeginsWith(
                ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                TABLE_NAME_PREFIX);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_existingCodes =
                getBroker().getGroupedCollectionByQuery(query,
                        new String[] {ReferenceCode.COL_REFERENCE_TABLE_OID, ReferenceCode.COL_CODE},
                        new int[] {INITIAL_MAP_SIZE, INITIAL_MAP_SIZE});
    }

    /**
     * Find existing reference tables
     */
    private void loadExistingReferenceTables() {
        X2Criteria criteria = new X2Criteria();
        criteria.addBeginsWith(ReferenceTable.COL_USER_NAME, TABLE_NAME_PREFIX);
        QueryByCriteria query = new QueryByCriteria(ReferenceTable.class, criteria);
        m_referenceTableByName =
                getBroker().getMapByQuery(query, ReferenceTable.COL_USER_NAME, INITIAL_MAP_SIZE);
    }

    /**
     * Detect if a parsed input string is empty
     *
     * @param fields
     * @return true if any of fields contains data
     */
    private boolean withData(List<String> fields) {
        boolean found = false;
        for (int i = 0; !found && i < fields.size(); i++) {
            found = !StringUtils.isEmpty(fields.get(i));
        }
        return found;
    }
}
