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
package com.x2dev.procedures.statereporting.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
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
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class OnSISFrenchImport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class OnSISFrenchImport extends XlsImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Input file headers.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private enum Header {
        // not used
        RELEASE,
        // table name, not used
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
     * Class to process input file record.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private class Record {
        private String m_code;
        private String m_descriptionShortFr;
        private int m_lineNumber;
        private int m_referenceCodesCount;

        /**
         * Create record object from lineNumber and list of field values.
         *
         * @param record List<String>
         * @param lineNumber int
         */
        public Record(List<String> record, int lineNumber) {
            m_lineNumber = lineNumber;
            m_code = getFieldValue(record, Header.CODE);
            m_descriptionShortFr = getFieldValue(record, Header.DESCRIPTION_SHORT_FR);
        }

        /**
         * Is record matched.
         *
         * @return boolean flag
         */
        public boolean isMatched() {
            return m_referenceCodesCount > 0;
        }

        /**
         * Process a row from import file.
         */
        public void process() {
            if (!validate()) {
                return;
            }
            if (m_existingCodes.containsKey(m_code)) {
                List<ReferenceCode> codes = m_existingCodes.get(m_code);
                m_referenceCodesCount = codes.size();
                for (ReferenceCode code : codes) {
                    saveResource(code);
                }
            }
            m_log.add(String.format(LOG_MESSAGE_FORMAT, m_code, m_referenceCodesCount));
        }

        /**
         * Extract input field value.
         *
         * @param record List<String>
         * @param field Header
         * @return field value
         */
        private String getFieldValue(List<String> record, Header field) {
            return record.get(field.ordinal()).trim();
        }

        /**
         * Log processing error message.
         *
         * @param error String
         */
        private void logError(String error) {
            m_errors.add(String.format("line %d. %s", m_lineNumber, error));
        }

        /**
         * Save localization resource for created/updated reference code.
         *
         * @param refCode ReferenceCode
         */
        private void saveResource(ReferenceCode refCode) {
            if (m_commitChanges) {
                LocalizationResource resource = new LocalizationResource(LOCALE_CANADA_FRENCH,
                        LocalizationUtils.generateKey(refCode.getOid(), m_beanPath), refCode.getOid(),
                        m_descriptionShortFr);
                LocalizationCache.setLocalizedResource(getBroker(), resource);
            }
        }

        /**
         * Validate fields values.
         *
         * @return true, if all the fields are valid
         */
        private boolean validate() {
            boolean valid = false;
            if (StringUtils.isEmpty(m_code)) {
                logError("Code is empty");
            } else if (StringUtils.isEmpty(m_descriptionShortFr)) {
                logError("Short description FR is empty");
            } else {
                valid = true;
            }
            return valid;
        }
    }

    private static final int INITIAL_MAP_SIZE = 1024;
    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String LOCALE_CANADA_FRENCH = Locale.CANADA_FRENCH.toString();
    private static final String LOG_MESSAGE_FORMAT = "Code: %s, matching reference codes: %d";
    private static final String REF_TABLE_OID_PARAM = "referenceTableOid";

    /**
     * Output results.
     *
     * @param results StringBuilder
     * @param data List<String>
     */
    private static void export(StringBuilder results, List<String> data) {
        for (String error : data) {
            results.append(error);
            results.append("\n");
        }
    }

    /**
     * Detect if a parsed input string is empty.
     *
     * @param fields List<String>
     * @return true if any of fields contains data
     */
    private static boolean withData(List<String> fields) {
        boolean found = false;
        for (int i = 0; !found && i < fields.size(); i++) {
            found = !StringUtils.isEmpty(fields.get(i));
        }
        return found;
    }

    private String m_beanPath;
    private ModelBroker m_broker;
    private boolean m_commitChanges;
    private List<String> m_errors = new LinkedList<>();
    private Map<String, LinkedList<ReferenceCode>> m_existingCodes = new HashMap<>();
    private List<String> m_log = new LinkedList<>();

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
            export(results, m_errors);
        }
        results.append(m_commitChanges ? "Commit Mode." : "Review Mode.");
        results.append("\n\n");
        export(results, m_log);
        results.append("\n\n");
        results.append(getImportStatistics().toString());
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
     * Import record.
     *
     * @param fields List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> fields, int lineNumber) throws Exception {
        if (withData(fields)) {
            Record record = new Record(fields, lineNumber);
            record.process();
            if (record.isMatched()) {
                incrementMatchCount();
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
     * Find the Reference Code description bean path.
     */
    private void loadBeanPath() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty property = new ModelProperty(ReferenceCode.class, ReferenceCode.COL_DESCRIPTION, dictionary);
        m_beanPath = property.getDictionaryPath();
    }

    /**
     * Find existing reference codes.
     */
    @SuppressWarnings("unchecked")
    private void loadExistingReferenceCodes() {
        String rtbOid = (String) getParameter(REF_TABLE_OID_PARAM);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_existingCodes =
                getBroker().getGroupedCollectionByQuery(query, ReferenceCode.COL_STATE_CODE, INITIAL_MAP_SIZE);
    }
}
