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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;
import org.apache.poi.ss.usermodel.DateUtil;

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
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;

/**
 * The Class RcdDescriptionImport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class RcdDescriptionImport extends ImportJavaSource {
    private static final long serialVersionUID = 1L;

    /**
     * Input file headers.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private enum Header {
        // table name
        TABLE,
        // code
        CODE,
        // short code description in English
        DESCRIPTION_SHORT_EN,
        // short code description in French
        DESCRIPTION_SHORT_FR,
        // long code description in English
        DESCRIPTION_LONG_EN,
        // long code description in French
        DESCRIPTION_LONG_FR,
        // long code description in French
        OPEN_DATE,
        // long code description in French
        CLOSE_DATE;
    }

    /**
     * Class to process input file record.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private class Record {
        private String m_code;
        private String m_descriptionShortEn;
        private String m_descriptionShortFr;
        private PlainDate m_endDate;
        private int m_lineNumber;
        private Result m_result;
        private PlainDate m_startDate;
        private String m_table;

        /**
         * Create record object from lineNumber and list of field values.
         *
         * @param record the record
         * @param lineNumber the line number
         */
        public Record(List<Object> record, int lineNumber) {
            m_lineNumber = lineNumber;
            m_result = Result.SKIPPED;

            try {
                m_code = getFieldValueString(record, Header.CODE);
                m_descriptionShortEn = getFieldValueString(record, Header.DESCRIPTION_SHORT_EN);
                if (m_descriptionShortEn.length() > MAX_CODE_DESCRIPTION_LENGTH) {
                    m_descriptionShortEn = m_descriptionShortEn.substring(0, MAX_CODE_DESCRIPTION_LENGTH);
                }
                m_descriptionShortFr = getFieldValueString(record, Header.DESCRIPTION_SHORT_FR);
                if (m_descriptionShortFr.length() > MAX_CODE_DESCRIPTION_LENGTH) {
                    m_descriptionShortFr = m_descriptionShortFr.substring(0, MAX_CODE_DESCRIPTION_LENGTH);
                }
                m_startDate = convertDate(getFieldValue(record, Header.OPEN_DATE));
                m_endDate = convertDate(getFieldValue(record, Header.CLOSE_DATE));
                m_table = getFieldValueString(record, Header.TABLE);
            } catch (Exception e) {
                throw new IllegalStateException(
                        "Conversion error for line number " + lineNumber + " with date " + record.toString(), e);
            }
        }

        /**
         * Is new record inserted.
         *
         * @return boolean flag
         */
        public boolean isInserted() {
            return Result.INSERTED == m_result;
        }

        /**
         * Is new record matched.
         *
         * @return boolean flag
         */
        public boolean isMatched() {
            return Result.MATCHED == m_result;
        }

        /**
         * Is existing record updated.
         *
         * @return boolean flag
         */
        public boolean isUpdated() {
            return Result.UPDATED == m_result;
        }

        /**
         * Process a row from import file.
         */
        public void process() {
            String rtbOid = m_referenceTableByName.get(m_table);
            if (!StringUtils.isEmpty(rtbOid)) {
                if (!validate()) {
                    return;
                }
                Map<String, LinkedList<ReferenceCode>> codes = m_existingCodes.get(rtbOid);
                LinkedList<ReferenceCode> code = null;
                if (codes != null && codes.containsKey(m_code)) {
                    code = codes.get(m_code);
                }
                if (code != null) {
                    m_result = Result.MATCHED;
                    if (updateCode(code)) {
                        m_result = Result.UPDATED;
                    }
                } else {
                    if (insertCode(rtbOid)) {
                        m_result = Result.INSERTED;
                    }
                }
            } else {
                addTableMissing(m_table);
            }
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return m_table + " code " + m_code;
        }

        /**
         * Convert date.
         *
         * @param object the object
         * @return the plain date
         */
        private PlainDate convertDate(Object object) {
            return object instanceof PlainDate ? (PlainDate) object : null;
        }

        /**
         * Extract input field value.
         *
         * @param record the record
         * @param field the field
         * @return field value
         */
        private Object getFieldValue(List<Object> record, Header field) {
            Object item = record.get(field.ordinal());
            return item instanceof String ? ((String) item).trim() : item;
        }

        /**
         * Extract input field value.
         *
         * @param record the record
         * @param field the field
         * @return field value
         */
        private String getFieldValueString(List<Object> record, Header field) {
            Object item = record.get(field.ordinal());
            return item instanceof String ? ((String) item).trim() : (item == null ? "" : item.toString());
        }

        /**
         * Insert new reference code.
         *
         * @param rtbOid the rtb oid
         * @return success flag
         */
        private boolean insertCode(String rtbOid) {
            ReferenceCode refCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            refCode.setReferenceTableOid(rtbOid);
            refCode.setCode(m_code);
            if (m_startDate != null) {
                refCode.setFieldValueByAlias(ALIAS_START_DATE, getDateString(m_startDate));
            }
            if (m_endDate != null) {
                refCode.setFieldValueByAlias(ALIAS_END_DATE, getDateString(m_endDate));
            }
            refCode.setFieldValueByAlias(ALIAS_MINISTRY_TABLE_NAME, m_table);
            refCode.setStateCode(m_code);
            refCode.setDescription(m_descriptionShortEn);
            refCode.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            refCode.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            logMessage("Add - " + this);
            return saveBean(refCode) && saveResource(refCode);
        }

        /**
         * Log processing error message.
         *
         * @param error the error
         */
        private void logError(String error) {
            m_errors.add(String.format("error - line %d. %s", m_lineNumber, error));
        }

        /**
         * Log processing error message.
         *
         * @param message the message
         */
        private void logMessage(String message) {
            m_messages.add(String.format("info - line %d. %s", m_lineNumber, message));
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
         * Save localization resource for created/updated reference code.
         *
         * @param refCode the ref code
         * @return success flag
         */
        private boolean saveResource(ReferenceCode refCode) {
            if (m_commitChanges) {
                if (!StringUtils.isBlank(m_descriptionShortFr)) {
                    LocalizationMessageResources messages =
                            LocalizationCache.getMessages(getBroker().getPersistenceKey(), LOCALE_FRENCH);
                    String generateKey = LocalizationUtils.generateKey(refCode.getOid(), m_beanPath);
                    String message = messages.getMessage(generateKey);
                    if (message.equals(m_descriptionShortFr)) {
                        return true;
                    }
                    LocalizationResource resource = new LocalizationResource(LOCALE_FRENCH,
                            generateKey, refCode.getOid(), m_descriptionShortFr);
                    LocalizationCache.setLocalizedResource(getBroker(), resource);
                }

                if (!StringUtils.isBlank(m_descriptionShortEn)) {
                    LocalizationMessageResources messages =
                            LocalizationCache.getMessages(getBroker().getPersistenceKey(), LOCALE_CANADA);
                    String generateKey = LocalizationUtils.generateKey(refCode.getOid(), m_beanPath);
                    String message = messages.getMessage(generateKey);
                    if (message.equals(m_descriptionShortEn)) {
                        return true;
                    }
                    LocalizationResource resource = new LocalizationResource(LOCALE_CANADA,
                            generateKey, refCode.getOid(), m_descriptionShortEn);
                    LocalizationCache.setLocalizedResource(getBroker(), resource);
                }

            }
            return true;
        }

        /**
         * Update existing reference code.
         *
         * @param code the ref code
         * @return success flag
         */
        private boolean updateCode(List<ReferenceCode> codes) {
            boolean returnValue = false;

            for (ReferenceCode code : codes) {
                PlainDate currentStartDate = getDateValue(code.getFieldValueByAlias(ALIAS_START_DATE));
                PlainDate currentEndDate = getDateValue(code.getFieldValueByAlias(ALIAS_END_DATE));

                boolean isDirty = false;
                if (currentStartDate == null) {
                    if (m_startDate != null) {
                        code.setFieldValueByAlias(ALIAS_START_DATE, getDateString(m_startDate));
                        isDirty = true;
                    }
                } else if (m_startDate == null || !m_startDate.equals(currentStartDate)) {
                    code.setFieldValueByAlias(ALIAS_START_DATE, getDateString(m_startDate));
                    isDirty = true;
                }

                if (currentEndDate == null) {
                    if (m_endDate != null) {
                        code.setFieldValueByAlias(ALIAS_END_DATE, getDateString(m_endDate));
                        isDirty = true;
                    }
                } else if (m_endDate == null || !m_endDate.equals(currentEndDate)) {
                    code.setFieldValueByAlias(ALIAS_END_DATE, getDateString(m_endDate));
                    isDirty = true;
                }


                String currentTable = (String) code.getFieldValueByAlias(ALIAS_MINISTRY_TABLE_NAME);
                if (currentTable == null || !m_table.equals(currentTable)) {
                    code.setFieldValueByAlias(ALIAS_MINISTRY_TABLE_NAME, m_table);
                    isDirty = true;
                }

                if (isDirty) {
                    returnValue = true;
                    logMessage("Update reference code: " + code.getCode() + " - " + this);
                    saveBean(code);
                }
            }
            return returnValue;
        }

        /**
         * Validate fields values.
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
            } else if (StringUtils.isEmpty(m_descriptionShortFr)) {
                logError("Short description FR is empty");
            } else {
                valid = true;
            }
            return valid;
        }
    }

    /**
     * Import results.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private enum Result {
        // new row inserted
        INSERTED,
        // row processing matched
        MATCHED,
        // row processing skipped due to some error
        SKIPPED,
        // existing row updated
        UPDATED
    }

    private static final String ALIAS_END_DATE = "all-rcd-EndDate";
    private static final String ALIAS_MINISTRY_TABLE_NAME = "all-rcd-MinistryTableName";
    private static final String ALIAS_START_DATE = "all-rcd-StartDate";

    private static final int INITIAL_MAP_SIZE = 1024;

    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String LOCALE_CANADA = Locale.CANADA.toString();
    private static final String LOCALE_FRENCH = Locale.FRANCE.toString();
    private static final int MAX_CODE_DESCRIPTION_LENGTH = 100;
    private static final String REFERENCE_CODE_TABLE_OID = "temp";

    private String m_beanPath;
    private ModelBroker m_broker;
    private boolean m_commitChanges;
    private DateAsStringConverter m_converter =
            (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                    Locale.getDefault(), true);
    private DictionaryExtractor m_dictExtractor;
    private Map<String, Set<String>> m_duplicateReferenceTableOids = new HashMap();
    private Map<String, Set<String>> m_duplicateMinistryTables = new HashMap();
    private List<String> m_errors = new LinkedList<>();
    private Map<String, Map<String, LinkedList<ReferenceCode>>> m_existingCodes = new HashMap<>();
    private List<String> m_messages = new LinkedList<>();
    private Set<String> m_missingTables = new TreeSet();
    private Map<String, String> m_referenceTableByName = new HashMap<>();
    private Map<String, String> m_referenceTableByOid = new HashMap<>();

    /**
     * Adds the table missing.
     *
     * @param tableName the table name
     */
    public void addTableMissing(String tableName) {
        m_missingTables.add(tableName);
    }

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
     * Gets the date string.
     *
     * @param date the date
     * @return the date string
     */
    public String getDateString(PlainDate date) {
        String value = "";
        if (date != null) {
            value = m_converter.getSystemString(date);
        }
        return value;
    }

    /**
     * Gets the date value.
     *
     * @param input the input
     * @return the date value
     */
    public PlainDate getDateValue(Object input) {
        PlainDate result = null;
        if (input != null && input instanceof String) {
            result = (PlainDate) m_converter.parseSystemString((String) input);
        }
        return result;
    }

    /**
     * Gets the dictionary extractor.
     *
     * @return Dictionary extractor
     */
    public DictionaryExtractor getDictionaryExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
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
        if (!m_duplicateReferenceTableOids.isEmpty()) {
            results.append("Reference tables with multiple ministry tables: \n");
            results.append(m_duplicateReferenceTableOids.entrySet().stream()
                    .map(entry -> {
                        StringBuilder result = new StringBuilder();
                        String rtbOid = entry.getKey();
                        ReferenceTable rtb = getBroker().getBeanByOid(ReferenceTable.class, rtbOid);
                        result.append(rtb == null ? rtbOid : rtb.getUserName());
                        result.append(" {");
                        result.append(entry.getValue().stream().limit(10).collect(Collectors.joining(",")));
                        result.append("}");
                        return result.toString();
                    })
                    .collect(Collectors.joining("\n")));
            results.append("\n\n");
        }
        if (!m_duplicateMinistryTables.isEmpty()) {
            results.append("Ministry tables contained in multiple reference tables: \n");
            results.append(m_duplicateMinistryTables.entrySet().stream()
                    .map(entry -> {
                        StringBuilder result = new StringBuilder();
                        result.append(entry.getKey());
                        result.append(" {");
                        result.append(entry.getValue().stream().limit(10).map(rtbOid -> {
                            ReferenceTable rtb = getBroker().getBeanByOid(ReferenceTable.class, rtbOid);
                            return rtb == null ? rtbOid : rtb.getUserName();
                        }).collect(Collectors.joining(",")));
                        result.append("}");
                        return "";
                    })
                    .collect(Collectors.joining("\n")));
            results.append("\n\n");
        }
        if (!m_missingTables.isEmpty()) {
            results.append("Missing tables: \n");
            results.append(m_missingTables.stream().collect(Collectors.joining("\n")));
            results.append("\n\n");
        }
        if (!m_errors.isEmpty()) {
            exportList(results, m_errors);
        }
        if (!m_messages.isEmpty()) {
            exportList(results, m_messages);
        }
        results.append(m_commitChanges ? "Commit Mode." : "Review Mode.");
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
    protected int getFieldCount() {
        return Header.values().length;
    }


    /**
     * Import record.
     *
     * @param fields the fields
     * @param lineNumber the line number
     * @throws Exception the exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    protected void importRecord(List<Object> fields, int lineNumber) throws Exception {
        if (lineNumber > 1 && withData(fields)) {
            Record record = new Record(fields, lineNumber);
            record.process();
            if (record.isUpdated()) {
                incrementMatchCount();
                incrementUpdateCount();
            } else if (record.isInserted()) {
                incrementInsertCount();
            } else if (record.isMatched()) {
                incrementMatchCount();
            } else {
                incrementSkipCount();
            }
        }
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        InputStream inputStream = null;

        try {
            inputStream = new FileInputStream(sourceFile.getPath());
        } catch (FileNotFoundException fnfe) {
            logInvalidRecord(0, "error.file.edit.notFound", sourceFile.getAbsolutePath());
        }

        if (inputStream != null) {
            try {
                POIFSFileSystem fileSystem = new POIFSFileSystem(inputStream);
                HSSFWorkbook workbook = new HSSFWorkbook(fileSystem);
                HSSFSheet sheet = workbook.getSheetAt(0);

                int lineNumber = 1;
                while (lineNumber <= sheet.getLastRowNum()) {
                    List<Object> record = new ArrayList(getFieldCount());

                    HSSFRow row = sheet.getRow(lineNumber);
                    for (short i = 0; i < getFieldCount(); i++) {
                        HSSFCell cell = row.getCell(i);
                        if (cell != null) {
                            switch (cell.getCellType()) {
                                case NUMERIC:
                                    if (DateUtil.isCellDateFormatted(cell)) {
                                        Date date = cell.getDateCellValue();
                                        record.add(date == null ? null : new PlainDate(date));
                                    } else {
                                        String value = new BigDecimal(cell.getNumericCellValue()).toPlainString();
                                        record.add(value);
                                    }
                                    break;

                                case STRING:
                                    record.add(cell.getRichStringCellValue().getString());
                                    break;

                                default:
                                    record.add("");
                                    break;
                            }
                        } else {
                            record.add("");
                        }
                    }

                    importRecord(record, lineNumber);
                    lineNumber++;
                }
            } finally {
                inputStream.close();
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

        DataDictionaryField field = getDictionaryExtractor().getFieldByAlias(ALIAS_MINISTRY_TABLE_NAME);
        if (field == null) {
            throw new IllegalStateException("Alias " + ALIAS_MINISTRY_TABLE_NAME + " is not found");
        }
        if (!SisBeanPaths.REF_CODE.getDictionaryID().equals(field.getDataTableOid().trim())) {
            throw new IllegalStateException("Alias " + ALIAS_MINISTRY_TABLE_NAME + " found on table "
                    + field.getDataTableOid() + ".  It should be on " + REFERENCE_CODE_TABLE_OID);
        }

        loadExistingReferenceTables(field.getJavaName());
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
     * Output errors to the results string.
     *
     * @param results the results
     * @param items the items
     */
    private void exportList(StringBuilder results, List<String> items) {
        for (String item : items) {
            results.append(item);
            results.append("\n");
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
    private void loadExistingReferenceCodes() {
        Set<String> rtbOids = new HashSet(m_referenceTableByOid.keySet());
        rtbOids.removeAll(m_duplicateReferenceTableOids.keySet());
        m_duplicateMinistryTables.values().stream().forEach(set -> rtbOids.removeAll(set));

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(ReferenceCode.COL_REFERENCE_TABLE_OID, rtbOids);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_existingCodes =
                getBroker().getGroupedCollectionByQuery(query,
                        new String[] {ReferenceCode.COL_REFERENCE_TABLE_OID, ReferenceCode.COL_STATE_CODE},
                        new int[] {INITIAL_MAP_SIZE, INITIAL_MAP_SIZE});
    }

    /**
     * Find existing reference tables.
     *
     * @param beanPath the bean path
     */
    private void loadExistingReferenceTables(String beanPath) {
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEmpty(beanPath, getBroker().getPersistenceKey());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);
        codes.stream().forEach(rcd -> {
            String tableName = (String) rcd.getFieldValueByAlias(ALIAS_MINISTRY_TABLE_NAME);
            if (m_duplicateReferenceTableOids.containsKey(rcd.getReferenceTableOid())) {
                Set<String> ministryTableNames = m_duplicateReferenceTableOids.get(rcd.getReferenceTableOid());
                ministryTableNames.add(tableName);
            } else if (m_duplicateMinistryTables.containsKey(tableName)) {
                Set<String> rtbOids = m_duplicateMinistryTables.get(tableName);
                rtbOids.add(rcd.getReferenceTableOid());
            } else {
                String existingRtbOid = m_referenceTableByName.get(tableName);
                if (StringUtils.isEmpty(existingRtbOid)) {
                    m_referenceTableByName.put(tableName, rcd.getReferenceTableOid());
                } else if (!existingRtbOid.equals(rcd.getReferenceTableOid())) {
                    Set<String> rtbOids = new HashSet();
                    rtbOids.add(existingRtbOid);
                    rtbOids.add(rcd.getReferenceTableOid());
                    m_duplicateMinistryTables.put(tableName, rtbOids);
                    m_referenceTableByName.remove(tableName);
                }
                String matchingName = m_referenceTableByOid.get(rcd.getReferenceTableOid());
                if (StringUtils.isEmpty(matchingName)) {
                    m_referenceTableByOid.put(rcd.getReferenceTableOid(), tableName);
                } else if (!matchingName.equals(tableName)) {
                    Set<String> ministryTableNames = new HashSet();
                    ministryTableNames.add(tableName);
                    ministryTableNames.add(matchingName);
                    m_duplicateReferenceTableOids.put(rcd.getReferenceTableOid(), ministryTableNames);
                    m_referenceTableByOid.remove(rcd.getReferenceTableOid());
                }
            }
        });
    }

    /**
     * Detect if a parsed input string is empty.
     *
     * @param fields the fields
     * @return true if any of fields contains data
     */
    private boolean withData(List<Object> fields) {
        boolean found = false;
        for (int i = 0; !found && i < fields.size(); i++) {
            Object item = fields.get(i);
            if (item instanceof String) {
                found = !StringUtils.isEmpty((String) item);
            }
        }
        return found;
    }
}
