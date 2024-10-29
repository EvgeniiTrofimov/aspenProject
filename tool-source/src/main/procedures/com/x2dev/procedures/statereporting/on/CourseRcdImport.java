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
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.business.localization.LocalizationUtils;
import com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.DecimalAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.Date;
import java.text.Format;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.poifs.filesystem.POIFSFileSystem;

/**
 * The Class CourseRcdImport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class CourseRcdImport extends XlsImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String OLD_NEW_SPLIT = "&|&";
    private static final String OLD_NEW_SPLIT_REGEX = "\\&\\|\\&";

    protected static final List<FIELDS> DCC = Arrays.asList(FIELDS.COURSE_CODE,
            FIELDS.OLD_COURSE_CODE,
            FIELDS.CREDIT,
            FIELDS.OPEN_DATE,
            FIELDS.CLOSE_DATE,
            FIELDS.COLLEGE_COURSE_TITLE,
            FIELDS.COLLEGE_COURSE,
            FIELDS.COLLEGE,
            FIELDS.INSTITUTION_CODE,
            FIELDS.COLLEGE_COURSE_CODE,
            FIELDS.PASSING_GRADE,
            FIELDS.HOURS,
            FIELDS.LANGUAGE,
            FIELDS.ENGLISH_DESCRIPTION,
            FIELDS.FRENCH_DESCRIPTION,
            FIELDS.UPDATE_DIRECTIVE);
    protected static final List<FIELDS> LDC =
            Arrays.asList(FIELDS.BOARD_NUMBER,
                    FIELDS.SCHOOL_NUMBER,
                    FIELDS.COURSE_CODE,
                    FIELDS.LANGUAGE,
                    FIELDS.ENGLISH_DESCRIPTION,
                    FIELDS.FRENCH_DESCRIPTION,
                    FIELDS.CLOSE_DATE,
                    FIELDS.UPDATE_DIRECTIVE);
    protected static final List<FIELDS> MDC =
            Arrays.asList(FIELDS.COURSE_CODE,
                    FIELDS.ENGLISH_DESCRIPTION,
                    FIELDS.FRENCH_DESCRIPTION,
                    FIELDS.LANGUAGE,
                    FIELDS.SCHOOL_LANGUAGE,
                    FIELDS.PLAR_RDA,
                    FIELDS.OPEN_DATE,
                    FIELDS.CLOSE_DATE,
                    FIELDS.UPDATE_DIRECTIVE);

    /**
     * The Enum FIELDS contains all of the possible fields for the reference table.
     */
    public enum FIELDS {
        BOARD_NUMBER(STRING_ALIAS_BEGIN + "rcd-crs-board-number" + STRING_ALIAS_END),
        //
        CLOSE_DATE(STRING_ALIAS_BEGIN + "rcd-crs-date-close" + STRING_ALIAS_END,
                Arrays.asList(new KeyValuePair<String, Format>("LDC", new SimpleDateFormat("dd/MM/yyyy")))),
        //
        COLLEGE(STRING_ALIAS_BEGIN + "rcd-crs-college" + STRING_ALIAS_END),
        //
        COLLEGE_COURSE(STRING_ALIAS_BEGIN + "rcd-crs-college-course" + STRING_ALIAS_END),
        //
        COLLEGE_COURSE_CODE(STRING_ALIAS_BEGIN + "rcd-crs-college-course-code" + STRING_ALIAS_END),
        //
        COLLEGE_COURSE_TITLE(STRING_ALIAS_BEGIN + "rcd-crs-college-course-title" + STRING_ALIAS_END),
        //
        COURSE_CODE("code"),
        //
        CREDIT(STRING_ALIAS_BEGIN + "rcd-crs-credit" + STRING_ALIAS_END),
        //
        ENGLISH_DESCRIPTION("description"),
        // No value since french description is handled as resource.
        FRENCH_DESCRIPTION(""),
        //
        HOURS(STRING_ALIAS_BEGIN + "rcd-crs-hours" + STRING_ALIAS_END),
        //
        INSTITUTION_CODE(STRING_ALIAS_BEGIN + "rcd-crs-institution-code" + STRING_ALIAS_END),
        //
        LANGUAGE(STRING_ALIAS_BEGIN + "rcd-crs-course-language" + STRING_ALIAS_END),
        //
        OLD_COURSE_CODE(STRING_ALIAS_BEGIN + "rcd-crs-old-ministry-code" + STRING_ALIAS_END),
        //
        OPEN_DATE(STRING_ALIAS_BEGIN + "rcd-crs-date-open" + STRING_ALIAS_END),
        //
        PASSING_GRADE(STRING_ALIAS_BEGIN + "rcd-crs-passing-grade" + STRING_ALIAS_END),
        //
        PLAR_RDA(STRING_ALIAS_BEGIN + "rcd-crs-plar-rda" + STRING_ALIAS_END),
        //
        SCHOOL_LANGUAGE(STRING_ALIAS_BEGIN + "rcd-crs-school-language" + STRING_ALIAS_END),
        //
        SCHOOL_NUMBER(STRING_ALIAS_BEGIN + "rcd-crs-school-number" + STRING_ALIAS_END),
        //
        UPDATE_DIRECTIVE(STRING_ALIAS_BEGIN + "rcd-crs-update-directive" + STRING_ALIAS_END);

        private String m_path;
        private List<KeyValuePair<String, Format>> m_formatters;

        /**
         * Instantiates a new fields.
         *
         * @param path String
         */
        FIELDS(String path) {
            m_path = path;
        }

        /**
         * Instantiates a new fields.
         *
         * @param path String
         * @param formatters List<KeyValuePair<String,Format>>
         */
        FIELDS(String path, List<KeyValuePair<String, Format>> formatters) {
            m_path = path;
            m_formatters = formatters;
        }

        /**
         * Gets the alias.
         *
         * @return String
         */
        public String getAlias() {
            Matcher matcher = PATTERN_ALIAS.matcher(m_path);
            return matcher.find() ? matcher.group(1) : null;
        }

        /**
         * Gets the bean path.
         *
         * @return String
         * @see com.x2dev.procedures.statereporting.on.CourseRcdImport.DataMap#getFieldValue()
         */
        public String getBeanPath() {
            return PATTERN_ALIAS.matcher(m_path).matches() ? null : m_path;
        }

        /**
         * Gets the formatter.
         *
         * @param key String
         * @return Format
         */
        public Format getFormatter(String key) {
            if (m_formatters != null) {
                for (KeyValuePair<String, Format> pair : m_formatters) {
                    if (key.equals(pair.getKey())) {
                        return pair.getValue();
                    }
                }
            }
            return null;
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
        // row processing skipped due to some error
        SKIPPED,
        // existing row updated
        UPDATED
    }

    /**
     * The Class FieldMapper.
     */
    private class FieldMapper {
        private DateAsStringConverter m_dateConverter = (DateAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.DATE_CONVERTER, null, true);
        private DecimalAsStringConverter m_decimalConverter = (DecimalAsStringConverter) ConverterFactory
                .getConverterForClass(Converter.BIG_DECIMAL_CONVERTER, null, true);
        private PlainDate m_excelZeroDate;
        private List<FIELDS> m_fields;
        private String m_fileType;
        private Set<String> m_processedReferenceCodes = new HashSet();

        /**
         * Instantiates a new field mapper.
         *
         * @param fileType String
         */
        public FieldMapper(String fileType) {
            m_fileType = fileType;
            switch (fileType) {
                case FILE_TYPE_DCC:
                    m_fields = DCC;
                    break;
                case FILE_TYPE_LDC:
                    m_fields = LDC;
                    break;
                case FILE_TYPE_MDC:
                    m_fields = MDC;
                    break;
            }
            try {
                m_excelZeroDate = new PlainDate(new SimpleDateFormat("yyyyMMdd").parse("18991230"));
            } catch (ParseException e) {
                // Should not fail
            }
        }

        /**
         * Gets the field.
         *
         * @param field FIELDS
         * @param cells List<String>
         * @return String
         */
        protected String getField(FIELDS field, List<String> cells) {
            String value = null;
            switch (field) {
                case COURSE_CODE:
                    value = getRawField(field, cells).replaceAll("\\s", "");
                    if (m_fields == DCC) {
                        String institutionCode = getRawField(FIELDS.INSTITUTION_CODE, cells);
                        if (!StringUtils.isEmpty(institutionCode)) {
                            value += "-" + institutionCode;
                        }
                    }
                    break;
                default:
                    value = getRawField(field, cells);
                    break;
            }
            return value;
        }

        /**
         * Gets the num fields.
         *
         * @return int
         */
        protected int getNumFields() {
            return m_fields.size();
        }

        /**
         * Sets the field.
         *
         * @param refCode ReferenceCode
         * @param field FIELDS
         * @param cells List<String>
         * @param changedValues
         */
        protected void setField(ReferenceCode refCode,
                                FIELDS field,
                                List<String> cells,
                                Map<String, String> changedValues) {
            switch (field) {
                case UPDATE_DIRECTIVE:
                    String update = getRawField(field, cells);
                    if ("delete".equalsIgnoreCase(update)) {
                        String oldDisabledInd = "" + refCode.getDisabledIndicator();
                        refCode.setDisabledIndicator(true);
                        String newDisabledInd = "" + refCode.getDisabledIndicator();
                        addIfDifferent(changedValues, field.toString(), oldDisabledInd, newDisabledInd);
                    }
                    break;
                case SCHOOL_NUMBER:
                    if (m_processedReferenceCodes.contains(getField(FIELDS.COURSE_CODE, cells))) {
                        String school = getRawField(field, cells);
                        String otherSchools =
                                (String) refCode.getFieldValueByAlias(field.getAlias(), getReferenceTableDictionary());
                        if (StringUtils.isEmpty(school)) {
                            school = otherSchools;
                        } else if (!StringUtils.isEmpty(otherSchools)) {
                            school += "," + otherSchools;
                        }


                        setRawField(refCode, field, school, changedValues);
                    } else {
                        setRawField(refCode, field, cells, changedValues);
                    }
                    break;
                default:
                    setRawField(refCode, field, cells, changedValues);
                    break;
            }
        }


        /**
         * @param refCode
         * @param field
         * @param cells
         * @param changedValues
         */
        private void setRawField(ReferenceCode refCode,
                                 FIELDS field,
                                 List<String> cells,
                                 Map<String, String> changedValues) {
            DataDictionaryField dictionaryField = getDataDictionaryField(field);
            if (dictionaryField != null) {
                String oldValue = "" + refCode.getFieldValueByBeanPath(dictionaryField.getJavaName());
                setRawField(refCode, field, cells);
                String newValue = "" + refCode.getFieldValueByBeanPath(dictionaryField.getJavaName());
                addIfDifferent(changedValues, field.toString(), oldValue, newValue);
            }
        }

        /**
         * @param refCode
         * @param field
         * @param school
         * @param changedValues
         */
        private void setRawField(ReferenceCode refCode,
                                 FIELDS field,
                                 String school,
                                 Map<String, String> changedValues) {
            DataDictionaryField dictionaryField = getDataDictionaryField(field);
            if (dictionaryField != null) {
                String oldValue = "" + refCode.getFieldValueByBeanPath(dictionaryField.getJavaName());
                setRawField(refCode, field, school);
                String newValue = "" + refCode.getFieldValueByBeanPath(dictionaryField.getJavaName());
                addIfDifferent(changedValues, field.toString(), oldValue, newValue);
            }
        }

        /**
         * Sets the fields.
         *
         * @param refCode ReferenceCode
         * @param cells List<String>
         */
        protected void setFields(ReferenceCode refCode, List<String> cells, Map<String, String> changedValues) {
            String oldDependencyCode = refCode.getDependencyCode();
            refCode.setDependencyCode(m_fileType);
            String newDependencyCode = refCode.getDependencyCode();
            addIfDifferent(changedValues, "DEPENDENCY_CODE", oldDependencyCode, newDependencyCode);

            String oldStateCode = refCode.getStateCode();
            refCode.setStateCode(getRawField(FIELDS.COURSE_CODE, cells).replaceAll("\\s", ""));
            String newStateCode = refCode.getStateCode();
            addIfDifferent(changedValues, "STATE_CODE", oldStateCode, newStateCode);

            for (FIELDS field : m_fields) {
                setField(refCode, field, cells, changedValues);
            }
            m_processedReferenceCodes.add(refCode.getCode());
        }

        /**
         * @param changedValues
         * @param string
         * @param oldDependencyCode
         * @param newDependencyCode
         */
        private void addIfDifferent(Map<String, String> changedValues,
                                    String key,
                                    String oldValue,
                                    String newValue) {
            if (!StringUtils.equals(oldValue, newValue)) {
                changedValues.put(key, oldValue + OLD_NEW_SPLIT + newValue);
            }
        }

        /**
         * Lookup and return a DataDictionaryField based on a root bean and bean path.
         * This allows multi-hop paths in the bean path.
         *
         * @param field FIELDS
         * @return DataDictionaryField
         */
        private DataDictionaryField getDataDictionaryField(FIELDS field) {
            DataDictionaryField dictionaryField = null;
            if (!StringUtils.isEmpty(field.getBeanPath())) {
                ModelProperty prop =
                        new ModelProperty(ReferenceCode.class, field.getBeanPath(), getBroker().getPersistenceKey());
                dictionaryField = getReferenceTableDictionary().findDataDictionaryField(prop.getFieldId());
            } else if (field.getAlias() != null) {
                dictionaryField = getReferenceTableDictionary().findDataDictionaryFieldByAlias(field.getAlias());
                if (dictionaryField == null) {
                    throw new IllegalStateException("Reference Code Alias " + field.getAlias() + " not found");
                }
                if (!dictionaryField.getDataTable().getDataClass().equals(ReferenceCode.class)) {
                    dictionaryField = null;
                    logError("Alias " + field.getAlias() + " is not on ReferenceCode");
                }
            }
            return dictionaryField;
        }

        /**
         * Gets the raw field.
         *
         * @param keyField FIELDS
         * @param cells List<String>
         * @return String
         */
        private String getRawField(FIELDS keyField, List<String> cells) {
            String value = null;
            int index = 0;
            for (FIELDS field : m_fields) {
                if (field.equals(keyField)) {
                    value = cells.get(index).trim();
                    if (value == null) {
                        value = "";
                    }
                    break;
                }
                ++index;
            }
            return value;
        }

        /**
         * Sets the raw field.
         *
         * @param refCode ReferenceCode
         * @param field FIELDS
         * @param cells List<String>
         */
        private void setRawField(ReferenceCode refCode, FIELDS field, List<String> cells) {
            String value = getField(field, cells);
            setRawField(refCode, field, value);
        }

        /**
         * Sets the raw field.
         *
         * @param refCode ReferenceCode
         * @param field FIELDS
         * @param value String
         */
        private void setRawField(ReferenceCode refCode, FIELDS field, String value) {
            DataDictionaryField dictionaryField = getDataDictionaryField(field);
            if (dictionaryField != null) {
                if (dictionaryField.getEffectiveJavaType().equals(String.class.getName())) {
                    if (dictionaryField.getDatabaseType().equals(DataField.CHAR_DATABASE_TYPE)
                            || dictionaryField.getDatabaseType().equals(DataField.VARCHAR_DATABASE_TYPE)) {
                        int maxLength = dictionaryField.getUserLength() > 0 ? dictionaryField.getUserLength()
                                : dictionaryField.getDatabaseLength();
                        if (value != null && value.length() > maxLength) {
                            logError("Value " + value + " is to long for field " + field + " Value truncated.");
                            value = value.substring(0, maxLength - 1);
                        }
                    }
                } else if (dictionaryField.getEffectiveJavaType().equals(BigDecimal.class.getName())) {
                    BigDecimal decimal = null;
                    try {
                        decimal = new BigDecimal(value);
                    } catch (Exception e) {
                        // generate error
                    }
                    if (decimal != null) {
                        value = m_decimalConverter.getSystemString(decimal);
                    } else {
                        if (!StringUtils.isEmpty(value)) {
                            logError("BigDecimal conversion error for " + value + " to field " + field);
                        }
                        value = null;
                    }
                } else if (dictionaryField.getEffectiveJavaType().equals(Date.class.getName())) {
                    PlainDate date = null;
                    Matcher matcher = PATTERN_EXCEL_DATE.matcher(value);
                    if (matcher.find()) {
                        date = DateUtils.add(m_excelZeroDate, Integer.parseInt(matcher.group(1)));
                    } else if (!StringUtils.isEmpty(value)) {
                        Format formatter = field.getFormatter(m_fileType);
                        if (formatter != null) {
                            try {
                                date = new PlainDate((java.util.Date) formatter.parseObject(value));
                            } catch (ParseException e) {
                                // ignore error
                            }
                        }
                        if (date == null) {
                            logError("No PlainDate converter/formatter for " + value + " to field " + field);
                        }
                    }
                    if (date != null) {
                        value = m_dateConverter.getSystemString(date);
                    }
                } else {
                    logError("No converter for " + value + " to field " + field);
                }
                refCode.setFieldValueByBeanPath(dictionaryField.getJavaName(), value);
            }
        }
    }

    /**
     * Class to process input file record.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private class Record {
        private List<String> m_cells;
        private Result m_result;

        /**
         * Create record object from lineNumber and list of field values.
         *
         * @param record List<String>
         */
        public Record(List<String> record) {
            m_cells = record;
            m_result = Result.SKIPPED;
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
            m_result = Result.SKIPPED;
            String codeValue = getField(FIELDS.COURSE_CODE);

            if (!validate()) {
                return;
            }
            ReferenceCode code = m_existingCodes.get(codeValue);

            if (code != null) {
                if (updateCode(code)) {
                    m_result = Result.UPDATED;
                }
            } else {
                code = insertCode();
                if (code != null) {
                    m_result = Result.INSERTED;
                }
            }
        }

        /**
         * @param code
         * @param changedValues
         */
        private void logResult(String action, ReferenceCode code, Map<String, String> changedValues) {
            if (code == null) {
                logError("Unable to " + action + " code", m_lineNumber);
                return;
            }

            if (changedValues.isEmpty()) {
                return;
            }

            logMessage("");
            logMessage(action + " " + code.getCode());
            for (String fieldName : changedValues.keySet()) {
                String value = changedValues.get(fieldName);
                String[] oldAndNew = value.split(OLD_NEW_SPLIT_REGEX, 2);
                String oldMsg = StringUtils.isBlank(oldAndNew[0]) || "null".equals(oldAndNew[0]) ? ""
                        : " Old value [" + oldAndNew[0] + "]";
                String msg = StringUtils.rightPad(fieldName + ":", 24)
                        + oldMsg + " New value [" + oldAndNew[1] + "]";
                logMessage(msg);
            }
        }

        /**
         * Gets the field.
         *
         * @param field FIELDS
         * @return String
         */
        private String getField(FIELDS field) {
            return getMapper().getField(field, m_cells);
        }

        /**
         * Insert new reference code.
         *
         * @return success flag
         */
        private ReferenceCode insertCode() {
            ReferenceCode refCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            refCode.setReferenceTableOid(getReferenceTable().getOid());
            Map<String, String> changedValues = new LinkedHashMap<>();
            getMapper().setFields(refCode, m_cells, changedValues);
            refCode.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            refCode.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            boolean saveOK = saveBean(refCode) && saveResource(refCode);
            if (saveOK) {
                m_existingCodes.put(refCode.getCode(), refCode);
                logResult("Insert", refCode, changedValues);
                return refCode;
            }
            return null;
        }

        /**
         * Matches board.
         *
         * @return true, if successful
         */
        private boolean matchesBoard() {
            String boardId = getField(FIELDS.BOARD_NUMBER);
            return (StringUtils.isEmpty(boardId) || boardId.equals(getOrganization().getId())) ? true : false;
        }

        /**
         * Save bean.
         *
         * @param bean X2BaseBean
         * @return true, if successful
         */
        private boolean saveBean(ReferenceCode bean) {
            boolean ok = true;
            if (m_commitChanges) {
                List<ValidationError> errors;
                try {
                    errors = getBroker().saveBean(bean);
                } catch (Exception e) {
                    logError("An Error ");
                    throw (e);
                }
                if (errors != null && !errors.isEmpty()) {
                    for (ValidationError error : errors) {
                        logError("Validation error for code " + bean.getCode() + ": " + error.toString(), m_lineNumber);
                    }
                    ok = false;
                }
            }
            return ok;
        }

        /**
         * Save localization resource for created/updated reference code.
         *
         * @param refCode ReferenceCode
         * @return success flag
         */
        private boolean saveResource(ReferenceCode refCode) {
            if (m_commitChanges) {
                String descriptionShortFr = getField(FIELDS.FRENCH_DESCRIPTION);
                if (StringUtils.isBlank(descriptionShortFr)) {
                    return true;
                }

                LocalizationMessageResources messages =
                        LocalizationCache.getMessages(getBroker().getPersistenceKey(), LOCALE_CANADA_FRENCH);
                String generateKey = LocalizationUtils.generateKey(refCode.getOid(), getDescriptionBeanPath());
                String message = messages.getMessage(generateKey);
                if (message.equals(descriptionShortFr)) {
                    return true;
                }

                LocalizationResource resource = new LocalizationResource(LOCALE_CANADA_FRENCH,
                        generateKey, refCode.getOid(), descriptionShortFr);

                LocalizationCache.setLocalizedResource(getBroker(), resource);
            }
            return true;
        }

        /**
         * Update existing reference code.
         *
         * @param refCode ReferenceCode
         * @return success flag
         */
        private boolean updateCode(ReferenceCode refCode) {
            Map<String, String> changedValues = new LinkedHashMap<>();
            getMapper().setFields(refCode, m_cells, changedValues);
            logResult("Update", refCode, changedValues);
            if (refCode.isDirty()) {
                return saveBean(refCode) && saveResource(refCode);
            }
            return saveResource(refCode);
        }

        /**
         * Validate fields values.
         *
         * @return true, if all the fields are valid
         */
        private boolean validate() {
            boolean valid = false;
            String descriptionShortEn = getField(FIELDS.ENGLISH_DESCRIPTION);
            if (StringUtils.isEmpty(getField(FIELDS.COURSE_CODE))) {
                logError("Code is empty", m_lineNumber);
            } else if (StringUtils.isEmpty(descriptionShortEn)) {
                logError("Short description EN is empty", m_lineNumber);
            } else if (StringUtils.isEmpty(getField(FIELDS.FRENCH_DESCRIPTION))) {
                logError("Short description FR is empty", m_lineNumber);
            } else if (matchesBoard()) {
                valid = true;
            } else {
                String boardId = getOrganization().getId();
                String rowBoardId = getField(FIELDS.BOARD_NUMBER);
                logError("Row has another board ID. Current Board ID: " + boardId + ", but row Board ID: " + rowBoardId,
                        m_lineNumber);
            }
            return valid;
        }
    }

    private static final String ALIAS_COURSE_CODE = "all-crs-MinistryCourseCode";
    private static final String FILE_TYPE_DCC = "DCC";
    private static final String FILE_TYPE_LDC = "LDC";
    private static final String FILE_TYPE_MDC = "MDC";
    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String INPUT_PARAM_FILE_TYPE = "filetype";
    private static final String LOCALE_CANADA_FRENCH = Locale.CANADA_FRENCH.toString();
    private static final Pattern PATTERN_ALIAS = Pattern.compile("\\[(.*)\\]");
    private static final Pattern PATTERN_EXCEL_DATE = Pattern.compile("^(\\d{5})(.\\d*)?$");
    private static final String STRING_ALIAS_BEGIN = "[";
    private static final String STRING_ALIAS_END = "]";

    private String m_beanPathRcdDescription;
    private boolean m_commitChanges;
    private DataDictionary m_dictionaryReferenceTable;
    private List<String> m_errors = new LinkedList<>();
    private List<String> m_messages = new LinkedList<>();
    private Map<String, ReferenceCode> m_existingCodes = new HashMap<>();
    private FieldMapper m_fieldMapper;
    private int m_lineNumber;
    private ReferenceTable m_referenceTable;

    /**
     * Display only errors, if any, and exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder results = new StringBuilder();
        results.append(m_commitChanges ? "Commit Mode." : "Review Mode.");
        results.append("\n\n");
        if (!m_errors.isEmpty()) {
            exportErrors(results);
        }
        results.append("\n\n");
        results.append(getImportStatistics().toString());

        if (!m_messages.isEmpty()) {
            results.append("\n\n");
            exportMessages(results);
        }
        try {
            export(results.toString());
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Gets the description bean path.
     *
     * @return String
     */
    protected String getDescriptionBeanPath() {
        return m_beanPathRcdDescription;
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return getMapper().getNumFields();
    }

    /**
     * Gets the mapper.
     *
     * @return Field mapper
     */
    protected FieldMapper getMapper() {
        return m_fieldMapper;
    }

    /**
     * Gets the reference table.
     *
     * @return X 2 base bean
     */
    protected X2BaseBean getReferenceTable() {
        return m_referenceTable;
    }

    /**
     * Gets the reference table dictionary.
     *
     * @return Data dictionary
     */
    protected DataDictionary getReferenceTableDictionary() {
        return m_dictionaryReferenceTable;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        InputStream inputStream = null;

        try {
            inputStream = new FileInputStream(sourceFile.getPath());
        } catch (FileNotFoundException fnfe) {
            logInvalidRecord(0, "error.file.edit.notFound", sourceFile.getAbsolutePath());
        }
        boolean isValid = true;
        if (inputStream != null) {
            try {
                POIFSFileSystem fileSystem = new POIFSFileSystem(inputStream);
                HSSFWorkbook workbook = new HSSFWorkbook(fileSystem);
                int numberOfSheets = workbook.getNumberOfSheets();
                if (numberOfSheets != 1) {
                    logError("Error: workbook must have only one sheet, but number of sheets is: " + numberOfSheets
                            + ". Does it have hidden sheets?");
                    isValid = false;
                }

                if (!isValid) {
                    return;
                }

                HSSFSheet sheet = workbook.getSheetAt(0);
                int fieldsCount = getFieldCount();
                for (int lineNumber = 0; lineNumber <= sheet.getLastRowNum(); lineNumber++) {
                    HSSFRow row = sheet.getRow(lineNumber);

                    if (lineNumber == 0) {
                        for (short i = 0; i < fieldsCount; i++) {
                            HSSFCell cell = row.getCell(i);
                            if (cell == null || cell.getRichStringCellValue() == null
                                    || StringUtils.isEmpty(cell.getRichStringCellValue().getString())) {
                                logError("Error: header row cell cannot be empty. Number of the empty cell: " + i,
                                        lineNumber + 1);
                                isValid = false;
                            }
                            if (!isValid) {
                                return;
                            }
                        }
                    }

                    if (row == null) {
                        logError("Error: sheet must not contain merged cells.", lineNumber + 1);
                        isValid = false;
                    }

                    if (!isValid) {
                        return;
                    }
                }
            } catch (Exception e) {
                logError(LoggerUtils.convertThrowableToString(e));
            } finally {
                inputStream.close();
            }
        }
        if (isValid) {
            super.importData(sourceFile);
        }
    }

    /**
     * Import record.
     *
     * @param fields List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> fields, int lineNumber) throws Exception {
        m_lineNumber = lineNumber;
        if (withData(fields)) {
            Record record = new Record(fields);
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
        m_fieldMapper = new FieldMapper((String) getParameter(INPUT_PARAM_FILE_TYPE));
        m_referenceTable = findReferenceTable();
        m_dictionaryReferenceTable = DataDictionary.getDistrictDictionary(m_referenceTable.getExtendedDataDictionary(),
                getBroker().getPersistenceKey());

        loadBeanPath();
        loadExistingReferenceCodes();
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
     * @param results StringBuilder
     */
    private void exportErrors(StringBuilder results) {
        for (String error : m_errors) {
            results.append(error);
            results.append("\n");
        }
    }

    /**
     * Output messages to the results string.
     *
     * @param results StringBuilder
     */
    private void exportMessages(StringBuilder results) {
        if (m_messages.isEmpty()) {
            return;
        }

        for (String msg : m_messages) {
            results.append(msg);
            results.append("\n");
        }
    }

    /**
     * Find reference table.
     *
     * @return ReferenceTable
     */
    private ReferenceTable findReferenceTable() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_COURSE_CODE);
        return field.getReferenceTable();
    }

    /**
     * Gets the current line number.
     *
     * @return int
     */
    private int getCurrentLineNumber() {
        return m_lineNumber;
    }

    /**
     * Find the Reference Code description bean path.
     */
    private void loadBeanPath() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty property = new ModelProperty(ReferenceCode.class, ReferenceCode.COL_DESCRIPTION, dictionary);
        m_beanPathRcdDescription = property.getDictionaryPath();
    }


    /**
     * Find existing reference codes.
     */
    private void loadExistingReferenceCodes() {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, m_referenceTable.getOid());
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_existingCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 4096);
    }

    /**
     * Log error.
     *
     * @param error String
     */
    private void logError(String error) {
        logError(error, getCurrentLineNumber());
    }

    /**
     * Log processing error message.
     *
     * @param error String
     * @param lineNumber int
     */
    private void logError(String error, int lineNumber) {
        m_errors.add(String.format("ERROR: line %d. %s", lineNumber, error));
    }

    /**
     *
     * @param msg
     */
    private void logMessage(String msg) {
        m_messages.add(msg);
    }

    /**
     * Detect if a parsed input string is empty.
     *
     * @param fields List<String>
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
