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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
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
import com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.converters.DecimalAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.Date;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class CourseRcdImport.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class BsidRcdImport extends XlsImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String OLD_NEW_SPLIT = "&|&";
    private static final String OLD_NEW_SPLIT_REGEX = "\\&\\|\\&";

    protected static final List<CellField> INTERNATIONAL =
            Arrays.asList(CellField.SCHOOL_NAME, CellField.SCHOOL_NUMBER, CellField.SCHOOL_SPECIAL_CONDITIONS,
                    CellField.ADDRESS_LINE_1, CellField.ADDRESS_LINE_2, CellField.ADDRESS_LINE_3, CellField.COUNTRY,
                    CellField.PHONE, CellField.FAX, CellField.SCHOOL_WEBSITE);
    protected static final List<CellField> PRIVATE =
            Arrays.asList(CellField.SCHOOL_NAME, CellField.SUITE, CellField.PO_BOX, CellField.STREET, CellField.CITY,
                    CellField.PROVINCE,
                    CellField.POSTAL_CODE, CellField.PHONE, CellField.FAX, CellField.REGION, CellField.SCHOOL_NUMBER,
                    CellField.SCHOOL_WEBSITE, CellField.SCHOOL_LEVEL, CellField.SCHOOL_SPECIAL_CONDITIONS);
    protected static final List<CellField> PUBLIC =
            Arrays.asList(CellField.REGION, CellField.BOARD_NUMBER, CellField.BOARD_NAME, CellField.BOARD_TYPE,
                    CellField.BOARD_LANGUAGE, CellField.SCHOOL_NUMBER, CellField.SCHOOL_NAME, CellField.SCHOOL_LEVEL,
                    CellField.SCHOOL_LANGUAGE, CellField.SCHOOL_TYPE, CellField.SCHOOL_SPECIAL_CONDITIONS,
                    CellField.SUITE,
                    CellField.PO_BOX,
                    CellField.STREET, CellField.CITY, CellField.PROVINCE, CellField.POSTAL_CODE, CellField.PHONE,
                    CellField.FAX,
                    CellField.GRADE_RANGE, CellField.DATE_OPEN, CellField.SCHOOL_EMAIL, CellField.SCHOOL_WEBSITE,
                    CellField.BOARD_WEBSITE);

    /**
     * The Enum FIELDS contains all of the possible fields for the reference table.
     */
    public enum CellField {
        ADDRESS_LINE_1(STRING_ALIAS_BEGIN + "rcd-bsid-school-address-lines" + STRING_ALIAS_END),
        //
        ADDRESS_LINE_2(""),
        //
        ADDRESS_LINE_3(""),
        //
        BOARD_NAME("", "description"),
        //
        BOARD_NUMBER(STRING_ALIAS_BEGIN + "rcd-bsid-board-number" + STRING_ALIAS_END, "code"),
        //
        BOARD_LANGUAGE("", STRING_ALIAS_BEGIN + "rcd-bsid-board-language" + STRING_ALIAS_END),
        //
        BOARD_TYPE("", STRING_ALIAS_BEGIN + "rcd-bsid-board-type" + STRING_ALIAS_END),
        //
        BOARD_WEBSITE("", STRING_ALIAS_BEGIN + "rcd-bsid-board-website" + STRING_ALIAS_END),
        //
        CITY(STRING_ALIAS_BEGIN + "rcd-bsid-city" + STRING_ALIAS_END),
        //
        COUNTRY(STRING_ALIAS_BEGIN + "rcd-bsid-country" + STRING_ALIAS_END),
        //
        DATE_OPEN(STRING_ALIAS_BEGIN + "rcd-bsid-date-open" + STRING_ALIAS_END),
        //
        FAX(STRING_ALIAS_BEGIN + "rcd-bsid-fax" + STRING_ALIAS_END),
        //
        GRADE_RANGE(STRING_ALIAS_BEGIN + "rcd-bsid-grade-range" + STRING_ALIAS_END),
        //
        PHONE(STRING_ALIAS_BEGIN + "rcd-bsid-phone" + STRING_ALIAS_END),
        //
        PO_BOX(""),
        //
        POSTAL_CODE(STRING_ALIAS_BEGIN + "rcd-bsid-postal-code" + STRING_ALIAS_END),
        //
        PROVINCE(STRING_ALIAS_BEGIN + "rcd-bsid-province" + STRING_ALIAS_END),
        //
        REGION(STRING_ALIAS_BEGIN + "rcd-bsid-region" + STRING_ALIAS_END),
        //
        SCHOOL_EMAIL(STRING_ALIAS_BEGIN + "rcd-bsid-school-email" + STRING_ALIAS_END),
        //
        SCHOOL_LANGUAGE(STRING_ALIAS_BEGIN + "rcd-bsid-language-type" + STRING_ALIAS_END),
        //
        SCHOOL_LEVEL(STRING_ALIAS_BEGIN + "rcd-bsid-school-level" + STRING_ALIAS_END),
        //
        SCHOOL_NAME("description"),
        //
        SCHOOL_NUMBER("code"),
        //
        SCHOOL_SPECIAL_CONDITIONS(STRING_ALIAS_BEGIN + "rcd-bsid-school-special-condit" + STRING_ALIAS_END),
        //
        SCHOOL_TYPE(STRING_ALIAS_BEGIN + "rcd-bsid-school-type" + STRING_ALIAS_END),
        //
        SCHOOL_WEBSITE(STRING_ALIAS_BEGIN + "rcd-bsid-school-website" + STRING_ALIAS_END),
        //
        STREET(STRING_ALIAS_BEGIN + "rcd-bsid-street" + STRING_ALIAS_END),
        //
        SUITE(STRING_ALIAS_BEGIN + "rcd-bsid-suite-po-box" + STRING_ALIAS_END);

        private Map<String, String> m_pathByDdxId = new HashMap<String, String>();

        /**
         * Instantiates a new fields.
         *
         * @param path String
         */
        CellField(String schoolRefCodePath) {
            m_pathByDdxId.put(DDX_ID_REF_BSID_SCHOOL, schoolRefCodePath);
        }

        /**
         * Instantiates a new fields.
         *
         * @param path String
         */
        CellField(String schoolRefCodePath, String boardRefCodePath) {
            m_pathByDdxId.put(DDX_ID_REF_BSID_SCHOOL, schoolRefCodePath);
            m_pathByDdxId.put(DDX_ID_REF_BSID_BOARD, boardRefCodePath);
        }

        /**
         * Gets the alias.
         *
         * @param ddxId String
         * @return String
         */
        public String getAlias(String ddxId) {
            String path = m_pathByDdxId.get(ddxId);
            Matcher matcher = PATTERN_ALIAS.matcher(path == null ? "" : path);
            return matcher.find() ? matcher.group(1) : null;
        }

        /**
         * Gets the bean path.
         *
         * @param ddxId String
         * @return String
         * @see com.x2dev.procedures.statereporting.on.CourseRcdImport.DataMap#getFieldValue()
         */
        public String getBeanPath(String ddxId) {
            String path = m_pathByDdxId.get(ddxId);
            if (StringUtils.isEmpty(path)) {
                return null;
            }
            return PATTERN_ALIAS.matcher(path == null ? "" : path).matches() ? null : path;
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
        private List<CellField> m_fields;
        private String m_fileType;
        private Map<String, ReferenceCode> m_mapSpecialConditionsRefCodesByCode = new HashMap();
        private Map<String, ReferenceCode> m_mapSpecialConditionsRefCodesByDescription = new HashMap();
        private Set<String> m_setNewSpecialConditions = new HashSet();
        private String m_specialConditionsRtbOid;

        /**
         * Instantiates a new field mapper.
         *
         * @param fileType String
         */
        public FieldMapper(String fileType) {
            m_fileType = fileType;
            switch (fileType) {
                case FILE_TYPE_INTERNATIONAL:
                    m_fields = INTERNATIONAL;
                    break;
                case FILE_TYPE_PRIVATE:
                    m_fields = PRIVATE;
                    break;
                case FILE_TYPE_PUBLIC:
                    m_fields = PUBLIC;
                    break;
            }
            initSpecialConditionsRefTable();
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
        protected String getField(CellField field, List<String> cells) {
            String value = null;
            switch (field) {
                case ADDRESS_LINE_1:
                    StringBuilder concatAddress = new StringBuilder();
                    for (CellField addrField : Arrays.asList(CellField.ADDRESS_LINE_1, CellField.ADDRESS_LINE_2,
                            CellField.ADDRESS_LINE_3)) {
                        String line = getRawField(addrField, cells);
                        if (!StringUtils.isEmpty(line)) {
                            if (concatAddress.length() > 0) {
                                concatAddress.append(", ");
                            }
                            concatAddress.append(line);
                        }
                    }
                    if (concatAddress.length() > 0) {
                        value = concatAddress.toString();
                    }
                    break;
                case SCHOOL_WEBSITE:
                    StringBuilder concatWebsite = new StringBuilder();
                    String schoolWebSite = getRawField(CellField.SCHOOL_WEBSITE, cells);
                    if (!StringUtils.isEmpty(schoolWebSite)) {
                        concatWebsite.append("School: ");
                        concatWebsite.append(schoolWebSite);
                    }
                    String boardWebSite = getRawField(CellField.BOARD_WEBSITE, cells);
                    if (!StringUtils.isEmpty(boardWebSite)) {
                        if (concatWebsite.length() > 1) {
                            concatWebsite.append(", ");
                        }
                        concatWebsite.append("Board: ");
                        concatWebsite.append(boardWebSite);
                    }
                    if (concatWebsite.length() > 0) {
                        value = concatWebsite.toString();
                    }
                    break;
                case SUITE:
                    StringBuilder concatSuitePoBox = new StringBuilder();
                    String suite = getRawField(CellField.SUITE, cells);
                    if (!StringUtils.isEmpty(suite)) {
                        if (PATTERN_SUITE.matcher(suite).matches()) {
                            concatSuitePoBox.append("Suite: ");
                        }
                        concatSuitePoBox.append(suite);
                    }
                    String box = getRawField(CellField.PO_BOX, cells);
                    if (!StringUtils.isEmpty(box)) {
                        if (concatSuitePoBox.length() > 1) {
                            concatSuitePoBox.append(" ,");
                        }
                        if (PATTERN_PO_BOX.matcher(box).matches()) {
                            concatSuitePoBox.append("PO Box: ");
                        }
                        concatSuitePoBox.append(box);
                    }
                    if (concatSuitePoBox.length() > 0) {
                        value = concatSuitePoBox.toString();
                        if (value.length() > 20) {
                            value = value.substring(0, 20);
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
         * Gets the new special conditions.
         *
         * @return Sets the
         */
        protected Set<String> getNewSpecialConditions() {
            return m_setNewSpecialConditions;
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
         * Gets the special conditions rtb oid.
         *
         * @return String
         */
        protected String getSpecialConditionsRtbOid() {
            return m_specialConditionsRtbOid;
        }

        /**
         * Sets the field.
         *
         * @param refCode ReferenceCode
         * @param field FIELDS
         * @param cells List<String>
         * @param ddxId String
         * @param changedValues Map<String, String>
         */
        protected void setField(ReferenceCode refCode,
                                CellField field,
                                List<String> cells,
                                String ddxId,
                                Map<String, String> changedValues) {
            switch (field) {
                case SCHOOL_SPECIAL_CONDITIONS:
                    if (DDX_ID_REF_BSID_SCHOOL.equals(ddxId)) {
                        String key = getField(field, cells);
                        if (m_mapSpecialConditionsRefCodesByCode.containsKey(key)) {
                            setRawField(refCode, field, key, ddxId, changedValues);
                        } else if (m_mapSpecialConditionsRefCodesByDescription.containsKey(key)) {
                            ReferenceCode rcdSpecialConditions = m_mapSpecialConditionsRefCodesByDescription.get(key);
                            setRawField(refCode, field, rcdSpecialConditions.getCode(), ddxId, changedValues);
                        } else {
                            m_setNewSpecialConditions.add(key);
                            m_mapSpecialConditionsRefCodesByCode.put(key, null);
                            setRawField(refCode, field, key, ddxId, changedValues);
                            logError("New Special Conditions Reference Code [" + key + "] created");
                        }
                    }

                    break;
                default:
                    setRawField(refCode, field, cells, ddxId, changedValues);
                    break;
            }
        }

        /**
         * Sets the fields.
         *
         * @param refCode ReferenceCode
         * @param cells List<String>
         * @param ddxId String
         * @param keyField CellField
         * @param changedValues Map<String, String>
         */
        protected void setFields(ReferenceCode refCode,
                                 List<String> cells,
                                 String ddxId,
                                 CellField keyField,
                                 Map<String, String> changedValues) {
            String oldStateCode = refCode.getStateCode();
            refCode.setStateCode(getRawField(keyField, cells).replaceAll("\\s", ""));
            String newStateCode = refCode.getStateCode();
            addIfDifferent(changedValues, "STATE_CODE", oldStateCode, newStateCode);

            String oldDependencyCode = refCode.getDependencyCode();

            /*
             * Default dependency code: file type (PUBLIC/PRIVATE/INDEP)
             * School ref tables only: Set DependencyCode to BoardId
             */
            String dependencyCode = m_fileType;

            boolean isSchool = CellField.SCHOOL_NUMBER.equals(keyField);
            for (CellField field : m_fields) {
                setField(refCode, field, cells, ddxId, changedValues);

                if (isSchool && CellField.BOARD_NUMBER.name().equals(field.name())) {
                    dependencyCode = getField(field, cells);
                }
            }

            refCode.setDependencyCode(dependencyCode);
            String newDependencyCode = refCode.getDependencyCode();
            addIfDifferent(changedValues, "DEPENDENCY_CODE", oldDependencyCode, newDependencyCode);
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
        private DataDictionaryField getDataDictionaryField(CellField field, String ddxId) {
            DataDictionaryField dictionaryField = null;
            if (!StringUtils.isEmpty(field.getBeanPath(ddxId))) {
                ModelProperty prop =
                        new ModelProperty(ReferenceCode.class, field.getBeanPath(ddxId),
                                getBroker().getPersistenceKey());
                dictionaryField = getDataDictionaryByDdxId(ddxId).findDataDictionaryField(prop.getFieldId());
            } else if (field.getAlias(ddxId) != null) {
                dictionaryField = getDataDictionaryByDdxId(ddxId).findDataDictionaryFieldByAlias(field.getAlias(ddxId));
                if (dictionaryField == null) {
                    throw new IllegalStateException("Reference Code Alias " + field.getAlias(ddxId) + " not found");
                }
                if (!dictionaryField.getDataTable().getDataClass().equals(ReferenceCode.class)) {
                    dictionaryField = null;
                    logError("Alias " + field.getAlias(ddxId) + " is not on ReferenceCode");
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
        private String getRawField(CellField keyField, List<String> cells) {
            String value = null;
            int index = 0;
            for (CellField field : m_fields) {
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
         * Inits the special conditions ref table.
         */
        private void initSpecialConditionsRefTable() {
            DataDictionaryField field =
                    getDataDictionaryField(CellField.SCHOOL_SPECIAL_CONDITIONS, DDX_ID_REF_BSID_SCHOOL);
            if (field.hasReferenceTable()) {
                m_specialConditionsRtbOid = field.getReferenceTableOid();
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, m_specialConditionsRtbOid);
                BeanQuery query = new BeanQuery(ReferenceCode.class, criteria);
                try (QueryIterator iterator = getBroker().getIteratorByQuery(query)) {
                    while (iterator.hasNext()) {
                        ReferenceCode bean = (ReferenceCode) iterator.next();
                        m_mapSpecialConditionsRefCodesByCode.put(bean.getCode(), bean);
                        m_mapSpecialConditionsRefCodesByDescription.put(bean.getDescription(), bean);
                    }
                }
            } else {
                throw new IllegalStateException(
                        "Field [" + CellField.SCHOOL_SPECIAL_CONDITIONS.getAlias(DDX_ID_REF_BSID_SCHOOL)
                                + "] must have reference table");
            }
        }

        /**
         * Sets the raw field.
         *
         * @param refCode ReferenceCode
         * @param field FIELDS
         * @param cells List<String>
         * @param ddxId String
         * @param changedValues Map<String, String>
         */
        private void setRawField(ReferenceCode refCode,
                                 CellField field,
                                 List<String> cells,
                                 String ddxId,
                                 Map<String, String> changedValues) {
            String value = getField(field, cells);
            setRawField(refCode, field, value, ddxId, changedValues);
        }

        /**
         * Sets the raw field.
         *
         * @param refCode ReferenceCode
         * @param field FIELDS
         * @param value String
         * @param ddxId String
         * @param changedValues Map<String, String>
         */
        private void setRawField(ReferenceCode refCode,
                                 CellField field,
                                 String value,
                                 String ddxId,
                                 Map<String, String> changedValues) {
            DataDictionaryField dictionaryField = getDataDictionaryField(field, ddxId);
            if (dictionaryField != null) {
                String oldValue = "" + refCode.getFieldValueByBeanPath(dictionaryField.getJavaName());
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
                    }
                    if (date != null) {
                        value = m_dateConverter.getSystemString(date);
                    }
                } else {
                    logError("No converter for " + value + " to field " + field);
                }
                refCode.setFieldValueByBeanPath(dictionaryField.getJavaName(), value);
                String newValue = "" + refCode.getFieldValueByBeanPath(dictionaryField.getJavaName());
                addIfDifferent(changedValues, field.toString(), oldValue, newValue);
            }
        }
    }

    /**
     * Class to process input file record.
     *
     * @author Follett Software Company
     * @copyright 2019
     */
    private class ReferenceCodeRecord {
        private List<String> m_cells;

        /**
         * Create record object from lineNumber and list of field values.
         *
         * @param cells List<String>
         */
        public ReferenceCodeRecord(List<String> cells) {
            m_cells = cells;
        }

        /**
         * Process a row from import file.
         *
         * @param refTable ReferenceTable
         * @param keyCellField CellField
         * @return Result
         */
        public Result processForRefTable(ReferenceTable refTable, CellField keyCellField) {
            String codeValue = getField(keyCellField);
            String ddxId = m_refTablesDdxIds.get(refTable.getOid());
            if (ddxId == null) {
                ExtendedDataDictionary dictionary = refTable.getExtendedDataDictionary();
                if (dictionary == null) {
                    throw new RuntimeException(
                            "Cannot find extended dictionary for reference table " + refTable.getUserName());
                }
                ddxId = refTable.getExtendedDataDictionary().getId();
                m_refTablesDdxIds.put(refTable.getOid(), ddxId);
            }

            if (!validate(keyCellField)) {
                return Result.SKIPPED;
            }
            ReferenceCode code = m_existingCodes.get(refTable.getOid()).get(codeValue);

            if (code != null) {
                if (updateCode(code, ddxId, keyCellField)) {
                    return Result.UPDATED;
                }
            } else {
                code = insertCode(refTable.getOid(), ddxId, keyCellField);
                if (code != null) {
                    return Result.INSERTED;
                }
            }
            return Result.SKIPPED;
        }

        /**
         * Gets the field.
         *
         * @param field FIELDS
         * @return String
         */
        private String getField(CellField field) {
            return getMapper().getField(field, m_cells);
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
         * Insert new reference code.
         *
         * @return ReferenceCode or null
         */
        private ReferenceCode insertCode(String rtbOid, String ddxId, CellField keyField) {
            ReferenceCode refCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
            refCode.setReferenceTableOid(rtbOid);
            Map<String, String> changedValues = new LinkedHashMap<>();
            getMapper().setFields(refCode, m_cells, ddxId, keyField, changedValues);
            refCode.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
            refCode.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            boolean saveOK = saveBean(refCode);
            if (saveOK) {
                m_existingCodes.get(rtbOid).put(refCode.getCode(), refCode);
                logResult("Insert", refCode, changedValues);
                return refCode;
            }
            return null;
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
         * Update existing reference code.
         *
         * @param refCode ReferenceCode
         * @return success flag
         */
        private boolean updateCode(ReferenceCode refCode, String ddxId, CellField keyField) {
            if (refCode.isDirty()) {
                return false;
            }
            boolean isUpdated = false;
            Map<String, String> changedValues = new LinkedHashMap<>();
            getMapper().setFields(refCode, m_cells, ddxId, keyField, changedValues);
            logResult("Update", refCode, changedValues);
            isUpdated = refCode.isDirty();
            if (isUpdated) {
                isUpdated = saveBean(refCode);
            }
            return isUpdated;
        }

        /**
         * Validate fields values.
         *
         * @return true, if all the fields are valid
         */
        private boolean validate(CellField keyCellField) {
            boolean valid = false;
            if (StringUtils.isEmpty(getField(keyCellField))) {
                logError("Key field " + keyCellField + " is empty", m_lineNumber);
            } else {
                valid = true;
            }
            return valid;
        }
    }

    private static final String ALIAS_BSID_BOARD_NUMBER = "rcd-bsid-board-number";
    private static final String ALIAS_SCHOOL_NUMBER = "all-enr-EntryDemitBsid";
    private static final String DDX_ID_REF_BSID_BOARD = "REF-Board-BSID";
    private static final String DDX_ID_REF_BSID_SCHOOL = "REF-School-BSID";
    private static final String FILE_TYPE_INTERNATIONAL = "INTERNATIONAL";
    private static final String FILE_TYPE_PRIVATE = "PRIVATE";
    private static final String FILE_TYPE_PUBLIC = "PUBLIC";
    private static final String INPUT_PARAM_COMMIT = "commit";
    private static final String INPUT_PARAM_FILE_TYPE = "filetype";
    private static final Pattern PATTERN_ALIAS = Pattern.compile("\\[(.*)\\]");
    private static final Pattern PATTERN_PO_BOX = Pattern.compile("^[0-9A-Z, ]+$");
    private static final Pattern PATTERN_EXCEL_DATE = Pattern.compile("^(\\d{5})(.\\d*)?$");
    private static final Pattern PATTERN_SUITE = Pattern.compile("^[0-9A-Z, ]+[-&]?[0-9A-Z, ]*$");
    private static final String STRING_ALIAS_BEGIN = "[";
    private static final String STRING_ALIAS_END = "]";

    private String m_beanPathRcdDescription;
    private Integer m_boardUpdateCount = 0;
    private Integer m_boardInsertCount = 0;
    private Integer m_boardSkipCount = 0;
    private boolean m_commitChanges;
    private List<String> m_errors = new LinkedList<>();
    private List<String> m_messages = new LinkedList<>();
    private Map<String, Map<String, ReferenceCode>> m_existingCodes = new HashMap<>();
    private FieldMapper m_fieldMapper;
    private int m_lineNumber;
    private Map<String, ReferenceTable> m_refTablesByAlias = new HashMap<String, ReferenceTable>();
    private Map<String, String> m_refTablesDdxIds = new HashMap<String, String>();

    /**
     * After import data.
     *
     * @param file File
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#afterImportData(java.io.File)
     */
    @Override
    protected void afterImportData(File file) {
        super.afterImportData(file);
        if (m_commitChanges) {
            for (String newCode : getMapper().getNewSpecialConditions()) {
                ReferenceCode refCode = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
                refCode.setReferenceTableOid(getMapper().getSpecialConditionsRtbOid());
                refCode.setCode(newCode);
                refCode.setOwnerOid(OrganizationManager.ROOT_ORGANIZATION);
                refCode.setOwnerType(Ownable.OWNER_TYPE_ORG1);
                List<ValidationError> errors;
                errors = getBroker().saveBean(refCode);
                if (errors != null && !errors.isEmpty()) {
                    for (ValidationError error : errors) {
                        logError("Validation error for code " + refCode.getCode() + ": " + error.toString(),
                                m_lineNumber);
                    }
                }
            }
        }
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
        }
        results.append(m_commitChanges ? "Commit Mode." : "Review Mode.");
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
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#getImportStatistics()
     */
    @Override
    protected StringBuilder getImportStatistics() {
        String sourceFileName = ((File) getParameter(FILE_KEY)).getName();

        MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        StringBuilder buffer = new StringBuilder(256);

        buffer.append(resources.getMessage(getLocale(), "message.import.results"));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.doubleRule"));
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.fileName", sourceFileName));

        buffer.append('\n');
        buffer.append('\n');
        buffer.append(
                "Statistics for " + getRefTableByAlias(ALIAS_BSID_BOARD_NUMBER, DDX_ID_REF_BSID_SCHOOL).getUserName());
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.updateCount", m_boardUpdateCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.insertCount", m_boardInsertCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.skipCount", m_boardSkipCount));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.rule", m_boardSkipCount));
        buffer.append('\n');
        buffer.append(
                resources.getMessage(getLocale(), "message.import.results.total",
                        m_boardInsertCount + m_boardSkipCount + m_boardUpdateCount));

        buffer.append('\n');
        buffer.append('\n');
        buffer.append("Statistics for " + getRefTableByAlias(ALIAS_SCHOOL_NUMBER).getUserName());
        buffer.append('\n');
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.updateCount", getUpdateCount()));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.insertCount", getInsertCount()));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.skipCount", getSkipCount()));
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.rule", getSkipCount()));
        buffer.append('\n');
        buffer.append(
                resources.getMessage(getLocale(), "message.import.results.total",
                        getInsertCount() + getSkipCount() + getUpdateCount()));
        buffer.append('\n');

        if (!getInvalidRecords().isEmpty()) {
            buffer.append('\n');
            buffer.append(resources.getMessage(getLocale(), "message.import.results.invalidHeader", getSkipCount()));
            buffer.append('\n');
            buffer.append(resources.getMessage(getLocale(), "message.import.results.invalidRule", getSkipCount()));
            buffer.append('\n');
            buffer.append('\n');

            for (KeyValuePair<Integer, String> invalidRecord : getInvalidRecords()) {
                String message = resources.getMessage(getLocale(),
                        "message.import.results.invalidRecord",
                        invalidRecord.getKey(),
                        invalidRecord.getValue());
                buffer.append(message);
                buffer.append('\n');
            }
        }

        return buffer;
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
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        getBroker().beginTransaction();
        boolean exception = false;
        try {
            super.importData(sourceFile);
        } catch (Exception e) {
            exception = true;
            throw e;
        } finally {
            if (exception || !m_commitChanges) {
                getBroker().rollbackTransaction();
                clearDirtyMembers(getRefTableByAlias(ALIAS_SCHOOL_NUMBER).getOid());
                clearDirtyMembers(getRefTableByAlias(ALIAS_BSID_BOARD_NUMBER, DDX_ID_REF_BSID_SCHOOL).getOid());
            } else {
                getBroker().commitTransaction();
            }
        }

    }

    /**
     * Import record.
     *
     * @param cells List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> cells, int lineNumber) throws Exception {
        m_lineNumber = lineNumber;
        if (areAllCellsWithData(cells)) {
            ReferenceCodeRecord record = new ReferenceCodeRecord(cells);

            ReferenceTable boardRefTable = getRefTableByAlias(ALIAS_BSID_BOARD_NUMBER, DDX_ID_REF_BSID_SCHOOL);
            Result boardRecordResult = record.processForRefTable(boardRefTable, CellField.BOARD_NUMBER);
            if (boardRecordResult == Result.UPDATED) {
                m_boardUpdateCount++;
            } else if (boardRecordResult == Result.INSERTED) {
                m_boardInsertCount++;
            } else {
                m_boardSkipCount++;
            }

            ReferenceTable schoolRefTable = getRefTableByAlias(ALIAS_SCHOOL_NUMBER);
            Result schoolRecordResult = record.processForRefTable(schoolRefTable, CellField.SCHOOL_NUMBER);
            if (schoolRecordResult == Result.UPDATED) {
                incrementUpdateCount();
            } else if (schoolRecordResult == Result.INSERTED) {
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

        loadBeanPath();
        loadExistingCodesForRefTable(getRefTableByAlias(ALIAS_SCHOOL_NUMBER).getOid());
        loadExistingCodesForRefTable(getRefTableByAlias(ALIAS_BSID_BOARD_NUMBER, DDX_ID_REF_BSID_SCHOOL).getOid());
    }

    /**
     * Clear dirty members.
     *
     * @param rtbOid String
     */
    private void clearDirtyMembers(String rtbOid) {
        m_existingCodes.get(rtbOid).entrySet().stream()
                .map(Entry::getValue)
                .forEach(refCode -> refCode.getDirtyMembers().clear());
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
     * Output errors to the results string.
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
     * Gets the current line number.
     *
     * @return int
     */
    private int getCurrentLineNumber() {
        return m_lineNumber;
    }

    /**
     * Gets the data dictionary by ddx id.
     *
     * @param ddxId String
     * @return Data dictionary
     */
    private DataDictionary getDataDictionaryByDdxId(String ddxId) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
        BeanQuery query = new BeanQuery(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary ddx = getBroker().getBeanByQuery(query);
        return DataDictionary.getDistrictDictionary(ddx, getBroker().getPersistenceKey());
    }

    /**
     * Find reference table.
     *
     * @return ReferenceTable
     */
    private ReferenceTable getRefTableByAlias(String alias) {
        return getRefTableByAlias(alias, null);
    }

    /**
     * Find reference table.
     *
     * @return ReferenceTable
     */
    private ReferenceTable getRefTableByAlias(String alias, String ddxId) {
        ReferenceTable refTable = m_refTablesByAlias.get(alias);
        if (refTable == null) {
            DataDictionary dictionary =
                    StringUtils.isEmpty(ddxId) ? DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey())
                            : getDataDictionaryByDdxId(ddxId);
            DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
            refTable = field.getReferenceTable();
            m_refTablesByAlias.put(alias, refTable);
        }
        return refTable;
    }

    /**
     * Find the Reference Code description bean path.
     */
    private void loadBeanPath() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty property = new ModelProperty(ReferenceCode.class, ReferenceCode.COL_DESCRIPTION, dictionary);
        m_beanPathRcdDescription = property.getDictionaryPath();
    }

    private void loadExistingCodesForRefTable(String refTableOid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, refTableOid);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_existingCodes.put(refTableOid, getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 4096));
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
    private boolean areAllCellsWithData(List<String> fields) {
        boolean found = false;
        for (int i = 0; !found && i < fields.size(); i++) {
            found = !StringUtils.isEmpty(fields.get(i));
        }
        return found;
    }
}
