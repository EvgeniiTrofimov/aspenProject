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

package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.PARCCXmlDefinitionImport;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.utils.StringUtils;
import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * MD Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class MDPARCCXmlDefinitionImport extends PARCCXmlDefinitionImport {

    /**
     * Initialize the XML definition from the assessment definition and input file header.
     *
     * @param dictionary DataDictionary
     * @return String
     */
    @Override
    protected String initFromHeader(DataDictionary dictionary) {
        String aliasPrefix = "";
        if (getParameter(PARAM_ALIAS_PREFIX) != null) {
            aliasPrefix = (String) getParameter(PARAM_ALIAS_PREFIX);
        }

        StringBuilder xml = new StringBuilder();

        String periodField = (String) getParameter(PARAM_PERIOD_FIELD);
        String sasidField = (String) getParameter(PARAM_SASID_FIELD);
        String schoolYearField = (String) getParameter(PARAM_SCHOOL_YEAR_FIELD);
        String testCodeField = (String) getParameter(PARAM_TEST_CODE_FIELD);
        String testDateField = (String) getParameter(PARAM_TEST_DATE_FIELD);
        String testDateFormat = (String) getParameter(PARAM_TEST_DATE_FORMAT);
        String testSchoolField = (String) getParameter(PARAM_TEST_SCHOOL_FIELD);
        String testSchoolMatchingField = (String) getParameter(PARAM_TEST_SCHOOL_MATCHING_FIELD);

        // value is either student assessment oid or assessment column definition
        Map<String, Object> acdMap = loadAliasColumns(dictionary, aliasPrefix);

        verifyRequiredAlias(acdMap, ALIAS_SCHOOL_YEAR_FIELD);
        verifyRequiredAlias(acdMap, ALIAS_PERIOD_FIELD);
        verifyRequiredAlias(acdMap, ALIAS_TEST_CODE_FIELD);

        List<String> record = readInputHeading();
        if (record == null) {
            m_initErrors.add("File contains incorrect heading rows.");
        } else {
            verifyRequiredField(record, sasidField, PARAM_SASID_FIELD);
            verifyRequiredField(record, schoolYearField, PARAM_SCHOOL_YEAR_FIELD);
            verifyRequiredField(record, periodField, PARAM_PERIOD_FIELD);
            verifyRequiredField(record, testCodeField, PARAM_TEST_CODE_FIELD);
        }

        if (m_initErrors.isEmpty()) {
            xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            xml.append("<import table-id=\"tblStdAssess\" mode=\"both\" force=\"false\" value-wrapper=\"\">");
            xml.append("<preset-fields>");
            xml.append("<preset-field id=\"asmAsdOID\" source=\"input\" value=\"asdOid\" match=\"true\" />");
            xml.append("</preset-fields>");
            xml.append("<direct-fields>");
            xml.append(
                    "<direct-field id=\"relAsmStdOid.stdIDState\" match=\"true\" source-id=\"" + sasidField + "\" />");
            xml.append("<direct-field id=\"" + getAliasField(acdMap, "PARCCTSTYEAR") + "\" match=\"true\" source-id=\""
                    + schoolYearField + "\" />");
            xml.append("<direct-field id=\"" + getAliasField(acdMap, "PARCCTSTPERIOD")
                    + "\" match=\"true\" source-id=\"" + periodField + "\" />");
            xml.append("<direct-field id=\"" + getAliasField(acdMap, "PARCCTSTCODE") + "\" match=\"true\" source-id=\""
                    + testCodeField + "\" />");

            // Optional fields from Input Definition
            if (!StringUtils.isEmpty(testDateField)) {
                xml.append("<direct-field id=\"asmDate\" source-id=\"" + testDateField + "\" ");
                if (!StringUtils.isEmpty(testDateFormat)) {
                    xml.append("format=\"" + testDateFormat + "\" ");
                }
                xml.append("/>");
            }
            if (!StringUtils.isEmpty(testSchoolField) && !StringUtils.isEmpty(testSchoolMatchingField)) {
                // test if bean path is alias

                Pattern p = Pattern.compile(".*\\[(.*)\\].*");
                Matcher m = p.matcher(testSchoolMatchingField);

                if (m.find()) {
                    String alias = m.group(1);
                    DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(alias);
                    if (field == null) {
                        m_initErrors.add("Alias for school not found - " + alias);
                    } else {
                        testSchoolMatchingField = field.getDataFieldConfig().getDataFieldOid();
                    }
                }
                xml.append("<direct-field id=\"relAsmSklOid." + testSchoolMatchingField + "\" source-id=\""
                        + testSchoolField + "\" format=\"0000\" formatter=\"decimal\" />");
            }

            if (m_initErrors.isEmpty()) {
                DataDictionaryField importFormatField = dictionary.findDataDictionaryFieldByAlias(ALIAS_IMPORT_FORMAT);

                for (Entry<String, Object> entry : acdMap.entrySet()) {
                    if (record.contains(entry.getKey())) {
                        String columnValue = getAliasField(acdMap, entry.getKey());
                        m_initMessages.add("Input field " + entry.getKey() + " is mapped to student assessment column "
                                + columnValue);
                        xml.append("<direct-field id=\"");
                        xml.append(columnValue);
                        xml.append("\" source-id=\"");
                        xml.append(entry.getKey());
                        xml.append("\"");
                        if (entry.getValue() instanceof AssessmentColumnDefinition && importFormatField != null
                                && !StringUtils.isEmpty(importFormatField.getJavaName())) {
                            String format = (String) ((AssessmentColumnDefinition) entry.getValue())
                                    .getFieldValueByBeanPath(importFormatField.getJavaName());
                            if (!StringUtils.isEmpty(format)) {
                                xml.append(" format=\"");
                                xml.append(format);
                                xml.append("\"");
                            }
                        }
                        xml.append(" />");
                    }
                }

                xml.append("</direct-fields>");
                xml.append("</import>");
                if (m_debug) {
                    m_initMessages.add(xml.toString());
                }
                return xml.toString();
            }
        }
        return null;
    }

    /**
     * Only import file if there are no initialization errors.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        if (m_initErrors.isEmpty()) {
            super.importData(sourceFile);
        }
        String initMessageError = !m_initErrors.isEmpty() ? parseErrors(m_initErrors) : "";
        String initMessage = !m_initMessages.isEmpty() ? parseErrors(m_initMessages) : "";
        String importMessage = super.getImportStatistics() != null
                ? super.getImportStatistics().toString()
                : "";
        String finalMessage =
                initMessageError + System.lineSeparator() + initMessage + System.lineSeparator() + importMessage;
        logToolMessage(Level.INFO, finalMessage, false);
    }

    /**
     * Parse errors and messages to string
     *
     * @param list
     * @return String
     */
    private String parseErrors(List<String> list) {
        StringBuilder buffer = new StringBuilder(256);
        for (String err : list) {
            buffer.append(err);
            buffer.append('\n');
        }
        return buffer.toString();
    }
}
