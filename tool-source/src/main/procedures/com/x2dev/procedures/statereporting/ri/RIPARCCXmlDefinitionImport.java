/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.x2dev.procedures.statereporting.PARCCXmlDefinitionImport;
import com.x2dev.sis.model.beans.AssessmentColumnDefinition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 * MD Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class RIPARCCXmlDefinitionImport extends PARCCXmlDefinitionImport {
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
        if (periodField == null) {
            periodField = ALIAS_PERIOD_FIELD;
        }
        String sasidField = (String) getParameter(PARAM_SASID_FIELD);
        String schoolYearField = (String) getParameter(PARAM_SCHOOL_YEAR_FIELD);
        if (schoolYearField == null) {
            schoolYearField = ALIAS_SCHOOL_YEAR_FIELD;
        }
        String testCodeField = (String) getParameter(PARAM_TEST_CODE_FIELD);
        if (testCodeField == null) {
            testCodeField = ALIAS_TEST_CODE_FIELD;
        }
        PlainDate testDateField = (PlainDate) getParameter(PARAM_TEST_DATE_FIELD);
        String testDateFormat = (String) getParameter(PARAM_TEST_DATE_FORMAT);
        if (testDateField == null || StringUtils.isEmpty(testDateFormat)) {
            m_initErrors.add("Assessment date or date format is not found");
        }

        // value is either student assessment oid or assessment column definition
        Map<String, Object> acdMap = loadAliasColumns(dictionary, aliasPrefix);
        verifyRequiredAlias(acdMap, ALIAS_TEST_CODE_FIELD);
        List<String> record = readInputHeading();
        verifyRequiredField(record, sasidField, PARAM_SASID_FIELD);
        verifyRequiredField(record, schoolYearField, PARAM_SCHOOL_YEAR_FIELD);
        verifyRequiredField(record, periodField, PARAM_PERIOD_FIELD);
        verifyRequiredField(record, testCodeField, PARAM_TEST_CODE_FIELD);

        if (m_initErrors.isEmpty()) {
            xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            xml.append("<import table-id=\"tblStdAssess\" mode=\"both\" force=\"false\" value-wrapper=\"\">");
            xml.append("<preset-fields>");
            xml.append("<preset-field id=\"asmAsdOID\" source=\"input\" value=\"asdOid\" match=\"true\" />");

            xml.append("<preset-field id=\"asmDate\"  source=\"input\" value=\"" + PARAM_TEST_DATE_FIELD + "\" ");
            if (!StringUtils.isEmpty(testDateFormat)) {
                xml.append("format=\"" + testDateFormat + "\" ");
            }
            xml.append("/>");

            xml.append("</preset-fields>");
            xml.append("<direct-fields>");
            // add key fields for find existing record
            xml.append(
                    "<direct-field id=\"relAsmStdOid.stdIDState\" match=\"true\" source-id=\"" + sasidField + "\" />");
            xml.append("<direct-field id=\"" + getAliasField(acdMap, ALIAS_TEST_CODE_FIELD)
                    + "\" match=\"true\" source-id=\""
                    + testCodeField + "\" />");
            xml.append("<direct-field id=\"" + getAliasField(acdMap, ALIAS_SCHOOL_YEAR_FIELD)
                    + "\" match=\"true\" source-id=\""
                    + schoolYearField + "\" />");
            xml.append("<direct-field id=\"" + getAliasField(acdMap, ALIAS_PERIOD_FIELD)
                    + "\" match=\"true\" source-id=\"" + periodField + "\" />");
            acdMap.remove(ALIAS_TEST_CODE_FIELD);
            acdMap.remove(ALIAS_SCHOOL_YEAR_FIELD);
            acdMap.remove(ALIAS_PERIOD_FIELD);

            if (m_initErrors.isEmpty()) {
                DataDictionaryField importFormatField = dictionary.findDataDictionaryFieldByAlias(ALIAS_IMPORT_FORMAT);

                for (Entry<String, Object> entry : acdMap.entrySet()) {
                    if (record.contains(entry.getKey())) {
                        String columnValue = getAliasField(acdMap, entry.getKey());
                        m_initMessages.add("Input field " + entry.getKey()
                                + " is mapped to student assessment column " + columnValue);
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
}
