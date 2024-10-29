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

package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.x2dev.procedures.statereporting.PARCCXmlDefinitionImport;
import com.x2dev.utils.StringUtils;
import java.util.List;
import java.util.Map;

/**
 * MD Assessment export for PARCC - Personal Needs Profile export.
 *
 * @author X2 Development Corporation
 */
public class NJPARCCXmlDefinitionImport extends PARCCXmlDefinitionImport {

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

        // value is either student assessment oid or assessment column definition
        Map<String, Object> acdMap = loadAliasColumns(dictionary, aliasPrefix);

        verifyRequiredAlias(acdMap, ALIAS_SCHOOL_YEAR_FIELD);
        verifyRequiredAlias(acdMap, ALIAS_PERIOD_FIELD);
        verifyRequiredAlias(acdMap, ALIAS_TEST_CODE_FIELD);

        List<String> record = readInputHeading();
        verifyRequiredField(record, sasidField, PARAM_SASID_FIELD);
        verifyRequiredField(record, schoolYearField, PARAM_SCHOOL_YEAR_FIELD);
        verifyRequiredField(record, periodField, PARAM_PERIOD_FIELD);
        verifyRequiredField(record, testCodeField, PARAM_TEST_CODE_FIELD);

        if (m_initErrors.isEmpty()) {
            xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            xml.append(
                    "<import table-id=\"tblStdAssess\" mode=\"both\" force=\"false\" value-wrapper=\"&quot;\" value-wrapping-mode=\"optional\">");
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

            if (m_initErrors.isEmpty()) {
                String additionalFields = addAdditionalImportFields(dictionary, record, acdMap);
                if (!StringUtils.isEmpty(additionalFields)) {
                    xml.append(additionalFields);
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
