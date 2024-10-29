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
package com.x2dev.procedures.statereporting.ri;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.DateAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Tool for importing three fields from Compu Claim Export to Aspen.
 *
 * Imported fields:
 * 1)Medicaid Eligible
 * 2)Parental Consent Received
 * 3)Parental Consent Date
 *
 * Only records with active status are used.
 *
 * Match on Student State ID.
 *
 * @author X2 Development Corporation
 */
public class CompuClaimImport extends TextImportJavaSource {
    private static final String ALIAS_MEDICAID_ELIGIBLE = "medicaid-eligible";
    private static final String ALIAS_MEDICAID_PARENT_CONSENT = "medicaid-parent-consent";
    private static final String ALIAS_MEDICAID_CONSENT_DATE = "medicaid-consent-date";

    private static final int DEFAULT_VALUE_SIZE = 16;

    private static final String FIELD_COUNT_PARAM = "fieldCount";

    private static final String PARAM_SASID_INDEX = "sasidIndex";
    private static final String PARAM_MEDICAID_ELIGIBLE_INDEX = "medicaidEligibleIndex";
    private static final String PARAM_PARENT_CONSENT_INDEX = "parentConsentIndex";
    private static final String PARAM_CONSENT_DATE_INDEX = "consentDateIndex";

    private int m_consentDateIndex;
    private DateAsStringConverter m_converter;
    private DateFormat m_dateFormat;
    private int m_fieldCount;
    private int m_medicaidEligIndex;
    private int m_parentConsentIndex;
    private int m_sasidIndex;

    private Map<ExtendedDictionaryAttributes, DataDictionary> m_dictionaryMap = new HashMap();

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return m_fieldCount;
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
        BufferedReader reader = null;

        try {
            if (getCharacterEncoding() != null && Charset.isSupported(getCharacterEncoding())) {
                reader = new BufferedReader(
                        new InputStreamReader(new FileInputStream(sourceFile), getCharacterEncoding()),
                        DEFAULT_VALUE_SIZE * getFieldCount());
            } else {
                reader = new BufferedReader(new FileReader(sourceFile), DEFAULT_VALUE_SIZE * getFieldCount());
            }
        } catch (FileNotFoundException fnfe) {
            logInvalidRecord(0, "error.file.edit.notFound", sourceFile.getAbsolutePath());
        }

        if (reader != null) {
            try {
                // Skip the header.
                reader.readLine();
                int lineNumber = 2;

                while (reader.ready()) {
                    List<String> record = splitLine(reader.readLine(), lineNumber);
                    if (record != null) {
                        if (record.size() == getFieldCount()) {
                            importRecord(record, lineNumber);
                        } else {
                            logInvalidRecord(lineNumber, "error.import.text.valueCount");
                            incrementSkipCount();
                        }
                    } else {
                        incrementSkipCount();
                    }

                    lineNumber++;
                }
            } finally {
                reader.close();
            }
        }
    }

    /**
     * Import record.
     *
     * @param record List
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List record, int lineNumber) throws Exception {
        String sasid = (String) record.get(m_sasidIndex);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.COL_STATE_ID, sasid);
        criteria.addEqualTo(IepData.COL_STATUS_CODE, Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()));

        if (isSchoolContext()) {
            criteria.addEqualTo(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    SisStudent.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            criteria.addEqualTo(IepData.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                    SisStudent.COL_ORGANIZATION1_OID, getOrganization().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(IepData.class, criteria);

        IepData iep = (IepData) getBroker().getBeanByQuery(query);
        if (iep != null) {
            DataDictionary dictionary = getDictionary(iep.getExtendedDataDictionary());
            incrementMatchCount();

            // Set "Medicaid Eligible"
            if (!StringUtils.isEmpty((String) record.get(m_medicaidEligIndex))) {
                String importedValue = record.get(m_medicaidEligIndex).equals("Y") ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;

                iep.setFieldValueByAlias(ALIAS_MEDICAID_ELIGIBLE, importedValue, dictionary);
            }

            // Set "Parental Consent Received"
            if (!StringUtils.isEmpty((String) record.get(m_parentConsentIndex))) {
                String importedValue = record.get(m_parentConsentIndex).equals("Y") ? BooleanAsStringConverter.TRUE
                        : BooleanAsStringConverter.FALSE;

                iep.setFieldValueByAlias(ALIAS_MEDICAID_PARENT_CONSENT, importedValue, dictionary);
            }

            // Set "Parental Consent Date"
            if (!StringUtils.isEmpty((String) record.get(m_consentDateIndex))) {
                PlainDate date = new PlainDate(m_dateFormat.parse((String) record.get(m_consentDateIndex)));

                iep.setFieldValueByAlias(ALIAS_MEDICAID_CONSENT_DATE, m_converter.getSystemString(date), dictionary);
            }

            if (iep.isDirty()) {
                getBroker().saveBeanForced(iep);
                incrementUpdateCount();
            }
        } else {
            incrementSkipCount();
            logInvalidRecord(lineNumber, "No matching student with active IEP for SASID: " + sasid);
        }
    }

    /**
     * Gets the dictionary.
     *
     * @param extendedDataDictionary ExtendedDictionaryAttributes
     * @return Data dictionary
     */
    private DataDictionary getDictionary(ExtendedDictionaryAttributes extendedDataDictionary) {
        DataDictionary dictionary;
        if (m_dictionaryMap.keySet().contains(extendedDataDictionary)) {
            dictionary = m_dictionaryMap.get(extendedDataDictionary);
        } else {
            dictionary = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
            m_dictionaryMap.put(extendedDataDictionary, dictionary);
        }
        return dictionary;
    }

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() {
        m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
        m_converter = (DateAsStringConverter) ConverterFactory.getConverterForClass(PlainDate.class.getName(),
                Locale.getDefault(),
                true);

        setValueDelimiter('\t');

        /*
         * How many fields are in each record?
         */
        m_fieldCount = ((Integer) getParameter(FIELD_COUNT_PARAM)).intValue();

        m_sasidIndex = ((Integer) getParameter(PARAM_SASID_INDEX)).intValue() - 1;
        m_consentDateIndex = ((Integer) getParameter(PARAM_CONSENT_DATE_INDEX)).intValue() - 1;
        m_medicaidEligIndex = ((Integer) getParameter(PARAM_MEDICAID_ELIGIBLE_INDEX)).intValue() - 1;
        m_parentConsentIndex = ((Integer) getParameter(PARAM_PARENT_CONSENT_INDEX)).intValue() - 1;
    }
}
