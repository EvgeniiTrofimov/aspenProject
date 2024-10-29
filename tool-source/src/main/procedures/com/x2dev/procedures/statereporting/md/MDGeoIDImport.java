/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class MDGeoIDImport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class MDGeoIDImport extends TextImportJavaSource {

    /**
     * Constants
     */
    private static final String ALIAS_ADR_GEO_ID = "all-adr-GeolocationID";
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    private static final String INPUT_INDEX_GEO_END = "geoIdIndexEnd";
    private static final String INPUT_INDEX_GEO_START = "geoIdIndexStart";
    private static final String INPUT_INDEX_SASID_END = "sasidIndexEnd";
    private static final String INPUT_INDEX_SASID_START = "sasidIndexStart";

    /**
     * Members
     */
    private DataDictionary m_dictionary;
    private List<String> m_errors = new LinkedList();
    private String m_fieldAdrGeoID;
    private int m_indexGeoIdEnd;
    private int m_indexGeoIdStart;
    private int m_indexSasidEnd;
    private int m_indexSasidStart;
    private List<String> m_setupErrors = new LinkedList();

    /**
     * Display only init errors if exit. Otherwise, add init messages to default messages.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        buffer.append('\n');
        buffer.append(getImportStatistics().toString());
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
        if (m_setupErrors.isEmpty()) {
            if (!m_errors.isEmpty()) {
                m_errors.add("\n");
                exportList(m_errors);
            }
        } else {
            exportList(m_setupErrors);
        }
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 1;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldLengths()
     */
    @Override
    protected int[] getFieldLengths() {
        return new int[220];
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        if (m_setupErrors.isEmpty()) {
            super.importData(sourceFile);
        }
    }

    /**
     * Import resource records.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        String wholeLine = record.get(0);
        int lineLength = wholeLine.length();
        if (m_indexSasidStart < lineLength && m_indexSasidEnd < lineLength && m_indexGeoIdStart < lineLength
                && m_indexGeoIdEnd <= lineLength) {
            String stdSasidToFind =
                    wholeLine.substring(m_indexSasidStart - 1, m_indexSasidEnd);
            String stdGeoIdToSet =
                    lineLength == m_indexGeoIdEnd ? wholeLine.substring(m_indexGeoIdStart - 1)
                            : wholeLine.substring(m_indexGeoIdStart - 1, m_indexGeoIdEnd);
            SisStudent student = getStudentBySASID(stdSasidToFind);
            if (student != null) {
                SisAddress stdAddr = student.getPerson().getPhysicalAddress();
                if (!StringUtils.isBlank(stdGeoIdToSet)) {
                    if (stdAddr != null) {
                        stdAddr.setFieldValueByBeanPath(m_fieldAdrGeoID, stdGeoIdToSet);
                        if (stdAddr.isDirty()) {
                            incrementUpdateCount();
                            getBroker().saveBeanForced(stdAddr);
                        } else {
                            addError(lineNumber, "GEO ID matches. SASID = " + stdSasidToFind);
                            incrementSkipCount();
                        }
                    } else {
                        incrementSkipCount();
                        logInvalidRecord(lineNumber,
                                "Student's physical address is not found. SASID = " + stdSasidToFind);
                    }
                } else {
                    incrementSkipCount();
                    logInvalidRecord(lineNumber, "Student's GEO ID is Empty. SASID = " + stdSasidToFind);
                }
            } else {
                incrementSkipCount();
                logInvalidRecord(lineNumber, "Student is not found with SASID = " + stdSasidToFind);
            }
        } else {
            addError(lineNumber,
                    "Line length is less than any input indexex. Line lengt = " + lineLength + ". SASID Start = "
                            + m_indexSasidStart
                            + ". SASID End = " + m_indexSasidEnd
                            + ". GEO ID Start = " + m_indexGeoIdStart
                            + ". GEO ID END = " + m_indexGeoIdEnd);
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
        setUseValueDelimiters(false);
        initializeIndexes();
        m_fieldAdrGeoID = translateAliasToJavaName(ALIAS_ADR_GEO_ID, true);
    }

    /**
     * Splits the line into a list of individual values base on the wrapper and delimiter
     * characters.
     *
     * @param line String
     * @param lineNumber int
     * @return A List of String objects, this value will be null if the line was invalid
     */
    @Override
    protected List<String> splitLine(String line, int lineNumber) {
        return splitFixedLengthLineCustom(line, lineNumber);
    }

    /**
     * Adds one validation error.
     *
     * @param lineNumber int
     * @param errorMessage String
     * @return true, if successful
     */
    private boolean addError(int lineNumber, String errorMessage) {
        m_errors.add("" + lineNumber + ". " + errorMessage);
        return true;
    }

    /**
     * Adds one validation error.
     *
     * @param errorType String
     * @param errorMessage String
     */
    private void addSetupError(String errorType, String errorMessage) {
        m_setupErrors.add(errorType + "-" + errorMessage);
    }

    /**
     * Write a list of strings to the results handler.
     *
     * @param list List<String>
     * @throws X2BaseException exception
     */
    private void exportList(List<String> list) throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        buffer.append('\n');
        if (!list.isEmpty()) {
            buffer.append("Statistics:" + '\n');
        }
        for (String err : list) {
            buffer.append(err);
            buffer.append('\n');
        }
        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Gets the data dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDataDictionary() {
        if (m_dictionary == null) {
            m_dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        }
        return m_dictionary;
    }

    /**
     * Gets the student by SASID.
     *
     * @param sasid String
     * @return Sis student
     */
    private SisStudent getStudentBySASID(String sasid) {
        X2Criteria stdCriteria = new X2Criteria();
        stdCriteria.addEqualTo(SisStudent.COL_STATE_ID, sasid);
        return getBroker().getBeanByQuery(new QueryByCriteria(SisStudent.class, stdCriteria));
    }

    /**
     * Initialize indexes.
     */
    private void initializeIndexes() {
        String sasidStart = (String) getParameter(INPUT_INDEX_SASID_START);
        String sasidEnd = (String) getParameter(INPUT_INDEX_SASID_END);
        String geoStart = (String) getParameter(INPUT_INDEX_GEO_START);
        String geoEnd = (String) getParameter(INPUT_INDEX_GEO_END);

        try {
            m_indexSasidStart = Integer.parseInt(sasidStart);
        } catch (Exception e) {
            m_indexSasidStart = 13;
        }
        try {
            m_indexSasidEnd = Integer.parseInt(sasidEnd);
        } catch (Exception e) {
            m_indexSasidEnd = 22;
        }
        if (m_indexSasidStart >= m_indexSasidEnd || m_indexSasidEnd - m_indexSasidEnd < 9) {
            m_indexSasidStart = 13;
            m_indexSasidEnd = 22;
        }
        try {
            m_indexGeoIdStart = Integer.parseInt(geoStart);
        } catch (Exception e) {
            m_indexGeoIdStart = 207;
        }
        try {
            m_indexGeoIdEnd = Integer.parseInt(geoEnd);
        } catch (Exception e) {
            m_indexGeoIdEnd = 221;
        }
        if (m_indexGeoIdStart >= m_indexGeoIdEnd || m_indexGeoIdEnd - m_indexGeoIdStart < 14) {
            m_indexGeoIdStart = 207;
            m_indexGeoIdEnd = 221;
        }
    }

    /**
     * Splits the line into a list of individual values base on the field lengths. This method
     * ignores the escape, value wrapper, and value-delmiter characters (if any).
     *
     * @param line String
     * @param lineNumber int
     * @return A List of String objects, this value will be null if the line was invalid
     */
    private List splitFixedLengthLineCustom(String line, int lineNumber) {
        List record = new ArrayList(getFieldCount());
        if (!StringUtils.isBlank(line)) {
            record.add(line);
        }
        return record;
    }

    /**
     * Translate alias to java name.
     *
     * @param alias String
     * @param required boolean
     * @return String
     */
    private String translateAliasToJavaName(String alias, boolean required) {
        String javaName = null;
        DataDictionaryField field = getDataDictionary().findDataDictionaryFieldByAlias(alias);
        if (field != null) {
            javaName = field.getJavaName();
        } else if (required) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }
        return javaName;
    }
}
