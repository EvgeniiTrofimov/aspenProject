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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class FLEducationIdentifierImport.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class FLEducationIdentifierImport extends TextImportJavaSource {

    /**
     * The Class Definition.
     */
    class Definition {
        private int[] m_arrFieldLengths;
        private ExportFormatDefinition m_bean;
        private List<Integer> m_listFieldLengths = new LinkedList();
        private Map<String, Integer> m_mapColumnNameIndex = new HashMap();

        /**
         * Instantiates a new definition.
         *
         * @param procedureId String
         */
        public Definition(String procedureId) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatDefinition.COL_PROCEDURE_ID, procedureId);
            BeanQuery query = new BeanQuery(ExportFormatDefinition.class, criteria);
            m_bean = (ExportFormatDefinition) getBroker().getBeanByQuery(query);
            if (m_bean != null) {
                criteria = new X2Criteria();
                criteria.addEqualTo(ExportFormatField.COL_DEFINITION_OID, m_bean.getOid());
                query = new BeanQuery(ExportFormatField.class, criteria);
                query.addOrderByAscending(ExportFormatField.COL_POSITION);
                QueryIterator iterator = getBroker().getIteratorByQuery(query);

                try {
                    int index = 0;
                    while (iterator.hasNext()) {
                        ExportFormatField field = (ExportFormatField) iterator.next();
                        m_mapColumnNameIndex.put(field.getName(), Integer.valueOf(index++));
                        m_listFieldLengths.add(Integer.valueOf(field.getMaximumLength()));
                    }
                } finally {
                    iterator.close();
                }
                m_arrFieldLengths = new int[m_listFieldLengths.size()];
                int index = 0;
                for (Integer item : m_listFieldLengths) {
                    m_arrFieldLengths[index++] = item.intValue();
                }

                m_listFieldLengths.toArray();
            } else {
                m_initErrors.add("Export Format Definition for procedure id " + procedureId + " could not be found");
            }

        }

        /**
         * Gets the field count.
         *
         * @return int
         */
        public int getFieldCount() {
            return m_arrFieldLengths.length;
        }

        /**
         * Gets the field index.
         *
         * @param fieldName String
         * @return int
         */
        public int getFieldIndex(String fieldName) {
            int index = -1;
            Integer fieldIndex = m_mapColumnNameIndex.get(fieldName);
            if (fieldIndex != null) {
                index = fieldIndex.intValue();
            }
            return index;
        }

        /**
         * Gets the field lengths.
         *
         * @return int[]
         */
        public int[] getFieldLengths() {
            return m_arrFieldLengths;
        }
    }

    /**
     * The Class OutputMessage.
     */
    private class OutputMessage {
        private int m_lineNumber;
        private String m_message;
        private String m_recordInfo;

        /**
         * Instantiates a new output message.
         *
         * @param lineNumber int
         * @param recordInfo String
         * @param message String
         */
        private OutputMessage(int lineNumber, String recordInfo, String message) {
            super();
            this.m_lineNumber = lineNumber;
            this.m_recordInfo = recordInfo;
            this.m_message = message;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            StringBuilder value = new StringBuilder();
            value.append("Line # ");
            value.append(m_lineNumber);
            value.append(") ");
            value.append(m_recordInfo);
            value.append(" --- ");
            value.append(m_message);
            return value.toString();
        }
    }

    private static final String ALIAS_FL_EDUCATION_ID = "all-psn-StateEducationId";

    private static final String HEADING_DOB_DAY = "DOB Day";
    private static final String HEADING_DOB_MONTH = "DOB Month";
    private static final String HEADING_DOB_YEAR = "DOB Year";
    private static final String HEADING_FIRST_NAME = "FirstName";
    private static final String HEADING_FLEID_ID = "Florida Education Id";
    private static final String HEADING_LAST_NAME = "LastName";
    private static final String HEADING_LOCAL_ID = "Local Number";
    private static final String HEADING_MIDDLE_NAME = "MiddleName";

    private static final String PROCEDURE_ID = "procedureId";

    private Definition m_definition;
    private List<String> m_initErrors = new LinkedList();
    private int m_matchCount = 0;
    private List<OutputMessage> m_messages = new LinkedList();

    /**
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        if (m_initErrors.isEmpty()) {
            if (!m_messages.isEmpty()) {
                exportList(m_messages);
            }
            List<String> list = new LinkedList();
            list.add("# Records where Local ID and State ID Matched - No updates: " + m_matchCount);
            exportList(list);
        } else {
            exportList(m_initErrors);
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
        return m_definition.getFieldCount();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldLengths()
     */
    @Override
    protected int[] getFieldLengths() {
        return m_definition.getFieldLengths();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        this.setUseValueDelimiters(false);
        String procedureId = (String) getParameter(PROCEDURE_ID);

        if (!StringUtils.isEmpty(procedureId)) {
            m_definition = new Definition(procedureId);
            super.importData(sourceFile);
        } else {
            m_initErrors.add("No procedure id for export format definition found");
        }
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        String idFloridaEducator = getField(record, HEADING_FLEID_ID);
        if (!StringUtils.isEmpty(idFloridaEducator)) {
            String localId = getField(record, HEADING_LOCAL_ID);
            SisStudent student = getStudent(localId);
            if (student != null) {
                String studentStateId = (String) student.getPerson().getFieldValueByAlias(ALIAS_FL_EDUCATION_ID);
                if (StringUtils.isEmpty(studentStateId)) {
                    student.getPerson().setFieldValueByAlias(ALIAS_FL_EDUCATION_ID, idFloridaEducator);
                    getBroker().saveBeanForced(student.getPerson());
                    addMessage("Student without FLEID id updated with FLEID id [" + idFloridaEducator + "]",
                            record, lineNumber);
                } else {
                    if (idFloridaEducator.equals(studentStateId)) {
                        ++m_matchCount;
                    } else {
                        student.getPerson().setFieldValueByAlias(ALIAS_FL_EDUCATION_ID, idFloridaEducator);
                        getBroker().saveBeanForced(student.getPerson());
                        addMessage(
                                "Student with FLEID id [" + studentStateId + "] updated with FLEID id ["
                                        + idFloridaEducator
                                        + "]",
                                record, lineNumber);
                    }
                }
            } else {
                addMessage("Student with local id [" + localId + "] is not found", record, lineNumber);
            }
        } else {
            addMessage("Import row wihtout FLEID", record, lineNumber);
        }
    }

    /**
     * Adds the message.
     *
     * @param message String
     * @param record List<String>
     * @param lineNumber int
     */
    private void addMessage(String message, List<String> record, int lineNumber) {
        String recordInfo = getRecordInfo(record);
        m_messages.add(new OutputMessage(lineNumber, recordInfo, message));
    }

    /**
     * Write a list of strings to the results handler.
     *
     * @param list List<String>
     * @throws X2BaseException exception
     */
    private void exportList(List<?> list) throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        for (Object err : list) {
            buffer.append(err.toString());
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
     * Gets the field.
     *
     * @param record List<String>
     * @param fieldName String
     * @return String
     */
    private String getField(List<String> record, String fieldName) {
        String value = null;
        int index = m_definition.getFieldIndex(fieldName);
        if (index >= 0) {
            value = record.get(index);
        }
        return value;
    }

    /**
     * Gets the record info.
     *
     * @param record List<String>
     * @return String
     */
    private String getRecordInfo(List<String> record) {
        StringBuilder value = new StringBuilder();
        value.append(getField(record, HEADING_LOCAL_ID));
        value.append(" - ");
        value.append(getField(record, HEADING_FIRST_NAME));
        value.append(" ");
        value.append(getField(record, HEADING_MIDDLE_NAME));
        value.append(" ");
        value.append(getField(record, HEADING_LAST_NAME));
        value.append(" - ");
        value.append(getField(record, HEADING_DOB_MONTH));
        value.append("/");
        value.append(getField(record, HEADING_DOB_DAY));
        value.append("/");
        value.append(getField(record, HEADING_DOB_YEAR));
        return value.toString();
    }

    /**
     * Gets the student.
     *
     * @param localId String
     * @return Sis student
     */
    private SisStudent getStudent(String localId) {
        SisStudent student = null;
        if (!StringUtils.isEmpty(localId)) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisStudent.COL_LOCAL_ID, localId);
            BeanQuery query = new BeanQuery(SisStudent.class, criteria);
            student = (SisStudent) getBroker().getBeanByQuery(query);
        }
        return student;
    }
}
