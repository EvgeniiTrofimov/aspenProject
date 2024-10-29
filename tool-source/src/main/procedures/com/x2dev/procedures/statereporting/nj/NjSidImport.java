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
package com.x2dev.procedures.statereporting.nj;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class NjSidImport extends TextImportJavaSource {
    private static final String EXPORT_FORMAT_PROCEDURE_ID = "EXP-NJ-SID";
    private static final String HEADING_LOCAL_ID = "LocalIdentificationNumber";
    private static final String HEADING_STATE_ID = "StateIdentificationNumber";
    private static final String HEADING_FIRST_NAME = "FirstName";
    private static final String HEADING_MIDDLE_NAME = "MiddleName";
    private static final String HEADING_LAST_NAME = "LastName";
    private static final String HEADING_DOB = "DateOfBirth";
    private static final List<String> HEADINGS_REQUIRED =
            Arrays.asList(HEADING_LOCAL_ID, HEADING_STATE_ID, HEADING_FIRST_NAME, HEADING_MIDDLE_NAME,
                    HEADING_LAST_NAME, HEADING_DOB);
    private static final Pattern PATTERN_STATE_ID = Pattern.compile("^[1-9][0-9]{9}$");

    private class OutputMessage {
        private int lineNumber;
        private String recordInfo;
        private String message;

        /**
         * @param type
         * @param lineNumber
         * @param recordInfo
         * @param message
         */
        private OutputMessage(int lineNumber, String recordInfo, String message) {
            super();
            this.lineNumber = lineNumber;
            this.recordInfo = recordInfo;
            this.message = message;
        }

        @Override
        public String toString() {
            StringBuilder value = new StringBuilder();
            value.append("Line # ");
            value.append(lineNumber);
            value.append(") ");
            value.append(recordInfo);
            value.append(" --- ");
            value.append(message);
            return value.toString();
        }
    }

    private ModelBroker m_broker;
    private int m_fieldCount = -1;
    private Map<String, Integer> m_headingPosition = new HashMap();
    private List<String> m_initErrors = new LinkedList();
    private int m_matchCount = 0;
    private List<OutputMessage> m_messages = new LinkedList();

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
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        if (m_fieldCount < 0) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ExportFormatField.REL_DEFINITION + ModelProperty.PATH_DELIMITER
                    + ExportFormatDefinition.COL_PROCEDURE_ID, EXPORT_FORMAT_PROCEDURE_ID);
            BeanQuery query = new BeanQuery(ExportFormatField.class, criteria);
            m_fieldCount = getBroker().getCount(query);
        }
        return m_fieldCount;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        if (lineNumber == 1) {
            loadHeading(record);
        } else if (m_initErrors.isEmpty()) {
            String stateId = getField(record, HEADING_STATE_ID);
            if (validStateId(stateId)) {
                String localId = getField(record, HEADING_LOCAL_ID);
                SisStudent student = getStudent(localId);
                if (student != null) {
                    String studentStateId = student.getStateId();
                    if (StringUtils.isEmpty(studentStateId)) {
                        student.setStateId(stateId);
                        addMessage("Student without state id updated with state id [" + stateId + "]",
                                record, lineNumber);
                    } else {
                        if (stateId.equals(studentStateId)) {
                            ++m_matchCount;
                        } else {
                            student.setStateId(stateId);
                            addMessage(
                                    "Student with state id [" + studentStateId + "] updated with state id [" + stateId
                                            + "]",
                                    record, lineNumber);
                        }
                    }
                    if (student.isStateIdDirty()) {
                        getBroker().saveBeanForced(student);
                    }
                } else {
                    addMessage("Student with local id [" + localId + "] is not found", record, lineNumber);
                }

            } else {
                addMessage("State Id[" + stateId + "] is not valid", record, lineNumber);
            }
        }
    }

    /**
     * @param localId
     * @return
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

    /**
     * @param msgWarn
     * @param string
     * @param record
     * @param lineNumber
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
     * @param record
     * @param headingStateId
     * @return
     */
    private String getField(List<String> record, String heading) {
        Integer pos = m_headingPosition.get(heading);
        return pos == null ? null : record.get(pos.intValue());
    }

    /**
     * @param record
     * @return
     */
    private String getRecordInfo(List<String> record) {
        StringBuilder value = new StringBuilder();
        value.append(getField(record, HEADING_LOCAL_ID));
        value.append(" - ");
        value.append(getField(record, HEADING_STATE_ID));
        value.append(" - ");
        value.append(getField(record, HEADING_FIRST_NAME));
        value.append(" ");
        value.append(getField(record, HEADING_MIDDLE_NAME));
        value.append(" ");
        value.append(getField(record, HEADING_LAST_NAME));
        value.append(" - ");
        value.append(getField(record, HEADING_DOB));
        return value.toString();
    }

    /**
     * @param record
     */
    private void loadHeading(List<String> record) {
        List headings = new ArrayList(record.size());
        for (String item : record) {
            headings.add(item.trim());
        }
        for (String heading : HEADINGS_REQUIRED) {
            int pos = headings.indexOf(heading);
            if (pos < 0) {
                m_initErrors.add("Headings on import file must include column " + heading);
            } else {
                m_headingPosition.put(heading, Integer.valueOf(pos));
            }
        }
    }

    /**
     * @param stateId
     * @return
     */
    private boolean validStateId(String stateId) {
        return PATTERN_STATE_ID.matcher(stateId).find();
    }

}
