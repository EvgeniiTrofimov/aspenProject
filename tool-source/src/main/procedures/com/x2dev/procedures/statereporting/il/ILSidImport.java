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
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;

/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class ILSidImport extends TextImportJavaSource {
    private static final String ACTIVE_ONLY = "activeOnly";
    private static final String EXPORT_FORMAT_PROCEDURE_ID = "EXP-IL-STDSID";
    private static final int POSITION_STATE_ID = 0;
    private static final int POSITION_LOCAL_ID = 1;
    private static final Pattern PATTERN_STATE_ID = Pattern.compile("^[0-9]{9}$");
    private Map<String, MinimalStudentInfo> m_localIdToStudent;

    /**
     * A data structure that tracks the essential student attributes
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    private class MinimalStudentInfo {
        private String m_firstName;
        private String m_lastName;
        private String m_localId;
        private String m_stateId;
        private String m_studentOid;

        /**
         * Gets the first name.
         *
         * @return String the m_firstName
         */
        public String getFirstName() {
            return m_firstName;
        }

        /**
         * Gets the last name.
         *
         * @return String the m_lastName
         */
        public String getLastName() {
            return m_lastName;
        }

        /**
         * Gets the local id.
         *
         * @return String the m_localId
         */
        public String getLocalId() {
            return m_localId;
        }

        /**
         * Gets the state id.
         *
         * @return String the m_stateId
         */
        public String getStateId() {
            return m_stateId;
        }

        /**
         * Gets the student oid.
         *
         * @return String the student oid
         */
        public String getStudentOid() {
            return m_studentOid;
        }

        /**
         * Sets the first name.
         *
         * @param firstName void
         */
        public void setFirstName(String firstName) {
            m_firstName = firstName;
        }

        /**
         * Sets the last name.
         *
         * @param lastName void
         */
        public void setLastName(String lastName) {
            m_lastName = lastName;
        }

        /**
         * Sets the local id.
         *
         * @param localId void
         */
        public void setLocalId(String localId) {
            m_localId = localId;
        }

        /**
         * Sets the state id.
         *
         * @param stateId void
         */
        public void setStateId(String stateId) {
            m_stateId = stateId;
        }

        /**
         * Sets the student oid.
         *
         * @param studentOid void
         */
        public void setStudentOid(String studentOid) {
            m_studentOid = studentOid;
        }
    }

    /**
     * The Class OutputMessage.
     */
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
            value.append("Line #");
            value.append(lineNumber);
            value.append(" | ");
            value.append(message);
            return value.toString();
        }
    }

    private ModelBroker m_broker;
    private int m_fieldCount = -1;
    private int m_matchCount = 0;
    private Writer m_outputWriter;
    private boolean m_initErrors = false;
    private PreparedStatement m_updateStatement;

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
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#beforeImportData(java.io.File)
     */
    @Override
    protected void beforeImportData(File file) {
        super.beforeImportData(file);

        try {
            m_outputWriter = new OutputStreamWriter(getResultHandler().getOutputStream());
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        }

        loadMinimalStudentInfo();

        String sql = buildUpdateQueryString();

        try {
            m_updateStatement = getBroker().borrowConnection().prepareStatement(sql);
        } catch (SQLException e) {
            // This should never happen
            write("Error in SQL: " + e.getMessage(), e);
        }
    }

    /**
     * @throws IOException
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#cleanup()
     */
    @Override
    protected void cleanup() {
        super.cleanup();
        try {
            m_updateStatement.close();
        } catch (SQLException e) {
            write("Error in SQL: " + e.getMessage(), e);
        }
        getBroker().returnConnection();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#exportResults()
     */
    @Override
    protected void exportResults() throws X2BaseException {
        try {
            try {
                m_updateStatement.executeBatch();
                m_updateStatement.clearBatch();
            } catch (SQLException e) {
                write("Error in SQL: " + e.getMessage(), e);
            }

            AppGlobals.getCache(((SisOrganization) getOrganization()).getPersistenceKey())
                    .clear(SisStudent.class.getName());

            if (!m_initErrors) {
                write("# Records where Local ID and State ID Matched - No updates: " + m_matchCount, null);
            }
        } finally {
            try {
                m_outputWriter.close();
            } catch (IOException ioe) {
                throw new X2BaseException(ioe);
            }
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
        if (!m_initErrors && lineNumber >= 1) {
            String stateId = getField(record, POSITION_STATE_ID);
            if (validStateId(stateId)) {
                String localId = getField(record, POSITION_LOCAL_ID);
                MinimalStudentInfo ms = m_localIdToStudent.get(localId);
                if (ms != null) {
                    String studentStateId = ms.getStateId();

                    if (stateId.equals(studentStateId)) {
                        ++m_matchCount;
                    } else {
                        setStateId(record, lineNumber, stateId, ms);
                    }
                } else {
                    addMessage("Student with Local ID [" + localId + "] is not found", record, lineNumber);
                }

            } else {
                addMessage("State Id[" + stateId + "] is not valid", record, lineNumber);
            }
        }
    }

    /**
     * Adds the message.
     *
     * @param message String
     * @param record List<String>
     * @param lineNumber int
     * @throws IOException
     */
    private void addMessage(String message, List<String> record, int lineNumber) throws IOException {
        String recordInfo = getRecordInfo(record);
        m_outputWriter.write(new OutputMessage(lineNumber, recordInfo, message).toString() + "\n");
    }

    /**
     * Builds the criteria.
     *
     * @return X2Criteria
     */
    private X2Criteria buildCriteria() {
        X2Criteria studentCriteria = new X2Criteria();
        if (getParameter(ACTIVE_ONLY).equals(BooleanAsStringConverter.TRUE)) {
            String activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                    SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);
            studentCriteria.addEqualTo(SisStudent.COL_ENROLLMENT_STATUS, activeCode);
        }
        return studentCriteria;
    }

    /**
     * Builds the message.
     *
     * @param stateId String
     * @param ms MinimalStudent
     * @param hasStateId boolean
     */
    private String buildMessage(String stateId, MinimalStudentInfo ms, boolean hasStateId) {
        String message = "";
        if (!hasStateId) {
            message = "Added State ID [" + stateId + "] for First Name [" + ms.getFirstName() + "], Last Name [" +
                    ms.getLastName() + "], Local ID [" + ms.getLocalId() + "]";
        } else {
            message = "Updated State ID from [" + ms.getStateId() + "] to [" + stateId + "] for First Name [" +
                    ms.getFirstName() + "], Last Name [" + ms.getLastName() + "], Local ID [" + ms.getLocalId() +
                    "]";
        }

        return message;
    }

    /**
     * Builds the update query string.
     *
     * @return String
     */
    private String buildUpdateQueryString() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        String tableName = dictionary.findDataDictionaryTableByClass(Student.class.getName()).getDatabaseName();
        String stateIdField = dictionary.findDataDictionaryField(Student.class.getName(),
                Student.COL_STATE_ID).getDatabaseName();
        String studentOidField = dictionary.findDataDictionaryField(Student.class.getName(),
                X2BaseBean.COL_OID).getDatabaseName();

        StringBuilder sql = new StringBuilder();
        sql.append("UPDATE ");
        sql.append(tableName);
        sql.append(" SET ");
        sql.append(stateIdField);
        sql.append(" = ? WHERE ");
        sql.append(studentOidField);
        sql.append(" = ?");

        return sql.toString();
    }

    /**
     * Returns the field (stripping out the BOM character)
     *
     * @param record
     * @param headingStateId
     * @return String
     */
    private String getField(List<String> record, int pos) {
        String returnValue = record.get(pos);
        // The CPS Import is using UTF-8-BOM. We need to remove character 65279 in order to get a
        // valid value
        if (pos == 0 && returnValue.length() > 0 && returnValue.charAt(0) == '\ufeff') {
            returnValue = returnValue.substring(1);
        }
        return returnValue;
    }

    /**
     * Gets the record info.
     *
     * @param record List<String>
     * @return String
     */
    private String getRecordInfo(List<String> record) {
        StringBuilder value = new StringBuilder();
        value.append(getField(record, POSITION_STATE_ID));
        value.append(" - ");
        value.append(getField(record, POSITION_LOCAL_ID));
        return value.toString();
    }

    /**
     * Loads the essential student attributes to memory for fast retrieval
     */
    private void loadMinimalStudentInfo() {
        m_localIdToStudent = new HashMap<>();

        String[] columns = {SisStudent.COL_LOCAL_ID,
                SisStudent.COL_STATE_ID,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                SisStudent.REL_PERSON + PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                X2BaseBean.COL_OID
        };
        X2Criteria studentCriteria = buildCriteria();
        ColumnQuery query = new ColumnQuery(SisStudent.class, columns, studentCriteria);

        try (ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);) {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();

                MinimalStudentInfo ms = new MinimalStudentInfo();
                ms.setFirstName((String) row[2]);
                ms.setLastName((String) row[3]);
                ms.setLocalId((String) row[0]);
                ms.setStateId((String) row[1]);
                ms.setStudentOid((String) row[4]);

                m_localIdToStudent.put((String) row[0], ms);
            }
        }
    }

    /**
     * Sets the state id.
     *
     * @param record List<String>
     * @param lineNumber int
     * @param stateId String
     * @param ms MinimalStudent
     * @throws IOException
     */
    private void setStateId(List<String> record,
                            int lineNumber,
                            String stateId,
                            MinimalStudentInfo ms)
            throws IOException {

        if (StringUtils.isEmpty(ms.getStateId())) {
            addMessage(buildMessage(stateId, ms, false), record, lineNumber);
        } else {
            addMessage(buildMessage(stateId, ms, true), record, lineNumber);
        }

        try {
            m_updateStatement.clearParameters();
            m_updateStatement.setString(1, stateId);
            m_updateStatement.setString(2, ms.getStudentOid());
            m_updateStatement.addBatch();
        } catch (SQLException e) {
            write("Error in SQL: " + e.getMessage(), e);
        }
    }

    /**
     * Valid state id.
     *
     * @param stateId String
     * @return true, if successful
     */
    private boolean validStateId(String stateId) {
        return PATTERN_STATE_ID.matcher(stateId).find();
    }

    /**
     * Writes the message
     *
     * @param str the String to write, followed by a return line.
     * @param throwable an optional Throwable to mention if our underlying OutputStream throws an
     *        IOException.
     */
    private void write(String str, Throwable throwable) {
        if (throwable != null) {
            m_initErrors = true;
        }
        try {
            m_outputWriter.write(str + "\n");
        } catch (IOException ioe) {
            if (throwable != null) {
                throw new RuntimeException(
                        "An error occurred while logging: " + LoggerUtils.convertThrowableToString(throwable), ioe);
            }
            throw new RuntimeException("An error occurred while writing \"" + str + "\"", ioe);
        }
    }
}
