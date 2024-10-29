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
import com.follett.fsc.core.k12.beans.BeanManager.PersistenceKey;
import com.follett.fsc.core.k12.beans.FileAttachment;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.FileAttachmentManager;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.sql.Blob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;


/**
 * The Class MoveAassignmentProcedures.
 */
public class MoveAassignmentProcedures extends ProcedureJavaSource {
    private static final String MYSQL_ALTER =
            "ALTER TABLE GRADEBOOK_STUDENT_SUBMISSION ADD COLUMN GSS_PATH VARCHAR (20) NULL;";
    private static final String ORACEL_ALTER =
            "ALTER TABLE GRADEBOOK_STUDENT_SUBMISSION ADD GSS_PATH VARCHAR2 (20) NULL;";
    private static final String SQLSERVER_ALTER =
            "ALTER TABLE GRADEBOOK_STUDENT_SUBMISSION ADD GSS_PATH NCHAR (20) COLLATE SQL_Latin1_General_CP1_CS_AS  NULL;";
    private static final String SELECT_SQL =
            "SELECT GSS_OID, GSS_FILE FROM GRADEBOOK_STUDENT_SUBMISSION WHERE GSS_FILE IS NOT NULL AND GSS_PATH IS NULL";
    private static final String UPDATE_SQL = "UPDATE GRADEBOOK_STUDENT_SUBMISSION SET GSS_PATH = ? WHERE GSS_OID = ?";

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Connection connection = getBroker().borrowConnection();
        try {
            DatabaseMetaData md = connection.getMetaData();
            String alterSQL = "";
            int fetchSize = Integer.MIN_VALUE;
            if ("Microsoft SQL Server".equalsIgnoreCase(md.getDatabaseProductName())) {
                alterSQL = SQLSERVER_ALTER;
                fetchSize = 0;
            } else if ("MySQL".equalsIgnoreCase(md.getDatabaseProductName())) {
                alterSQL = MYSQL_ALTER;
            } else {
                alterSQL = ORACEL_ALTER;
            }

            int count = 0;

            if (isColsReady(md, connection, alterSQL)) {
                Organization rootOrg = OrganizationManager.getRootOrganization(getBroker());
                Statement selectStatement =
                        connection.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
                PreparedStatement updateStatement = connection.prepareStatement(UPDATE_SQL);

                /*
                 * This signals the driver to stream the data rather than send it all-at-once.
                 * If don't call it, will throws out of memory error with huge table
                 */
                selectStatement.setFetchSize(fetchSize);
                ResultSet resultSet = selectStatement.executeQuery(SELECT_SQL);
                // int target = 100;
                while (resultSet.next()) {
                    String oid = null;
                    try {
                        oid = resultSet.getString("GSS_OID");
                        Blob blob = resultSet.getBlob("GSS_FILE");
                        byte[] allBytesInBlob = blob.getBytes(1, (int) blob.length());
                        if (allBytesInBlob != null && allBytesInBlob.length > 0) {
                            AssignmentAttachment attachement =
                                    new AssignmentAttachment(oid, getBroker().getPersistenceKey(), getBroker());
                            File outputFile = getOutPutFile(rootOrg, attachement, allBytesInBlob);
                            if (outputFile != null) {
                                attachement.setBinaryFile(outputFile);
                                FileAttachmentManager.saveToDisk(getBroker(), true, attachement);
                                outputFile.delete();
                                updateStatement.setString(1, attachement.getPath());
                                updateStatement.setString(2, oid);
                                updateStatement.addBatch();
                                count++;
                            }
                        }
                    } catch (Exception e) {
                        logMessage("oid : " + oid + " failed: " + e.toString() + "\r\n");
                    }
                }

                resultSet.close();
                selectStatement.close();

                if (count > 0) {
                    updateStatement.executeBatch();
                }

                updateStatement.close();
                logMessage(count + " records updated");
            } else {
                logMessage("GRADEBOOK_STUDENT_SUBMISSION.GSS_PATH is not ready");
            }
        } catch (Exception e) {
            logMessage(e.toString());
            throw e;
        } finally {
            getBroker().returnConnection();
        }

    }

    /**
     * Return temp File for given binary data.
     *
     * @param organization Organization
     * @param fileAttachment FileAttachment
     * @param rawData byte[]
     * @return File
     */
    private File getOutPutFile(Organization organization, FileAttachment fileAttachment, byte[] rawData) {
        File outputFile = null;
        if (rawData != null) {
            try {
                ByteArrayInputStream byteStream = new ByteArrayInputStream(rawData);
                outputFile = FileAttachmentManager.createDestinationFile(organization, byteStream);
            } catch (IOException ioe) {
                logMessage("Unable to create temp file for: " + fileAttachment.getOid());
            }
        }

        return outputFile;
    }

    /**
     * Check if GSS_PATH exist, if not then create it, return if GSS_PATH is ready.
     *
     * @param md DatabaseMetaData
     * @param connection Connection
     * @param alterSQL String
     * @return boolean
     * @throws Exception exception
     */
    private boolean isColsReady(DatabaseMetaData md, Connection connection, String alterSQL) throws Exception {
        boolean colsReady = false;

        ResultSet rs = md.getColumns(null, null, "GRADEBOOK_STUDENT_SUBMISSION", "GSS_PATH");
        if (!rs.next()) {
            Statement alterStatement = connection.createStatement();
            if (alterStatement.executeUpdate(alterSQL) >= 0) {
                colsReady = true;
            }
            alterStatement.close();
        } else {
            colsReady = true;
        }
        rs.close();
        return colsReady;
    }

    /**
     * The Class AssignmentAttachment.
     */
    private class AssignmentAttachment implements FileAttachment {
        private String m_oid;
        private PersistenceKey m_persistenceKey;
        private X2Broker m_broker;
        private File m_binaryFile;

        /**
         * Instantiates a new assignment attachment.
         *
         * @param oid String
         * @param persistenceKey PersistenceKey
         * @param broker X2Broker
         */
        public AssignmentAttachment(String oid, PersistenceKey persistenceKey, X2Broker broker) {
            m_persistenceKey = persistenceKey;
            m_oid = oid;
            m_broker = broker;
        }

        /**
         * Gets the binary file.
         *
         * @return File
         * @see com.follett.fsc.core.k12.beans.FileAttachment#getBinaryFile()
         */
        @Override
        public File getBinaryFile() {
            return m_binaryFile;
        }

        /**
         * Gets the oid.
         *
         * @return String
         * @see com.follett.fsc.core.k12.beans.FileAttachment#getOid()
         */
        @Override
        public String getOid() {
            return m_oid;
        }

        /**
         * Gets the path.
         *
         * @return String
         * @see com.follett.fsc.core.k12.beans.FileAttachment#getPath()
         */
        @Override
        public String getPath() {
            return FileAttachmentManager.getSubFolderPathForFile(OrganizationManager.getRootOrganization(m_broker));
        }

        /**
         * Gets the persistence key.
         *
         * @return Persistence key
         * @see com.follett.fsc.core.k12.beans.FileAttachment#getPersistenceKey()
         */
        @Override
        public PersistenceKey getPersistenceKey() {
            return m_persistenceKey;
        }

        /**
         * Gets the public indicator.
         *
         * @return boolean
         * @see com.follett.fsc.core.k12.beans.FileAttachment#getPublicIndicator()
         */
        @Override
        public boolean getPublicIndicator() {
            return false;
        }

        /**
         * Sets the binary file.
         *
         * @param data void
         * @see com.follett.fsc.core.k12.beans.FileAttachment#setBinaryFile(java.io.File)
         */
        @Override
        public void setBinaryFile(File data) {
            m_binaryFile = data;
        }

        /**
         * Sets the path.
         *
         * @param path void
         * @see com.follett.fsc.core.k12.beans.FileAttachment#setPath(java.lang.String)
         */
        @Override
        public void setPath(String path) {
            // Auto-generated method stub

        }

        /**
         * Sets the public indicator.
         *
         * @param isPublic void
         * @see com.follett.fsc.core.k12.beans.FileAttachment#setPublicIndicator(boolean)
         */
        @Override
        public void setPublicIndicator(boolean isPublic) {
            // Auto-generated method stub
        }

    }
}
