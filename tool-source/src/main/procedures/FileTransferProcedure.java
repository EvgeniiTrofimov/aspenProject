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

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Properties;
import org.ftp4che.FTPConnection;
import org.ftp4che.FTPConnectionFactory;
import org.ftp4che.exception.ConfigurationException;
import org.ftp4che.exception.FtpIOException;
import org.ftp4che.exception.FtpWorkflowException;
import org.ftp4che.util.ftpfile.FTPFile;

/**
 * Procedure to perform FTP transfer of a file to or from a remote FTP server.
 *
 * @author X2 Development Corporation
 */
public class FileTransferProcedure extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    // Default values
    private static final int FTP_DEFAULT_PORT = 21;
    private static final int FTPS_DEFAULT_PORT = 990;

    // Input parameter keys
    private static final String PARAM_CONNECTION = "connectionType";
    private static final String PARAM_DIRECTION = "transDirection";
    private static final String PARAM_LOCAL_FILE = "localFile";
    private static final String PARAM_MODE = "transMode";
    private static final String PARAM_PASSIVE = "passiveMode";
    private static final String PARAM_PASSWORD = "password";
    private static final String PARAM_PORT = "port";
    private static final String PARAM_REMOTE_FILE = "remoteFile";
    private static final String PARAM_SERVER = "server";
    private static final String PARAM_USERID = "userId";

    // Log message keys
    private static final String MSG_ERROR_CONFIGURATION = "procedure.fileTransfer.errConfig";
    private static final String MSG_ERROR_CONNECTION = "procedure.fileTransfer.errConnect";
    private static final String MSG_ERROR_TRANSFER = "procedure.fileTransfer.errTrans";
    private static final String MSG_ERROR_UNKNOWN = "procedure.fileTransfer.errUnknown";

    // Other constants
    private static final String CONNECTION_TIMEOUT_LIMIT = "20000";
    private static final int DIRECTION_SEND = 0;
    private static final int MAX_CONNECTION_ATTEMPTS = 3;
    private static final int MODE_ASCII = 0;
    private static final int TIME_BETWEEN_ATTEMPTS = 3000; // In milliseconds

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String localFilePath = (String) getParameter(PARAM_LOCAL_FILE);
        String remoteFilePath = (String) getParameter(PARAM_REMOTE_FILE);
        int direction = ((Integer) getParameter(PARAM_DIRECTION)).intValue();
        int mode = ((Integer) getParameter(PARAM_MODE)).intValue();

        boolean error = false;
        FTPConnection connection = null;

        try {
            // Configure connection object.
            try {
                logMessage("Configuring connection.");
                connection = FTPConnectionFactory.getInstance(getConnectionProperties());
            } catch (ConfigurationException ce) {
                logMessage(getErrorMessage(ce, MSG_ERROR_CONFIGURATION));
                error = true;
            }

            // Make the connection
            if (!error) {
                try {
                    logMessage("Opening connection...");
                    openFtpConnection(connection);
                } catch (Exception e) {
                    logMessage(getErrorMessage(e, MSG_ERROR_CONNECTION));
                    error = true;
                }
            }

            // Transfer the file.
            if (!error) {
                try {
                    long size = transferFile(connection, validateLocalFilePath(localFilePath),
                            remoteFilePath, direction, mode);

                    logMessage("File transmitted: " + Long.toString(size) + " bytes.");
                } catch (Exception e) {
                    logMessage(getErrorMessage(e, MSG_ERROR_TRANSFER));
                    error = true;
                }
            }
        } catch (RuntimeException re) {
            logMessage(getErrorMessage(re, MSG_ERROR_UNKNOWN));
            error = true;
        } finally {
            // Disconnect when done.
            if (connection != null) {
                logMessage("Closing connection...");

                try {
                    connection.disconnect();
                } catch (RuntimeException re) {
                    logMessage(getErrorMessage(re, MSG_ERROR_UNKNOWN));
                }

                logMessage("Connection closed.");
            }
        }
    }

    /**
     * Returns an FTPFile after breaking the file path into directory path and file name.
     *
     * The constructor 'FTPFile(String, boolean)' assumes '/' and fails for windows paths. This
     * version checks for '\\' and '\' as well.
     *
     * @param path to break into directory and file.
     *
     * @return FTPFile an object initialized with the file.
     */
    private FTPFile createFTPFile(String path) {
        KeyValuePair<String, String> pathElements = FolderUtils.getPathElements(path);

        return new FTPFile(pathElements.getKey(), pathElements.getValue(), false);
    }

    /**
     * Retrieves parameters from the user and builds a Properties object to use in the connection.
     * 
     * @return Properties a object holding connection related information.
     */
    private Properties getConnectionProperties() {
        // Get parameters.
        String server = (String) getParameter(PARAM_SERVER);
        Integer port = (Integer) getParameter(PARAM_PORT);
        String userId = (String) getParameter(PARAM_USERID);
        String password = (String) getParameter(PARAM_PASSWORD);
        Integer connectionType = (Integer) getParameter(PARAM_CONNECTION);
        Boolean passive = (Boolean) getParameter(PARAM_PASSIVE);

        if (port == null) {
            if (connectionType.intValue() == 0 || connectionType.intValue() == 1 || connectionType.intValue() == 4) {
                // FTP or Explicit FTPS
                port = Integer.valueOf(FTP_DEFAULT_PORT);
            } else {
                // Implicit FTPS
                port = Integer.valueOf(FTPS_DEFAULT_PORT);
            }
        }

        /*
         * FTP4CHE connection configuration supports these properties
         * connection.host = hostname to the server you want to connect (String)
         * connection.port = port you want to connect to (String)
         * user.login = login name (String)
         * user.password = password (Sring).
         * connection.timeout = The timeout that will be used (Long object)
         * connection.passive = Should the DataConnection be established in passive mode (Boolean
         * Object)
         * connection.downloadbw = Maximum bytes / second that should be used for downloading
         * connection.uploadbw = Maximum bytes / second that should be used for uploading
         *
         * These parameters are optional
         * user.account = Account Information (String).
         * connection.type = The connection you want to have (normal,auth ssl,auth tls,...).
         * There are constants (int primitiv type) in FTPConnection.
         * You have to give a Integer object.
         *
         */
        Properties properties = new Properties();

        // Set host information.
        properties.setProperty("connection.host", server);
        properties.setProperty("connection.port", port.toString());
        properties.setProperty("user.login", userId);
        // Do not include the password until the properties have been displayed in the log file.

        // Set connection type.
        switch (connectionType.intValue()) {
            case 0:
                properties.setProperty("connection.type", "FTP_CONNECTION");
                break;

            case 1:
                properties.setProperty("connection.type", "AUTH_SSL_FTP_CONNECTION");
                break;

            case 2:
                properties.setProperty("connection.type", "IMPLICIT_SSL_FTP_CONNECTION");
                break;

            case 3:
                properties.setProperty("connection.type", "IMPLICIT_SSL_WITH_CRYPTED_DATA_FTP_CONNECTION");
                break;

            case 4:
                properties.setProperty("connection.type", "AUTH_TLS_FTP_CONNECTION");
                break;

            case 5:
                properties.setProperty("connection.type", "IMPLICIT_TLS_FTP_CONNECTION");
                break;

            case 6:
                properties.setProperty("connection.type", "IMPLICIT_TLS_WITH_CRYPTED_DATA_FTP_CONNECTION");
                break;
        }

        // Set transfer method.
        properties.setProperty("connection.passive", passive.toString());
        properties.setProperty("connection.timeout", CONNECTION_TIMEOUT_LIMIT);

        // Display properties for log file.
        logMessage("Connection properties:");
        logMessage(properties.toString());

        // Put in password after displaying the rest of the properties.
        properties.setProperty("user.password", password);

        return properties;
    }

    /**
     * Returns a string containing error message and diagnostic information.
     *
     * @param e an Exception
     * @param messageKey a message key for a message to include in the error string.
     *
     * @return an assembled error message.
     */
    private String getErrorMessage(Exception e, String messageKey) {
        StringBuilder buffer = new StringBuilder(1024);
        String message =
                LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale()).getMessage(messageKey);

        buffer.append(message);
        buffer.append("\n");

        // Build the stack trace into string text.
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        e.printStackTrace(printWriter);
        printWriter.flush();
        printWriter.close();
        buffer.append(stringWriter.getBuffer().toString());

        return buffer.toString();
    }

    /**
     * Attempts to open the FTP connection for the specified number of times and interval between
     * connections. This is done in case the connection attempt times out on the first try.
     *
     * @param connection FTPConnection
     * @throws Exception exception
     */
    private void openFtpConnection(FTPConnection connection) throws Exception {
        Exception exception = null;

        int count = 1;
        while (connection.getConnectionStatus() != FTPConnection.CONNECTED &&
                connection.getConnectionStatus() != FTPConnection.IDLE &&
                count <= MAX_CONNECTION_ATTEMPTS) {
            if (count > 1) {
                try {
                    Thread.sleep(TIME_BETWEEN_ATTEMPTS);
                } catch (InterruptedException ie) {
                    // Do nothing.
                }
            }

            logMessage("Attempt " + count);

            try {
                connection.connect();
            } catch (Exception e) {
                exception = e;
            }

            count++;
        }

        if (connection.getConnectionStatus() != FTPConnection.CONNECTED &&
                connection.getConnectionStatus() != FTPConnection.IDLE) {
            throw exception;
        }
    }

    /**
     * Transfers the file to or from the remote server based on the user input.
     *
     * @param connection FTPConnection
     * @param localFilePath String
     * @param remoteFilePath String
     * @param direction int
     * @param mode int
     * @return a long representing the file size that was transfered.
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws FtpWorkflowException exception
     * @throws FtpIOException exception
     */
    private long transferFile(FTPConnection connection,
                              String localFilePath,
                              String remoteFilePath,
                              int direction,
                              int mode)
            throws IOException, FtpWorkflowException, FtpIOException {
        File localFile = new File(localFilePath);
        FTPFile localFTPFile = new FTPFile(localFile);
        FTPFile remoteFTPFile = createFTPFile(remoteFilePath);

        // Set IMAGE/ASCII mode for transfer.
        logMessage("Setting transfer mode: " + (mode == MODE_ASCII ? "ASCII" : "Image"));

        connection.setTransferType(mode == MODE_ASCII ? false : true);

        // Transfer the file.
        logMessage("Transfering file...");
        if (direction == DIRECTION_SEND) {
            // Send the file.
            if (!localFile.exists()) {
                throw new IOException("Local file not found: " + localFilePath);
            }
            connection.uploadFile(localFTPFile, remoteFTPFile);
        } else // DIRECTION_RECEIVE
        {
            // Receive the file.
            connection.downloadFile(remoteFTPFile, localFTPFile);
        }

        return localFile.length();
    }

    /**
     * Validates the local file path.
     *
     * @param localFilePath String
     * @return String
     */
    private String validateLocalFilePath(String localFilePath) {
        // Translate the local file path to the secure directory.
        StringBuilder newLocalFilePath = new StringBuilder(512);
        if (AppGlobals.getSchedulerUseSecure()) {
            Organization organization = getOrganization();
            String deploymentId = organization.getPersistenceKey().getDeploymentId();

            File secureRoot = AppGlobals.getSecureRootDirectory(organization, deploymentId);
            if (secureRoot != null) {
                newLocalFilePath.append(secureRoot.getAbsolutePath());
                newLocalFilePath.append(File.separator);
            }

            if (localFilePath != null) {
                // Check for "../" or "..\" in delivery path and remove as a potential escape.
                String deliveryPart = localFilePath;

                int position = 0;
                while ((position = deliveryPart.indexOf("../")) > -1) {
                    deliveryPart = newLocalFilePath.substring(0, position) + newLocalFilePath.substring(position + 3);
                }

                while ((position = deliveryPart.indexOf("..\\")) > -1) {
                    deliveryPart = deliveryPart.substring(0, position) + deliveryPart.substring(position + 3);
                }

                newLocalFilePath.append(deliveryPart);
            }
        } else {
            if (localFilePath != null) {
                newLocalFilePath.append(localFilePath);
            }
        }

        String newLocalFile = newLocalFilePath.toString();

        // Add information to output log for diagnostics.
        if (!newLocalFile.equals(localFilePath)) {
            logMessage("Local file '" + localFilePath + "' translated to '" + newLocalFile + "'.");
        }

        return newLocalFile;
    }
}
