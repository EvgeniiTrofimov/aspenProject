/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2002-2015 X2 Development Corporation / Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppConstants;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpATTRS;
import com.jcraft.jsch.SftpException;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;


/**
 * Procedure to perform the transfer of a file to or from a remote server using SFTP protocol over
 * SSH connection. This procedure uses the Jsch library instead of the legacy J2ssh.
 * <p>
 * <ul>
 * <li>http://www.jcraft.com/jsch/</li>
 * <li>For any development changes - this tool is not included in the standard bundle and needs to
 * be manually
 * rolled out to clients</li>
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class SshJschFileTransferProcedure extends ProcedureJavaSource {
    // Input parameters
    private static final String PARAM_DIRECTION = "direction";
    private static final String PARAM_LOCAL_FILE = "localFile";
    private static final String PARAM_PASSWORD = "password";
    private static final String PARAM_PORT = "port";
    private static final String PARAM_REMOTE_FILE = "remoteFile";
    private static final String PARAM_SERVER = "server";
    private static final String PARAM_USERID = "userId";

    // Other constants
    private static final int DIRECTION_SEND = 0;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Session session = null;
        Channel channel = null;

        /**
         * In some cases, you can use application specific tricks. For example, if a thread is
         * waiting on a known socket, you can close the socket to cause the thread to return
         * immediately. Unfortunately, there really isn't any technique that works in general. It
         * should be noted that in all situations where a waiting thread doesn't respond to
         * Thread.interrupt, it wouldn't respond to Thread.stop either. Such cases include
         * deliberate denial-of-service attacks, and I/O operations for which thread.stop and
         * thread.interrupt do not work properly.
         */
        int timeOut = Integer.parseInt(
                AppGlobals.getConfig().getProperty(AppConstants.JOB_QUEUE_AUTO_KILL_MINUTES)) * 60 * 1000;
        try {
            String userName = (String) getParameter(PARAM_USERID);
            String host = (String) getParameter(PARAM_SERVER);
            String password = (String) getParameter(PARAM_PASSWORD);
            int port = ((Integer) getParameter(PARAM_PORT)).intValue();

            JSch ssh = new JSch();
            JSch.setConfig("StrictHostKeyChecking", "no");
            session = ssh.getSession(userName, host, port);
            session.setPassword(password);

            logMessage("Opening SSH connection...");
            session.connect(timeOut);

            if (session.isConnected()) {
                channel = session.openChannel("sftp");
                channel.connect(timeOut);

                ChannelSftp sftp = (ChannelSftp) channel;

                String localFilePath = (String) getParameter(PARAM_LOCAL_FILE);
                String remoteFilePath = (String) getParameter(PARAM_REMOTE_FILE);
                int direction = ((Integer) getParameter(PARAM_DIRECTION)).intValue();

                logMessage("Starting file transfer...");
                long fileSize = transferFile(sftp, localFilePath, remoteFilePath, direction);
                logMessage("File transfer completed. Transfered " + fileSize + " bytes.");
            }
        } catch (SftpException | JSchException e) {
            logMessage(getErrorMessage(e, "Failed to connect "));

        } finally {
            try {
                if (channel != null) {
                    channel.disconnect();
                }
            } catch (Exception e) {
                logMessage(getErrorMessage(e, "Failed to disconnect channel."));
            }

            try {
                if (session != null) {
                    session.disconnect();
                }
            } catch (Exception e) {
                logMessage(getErrorMessage(e, "Failed to disconnect session."));
            }
        }
    }

    /**
     * Returns a string containing error message and diagnostic information.
     *
     * @param exception an Exception
     * @param message a message key for a message to include in the error string.
     *
     * @return String
     */
    private String getErrorMessage(Exception exception, String message) {
        StringBuilder buffer = new StringBuilder(1024);

        buffer.append(message);
        buffer.append("\n");

        // Build the stack trace into string text.
        StringWriter stringWriter = new StringWriter();
        PrintWriter printWriter = new PrintWriter(stringWriter);

        exception.printStackTrace(printWriter);
        printWriter.flush();
        printWriter.close();
        buffer.append(stringWriter.getBuffer().toString());

        return buffer.toString();
    }

    /**
     * Returns the absolute path of the secure directory after checking if it exists. Otherwise
     * returns an empty string.
     *
     * @return String secure directory path
     */
    private String getSecureDirectoryPath() {
        String absolutePath = "";

        Organization organization = getOrganization();
        String deploymentId = organization.getPersistenceKey().getDeploymentId();

        File secureRootDirectory = AppGlobals.getSecureRootDirectory(organization, deploymentId);
        if (secureRootDirectory != null && secureRootDirectory.isDirectory()) {
            absolutePath = secureRootDirectory.getAbsolutePath() + File.separator;
        }

        return absolutePath;
    }


    /**
     * Validates the existence of a directory and logs a validation message.
     *
     * @param sftpClient ChannelSftp
     * @param path String
     * @param isRemote boolean
     * @return boolean
     */
    private boolean isValidDirectory(ChannelSftp sftpClient, String path, boolean isRemote) {
        boolean valid = false;
        String directoryPath = null;

        if (path != null) {
            KeyValuePair<String, String> pathElements = FolderUtils.getPathElements(path);

            directoryPath = pathElements.getKey();
            if (directoryPath != null) {
                if (isRemote) {
                    try {
                        sftpClient.cd(directoryPath);
                        valid = true;
                    } catch (SftpException sfe) {
                        logMessage(getErrorMessage(sfe, "Failed to navigate to " + directoryPath + "."));
                    }
                } else {
                    try {
                        valid = new File(directoryPath).isDirectory();
                    } catch (SecurityException se) {
                        logMessage(
                                getErrorMessage(se, "Cannot access '" + directoryPath + "'. Read privilege missing."));
                    }
                }
            }
            // Case for storing in root folder
            else if (isRemote) {
                valid = true;
            }
        }

        if (!valid) {
            logMessage("Invalid " + (isRemote ? "remote" : "local") + " directory: " + directoryPath);
        }

        return valid;
    }

    /**
     * Validates the existence of a file and logs a validation message.
     *
     * @param sftpChannel ChannelSftp
     * @param path String
     * @param isRemote boolean
     * @return boolean
     */
    private boolean isValidFile(ChannelSftp sftpChannel, String path, boolean isRemote) {
        boolean valid = false;

        if (path != null) {
            if (isRemote) {
                try {
                    if (sftpChannel != null) {
                        SftpATTRS fileAttributes = sftpChannel.lstat(path);
                        if (fileAttributes != null && !fileAttributes.isDir()) {
                            valid = true;
                        }
                    }
                } catch (SftpException sfe) {
                    logMessage(getErrorMessage(sfe, "Failed to retrieve file attributes " + path + "."));
                }
            } else {
                try {
                    valid = new File(path).isFile();
                } catch (SecurityException se) {
                    logMessage(getErrorMessage(se, "Cannot access '" + path + "'. Read privilege missing."));
                }
            }
        }

        if (!valid) {
            logMessage("Invalid " + (isRemote ? "remote" : "local") + " file: " + path);
        }

        return valid;
    }

    /**
     * Returns the file size after transferring it between client and host in the specified
     * direction. Absolute paths for both local and remote files are expected.
     *
     * @param sftpClient ChannelSftp
     * @param localFilePath String
     * @param remoteFilePath String
     * @param direction int
     * @return long
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws SftpException exception
     */
    private long transferFile(ChannelSftp sftpClient,
                              String localFilePath,
                              String remoteFilePath,
                              int direction)
            throws IOException, SftpException {
        if (AppGlobals.getSchedulerUseSecure()) {
            localFilePath = getSecureDirectoryPath() + localFilePath;
        }

        if (direction == DIRECTION_SEND) {
            logMessage("Transfer mode: Send");

            if (isValidDirectory(sftpClient, remoteFilePath, true) && isValidFile(sftpClient, localFilePath, false)) {
                sftpClient.put(localFilePath, remoteFilePath);
            } else {
                throw new IOException("Could not validate 'Send' operation.");
            }
        } else {
            logMessage("Transfer mode: Receive");

            if (isValidDirectory(sftpClient, localFilePath, false) && isValidFile(sftpClient, remoteFilePath, true)) {
                sftpClient.get(remoteFilePath, localFilePath);
            } else {
                throw new IOException("Could not validate 'Receive' operation.");
            }
        }

        return new File(localFilePath).length();
    }
}
