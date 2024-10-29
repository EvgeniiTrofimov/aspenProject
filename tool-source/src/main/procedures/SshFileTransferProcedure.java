/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
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
import com.follett.fsc.core.k12.web.AppGlobals;
import com.sshtools.j2ssh.SftpClient;
import com.sshtools.j2ssh.SshClient;
import com.sshtools.j2ssh.authentication.AuthenticationProtocolState;
import com.sshtools.j2ssh.authentication.KBIAuthenticationClient;
import com.sshtools.j2ssh.authentication.KBIPrompt;
import com.sshtools.j2ssh.authentication.KBIRequestHandler;
import com.sshtools.j2ssh.authentication.PasswordAuthenticationClient;
import com.sshtools.j2ssh.authentication.SshAuthenticationClient;
import com.sshtools.j2ssh.authentication.SshAuthenticationClientFactory;
import com.sshtools.j2ssh.configuration.SshConnectionProperties;
import com.sshtools.j2ssh.sftp.FileAttributes;
import com.sshtools.j2ssh.sftp.SftpSubsystemClient;
import com.sshtools.j2ssh.transport.IgnoreHostKeyVerification;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.LinkedList;
import java.util.List;

/* DEBUG */

/**
 * Procedure to perform the transfer of a file to or from a remote server using SFTP protocol over
 * SSH connection.
 * <p>
 * TODO:
 * <ul>
 * <li>Implement Public Key authentication client.
 * <li>Implement SCP connection protocol.
 * <li>For any development changes - this tool is not included in the standard bundle and needs to
 * be manually
 * rolled out to clients
 * </ul>
 *
 * @author X2 Development Corporation
 */
public class SshFileTransferProcedure extends ProcedureJavaSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

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
    private static final int MAX_CONNECTION_ATTEMPTS = 3;
    private static final int TIME_BETWEEN_ATTEMPTS = 3000; // In milliseconds

    // Member variables
    protected String m_password;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        SshClient sshClient = new SshClient();
        SftpClient sftpClient = null;

        try {
            logMessage("Opening SSH connection...");
            openSshConnection(sshClient, getSshConnectionProperties());

            logMessage("Authenticating SSH connection...");
            List<SshAuthenticationClient> authClients = getSshAuthenticationClients(sshClient);

            for (SshAuthenticationClient authClient : authClients) {
                int authenticationResult = sshClient.authenticate(authClient);

                if (authenticationResult == AuthenticationProtocolState.COMPLETE) {
                    logMessage("Succesfully authenticated using " + authClient.getMethodName() + " mode. Result code="
                            + authenticationResult);
                    break;
                }
                logMessage("Unable to authenticate using " + authClient.getMethodName() + " mode. Result code="
                        + authenticationResult);
            }

            if (sshClient.isAuthenticated()) {
                logMessage("Opening SFTP connection...");
                sftpClient = sshClient.openSftpClient();

                String localFilePath = (String) getParameter(PARAM_LOCAL_FILE);
                String remoteFilePath = (String) getParameter(PARAM_REMOTE_FILE);
                int direction = ((Integer) getParameter(PARAM_DIRECTION)).intValue();

                logMessage("Starting file transfer...");
                long fileSize = transferFile(sshClient, sftpClient, localFilePath, remoteFilePath, direction);
                logMessage("File transfer completed. Transfered " + fileSize + " bytes.");

                logMessage("Closing SFTP connection...");
                sftpClient.quit();
            } else {
                throw new Exception();
            }
        } catch (Exception e) {
            logMessage(getErrorMessage(e, "Operation error."));
        }

        if (sshClient.isConnected()) {
            logMessage("Closing SSH connection...");
            sshClient.disconnect();
            logMessage("SSH connection closed.");
        }
    }

    /**
     * Returns a string containing error message and diagnostic information.
     *
     * @param exception an Exception
     * @param message String
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
     * Returns the possible SSH authentication clients for the SSH connection. If the remote server
     * supports a KBI Authentication Client,
     * it will return that mode first, then Password mode. At a minimum, password mode will always
     * be in the returned collection.
     *
     * @param sshClient SshClient
     * @return List<SshAuthenticationClient>
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private List<SshAuthenticationClient> getSshAuthenticationClients(SshClient sshClient) throws IOException {
        List<SshAuthenticationClient> authenticationClients = new LinkedList<SshAuthenticationClient>();

        String userName = (String) getParameter(PARAM_USERID);
        m_password = (String) getParameter(PARAM_PASSWORD);

        List<String> authenticationMethods = sshClient.getAvailableAuthMethods(userName);

        if (authenticationMethods.contains(SshAuthenticationClientFactory.AUTH_KBI)) {
            SshAuthenticationClient authenticationClientKbi = new KBIAuthenticationClient();
            authenticationClientKbi.setUsername(userName);
            ((KBIAuthenticationClient) authenticationClientKbi).setKBIRequestHandler(new KBIRequestHandler() {

                @Override
                public void showPrompts(String name, String instructions, KBIPrompt[] prompts) {
                    if (prompts != null) {
                        for (int i = 0; i < prompts.length; i++) {
                            prompts[i].setResponse(m_password);
                        }
                    }
                }
            });

            authenticationClients.add(authenticationClientKbi);
        }

        SshAuthenticationClient authenticationClientPwd = new PasswordAuthenticationClient();
        authenticationClientPwd.setUsername(userName);

        ((PasswordAuthenticationClient) authenticationClientPwd).setPassword(m_password);
        authenticationClients.add(authenticationClientPwd);

        return authenticationClients;
    }

    /**
     * Returns the SSH connection properties.
     *
     * @return SshConnectionProperties
     */
    private SshConnectionProperties getSshConnectionProperties() {
        SshConnectionProperties sshProperties = new SshConnectionProperties();

        sshProperties.setHost((String) getParameter(PARAM_SERVER));
        sshProperties.setPort(((Integer) getParameter(PARAM_PORT)).intValue());

        return sshProperties;
    }

    /**
     * Validates the existence of a directory and logs a validation message.
     *
     * @param sftpClient SftpClient
     * @param path String
     * @param isRemote boolean
     * @return boolean
     */
    private boolean isValidDirectory(SftpClient sftpClient, String path, boolean isRemote) {
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
                    } catch (IOException ioe) {
                        // Do nothing. The logging happens if this method returns false.
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
        }

        if (!valid) {
            logMessage("Invalid " + (isRemote ? "remote" : "local") + " directory: " + directoryPath);
        }

        return valid;
    }

    /**
     * Validates the existence of a file and logs a validation message.
     *
     * @param sshClient SshClient
     * @param path String
     * @param isRemote boolean
     * @return boolean
     */
    private boolean isValidFile(SshClient sshClient, String path, boolean isRemote) {
        boolean valid = false;

        if (path != null) {
            if (isRemote) {
                try {
                    SftpSubsystemClient sftpSubsystemClient = null;

                    List<Object> sshActiveChannels = sshClient.getActiveChannels();
                    for (Object object : sshActiveChannels) {
                        if (object instanceof SftpSubsystemClient) {
                            sftpSubsystemClient = (SftpSubsystemClient) object;
                            break;
                        }
                    }

                    if (sftpSubsystemClient != null) {
                        FileAttributes fileAttributes = sftpSubsystemClient.getAttributes(path);
                        if (fileAttributes != null && !fileAttributes.isDirectory()) {
                            valid = true;
                        }
                    }
                } catch (IOException ioe) {
                    // Do nothing. The logging happens if this method returns false.
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
     * Attempts to open the SSH connection for the specified number of times and interval between
     * connections. This is done in case the connection attempt times out on the first try.
     *
     * @param sshClient SshClient
     * @param sshConnectionProperties SshConnectionProperties
     * @throws Exception exception
     */
    private void openSshConnection(SshClient sshClient, SshConnectionProperties sshConnectionProperties)
            throws Exception {
        Exception exception = null;

        int count = 1;
        while (!sshClient.isConnected() && count <= MAX_CONNECTION_ATTEMPTS) {
            if (count > 1) {
                try {
                    Thread.sleep(TIME_BETWEEN_ATTEMPTS);
                } catch (InterruptedException ie) {
                    // Do nothing.
                }
            }

            logMessage("Attempt " + count);

            try {
                sshClient.connect(sshConnectionProperties, new IgnoreHostKeyVerification());
            } catch (Exception e) {
                exception = e;
            }

            count++;
        }

        if (!sshClient.isConnected()) {
            throw exception;
        }
    }

    /**
     * Returns the file size after transferring it between client and host in the specified
     * direction. Absolute paths for both local and remote files are expected.
     *
     * @param sshClient SshClient
     * @param sftpClient SftpClient
     * @param localFilePath String
     * @param remoteFilePath String
     * @param direction int
     * @return long
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private long transferFile(SshClient sshClient,
                              SftpClient sftpClient,
                              String localFilePath,
                              String remoteFilePath,
                              int direction)
            throws IOException {
        if (AppGlobals.getSchedulerUseSecure()) {
            localFilePath = getSecureDirectoryPath() + localFilePath;
        }

        if (direction == DIRECTION_SEND) {
            logMessage("Transfer mode: Send");

            if (isValidDirectory(sftpClient, remoteFilePath, true) && isValidFile(sshClient, localFilePath, false)) {
                sftpClient.put(localFilePath, remoteFilePath);
            } else {
                throw new IOException("Could not validate 'Send' operation.");
            }
        } else {
            logMessage("Transfer mode: Receive");

            if (isValidDirectory(sftpClient, localFilePath, false) && isValidFile(sshClient, remoteFilePath, true)) {
                sftpClient.get(remoteFilePath, localFilePath);
            } else {
                throw new IOException("Could not validate 'Receive' operation.");
            }
        }

        return new File(localFilePath).length();
    }
}
