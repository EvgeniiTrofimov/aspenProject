/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2015 X2 Development Corporation.
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
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import org.apache.commons.net.PrintCommandListener;
import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPSClient;
import org.apache.commons.net.util.TrustManagerUtils;

/**
 * Procedure to perform the transfer of a file to or from a remote server using FTPS protocol over
 * TLS/SSL connection using the apache commons-net library (Jakarta).
 * <p>
 * <table style="border-collapse: collapse; border: 1px solid red;">
 * <tr>
 * <td style=" padding:10px;">
 * <b>NOTE:</b>&nbsp;
 * This procedure can be enhanced to be THE single FTP procedure if enough parameters are set and
 * handled to manage all cases of FTP: FTP, SFTP, FTPS implicit/explicit, etc.
 * </td>
 * </tr>
 * </table>
 *
 * @author X2 Development Corporation
 */
public class SslFileTransferProcedure extends ProcedureJavaSource {
    // Other constants
    private static final int DIRECTION_SEND = 0;

    // Input parameters
    private static final String PARAM_DIRECTION = "direction";
    private static final String PARAM_LOCAL_FILE = "localFile";
    private static final String PARAM_PASSWORD = "password";
    private static final String PARAM_REMOTE_FILE = "remoteFile";
    private static final String PARAM_SERVER = "server";
    private static final String PARAM_USERID = "userId";

    private static final long serialVersionUID = 1L;

    private FTPFile[] m_fileList = null;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        FTPSClient ftps = null;
        try {
            logMessage("Opening TLS/SSL connection...");
            ftps = openSSLConnection();

            if (ftps.isConnected()) {
                String localFilePath = (String) getParameter(PARAM_LOCAL_FILE);
                String remoteFilePath = (String) getParameter(PARAM_REMOTE_FILE);
                int direction = ((Integer) getParameter(PARAM_DIRECTION)).intValue();

                logMessage("Starting file transfer...");
                long fileSize = transferFile(ftps, localFilePath, remoteFilePath, direction);
                logMessage("File transfer completed. Transfered " + fileSize + " bytes.");

                logMessage("Closing FTP connection...");
                ftps.disconnect();
            } else {
                logMessage("Connection failed.");
            }
        } catch (Exception e) {
            logMessage(getErrorMessage(e, "Operation error."));
        } finally {
            if (ftps != null && ftps.isConnected()) {
                logMessage("Closing TLS/SSL connection...");
                ftps.logout();
                ftps.disconnect();
                logMessage("TLS/SSL connection closed.");
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

        PrintWriter printWriter = null;
        try {
            printWriter = new PrintWriter(stringWriter);
            exception.printStackTrace(printWriter);
        } catch (Exception e) {
            throw e;
        } finally {
            if (printWriter != null) {
                printWriter.flush();
                printWriter.close();
            }
        }
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
     * @param ftps FTPSClient
     * @param path String
     * @param isRemote boolean
     * @return boolean
     */
    private boolean isValidDirectory(FTPSClient ftps, String path, boolean isRemote) {
        boolean valid = false;
        String directoryPath = null;

        if (path != null) {
            KeyValuePair<String, String> pathElements = FolderUtils.getPathElements(path);

            directoryPath = pathElements.getKey();
            if (directoryPath != null) {
                if (isRemote) {
                    try {
                        logMessage("Validating directory " + directoryPath);
                        ftps.changeWorkingDirectory(directoryPath);
                        valid = true;
                    } catch (Exception e) {
                        // Do nothing. The logging happens if this method returns false.
                        logMessage(e.getMessage());
                    }
                } else {
                    try {
                        valid = new File(directoryPath).isDirectory();
                    } catch (SecurityException se) {
                        logMessage(
                                getErrorMessage(se, "Cannot access '" + directoryPath + "'. Read privilege missing."));
                    }
                }
            } else {
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
     * @param ftps FTPSClient
     * @param path String
     * @param isRemote boolean
     * @return boolean
     */
    private boolean isValidFile(FTPSClient ftps, String path, boolean isRemote) {
        boolean valid = false;

        if (path != null) {
            if (isRemote) {
                if (m_fileList != null && m_fileList.length > 0) {
                    for (FTPFile f : m_fileList) {
                        if (f.getName().equals(path)) {
                            valid = true;
                            break;
                        }
                    }
                } else {
                    valid = true; // punt if the directory is empty
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
     * Attempts to open the SSL connection for the specified number of times and interval between
     * connections. This is done in case the connection attempt times out on the first try.
     *
     * @return FTPConnection
     * @throws Exception exception
     */
    private FTPSClient openSSLConnection() throws Exception {
        String hostname = (String) getParameter(PARAM_SERVER);
        String username = (String) getParameter(PARAM_USERID);
        String password = (String) getParameter(PARAM_PASSWORD);

        FTPSClient ftps = new FTPSClient("SSL", true);
        // suppress login details
        ftps.addProtocolCommandListener(new PrintCommandListener(new PrintWriter(System.out), true));

        ftps.connect(hostname);
        Exception exception = null;
        try {
            ftps.setBufferSize(1024 * 1024);
            ftps.enterLocalPassiveMode();
            ftps.setAutodetectUTF8(true);
            ftps.setTrustManager(TrustManagerUtils.getAcceptAllTrustManager());

            logMessage("Connected to " + hostname + " on " + ftps.getDefaultPort());

            ftps.enterLocalPassiveMode();
            if (!ftps.login(username, password)) {
                ftps.logout();
            }

            /*
             * RFC2228 - PBSZ (Protection Buffer Size) and PROT (Data Channel Protection Level)
             * PBSZ must be set before setting PROT. Setting the protection level to "P" seems
             * to imply "required implicit FTP over TLS."
             *
             * The PBSZ command is intended to define the buffer-size to be used by the security
             * mechanism when it is encrypting data on the data-channel. However for TLS this
             * setting is redundant and a value of '0' is always passed as a parameter.
             *
             * PROT defines whether or not the data channel is to be protected or not. Either the
             * data channel is Clear (the default), or Private. Clear means that no security is
             * used on the data-channel (meaning files are transmitted without encryption), and
             * Private means that the data-channel should be encrypted. So there are two
             * possible PROT commands:
             */
            ftps.execPBSZ(0);
            ftps.execPROT("P");

            ftps.enterLocalPassiveMode();
            m_fileList = ftps.listFiles("./");
            for (FTPFile f : m_fileList) {
                System.out.println(f.toFormattedString());
                System.out.println((f.isDirectory() ? "/" : " ") + f.getName());
            }

            ftps.setFileType(FTP.BINARY_FILE_TYPE);
        } catch (Exception e) {
            exception = e;
            logMessage(getErrorMessage(e, "Exception connecting to '" + hostname + "'."));
        } finally {
            if (exception != null) {
                ftps.disconnect();
            }
        }

        return ftps;
    }

    /**
     * Returns the file size after transferring it between client and host in the specified
     * direction. Absolute paths for both local and remote files are expected.
     *
     * @param ftps FTPSClient
     * @param localFilePath String
     * @param remoteFilePath String
     * @param direction int
     * @return long
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private long transferFile(FTPSClient ftps,
                              String localFilePath,
                              String remoteFilePath,
                              int direction)
            throws IOException {
        if (AppGlobals.getSchedulerUseSecure()) {
            localFilePath = getSecureDirectoryPath() + localFilePath;
        }

        if (direction == DIRECTION_SEND) {
            logMessage("Transfer mode: Send");

            if (isValidDirectory(ftps, remoteFilePath, true) && isValidFile(ftps, localFilePath, false)) {
                try {
                    logMessage("Deleting old file... " + remoteFilePath);
                    ftps.deleteFile(remoteFilePath);
                } catch (Exception ex) {
                    logMessage(getErrorMessage(ex, " when deleting file."));
                }

                logMessage("Uploading file... " + localFilePath);
                try {
                    InputStream input = null;

                    try {
                        input = new FileInputStream(localFilePath);
                        ftps.storeFile(remoteFilePath, input);
                    } catch (Exception e) {
                        throw e;
                    } finally {
                        if (input != null) {
                            input.close();
                        }
                    }
                } catch (Exception e) {
                    logMessage(getErrorMessage(e, "uploadFile"));
                    throw e;
                }
            } else {
                throw new IOException("Could not validate 'Send' operation.");
            }
        } else {
            logMessage("Transfer mode: Receive");

            if (isValidDirectory(ftps, localFilePath, false) && isValidFile(ftps, remoteFilePath, true)) {
                try {
                    OutputStream output;

                    output = new FileOutputStream(localFilePath);

                    ftps.enterLocalPassiveMode();
                    ftps.retrieveFile(remoteFilePath, output);

                    output.close();
                } catch (Exception e) {
                    logMessage(getErrorMessage(e, "downloadFile"));
                    throw e;
                }
            } else {
                throw new IOException("Could not validate 'Receive' operation.");
            }
        }

        ftps.noop(); // check that control connection is working OK

        File localFile = new File(localFilePath);

        return localFile.length();
    }

}
