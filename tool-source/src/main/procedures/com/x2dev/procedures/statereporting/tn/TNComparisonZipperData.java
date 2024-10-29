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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
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
/* DEBUG */
import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.zip.CRC32;
import java.util.zip.CheckedInputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * Procedure TN Comparison Zipper.
 * Expects full path to file from input, renames it, and packs file in zip-archive.
 */
public class TNComparisonZipperData extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    // Default values
    private static final int SFTP_DEFAULT_PORT = 22;

    // Log message keys
    private static final String MSG_ERROR_CONFIGURATION = "procedure.fileTransfer.errConfig";
    private static final String MSG_ERROR_CONNECTION = "procedure.fileTransfer.errConnect";
    private static final String MSG_ERROR_TRANSFER = "procedure.fileTransfer.errTrans";
    private static final String MSG_ERROR_UNKNOWN = "procedure.fileTransfer.errUnknown";

    // Input parameter keys
    private static final String PARAM_DIRECTION = "transDirection";
    private static final String PARAM_LOCAL_FILE = "localFile";
    private static final String PARAM_MODE = "transMode";
    private static final String PARAM_PASSIVE = "passiveMode";
    private static final String PARAM_PASSWORD = "password";
    private static final String PARAM_PORT = "port";
    private static final String PARAM_REMOTE_FILE = "remoteFile";
    private static final String PARAM_SAVE_HISTORY = "saveHistory";
    private static final String PARAM_SERVER = "server";
    private static final String PARAM_USERID = "userId";

    // Other constants
    private static final int DIRECTION_SEND = 0;
    private static final int MAX_CONNECTION_ATTEMPTS = 3;
    private static final int MODE_ASCII = 0;
    private static final int TIME_BETWEEN_ATTEMPTS = 3000; // In milliseconds
    private static final int ZIP_BUFFER_SIZE = 4096;

    /**
     * Creates a zip file storing the payload. This does not use compression and is
     * believed to be the only method that will operate with the TN unzip utility.
     *
     * @param file The file loaded into the Zip File
     * @param zipFile File
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private static void zipFileToPath(File file, File zipFile) throws IOException {
        byte[] zipInputBuffer = new byte[ZIP_BUFFER_SIZE];
        FileOutputStream outputStream = new FileOutputStream(zipFile);
        ZipOutputStream zipOutStream = new ZipOutputStream(outputStream);
        // get byte count
        FileInputStream fileInStream = new FileInputStream(file);
        BufferedInputStream origin = new BufferedInputStream(fileInStream, ZIP_BUFFER_SIZE);
        int bytes = 0;
        int len;
        while ((len = origin.read(zipInputBuffer, 0, ZIP_BUFFER_SIZE)) > 0) {
            bytes += len;
        }
        origin.close();
        fileInStream.close();

        // get crc
        fileInStream = new FileInputStream(file);
        origin = new BufferedInputStream(fileInStream, ZIP_BUFFER_SIZE);
        CheckedInputStream originCheck = new CheckedInputStream(origin, new CRC32());
        while ((len = originCheck.read(zipInputBuffer, 0, ZIP_BUFFER_SIZE)) > 0) {
            continue;
        }
        long crcChecksum = originCheck.getChecksum().getValue();
        originCheck.close();
        origin.close();
        fileInStream.close();

        ZipEntry zipEntry = new ZipEntry(file.getName());
        zipEntry.setMethod(ZipEntry.STORED);
        zipEntry.setSize(bytes);
        zipEntry.setCrc(crcChecksum);
        zipOutStream.putNextEntry(zipEntry);

        fileInStream = new FileInputStream(file);
        while ((len = fileInStream.read(zipInputBuffer)) > 0) {
            zipOutStream.write(zipInputBuffer, 0, len);
        }
        fileInStream.close();

        zipOutStream.closeEntry();
        zipOutStream.close();
        outputStream.close();
    }

    // Fields
    private Map<String, Integer> m_deletesMap;
    private Map<String, Integer> m_tresholdsMap;

    // Other
    private TNImportEISHistory m_importHelper;
    private String m_outputName;
    private int m_recordsCount = 0;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        String fileName = (String) getParameter(PARAM_LOCAL_FILE);
        if (StringUtils.isEmpty(fileName)) {
            logMessage("Input parameter " + PARAM_LOCAL_FILE + ", not specified. Abort execution.");
            return;
        }
        logMessage("Transfer file is " + fileName);
        BufferedReader reader = null;
        try {
            StringBuilder newPath = new StringBuilder();
            if (AppGlobals.getSchedulerUseSecure()) {
                File secureRoot = AppGlobals.getSecureRootDirectory(getOrganization(),
                        getBroker().getPersistenceKey().getDeploymentId());
                if (secureRoot != null) {
                    newPath.append(secureRoot.getAbsolutePath());
                    newPath.append(File.separator);
                }
            }
            newPath.append(fileName);
            File file = new File(newPath.toString());// PROD
            // DEBUG File file = new File("/Users/kenbakke/Dropbox/Junk/96450910R01.EIS");
            reader = new BufferedReader(new FileReader(file));
            // First line should stands for export file name
            String firstLine = reader.readLine();
            if (firstLine != null) {
                firstLine = firstLine.trim();
            }
            if (populateDeletesMap(reader, firstLine)) // PROD
            {// PROD
                reader.close();// PROD
                return;// PROD
            } // PROD
            reader.close();
            populateTresholdsMap();// PROD
            boolean startTransfering = compareTresholdsAndDeletes();// PROD
            // DEBUG boolean startTransfering = true;
            if (!StringUtils.isEmpty(firstLine) && firstLine.endsWith(".EIS") && startTransfering) {
                m_outputName = firstLine.substring(18);
                File newFile = new File(file.getParent() + File.separator + m_outputName);
                if (!file.renameTo(newFile)) {
                    FileUtils.copyFile(file, newFile);
                }
                String zipFileName = m_outputName.replace(".EIS", ".zip");
                String path = newFile.getParent();
                String zipFilePath = path + File.separator + zipFileName;
                File zipFile = new File(zipFilePath);
                zipFileToPath(newFile, zipFile);
                boolean saveHistory = getParameter(PARAM_SAVE_HISTORY) == null ? false
                        : Boolean.TRUE.equals(getParameter(PARAM_SAVE_HISTORY));
                if (saveHistory)// PROD
                {// PROD
                    beforeTransfer(newFile);// PROD
                } // PROD
                transfer(zipFile);// PROD
            } else if (!startTransfering) {
                logMessage("The delete test was not successful.");
            } else {
                // if first line of file does not contain file suffix,
                // probably this is not legal file from TN Export
                logMessage("Specified file is not legal TN export file.");
            }
        } catch (FileNotFoundException fnfe) {
            logMessage(fnfe.getMessage());
            return;
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
    }

    /**
     * save file into permanent storage .
     *
     * @param newFile File
     * @throws Exception exception
     */
    private void beforeTransfer(File newFile) throws Exception {
        m_importHelper = new TNImportEISHistory(getOrganization(), getBroker(), getParameters());
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(newFile));

            String firstLine = reader.readLine();
            if (firstLine != null) {
                firstLine = firstLine.trim();
            }

            if (firstLine != null && firstLine.endsWith(".EIS")) {
                m_importHelper.setDate(firstLine);
                String lineToRead = null;
                while ((lineToRead = reader.readLine()) != null) {
                    if (!lineToRead.contains(".EIS") && lineToRead.length() > 5) {
                        m_recordsCount += m_importHelper.updateEISHistory(lineToRead);
                    }
                }
            }
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
    }

    /**
     * Compare tresholds and deletes for every record.</br>
     * If any thresholds are exceeded, write a message to the log file containing
     * <li>the record type</li>
     * <li>threshold</li>
     * <li>number of deletes</li>
     * for each record type that exceeds the threshold value.
     *
     * @return true, if successful
     */
    private boolean compareTresholdsAndDeletes() {
        boolean toTransferFile = true;

        if (m_deletesMap != null && m_tresholdsMap != null) {
            for (String deletesKey : m_deletesMap.keySet()) {
                if (m_tresholdsMap.containsKey(deletesKey)
                        && m_deletesMap.get(deletesKey).intValue() > m_tresholdsMap.get(deletesKey).intValue()) {
                    toTransferFile = false;
                    logMessage("Record type : " + deletesKey + ". Threshold : " + m_tresholdsMap.get(deletesKey)
                            + " Deletes : " + m_deletesMap.get(deletesKey));
                } else if (!m_tresholdsMap.containsKey(deletesKey) && m_deletesMap.get(deletesKey).intValue() > 0) {
                    toTransferFile = false;
                    logMessage("Record type : " + deletesKey + ". Threshold : 0 (default value). " + "Deletes : "
                            + m_deletesMap.get(deletesKey));
                }
            }
        }

        if (toTransferFile) {
            logMessage("The delete test was successful. Start processing FTP transfer...");
        }

        return toTransferFile;
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
     * Read every line of file and populate map keyed with record id and valued with count of
     * deletes per record id.
     *
     * @param reader BufferedReader
     * @param firstLine String
     * @return true, if successful
     * @throws Exception exception
     */
    private boolean populateDeletesMap(BufferedReader reader, String firstLine) throws Exception {
        boolean invalidRecord = false;
        if (m_deletesMap == null) {
            m_deletesMap = new HashMap<String, Integer>();
        }
        if (!StringUtils.isEmpty(firstLine) && firstLine.endsWith(".EIS")) {
            String lineToRead = null;
            while ((lineToRead = reader.readLine()) != null) {
                if (!lineToRead.contains(".EIS")) {
                    if (lineToRead.length() > 5 && lineToRead.substring(0, 5).matches("\\d{5}")
                            && lineToRead.substring(5, 6).matches("[NED]")) {
                        String recordType = lineToRead.substring(5, 6);
                        if ("D".equals(recordType)) {
                            String key = lineToRead.substring(0, 3);
                            Integer value = m_deletesMap.get(key);
                            if (value == null) {
                                value = Integer.valueOf(1);
                                m_deletesMap.put(key, value);
                            } else {
                                value = Integer.valueOf(value.intValue() + 1);
                                m_deletesMap.put(key, value);
                            }
                        }
                    } else {
                        invalidRecord = true;
                        logMessage("Invalid line found.[" + lineToRead + "]");
                        return invalidRecord;
                    }
                }
            }
        }
        return invalidRecord;
    }

    /**
     * Get comment from the procedure and put record id as a key to the map valued with tresholds.
     */
    private void populateTresholdsMap() {
        if (m_tresholdsMap == null) {
            m_tresholdsMap = new HashMap<String, Integer>();
        }
        String comment = null;
        Procedure procedure = (Procedure) getJob().getTool();
        if (procedure != null) {
            comment = procedure.getComment() != null ? procedure.getComment() : "";
        }
        String[] tresholds = comment.split("\r\n");
        for (int i = 0; i < tresholds.length; i++) {
            String putToMap = tresholds[i];
            if (putToMap != null) {
                String[] treshold = putToMap.split(",");
                if (treshold != null && treshold.length > 1 && treshold[1].matches("\\d+")) {
                    m_tresholdsMap.put(treshold[0], Integer.valueOf(treshold[1]));
                }
            }
        }
    }

    /**
     * Performs file transfer.
     *
     * @param localFile File
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void transfer(File localFile) throws IOException {
        Session session = null;
        Channel channel = null;
        try {
            String userName = (String) getParameter(PARAM_USERID);
            String host = (String) getParameter(PARAM_SERVER);
            String password = (String) getParameter(PARAM_PASSWORD);
            int port = getParameter(PARAM_PORT) != null ? ((Integer) getParameter(PARAM_PORT)).intValue()
                    : SFTP_DEFAULT_PORT;
            JSch ssh = new JSch();
            JSch.setConfig("StrictHostKeyChecking", "no");
            session = ssh.getSession(userName, host, port);
            session.setPassword(password);
            logMessage("Opening SSH connection...");
            session.connect();
            if (session.isConnected()) {
                channel = session.openChannel("sftp");
                channel.connect();
                ChannelSftp sftp = (ChannelSftp) channel;
                KeyValuePair<String, String> pathElements = FolderUtils.getPathElements(localFile.getAbsolutePath());
                String remoteFilePath =
                        (String) getParameter(PARAM_REMOTE_FILE) + File.separator + pathElements.getValue();
                int direction = ((Integer) getParameter(PARAM_DIRECTION)).intValue();
                logMessage("Starting file transfer...");
                transferFile(sftp, localFile, remoteFilePath, direction);
                if (m_recordsCount > 0) {
                    logMessage("File: " + m_outputName + ". Number of history records saved: "
                            + Integer.valueOf(m_recordsCount) + " .");
                } else {
                    logMessage("File: " + m_outputName + ". History was not saved.");
                }
                logMessage("File transfer completed. Transferred " + localFile.length() + " bytes.");
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
     * Returns the file size after transferring it between client and host in the specified
     * direction. Absolute paths for both local and remote files are expected.
     *
     * @param sftpClient ChannelSftp
     * @param localFile File
     * @param remoteFilePath String
     * @param direction int
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws SftpException exception
     */
    private void transferFile(ChannelSftp sftpClient,
                              File localFile,
                              String remoteFilePath,
                              int direction)
            throws IOException, SftpException {
        if (direction == DIRECTION_SEND) {
            logMessage("Transfer mode: Send");
            if (isValidDirectory(sftpClient, remoteFilePath, true)
                    && isValidFile(sftpClient, localFile.getAbsolutePath(), false)) {
                sftpClient.put(localFile.getAbsolutePath(), remoteFilePath);
            } else {
                throw new IOException("Could not validate 'Send' operation.");
            }
        } else {
            logMessage("Transfer mode: Receive");

            if (isValidDirectory(sftpClient, localFile.getAbsolutePath(), false)
                    && isValidFile(sftpClient, remoteFilePath, true)) {
                sftpClient.get(remoteFilePath, localFile.getAbsolutePath());
            } else {
                throw new IOException("Could not validate 'Receive' operation.");
            }
        }
    }
}
