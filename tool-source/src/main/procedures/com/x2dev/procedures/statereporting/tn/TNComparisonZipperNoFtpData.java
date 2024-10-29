/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) 2020 Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */


package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
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
public class TNComparisonZipperNoFtpData extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    // Input parameter keys
    private static final String PARAM_LOCAL_FILE = "localFile";
    private static final String PARAM_SAVE_HISTORY = "saveHistory";

    // Other constants
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
        logMessage("Source file is " + fileName);
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
                    logMessage("Record type : " + deletesKey + ". Treshold : " + m_tresholdsMap.get(deletesKey)
                            + " Deletes : " + m_deletesMap.get(deletesKey));
                } else if (!m_tresholdsMap.containsKey(deletesKey) && m_deletesMap.get(deletesKey).intValue() > 0) {
                    toTransferFile = false;
                    logMessage("Record type : " + deletesKey + ". Treshold : 0 (default value). " + "Deletes : "
                            + m_deletesMap.get(deletesKey));
                }
            }
        }

        if (toTransferFile) {
            logMessage("The delete test was successful.");
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
                                value = new Integer(1);
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
}
