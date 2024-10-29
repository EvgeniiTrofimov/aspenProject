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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.ZipUtils;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.zip.ZipException;
import org.apache.commons.io.FileUtils;
import org.apache.struts.util.MessageResources;

/**
 * Procedure to import IEP data.
 *
 * @author Follett Software Company
 */
public class EISHistoryUpdate extends TextImportJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class FileComparator.
     */
    class FileComparator implements Comparator<File> {

        /**
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(File o1, File o2) {
            return o1.getName().compareTo(o2.getName());
        }
    }

    private TNImportEISHistory m_importHelper;
    private List<String> m_messages = new LinkedList();

    /**
     * Exports a text file detailing the results of the import.
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);

        MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

        buffer.append("EIS History Update Results");
        buffer.append('\n');
        buffer.append(resources.getMessage(getLocale(), "message.import.results.doubleRule"));
        buffer.append('\n');
        buffer.append('\n');

        if (m_messages.isEmpty()) {
            buffer.append(resources.getMessage(getLocale(), "message.procedure.results.noMessages"));
        } else {
            Iterator messages = m_messages.iterator();
            while (messages.hasNext()) {
                String key = (String) messages.next();

                String message = getMessage(key, resources);

                buffer.append(message);
                buffer.append('\n');
            }
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
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 0;
    }

    /**
     * Returns the value of the passed resource key. If the key is not found, the key itself is
     * returned.
     *
     * @param key String
     * @param resources MessageResources
     * @return String
     */
    protected String getMessage(String key, MessageResources resources) {
        String message = resources.getMessage(getLocale(), key);
        if (message == null) {
            message = key;
        }

        return message;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        File targetDirectory = FolderUtils.createUniqueFolder(AppGlobals.getRootTemporaryFolder());

        ArrayList<File> unzippedFiles = readZipFile(sourceFile, targetDirectory);

        for (File file : unzippedFiles) {
            BufferedReader reader = null;
            int recordsCount = 0;
            try {
                if (getCharacterEncoding() != null && Charset.isSupported(getCharacterEncoding())) {
                    reader = new BufferedReader(
                            new InputStreamReader(new FileInputStream(file), getCharacterEncoding()));
                } else {
                    reader = new BufferedReader(new FileReader(file));
                }
            } catch (FileNotFoundException fnfe) {
                /**
                 * Do nothing as we can read empty folder
                 */
            }

            if (reader != null) {
                try {
                    String firstLine = reader.readLine().trim();

                    if (firstLine != null && firstLine.endsWith(".EIS")) {
                        m_importHelper = new TNImportEISHistory(getOrganization(), getBroker(), getParameters());
                        m_importHelper.setDate(firstLine);
                        recordsCount = readRecords(reader);
                    }
                } finally {
                    reader.close();
                }
            }

            if (file.getName().endsWith(".EIS")) {
                logMessage("File: " + file.getName() + "; Number of history records saved: "
                        + Integer.valueOf(recordsCount) + " .");
            }

        }

        FileUtils.deleteDirectory(targetDirectory);
    }

    /**
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        return;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
    }

    /**
     * Logs a message that will be written to the results file. Each message is written on its own
     * line.
     *
     * @param key a resource key, if a corresponding message cannot be found then the key itself
     *        will be logged
     */
    protected void logMessage(String key) {
        m_messages.add(key);
    }

    /**
     * Read every line of file and populate map keyed with record id and valued with count of
     * deletes per record id.
     *
     * @param reader BufferedReader
     * @return int
     * @throws Exception exception
     */
    private int readRecords(BufferedReader reader) throws Exception {
        String lineToRead = null;
        int recordCount = 0;
        while ((lineToRead = reader.readLine()) != null) {
            if (!lineToRead.contains(".EIS") && lineToRead.length() > 5) {
                recordCount += m_importHelper.updateEISHistory(lineToRead);
            }
        }

        return recordCount;
    }

    /**
     * Get zip file from input and unzip it to disk.
     *
     * @param sourceFile File
     * @param targetDirectory File
     * @return list of files from zip
     * @throws ZipException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private ArrayList<File> readZipFile(File sourceFile, File targetDirectory) throws ZipException, IOException {
        ArrayList<File> files = new ArrayList<File>();
        if (sourceFile.exists() && sourceFile.toString().endsWith(".zip")) {
            ZipUtils.unzipFilesAndFoldersToDisk(sourceFile, targetDirectory, false);

            File[] dirs = targetDirectory.listFiles();
            ArrayList<File> listOfZips = new ArrayList<File>();

            for (File file : dirs) {
                if (file.isDirectory()) {
                    listOfZips.addAll(Arrays.asList(file.listFiles()));
                }
            }

            for (File file : listOfZips) {
                if (file.exists() && file.getName().endsWith(".zip")) {
                    ZipUtils.unzipFilesAndFoldersToDisk(file, targetDirectory, false);
                }
            }
            files.addAll(Arrays.asList(targetDirectory.listFiles()));
        }

        Collections.sort(files, new FileComparator());
        return files;
    }
}
