/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.ColumnQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.JobEntry;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.JSchException;
import com.jcraft.jsch.Session;
import com.jcraft.jsch.SftpException;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.io.FileUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * Procedure to perform the transfer of a file to or from a remote server using SFTP protocol over
 * SSH connection.
 *
 * @author X2 Development Corporation
 */
public class FtpFileTransferProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    // Input parameters
    private static final String PARAM_DEBUG = "debug";
    private static final String PARAM_DOE_PIN_ALIAS = "studentDoePinAlias";
    private static final String PARAM_IMPORT_FLAG_ALIAS = "programImportFlagAlias";
    private static final String PARAM_IMPORT_TYPE_ALIAS = "programImportTypeAlias";
    private static final String PARAM_PASSWORD = "password";
    private static final String PARAM_PORT = "port";
    private static final String PARAM_REMOTE_FILE = "remoteFile";
    private static final String PARAM_SERVER = "server";
    private static final String PARAM_USERID = "userId";

    // Other constants
    private static final String DATE_PATTERN = "\\d{4}-\\d{2}-\\d{2}";
    private static final String FILE_NAME_PATTERN = "TN-EIS-Combined-Extract-TN Report-\\d{4}-\\d{2}-\\d{2}.txt";
    private static final String FILE_NAME_STARTING = "TN-EIS-Combined-Extract-TN Report-";
    private static final String PROCEDURE_ID = "TN-FTP-TRANSFER";
    private static final Object STATUS_COMPLETE = Integer.valueOf(JobResult.StatusCode.COMPLETE.ordinal());

    // Member variables
    String m_doePinAlias;
    String m_importFlagAlias;
    String m_importTypeAlias;
    List<KeyValuePair<Integer, String>> m_invalidRecords;

    protected String m_password;

    private Map<String, File> m_filesByDateMap;
    private ArrayList<Date> m_filesDates;
    private String m_finalPath;
    private X2Broker m_modelBroker;
    private Procedure m_procedure;

    /**
     * Log debug.
     *
     * @param message String
     */
    public void logDebug(String message) {
        if (getParameter(PARAM_DEBUG) != null && getParameter(PARAM_DEBUG) instanceof Boolean
                && ((Boolean) getParameter(PARAM_DEBUG)).booleanValue()) {
            logMessage(message);
        }
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.x2dev.sis.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        Session session = null;
        Channel channel = null;
        boolean fileTransferComplete = false;

        initializeFileds();

        try {
            String userName = (String) getParameter(PARAM_USERID);
            String host = (String) getParameter(PARAM_SERVER);
            String password = (String) getParameter(PARAM_PASSWORD);
            int port = ((Integer) getParameter(PARAM_PORT)).intValue();
            String remoteFilePath = (String) getParameter(PARAM_REMOTE_FILE);
            File root = AppGlobals.getSecureRootDirectory(getOrganization(),
                    getBroker().getPersistenceKey().getDeploymentId());
            String localFilePath = root.getAbsolutePath();

            JSch ssh = new JSch();
            JSch.setLogger(new TNSshLogger());
            JSch.setConfig("StrictHostKeyChecking", "no");
            session = ssh.getSession(userName, host, port);
            session.setPassword(password);

            logMessage("Opening SSH connection...");
            session.connect();

            if (session.isConnected()) {
                channel = session.openChannel("sftp");
                channel.connect();

                ChannelSftp sftp = (ChannelSftp) channel;


                logMessage("Starting file transfer...");
                logMessage("Transfer mode: Receive");

                localFilePath = getLocalAbsolutePath(localFilePath);

                logMessage("localFilePath: " + localFilePath);
                logMessage("Transfer mode: Receive");

                m_finalPath = getLastRemoteDirectoryName(localFilePath, remoteFilePath);

                File mkDir = new File(m_finalPath);

                if (!mkDir.exists()) {
                    mkDir.mkdirs();
                }
                if (isValidDirectory(sftp, m_finalPath, false) && isValidDirectory(sftp, remoteFilePath, true)) {
                    Vector<ChannelSftp.LsEntry> list = sftp.ls(remoteFilePath); // List source
                                                                                // directory
                                                                                // structure.
                    for (ChannelSftp.LsEntry item : list) {
                        if (!item.getAttrs().isDir()) {
                            String remoteFile = remoteFilePath + "/" + item.getFilename();
                            String localFile = m_finalPath + "/" + item.getFilename();
                            sftp.get(remoteFile, localFile);
                            logMessage("File transferred - " + remoteFile);
                        }
                    }
                } else {
                    throw new IOException("Could not validate 'Receive' operation.");
                }
                File finalDir = new File(m_finalPath);

                if (finalDir.exists() && finalDir.isDirectory()) {
                    logMessage("Directory has been transferred - " + m_finalPath);
                    fileTransferComplete = true;
                } else {
                    throw new IOException("Could not transfer directory " + m_finalPath);
                }
            }
        } catch (SftpException | JSchException e) {
            logMessage(getErrorMessage(e, "Failed to connect "));
        } finally {
            try {
                JSch.setLogger(null);
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

        if (fileTransferComplete) {
            readTransferredFiles();

            /**
             * Getting comment from last proceed import
             */
            String comment = getProcedureComment();

            findFilesToImport(comment);

            try {
                executeEasyIEPImport();
            } catch (Exception e) {
                logMessage("SSH connection closed.");
            } finally {
                try {
                    FileUtils.deleteDirectory(new File(m_finalPath));
                } catch (Exception exception) {
                    StringWriter sw = new StringWriter();
                    PrintWriter pw = new PrintWriter(sw);
                    if (exception.getMessage() != null) {
                        pw.println(exception.getMessage());
                    }
                    exception.printStackTrace(pw);
                    logMessage("Exception on FileUtils.deleteDirectory for " + m_finalPath + "\n" + sw.toString());
                }
            }
        }

    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        return m_modelBroker;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#isRunOnApplicationServer()
     */
    @Override
    protected boolean isRunOnApplicationServer() {
        return true;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        m_modelBroker = new ModelBroker(userData);
    }

    /**
     * Import file.
     */
    private void executeEasyIEPImport() {
        String key = "";
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd");

        File fileToImport = null;

        JobResult result = null;

        if (this.getJob().getJobResultOid() != null) {
            result = (JobResult) getBroker().getBeanByOid(JobResult.class, this.getJob().getJobResultOid());
        }

        for (Date date : m_filesDates) {

            key = df.format(date);

            if ((fileToImport = m_filesByDateMap.get(key)) != null) {
                EasyIEPImportForFtpTransfer importProcedure =
                        new EasyIEPImportForFtpTransfer(fileToImport, getBroker(), getOrganization());
                try {
                    importProcedure.initialize();
                } catch (X2BaseException x2be) {
                    logMessage("Error while initializing import procedure.");
                    break;
                }

                try {
                    String fileName = FILE_NAME_STARTING + key + ".txt";
                    logMessage("Starting import " + fileName + " ...");
                    importProcedure.importData(fileToImport);
                    importProcedure.logImportResults(fileName);
                    logMessage(importProcedure.getImportBuffer().toString());
                    logMessage("Completed importing " + fileName + " .\n");
                    result.setComments(key);

                    if (result.isDirty()) {
                        getBroker().saveBeanForced(result);
                    }

                    m_procedure.setComment(key);

                    if (m_procedure.isDirty()) {
                        getBroker().saveBeanForced(m_procedure);
                    }
                } catch (Exception e) {
                    logMessage("Error while importing file.");
                    break;
                }

            }
        }
    }

    /**
     * Populate list of files to import.<br>
     * The purpose of this method is to get dates of files, sort it and according
     * last comment and get only files where not proceed.
     *
     * @param comment String
     */
    private void findFilesToImport(String comment) {
        DateFormat df = new SimpleDateFormat("yyyy-MM-dd");
        Date lastCommentConverted = null;

        if (m_filesDates == null) {
            m_filesDates = new ArrayList<Date>();
        }

        if (comment != null) {
            try {
                lastCommentConverted = df.parse(comment);
            } catch (ParseException e) {
                logMessage("Wrong comment format was set last time or was absent.");
            }
        }

        Set<String> keySet = null;
        if (m_filesByDateMap != null) {
            keySet = m_filesByDateMap.keySet();

            for (String key : keySet) {
                try {
                    Date keyDate = df.parse(key);
                    m_filesDates.add(keyDate);
                } catch (ParseException e) {
                    logMessage("Wrong file format.");
                }
            }
        }

        Collections.sort(m_filesDates);

        if (m_filesDates != null && m_filesDates.contains(lastCommentConverted)) {
            for (int i = m_filesDates.indexOf(lastCommentConverted); i > -1; i--) {
                m_filesDates.remove(i);
            }
        }
    }

    /**
     * Looking for the last result of the job and return its comment.
     *
     * @return String
     */
    @SuppressWarnings("unused")
    private String getLastResultComment() {
        String comment = "";
        Date tempDate = null;
        X2Criteria jobCriteria = new X2Criteria();
        jobCriteria.addEqualTo(JobEntry.COL_TOOL_OID, getJob().getTool().getOid());
        SubQuery jobSubQuery = new SubQuery(JobEntry.class, X2BaseBean.COL_OID, jobCriteria);

        X2Criteria jobResultsCriteria = new X2Criteria();

        jobResultsCriteria.addIn(JobResult.REL_JOB_ENTRY + "." + X2BaseBean.COL_OID, jobSubQuery);
        jobResultsCriteria.addEqualTo(JobResult.COL_STATUS, STATUS_COMPLETE);

        QueryByCriteria jrsQuery = new QueryByCriteria(JobResult.class, jobResultsCriteria);

        Collection<JobResult> results = getBroker().getCollectionByQuery(jrsQuery);

        for (JobResult jrs : results) {
            Date jrsEndDate = new Date(jrs.getEndTime() * 1000);

            if (tempDate == null || jrsEndDate.after(tempDate)) {
                tempDate = jrsEndDate;
                comment = jrs.getComments();
            }
        }

        return comment;
    }

    /**
     * Returns the absolute path for the local file.
     *
     * @param fileName String
     * @return String absolute path
     */
    private String getLocalAbsolutePath(String fileName) {
        StringBuilder absolutePath = new StringBuilder(512);

        if (AppGlobals.getSchedulerUseSecure()) {
            String deploymentId = null;
            if (getOrganization().getPersistenceKey() != null) {
                deploymentId = getOrganization().getPersistenceKey().getDeploymentId();
            }

            logMessage("Org: " + getOrganization().getName() + " Deployment: " + deploymentId);

            File secureRootDirectory = AppGlobals.getSecureRootDirectory(getOrganization(), deploymentId);
            if (secureRootDirectory != null && secureRootDirectory.isDirectory()) {
                absolutePath.append(secureRootDirectory.getAbsolutePath());
            }
        }

        if (absolutePath.length() == 0) {
            absolutePath.append(fileName);
        }

        return absolutePath.toString();
    }

    /**
     * Get comment from the procedure .
     *
     * @return String
     */
    private String getProcedureComment() {
        String comment = "";
        X2Criteria prcCriteria = new X2Criteria();
        prcCriteria.addEqualTo(Procedure.COL_ID, PROCEDURE_ID);
        QueryByCriteria prcQuery = new BeanQuery(Procedure.class, prcCriteria);
        m_procedure = (Procedure) getBroker().getBeanByQuery(prcQuery);

        if (m_procedure != null) {
            comment = m_procedure.getComment() != null ? m_procedure.getComment() : "";
        }
        return comment;
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
     * Gets the last remote directory name.
     *
     * @param localFilePath String
     * @param path String
     * @return String
     */
    private String getLastRemoteDirectoryName(String localFilePath, String path) {
        String directoryName = null;

        // Check for valid path separator.
        int position = 0;

        /*
         * Check for all forms of path separators.
         * The remote file may be a different system type.
         */
        position = path.lastIndexOf("\\\\"); // Look for double backslash in the
                                             // path. (Dos/Windows)
        if (position != -1) {
            directoryName = path.substring(position + 2);
        } else {
            position = path.lastIndexOf("\\"); // Look for single backslash in
                                               // the path. (Dos/Windows)
            if (position != -1) {
                directoryName = path.substring(position + 1);
            } else {
                position = path.lastIndexOf("/"); // Look for single slash in
                                                  // the path. (Unix)
                if (position != -1) {
                    directoryName = path.substring(position + 1);
                } else {
                    directoryName = path;
                }
            }
        }

        return localFilePath + File.separator + directoryName;
    }

    /**
     * Initialize fields.
     */
    private void initializeFileds() {
        m_invalidRecords = new LinkedList<KeyValuePair<Integer, String>>();
        m_doePinAlias = (String) getParameter(PARAM_DOE_PIN_ALIAS);
        m_importFlagAlias = (String) getParameter(PARAM_IMPORT_FLAG_ALIAS);
        m_importTypeAlias = (String) getParameter(PARAM_IMPORT_TYPE_ALIAS);
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
     * 1. Loop all transferred form remote directory.<br>
     * 2. Check if files' names match given pattern.<br>
     * 3. Getting date from file name.<br>
     * 4. Populate map keyed on file's name date with files as values.
     */
    private void readTransferredFiles() {
        Pattern datePattern = Pattern.compile(DATE_PATTERN);

        if (m_filesByDateMap == null) {
            m_filesByDateMap = new HashMap<String, File>();
        }

        File directory = new File(m_finalPath);

        if (directory != null && directory.exists() && directory.isDirectory()) {
            File[] listOfFiles = directory.listFiles();

            for (int i = 0; i < listOfFiles.length; i++) {
                File file = listOfFiles[i];
                String output = "";
                if (file.isFile() && file.getName() != null && file.getName().matches(FILE_NAME_PATTERN)) {
                    Matcher match = datePattern.matcher(file.getName());
                    if (match.find()) {
                        output = match.group();
                    }
                }

                m_filesByDateMap.put(output, file);
            }
        }
    }

    /**
     * The Class TNSshLogger.
     */
    private class TNSshLogger implements com.jcraft.jsch.Logger {
        java.util.Hashtable name = new java.util.Hashtable();

        /**
         * Instantiates a new TN ssh logger.
         */
        public TNSshLogger() {
            name.put(Integer.valueOf(DEBUG), "DEBUG: ");
            name.put(Integer.valueOf(INFO), "INFO: ");
            name.put(Integer.valueOf(WARN), "WARN: ");
            name.put(Integer.valueOf(ERROR), "ERROR: ");
            name.put(Integer.valueOf(FATAL), "FATAL: ");
        }

        /**
         * @see com.jcraft.jsch.Logger#isEnabled(int)
         */
        @Override
        public boolean isEnabled(int level) {
            return true;
        }

        /**
         * @see com.jcraft.jsch.Logger#log(int, java.lang.String)
         */
        @Override
        public void log(int level, String message) {
            logDebug(name.get(Integer.valueOf(level)) + message);
        }
    }

    /**
     * Helper class to import data files.
     *
     * @author X2 Development Corporation
     *
     */
    public class EasyIEPImportForFtpTransfer extends EasyIEPImport {

        File m_file;
        Organization m_organization;

        private int m_ftpInsertCount = 0;
        private int m_ftpMatchCount = 0;
        private int m_ftpSkipCount = 0;
        private int m_ftpUpdateCount = 0;
        private StringBuilder m_importBuffer;
        private X2Broker m_broker;

        /**
         * Instantiates a new easy IEP import for ftp transfer.
         *
         * @param file File
         * @param broker X2Broker
         * @param organization Organization
         */
        public EasyIEPImportForFtpTransfer(File file, X2Broker broker, Organization organization) {
            m_broker = broker;
            m_file = file;
            m_organization = organization;

        }

        /**
         * Gets the file.
         *
         * @return the m_file
         */
        public File getFile() {
            return m_file;
        }

        /**
         * Gets the import buffer.
         *
         * @return the m_importBuffer
         */
        public StringBuilder getImportBuffer() {
            return m_importBuffer;
        }

        /**
         * @see com.x2dev.procedures.statereporting.tn.EasyIEPImport#getBroker()
         */
        @Override
        protected X2Broker getBroker() {
            return m_broker;
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
            super.importData(m_file);
        }

        /**
         * Always commit changes when running in transfer and import mode
         *
         * @see com.x2dev.procedures.statereporting.tn.EasyIEPImport#commitChanges()
         */
        @Override
        protected boolean commitChanges() {
            return true;
        }

        /**
         * Initialize.
         *
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
         */
        @Override
        protected void initialize() throws X2BaseException {
            super.setFtpJobRunningInd(true);
            super.initialize();
            setValueDelimiter('\t');
            setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);

            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
            DataDictionaryField doePinField = dictionary.findDataDictionaryFieldByAlias(m_doePinAlias);
            super.setDoePinField(doePinField.getJavaName());

            super.setImportFlagAlias(m_importFlagAlias);
            DataDictionaryField importTypeField = dictionary.findDataDictionaryFieldByAlias(m_importTypeAlias);
            super.setImportTypeField(importTypeField != null ? importTypeField.getJavaName() : null);
            super.setAllRecords(new ArrayList());
            super.setCurrentDate(new PlainDate());
            super.setRecordsByStudentOid(new HashMap());
            super.setOrganization(m_organization);
            super.setBroker(getBroker());

        }

        /**
         * @see com.x2dev.procedures.statereporting.tn.EasyIEPImport#loadDisabilityAndOptionsCodes()
         */
        @SuppressWarnings("static-access")
        @Override
        protected void loadDisabilityAndOptionsCodes() {
            super.setAllCodes(new HashSet<String>());
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

            DataDictionaryField field = dictionary.findDataDictionaryField(StudentProgramParticipation.class.getName(),
                    StudentProgramParticipation.COL_PROGRAM_CODE);

            Criteria criteria = new Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            Criteria categoryCriteria = new Criteria();
            categoryCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, super.CATEGORY_DISABILITY);
            Criteria optionsCriteria = new Criteria();
            optionsCriteria.addEqualTo(ReferenceCode.COL_CATEGORY, super.CATEGORY_OPTIONS);
            categoryCriteria.addOrCriteria(optionsCriteria);
            criteria.addAndCriteria(categoryCriteria);

            String[] columns = new String[] {ReferenceCode.COL_CODE};
            ColumnQuery query = new ColumnQuery(ReferenceCode.class, columns, criteria);

            ReportQueryIterator queryItr = getBroker().getReportQueryIteratorByQuery(query);
            try {
                while (queryItr.hasNext()) {
                    Object[] row = (Object[]) queryItr.next();
                    getAllCodes().add((String) row[0]);
                }
            } finally {
                queryItr.close();
            }
        }

        /**
         * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#logInvalidRecord(int,
         *      java.lang.String)
         */
        @Override
        protected void logInvalidRecord(int lineNumber, String key) {
            String message = null;

            MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());
            message = resources.getMessage(getLocale(), key);

            if (message == null) {
                message = key;
            }

            m_invalidRecords.add(new KeyValuePair(Integer.valueOf(lineNumber), message));
        }

        /**
         * Exports a text file detailing the results of the import.
         *
         * @param fileName String
         * @return the name of the output file, this value does not contain any path information
         */
        protected void logImportResults(String fileName) {
            m_importBuffer = new StringBuilder(256);
            MessageResources resources = LocalizationCache.getMessages(getBroker().getPersistenceKey(), getLocale());

            Integer insertCount = Integer.valueOf(m_ftpInsertCount);
            Integer matchCount = Integer.valueOf(m_ftpMatchCount);
            Integer skipCount = Integer.valueOf(m_ftpSkipCount);
            Integer updateCount = Integer.valueOf(m_ftpUpdateCount);
            Integer total = Integer.valueOf(m_ftpMatchCount + m_ftpInsertCount + m_ftpSkipCount);

            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results"));
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.doubleRule"));
            m_importBuffer.append('\n');
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.fileName", fileName));
            m_importBuffer.append('\n');
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.matchCount", matchCount));
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.updateCount", updateCount));
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.insertCount", insertCount));
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.skipCount", skipCount));
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.rule", skipCount));
            m_importBuffer.append('\n');
            m_importBuffer.append(resources.getMessage(getLocale(), "message.import.results.total", total));
            m_importBuffer.append('\n');

            if (!m_invalidRecords.isEmpty()) {
                m_importBuffer.append('\n');
                m_importBuffer
                        .append(resources.getMessage(getLocale(), "message.import.results.invalidHeader", skipCount));
                m_importBuffer.append('\n');
                m_importBuffer
                        .append(resources.getMessage(getLocale(), "message.import.results.invalidRule", skipCount));
                m_importBuffer.append('\n');
                m_importBuffer.append('\n');

                for (KeyValuePair<Integer, String> invalidRecord : m_invalidRecords) {
                    String message = resources.getMessage(getLocale(),
                            "message.import.results.invalidRecord",
                            invalidRecord.getKey(),
                            invalidRecord.getValue());
                    m_importBuffer.append(message);
                    m_importBuffer.append('\n');
                }
            }
        }

        /**
         * Increments the number of beans that have been created by one.
         */
        @Override
        protected void incrementInsertCount() {
            m_ftpInsertCount++;
        }

        /**
         * Increments the number of beans that have been created by the passed value.
         *
         * @param inserted int
         */
        @Override
        protected void incrementInsertCount(int inserted) {
            m_ftpInsertCount += inserted;
        }

        /**
         * Increments the number of records in the source file that have been matched by one.
         */
        @Override
        protected void incrementMatchCount() {
            m_ftpMatchCount++;
        }

        /**
         * Increments the number of records in the source file that have been matched by the passed
         * value.
         *
         * @param matched int
         */
        @Override
        protected void incrementMatchCount(int matched) {
            m_ftpMatchCount += matched;
        }

        /**
         * Increments the number of records in the source file that have been skipped by one.
         */
        @Override
        protected void incrementSkipCount() {
            m_ftpSkipCount++;
        }

        /**
         * Increments the number of records in the source file that have been skipped by the passed
         * value.
         *
         * @param skipped int
         */
        @Override
        protected void incrementSkipCount(int skipped) {
            m_ftpSkipCount += skipped;
        }

        /**
         * Increments the number of existing beans that have been updated by one. Records that were
         * updated are a subset of the records that matched.
         */
        @Override
        protected void incrementUpdateCount() {
            m_ftpUpdateCount++;
        }

        /**
         * Increments the number of existing beans that have been updated by the passed value.
         * Records
         * that were updated are a subset of the records that matched.
         *
         * @param updated int
         */
        @Override
        protected void incrementUpdateCount(int updated) {
            m_ftpUpdateCount += updated;
        }

    }

}
