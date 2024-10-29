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

import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.DefaultValue;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class TNExtractRunProcedure.
 */
public class TNExtractRunProcedure extends ToolJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final long serialVersionUID = 1L;

    /**
     * Static processing class used to process tool input and generate default elements.
     */
    private static class ToolInputProcessor {
        private static final String GROUP_ELEMENT = "group";
        private static final String INPUT_DEFAULT_VALUE_SOURCE_ATTRIB = "default-value-source";
        private static final String ROOT_ALLOW_SCHOOL_SELECT_ATTRIB = "allow-school-select";


        /**
         * This method will rebuild the ToolInput object from information in the
         * Tool.
         *
         * @param tool Tool
         * @param inputDefaults boolean
         * @param userData UserDataContainer
         * @param broker X2Broker
         * @param locale Locale
         * @return a fully populated ToolInput from the data in JobEntry.
         * @throws X2BaseException exception
         */
        public static ToolInput restoreToolInput(Tool tool,
                                                 boolean inputDefaults,
                                                 UserDataContainer userData,
                                                 X2Broker broker,
                                                 Locale locale)
                throws X2BaseException {
            ToolInput input = null;
            if (tool != null) {
                SAXBuilder builder = new SAXBuilder();

                // Parameter values defined in the Tool input.xml
                // Make a list of all params and params with default values if the
                // inputDefaults flag is set.
                Set<String> allParams = new HashSet<String>();
                Map<String, DefaultValue> defaultParams = new HashMap<String, DefaultValue>();
                String inputDef = tool.getFullInputDefinition();
                if (!StringUtils.isEmpty(inputDef)) {
                    try {
                        Document inputDefDoc = builder.build(new ByteArrayInputStream(inputDef.getBytes()));
                        Element idRoot = inputDefDoc.getRootElement();
                        // See if the root element has attribute for "allow school selection"
                        Attribute schoolSelect = idRoot.getAttribute(ROOT_ALLOW_SCHOOL_SELECT_ATTRIB);
                        if (schoolSelect != null && schoolSelect.getValue().equalsIgnoreCase("true")) {
                            // "schoolOid" is a valid parameter.
                            allParams.add(ToolInput.SCHOOL_OID_PARAM);
                        }

                        parseInputElements(inputDefaults, allParams, defaultParams, idRoot);
                    } catch (JDOMException jde) {
                        // Ignore. The tool input template is bad somehow. We can't get the input
                        // defaults from it.
                    } catch (IOException ioe) {
                        // Ignore. The tool input template is bad somehow. We can't get the input
                        // defaults from it.
                    }
                }

                input = new ToolInput(tool, null, userData, locale);
                for (String key : defaultParams.keySet()) {
                    // From the DefaultValue, get the default object.
                    // Then try to convert it to a string with a converter.
                    DefaultValue dv = defaultParams.get(key);
                    if (dv != null) {
                        // Get the default value and convert it to String for display.
                        Object resValue = dv.resolve(userData, broker, locale);
                        String resString = null;
                        if (resValue instanceof String) {
                            resString = (String) resValue;
                        } else {
                            Converter someConverter =
                                    ConverterFactory.getConverterForClass(resValue.getClass().getName(), locale);
                            if (someConverter != null) {
                                resString = someConverter.javaToString(resValue);
                            } else {
                                resString = resValue.toString(); // what else?
                            }
                        }
                        input.setParameterAsString(key, resString);
                    }
                }
            }
            return input;
        }

        /**
         * Helper method to recursively parse input parameters from the root element.
         *
         * @param inputDefaults boolean
         * @param allParams Set<String>
         * @param defaultParams Map<String,DefaultValue>
         * @param root Element
         */
        private static void parseInputElements(boolean inputDefaults,
                                               Set<String> allParams,
                                               Map<String, DefaultValue> defaultParams,
                                               Element root) {
            for (Object obj : root.getChildren()) {
                Element child = (Element) obj;
                if (child.getName().equals(ToolInput.INPUT_ELEMENT)) {
                    allParams.add(child.getAttributeValue(ToolInput.INPUT_NAME_ATTRIB));

                    if (inputDefaults) {
                        Attribute defaultAttr = child.getAttribute(ToolInput.INPUT_DEFAULT_VALUE_ATTRIB);
                        Attribute defaultAttrSource = child.getAttribute(INPUT_DEFAULT_VALUE_SOURCE_ATTRIB);
                        if (defaultAttr != null) {
                            String dfltVal = defaultAttr.getValue();
                            String dfltValueSource = null;
                            if (defaultAttrSource != null) {
                                // This input has a default value with source.
                                // Add to the DefaultValue object for this value and source.
                                dfltValueSource = defaultAttrSource.getValue();
                            }
                            defaultParams.put(child.getAttributeValue(ToolInput.INPUT_NAME_ATTRIB),
                                    new DefaultValue(dfltVal, dfltValueSource));
                        }
                    }
                } else if (child.getName().equals(GROUP_ELEMENT)) {
                    parseInputElements(inputDefaults, allParams, defaultParams, child);
                }
            }
        }
    }

    protected static final String PARAM_FILE_SEQ = "fileSeq";
    protected static final String PARAM_SAVE_RESULTS = "saveResults";
    protected static final String PARAM_SUPPRESS_HEADING = "suppressHeading";
    protected static final String PARAM_SUPPRESS_OUTPUT = "suppressOutput";
    protected static final String PARAM_SUPPRESS_TRAILER = "suppressTrailer";

    private List<String> m_commentList = new ArrayList<String>();
    private List<String> m_exceptionList = new ArrayList<String>();
    private List<ImportExportDefinition> m_exportDefinitionList = new ArrayList<ImportExportDefinition>();
    private String m_resultOid = null;
    private boolean m_suppressOutput = false;
    SimpleDateFormat m_timeFormat = new SimpleDateFormat("HH:mm:ss");
    private UserDataContainer m_userData = null;

    /**
     * This method returns the String, which is essentially a concatenation of all comments.
     * This will be used by the scheduled job.
     *
     * @return String
     */
    protected String getComment() {
        String comments = "";
        if (!m_commentList.isEmpty()) {
            Collections.sort(m_commentList);
            for (String comment : m_commentList) {
                comments += comment + "\n";
            }
        }
        if (!m_exceptionList.isEmpty()) {
            comments += "\nExceptions:\n";
            for (String comment : m_exceptionList) {
                comments += comment + "\n";
            }
        }
        return comments;
    }

    /**
     * Returns num of records for passed result handler.
     *
     * @param handler ResultHandler
     * @return int
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected int getNumOfRecords(ResultHandler handler) throws IOException {
        ArrayList<String> list = new ArrayList<String>();

        FileInputStream result = new FileInputStream(handler.getFilePath());
        try (BufferedReader br = new BufferedReader(new InputStreamReader(result))) {
            String row = null;
            while ((row = br.readLine()) != null) {
                list.add(row);
            }
        }

        return list.size();
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

        m_resultOid = getJob().getJobResultOid();

        if (getParameter("exportIds") != null) {
            String[] exportIds = ((String) getParameter("exportIds")).split(",");

            for (String exportId : exportIds) {
                ImportExportDefinition exportDefinition =
                        (ImportExportDefinition) getBroker().getBeanByOid(ImportExportDefinition.class, exportId);
                if (exportDefinition != null) {
                    m_exportDefinitionList.add(exportDefinition);
                }
            }
        }
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        execute();

        if (m_resultOid != null && m_resultOid.length() > 0) {
            X2Broker broker = getBroker();
            JobResult result = (JobResult) broker.getBeanByOid(JobResult.class, m_resultOid);
            if (result != null) {
                if (this.getComment() != null) {
                    result.setComments(this.getComment());
                }
                broker.saveBeanForced(result);
            }
        }

    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);

        m_userData = userData;
    }

    /**
     * Format comment string.
     *
     * @param extractedOutput String
     * @param recordType String
     * @param numRecords int
     * @param status String
     * @param startTime Date
     * @param endTime Date
     * @return String
     */
    private String getCommentString(String extractedOutput,
                                    String recordType,
                                    int numRecords,
                                    String status,
                                    Date startTime,
                                    Date endTime) {
        StringBuilder output = new StringBuilder();
        if (extractedOutput == null) {
            output.append("Total Records with Record Type = ");
            output.append(recordType);
            output.append(" - ");
            output.append(String.valueOf(numRecords));
            output.append(".");
        } else {
            output.append(extractedOutput);
        }
        output.append(" Status:");
        output.append(status);
        output.append(" - Run interval from ");
        output.append(m_timeFormat.format(startTime));
        output.append(" to ");
        output.append(m_timeFormat.format(endTime));
        return output.toString();
    }

    /**
     * Creates the tool job for the export definition and loads default parameters for the export.
     *
     * @param exportDefinition ImportExportDefinition
     * @param includeHeading boolean
     * @param includeTrailer boolean
     * @param saveResults boolean
     * @param suppressOutput boolean
     * @return ToolJob
     * @throws Exception exception
     */
    private ToolJob createToolJob(ImportExportDefinition exportDefinition,
                                  boolean includeHeading,
                                  boolean includeTrailer,
                                  boolean saveResults,
                                  boolean suppressOutput)
            throws Exception {
        ToolInput exportInput =
                ToolInputProcessor.restoreToolInput(exportDefinition, true, m_userData, getBroker(), getLocale());

        if (!includeHeading) {
            exportInput.setParameterAsString(PARAM_SUPPRESS_HEADING, "true");
        }
        if (!includeTrailer) {
            exportInput.setParameterAsString(PARAM_SUPPRESS_TRAILER, "true");
        }
        exportInput.setParameterAsString(PARAM_SAVE_RESULTS, saveResults ? "true" : "false");
        exportInput.setParameterAsString(PARAM_SUPPRESS_OUTPUT, suppressOutput ? "true" : "false");
        exportInput.setParameterAsString(ToolInput.CONTEXT_OID_PARAM,
                (String) getParameter(ToolInput.CONTEXT_OID_PARAM));

        m_userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);
        return ToolJob.createJob(exportDefinition, m_userData, tempFolder, false, getLocale());
    }

    /**
     * Returns record type based on parsing of result rows.
     * NOTE: It is possible that returning record type will be incorrect,
     * e.g. in cases if there are initial errors.
     *
     * @param job ToolJob
     * @return String
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private String getCodeFromRows(ToolJob job) throws IOException {
        String code = null;
        BufferedReader reader = new BufferedReader(new FileReader(job.getResultHandler().getFilePath()));
        try {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() >= 3) {
                    if (code == null) {
                        code = line.substring(0, 3);
                        break;
                    }
                }
            }
        } finally {
            reader.close();
        }

        return code;
    }

    /**
     * Executes the jobs selected.
     *
     * @throws Exception exception
     */
    private void execute() throws Exception {
        boolean includeHeader = false;
        boolean includeFooter = false;
        Integer fileSeq = (Integer) getParameter(PARAM_FILE_SEQ);
        Boolean saveResults = (Boolean) getParameter(PARAM_SAVE_RESULTS);
        Boolean suppressOutput = (Boolean) getParameter(PARAM_SUPPRESS_OUTPUT);
        m_suppressOutput = suppressOutput == null ? false : suppressOutput.booleanValue();
        int iSeq = fileSeq == null ? 1 : fileSeq.intValue();

        TNHeadingTrailing headingTrailing = new TNHeadingTrailing(getOrganization(), iSeq);
        PrintStream printStream = new PrintStream(getResultHandler().getOutputStream());
        if (!m_suppressOutput) {
            printStream.print(headingTrailing.getHeading());
        }

        Pattern countPattern = Pattern.compile("- (\\d+).", Pattern.CASE_INSENSITIVE);

        // sort export definitions so that execution is in consistent order
        Comparator<ImportExportDefinition> comparator = new Comparator<ImportExportDefinition>() {
            @Override
            public int compare(ImportExportDefinition o1, ImportExportDefinition o2) {
                return o1.getName().compareTo(o2.getName());
            }
        };

        Collections.sort(m_exportDefinitionList, comparator);

        Iterator<ImportExportDefinition> iterator = m_exportDefinitionList.iterator();
        List<KeyValuePair<ImportExportDefinition, ToolJob>> jobsList = new LinkedList();
        int total = 0;
        while (iterator.hasNext()) {
            ImportExportDefinition exportDefinition = iterator.next();
            ToolJob job = createToolJob(exportDefinition, includeHeader, includeFooter,
                    saveResults == null ? false : saveResults.booleanValue(),
                    suppressOutput == null ? false : suppressOutput.booleanValue());
            jobsList.add(new KeyValuePair(exportDefinition, job));
        }

        for (KeyValuePair<ImportExportDefinition, ToolJob> item : jobsList) {
            ImportExportDefinition exportDefinition = item.getKey();
            ToolJob job = item.getValue();

            Date startTime = Calendar.getInstance().getTime();

            try {
                job.run();
            } finally {
                ResultHandler resultHandler = job.getResultHandler();
                resultHandler.close();

                try {
                    Date endTime = Calendar.getInstance().getTime();
                    String recordType = exportDefinition.getName().substring(0, 6);
                    if (job.getStatus() == ToolJob.STATUS_SUCCESS) {
                        String codeFromRows = getCodeFromRows(job);
                        FileInputStream inputStream = new FileInputStream(job.getResultHandler().getFilePath());

                        // Skip result if rows are containing incorrect record type.
                        if (codeFromRows != null && codeFromRows.equals(recordType.substring(3, 6))
                                || m_suppressOutput) {
                            if (!m_suppressOutput) {
                                headingTrailing.incementCount(codeFromRows, getNumOfRecords(job.getResultHandler()));
                            }

                            // Add comment for job result.
                            if (m_resultOid != null && m_resultOid.length() > 0) {
                                if (m_suppressOutput) {
                                    BufferedReader br = null;
                                    try {
                                        br = new BufferedReader(new FileReader(job.getResultHandler().getFilePath()));
                                        String extractContent = br.readLine(); //
                                        if (!StringUtils.isEmpty(extractContent)) {
                                            String comment = getCommentString(extractContent, recordType, 0, "Success",
                                                    startTime, endTime);
                                            m_commentList.add(comment);
                                            Matcher m = countPattern.matcher(extractContent);
                                            if (m.find()) {
                                                total += Long.parseLong(m.group(1));
                                            }
                                        }
                                    } finally {
                                        br.close();
                                    }
                                } else {
                                    int numOfRecords = getNumOfRecords(job.getResultHandler());
                                    String comment = getCommentString(null, recordType, numOfRecords, "Success",
                                            startTime, endTime);
                                    m_commentList.add(comment);
                                    total += numOfRecords;
                                }
                            }
                            try {
                                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
                            } finally {
                                inputStream.close();
                            }
                        }
                    } else {
                        String status = null;
                        switch (job.getStatus()) {
                            case ToolJob.STATUS_ERROR:
                                status = "Error";
                                break;
                            case ToolJob.STATUS_ABORT:
                                status = "Abort";
                                break;
                            case ToolJob.STATUS_NO_DATA:
                                status = "No Data";
                                break;
                            default:
                                status = "Unexpected " + job.getStatus();
                                break;
                        }
                        String comment = getCommentString(null, recordType, 0, status, startTime, endTime);
                        m_commentList.add(comment);
                        if (m_suppressOutput) {
                            PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
                            stream.print(comment + "\n");
                        }
                        if (job.getThrowable() != null) {
                            m_exceptionList.add("\nStack Trace for : " + exportDefinition.getName());
                            StringWriter sw = new StringWriter();
                            PrintWriter pw = new PrintWriter(sw);
                            job.getThrowable().printStackTrace(pw);
                            m_exceptionList.add(sw.toString());
                        }
                    }
                } catch (FileNotFoundException fnfe) {
                    throw new X2BaseException(fnfe);
                } catch (IOException ioe) {
                    throw new X2BaseException(ioe);
                }
                // writeToConsole(job.getResultHandler());
            }
        }
        // Add Grand Total comment for job result.
        if (m_resultOid != null && m_resultOid.length() > 0) {
            m_commentList.add("Grand Total: " + total);
        }
        if (!m_suppressOutput) {
            printStream.print(headingTrailing.getTrailer());
        }
    }

}
