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
package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.DefaultValue;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLSurvey;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationError;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
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
 * The Class FLBulkRunProcedure.
 */
public class FLBulkRunProcedure extends ToolJavaSource {
    // private static final long serialVersionUID = 1L;

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
                Set<String> allParams = new HashSet<>();
                Map<String, DefaultValue> defaultParams = new HashMap<>();
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

    protected static final String PARAM_PERFORM_EXPORT = "performExport";
    protected static final String PARAM_PERFORM_VALIDATION = "performValidation";
    protected static final String PARAM_SURVEY_PERIOD = "surveyPeriod";
    protected static final String PARAM_SYSTEM_ID = "systemId";

    protected static final String SYSTEM_ID_SIS = "SIS";
    protected static final String SYSTEM_ID_WDIS = "WDIS";

    private Pattern m_countPattern;
    private List<String> m_exceptionList = new ArrayList<>();
    private List<ImportExportDefinition> m_exportDefinitionList = new ArrayList<>();
    private FLExportConfiguration m_helper;
    private String m_resultOid = null;
    private FLSurvey m_survey;
    private SimpleDateFormat m_timeFormat = new SimpleDateFormat("HH:mm:ss");
    private UserDataContainer m_userData = null;

    /**
     * Returns num of records for passed result handler.
     *
     * @param handler ResultHandler
     * @return int
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected int getNumOfRecords(ResultHandler handler) throws IOException {
        int count = 0;

        FileInputStream result = new FileInputStream(handler.getFilePath());
        try (BufferedReader br = new BufferedReader(new InputStreamReader(result))) {
            String extractContent = br.readLine(); //
            if (!StringUtils.isEmpty(extractContent)) {
                Matcher m = m_countPattern.matcher(extractContent);
                if (m.find()) {
                    count = Integer.parseInt(m.group(1));
                }
            }
        }

        return count;
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

        ReferenceCode surveyPeriodBean = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class,
                (String) getParameter(PARAM_SURVEY_PERIOD));
        String surveyCode = surveyPeriodBean.getCode();

        DistrictSchoolYearContext currentContext = getOrganization().getCurrentContext();

        String systemId = (String) getParameter(PARAM_SYSTEM_ID);
        if (SYSTEM_ID_SIS.equals(systemId) || systemId == null) {
            m_helper = new FLExportConfiguration(currentContext, surveyCode, getBroker());
        } else if (SYSTEM_ID_WDIS.equals(systemId)) {
            m_helper = new FLWdisExportConfiguration(currentContext, surveyCode, getBroker());
        }

        m_survey = m_helper.getSurveyFromCode(surveyCode);

        if (m_survey != null) {
            List<FLExport> exportList = m_survey.getExports();

            List<String> exportIds = new ArrayList<>();

            for (FLExport export : exportList) {
                exportIds.add(export.getExportId());
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(ImportExportDefinition.COL_ID, exportIds);
            Map<String, ImportExportDefinition> mapExports = getBroker().getMapByQuery(
                    new BeanQuery(ImportExportDefinition.class, criteria), ImportExportDefinition.COL_ID, 30);

            for (String exportId : exportIds) {
                ImportExportDefinition exportDefinition = mapExports.get(exportId);
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
        Boolean performExport = (Boolean) getParameter(PARAM_PERFORM_EXPORT);
        if (performExport == null || performExport.booleanValue()) {
            execute();
        }

        Boolean performValidation = (Boolean) getParameter(PARAM_PERFORM_VALIDATION);
        if (performValidation == null || performValidation.booleanValue()) {
            executeValidation();
        }

        if (m_resultOid != null && m_resultOid.length() > 0) {
            X2Broker broker = getBroker();
            JobResult result = (JobResult) broker.getBeanByOid(JobResult.class, m_resultOid);
            if (result != null) {
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
     * Creates the tool job for the export definition and loads default parameters for the export.
     *
     * @param exportDefinition ImportExportDefinition
     * @return ToolJob
     * @throws Exception exception
     */
    private ToolJob createToolJob(ImportExportDefinition exportDefinition) throws Exception {
        ToolInput exportInput =
                ToolInputProcessor.restoreToolInput(exportDefinition, true, m_userData, getBroker(), getLocale());

        exportInput.setParameterAsString(PARAM_SURVEY_PERIOD, (String) getParameter(PARAM_SURVEY_PERIOD));
        exportInput.setParameterAsString("saveResults", "true");
        exportInput.setParameterAsString("suppressOutput", "true");

        m_userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);
        return ToolJob.createJob(exportDefinition, m_userData, tempFolder, false, getLocale());
    }

    /**
     * Executes the jobs selected.
     *
     * @throws Exception exception
     */
    private void execute() throws Exception {
        List<String> outputList = new ArrayList<>();
        m_countPattern = Pattern.compile("Total Records with Record Type = .* - (\\d+).", Pattern.CASE_INSENSITIVE);


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
            ToolJob job = createToolJob(exportDefinition);
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
                    if (job.getStatus() == ToolJob.STATUS_SUCCESS) {
                        int numOfRecords = getNumOfRecords(job.getResultHandler());
                        String comment = getCommentString(null, exportDefinition.getId(), numOfRecords, "Success",
                                startTime, endTime);
                        outputList.add(comment);
                        total += numOfRecords;
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
                        String comment =
                                getCommentString(null, exportDefinition.getId(), 0, status, startTime, endTime);
                        outputList.add(comment);

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
            }
        }

        outputList.add("Grand Total: " + total);

        // Copy exceptions to comments
        if (!m_exceptionList.isEmpty()) {
            outputList.add("\nExceptions:\n");
            for (String comment : m_exceptionList) {
                outputList.add(comment + "\n");
            }
        }

        // Copy comments to output stream
        for (String comment : outputList) {
            PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
            stream.print(comment + "\n");
        }
    }

    /**
     * Execute validation.
     *
     * @throws Exception exception
     */
    private void executeValidation() throws Exception {
        m_helper.deleteValidationErrors();

        List<String> outputList = new ArrayList<>();
        if (m_survey != null) {
            for (FLExport export : m_survey.getExports()) {
                int validationErrorsTotal = 0;
                List<FLValidationError> exportErrors = new ArrayList<>();
                outputList.add("Export: " + export.name());
                Collection<ExportFormatRow> exportFormatRows = m_helper.getExportFormatRows(export);
                List<FLValidationRule> rules = export.getValidationRules();
                for (ExportFormatRow row : exportFormatRows) {
                    for (FLValidationRule rule : rules) {
                        List<FLValidationError> errors;
                        try {
                            errors = rule.getProcessor().getValidationErrors(m_helper, export, row);
                        } catch (IllegalStateException e) {
                            StringBuilder message = new StringBuilder();
                            message.append("\n");
                            message.append("Validate Rule processing exception for export ");
                            message.append(export.name());
                            message.append(" rule # ");
                            message.append(rule.getRuleNumber());
                            message.append("\n");
                            message.append(e.getMessage());
                            message.append("\n");
                            throw new IllegalStateException(message.toString());
                        }
                        for (FLValidationError error : errors) {
                            error.setRuleNumber(rule.getRuleNumber());
                        }
                        exportErrors.addAll(errors);
                    }
                }
                m_helper.persistValidationErrors(exportErrors);
                validationErrorsTotal += exportErrors.size();
                outputList.add("rows: " + exportFormatRows.size());
                outputList.add("validation errors: " + validationErrorsTotal + "\n");
            }
        }

        for (String comment : outputList) {
            PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
            stream.print(comment + "\n");
        }
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
            output.append("Total Records from Export ");
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

}
