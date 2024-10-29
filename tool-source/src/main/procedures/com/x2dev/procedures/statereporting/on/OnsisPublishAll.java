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
package com.x2dev.procedures.statereporting.on;

import static com.x2dev.procedures.statereporting.on.revised.OnsisConstants.INPUT_PARAM_DEBUG_DETAIL;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_BATCH_FILE;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_BATCH_FILE_ID;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_BATCH_TYPE;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_DATA;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_DATE;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_HEADER;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_TIME;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_VERSION;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.utils.debug.PerformanceData;
import com.follett.fsc.core.utils.debug.PerformanceTimer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.ToolUserDataContainer;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.SubmissionType;
import com.x2dev.procedures.statereporting.on.revised.OnsisConstants;
import com.x2dev.procedures.statereporting.on.revised.OnsisExtractHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisHelpersContainer.XMLStreamWriterHolder;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultsHelper.OnsisResult;
import com.x2dev.procedures.statereporting.on.revised.OnsisSchoolSubmission;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisExtractor;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile;
import com.x2dev.procedures.statereporting.on.revised.OnsisValidations.OnsisValidator;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.stream.Collectors;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * The Class OnsisPublishAll.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class OnsisPublishAll extends ImportJavaSource {
    private static final long serialVersionUID = 1L;

    public static final String PARAM_CLASS_EXTRACT_CSV = "classListExtract";
    public static final String PARAM_COMPARE_XML = "compareXml";
    public static final String PARAM_CUSTOM_FILENAME = "customName";
    public static final String PARAM_DISTRICT_ID = "districtId";
    public static final String PARAM_PROCESS_SELECTOR = "processSelector";
    public static final String PARAM_RUN_OPTION = "runOption";
    public static final String PARAM_SCHOOL_OIDS = "schoolOids";
    public static final String PARAM_SUBMISSION_TYPE = "submissionType";
    public static final String PARAM_LOG_PERF_DATA = "logPerfData";

    private static final String ALIAS_SKL_BSID = "all-skl-BSID";

    private static final String DATE_TIME_FORMAT_YYYYMMDD_HHMMSS = "yyyyMMdd_hhmmss";

    private static final String DDX_ID_RECORDS = "ON-SIS-RECORD";

    private Set<String> m_bsids;
    private DictionaryExtractor m_dictExtractor;
    private OnsisExtractHelper m_extractHelper;
    private boolean m_logPerfData = false;
    private List<String> m_messages = new LinkedList();
    private SubmissionType m_submissionType;
    private ToolUserDataContainer m_userData;

    /**
     * Write the Onsis XML DOM to the output file.
     *
     * Public for unit tests
     *
     * @throws X2BaseException exception
     */
    @Override
    public void exportResults() throws X2BaseException {
        removeOldRecords();

        try (ByteArrayOutputStream resultOutputStream = new ByteArrayOutputStream()) {
            OnsisResult resultBean = null;
            OnsisValidator validator = null;
            if (m_messages.size() == 0) {
                getJob().getInput().setFormat(ToolInput.ZIP_FORMAT);

                Map<String, Set<ResultException>> exceptions = new HashMap<>();

                try (Writer writer =
                        new BufferedWriter(new OutputStreamWriter(resultOutputStream,
                                OnsisXmlBatchFile.ENCODING_UTF_8))) {

                    if (m_logPerfData) {
                        OnsisConstants.clearTimeByGetters();
                        performExportWrappedByPerf(writer, exceptions, validator);
                        dumpTimeByGetter();
                    } else {
                        performExportRevised_STREAMING(writer, exceptions, null);
                    }

                    resultBean = saveResult(exceptions);
                } catch (Exception e) {
                    logException(e);
                    // throw e;
                }
            }

            /*
             * Zip all files in workFolder
             * and write zip file to resultHandler
             */
            try {
                String result = resultOutputStream.toString(OnsisXmlBatchFile.ENCODING_UTF_8);
                String resultToPrint = null;
                if (m_messages.size() > 0) {
                    StringBuilder messages = new StringBuilder();
                    for (String message : m_messages) {
                        messages.append(message);
                        messages.append('\n');
                    }
                    resultToPrint = messages.toString();
                } else {
                    resultToPrint = result;
                }

                try (InputStream in = new ByteArrayInputStream(resultToPrint.getBytes(StandardCharsets.UTF_8));
                        OutputStream out = getResultHandler().getOutputStream()) {
                    StreamUtils.copyStream(in, out);
                }

                AppGlobals.getCache(((SisOrganization) getOrganization()).getPersistenceKey())
                        .clear(new String[] {SisStudent.class.getName(), StudentEnrollment.class.getName()});

            } catch (IOException e) {
                AppGlobals.getLog().severe("OnSIS Publish All: " + LoggerUtils.convertThrowableToString(e));
                throw new X2BaseException(e);
            }
        } catch (Exception e) {
            throw new X2BaseException(e);
        }
    }

    /**
     * Gets the bsids.
     *
     * @return the bsids
     */
    public Set<String> getBsids() {
        if (m_bsids == null) {
            m_bsids = getSchoolOids().stream()
                    .map(oid -> {
                        SisSchool skl = getBroker().getBeanByOid(SisSchool.class, oid);
                        return (String) skl.getFieldValueByAlias(ALIAS_SKL_BSID);
                    })
                    .collect(Collectors.toSet());
        }
        return m_bsids;
    }

    /**
     * Gets the custom file name.
     *
     * @return the custom file name
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCustomFileName()
     */
    @Override
    public String getCustomFileName() {
        m_logPerfData = Boolean.TRUE.equals(getParameter(PARAM_LOG_PERF_DATA));

        ToolBean.setDictionaryExtractor(getDictExtractor());

        SimpleDateFormat dateFormatter = new SimpleDateFormat(DATE_TIME_FORMAT_YYYYMMDD_HHMMSS);
        String dateStr = dateFormatter.format(new Date());

        String customName = (String) getParameter(PARAM_CUSTOM_FILENAME);

        String prefix = m_logPerfData ? "OnsisPerfLog" : "OnsisExport";

        String fileNamePrefix = StringUtils.isEmpty(customName) ? prefix : customName;
        String extension = m_logPerfData ? ".txt" : ".xml";

        return fileNamePrefix.replaceAll("\\s", "_") + "_" + dateStr + extension;
    }

    /**
     * Gets the dict extractor.
     *
     * @return the dict extractor
     */
    public DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the file content as string.
     *
     * @param file File
     * @return String
     */
    public static String getFileContentAsString(File file) {
        try {
            AppGlobals.getLog().log(Level.SEVERE, "File to parse: " + file);
            InputStream stream = new FileInputStream(file);
            StringBuilder lines = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream))) {
                String line = null;
                while ((line = reader.readLine()) != null) {
                    lines.append(line + "\r\n");
                    AppGlobals.getLog().log(Level.SEVERE, "Append line: " + line);
                }
            }
            AppGlobals.getLog().log(Level.SEVERE, "Format lines");
            return new XMLOutputter(Format.getPrettyFormat()).outputString(
                    new SAXBuilder().build(new StringReader(lines.toString())));
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Public for unit tests.
     *
     * @param key String
     * @return Object
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getParameter(java.lang.String)
     */
    @Override
    public Object getParameter(String key) {
        return super.getParameter(key);
    }

    /**
     * Public for unit tests.
     *
     * @return Result handler
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getResultHandler()
     */
    @Override
    public ResultHandler getResultHandler() {
        return super.getResultHandler();
    }

    /**
     * Gets the school oids.
     *
     * @return List
     */
    public List<String> getSchoolOids() {
        String schoolOids = (String) getParameter(PARAM_SCHOOL_OIDS);
        return Arrays.asList(schoolOids.split(","));
    }

    /**
     * Import data.
     *
     * Public for unit tests
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    public void importData(File sourceFile) throws Exception {
        try {
            if (sourceFile != null) {
                Charset charset = Charset.forName("windows-1252");
                getExtractHelper().initializeMatchers(sourceFile, charset, getJob().getTempFolder(), getBsids());
            }
        } catch (Exception e) {
            logException(e);
        }
    }

    /**
     * Public for unit tests.
     *
     * @param organization void
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#setOrganization(com.follett.fsc.core.k12.beans.Organization)
     */
    @Override
    public void setOrganization(Organization organization) {
        super.setOrganization(organization);
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param tagName String
     * @param text String
     * @throws XMLStreamException exception
     */
    public void writeTag(XMLStreamWriter xmlWriter, String tagName, String text) throws XMLStreamException {
        xmlWriter.writeStartElement(tagName);
        xmlWriter.writeCharacters(text);
        xmlWriter.writeEndElement();
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param tagName String
     * @param date PlainDate
     * @throws XMLStreamException exception
     */
    public void writeTag(XMLStreamWriter xmlWriter, String tagName, PlainDate date) throws XMLStreamException {
        String dateStr = OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.format(date);

        writeTag(xmlWriter, tagName, dateStr);
    }

    /**
     * Write tag.
     *
     * @param xmlWriter XMLStreamWriter
     * @param tagName String
     * @param time PlainTime
     * @throws XMLStreamException exception
     */
    public void writeTag(XMLStreamWriter xmlWriter, String tagName, PlainTime time) throws XMLStreamException {
        String timeStr = OnsisConstants.TIME_FORMATTER_HHMMSS.format(time);

        writeTag(xmlWriter, tagName, timeStr);
    }

    /**
     * After import data.
     *
     * @param file the file
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#afterImportData(java.io.File)
     */
    @Override
    protected void afterImportData(File file) {
        try {
            validateCsv();
        } catch (Exception e) {
            logException(e);
        }
    }

    /**
     * Gets the extract helper.
     *
     * @return Onsis extract helper
     */
    protected OnsisExtractHelper getExtractHelper() {
        if (m_extractHelper == null) {
            m_extractHelper = new OnsisExtractHelper(getBroker());
        }
        return m_extractHelper;
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

        // These are set earlier during getCustomFileName
        // m_doValidate = RUN_OPTION_VALIDATE.equals(getParameter(PARAM_RUN_OPTION));
        // m_logPerfData = Boolean.TRUE.equals(getParameter(PARAM_LOG_PERF_DATA));
    }

    /**
     * Logs a message that will be written to the results file.
     * Each message is written on its own line.
     *
     * @param message the message
     */
    protected void logMessage(String message) {
        m_messages.add(message);
    }

    /**
     * Log exception.
     *
     * @param ex the ex
     */
    protected void logException(Exception ex) {
        logException(null, ex);
    }

    /**
     * Log exception.
     *
     * @param message the message
     * @param ex the ex
     */
    protected void logException(String message, Exception ex) {
        if (!com.x2dev.utils.StringUtils.isBlank(message)) {
            AppGlobals.getLog().log(Level.SEVERE, message);
            m_messages.add(message);
        }

        String exceptionStr = LoggerUtils.convertThrowableToString(ex);
        AppGlobals.getLog().log(Level.SEVERE, exceptionStr);
        m_messages.add(exceptionStr);
    }

    /**
     * Save state.
     *
     * @param userData the user data
     * @throws X2BaseException the x 2 base exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_userData = new ToolUserDataContainer(userData.getPrivilegeSet(), userData.getUserOid());
        super.saveState(userData);
    }

    /**
     * Creates the doc and export revised STREAMING.
     *
     * @param xmlHolder the xml holder
     * @param school the school
     * @param zoneId the zone id
     * @param broker the broker
     * @param exceptions the exceptions
     * @param onsisValidator the onsis validator
     * @throws X2BaseException the x 2 base exception
     * @throws XMLStreamException the XML stream exception
     * @throws ParserConfigurationException the parser configuration exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void createDocAndExportRevised_STREAMING(XMLStreamWriterHolder xmlHolder,
                                                     SisSchool school,
                                                     String zoneId,
                                                     X2Broker broker,
                                                     Map<String, Set<ResultException>> exceptions,
                                                     OnsisValidator onsisValidator)
            throws X2BaseException, XMLStreamException, ParserConfigurationException, IOException {

        /*
         * Create empty DOM hierarchy
         */
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        Element batchFileElement = OnsisStateReportData.createTextElement(ONSIS_ELEM_BATCH_FILE, null, document);
        Element headerElement = appendHeader(batchFileElement);
        document.appendChild(batchFileElement);
        Element dataElement = OnsisStateReportData.appendTextElement(ONSIS_ELEM_DATA, null, batchFileElement);

        XMLStreamWriter xmlWriter = xmlHolder.xmlWriter;
        xmlWriter.writeStartDocument();

        xmlWriter.writeCharacters("\n");
        xmlWriter.writeStartElement(OnsisXmlBatchFile.ONSIS_ELEM_BATCH_FILE);
        xmlHolder.writeElementNextLine(headerElement);

        xmlWriter.writeCharacters("\n");
        xmlWriter.writeStartElement(OnsisXmlBatchFile.ONSIS_ELEM_DATA);

        Set<String> missingTags = new HashSet<String>();

        OnsisExtractor onsisExtractor = new OnsisExtractor();
        onsisExtractor.setBroker(broker)
                .setContext(getCurrentContext())
                .setExtractHelper(getExtractHelper())
                .setExceptions(exceptions)
                .setOrganization(getOrganization())
                .setParameters(getParameters())
                .setSubmissionType(getSubmissionType())
                .setTopic(OnsisSchoolSubmission.ONSIS_TOPIC)
                .setMissingTags(missingTags)
                .setValidator(onsisValidator)
                .setUserData(m_userData);

        // Export to xmlWriter
        onsisExtractor.extractOnsis(xmlHolder, dataElement);

        xmlWriter.writeCharacters("\n");
        xmlWriter.writeEndElement(); // </DATA>

        xmlWriter.writeCharacters("\n");
        xmlWriter.writeEndElement(); // </ONSIS_BATCH_FILE>

        xmlWriter.writeCharacters("\n");
        xmlWriter.writeEndDocument();
        xmlWriter.flush();

        for (String missingTag : missingTags) {
            System.out.println(missingTag);
        }

        if (!missingTags.isEmpty()) {
            StringBuilder errorMessage = new StringBuilder();
            errorMessage.append("Missing tags exception:");
            errorMessage.append("\r\n");
            for (String missingTag : missingTags) {
                errorMessage.append(missingTag);
                errorMessage.append("\r\n");
            }
            throw new RuntimeException(errorMessage.toString());
        }
        Boolean debugDetail = getParameter(INPUT_PARAM_DEBUG_DETAIL) != null
                && getParameter(INPUT_PARAM_DEBUG_DETAIL) instanceof Boolean
                && ((Boolean) getParameter(INPUT_PARAM_DEBUG_DETAIL));

        String resultOid = getJob().getJobResultOid();
        if (resultOid != null && resultOid.length() > 0 && debugDetail) {
            JobResult jobResult = (JobResult) broker.getBeanByOid(JobResult.class, resultOid);
            if (jobResult != null && onsisExtractor.getReportData() != null) {
                jobResult.setComments(
                        onsisExtractor.getReportData().getGlobalData().getFieldPerformanceMonitor().toString());
                broker.saveBeanForced(jobResult);
            }
        }
    }


    /**
     * Dump time by getter.
     */
    private void dumpTimeByGetter() {
        OnsisConstants.getTimeByGetters().forEach(msg -> logMessage(msg));
    }

    /**
     * Gets the submission type.
     *
     * @return the submission type
     */
    private SubmissionType getSubmissionType() {
        if (m_submissionType == null) {
            String submissionOid = (String) getParameter(PARAM_SUBMISSION_TYPE);
            try {
                m_submissionType = ToolBean.getBeanByOid(getBroker(), getDictExtractor(),
                        SubmissionType.class, submissionOid,
                        false);
            } catch (Exception e) {
                logException(e);
            }
        }
        return m_submissionType;
    }

    /**
     * Perform export revised STREAMING.
     *
     * @param writer the writer
     * @param exceptions the exceptions
     * @param validator the validator
     * @throws X2BaseException the x 2 base exception
     * @throws XMLStreamException the XML stream exception
     * @throws ParserConfigurationException the parser configuration exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void performExportRevised_STREAMING(Writer writer,
                                                Map<String, Set<ResultException>> exceptions,
                                                OnsisValidator validator)
            throws X2BaseException, XMLStreamException, ParserConfigurationException, IOException {

        /*
         * If a single school is selected, district will be determined from it
         */
        SisSchool school =
                getBroker().getBeanByOid(SisSchool.class, (String) getParameter(ToolInput.SCHOOL_OID_PARAM));

        XMLStreamWriter xmlWriter = XMLOutputFactory.newInstance().createXMLStreamWriter(writer);
        XMLStreamWriterHolder xmlHolder = new XMLStreamWriterHolder(xmlWriter, writer);
        try {
            createDocAndExportRevised_STREAMING(xmlHolder, school, null /* zoneId */, getBroker(), exceptions,
                    validator);
        } finally {
            xmlWriter.close();
        }
    }

    /**
     * Perform export wrapped by perf.
     *
     * @param writer the writer
     * @param exceptions the exceptions
     * @param validator the validator
     * @throws X2BaseException the x 2 base exception
     * @throws XMLStreamException the XML stream exception
     */
    private void performExportWrappedByPerf(Writer writer,
                                            Map<String, Set<ResultException>> exceptions,
                                            OnsisValidator validator)
            throws X2BaseException, XMLStreamException {
        PerformanceTimer perfTimer = null;
        OnsisPerfLogResultsListener resultsListener = null;

        // java.util.logging.Logger perfLogger = AppGlobals.getLog();
        OnsisAccumulatingLogger perfLogger = new OnsisAccumulatingLogger("", null);

        long pollingInterval = 2000;
        boolean startImmediately = false;

        perfTimer = new PerformanceTimer(pollingInterval, startImmediately, perfLogger);

        resultsListener = new OnsisPerfLogResultsListener(Level.ALL, true, perfLogger);
        perfTimer.clearListeners();
        perfTimer.addListener(resultsListener);
        perfTimer.start();

        Thread.currentThread().setName("OnsisPublishAll_" + Thread.currentThread().getId());
        String perfMessage = "";

        try {

            performExportRevised_STREAMING(writer, exceptions, null);

        } catch (Throwable t) {
            logMessage(LoggerUtils.convertThrowableToString(t));
        } finally {
            if (perfTimer != null) {
                try {
                    perfTimer.stop();
                    PerformanceData perfData = perfTimer.getPerformanceData();
                    resultsListener.notifyFinished(perfData);
                    perfMessage = resultsListener.m_lastResults;
                } catch (Throwable t) {
                    logMessage(LoggerUtils.convertThrowableToString(t));
                } finally {
                    perfTimer = null;
                }
            }
        }

        logMessage(perfMessage);
    }


    /**
     * Removes the old records.
     */
    private void removeOldRecords() {
        String recordsDictionaryOid = getDictExtractor().getDictionary(DDX_ID_RECORDS).getExtendedDictionaryOid();
        X2Criteria recordsToDeleteCriteria = new X2Criteria();
        recordsToDeleteCriteria.addEqualTo(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID,
                recordsDictionaryOid);
        recordsToDeleteCriteria.addIn(UserDefinedTableB.COL_SCHOOL_OID, getSchoolOids());

        Filterable<UserDefinedTableB> recordsToDeleteHelper =
                FilterableFactory.create(getBroker(), UserDefinedTableB.class, recordsToDeleteCriteria);
        Set<String> resultOids =
                new HashSet<>(recordsToDeleteHelper.<String>extract(UserDefinedTableB.COL_USER_DEFINED_TABLE_A_OID));

        recordsToDeleteHelper.extract().stream()
                .forEach(record -> OnsisResultsHelper.UdtHelper.deleteCascadeB(getBroker(), record));

        resultOids.stream()
                .map(resultOid -> (UserDefinedTableA) getBroker().getBeanByOid(UserDefinedTableA.class, resultOid))
                .filter(result -> result != null && result.getUserDefinedRecordsB().isEmpty())
                .forEach(resultWithoutRecords -> OnsisResultsHelper.UdtHelper.deleteCascadeA(getBroker(),
                        resultWithoutRecords));
    }

    /**
     * Save result.
     *
     * @param exceptions the exceptions
     * @return the onsis result
     */
    private OnsisResult saveResult(Map<String, Set<ResultException>> exceptions) {
        StringBuilder descriptionBuilder = new StringBuilder();
        descriptionBuilder.append("Submission type: " + getSubmissionType().getName());
        descriptionBuilder.append(" \r\n");
        descriptionBuilder.append("Start date: " + getSubmissionType().getPeriodStartDate());
        descriptionBuilder.append(" \r\n");
        descriptionBuilder.append("End date: " + getSubmissionType().getPeriodEndDate());
        descriptionBuilder.append(" \r\n");
        return OnsisResultsHelper.saveResult(getOrganization(), getBroker(), descriptionBuilder.toString(), exceptions,
                m_dictExtractor);
    }

    /**
     * Validate csv.
     */
    private void validateCsv() {
        String csvSubmissionPeriod = getExtractHelper().getSubmissionPeriod().getPeriodCode().trim();
        SubmissionType submissionType = getSubmissionType();
        if (submissionType != null) {
            String selectedSubmissionPeriodCode = submissionType.getSubmissionPeriodCode().trim();
            String selectedSubmissionPeriodDescription = submissionType.getPeriodDescription().trim();
            if (!selectedSubmissionPeriodCode.equalsIgnoreCase(csvSubmissionPeriod)
                    && !selectedSubmissionPeriodDescription.equalsIgnoreCase(csvSubmissionPeriod)) {
                throw new RuntimeException(
                        "Submission period code of CSV file (" + csvSubmissionPeriod
                                + ") and submission period code of selected Submission Type ("
                                + selectedSubmissionPeriodCode + ") are not the same, "
                                + "please ensure that you are using appropriate CSV files for selected Submission Type.");
            }
        }
    }

    /**
     * Append header.
     *
     * @param parentElement the parent element
     * @return the element
     */
    private Element appendHeader(Element parentElement) {
        Element headerElement =
                OnsisStateReportData.createTextElement(ONSIS_ELEM_HEADER, null, parentElement.getOwnerDocument());

        OnsisStateReportData.appendTextElement(ONSIS_ELEM_VERSION, getVersion(), headerElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_BATCH_TYPE,
                getSubmissionType().getBatchType(), headerElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_BATCH_FILE_ID, "0001", headerElement);

        String dateStr = OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES.format(new PlainDate());
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_DATE, dateStr, headerElement);

        String timeStr = OnsisConstants.TIME_FORMATTER_HHMMSS.format(new PlainTime());

        OnsisStateReportData.appendTextElement(ONSIS_ELEM_TIME, timeStr, headerElement);
        parentElement.appendChild(headerElement);

        return headerElement;
    }

    /**
     * Gets the version.
     *
     * @return the version
     */
    private String getVersion() {
        Tool tool = getJob().getTool();
        if (StringUtils.isBlank(tool.getComment())) {
            return "1.0";
        }

        return tool.getComment();
    }
}
