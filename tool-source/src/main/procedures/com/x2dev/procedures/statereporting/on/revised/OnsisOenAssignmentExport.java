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
package com.x2dev.procedures.statereporting.on.revised;

import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.appendTextElement;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.procedures.JarPluginNotFoundException;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolBean;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.StateReportValidationError;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.ToolUserDataContainer;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultHeaderHelper.OnsisResultHeader;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile;
import com.x2dev.utils.LoggerUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;


/**
 * @author Follett Software Company
 * @copyright 2017
 */
public class OnsisOenAssignmentExport extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String DATE_TIME_FORMAT_YYYYMMDD_HHMMSS = "yyyyMMdd_hhmmss";

    private static final String DDX_ID = "ON-SIS-OEN-ASMNT";
    private static final String ENCODING_WINDOWS_1252 = "windows-1252";
    private static final String OEN_DETAILS_TOPIC = "OEN_DETAILS";
    private static final String ONSIS_ELEM_BATCH_FILE_ID = "BATCH_FILE_ID";
    private static final String ONSIS_ELEM_DATE = "DATE";
    private static final String ONSIS_ELEM_OEN_BATCH_MULTIPLES = "OEN_BATCH_MULTIPLES";
    private static final String ONSIS_ELEM_OEN_DOCTYPE = "OEN_DOCTYPE";
    private static final String ONSIS_ELEM_OEN_VERSION = "OEN_VERSION";
    private static final String ONSIS_ELEM_TIME = "TIME";
    private static final String ONSIS_TOPIC_SCHOOL = "SCHOOL";

    private static final String PARAM_CUSTOM_FILENAME = "customName";

    private static final String SCHOOL_OID_SEPARATOR = ",";

    private String m_customFileName;
    private DictionaryExtractor m_dictExtractor;
    private Filterable<OnSchool> m_schoolsHelper;
    private ToolUserDataContainer m_userData;

    @Override
    public String getCustomFileName() {
        if (m_customFileName == null) {
            SimpleDateFormat dateFormatter = new SimpleDateFormat(DATE_TIME_FORMAT_YYYYMMDD_HHMMSS);
            String dateStr = dateFormatter.format(new Date());

            String customName = (String) getParameter(PARAM_CUSTOM_FILENAME);

            String fileNamePrefix = StringUtils.isEmpty(customName) ? "onsis_oen_request" : customName;

            m_customFileName = fileNamePrefix.replaceAll("\\s", "_") + "_" + dateStr + ".xml";
        }
        return m_customFileName;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        //

    }

    /**
     * Write the Onsis XML DOM to the output file
     *
     * @throws X2BaseException exception
     */
    @Override
    protected void exportResults() throws X2BaseException {
        try (ByteArrayOutputStream resultOutputStream = new ByteArrayOutputStream()) {
            try (Writer writer =
                    new BufferedWriter(new OutputStreamWriter(resultOutputStream,
                            ENCODING_WINDOWS_1252))) {

                exportAllSchools(writer);

                String result = resultOutputStream.toString(ENCODING_WINDOWS_1252);
                if (!StringUtils.isEmpty(result)) {
                    result =
                            new XMLOutputter(
                                    Format.getPrettyFormat().setEncoding(ENCODING_WINDOWS_1252))
                                            .outputString(new SAXBuilder().build(new StringReader(result)));
                }
                printResult(result);

                OnsisResultHeader header =
                        OnsisResultHeaderHelper.OnsisResultHeader.createResult(getOrganization(), getBroker());
                header.setUsername(m_userData.getUser().getNameView());
                header.setSchools(getSchools());
                header.setFilename(getCustomFileName());
                header.setRecordsDdxId(DDX_ID);

                OnsisResultHeaderHelper.saveResult(getBroker(), header);
            }
        } catch (Exception ex) {
            String message = LoggerUtils.convertThrowableToString(ex);
            try {
                printResult(message);
            } catch (IOException e) {
                // message = LoggerUtils.convertThrowableToString(e);
                // OnsisStateReportData.logSevere(message);
            }
        }
    }

    /**
     *
     * @param result
     * @throws IOException
     */
    private void printResult(String result) throws IOException {
        OutputStreamWriter writer =
                new OutputStreamWriter(getResultHandler().getOutputStream(), ENCODING_WINDOWS_1252);
        writer.append(result);
        writer.close();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        ToolBean.registerClass(OnSchool.class);
        ToolBean.setBroker(getBroker());
        ToolBean.setDictionaryExtractor(getDictExtractor());
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_userData = new ToolUserDataContainer(userData.getPrivilegeSet(), userData.getUserOid());
        super.saveState(userData);
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * School loop
     *
     * @param writer
     * @param broker
     * @throws X2BaseException
     * @throws ParserConfigurationException
     */
    private void exportAllSchools(Writer writer) throws X2BaseException, ParserConfigurationException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        Element rootElement =
                OnsisStateReportData.createTextElement(OnsisXmlBatchFile.ONSIS_ELEM_OEN_BATCH_FILE, null, document);
        document.appendChild(rootElement);

        addHeader(rootElement);

        Element oenBatchMultiples =
                OnsisStateReportData.createTextElement(ONSIS_ELEM_OEN_BATCH_MULTIPLES, null, document);
        rootElement.appendChild(oenBatchMultiples);
        OnsisStudentOenDetails studentReportData =
                (OnsisStudentOenDetails) loadReportByTopic(OEN_DETAILS_TOPIC);
        studentReportData.setBroker(getBroker());
        studentReportData.setCurrentContext(getCurrentContext());
        studentReportData.setOrganization(getOrganization());
        studentReportData.setParameters(getParameters());
        studentReportData.getGlobalData()
                .setBroker(getBroker())
                .setCurrentContext(getCurrentContext())
                .setOrganization(getOrganization())
                .setParameters(getParameters())
                .setSubmissionType(null)
                .setUserData(m_userData);
        studentReportData.initializeEntityClass();
        for (OnSchool school : getSchools()) {
            try {
                studentReportData.getGlobalData().setCurrentSchool(Arrays.asList(school));
                studentReportData.buildBeans();
                studentReportData.renderReportData(oenBatchMultiples);
            } catch (Exception e) {
                String message = LoggerUtils.convertThrowableToString(e);
                appendTextElement("DEBUG", message, oenBatchMultiples);
            }
        }

        try {
            String resultToPrint = new OnsisStateReportData.DebugOutputter().outputString(document);
            writer.append(resultToPrint);
            writer.flush();
        } catch (IOException e) {
            e.printStackTrace();
            String message = LoggerUtils.convertThrowableToString(e);
            AppGlobals.getLog().log(Level.SEVERE, message);
        }
    }

    /**
     * Generate file id.
     *
     * @return String
     */
    public String generateFileId() {
        return "" + Math.round(Math.random() * 100000);
    }


    private List<OnSchool> getSchools() {
        String schoolOids = (String) getParameter(OnsisStateReportData.INPUT_PARAM_SCHOOL_OIDS);
        if (StringUtils.isEmpty(schoolOids) && getSchool() != null) {
            schoolOids = getSchool().getOid();
        }
        if (m_schoolsHelper == null) {
            X2Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addIn(X2BaseBean.COL_OID, Arrays.asList(schoolOids.split(SCHOOL_OID_SEPARATOR)));
            m_schoolsHelper = FilterableFactory.create(ToolBean.getBroker(true), ToolBean.getDictionaryExtractor(), OnSchool.class, schoolCriteria, Arrays.asList(OnSchool.FIELD_NAME));
        }
        if (!StringUtils.isEmpty(schoolOids)) {
            return Stream.of(schoolOids).flatMap(toSplit -> Arrays.asList(toSplit.split(",")).stream())
                    .map(schoolOid -> m_schoolsHelper.extractFirst(ToolBean.FIELD_OID, schoolOid))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        }
        return Collections.EMPTY_LIST;
    }

    /**
     * Get the set of publishable SisSchool beans for a district
     *
     * @param singleSchool
     * @param zoneId
     * @param publishOptions
     * @param broker
     *
     * @return
     *
     * @throws X2BaseException
     */
    private OnsisSchoolData getSchoolsReportData()
            throws X2BaseException {

        OnsisSchoolData schoolReportData = (OnsisSchoolData) loadReportByTopic(ONSIS_TOPIC_SCHOOL);

        schoolReportData.setBroker(getBroker());
        schoolReportData.setCurrentContext(getCurrentContext());
        schoolReportData.setOrganization(getOrganization());
        schoolReportData.setParameters(getParameters());
        schoolReportData.getGlobalData()
                .setBroker(getBroker())
                .setCurrentContext(getCurrentContext())
                .setOrganization(getOrganization())
                .setParameters(getParameters())
                .setSubmissionType(null)
                .setUserData(m_userData);
        schoolReportData.buildBeans();
        schoolReportData.initializeEntityClass();

        return schoolReportData;
    }

    private OnsisStateReportData loadReportByTopic(String topic) {
        ArrayList<StateReportValidationError> errors = new ArrayList<>();
        OnsisStateReportData studentReportData = null;
        try {
            studentReportData =
                    OnsisStateReportData.loadPluginStateReportData(topic, null, errors, getBroker(), m_userData);
        } catch (JarPluginNotFoundException e) {
            throw new RuntimeException(LoggerUtils.convertThrowableToString(e));
        } catch (ToolRunException e) {
            throw new RuntimeException(LoggerUtils.convertThrowableToString(e));
        } catch (Throwable t) {
            throw new RuntimeException(LoggerUtils.convertThrowableToString(t));
        }
        if (!errors.isEmpty()) {
            StringBuilder errorsMessages = new StringBuilder();
            for (StateReportValidationError error : errors) {
                errorsMessages.append(error.getErrorMessage());
                errorsMessages.append("\n");
            }
            throw new RuntimeException(errorsMessages.toString());
        }
        return studentReportData;
    }

    /**
     *
     * @param parentElement
     */
    private void addHeader(Element parentElement) {
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_OEN_VERSION, "1.0", parentElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_OEN_DOCTYPE, "01", parentElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_BATCH_FILE_ID, generateFileId(),
                parentElement);

        OnsisStateReportData.OnsisHelper helper = new OnsisStateReportData.OnsisHelper();
        SimpleDateFormat dateFormatter = OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES;
        String dateStr = dateFormatter.format(new PlainDate());
        SimpleDateFormat timeFormatter = OnsisConstants.DATE_FORMATTER_HH_mm_ss_aaa;
        String timeStr = timeFormatter.format(new PlainTime());

        OnsisStateReportData.appendTextElement(ONSIS_ELEM_DATE, dateStr, parentElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_TIME, timeStr, parentElement);
    }
}
