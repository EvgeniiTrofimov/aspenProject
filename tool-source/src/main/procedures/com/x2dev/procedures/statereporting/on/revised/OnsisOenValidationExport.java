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

import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_DATE;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_FILE_ID;
import static com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisXmlBatchFile.ONSIS_ELEM_TIME;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.ToolUserDataContainer;
import com.x2dev.procedures.statereporting.on.ResultException;
import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnSchool;
import com.x2dev.procedures.statereporting.on.revised.OnsisResultHeaderHelper.OnsisResultHeader;
import com.x2dev.procedures.statereporting.on.revised.OnsisStateReportData.OnsisExtractor;
import com.x2dev.utils.LoggerUtils;
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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.stream.Collectors;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.commons.lang3.StringUtils;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * The Class OnsisValidationFileOen.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class OnsisOenValidationExport extends ProcedureJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    private static final String DATE_TIME_FORMAT_YYYYMMDD_HHMMSS = "yyyyMMdd_hhmmss";
    private static final String DDX_ID_OEN_VAL = "ON-SIS-OEN-VAL";
    private static final String ENCODING_WINDOWS_1252 = "windows-1252";
    private static final String TOPIC_OEN_VALIDATION_FILE = "OEN_VALIDATION_FILE";
    private static final String TOPIC_OEN_BATCH_MULTIPLES = "OEN_BATCH_MULTIPLES";
    private static final String XML_VERSION = "1.0";

    private static final String PARAM_CUSTOM_FILENAME = "customName";
    private static final String PARAM_SCHOOL_OIDS = "schoolOids";
    private List<String> m_messages = new LinkedList();
    private Filterable<OnSchool> m_schoolsHelper;
    private ToolUserDataContainer m_userData;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#exportResults()
     */
    @Override
    public void exportResults() throws X2BaseException {
        try (ByteArrayOutputStream resultOutputStream = new ByteArrayOutputStream()) {
            Map<String, Set<ResultException>> exceptions = new HashMap<>();
            if (m_messages.size() == 0) {
                try (Writer writer =
                        new BufferedWriter(new OutputStreamWriter(resultOutputStream,
                                ENCODING_WINDOWS_1252))) {

                    createDocAndExport(writer, exceptions);
                } catch (Exception e) {
                    AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
                }
            }

            /*
             * Write errors/results log to work folder
             */
            if (m_messages.size() > 0) {
                StringBuilder messages = new StringBuilder();
                for (String message : m_messages) {
                    messages.append(message);
                    messages.append("\n");
                }
                prepareResult(messages.toString());
            } else {
                try {
                    String result = resultOutputStream.toString(ENCODING_WINDOWS_1252);

                    if (!exceptions.isEmpty()) {
                        AppGlobals.getLog().log(Level.SEVERE,
                                "Exceptions are not empty, look OEN Exceptions section for details");
                        throw new RuntimeException(
                                "Exceptions are not empty, look OEN Exceptions section for details");
                    }

                    if (!StringUtils.isEmpty(result)) {
                        result =
                                new XMLOutputter(
                                        Format.getPrettyFormat().setEncoding(ENCODING_WINDOWS_1252))
                                .outputString(new SAXBuilder().build(new StringReader(result)));
                    }
                    prepareResult(result);
                } catch (IOException e) {
                    AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
                }
            }
        } catch (Exception e) {
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
        }
    }

    @Override
    public String getCustomFileName() {
        SimpleDateFormat dateFormatter = new SimpleDateFormat(DATE_TIME_FORMAT_YYYYMMDD_HHMMSS);
        String dateStr = dateFormatter.format(new Date());

        String customName = (String) getParameter(PARAM_CUSTOM_FILENAME);

        String fileNamePrefix = StringUtils.isEmpty(customName) ? "oen_validation" : customName;

        return fileNamePrefix.replaceAll("\\s", "_") + "_" + dateStr + ".xml";
    }

    /**
     * Extract onsis.
     *
     * @param writer Writer
     * @param resultOutputStream
     * @param exceptions Map<String,Set<String>>
     * @throws ParserConfigurationException
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    protected void createDocAndExport(Writer writer, Map<String, Set<ResultException>> exceptions)
            throws ParserConfigurationException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document document = builder.newDocument();
        Element rootElement = OnsisStateReportData.createTextElement(TOPIC_OEN_VALIDATION_FILE, null, document);
        document.appendChild(rootElement);

        appendHeader(rootElement);

        try {
            OnsisExtractor onsisExtractor = new OnsisExtractor();
            onsisExtractor.setBroker(getBroker())
            .setContext(getCurrentContext())
            .setExceptions(exceptions)
            .setOrganization(getOrganization())
            .setParameters(getParameters())
            .setTopic(TOPIC_OEN_BATCH_MULTIPLES)
            .setUserData(m_userData);

            // Export to DOM
            onsisExtractor.extractOnsis(null, rootElement);
            // Write XML to output
            onsisExtractor.writeDocument(document, writer);

        } catch (Exception e) {
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
        }
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        //
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_schoolsHelper = FilterableFactory.create(getBroker(), OnSchool.class, new X2Criteria(), null);
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
     *
     * @param parentElement
     * @return
     */
    private void appendHeader(Element parentElement) {
        SimpleDateFormat dateFormatter = OnsisConstants.DATE_FORMATTER_YYYY_MM_DD_SLASHES;
        String dateStr = dateFormatter.format(new PlainDate());
        SimpleDateFormat timeFormatter = OnsisConstants.DATE_FORMATTER_HH_mm_ss_aaa;
        String timeStr = timeFormatter.format(new PlainTime());

        OnsisStateReportData.appendTextElement(ONSIS_ELEM_DATE, dateStr, parentElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_TIME, timeStr, parentElement);
        OnsisStateReportData.appendTextElement(ONSIS_ELEM_FILE_ID, generateFileId(), parentElement);
    }

    /**
     * Generate file id.
     *
     * @return String
     */
    private static String generateFileId() {
        return "" + Math.round(Math.random() * 100000);
    }

    private List<OnSchool> getSchools() {
        String paramOids = (String) getParameter(PARAM_SCHOOL_OIDS);
        List<String> schoolOids = new ArrayList();
        if (!StringUtils.isEmpty(paramOids)) {
            schoolOids.addAll(Arrays.asList(paramOids.split(",")));
        } else if (getSchool() != null) {
            schoolOids.add(getSchool().getOid());
        }
        if (!schoolOids.isEmpty()) {
            return schoolOids.stream()
                    .map(schoolOid -> m_schoolsHelper.extractFirst(X2BaseBean.COL_OID, schoolOid))
                    .collect(Collectors.toList());
        }
        return Collections.EMPTY_LIST;
    }

    private void prepareResult(String result) {
        try {
            OutputStreamWriter writer =
                    new OutputStreamWriter(getResultHandler().getOutputStream(), ENCODING_WINDOWS_1252);
            writer.append(result);
            writer.close();

            OnsisResultHeader header = OnsisResultHeader.createResult(getOrganization(), getBroker());
            header.setUsername(m_userData.getUser().getNameView());
            header.setSchools(getSchools());
            header.setFilename(getCustomFileName());
            header.setRecordsDdxId(DDX_ID_OEN_VAL);

            OnsisResultHeaderHelper.saveResult(getBroker(), header);
        } catch (IOException e) {
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
        }
    }
}
