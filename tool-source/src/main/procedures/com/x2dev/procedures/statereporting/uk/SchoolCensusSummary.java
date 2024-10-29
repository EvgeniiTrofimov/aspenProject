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
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.JDOMException;

/**
 * Java source for bulk exporting exam results.
 *
 * @author X2 Development Corporation
 */
public class SchoolCensusSummary extends ProcedureJavaSource {
    private static final char CHAR_FORWARD_SLASH = '/';
    private static final char CHAR_BACK_SLASH = '\\';
    private static final String WINDOWS_OS_DIRECTORY_DELIMITER = "\\";
    private static final String UNIX_OS_DIRECTORY_DELIMITER = "/";
    private static final String XML_FILE_NAME_EXTENTION = ".xml";
    private static final String TXT_FILE_NAME_EXTENTION = ".txt";

    private static final String PARAM_INTERMEDIATE_FILE = "intermediateFile";
    private static final String PARAM_PRESENTATION_FILE = "presentationFile";
    private static final String PARAM_TERM = "term";
    private static final String PARAM_XSL_DIRECTORY = "xslfiles";

    private static final String WEB_TEMP_DIRECTORY_NAME = "temp";
    private static final String VALIDATION_RESULT_FILE_NAME_SUFFIX = "SC14ValidationResults.xml";
    private static final String SUMMARY_RESULT_FILE_NAME_SUFFIX = "_summary.html";

    private String m_term;
    private ImportExportDefinition m_export;
    protected Collection<ExamSeries> m_series; // exam series
    private UserDataContainer m_userData;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_term = (String) getParameter(PARAM_TERM);
        if (StringUtils.isEmpty(m_term)) {
            m_term = "";
        }

        ToolJob job = createToolJob();
        try {
            job.run();
        } finally {
            job.getResultHandler().close();

            ResultHandler resultHandler = job.getResultHandler();
            writeToConsole(resultHandler);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_userData = userData;

        /*
         * The exam export procedure to call.
         */
        X2Criteria exportCriteria = new X2Criteria();
        exportCriteria.addEqualTo(ImportExportDefinition.COL_ID, "EXP-UK-SC");

        QueryByCriteria exportQuery = new QueryByCriteria(ImportExportDefinition.class, exportCriteria);
        m_export = (ImportExportDefinition) getBroker().getBeanByQuery(exportQuery);
    }

    /**
     * Creates a ToolJob.
     *
     * @return ToolJob
     * @throws Exception exception
     */
    private ToolJob createToolJob() throws Exception {
        ToolInput exportInput = new ToolInput(m_export, null, m_userData, Locale.getDefault());

        exportInput.setParameterAsString("procedureId", (String) getParameter("procedureId"));

        exportInput.setParameterAsString("schoolOid", getSchool() == null ? null : getSchool().getOid());

        exportInput.setParameterAsString(PARAM_TERM, m_term);

        // exportInput.setParameterAsString("examSeason", (String) getParameter("examSeason"));

        exportInput.setParameterAsString("periodNumber", (String) getParameter("periodNumber"));

        Converter dateConverter = ConverterFactory.getConverterForClass(PlainDate.class.getName(), Locale.getDefault());
        PlainDate censusDate = (PlainDate) getParameter("censusDate");
        String censusDateString = dateConverter.javaToString(censusDate);
        exportInput.setParameterAsString("censusDate", censusDateString);

        PlainDate termStartDate = (PlainDate) getParameter("termStartDate");
        String termStartDateString = dateConverter.javaToString(termStartDate);
        exportInput.setParameterAsString("termStartDate", termStartDateString);

        PlainDate termEndDate = (PlainDate) getParameter("termEndDate");
        String termEndDateString = dateConverter.javaToString(termEndDate);
        exportInput.setParameterAsString("termEndDate", termEndDateString);

        PlainDate fsmStartDate = (PlainDate) getParameter("fsmStartDate");
        String fsmStartDateString = dateConverter.javaToString(fsmStartDate);
        exportInput.setParameterAsString("fsmStartDate", fsmStartDateString);

        PlainDate fsmEndDate = (PlainDate) getParameter("fsmEndDate");
        String fsmEndDateString = dateConverter.javaToString(fsmEndDate);
        exportInput.setParameterAsString("fsmEndDate", fsmEndDateString);

        PlainDate termlyExclustionStartDate = (PlainDate) getParameter("termlyExclusionStartDate");
        String termlyExclustionStartDateString = dateConverter.javaToString(termlyExclustionStartDate);
        exportInput.setParameterAsString("termlyExclusionStartDate", termlyExclustionStartDateString);

        PlainDate termlyExclustionEndDate = (PlainDate) getParameter("termlyExclusionEndDate");
        String termlyExclustionEndDateString = dateConverter.javaToString(termlyExclustionEndDate);
        exportInput.setParameterAsString("termlyExclusionEndDate", termlyExclustionEndDateString);

        PlainDate termlyAttendanceStartDate = (PlainDate) getParameter("termlyAttendanceStartDate");
        String termlyAttendanceStartDateString = dateConverter.javaToString(termlyAttendanceStartDate);
        exportInput.setParameterAsString("termlyAttendanceStartDate", termlyAttendanceStartDateString);

        PlainDate termlyAttendanceEndDate = (PlainDate) getParameter("termlyAttendanceEndDate");
        String termlyAttendanceEndDateString = dateConverter.javaToString(termlyAttendanceEndDate);
        exportInput.setParameterAsString("termlyAttendanceEndDate", termlyAttendanceEndDateString);

        if ("AUT".equals(m_term)) {
            PlainDate summerAttendanceStartDate = (PlainDate) getParameter("summerAttendanceStartDate");
            String summerAttendanceStartDateString = dateConverter.javaToString(summerAttendanceStartDate);
            exportInput.setParameterAsString("summerAttendanceStartDate", summerAttendanceStartDateString);

            PlainDate summerAttendanceEndDate = (PlainDate) getParameter("summerAttendanceEndDate");
            String summerAttendanceEndDateString = dateConverter.javaToString(summerAttendanceEndDate);
            exportInput.setParameterAsString("summerAttendanceEndDate", summerAttendanceEndDateString);
        }

        PlainDate previousTermStartDate = (PlainDate) getParameter("previousTermStartDate");
        String previousTermStartDateString = dateConverter.javaToString(previousTermStartDate);
        exportInput.setParameterAsString("previousTermStartDate", previousTermStartDateString);

        if ("PRU".equals(m_term) || "AUT".equals(m_term)) {
            PlainDate annualAttendanceEndDate = (PlainDate) getParameter("annualAttendanceEndDate");
            String annualAttendanceEndDateString = dateConverter.javaToString(annualAttendanceEndDate);
            exportInput.setParameterAsString("annualAttendanceEndDate", annualAttendanceEndDateString);
        }

        exportInput.setParameterAsString("ageAtDate", (String) getParameter("ageAtDate"));

        m_userData.setToolInput(exportInput);

        return ToolJob.createJob(m_export, m_userData, m_userData.getUserTempFolder(), false, Locale.getDefault());
    }

    /**
     * Gets the file name of the txt/xml file.
     *
     * @param fullFileName String
     * @return String
     */
    private static String getFileName(String fullFileName) {
        String fileName = null;
        if (fullFileName != null) {
            int endPosition = fullFileName.indexOf(XML_FILE_NAME_EXTENTION);
            if (endPosition == -1) {
                endPosition = fullFileName.indexOf(TXT_FILE_NAME_EXTENTION);
            }
            fileName = fullFileName.substring(0, endPosition);
        }
        return fileName;
    }

    /**
     * Writes the results to console.
     *
     * @param resultHandler ResultHandler
     * @throws IOException Signals that an I/O exception has occurred.
     * @throws JDOMException exception
     * @throws TransformerException exception
     */
    private void writeToConsole(ResultHandler resultHandler) throws IOException, JDOMException, TransformerException {
        // Check all file parameters
        // Check that the Directory parameter is valid
        String xslDirectoryName = (String) getParameter(PARAM_XSL_DIRECTORY);
        if (xslDirectoryName == null) {
            logMessage("ERROR: The '" + PARAM_XSL_DIRECTORY + "' parameter was not set in the Input Definition!");
            return;
        }
        File xslDirectory = new File(xslDirectoryName);
        if (!(xslDirectory.exists() && xslDirectory.isDirectory())) {
            logMessage("ERROR: The '" + PARAM_XSL_DIRECTORY + "': '" + xslDirectoryName
                    + "' was not found on the system!");
            return;
        }

        // Check that the intermediate file parameter is valid
        String intermediateFileName = (String) getParameter(PARAM_INTERMEDIATE_FILE);
        if (intermediateFileName == null) {
            logMessage("ERROR: The '" + PARAM_INTERMEDIATE_FILE + "' parameter was not set in the Input Definition!");
            return;
        }
        File intermediateFile = new File(xslDirectoryName + intermediateFileName);
        if (!intermediateFile.exists()) {
            logMessage("ERROR: The '" + PARAM_INTERMEDIATE_FILE + "': '" + intermediateFile.getPath()
                    + "' was not found on the system!");
            return;
        }

        // Check that the presentation file parameter is valid
        String presentationFileName = (String) getParameter(PARAM_PRESENTATION_FILE);
        if (presentationFileName == null) {
            logMessage("ERROR: The '" + PARAM_PRESENTATION_FILE + "' parameter was not set in the Input Definition!");
            return;
        }
        File presentationFile = new File(xslDirectoryName + presentationFileName);
        if (!presentationFile.exists()) {
            logMessage("ERROR: The '" + PARAM_PRESENTATION_FILE + "': '" + presentationFile.getPath()
                    + "' was not found on the system!");
            return;
        }

        // Get School Census Result File
        String xmlFile = resultHandler.getFilePath();
        File schoolWorkforceExportFile = new File(xmlFile);
        String schoolWorkforceExportFileName = getFileName(xmlFile);
        if (!schoolWorkforceExportFile.exists()) {
            logMessage("ERROR: The UK School Census Export File: '" + schoolWorkforceExportFile.getPath()
                    + "' was not found on the system!");
            return;
        }

        // Transform results to produce School Census Summary report in HTML form.
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        StreamSource reportStreamSource = new StreamSource(schoolWorkforceExportFile);

        // Transform School Census Detail File to produce validation errors
        FileWriter validationResultsFileWriter = new FileWriter(xslDirectoryName + VALIDATION_RESULT_FILE_NAME_SUFFIX);
        StreamResult validationStreamResult = new StreamResult(validationResultsFileWriter);
        StreamSource intermediateStreamSource = new StreamSource(intermediateFile);
        Transformer intermediateTransformer = transformerFactory.newTransformer(intermediateStreamSource);
        if (intermediateTransformer == null) {
            logMessage("ERROR: Unable to create XML Transformer for File: '" + xslDirectoryName
                    + VALIDATION_RESULT_FILE_NAME_SUFFIX + "'");
            return;
        }
        intermediateTransformer.transform(reportStreamSource, validationStreamResult);

        // Transform School Census Summary File to present in prettier format
        File summaryResultFile = new File(schoolWorkforceExportFileName + SUMMARY_RESULT_FILE_NAME_SUFFIX);
        StreamResult summaryResultStreamResult = new StreamResult(summaryResultFile);
        StreamSource presentationStreamSource = new StreamSource(presentationFile);
        Transformer presentationTransformer = transformerFactory.newTransformer(presentationStreamSource);
        if (presentationTransformer == null) {
            logMessage("ERROR: Unable to create XML Transformer for File: '" + schoolWorkforceExportFileName
                    + SUMMARY_RESULT_FILE_NAME_SUFFIX + "'");
            return;
        }
        presentationTransformer.transform(reportStreamSource, summaryResultStreamResult);

        // Provide a message to the user to instruct them on how to access the School Census Summary
        // Report HTML file.
        String summaryReportMessage = getSummaryReportMessage(summaryResultFile);
        logMessage(summaryReportMessage);
    }

    /**
     * Provide a message to the user to instruct them on how to access the School Work force Summary
     * Report HTML file.
     *
     * @param summaryResultFile File
     * @return String
     */
    private String getSummaryReportMessage(File summaryResultFile) {
        String summaryReportMessage;

        int aspenPathLocation = summaryResultFile.getAbsolutePath().indexOf(WEB_TEMP_DIRECTORY_NAME);
        int aspenNameWindows = summaryResultFile.getAbsolutePath().substring(1, (aspenPathLocation - 1))
                .lastIndexOf(WINDOWS_OS_DIRECTORY_DELIMITER) + 2;
        int aspenNameUnix = summaryResultFile.getAbsolutePath().substring(1, (aspenPathLocation - 1))
                .lastIndexOf(UNIX_OS_DIRECTORY_DELIMITER) + 2;
        int aspenName = 0;
        if (aspenNameWindows > aspenNameUnix) {
            aspenName = aspenNameWindows;
        } else {
            aspenName = aspenNameUnix;
        }

        String directoryName = summaryResultFile.getAbsolutePath().substring(aspenName, aspenPathLocation - 1);
        String fileLocation = summaryResultFile.getAbsolutePath().substring(aspenPathLocation);
        fileLocation = fileLocation.replace(CHAR_BACK_SLASH, CHAR_FORWARD_SLASH);

        summaryReportMessage = "Please paste this file path after \"" + directoryName + "/\" in your browser: "
                + fileLocation + " in order to access the School Workforce Census Summary Report.";

        return summaryReportMessage;
    }

}
