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
public class SchoolWorkforceSummary extends ProcedureJavaSource {
    private static final char CHAR_FORWARD_SLASH = '/';
    private static final char CHAR_BACK_SLASH = '\\';
    private static final String WINDOWS_OS_DIRECTORY_DELIMITER = "\\";
    private static final String UNIX_OS_DIRECTORY_DELIMITER = "/";
    private static final String XML_FILE_NAME_EXTENTION = ".xml";
    private static final String TXT_FILE_NAME_EXTENTION = ".txt";

    private static final String PARAM_CENSUSU_DATE = "censusDate";
    private static final String PARAM_CONTRACT_START_DATE = "contractStartDate";
    private static final String PARAM_CONTRACT_END_DATE = "contractEndDate";
    private static final String PARAM_TERM_START_DATE = "termStartDate";
    private static final String PARAM_TERM_END_DATE = "termEndDate";
    private static final String PARAM_ABSENCE_START_DATE = "absenceStartDate";
    private static final String PARAM_ABSENCE_END_DATE = "absenceEndDate";
    private static final String PARAM_INTERMEDIATE_FILE = "intermediateFile";
    private static final String PARAM_PRESENTATION_FILE = "presentationFile";
    private static final String PARAM_XSL_DIRECTORY = "xslfiles";

    private static final String EXPORT_PARAM_PROCEDURE_ID = "procedureId";
    private static final String EXPORT_PARAM_SCHOOL_OID = "schoolOid";

    private static final String SCHOOL_WORKFORCE_EXPORT_ID = "EXP-UK-SWC";

    private static final String WEB_TEMP_DIRECTORY_NAME = "temp";
    private static final String VALIDATION_RESULT_FILE_NAME_SUFFIX = "SWF12ValidationResults.xml";
    private static final String SUMMARY_RESULT_FILE_NAME_SUFFIX = "_summary.html";

    private ImportExportDefinition m_export;
    private UserDataContainer m_userData;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        ToolJob job = createToolJob();
        ResultHandler resultHandler = null;

        try {
            job.run();
        } catch (Exception ex) {
            throw ex;
        } finally {
            resultHandler = job.getResultHandler();
            if (resultHandler != null) {
                resultHandler.close();
            }
        }

        writeToConsole(resultHandler);
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_userData = userData;

        // The exam export procedure to call.
        X2Criteria exportCriteria = new X2Criteria();
        exportCriteria.addEqualTo(ImportExportDefinition.COL_ID, SCHOOL_WORKFORCE_EXPORT_ID);

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
        Locale userLocale = m_userData.getLocale();
        if (userLocale == null) {
            userLocale = Locale.getDefault();
        }

        Converter dateConverter = ConverterFactory.getConverterForClass(PlainDate.class.getName(), userLocale);

        ToolInput exportInput = new ToolInput(m_export, null, m_userData, userLocale);

        String procedureId = (String) getParameter(EXPORT_PARAM_PROCEDURE_ID);
        exportInput.setParameterAsString(EXPORT_PARAM_PROCEDURE_ID, procedureId);

        String schoolOid = getSchool() == null ? null : getSchool().getOid();
        exportInput.setParameterAsString(EXPORT_PARAM_SCHOOL_OID, schoolOid);

        PlainDate censusDate = (PlainDate) getParameter(PARAM_CENSUSU_DATE);
        String censusDateString = dateConverter.javaToString(censusDate);
        exportInput.setParameterAsString(PARAM_CENSUSU_DATE, censusDateString);

        PlainDate contractStartDate = (PlainDate) getParameter(PARAM_CONTRACT_START_DATE);
        String contractStartDateString = dateConverter.javaToString(contractStartDate);
        exportInput.setParameterAsString(PARAM_CONTRACT_START_DATE, contractStartDateString);

        PlainDate contractEndDate = (PlainDate) getParameter(PARAM_CONTRACT_END_DATE);
        String contractEndDateString = dateConverter.javaToString(contractEndDate);
        exportInput.setParameterAsString(PARAM_CONTRACT_END_DATE, contractEndDateString);

        PlainDate termStartDate = (PlainDate) getParameter(PARAM_TERM_START_DATE);
        String termStartDateString = dateConverter.javaToString(termStartDate);
        exportInput.setParameterAsString(PARAM_TERM_START_DATE, termStartDateString);

        PlainDate termEndDate = (PlainDate) getParameter(PARAM_TERM_END_DATE);
        String termEndDateString = dateConverter.javaToString(termEndDate);
        exportInput.setParameterAsString(PARAM_TERM_END_DATE, termEndDateString);

        PlainDate absenceStartDate = (PlainDate) getParameter(PARAM_ABSENCE_START_DATE);
        String absenceStartDateString = dateConverter.javaToString(absenceStartDate);
        exportInput.setParameterAsString(PARAM_ABSENCE_START_DATE, absenceStartDateString);

        PlainDate absenceEndDate = (PlainDate) getParameter(PARAM_ABSENCE_END_DATE);
        String absenceEndDateString = dateConverter.javaToString(absenceEndDate);
        exportInput.setParameterAsString(PARAM_ABSENCE_END_DATE, absenceEndDateString);

        m_userData.setToolInput(exportInput);

        ToolJob schoolWorkforceCensusJob =
                ToolJob.createJob(m_export, m_userData, m_userData.getUserTempFolder(), false, userLocale);
        return schoolWorkforceCensusJob;
    }

    /**
     * Gets the file name of the txt/xml file.
     *
     * @param fullFileName String
     * @return file name
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

        // Get School Work force Census Result File
        String xmlFile = resultHandler.getFilePath();
        File schoolWorkforceExportFile = new File(xmlFile);
        String schoolWorkforceExportFileName = getFileName(xmlFile);
        if (!schoolWorkforceExportFile.exists()) {
            logMessage("ERROR: The UK School Census Export File: '" + schoolWorkforceExportFile.getPath()
                    + "' was not found on the system!");
            return;
        }

        // Transform results to produce School Work force Summary report in HTML form.
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        StreamSource reportStreamSource = new StreamSource(schoolWorkforceExportFile);

        // Transform SchoolWorkforce Detail File to produce validation errors
        FileWriter validationResultsFileWriter = new FileWriter(xslDirectoryName + VALIDATION_RESULT_FILE_NAME_SUFFIX);
        StreamResult validationStreamResult = new StreamResult(validationResultsFileWriter);
        StreamSource intermediateStreamSource = new StreamSource(intermediateFile);
        Transformer intermediateTransformer = transformerFactory.newTransformer(intermediateStreamSource);
        intermediateTransformer.transform(reportStreamSource, validationStreamResult);

        // Transform SchoolWorkforce Summary File to present in prettier format
        File summaryResultFile = new File(schoolWorkforceExportFileName + SUMMARY_RESULT_FILE_NAME_SUFFIX);
        StreamResult summaryResultStreamResult = new StreamResult(summaryResultFile);
        StreamSource presentationStreamSource = new StreamSource(presentationFile);
        Transformer presentationTransformer = transformerFactory.newTransformer(presentationStreamSource);
        presentationTransformer.transform(reportStreamSource, summaryResultStreamResult);

        // Provide a message to the user to instruct them on how to access the School Work force
        // Summary Report HTML file.
        String summaryReportMessage = getSummaryReportMessage(summaryResultFile);
        logMessage(summaryReportMessage);
    }

}
