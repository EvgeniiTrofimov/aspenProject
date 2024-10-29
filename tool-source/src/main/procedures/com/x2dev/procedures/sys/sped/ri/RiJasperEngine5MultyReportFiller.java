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
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.JarPluginManager;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.ReportConstants;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports5.engine.JRException;
import net.sf.jasperreports5.engine.JRExporterParameter;
import net.sf.jasperreports5.engine.JRPrintElement;
import net.sf.jasperreports5.engine.JRPrintPage;
import net.sf.jasperreports5.engine.JRPrintText;
import net.sf.jasperreports5.engine.JasperExportManager;
import net.sf.jasperreports5.engine.JasperFillManager;
import net.sf.jasperreports5.engine.JasperPrint;
import net.sf.jasperreports5.engine.export.JRCsvExporter;
import net.sf.jasperreports5.engine.export.JRHtmlExporter;
import net.sf.jasperreports5.engine.export.JRHtmlExporterParameter;
import net.sf.jasperreports5.engine.export.JRXlsExporter;
import net.sf.jasperreports5.engine.util.JRClassLoader;
import org.apache.pdfbox.util.PDFMergerUtility;

/**
 * Helper class for filling few reports using the JasperReports 5.0 series engine
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class RiJasperEngine5MultyReportFiller {

    private static final String CURRENT_PAGE_NUMBER = "${CPN}";
    private static final String TOTAL_PAGE_NUMBER = "${TPN}";
    private List<JasperPrint> m_reports = new ArrayList<JasperPrint>();
    private ToolJob m_toolJob = null;
    private OutputStream m_outputStream = null;
    private X2Broker m_broker = null;

    public RiJasperEngine5MultyReportFiller(ToolJob toolJob, OutputStream outputStream, X2Broker broker) {
        m_broker = broker;
        m_toolJob = toolJob;
        m_outputStream = outputStream;
    }

    /**
     * Fills the passed reports which put into addReport method
     * concatenate reports and and write it into outputstream in m_resultHandler
     *
     * @throws X2BaseException
     * @throws IOException
     */
    public void fillReport()
            throws X2BaseException, IOException {
        try {
            JarPluginManager jarPluginManager = new JarPluginManager();
            Tool tool = m_toolJob.getTool();
            if (!StringUtils.isEmpty(tool.getJarPluginPath())) {
                ClassLoader jarClassLoader =
                        jarPluginManager.getParentClassLoader(tool, m_broker, tool.getClass().getClassLoader(),
                                null);
                JRClassLoader.setCustomClassLoader(jarClassLoader);
            }

            try {
                if (!m_reports.isEmpty()) {
                    exportResults(m_toolJob, m_outputStream);
                }

            } finally {
                JRClassLoader.clearCustomClassLoader();
            }

        } catch (JRException jre) {
            throw new X2BaseException(jre);
        }
    }

    /**
     * add report which will fill
     *
     * @param format
     * @param parameters
     * @param dataSource
     * @throws X2BaseException
     * @throws IOException
     */
    public void addReport(InputStream format,
                          Map<String, Object> parameters,
                          net.sf.jasperreports5.engine.JRDataSource dataSource

    )
            throws X2BaseException, IOException {
        try {
            JarPluginManager jarPluginManager = new JarPluginManager();
            Tool tool = m_toolJob.getTool();

            if (!StringUtils.isEmpty(tool.getJarPluginPath())) {
                ClassLoader jarClassLoader =
                        jarPluginManager.getParentClassLoader(tool, m_broker, tool.getClass().getClassLoader(),
                                null);
                JRClassLoader.setCustomClassLoader(jarClassLoader);
            }

            try {



                JasperPrint reportPrint =
                        JasperFillManager.fillReport(format, parameters, dataSource);



                if (reportPrint != null && reportPrint.getPages().size() > 0) {
                    m_reports.add(reportPrint);
                }
            } finally {
                JRClassLoader.clearCustomClassLoader();
            }
        } catch (JRException jre) {
            throw new X2BaseException(jre);
        }
    }

    /**
     * Concatenated input PDFs and write it in resultHandler's outputstream
     *
     * @param pdffiles
     * @param outputStream
     * @throws IOException
     */
    public void sendConcatenatedPDFStream(List<byte[]> pdffiles, OutputStream outputStream)
            throws IOException {

        List<ByteArrayInputStream> pdfs = new ArrayList<ByteArrayInputStream>();
        for (int i = 0; i < pdffiles.size(); i++) {
            pdfs.add(new ByteArrayInputStream(pdffiles.get(i)));
        }

        PDFMergerUtility mergePdf = new PDFMergerUtility();
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        mergePdf.setDestinationStream(output);
        try {
            Iterator<ByteArrayInputStream> iteratorPDFs = pdfs.iterator();

            while (iteratorPDFs.hasNext()) {
                InputStream pdf = iteratorPDFs.next();
                mergePdf.addSource(pdf);

            }
            // Create a writer for the outputstream
            mergePdf.mergeDocuments();
            outputStream.write(output.toByteArray());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * swap expression put into text
     * current behavior swap ${CPN} into "Page currentPageN of"
     * ${TPN} into totPageNumber
     *
     * @param reports
     */
    protected void swapExpression(List<JasperPrint> reports) {
        int totPageNumber = 0;
        for (JasperPrint reportPrint : reports) {
            totPageNumber += reportPrint.getPages().size();
        }

        int currentPage = 1;
        for (JasperPrint reportPrint : reports) {
            for (JRPrintPage page : reportPrint.getPages()) {
                List<JRPrintElement> elements = page.getElements();
                // Loop all elements on page
                for (JRPrintElement jpe : elements) {
                    // Check if text element
                    if (jpe instanceof JRPrintText) {
                        JRPrintText jpt = (JRPrintText) jpe;
                        // Check if current page marker
                        if (CURRENT_PAGE_NUMBER.equals(jpt.getFullText())) {
                            jpt.setText("Page " + currentPage + " of"); // Replace marker
                            continue;
                        }
                        // Check if totale page marker
                        if (TOTAL_PAGE_NUMBER.equals(jpt.getFullText())) {
                            jpt.setText(" " + totPageNumber); // Replace marker
                        }
                    }
                }
                currentPage++;
            }
        }
    }

    /**
     * Exports the Jasper results to an appropriately named file based on the output format.
     *
     * @param reportPrint JasperPrint
     * @param job ToolJob
     * @param outputStream OutputStream
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportResults(ToolJob job, OutputStream outputStream)
            throws JRException, IOException {


        swapExpression(m_reports);
        switch (job.getInput().getFormat()) {
            case ToolInput.CSV_FORMAT:
                JRCsvExporter csvExporter = new JRCsvExporter();
                csvExporter.setParameter(JRExporterParameter.JASPER_PRINT_LIST, m_reports);
                csvExporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
                csvExporter.exportReport();
                break;

            case ToolInput.HTML_FORMAT:


                JRHtmlExporter exporter = new JRHtmlExporter();
                exporter.setParameter(JRExporterParameter.JASPER_PRINT_LIST, m_reports);
                exporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
                exporter.setParameter(JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN, Boolean.FALSE);
                exporter.exportReport();
                break;

            case ToolInput.PDF_FORMAT:
                List<byte[]> reports = new ArrayList<byte[]>();
                for (JasperPrint jasperPrint : m_reports) {
                    reports.add(JasperExportManager.exportReportToPdf(jasperPrint));
                }
                sendConcatenatedPDFStream(reports, outputStream);

                break;

            case ToolInput.XLS_FORMAT:
                // report name must be less than 31 characters
                for (JasperPrint reportPrint : m_reports) {
                    reportPrint.setName(StringUtils.truncate(reportPrint.getName(),
                            ReportConstants.XLS_NAME_MAX_CHARS));
                }
                JRXlsExporter xlsExporter = new JRXlsExporter();
                xlsExporter.setParameter(JRExporterParameter.JASPER_PRINT_LIST, m_reports);
                xlsExporter.setParameter(JRExporterParameter.OUTPUT_STREAM, outputStream);
                xlsExporter.exportReport();
                break;
        }

    }
}
