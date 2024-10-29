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

import com.follett.fsc.core.k12.beans.Document;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PublishReportsManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;

/**
 * Extended BaseFormReportJavaSource, provide ability concatenate reports with different format.
 * Using when one format is landscape and other is portrait, or has difference size.
 * Warning! This decision is imposed a restriction to use jasper report's aggregate functionality,
 * because reports prepared separately (fore each format) and nothing to know about yourself.
 * Default implementation provide just next aggregate data - it is total page count and current page
 * for use this put next expression into text field - ${CPN} - for Current page Number
 * ${TPN} for total page number.
 * For add additional expression need to override swapExpression method for each jasper engine
 * version
 * see getFiller1,getFiller3,getFiller5 method
 *
 * @author Follett Software Company
 * @copyright 2017
 */
abstract public class RiMultipleGridReportJavaSource extends BaseFormReportJavaSource {

    protected static final String SAVE_TO_DOCUMENTS_IEP_PARAM = "saveToDocumentsIEP";

    /**
     * contain data about single report
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    public class ReportData {

        public ReportData(Object grid, InputStream format, Map<String, Object> parameters) {
            m_grid = grid;
            m_format = format;
            m_parameters = parameters;

        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            ReportData other = (ReportData) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_grid == null) {
                if (other.m_grid != null) {
                    return false;
                }
            } else if (!m_grid.equals(other.m_grid)) {
                return false;
            }
            if (m_parameters == null) {
                if (other.m_parameters != null) {
                    return false;
                }
            } else if (!m_parameters.equals(other.m_parameters)) {
                return false;
            }
            return true;
        }

        private InputStream m_format = null;
        private Map<String, Object> m_parameters;
        private Object m_grid;

        public InputStream getFormat() {
            return m_format;
        }

        public Map<String, Object> getParameters() {
            return m_parameters;
        }

        public Object getGrid() {
            return m_grid;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_grid == null) ? 0 : m_grid.hashCode());
            result = prime * result + ((m_parameters == null) ? 0 : m_parameters.hashCode());
            return result;
        }

        private RiMultipleGridReportJavaSource getOuterType() {
            return RiMultipleGridReportJavaSource.this;
        }

    }



    private List<ReportData> m_grids = null;
    private File m_saveToDocumentsResultFile = null;
    private OutputStream m_saveToDocumentsOutputStream = null;

    /**
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected final JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        grid.beforeTop();
        m_grids = gatherDataList();
        if (m_grids == null) {
            m_grids = new ArrayList<ReportData>();
        }
        return grid;
    }

    /**
     * Prepares the List&lt;ReportData&gt; that will be used by the Jasper design. This method
     * is
     * called
     * after <code>initialize(UserDataContainer)</code> and before <code>releaseResources()</code>.
     *
     * @return List<ReportData> ReportData
     * @throws Exception exception
     */
    protected abstract List<ReportData> gatherDataList() throws Exception;

    /**
     *
     * @param toolJob
     * @param outputStream
     * @param broker
     * @return Jasper Engine Multy ReportFiller for 1.0 version
     */
    protected RiJasperEngine1MultyReportFiller getFiller1(ToolJob toolJob, OutputStream outputStream, X2Broker broker) {
        return new RiJasperEngine1MultyReportFiller(toolJob, outputStream, broker);
    }

    /**
     *
     * @param toolJob
     * @param outputStream
     * @param broker
     * @return Jasper Engine Multy ReportFiller for 3.0 version
     */
    protected RiJasperEngine3MultyReportFiller getFiller3(ToolJob toolJob, OutputStream outputStream, X2Broker broker) {
        return new RiJasperEngine3MultyReportFiller(toolJob, outputStream, broker);
    }

    /**
     *
     * @param toolJob
     * @param outputStream
     * @param broker
     * @return Jasper Engine Multy ReportFiller for 5.0 version
     */
    protected RiJasperEngine5MultyReportFiller getFiller5(ToolJob toolJob, OutputStream outputStream, X2Broker broker) {
        return new RiJasperEngine5MultyReportFiller(toolJob, outputStream, broker);
    }


    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     */
    @Override
    protected void publishResults() throws Exception {
        // TODO Auto-generated method stub
        String engineVersion = ((Report) getJob().getTool()).getEngineVersion();
        boolean saveToDocuments = getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM) != null &&
                ((Boolean) getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM)).booleanValue();


        /*
         * Check to see if gatherData returned a structure that contains no data.
         * If it does, discard it so that a JREmptyDataSource is created and used below
         */
        Iterator<ReportData> gridIterator = m_grids.iterator();
        while (gridIterator.hasNext()) {
            ReportData reportData = gridIterator.next();
            Object object = reportData.getGrid();
            if (object instanceof DataGrid) {
                DataGrid dataSource = (DataGrid) object;
                if (dataSource.getRows().isEmpty()) {
                    gridIterator.remove();

                }
            }
        }

        if (m_grids.isEmpty()) {
            getResultHandler().setEmpty(true);
        }

        if (m_grids.isEmpty()) {
            Object data = null;
            if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
                data = new net.sf.jasperreports.engine.JREmptyDataSource(0);
            } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
                data = new net.sf.jasperreports3.engine.JREmptyDataSource(0);
            } else if (Report.REPORT_ENGINE_5_RELEASE.equals(engineVersion)) {
                data = new net.sf.jasperreports5.engine.JREmptyDataSource(0);
            } else {
                throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00008", new Object[] {
                        engineVersion
                });
            }
            ReportData reportData = new ReportData(data, getFormat(), getParameters());
            m_grids.add(reportData);
        }

        logDataPrepared();

        ThreadUtils.checkInterrupt();

        OutputStream pdfOutputStream = getResultHandler().getOutputStream();
        if (saveToDocuments) {
            pdfOutputStream = prepareSaveToDocuments(pdfOutputStream);
        }

        Object grid0 = m_grids.get(0).getGrid();
        /*
         * If the job has been aborted then don't bother filling the format or exporting the
         * results.
         */
        if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
            if (PublishReportsManager.isPublishing(getJob(), grid0, this)) {
                PublishReportsManager publishManager =
                        createReportsManager(getJob(),
                                getFormat(),
                                getParameters(),
                                getSchool(),
                                getOrganization(),
                                getBroker());
                publishManager.publishReport();
            } else if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
                RiJasperEngine1MultyReportFiller filler = getFiller1(getJob(), pdfOutputStream, getBroker());
                for (ReportData reportData : m_grids) {
                    Object dataSource = reportData.getGrid();
                    if (dataSource instanceof net.sf.jasperreports.engine.JRDataSource) {
                        filler.addReport(reportData.getFormat(), reportData.getParameters(),
                                (net.sf.jasperreports.engine.JRDataSource) dataSource);
                    } else {
                        throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                                dataSource.getClass().toString(), engineVersion
                        });
                    }
                }
                filler.fillReport();
            } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
                RiJasperEngine3MultyReportFiller filler = getFiller3(getJob(), pdfOutputStream, getBroker());
                for (ReportData reportData : m_grids) {
                    Object dataSource = reportData.getGrid();
                    if (dataSource instanceof net.sf.jasperreports3.engine.JRDataSource) {
                        filler.addReport(reportData.getFormat(), reportData.getParameters(),
                                (net.sf.jasperreports3.engine.JRDataSource) dataSource);
                    } else {
                        throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                                dataSource.getClass().toString(), engineVersion
                        });
                    }
                }
                filler.fillReport();
            } else if (Report.REPORT_ENGINE_5_RELEASE.equals(engineVersion)) {
                RiJasperEngine5MultyReportFiller filler = getFiller5(getJob(), pdfOutputStream, getBroker());
                for (ReportData reportData : m_grids) {
                    Object dataSource = reportData.getGrid();
                    if (dataSource instanceof net.sf.jasperreports5.engine.JRDataSource) {
                        filler.addReport(reportData.getFormat(), reportData.getParameters(),
                                (net.sf.jasperreports5.engine.JRDataSource) dataSource);
                    } else {
                        throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                                dataSource.getClass().toString(), engineVersion
                        });
                    }
                }
                filler.fillReport();
            } else {
                throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00008", engineVersion);
            }
        }
        if (saveToDocuments) {
            saveToDocuments(pdfOutputStream);
        }
    }


    /**
     * @param outputStream
     * @return
     * @throws FileNotFoundException
     */
    private OutputStream prepareSaveToDocuments(OutputStream outputStream) throws FileNotFoundException {
        m_saveToDocumentsResultFile = new File(
                getJob().getTempFolder() + File.separator + System.currentTimeMillis()
                        + Integer.toString(getJob().getJobId()) + "report.pdf");

        m_saveToDocumentsOutputStream = new FileOutputStream(m_saveToDocumentsResultFile);
        OutputStream oss[] = new OutputStream[2];
        oss[0] = outputStream;
        oss[1] = m_saveToDocumentsOutputStream;
        DuplicateOutputStream dos = new DuplicateOutputStream(oss);
        return dos;
    }

    /**
     * @param pdfOutputStream
     * @throws IOException
     */
    private void saveToDocuments(OutputStream pdfOutputStream) throws IOException {
        // Create a Student Document record with the PDF set as the file source.
        if (m_saveToDocumentsResultFile.isFile()) {
            m_saveToDocumentsOutputStream.close();
            Student student = null;
            Person person = null;
            School school = null;
            X2BaseBean ownerBean = getFormOwner();
            if (ownerBean != null && ownerBean instanceof IepData) {
                student = ((IepData) ownerBean).getStudent();
                person = student.getPerson();
                school = student.getSchool();
                if (student != null && person != null) {

                    // Prepare some global variables so the method createDocument() will work.
                    DataDictionary dictionary =
                            DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                    DataDictionaryField documentTypeField = dictionary.findDataDictionaryField(
                            com.follett.fsc.core.k12.beans.Document.class.getName(),
                            com.follett.fsc.core.k12.beans.Document.COL_TYPE_CODE);
                    m_fileNameField = dictionary.findDataDictionaryField(
                            com.follett.fsc.core.k12.beans.Document.class.getName(),
                            com.follett.fsc.core.k12.beans.Document.COL_FILENAME);
                    String reportName = (getParameter(DOCUMENT_NAME_PARAM) != null
                            ? (String) getParameter(DOCUMENT_NAME_PARAM)
                            : getReportName());
                    m_documentName = buildDocumentName(reportName, getCurrentContext(), dictionary);
                    m_documentType =
                            getParameter(DOCUMENT_TYPE_PARAM) != null ? (String) getParameter(DOCUMENT_TYPE_PARAM)
                                    : "IEP";
                    if (m_documentType != null && m_documentType.length() > documentTypeField.getDatabaseLength()) {
                        m_documentType = m_documentType.substring(0, documentTypeField.getDatabaseLength());
                    }

                    String[] columnKeys = {
                            com.follett.fsc.core.k12.beans.Document.COL_PERSON_OID,
                            com.follett.fsc.core.k12.beans.Document.COL_NAME};
                    int[] mapSizes = {500, 1};
                    m_existingDocuments = getExistingDocuments(m_documentName, columnKeys, mapSizes);

                    // Find or create the document record.
                    com.follett.fsc.core.k12.beans.Document document =
                            createDocument(true, person, student, school, m_saveToDocumentsResultFile);

                    getBroker().saveBeanForced(document);
                }
            }
            m_saveToDocumentsResultFile.delete();
        }

    }

    /**
     * Some customers have custom fields on the Documents table for upload date and school year.
     * If the aliases are present, populate them.
     * upload-date
     * school-year
     *
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#createDocument(boolean,
     *      com.follett.fsc.core.k12.beans.Person, com.follett.fsc.core.k12.beans.X2BaseBean,
     *      com.follett.fsc.core.k12.beans.School, java.io.File)
     */
    @Override
    public Document createDocument(boolean overwriteExisting,
                                   Person person,
                                   X2BaseBean bean,
                                   School school,
                                   File resultFile) {
        Document document = super.createDocument(overwriteExisting, person, bean, school, resultFile);
        if (document != null) {
            document.setFieldValueByAlias("school-year", getCurrentContext().getContextId());

            PlainDate today = new PlainDate();
            SystemStringConverter converter = (SystemStringConverter) ConverterFactory
                    .getConverterForClass(Converter.DATE_CONVERTER, getLocale(), true);
            document.setFieldValueByAlias("upload-date", converter.getSystemString(today));
        }
        return document;
    }

    /**
     * A duplicating output stream.
     * Anything written to the output stream will be written to
     * multiple underlying output streams.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    private static class DuplicateOutputStream extends FilterOutputStream {
        private OutputStream m_streams[];

        /**
         * Constructor.
         * Provides a list of output streams to be written to.
         *
         * @param streams
         */
        public DuplicateOutputStream(OutputStream streams[]) {
            super(streams[0]);
            m_streams = streams;
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(int)
         */
        @Override
        public void write(int i) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(i);
            }
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(byte[])
         */
        @Override
        public void write(byte[] b) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b);
            }
        }

        /**
         * Write to all underlying output streams
         *
         * @see java.io.FilterOutputStream#write(byte[], int, int)
         */
        @Override
        public void write(byte[] b, int offset, int length) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b, offset, length);
            }
        }

        /**
         * Flush all underlying output streams
         *
         * @see java.io.FilterOutputStream#flush()
         */
        @Override
        public void flush() throws IOException {
            for (OutputStream os : m_streams) {
                os.flush();
            }
        }

        /**
         * Close all underlying output streams
         *
         * @see java.io.FilterOutputStream#close()
         */
        @Override
        public void close() throws IOException {
            for (OutputStream os : m_streams) {
                os.close();
            }
        }
    }
}
