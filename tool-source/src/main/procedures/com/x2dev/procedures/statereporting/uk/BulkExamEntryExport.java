/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2013 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.uk;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.InvalidPreferenceException;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.ExamEntry;
import com.x2dev.sis.model.beans.ExamOption;
import com.x2dev.sis.model.beans.ExamSeries;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for bulk exporting exam results.
 *
 * @author X2 Development Corporation
 */
public class BulkExamEntryExport extends ProcedureJavaSource {
    private static final String ALIAS_CENTRE_NUMBER = "DFE CENTRE NUMBER";
    private static final String EXAM_EXPORT_FOLDER = "EXAM EXPORT FOLDER";

    private static final String PARAM_DATA_TYPE = "dataType";
    private static final String PARAM_SEASON = "examSeason";
    private static final String PARAM_QUERY_BY = "queryBy";

    private static final String QUERY_BY_CURRENT = "##current";
    private static final String QUERY_BY_SEASON = "##season";

    /*
     * File Types
     */
    private static final String FILE_TYPE_AMENDMENT = "A";
    private static final String FILE_TYPE_ENTRY = "E";
    private static final String FILE_TYPE_FORECAST = "F";

    /*
     * Export definition ID's
     */
    private static final String AMENDMENT_ENTRY_DEFINITION_ID = "EXP-UK-ENTRIES";
    private static final String FORECAST_DEFINITION_ID = "EXP-UK-FORECAST";

    /*
     * Procedure ID's
     */
    private static final String AMENDMENT_ENTRY_PROCEDURE_ID = "EXPDATA-UK-ENTRIES";
    private static final String FORECAST_PROCEDURE_ID = "EXPDATA-UK-FORECAST";

    private ImportExportDefinition m_export;
    private String m_exportDefinitionId;
    private File m_exportFolder;
    private boolean m_preview;
    private String m_procedureId;
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
        if (m_exportFolder != null) {
            for (ExamSeries series : m_series) {
                KeyValuePair<String, String> sequenceNumber = getNextSequenceNumber(series);

                ToolJob job = createToolJob(series, sequenceNumber.getKey(), sequenceNumber.getValue());
                try {
                    job.run();
                } finally {
                    job.getResultHandler().close();

                    ResultHandler resultHandler = job.getResultHandler();
                    writeToConsole(job.getResultHandler());

                    if (!m_preview) {
                        performDelivery(new File(resultHandler.getFilePath()), series, sequenceNumber.getKey(),
                                sequenceNumber.getValue());
                    }
                }
            }
        } else {
            logMessage("Please specify the folder where the exported files should be written into");
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        m_userData = userData;
        m_preview = ((Boolean) getParameter("preview")).booleanValue();

        /*
         * The folder where the files should be written into
         */
        String exportFileDirectory = (String) getSchool().getFieldValueByAlias(EXAM_EXPORT_FOLDER);
        if (!StringUtils.isEmpty(exportFileDirectory)) {
            exportFileDirectory = StringUtils.replaceAll(exportFileDirectory, "\\", "\\\\\\\\");
            m_exportFolder = new File(exportFileDirectory);
        }

        String fileType = (String) getParameter(PARAM_DATA_TYPE);
        if (FILE_TYPE_AMENDMENT.equals(fileType) || FILE_TYPE_ENTRY.equals(fileType)) {
            m_exportDefinitionId = AMENDMENT_ENTRY_DEFINITION_ID;
            m_procedureId = AMENDMENT_ENTRY_PROCEDURE_ID;
        } else if (FILE_TYPE_FORECAST.equals(fileType)) {
            m_exportDefinitionId = FORECAST_DEFINITION_ID;
            m_procedureId = FORECAST_PROCEDURE_ID;
        } else {
            throw new ToolRunException("Data type chosen is invalid.");
        }

        /*
         * The exam export to call.
         */
        X2Criteria exportCriteria = new X2Criteria();
        exportCriteria.addEqualTo(ImportExportDefinition.COL_ID, m_exportDefinitionId);

        QueryByCriteria exportQuery = new QueryByCriteria(ImportExportDefinition.class, exportCriteria);
        m_export = (ImportExportDefinition) getBroker().getBeanByQuery(exportQuery);

        /*
         * The scope to export
         */
        String queryBy = (String) getParameter(PARAM_QUERY_BY);
        if (queryBy.equals(QUERY_BY_CURRENT)) {
            SubQuery examEntriesQuery = new SubQuery(ExamEntry.class,
                    ExamEntry.REL_OPTION + "." + ExamOption.COL_SERIES_OID, getCurrentCriteria());

            X2Criteria examSeriesCriteria = new X2Criteria();
            examSeriesCriteria.addIn(X2BaseBean.COL_OID, examEntriesQuery);

            QueryByCriteria examSeriesQuery = new QueryByCriteria(ExamSeries.class, examSeriesCriteria);
            examSeriesQuery.setDistinct(true);
            m_series = getBroker().getCollectionByQuery(examSeriesQuery);
        } else if (queryBy.equals(QUERY_BY_SEASON)) {
            X2Criteria examSeriesCriteria = new X2Criteria();
            examSeriesCriteria.addEqualTo(ExamSeries.COL_SEASON_OID, getParameter(PARAM_SEASON));

            QueryByCriteria examSeriesQuery = new QueryByCriteria(ExamSeries.class, examSeriesCriteria);
            m_series = getBroker().getCollectionByQuery(examSeriesQuery);
        }
    }

    /**
     * Performs delivery.
     *
     * @param resultData File
     * @param series ExamSeries
     * @param sequenceNumber String
     * @param type String
     * @throws InvalidPreferenceException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void performDelivery(File resultData, ExamSeries series, String sequenceNumber, String type)
            throws InvalidPreferenceException, IOException {
        StringBuilder newPath = new StringBuilder();
        newPath.append(m_exportFolder.getPath());
        newPath.append(File.separator);
        newPath.append(getFilename(series, sequenceNumber, type));

        if (newPath != null && newPath.length() > 0) {
            FileOutputStream fos = null;
            FileInputStream fis = null;
            try {
                File destFile = new File(newPath.toString());
                File destPath = new File(destFile.getParent());
                if (!destPath.exists()) {
                    destPath.mkdirs();
                }
                if (destFile.exists()) {
                    destFile.delete();
                }
                destFile.createNewFile();
                fos = new FileOutputStream(destFile);
                fis = new FileInputStream(resultData);
                byte[] buffer = new byte[8192];
                int buffRead = 0;
                while ((buffRead = (fis.read(buffer))) > 0) {
                    fos.write(buffer, 0, buffRead);
                }
            } catch (IOException ioe) {
                // unable to copy file.
                logMessage(ioe.toString());
            } finally {
                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException ioe) {
                        // ignore.
                    }
                }
                if (fos != null) {
                    try {
                        fos.close();
                    } catch (IOException ioe) {
                        // ignore.
                    }
                }
            }
        }
    }

    /**
     * Generate the filename for the report.
     *
     * @param series ExamSeries
     * @param sequenceNumber String
     * @param type String
     * @return <data type><centre number><AB identifier>.X<sequence number>
     * @throws InvalidPreferenceException exception
     */
    private String getFilename(ExamSeries series, String sequenceNumber, String type)
            throws InvalidPreferenceException {
        StringBuilder filename = new StringBuilder();

        String seriesAbIdentifier = series.getAbIdentifier();
        if (StringUtils.isEmpty(seriesAbIdentifier)) {
            seriesAbIdentifier = series.getAwardingBody();
        }

        filename.append(type);
        filename.append(getSchool().getFieldValueByAlias(ALIAS_CENTRE_NUMBER));
        filename.append(seriesAbIdentifier); // AB identifier
        filename.append(".X");

        filename.append(sequenceNumber);

        return filename.toString();
    }

    /**
     * Returns the sequence number for the given exam series
     * The format of preference to save the sequence number is as follow:
     * SeasonOid:SeriesOid:xx,SeasonOid:SeriesOid:xx,...
     *
     * @param series ExamSeries
     * @return KeyValuePair
     * @throws InvalidPreferenceException exception
     */
    private KeyValuePair getNextSequenceNumber(ExamSeries series) throws InvalidPreferenceException {
        String sequenceNumbersString = PreferenceManager.getPreferenceValue(getOrganization().getRootOrganization(),
                "exam.export.sequenceNumber");

        Collection<String> seriesSeqNumbers = StringUtils.isEmpty(sequenceNumbersString) ? new ArrayList<String>()
                : StringUtils.convertDelimitedStringToList(sequenceNumbersString, ',');
        int seqNumber = 0;
        String existingSeriesSeq = "";
        for (String seriesSeqNum : seriesSeqNumbers) {
            if (seriesSeqNum.startsWith(series.getSeasonOid())) {
                int indexSeparation = seriesSeqNum.lastIndexOf(":");
                int currentSeqNumber = Integer.valueOf(seriesSeqNum.substring(indexSeparation + 1)).intValue();
                if (currentSeqNumber >= seqNumber) {
                    seqNumber = currentSeqNumber;
                }
            }

            if (seriesSeqNum.startsWith(series.getSeasonOid() + ":" + series.getOid())) {
                existingSeriesSeq = seriesSeqNum;
            }
        }

        seqNumber += 1;

        seriesSeqNumbers.remove(existingSeriesSeq);
        seriesSeqNumbers.add(series.getSeasonOid() + ":" + series.getOid() + ":" + seqNumber);
        if (!m_preview) {
            PreferenceManager.setPreferenceValue(getOrganization().getRootOrganization(), getBroker(),
                    "exam.export.sequenceNumber",
                    StringUtils.convertCollectionToDelimitedString(seriesSeqNumbers, ','));
        }

        String seqNumPaddedString = String.valueOf(seqNumber);
        seqNumPaddedString = org.apache.commons.lang3.StringUtils.substring(seqNumPaddedString, 0, 2);
        seqNumPaddedString = org.apache.commons.lang3.StringUtils.leftPad(seqNumPaddedString, 2, '0');

        String fileType = (String) getParameter(PARAM_DATA_TYPE); // E (Entries), A (Amendments), or
                                                                  // F (Forecast)
        if (StringUtils.isEmpty(fileType)) // If the file type is empty, decide the data type based
                                           // on the series sent.
        {
            if (!StringUtils.isEmpty(existingSeriesSeq)) {
                fileType = "A";
            } else {
                fileType = "E";
            }
        }

        return new KeyValuePair<String, String>(seqNumPaddedString, fileType);
    }

    /**
     * Writes the results to console.
     *
     * @param resultHandler ResultHandler
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void writeToConsole(ResultHandler resultHandler) throws IOException {
        BufferedReader in = new BufferedReader(new FileReader(resultHandler.getFilePath()));
        BufferedReader in_eol = new BufferedReader(new FileReader(resultHandler.getFilePath()));
        ArrayList<Integer> lineSizes = new ArrayList();

        try {
            while (in.ready()) {
                String line = in.readLine();
                if (line != null) {
                    // We store the line sizes to check the EOL characters in the next loop.
                    lineSizes.add(Integer.valueOf(line.length()));
                    logMessage(line);
                }
            }

            /*
             * Check the line ending matches what is specified in export definition
             */
            char buffer[] = new char[2];
            int j = 0;
            while (in_eol.ready()) {
                /*
                 * The first line is always the same to skip to the number of bytes read the first
                 * time.
                 *
                 * For the rest of the lines we have to take into account where we read two
                 * characters when
                 * our EOL might only be one character long. So for the Windows case we always skip
                 * the
                 * length of the line. For Unix format we skip the length of the line minus one.
                 */
                if (j == 0) {
                    in_eol.skip(lineSizes.get(j).longValue());
                } else {
                    in_eol.skip(lineSizes.get(j).longValue());
                }
                // Read the next two characters since both EOL formats are less than 2 chars
                in_eol.read(buffer, 0, 2);
                j++;
            }

        } finally {
            /*
             * Delete the result file
             */
            in.close();
            in_eol.close();
        }
    }

    /**
     * Creates a ToolJob.
     *
     * @param series ExamSeries
     * @param sequenceNumber String
     * @param dataType String
     * @return ToolJob
     * @throws Exception exception
     */
    private ToolJob createToolJob(ExamSeries series, String sequenceNumber, String dataType) throws Exception {
        ToolInput exportInput = new ToolInput(m_export, null, m_userData, Locale.getDefault());

        exportInput.setParameterAsString("procedureId", m_procedureId);
        exportInput.setParameterAsString("examSeason", series.getSeasonOid());
        exportInput.setParameterAsString("examSeries", series.getOid());
        exportInput.setParameterAsString("dataType", dataType);
        exportInput.setParameterAsString("sequenceNum", sequenceNumber);

        String queryBy = (String) getParameter(PARAM_QUERY_BY);
        if (queryBy.equals(QUERY_BY_CURRENT)) {
            SubQuery examEntriesQuery = new SubQuery(ExamEntry.class, X2BaseBean.COL_OID, getCurrentCriteria());
            Collection<String> entriesOids = getBroker().getSubQueryCollectionByQuery(examEntriesQuery);
            exportInput.setParameterAsString("entrieOids",
                    StringUtils.convertCollectionToDelimitedString(entriesOids, ','));
        }
        exportInput.setParameterAsString("queryBy", (String) getParameter("queryBy"));
        exportInput.setParameterAsString("sortBy", (String) getParameter("sortBy"));
        exportInput.setParameterAsString("saveResults", String.valueOf(getParameter("saveResults")));

        m_userData.setToolInput(exportInput);

        return ToolJob.createJob(m_export, m_userData, m_userData.getUserTempFolder(), false, getLocale());
    }
}
