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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FIELD_STD_NUM_ID_FL;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLRelationshipKey;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldBeanInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldExportFormatInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.FieldPlainRowInfo;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.StudentTransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObjectRecord;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StreamUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * The Class FLBulkRunProcedure.
 */
public class FLFasterImportProcedure extends ToolJavaSource {

    private static final String HEADER_RECORD_CODE = "00";

    private List<String> m_messages = new LinkedList<String>();
    private List<String> m_records = new ArrayList<String>();
    private int m_studentTransfersCounter = 0;
    private SimpleDateFormat m_timestampDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
    private SimpleDateFormat m_uiDateFormat = new SimpleDateFormat("M/dd/yyyy hh:mm a");

    /**
     * Import the data by reading each line by line.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    protected void importData(File sourceFile) throws Exception {
        parse(sourceFile);
        createStudentTransfers();
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
    }

    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        getJob().getInput().setFormat(ToolInput.TXT_FORMAT);
        importData((File) getParameter(FILE_KEY));
        ThreadUtils.checkInterrupt();
        exportResults();
    }

    /**
     * Creates the student transfers.
     *
     * @throws ParseException
     */
    private void createStudentTransfers() throws ParseException {
        TransferObjectHelper helper = new TransferObjectHelper(getBroker());
        FLFasterExportConfiguration exportConfig =
                new FLFasterExportConfiguration(getCurrentContext(), null, getBroker());

        List<List<String>> studentTransferRecords = getGroupedRecords(m_records);
        TransferObject transferObject = new TransferObject(helper);
        String description = "Imported " + transferObject.getCreationDate();
        transferObject.setDescription(description);
        transferObject.setStatus(StudentTransferObject.STATUS_INCOMING);
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        for (List<String> records : studentTransferRecords) {
            StudentTransferObject importedStudentTransfer = new StudentTransferObject(helper);

            transferObject.addStudentTransfer(importedStudentTransfer);

            String importedHeader = records.iterator().next();
            if (!importedHeader.substring(1, 3).equals(HEADER_RECORD_CODE)) {
                throw new X2RuntimeException();
            }
            String recordsType = importedHeader.substring(0, 1);
            String transferType =
                    records.size() > 1 ? TransferObject.TRANSFER_TYPE_RESPONSE : TransferObject.TRANSFER_TYPE_REQUEST;
            importedStudentTransfer.setRecordsType(recordsType);
            importedStudentTransfer.setTransferType(transferType);
            importedStudentTransfer.setDescription(description);
            importedStudentTransfer.setCreationDate(transferObject.getCreationDate());

            for (int i = 0; i < records.size(); i++) {
                TransferObjectRecord currentRecordObject = new TransferObjectRecord(helper, records.get(i));
                currentRecordObject.setOrderNumber(i + 1);
                importedStudentTransfer.addRecord(currentRecordObject);
            }

            if (transferType.equals(TransferObject.TRANSFER_TYPE_REQUEST)) {
                importedStudentTransfer.setStatus(StudentTransferObject.STATUS_INCOMING);
            } else {
                if (!transferType.equals(TransferObject.TRANSFER_TYPE_RESPONSE)) {
                    throw new X2RuntimeException();
                }
                StudentTransferObject request = helper.getRequestToReply(importedStudentTransfer, exportConfig);
                if (request != null) {
                    if (StudentTransferObject.STATUS_SENT.equals(request.getStatus())) {
                        importedStudentTransfer.assignStudent(request.getStudentOid());
                        importedStudentTransfer.setStatus(StudentTransferObject.STATUS_RECEIVED);
                        String requestCreationDate = request.getCreationDate();
                        String uiRequestDate = m_uiDateFormat.format(m_timestampDateFormat.parse(requestCreationDate));
                        importedStudentTransfer
                                .setDescription(importedStudentTransfer.getTransferType()
                                        + " received for "
                                        + request.getTransferType() + " from " + uiRequestDate);
                        request.setStatus(StudentTransferObject.STATUS_RECEIVED);
                        request.getTransferObject().persist();
                    }
                } else {
                    importedStudentTransfer.setStatus(StudentTransferObject.STATUS_INCOMING);
                }
            }

            // determine related student
            FieldPlainRowInfo stdIdPlainRowInfo =
                    new FieldPlainRowInfo(exportConfig, importedHeader, FIELD_STD_NUM_ID_FL);
            FLRelationshipKey relKey = FLFasterExportConfiguration
                    .getRelationshipKey(stdIdPlainRowInfo.getRecordType(), FIELD_STD_NUM_ID_FL);
            FieldBeanInfo stdIdBeanInfo = new FieldBeanInfo(relKey.getQueryClass(), relKey.getBeanPath(), dictionary);
            FieldExportFormatInfo stdIdExport = new FieldExportFormatInfo(exportConfig,
                    stdIdPlainRowInfo.getRecordType(), FIELD_STD_NUM_ID_FL);
            FieldInfo stdIdInfo = new FieldInfo(stdIdPlainRowInfo, stdIdBeanInfo, stdIdExport, dictionary);
            SisStudent relatedStudent = (SisStudent) helper.getRelatedBean(stdIdInfo);
            if (relatedStudent != null) {
                importedStudentTransfer.assignStudent(relatedStudent.getOid());
            }

            String sendingDst = helper.getFieldValue(importedHeader,
                    FLFasterExportConfiguration.FIELD_SENDING_DST_NUM, exportConfig);
            String seindingSkl = helper.getFieldValue(importedHeader,
                    FLFasterExportConfiguration.FIELD_SENDING_SKL_NUM, exportConfig);
            importedStudentTransfer.setFromInstitution(sendingDst + seindingSkl);

            logRecord(
                    "Student Transfer Object imported:" +
                            "\nStudent FL Identifier: "
                            + helper.getFieldValue(importedHeader, FIELD_STD_NUM_ID_FL, exportConfig) +
                            "\nTransfer Type: " + importedStudentTransfer.getTransferType() +
                            "\nRecords Type: " + importedStudentTransfer.getRecordsType() +
                            "\nNumber of records: " + records.size() +
                            "\nStudent Transfer status: " + importedStudentTransfer.getStatus() +
                            "\nFrom Institution: " + sendingDst + seindingSkl + "\n\n");
            m_studentTransfersCounter++;
        }
        transferObject.persist();
    }

    /**
     * Print out the logs to the user.
     *
     * @throws X2BaseException exception
     */
    private void exportResults() throws X2BaseException {
        StringBuilder buffer = new StringBuilder(256);
        String sourceFileName = ((File) getParameter(FILE_KEY)).getName();

        buffer.append("  Results" + '\n');
        buffer.append("------------------------------------------------\n");
        buffer.append("   File name: " + sourceFileName + '\n');
        buffer.append("   Student Transfer Objects Imported: " + m_studentTransfersCounter + '\n');
        buffer.append("------------------------------------------------\n");

        buffer.append(StringUtils.convertCollectionToDelimitedString(m_messages, "\n"));

        try {
            ByteArrayInputStream inputStream = new ByteArrayInputStream(buffer.toString().getBytes());
            try {
                StreamUtils.copyStream(inputStream, getResultHandler().getOutputStream());
            } finally {
                inputStream.close();
            }
        } catch (FileNotFoundException fnfe) {
            throw new X2BaseException(fnfe);
        } catch (IOException ioe) {
            throw new X2BaseException(ioe);
        }
    }

    /**
     * Log a message with a line number.
     *
     * @param lineNumber int
     * @param message String
     */
    private void logRecord(String message) {
        m_messages.add(message);
    }

    /**
     * Parses a file.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    private void parse(File sourceFile) throws Exception {
        try (BufferedReader br = new BufferedReader(new FileReader(sourceFile.getPath()))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.length() == 1020) {
                    m_records.add(line);
                } else {
                    throw new X2RuntimeException();
                }
            }
        }
    }

    /**
     * Group by student transfer.
     *
     * @param records ArrayList<String>
     * @return List
     */
    private List<List<String>> getGroupedRecords(List<String> records) {
        ArrayList<List<String>> groupedRecords = new ArrayList<>();
        ArrayList<String> currentRecords = null;
        for (String record : records) {
            String recordType = record.substring(1, 3);
            if (recordType.equals(HEADER_RECORD_CODE)) {
                if (currentRecords != null) {
                    groupedRecords.add(currentRecords);
                }
                currentRecords = new ArrayList<>();
            }
            currentRecords.add(record);
        }
        groupedRecords.add(currentRecords);
        return groupedRecords;
    }
}
