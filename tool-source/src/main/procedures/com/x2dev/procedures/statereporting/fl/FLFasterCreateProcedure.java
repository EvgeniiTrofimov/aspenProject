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

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.s_sectionAToBFields;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.StudentTransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObjectRecord;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;


/**
 * The Class FLFasterCreateProcedure.
 */
public class FLFasterCreateProcedure extends ToolJavaSource {


    public static final String RECORDS_TYPE_INTERDISTRICT = "I";
    public static final String RECORDS_TYPE_SECONDARY = "S";

    protected static final String ID_EXPORT = "EXP-FL-FST";
    protected static final String ID_VALIDATION_PROCEDURE = "FL-FASTER-VAL";

    protected static final String INPUT_PARAM_RECORDS_TYPE = "recordsType";
    protected static final String INPUT_PARAM_RESTRICT_BY_RECORD_TYPE = "restrictByRecordType";
    protected static final String INPUT_PARAM_RUN_VALIDATIONS = "runValidations";
    protected static final String INPUT_PARAM_SHOW_TOTALS = "showTotals";
    protected static final String INPUT_PARAM_SORT_BY = "sortBy";
    protected static final String INPUT_PARAM_STUDENT_TRANSFER_OID = "studentTransferOid";
    protected static final String INPUT_PARAM_TRANSFER_OBJECT_OID = "transferObjectOid";
    protected static final String INPUT_PARAM_TRANSFER_TYPE = "transferType";
    protected static final String INPUT_PARAM_QUERY_BY = "queryBy1";
    protected static final String INPUT_PARAM_QUERY_STRING = "queryString1";
    protected static final String INPUT_PARAM_DESCRIPTION = "description";

    private static final String HEADER_RECORD_CODE = "00";

    private List<String> m_exceptionList = new ArrayList<>();

    private String m_resultOid = null;
    private List<String> m_rows = null;
    private SimpleDateFormat m_timeFormat = new SimpleDateFormat("HH:mm:ss");
    private UserDataContainer m_userData = null;


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
    }


    /**
     * Run.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        execute();

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
     * Creates the transfer object.
     *
     * @param transferType String
     * @param recordsType String
     * @param description String
     * @param job ToolJob
     * @param helper TransferObjectHelper
     * @param exportConfig FLFasterExportConfiguration
     * @param resultRows Map<ExportFormatResult,List<ExportFormatRow>>
     * @return TransferObject
     * @throws FileNotFoundException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private TransferObject createTransferObject(String transferType,
                                                String recordsType,
                                                String description,
                                                ToolJob job,
                                                TransferObjectHelper helper,
                                                FLFasterExportConfiguration exportConfig,
                                                Map<ExportFormatResult, List<ExportFormatRow>> resultRows)
            throws FileNotFoundException, IOException {
        List<String> plainRows = new ArrayList<>();
        ResultHandler resultHandler = job.getResultHandler();
        try (FileReader fr = new FileReader(new File((resultHandler.getFilePath())))) {
            BufferedReader br = new BufferedReader(fr);
            String line = null;
            while ((line = br.readLine()) != null) {
                plainRows.add(line);
            }
        }
        if (plainRows.size() > 0) {
            if (!resultRows.entrySet().isEmpty()) {
                TransferObject transferObject = new TransferObject(helper);
                transferObject.setTransferType(transferType);
                transferObject.setRecordsType(recordsType);
                transferObject.setDescription(description);
                transferObject.assignResult(resultRows.entrySet().iterator().next().getKey().getOid());

                List<ExportFormatRow> exportFormatRows = resultRows.values().iterator().next();

                StudentTransferObject currentStudentTransfer = null;
                String currentStudentOid = null;
                for (int i = 0; i < plainRows.size(); i++) {

                    String currentPlainRow = plainRows.get(i);
                    ExportFormatRow currentRow = exportFormatRows.get(i);

                    String studentOid = exportConfig.getStudentOidByRow(currentRow);
                    boolean isStudentChanged = !studentOid.equals(currentStudentOid);

                    if (currentStudentTransfer == null || isStudentChanged) {
                        String importedHeader = currentPlainRow;
                        if (!importedHeader.substring(1, 3).equals(HEADER_RECORD_CODE)) {
                            throw new X2RuntimeException();
                        }
                        currentStudentOid = exportConfig.getStudentOidByRow(currentRow);
                        currentStudentTransfer = new StudentTransferObject(helper, transferObject);
                        currentStudentTransfer.assignStudent(currentStudentOid);
                        // Create procedure can create transfer objects only with status NEW
                        currentStudentTransfer.setStatus(StudentTransferObject.STATUS_NEW);
                        String addressedDst = helper.getFieldValue(importedHeader,
                                FLFasterExportConfiguration.FIELD_ADDRESSED_DST_NUM, exportConfig);
                        String addressedSkl = helper.getFieldValue(importedHeader,
                                FLFasterExportConfiguration.FIELD_ADDRESSED_SKL_NUM, exportConfig);
                        currentStudentTransfer.setToInstitution(addressedDst + addressedSkl);
                        transferObject.addStudentTransfer(currentStudentTransfer);
                    }
                    TransferObjectRecord record = new TransferObjectRecord(helper, currentPlainRow);
                    record.assignResultRow(currentRow);
                    currentStudentTransfer.addRecord(record);
                }
                transferObject.persist();

                return transferObject;
            }
        }

        return null;
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     */
    private void execute() throws Exception {
        List<String> outputList = new ArrayList<>();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ImportExportDefinition.COL_ID, ID_EXPORT);
        ImportExportDefinition exportDefinition = (ImportExportDefinition) getBroker()
                .getBeanByQuery(new QueryByCriteria(ImportExportDefinition.class, criteria));

        if (exportDefinition != null) {
            ToolJob exportJob = FLFasterUtils.createToolJob(exportDefinition, m_userData, getBroker(), getLocale(),
                    getParameters(), getCurrentCriteria());
            ToolJavaSource fasterExport = exportJob.getToolJavaSource();
            Date startTime = Calendar.getInstance().getTime();
            int total = 0;
            Boolean showTotals = (Boolean) getParameter(INPUT_PARAM_SHOW_TOTALS);
            try {
                exportJob.run();
            } finally {
                ResultHandler resultHandler = exportJob.getResultHandler();
                resultHandler.close();
                try {
                    Date endTime = Calendar.getInstance().getTime();
                    if (exportJob.getStatus() == ToolJob.STATUS_SUCCESS) {
                        TransferObjectHelper helper = new TransferObjectHelper(getBroker());
                        String transferType = (String) getParameter(INPUT_PARAM_TRANSFER_TYPE);
                        String recordsType = (String) getParameter(INPUT_PARAM_RECORDS_TYPE);
                        String description = (String) getParameter(INPUT_PARAM_DESCRIPTION);
                        Boolean runValidations = (Boolean) getParameter(INPUT_PARAM_RUN_VALIDATIONS);
                        String restrictionByRecordType = (String) getParameter(INPUT_PARAM_RESTRICT_BY_RECORD_TYPE);

                        m_rows = FLFasterUtils.getRecords(exportJob.getResultHandler());

                        if (StringUtils.isEmpty(restrictionByRecordType)
                                || restrictionByRecordType.equals(TransferObjectRecord.RECORD_TYPE_00)) {
                            FLFasterExportConfiguration exportConfig =
                                    new FLFasterExportConfiguration(getCurrentContext(), null, getBroker());
                            Map<ExportFormatResult, List<ExportFormatRow>> resultRows = getResultRows(fasterExport);
                            TransferObject transferObject =
                                    createTransferObject(transferType,
                                            recordsType,
                                            description, exportJob, helper,
                                            exportConfig,
                                            resultRows);
                            if (transferObject != null) {
                                Collection<StudentTransferObject> studentTransfers =
                                        transferObject.getStudentTransfers();
                                prepareReplies(studentTransfers, helper, exportConfig);
                                if (runValidations == null || runValidations.booleanValue()) {
                                    for (StudentTransferObject studentTransfer : studentTransfers) {
                                        int numOfErrors = validateStudentTransfer(studentTransfer);
                                        if (numOfErrors > 0) {
                                            studentTransfer.setStatus(StudentTransferObject.STATUS_ERROR);
                                            studentTransfer.persist();
                                        }
                                    }
                                }
                            }
                        }

                        outputList.addAll(m_rows);

                        if (showTotals == null || showTotals.booleanValue()) {
                            String comment = getCommentString(null, exportDefinition.getId(), m_rows.size(), "Success",
                                    startTime, endTime);
                            outputList.add(comment);
                        }

                        total += m_rows.size();

                        fasterExport = null;
                    } else {
                        String status = null;
                        switch (exportJob.getStatus()) {
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
                                status = "Unexpected " + exportJob.getStatus();
                                break;
                        }
                        if (showTotals == null || showTotals.booleanValue()) {
                            String comment =
                                    getCommentString(null, exportDefinition.getId(), 0, status, startTime, endTime);
                            outputList.add(comment);
                        }

                        if (exportJob.getThrowable() != null) {
                            m_exceptionList.add("\nStack Trace for : " + exportDefinition.getName());
                            StringWriter sw = new StringWriter();
                            PrintWriter pw = new PrintWriter(sw);
                            exportJob.getThrowable().printStackTrace(pw);
                            m_exceptionList.add(sw.toString());
                        }
                    }
                } catch (FileNotFoundException fnfe) {
                    throw new X2BaseException(fnfe);
                } catch (IOException ioe) {
                    throw new X2BaseException(ioe);
                }
            }

            if (showTotals == null || showTotals.booleanValue()) {
                outputList.add("Grand Total: " + total);
            }

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
    }


    /**
     * Fill section B.
     *
     * @param replies Collection<StudentTransferObject>
     * @param helper TransferObjectHelper
     * @param exportConfig FLFasterExportConfiguration
     */
    private void fillSectionB(Collection<StudentTransferObject> replies,
                              TransferObjectHelper helper,
                              FLFasterExportConfiguration exportConfig) {
        for (StudentTransferObject reply : replies) {
            if (!reply.getTransferType().equals(TransferObject.TRANSFER_TYPE_RESPONSE)) {
                throw new X2RuntimeException();
            }
            StudentTransferObject request = helper.getRequestToReply(reply, exportConfig);
            if (request == null) {
                throw new X2RuntimeException();
            }
            TransferObjectRecord responseHeaderRecord = reply.getRecords().iterator().next();
            TransferObjectRecord requestHeaderRecord = request.getRecords().iterator().next();

            String currentHeader = reply.getRecords().iterator().next().getPlainRow();
            int currentHeaderIndex = -1;
            for (int i = 0; i < m_rows.size(); i++) {
                if (m_rows.get(i).equals(currentHeader)) {
                    currentHeaderIndex = i;
                }
            }

            if (currentHeaderIndex == -1) {
                throw new X2RuntimeException();
            }

            for (Entry<String, String> aToBFields : s_sectionAToBFields.entrySet()) {
                String requestField = aToBFields.getKey();
                String replyHeaderValue = exportConfig.getPlainRowFieldValue(requestHeaderRecord.getPlainRow(),
                        FL_EXPORT.RECORD00, requestField);

                String replyField = aToBFields.getValue();
                responseHeaderRecord.setPlainRowFieldValue(replyField, replyHeaderValue, exportConfig);
            }

            m_rows.set(currentHeaderIndex, responseHeaderRecord.getPlainRow());

            reply.getTransferObject().persist();
        }
    }


    /**
     * Gets the comment string.
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
            output.append("Total Records ");
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


    /**
     * Gets the result rows.
     *
     * @param fasterExport ToolJavaSource
     * @return Map
     */
    private Map<ExportFormatResult, List<ExportFormatRow>> getResultRows(ToolJavaSource fasterExport) {
        Method getResultRows;
        try {
            getResultRows = fasterExport.getClass().getDeclaredMethod("getResultRows");
            getResultRows.setAccessible(true);
            return (Map<ExportFormatResult, List<ExportFormatRow>>) getResultRows.invoke(fasterExport);
        } catch (NoSuchMethodException | SecurityException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        }
        return null;
    }


    /**
     * Prepare replies.
     *
     * @param studentTransfers Collection<StudentTransferObject>
     * @param helper TransferObjectHelper
     * @param exportConfig FLFasterExportConfiguration
     */
    private void prepareReplies(Collection<StudentTransferObject> studentTransfers,
                                TransferObjectHelper helper,
                                FLFasterExportConfiguration exportConfig) {
        List<StudentTransferObject> replies = new ArrayList<StudentTransferObject>();
        for (StudentTransferObject studentTransfer : studentTransfers) {
            if (studentTransfer.getTransferType().equals(TransferObject.TRANSFER_TYPE_RESPONSE)) {
                StudentTransferObject request = helper.getRequestToReply(studentTransfer, exportConfig);
                if (request != null) {
                    replies.add(studentTransfer);
                    studentTransfer.setDescription("Reply prepared for Request " + request.getDescription());
                }
            }
        }
        fillSectionB(replies, helper, exportConfig);
    }


    /**
     * Validate student transfer.
     *
     * @param studentTransfer StudentTransferObject
     * @return int
     * @throws Exception exception
     */
    private int validateStudentTransfer(StudentTransferObject studentTransfer) throws Exception {
        int validationErrorsTotal = 0;
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Procedure.COL_ID, ID_VALIDATION_PROCEDURE);
        Procedure procedure = (Procedure) getBroker()
                .getBeanByQuery(new QueryByCriteria(Procedure.class, criteria));

        Map<String, Object> parameters = new HashMap<>();
        parameters.put(INPUT_PARAM_STUDENT_TRANSFER_OID, studentTransfer.getOid());
        parameters.put(INPUT_PARAM_TRANSFER_OBJECT_OID, studentTransfer.getTransferObject().getOid());
        ToolJob procedureJob = FLFasterUtils.createToolJob(procedure, parameters, m_userData, getBroker(), getLocale());

        try {
            procedureJob.run();
        } finally {
            ResultHandler resultHandler = procedureJob.getResultHandler();
            resultHandler.close();
            try {
                List<String> outputList = new ArrayList<>();
                if (procedureJob.getStatus() == ToolJob.STATUS_SUCCESS) {
                    List<String> rows = FLFasterUtils.getRecords(procedureJob.getResultHandler());
                    for (String row : rows) {
                        if (row.startsWith(FLFasterValidationProcedure.MESSAGE_VALIDATION_ERRORS_TOTAL)) {
                            validationErrorsTotal = Integer.parseInt(
                                    row.replace(FLFasterValidationProcedure.MESSAGE_VALIDATION_ERRORS_TOTAL, ""));
                            break;
                        }
                    }
                    outputList.addAll(rows);
                    for (String comment : outputList) {
                        PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
                        stream.print(comment + "\n");
                    }
                } else {
                    if (procedureJob.getThrowable() != null) {
                        m_exceptionList.add("\nStack Trace for : " + procedure.getName());
                        StringWriter sw = new StringWriter();
                        PrintWriter pw = new PrintWriter(sw);
                        procedureJob.getThrowable().printStackTrace(pw);
                        m_exceptionList.add(sw.toString());
                    }
                }
            } catch (FileNotFoundException fnfe) {
                throw new X2BaseException(fnfe);
            } catch (IOException ioe) {
                throw new X2BaseException(ioe);
            }
        }
        return validationErrorsTotal;
    }
}
