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

import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.JobResult;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLRelationshipKey;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationError;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.FasterExport;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * The Class FLFasterValidationProcedure.
 */
public class FLFasterValidationProcedure extends ToolJavaSource {
    public static final String MESSAGE_VALIDATION_ERRORS_TOTAL = "Validation errors total: ";

    public static final String RECORDS_TYPE_INTERDISTRICT = "I";
    public static final String RECORDS_TYPE_SECONDARY = "S";

    public static final String TRANSFER_TYPE_RESPONSE = "Response";
    public static final String TRANSFER_TYPE_REQUEST = "Request";

    protected static final String EXPORT_ID = "EXP-FL-FST";

    protected static final String INPUT_PARAM_PERFORM_EXPORT = "performExport";
    protected static final String INPUT_PARAM_PERFORM_VALIDATION = "performValidation";
    protected static final String INPUT_PARAM_RECORDS_TYPE = "recordsType";
    protected static final String INPUT_PARAM_STUDENT_TRANSFER_OID = "studentTransferOid";
    protected static final String INPUT_PARAM_TRANSFER_OBJECT_OID = "transferObjectOid";
    protected static final String INPUT_PARAM_TRANSFER_TYPE = "transferType";
    protected static final String INPUT_PARAM_USER_NAME = "userName";

    private Pattern m_countPattern;
    private String m_resultOid = null;


    /**
     * Gets the num of records.
     *
     * @param handler ResultHandler
     * @return int
     * @throws IOException Signals that an I/O exception has occurred.
     */
    protected int getNumOfRecords(ResultHandler handler) throws IOException {
        int count = 0;
        FileInputStream result = new FileInputStream(handler.getFilePath());
        try (BufferedReader br = new BufferedReader(new InputStreamReader(result))) {
            String extractContent = br.readLine(); //
            if (!StringUtils.isEmpty(extractContent)) {
                Matcher m = m_countPattern.matcher(extractContent);
                if (m.find()) {
                    count = Integer.parseInt(m.group(1));
                }
            }
        }

        return count;
    }


    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_resultOid = getJob().getJobResultOid();
    }


    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#run()
     */
    @Override
    protected void run() throws Exception {
        Boolean performValidation = (Boolean) getParameter(INPUT_PARAM_PERFORM_VALIDATION);
        if (performValidation == null || performValidation.booleanValue()) {
            execute();
        }

        if (m_resultOid != null && m_resultOid.length() > 0) {
            X2Broker broker = getBroker();
            JobResult result = (JobResult) broker.getBeanByOid(JobResult.class, m_resultOid);
            if (result != null) {
                broker.saveBeanForced(result);
            }
        }
    }


    /**
     * Execute.
     *
     * @throws Exception exception
     */
    private void execute() throws Exception {
        int validationErrorsTotal = 0;
        int rowsTotal = 0;
        List<String> outputList = new ArrayList<>();

        String studentTransferOid = (String) getParameter(INPUT_PARAM_STUDENT_TRANSFER_OID);
        String transferObjectOid = (String) getParameter(INPUT_PARAM_TRANSFER_OBJECT_OID);

        FLFasterExportConfiguration helper =
                new FLFasterExportConfiguration(getCurrentContext(), transferObjectOid, getBroker());

        FLFasterValidations validations = new FLFasterValidations();

        for (FLExport export : helper.getExports()) {
            Collection<ExportFormatRow> exportFormatRows = helper.getExportFormatRows(export, studentTransferOid);
            rowsTotal += exportFormatRows.size();
            Map<String, Map<String, List<X2BaseBean>>> stdRowErrors = new HashMap<>();
            for (ExportFormatRow row : exportFormatRows) {
                FLRelationshipKey relKey = export.getRelationshipKeyByClassName(SisStudent.class.getName());
                String stdLocalId = helper.getExportFormatRowFieldValue(row, export, relKey);
                List<FLValidationError> exportErrors = new ArrayList<>();
                for (FLValidationRule rule : ((FasterExport) export).getValidationRules(helper, validations)) {
                    List<FLValidationError> errors;
                    try {
                        errors = rule.getProcessor().getValidationErrors(helper, export, row);
                    } catch (IllegalStateException e) {
                        StringBuilder message = new StringBuilder();
                        message.append("\n");
                        message.append("Validate Rule processing exception for export ");
                        message.append(export.name());
                        message.append(" rule # ");
                        message.append(rule.getRuleNumber());
                        message.append("\n");
                        message.append(e.getMessage());
                        message.append("\n");
                        throw new IllegalStateException(message.toString());
                    }
                    for (FLValidationError error : errors) {
                        error.setRuleNumber(rule.getRuleNumber());
                    }
                    exportErrors.addAll(errors);
                }
                List<X2BaseBean> errors = helper.persistValidationErrors(exportErrors);

                if (!errors.isEmpty()) {
                    Map<String, List<X2BaseBean>> rowErrors = stdRowErrors.get(stdLocalId);
                    if (rowErrors == null) {
                        rowErrors = new HashMap<>();
                        stdRowErrors.put(stdLocalId, rowErrors);
                    }

                    rowErrors.put(helper.getPlainRow(row), errors);
                }
            }

            List<String> errorMessages = new ArrayList<>();
            if (stdRowErrors.size() > 0) {
                DataDictionary dictionary = helper.getValidationErrorDictionary();
                for (Map<String, List<X2BaseBean>> rowErrors : stdRowErrors.values()) {
                    for (Entry<String, List<X2BaseBean>> entry : rowErrors.entrySet()) {
                        List<X2BaseBean> errors = entry.getValue();
                        for (X2BaseBean error : errors) {
                            StringBuilder message = new StringBuilder();
                            message.append("Record violating the rule:");
                            message.append("\n");
                            message.append(entry.getKey());
                            message.append("\n");
                            message.append("Rule # " + error
                                    .getFieldValueByAlias(FLExportConfiguration.ALIAS_VALIDATION_ERR_RULE_NUMBER,
                                            dictionary));
                            message.append("\n");
                            message.append(
                                    error.getFieldValueByAlias(FLExportConfiguration.ALIAS_VALIDATION_ERR_MESSAGE,
                                            dictionary));
                            message.append("\n");
                            errorMessages.add(message.toString());
                            validationErrorsTotal++;
                        }
                    }
                }
            }

            outputList.addAll(errorMessages);
        }

        outputList.add("Records total: " + rowsTotal);
        outputList.add(MESSAGE_VALIDATION_ERRORS_TOTAL + validationErrorsTotal + "\n");

        for (String comment : outputList) {
            PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
            stream.print(comment + "\n");
        }
    }
}
