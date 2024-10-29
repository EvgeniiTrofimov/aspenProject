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

import com.follett.fsc.core.k12.tools.ToolJavaSource;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.StudentTransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObject;
import com.x2dev.procedures.statereporting.fl.TransferObjectHelper.TransferObjectRecord;
import com.x2dev.utils.X2BaseException;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * The Class FLFasterSendProcedure.
 */
public class FLFasterSendProcedure extends ToolJavaSource {

    protected static final String INPUT_PARAM_ALL_STUDENTS = "allStudents";
    protected static final String INPUT_PARAM_STUDENT_TRANSFER_OIDS = "studentTransferOids";
    protected static final String INPUT_PARAM_TRANSFER_OBJECT_OID = "transferObjectOid";

    private FLFasterExportConfiguration m_exportConfig = null;
    private TransferObjectHelper m_helper = null;
    private Collection<StudentTransferObject> m_studentTransfers = null;
    private TransferObject m_transferObject = null;

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_helper = new TransferObjectHelper(getBroker());
        String transferObjectOid = (String) getParameter(INPUT_PARAM_TRANSFER_OBJECT_OID);
        m_transferObject = m_helper.findTransferObject(transferObjectOid);

        m_exportConfig = new FLFasterExportConfiguration(getCurrentContext(), m_transferObject.getOid(), getBroker());

        Boolean allStudents = (Boolean) getParameter(INPUT_PARAM_ALL_STUDENTS);
        List<String> filteredStudents = null;
        if (!allStudents.booleanValue()) {
            String studentTransferOids = (String) getParameter(INPUT_PARAM_STUDENT_TRANSFER_OIDS);
            filteredStudents = studentTransferOids == null ? new ArrayList<String>()
                    : Arrays.asList(studentTransferOids.split(";"));
        }
        m_studentTransfers = m_transferObject.getStudentTransfers();
        if (filteredStudents != null) {
            List<StudentTransferObject> filteredStudentTransfers = new ArrayList<>();
            for (StudentTransferObject studentTransfer : m_studentTransfers) {
                if (filteredStudents.contains(studentTransfer.getOid())) {
                    filteredStudentTransfers.add(studentTransfer);
                }
            }
            m_studentTransfers = filteredStudentTransfers;
        }
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
    }

    /**
     * Adds the to transferred file.
     *
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void addRecordsToTransFile() throws IOException {
        PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
        for (StudentTransferObject studentTransfer : m_studentTransfers) {
            for (TransferObjectRecord record : studentTransfer.getRecords()) {
                stream.print(record.getPlainRow() + "\r\n");
            }
        }
    }

    /**
     * Change status.
     */
    private void changeStatus() {
        if (m_transferObject.getTransferType().equals(TransferObject.TRANSFER_TYPE_REQUEST)) {
            for (StudentTransferObject studentTransfer : m_studentTransfers) {
                studentTransfer.setStatus(StudentTransferObject.STATUS_SENT);
            }
        } else if (m_transferObject.getTransferType().equals(TransferObject.TRANSFER_TYPE_RESPONSE)) {
            for (StudentTransferObject studentTransfer : m_studentTransfers) {
                StudentTransferObject requestToReply = m_helper.getRequestToReply(studentTransfer, m_exportConfig);
                if (requestToReply != null) {
                    studentTransfer.setStatus(StudentTransferObject.STATUS_REPLIED);
                    requestToReply.setStatus(StudentTransferObject.STATUS_REPLIED);
                    TransferObject reqTransferObject = requestToReply.getTransferObject();
                    if (isAllStudentTransfersFulfilled(reqTransferObject)) {
                        reqTransferObject.setStatus(StudentTransferObject.STATUS_REPLIED);
                        reqTransferObject.persist();
                    }
                } else {
                    studentTransfer.setStatus(StudentTransferObject.STATUS_SENT);
                }
            }
        }

        if (isAllStudentTransfersFulfilled(m_transferObject)) {
            m_transferObject.setStatus(StudentTransferObject.STATUS_SENT);
            m_transferObject.persist();
        }
    }

    /**
     * Send.
     *
     * @return true, if successful
     */
    private boolean send() {
        return true;
    }

    /**
     * Executes the jobs selected.
     *
     * @throws Exception exception
     */
    private void execute() throws Exception {
        addRecordsToTransFile();
        if (send()) {
            changeStatus();
        }
    }

    /**
     * Checks if is all student transfers fulfilled.
     *
     * @param transferObject TransferObject
     * @return true, if is all student transfers fulfilled
     */
    private boolean isAllStudentTransfersFulfilled(TransferObject transferObject) {
        boolean isAllStudentTransfersSent = true;
        Collection<StudentTransferObject> studentTransfers = transferObject.getStudentTransfers();
        for (StudentTransferObject studentTransfer : studentTransfers) {
            if (studentTransfer.getStatus().equals(StudentTransferObject.STATUS_NEW)) {
                isAllStudentTransfersSent = false;
            }
        }
        return isAllStudentTransfersSent;
    }
}
