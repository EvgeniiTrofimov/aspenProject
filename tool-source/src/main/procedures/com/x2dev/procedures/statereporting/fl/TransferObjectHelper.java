/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.fl;

import static com.x2dev.procedures.statereporting.fl.FLFasterExportConfiguration.*;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExportFormatResult;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DictionaryHelper;
import com.ibm.icu.text.SimpleDateFormat;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2RuntimeException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class FASTERTransferHelper.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class TransferObjectHelper {

    /**
     * The Class StudentTransferObject.
     */
    public static class StudentTransferObject {
        public static final String STATUS_MERGED = "MERGED";
        public static final String STATUS_ERROR = "ERROR";
        public static final String STATUS_INCOMING = "INCOMING";
        public static final String STATUS_NEW = "NEW";
        public static final String STATUS_RECEIVED = "RECEIVED";
        public static final String STATUS_REPLIED = "REPLIED";
        public static final String STATUS_SENT = "SENT";

        public static final String ALIAS_TNR_STD_CREATION_DATE = "tnr-std-creation-date";
        public static final String ALIAS_TNR_STD_DDX_OID = "tnr-std-ddx-oid";
        public static final String ALIAS_TNR_STD_DESCRIPTION = "tnr-std-description";
        public static final String ALIAS_TNR_STD_FROM_INSTITUTION = "tnr-std-from-institution";
        public static final String ALIAS_TNR_STD_MERGE_RESULT = "tnr-std-merge-result";
        public static final String ALIAS_TNR_STD_NAME = "tnr-std-name";
        public static final String ALIAS_TNR_STD_OID = "tnr-std-oid";
        public static final String ALIAS_TNR_STD_STATUS = "tnr-std-status";
        public static final String ALIAS_TNR_STD_RECORDS_TYPE = "tnr-std-records-type";
        public static final String ALIAS_TNR_STD_RESULT_OID = "tnr-std-result-oid";
        public static final String ALIAS_TNR_STD_TNR_OID = "tnr-std-tnr-oid";
        public static final String ALIAS_TNR_STD_TO_INSTITUTION = "tnr-std-to-institution";
        public static final String ALIAS_TNR_STD_TRANSFER_MESSAGE = "tnr-std-transfer-message";
        public static final String ALIAS_TNR_STD_TRANSFER_TYPE = "tnr-std-tnr-type";

        public static final String DDX_ID = "FL-FASTER-TNR-STD";

        private static final String DDX_OID = "ddxFlFstTnrStd";

        private TransferObjectHelper m_helper = null;
        private UserDefinedTableC m_studentTransfer = null;
        private TransferObject m_transferObject = null;
        private List<TransferObjectRecord> m_records;

        /**
         * Instantiates a new transfer object.
         *
         * @param helper FASTERTransferHelper
         * @param transfer UserDefinedTableA
         */
        public StudentTransferObject(TransferObjectHelper helper, UserDefinedTableC transfer) {
            m_helper = helper;
            m_studentTransfer = transfer;
        }

        /**
         * Instantiates a new student transfer object.
         *
         * @param helper TransferObjectHelper
         * @param to TransferObject
         */
        public StudentTransferObject(TransferObjectHelper helper, TransferObject to) {
            m_helper = helper;
            m_studentTransfer = new UserDefinedTableC(m_helper.getBroker().getPersistenceKey());

            DataDictionary dictionary = m_helper.getDictionary(DDX_ID);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_DDX_OID, DDX_OID, dictionary);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TNR_OID, to.getOid(), dictionary);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_CREATION_DATE, to.getCreationDate(), dictionary);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_DESCRIPTION, to.getDescription(), dictionary);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_RECORDS_TYPE, to.getRecordsType(), dictionary);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TRANSFER_TYPE, to.getTransferType(), dictionary);
            m_studentTransfer.setOrganization1Oid("*dst");

            m_transferObject = to;
        }

        /**
         * Instantiates a new student transfer object.
         *
         * @param helper TransferObjectHelper
         */
        public StudentTransferObject(TransferObjectHelper helper) {
            m_helper = helper;
            m_studentTransfer = new UserDefinedTableC(m_helper.getBroker().getPersistenceKey());
            DataDictionary dictionary = m_helper.getDictionary(DDX_ID);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_DDX_OID, DDX_OID, dictionary);
            m_studentTransfer.setOrganization1Oid("*dst");
        }

        /**
         * Adds the record.
         *
         * @param record String
         */
        public void addRecord(TransferObjectRecord record) {
            getRecords().add(record);
        }

        /**
         * Assign student.
         *
         * @param studentOid String
         */
        public void assignStudent(String studentOid) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_OID, studentOid, m_helper.getDictionary(DDX_ID));
            SisStudent student = (SisStudent) m_helper.getBroker().getBeanByOid(SisStudent.class, studentOid);
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_NAME, student.getNameView(),
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Equals.
         *
         * @param o Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof StudentTransferObject)) {
                return false;
            }
            StudentTransferObject studentTransfer = (StudentTransferObject) o;
            return studentTransfer.equals(o);
        }

        /**
         * Gets the creation date.
         *
         * @return String
         */
        public String getCreationDate() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_CREATION_DATE,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the description.
         *
         * @return String
         */
        public String getDescription() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_DESCRIPTION,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the from institution.
         *
         * @return void
         */
        public void getFromInstitution() {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_FROM_INSTITUTION, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the java name.
         *
         * @param alias String
         * @param helper TransferObjectHelper
         * @return String
         */
        public static String getJavaName(String alias, TransferObjectHelper helper) {
            DataDictionaryField field = helper.getDictionary(DDX_ID).findDataDictionaryFieldByAlias(alias);
            String javaName = null;
            if (field != null) {
                javaName = field.getJavaName();
            }
            return javaName;
        }

        /**
         * Gets the student transfer oid.
         *
         * @return void
         */
        public String getOid() {
            return m_studentTransfer.getOid();
        }

        /**
         * Gets the records.
         *
         * @return Collection
         */
        public List<TransferObjectRecord> getRecords() {
            if (m_records == null || m_records.isEmpty()) {
                m_records = new ArrayList<>();
                DataDictionary recDictionary = m_helper.getDictionary(TransferObjectRecord.DDX_ID);
                DataDictionaryField udcOidField =
                        recDictionary.findDataDictionaryFieldByAlias(TransferObjectRecord.ALIAS_REC_TNR_STD_OID);
                X2Criteria recCriteria = new X2Criteria();
                recCriteria.addEqualTo(udcOidField.getJavaName(), getOid());
                QueryByCriteria recQuery = new QueryByCriteria(UserDefinedTableB.class, recCriteria);
                List<UserDefinedTableB> records =
                        (List<UserDefinedTableB>) m_helper.getBroker().getCollectionByQuery(recQuery);

                if (!records.isEmpty()) {
                    for (UserDefinedTableB udb : records) {
                        TransferObjectRecord record = new TransferObjectRecord(m_helper, udb);
                        m_records.add(record);
                    }

                    Collections.sort(m_records, new Comparator<TransferObjectRecord>() {
                        @Override
                        public int compare(TransferObjectRecord o1, TransferObjectRecord o2) {
                            int compResult = o1.getRecordSortOrder().compareTo(o2.getRecordSortOrder());
                            if (compResult == 0) {
                                compResult = o1.getRecordType().compareTo(o2.getRecordType());
                            }
                            return compResult;
                        }

                    });
                }
            }
            return m_records;
        }

        /**
         * Gets the records type.
         *
         * @return String
         */
        public String getRecordsType() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_RECORDS_TYPE,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the status.
         *
         * @return String
         */
        public String getStatus() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_STATUS,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the student name.
         *
         * @return String
         */
        public String getStudentName() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_NAME, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the student oid.
         *
         * @return void
         */
        public String getStudentOid() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_OID, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the to institution.
         *
         * @return void
         */
        public void getToInstitution() {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TO_INSTITUTION, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the transfer object.
         *
         * @return Transfer object
         */
        public TransferObject getTransferObject() {
            if (m_transferObject == null) {
                String transferObjectOid = (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_TNR_OID,
                        m_helper.getDictionary(DDX_ID));
                m_transferObject = m_helper.findTransferObject(transferObjectOid);
            }
            return m_transferObject;
        }

        /**
         * Gets the transfer type.
         *
         * @return String
         */
        public String getTransferType() {
            return (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_TRANSFER_TYPE,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            int result = 17;
            result = 31 * result + getOid().hashCode();
            return result;
        }

        /**
         * Persist.
         */
        public void persist() {
            m_helper.m_broker.saveBeanForced(m_studentTransfer, m_helper.getDictionary(DDX_ID));

            for (TransferObjectRecord record : getRecords()) {
                record.assignStudentTransfer(getOid());
                record.persist();
            }
        }

        /**
         * Sets the creation date.
         *
         * @param date void
         */
        public void setCreationDate(String date) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_CREATION_DATE, date, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the description.
         *
         * @param description void
         */
        public void setDescription(String description) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_DESCRIPTION, description,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the from institution.
         *
         * @param fromInstitution void
         */
        public void setFromInstitution(String fromInstitution) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_FROM_INSTITUTION, fromInstitution,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the merge result.
         *
         * @param mergeResult void
         */
        public void setMergeResult(String mergeResult) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_MERGE_RESULT, mergeResult,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the records type.
         *
         * @param recordsType void
         */
        public void setRecordsType(String recordsType) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_RECORDS_TYPE, recordsType,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the status.
         *
         * @param status void
         */
        public void setStatus(String status) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_STATUS, status, m_helper.getDictionary(DDX_ID));
            if (!StudentTransferObject.STATUS_ERROR.equals(status)) {
                for (TransferObjectRecord record : getRecords()) {
                    record.setStatus(status);
                }
            }
        }

        /**
         * Sets the to institution.
         *
         * @param toInstitution void
         */
        public void setToInstitution(String toInstitution) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TO_INSTITUTION, toInstitution,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the transfer message.
         *
         * @param transferMessage void
         */
        public void setTransferMessage(String transferMessage) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TRANSFER_MESSAGE, transferMessage,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the transfer type.
         *
         * @param transferType void
         */
        public void setTransferType(String transferType) {
            m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TRANSFER_TYPE, transferType,
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Assign transfer object.
         *
         * @param transferObjectOid String
         */
        private void assignTransferObject(String transferObjectOid) {
            String currentTransferObjectOid =
                    (String) m_studentTransfer.getFieldValueByAlias(ALIAS_TNR_STD_TNR_OID,
                            m_helper.getDictionary(DDX_ID));

            if (StringUtils.isEmpty(currentTransferObjectOid)) {
                m_studentTransfer.setFieldValueByAlias(ALIAS_TNR_STD_TNR_OID, transferObjectOid,
                        m_helper.getDictionary(DDX_ID));
            }
        }
    }

    /**
     * The Class FASTERTransferObject.
     */
    public static class TransferObject {
        public static final String ALIAS_TNR_CREATION_DATE = "tnr-creation-date";
        public static final String ALIAS_TNR_DDX_OID = "tnr-ddx-oid";
        public static final String ALIAS_TNR_DESCRIPTION = "tnr-description";
        public static final String ALIAS_TNR_RECORDS_TYPE = "tnr-records-type";
        public static final String ALIAS_TNR_RESULT_OID = "tnr-result-oid";
        public static final String ALIAS_TNR_STATUS = "tnr-status";
        public static final String ALIAS_TNR_TYPE = "tnr-type";

        public static final String DDX_ID = "FL-FASTER-TNR";

        public static final String RECORDS_TYPE_INTERDISTRICT = "I";
        public static final String RECORDS_TYPE_SECONDARY = "S";

        public static final String TRANSFER_TYPE_RESPONSE = "Response";
        public static final String TRANSFER_TYPE_REQUEST = "Request";


        private static final String DDX_OID = "ddxFlFstTnr";

        private TransferObjectHelper m_helper;
        private ExportFormatResult m_result;
        private List<StudentTransferObject> m_studentTransfers;
        private UserDefinedTableA m_transfer;

        /**
         * Instantiates a new transfer object.
         *
         * @param helper FASTERTransferHelper
         * @param transfer UserDefinedTableA
         */
        public TransferObject(TransferObjectHelper helper, UserDefinedTableA transfer) {
            m_helper = helper;
            m_transfer = transfer;
        }

        /**
         * Instantiates a new FASTER transfer object.
         *
         * @param helper FASTERTransferHelper
         */
        public TransferObject(TransferObjectHelper helper) {
            m_helper = helper;
            m_transfer = new UserDefinedTableA(m_helper.getBroker().getPersistenceKey());

            DataDictionary dictionary = m_helper.getDictionary(DDX_ID);
            m_transfer.setFieldValueByAlias(ALIAS_TNR_CREATION_DATE, helper.getCurrentDate(), dictionary);
            m_transfer.setFieldValueByAlias(ALIAS_TNR_DDX_OID, DDX_OID, dictionary);
            m_transfer.setOrganization1Oid("*dst");
        }

        /**
         * Adds the student transfer.
         *
         * @param studentTransfer StudentTransferObject
         */
        public void addStudentTransfer(StudentTransferObject studentTransfer) {
            getStudentTransfers().add(studentTransfer);
        }

        /**
         * Assign result.
         *
         * @param resultOid String
         */
        public void assignResult(String resultOid) {
            m_transfer.setFieldValueByAlias(ALIAS_TNR_RESULT_OID, resultOid, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Equals.
         *
         * @param o Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof TransferObject)) {
                return false;
            }
            TransferObject transferObject = (TransferObject) o;
            return transferObject.equals(o);
        }

        /**
         * Gets the oid.
         *
         * @return String
         */
        public String getOid() {
            return m_transfer.getOid();
        }

        /**
         * Gets the creation date.
         *
         * @return String
         */
        public String getCreationDate() {
            return (String) m_transfer.getFieldValueByAlias(ALIAS_TNR_CREATION_DATE, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the description.
         *
         * @return String
         */
        public String getDescription() {
            return (String) m_transfer.getFieldValueByAlias(ALIAS_TNR_DESCRIPTION, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the field value by alias.
         *
         * @param alias String
         * @return String
         */
        public String getFieldValueByAlias(String alias) {
            return (String) m_transfer.getFieldValueByAlias(alias, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the result.
         *
         * @return Export format result
         */
        public ExportFormatResult getResult() {
            if (m_result == null) {
                String resultOid =
                        (String) m_transfer.getFieldValueByAlias(ALIAS_TNR_RESULT_OID, m_helper.getDictionary(DDX_ID));
                m_result = (ExportFormatResult) m_helper.getBroker().getBeanByOid(ExportFormatResult.class, resultOid);
            }
            return m_result;
        }

        /**
         * Gets the records type.
         *
         * @return String
         */
        public String getRecordsType() {
            return (String) m_transfer.getFieldValueByAlias(ALIAS_TNR_RECORDS_TYPE, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the student transfers.
         *
         * @return Collection
         */
        public List<StudentTransferObject> getStudentTransfers() {
            if (m_studentTransfers == null) {
                m_studentTransfers = new ArrayList<>();
                DataDictionary tnrStdDictionary = m_helper.getDictionary(StudentTransferObject.DDX_ID);
                DataDictionaryField udaOidField =
                        tnrStdDictionary.findDataDictionaryFieldByAlias(StudentTransferObject.ALIAS_TNR_STD_TNR_OID);
                X2Criteria tnrStdCriteria = new X2Criteria();
                tnrStdCriteria.addEqualTo(udaOidField.getJavaName(), m_transfer.getOid());
                QueryByCriteria recQuery = new QueryByCriteria(UserDefinedTableC.class, tnrStdCriteria);
                List<UserDefinedTableC> studentTransfers =
                        (List<UserDefinedTableC>) m_helper.getBroker().getCollectionByQuery(recQuery);

                if (!studentTransfers.isEmpty()) {
                    for (UserDefinedTableC udc : studentTransfers) {
                        StudentTransferObject studentTransfer = new StudentTransferObject(m_helper, udc);
                        m_studentTransfers.add(studentTransfer);
                    }
                }
            }
            return m_studentTransfers;
        }

        /**
         * Gets the transfer.
         *
         * @return User defined table A
         */
        public UserDefinedTableA getTransfer() {
            return m_transfer;
        }

        /**
         * Gets the transfer type.
         *
         * @return String
         */
        public String getTransferType() {
            return (String) m_transfer.getFieldValueByAlias(ALIAS_TNR_TYPE, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            int result = 17;
            result = 31 * result + getOid().hashCode();
            return result;
        }

        /**
         * Persist.
         */
        public void persist() {
            m_helper.getBroker().beginTransaction();
            try {
                m_helper.getBroker().saveBeanForced(getTransfer(), m_helper.getDictionary(DDX_ID));
                for (StudentTransferObject studentTransfer : getStudentTransfers()) {
                    studentTransfer.assignTransferObject(getOid());
                    studentTransfer.persist();
                }
            } catch (Exception e) {
                m_helper.getBroker().rollbackTransaction();
                throw new X2RuntimeException();
            }
            m_helper.getBroker().commitTransaction();
        }

        /**
         * Sets the description.
         *
         * @param description void
         */
        public void setDescription(String description) {
            DataDictionary dictionary = m_helper.getDictionary(DDX_ID);
            m_transfer.setFieldValueByAlias(ALIAS_TNR_DESCRIPTION, description, dictionary);
        }

        /**
         * Sets the records type.
         *
         * @param recordsType void
         */
        public void setRecordsType(String recordsType) {
            m_transfer.setFieldValueByAlias(ALIAS_TNR_RECORDS_TYPE, recordsType, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the status.
         *
         * @param status void
         */
        public void setStatus(String status) {
            m_transfer.setFieldValueByAlias(ALIAS_TNR_STATUS, status, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the transfer type.
         *
         * @param transferType void
         */
        public void setTransferType(String transferType) {
            m_transfer.setFieldValueByAlias(ALIAS_TNR_TYPE, transferType, m_helper.getDictionary(DDX_ID));
        }
    }

    /**
     * The Class TransferObjectRecord.
     */
    public static class TransferObjectRecord {
        public static final String ALIAS_REC_DDX_OID = "tnr-rec-ddx-oid";
        public static final String ALIAS_REC_ORDER = "tnr-rec-order";
        public static final String ALIAS_REC_RECORD = "tnr-rec-record";
        public static final String ALIAS_REC_ROW_OID = "tnr-rec-row-oid";
        public static final String ALIAS_REC_STATUS = "tnr-rec-status";
        public static final String ALIAS_REC_TNR_STD_OID = "tnr-rec-tnr-std-oid";
        public static final String ALIAS_REC_TNR_OID = "tnr-rec-tnr-oid";

        public static final String DDX_ID = "FL-FASTER-TNR-REC";

        public static final String RECORD_TYPE_00 = "00";
        public static final String RECORD_TYPE_01 = "01";
        public static final String RECORD_TYPE_02 = "02";
        public static final String RECORD_TYPE_03 = "03";
        public static final String RECORD_TYPE_04 = "04";
        public static final String RECORD_TYPE_05 = "05";
        public static final String RECORD_TYPE_06 = "06";
        public static final String RECORD_TYPE_07 = "07";
        public static final String RECORD_TYPE_08 = "08";
        public static final String RECORD_TYPE_09 = "09";
        public static final String RECORD_TYPE_10 = "10";
        public static final String RECORD_TYPE_11 = "11";
        public static final String RECORD_TYPE_99 = "99";
        public static final String RECORD_TYPE_99ATV = "99ATV";
        public static final String RECORD_TYPE_99HS = "99HS";
        public static final String RECORD_TYPE_99HC = "99HC";
        public static final String RECORD_TYPE_99IMM = "99IMM";

        private static final String DDX_OID = "ddxFlFstTnrRec";

        private TransferObjectHelper m_helper;
        private UserDefinedTableB m_record;
        private ExportFormatRow m_exportFormatRow;
        private TransferObject m_transferObject;

        /**
         * Instantiates a new transfer object record.
         *
         * @param helper FASTERTransferHelper
         * @param record UserDefinedTableB
         */
        public TransferObjectRecord(TransferObjectHelper helper, UserDefinedTableB record) {
            m_helper = helper;
            m_record = record;
        }

        /**
         * Instantiates a new transfer object record.
         *
         * @param helper FASTERTransferHelper
         * @param record String
         */
        public TransferObjectRecord(TransferObjectHelper helper, String record) {
            if (record.length() != 1020) {
                throw new X2RuntimeException();
            }
            m_helper = helper;
            m_record = new UserDefinedTableB(m_helper.getBroker().getPersistenceKey());
            m_record.setFieldValueByAlias(ALIAS_REC_RECORD, record, m_helper.getDictionary(DDX_ID));
            m_record.setFieldValueByAlias(ALIAS_REC_DDX_OID, DDX_OID, m_helper.getDictionary(DDX_ID));
            m_record.setOrganization1Oid("*dst");
        }

        /**
         * Assign result row.
         *
         * @param row ExportFormatRow
         */
        public void assignResultRow(ExportFormatRow row) {
            m_record.setFieldValueByAlias(ALIAS_REC_ROW_OID, row.getOid(), m_helper.getDictionary(DDX_ID));
            m_record.setFieldValueByAlias(ALIAS_REC_ORDER, StringUtils.padLeft(row.getSortOrder().toString(), 10, '0'),
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the oid.
         *
         * @return String
         */
        public String getOid() {
            return m_record.getOid();
        }

        /**
         * Gets the plain row.
         *
         * @return String
         */
        public String getPlainRow() {
            return (String) m_record.getFieldValueByAlias(ALIAS_REC_RECORD, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the export format row.
         *
         * @return Export format row
         */
        public ExportFormatRow getExportFormatRow() {
            if (m_exportFormatRow == null) {
                m_exportFormatRow =
                        (ExportFormatRow) m_helper.getBroker().getBeanByOid(ExportFormatRow.class,
                                getExportFormatRowOid());
            }
            return m_exportFormatRow;
        }

        /**
         * Gets the row oid.
         *
         * @return String
         */
        public String getExportFormatRowOid() {
            return (String) m_record.getFieldValueByAlias(ALIAS_REC_ROW_OID, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Gets the record sort order.
         *
         * @return Integer
         */
        public Integer getRecordSortOrder() {
            return Integer
                    .valueOf((String) m_record.getFieldValueByAlias(ALIAS_REC_ORDER, m_helper.getDictionary(DDX_ID)));
        }

        /**
         * Gets the record type.
         *
         * @return String
         */
        public String getRecordType() {
            return TransferObjectHelper.getRecordType(getPlainRow());
        }

        /**
         * Gets the student transfer.
         *
         * @return Student transfer object
         */
        public StudentTransferObject getStudentTransfer() {
            String studentTransferOid =
                    (String) m_record.getFieldValueByAlias(ALIAS_REC_TNR_STD_OID, m_helper.getDictionary(DDX_ID));
            StudentTransferObject sto = null;
            if (!StringUtils.isEmpty(studentTransferOid)) {
                sto = m_helper.findStudentTransfer(studentTransferOid);
            }
            return sto;
        }

        /**
         * Gets the transfer object.
         *
         * @return Transfer object
         */
        public TransferObject getTransferObject() {
            if (m_transferObject == null) {
                m_transferObject = m_helper.findTransferObject(
                        (String) m_record.getFieldValueByAlias(ALIAS_REC_TNR_OID, m_helper.getDictionary(DDX_ID)));
            }
            return m_transferObject;
        }

        /**
         * Persist.
         */
        public void persist() {
            m_helper.m_broker.saveBeanForced(m_record, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the order number.
         *
         * @param number void
         */
        public void setOrderNumber(int number) {
            m_record.setFieldValueByAlias(ALIAS_REC_ORDER, StringUtils.padLeft(String.valueOf(number), 10, '0'),
                    m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the plain row.
         *
         * @param plainRow void
         */
        public void setPlainRow(String plainRow) {
            m_record.setFieldValueByAlias(ALIAS_REC_RECORD, plainRow, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Sets the plain row field value.
         *
         * @param fieldName String
         * @param value String
         * @param exportConfig FLFasterExportConfiguration
         */
        public void setPlainRowFieldValue(String fieldName, String value, FLFasterExportConfiguration exportConfig) {
            String recordType = getRecordType();
            int startPosition = exportConfig.getFieldStartPosition(recordType, fieldName);
            int fieldLength = exportConfig.getFieldLength(recordType, fieldName);
            String plainRow = getPlainRow();
            char[] plainRowArray = plainRow.toCharArray();
            char[] newValueArray = value.toCharArray();
            for (int i = startPosition; i < startPosition + fieldLength; i++) {
                Character charToInsert = null;
                if (i - startPosition >= newValueArray.length) {
                    charToInsert = Character.valueOf(' ');
                }
                if (charToInsert == null) {
                    charToInsert = Character.valueOf(newValueArray[i - startPosition]);
                }
                plainRowArray[i] = charToInsert.charValue();
            }
            setPlainRow(new String(plainRowArray));
        }

        /**
         * Sets the status.
         *
         * @param status void
         */
        public void setStatus(String status) {
            m_record.setFieldValueByAlias(ALIAS_REC_STATUS, status, m_helper.getDictionary(DDX_ID));
        }

        /**
         * Assign student transfer.
         *
         * @param studentTransferOid String
         */
        private void assignStudentTransfer(String studentTransferOid) {
            StudentTransferObject studentTranfer = getStudentTransfer();
            if (studentTranfer == null) {
                m_record.setFieldValueByAlias(ALIAS_REC_TNR_STD_OID, studentTransferOid,
                        m_helper.getDictionary(DDX_ID));
                m_record.setFieldValueByAlias(ALIAS_REC_TNR_OID, getStudentTransfer().getTransferObject().getOid(),
                        m_helper.getDictionary(DDX_ID));
                if (getRecordType().equals(RECORD_TYPE_00)) {
                    studentTranfer = getStudentTransfer();
                    String messageType = getPlainRow().substring(35, 38);
                    studentTranfer.setTransferMessage(
                            getTransferMessageByMessageType(messageType, m_helper.getBroker()));
                    studentTranfer.persist();
                }
            }
        }
    }

    /**
     * The Class FieldValueInfo.
     */
    public static class FieldInfo {
        private DataDictionary m_dictionary;
        private FieldBeanInfo m_fieldBeanInfo;
        private FieldPlainRowInfo m_fieldPlainRowInfo;
        private FieldExportFormatInfo m_fieldExportFormatInfo;

        /**
         * Instantiates a new field value info.
         *
         * @param fieldPlainRowInfo FieldPlainRowInfo
         * @param fieldBeanInfo FieldBeanInfo
         * @param fieldExportFormatInfo FieldExportFormatInfo
         * @param dictionary
         */
        public FieldInfo(FieldPlainRowInfo fieldPlainRowInfo, FieldBeanInfo fieldBeanInfo,
                FieldExportFormatInfo fieldExportFormatInfo, DataDictionary dictionary) {
            m_fieldBeanInfo = fieldBeanInfo;
            m_fieldPlainRowInfo = fieldPlainRowInfo;
            m_fieldExportFormatInfo = fieldExportFormatInfo;
            m_dictionary = dictionary;
        }

        /**
         * Instantiates a new field info.
         *
         * @param exportConfig FLFasterExportConfiguration
         * @param plainRow String
         * @param fieldName String
         * @param dictionary
         */
        public FieldInfo(FLFasterExportConfiguration exportConfig, String plainRow, String fieldName,
                DataDictionary dictionary) {
            m_fieldPlainRowInfo = new FieldPlainRowInfo(exportConfig, plainRow, fieldName);
            m_fieldExportFormatInfo = new FieldExportFormatInfo(exportConfig, getRecordType(plainRow), fieldName);
            Class<?> baseClass = m_fieldExportFormatInfo.getBaseClass();
            String fieldPath = m_fieldExportFormatInfo.getFieldPath();
            m_fieldBeanInfo = new FieldBeanInfo(baseClass, fieldPath, dictionary);
            m_dictionary = dictionary;
        }

        /**
         * Instantiates a new field info.
         *
         * @param exportConfig FLFasterExportConfiguration
         * @param plainRow String
         * @param fieldName String
         * @param clazz Class<?>
         * @param dictionary
         */
        public FieldInfo(FLFasterExportConfiguration exportConfig, String plainRow, String fieldName, Class<?> clazz,
                DataDictionary dictionary) {
            m_fieldPlainRowInfo = new FieldPlainRowInfo(exportConfig, plainRow, fieldName);
            m_fieldExportFormatInfo = new FieldExportFormatInfo(exportConfig, getRecordType(plainRow), fieldName);
            m_fieldBeanInfo = new FieldBeanInfo(clazz, m_fieldExportFormatInfo.getFieldPath(), dictionary);
            m_dictionary = dictionary;
        }

        /**
         * Instantiates a new field info.
         *
         * @param exportConfig FLFasterExportConfiguration
         * @param plainRow String
         * @param fieldName String
         * @param clazz Class<?>
         * @param beanPath String
         * @param dictionary
         */
        public FieldInfo(FLFasterExportConfiguration exportConfig, String plainRow, String fieldName, Class<?> clazz,
                String beanPath, DataDictionary dictionary) {
            m_fieldPlainRowInfo = new FieldPlainRowInfo(exportConfig, plainRow, fieldName);
            m_fieldExportFormatInfo = new FieldExportFormatInfo(exportConfig, getRecordType(plainRow), fieldName);
            String beanPathToUse = null;
            String formatFieldPath = m_fieldExportFormatInfo.getFieldPath();
            if (formatFieldPath != null) {
                beanPathToUse = formatFieldPath;
            } else {
                beanPathToUse = beanPath;
            }
            m_fieldBeanInfo = new FieldBeanInfo(clazz, beanPathToUse, dictionary);
            m_dictionary = dictionary;
        }

        /**
         * Sets the field bean info.
         *
         * @param fieldBeanInfo void
         */
        public void setFieldBeanInfo(FieldBeanInfo fieldBeanInfo) {
            m_fieldBeanInfo = fieldBeanInfo;
        }

        /**
         * Gets the base class.
         *
         * @return Class
         */
        public Class getBaseClass() {
            if (m_fieldBeanInfo == null) {
                throw new X2RuntimeException();
            }
            return m_fieldBeanInfo.getBaseClass();
        }

        /**
         * Gets the bean path.
         *
         * @return String
         */
        public String getBeanPath() {
            if (m_fieldBeanInfo == null) {
                throw new X2RuntimeException();
            }
            return m_fieldBeanInfo.getBeanPath();
        }

        /**
         * Gets the criteria.
         *
         * @return X 2 criteria
         */
        public X2Criteria getCriteria() {
            if (m_fieldBeanInfo == null) {
                throw new X2RuntimeException();
            }
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(getBeanPath(), getValue());
            return criteria;
        }

        public DataDictionary getDictionary() {
            return m_dictionary;
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_fieldExportFormatInfo.getFieldName();
        }

        /**
         * Gets the value.
         *
         * @return String
         */
        public String getValue() {
            return m_fieldPlainRowInfo.getValue();
        }

        /**
         * Gets the query.
         *
         * @return Query by criteria
         */
        public QueryByCriteria getQuery() {
            if (m_fieldBeanInfo == null) {
                throw new X2RuntimeException();
            }
            QueryByCriteria query = new QueryByCriteria(getBaseClass(), getCriteria());
            return query;
        }

        /**
         * Gets the field bean info.
         *
         * @return Field bean info
         */
        public FieldBeanInfo getFieldBeanInfo() {
            return m_fieldBeanInfo;
        }

        /**
         * To string.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getFieldName() + ": " + getValue();
        }
    }

    /**
     * The Class FieldBean.
     */
    public static class FieldBeanInfo {
        private String m_beanPath;
        private Class m_class;

        /**
         * Instantiates a new field bean.
         *
         * @param clazz Class<?>
         * @param beanPath String
         * @param dictionary
         */
        public FieldBeanInfo(Class<?> clazz, String beanPath, DataDictionary dictionary) {
            if (!StringUtils.isEmpty(beanPath)) {
                beanPath = beanPath.replaceAll("\\[|\\]", "");
                String id = DictionaryHelper.translateAlias(beanPath, dictionary, true);
                ModelProperty property = new ModelProperty(clazz, id, dictionary);
                m_beanPath = property.getBeanPath();
            }

            m_class = clazz;
        }

        /**
         * Gets the base class.
         *
         * @return Class
         */
        public Class<?> getBaseClass() {
            return m_class;
        }

        /**
         * Gets the bean path.
         *
         * @return String
         */
        public String getBeanPath() {
            return m_beanPath;
        }
    }

    /**
     * The Class FieldExportFormatInfo.
     */
    public static class FieldExportFormatInfo {
        private ExportFormatField m_exportFormatField;

        /**
         * Instantiates a new field export format info.
         *
         * @param exportConfig FLFasterExportConfiguration
         * @param recordType String
         * @param fieldName String
         */
        public FieldExportFormatInfo(FLFasterExportConfiguration exportConfig, String recordType, String fieldName) {
            List<ExportFormatField> exportFormatFields = exportConfig.getFieldDefinitionsByRecordType(recordType);
            for (ExportFormatField exportFormatField : exportFormatFields) {
                if (exportFormatField.getName().equals(fieldName)) {
                    m_exportFormatField = exportFormatField;
                }
            }
        }

        /**
         * Gets the field path.
         *
         * @return String
         */
        public String getFieldPath() {
            return m_exportFormatField.getFieldPath();
        }

        /**
         * Gets the field name.
         *
         * @return String
         */
        public String getFieldName() {
            return m_exportFormatField.getName();
        }

        /**
         * Gets the base class.
         *
         * @return Class
         */
        public Class getBaseClass() {
            return m_exportFormatField.getDefinition().getSourceTable().getDataTable().getDataClass();
        }
    }

    /**
     * The Class FieldPlainRow.
     */
    public static class FieldPlainRowInfo {
        private int m_endPosition = -1;
        private String m_recordType = null;
        private int m_startPosition = -1;
        private String m_value = null;

        /**
         * Instantiates a new field plain row.
         *
         * @param exportConfig FLFasterExportConfiguration
         * @param plainRow String
         * @param fieldName String
         */
        public FieldPlainRowInfo(FLFasterExportConfiguration exportConfig, String plainRow, String fieldName) {
            m_recordType = TransferObjectHelper.getRecordType(plainRow);
            m_startPosition = exportConfig.getFieldStartPosition(m_recordType, fieldName);
            int fieldLength = exportConfig.getFieldLength(m_recordType, fieldName);
            m_endPosition = m_startPosition + fieldLength;
            m_value = plainRow.substring(m_startPosition, m_endPosition);
        }

        /**
         * Instantiates a new field value info.
         *
         * @param startPosition int
         * @param endPosition int
         * @param value String
         */
        public FieldPlainRowInfo(int startPosition, int endPosition, String value) {
            m_startPosition = startPosition;
            m_endPosition = endPosition;
            m_value = value;
        }

        /**
         * Gets the end position.
         *
         * @return int
         */
        public int getEndPosition() {
            return m_endPosition;
        }

        /**
         * Gets the record type.
         *
         * @return String
         */
        public String getRecordType() {
            return m_recordType;
        }

        /**
         * Gets the start position.
         *
         * @return int
         */
        public int getStartPosition() {
            return m_startPosition;
        }

        /**
         * Gets the value.
         *
         * @return String
         */
        public String getValue() {
            return m_value;
        }
    }

    /**
     * The Class ValuesToIdentify.
     */
    private class ValuesToIdentifyPlainRow {
        Collection<FieldPlainRowInfo> m_valuesToIdentify = null;

        /**
         * Instantiates a new values to identify.
         *
         * @param fieldValues FieldValueInfo[]
         */
        public ValuesToIdentifyPlainRow(FieldPlainRowInfo... fieldValues) {
            m_valuesToIdentify = Arrays.asList(fieldValues);
        }

        /**
         * Checks if is identified.
         *
         * @param plainRow String
         * @return true, if is identified
         */
        public boolean isIdentified(String plainRow) {
            boolean isIdentified = true;
            for (FieldPlainRowInfo fieldValueInfo : m_valuesToIdentify) {
                String valueFromRow =
                        plainRow.substring(fieldValueInfo.getStartPosition(), fieldValueInfo.getEndPosition());
                if (!valueFromRow.equals(fieldValueInfo.getValue())) {
                    isIdentified = false;
                }
            }
            return isIdentified;
        }
    }

    private static final String ALIAS_REF_CODE_MESSAGE = "fst-msg-types-message";

    private static final String REF_TABLE_NAME_REQUEST_RESPONSE_CODES = "FL FASTER Request/Response Codes";
    private static final String REF_TABLE_NAME_REQUEST_RESPONSE_CODES_MSIX = "FL FASTER Response MSIX Codes";

    private X2Broker m_broker;

    private Map<String, DataDictionary> m_ddxIdDictionaries = new HashMap<>();

    private SimpleDateFormat m_timestampDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

    private Map<String, StudentTransferObject> m_studentTransfersByOid = new HashMap<>();

    private Map<String, TransferObject> m_transferObjectsByOid = new HashMap<>();

    /**
     * Instantiates a new FASTER helper.
     *
     * @param broker X2Broker
     */
    public TransferObjectHelper(X2Broker broker) {
        m_broker = broker;
    }

    /**
     * Find student transfer object.
     *
     * @param oid String
     * @return TransferObject
     */
    public StudentTransferObject findStudentTransfer(String oid) {
        StudentTransferObject studentTransfer = m_studentTransfersByOid.get(oid);
        if (studentTransfer == null) {
            UserDefinedTableC studentTransferRecord =
                    (UserDefinedTableC) m_broker.getBeanByOid(UserDefinedTableC.class, oid);
            if (studentTransferRecord != null) {
                studentTransfer = new StudentTransferObject(this, studentTransferRecord);
                m_studentTransfersByOid.put(oid, studentTransfer);
            }
        }
        return studentTransfer;
    }

    /**
     * Find transfer object.
     *
     * @param oid String
     * @return TransferObject
     */
    public TransferObject findTransferObject(String oid) {
        TransferObject transferObject = m_transferObjectsByOid.get(oid);
        if (transferObject == null) {
            UserDefinedTableA transfer = (UserDefinedTableA) m_broker.getBeanByOid(UserDefinedTableA.class, oid);
            if (transfer != null) {
                transferObject = new TransferObject(this, transfer);
                m_transferObjectsByOid.put(oid, transferObject);
            }
        }
        return transferObject;
    }

    /**
     * Find transfer object.
     *
     * @param transferType String
     * @param status String
     * @param identifiers ValuesToIdentify[]
     * @return StudentTransferObject
     */
    public StudentTransferObject findStudentTransfer(String transferType,
                                                     String status,
                                                     ValuesToIdentifyPlainRow... identifiers) {
        StudentTransferObject searchResult = null;

        X2Criteria studentTransferCriteria = new X2Criteria();
        String transferTypeField =
                StudentTransferObject.getJavaName(StudentTransferObject.ALIAS_TNR_STD_TRANSFER_TYPE, this);
        String studentTransferStatusField =
                StudentTransferObject.getJavaName(StudentTransferObject.ALIAS_TNR_STD_STATUS, this);
        String ddxOidField = StudentTransferObject.getJavaName(StudentTransferObject.ALIAS_TNR_STD_DDX_OID, this);

        studentTransferCriteria.addEqualTo(transferTypeField, transferType);
        studentTransferCriteria.addEqualTo(studentTransferStatusField, status);
        studentTransferCriteria.addEqualTo(ddxOidField, StudentTransferObject.DDX_OID);

        QueryByCriteria studentTransferQuery = new QueryByCriteria(UserDefinedTableC.class, studentTransferCriteria);

        Collection<UserDefinedTableC> studentTransferTable = getBroker().getCollectionByQuery(studentTransferQuery);
        Collection<StudentTransferObject> studentTransfers = new ArrayList<>();
        for (UserDefinedTableC currentStudentTransfer : studentTransferTable) {
            StudentTransferObject studentTransfer = new StudentTransferObject(this, currentStudentTransfer);
            studentTransfers.add(studentTransfer);
        }
        for (StudentTransferObject currentStudentTransfer : studentTransfers) {
            TransferObjectRecord headerObject = currentStudentTransfer.getRecords().iterator().next();
            String header = headerObject.getPlainRow();

            for (ValuesToIdentifyPlainRow identifier : identifiers) {
                if (identifier.isIdentified(header)) {
                    searchResult = currentStudentTransfer;
                }
            }
        }

        return searchResult;
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     */
    public X2Broker getBroker() {
        return m_broker;
    }

    /**
     * Gets the current date.
     *
     * @return String
     */
    public String getCurrentDate() {
        return m_timestampDateFormat.format(new Date());
    }

    /**
     * Returns a instance of a district data dictionary by ddx id.
     *
     * @param ddxId String
     * @return DataDictionary.
     */
    public DataDictionary getDictionary(String ddxId) {
        DataDictionary dictionary = m_ddxIdDictionaries.get(ddxId);
        if (dictionary == null) {
            X2Criteria ddxCriteria = new X2Criteria();
            ddxCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, ddxId);
            QueryByCriteria ddxQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) m_broker.getBeanByQuery(ddxQuery);
            dictionary = DataDictionary.getDistrictDictionary(ddx, m_broker.getPersistenceKey());
            m_ddxIdDictionaries.put(ddxId, dictionary);
        }
        return dictionary;
    }

    /**
     * Gets the field value.
     *
     * @param plainRow String
     * @param fieldName String
     * @param exportConfig FLFasterExportConfiguration
     * @return String
     */
    public String getFieldValue(String plainRow, String fieldName, FLFasterExportConfiguration exportConfig) {
        FieldPlainRowInfo fieldValueInfo = new FieldPlainRowInfo(exportConfig, plainRow, fieldName);
        return fieldValueInfo.getValue();
    }

    /**
     * Gets the transfer message by message type.
     *
     * @param messageType String
     * @param broker X2Broker
     * @return String
     */
    public static String getTransferMessageByMessageType(String messageType, X2Broker broker) {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(ReferenceCode.REL_REFERENCE_TABLE + ModelProperty.PATH_DELIMITER + ReferenceTable.COL_USER_NAME,
                Arrays.asList(REF_TABLE_NAME_REQUEST_RESPONSE_CODES, REF_TABLE_NAME_REQUEST_RESPONSE_CODES_MSIX));
        criteria.addEqualTo(ReferenceCode.COL_CODE, messageType);
        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        ReferenceCode code = ((ReferenceCode) broker.getBeanByQuery(query));
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(code.getExtendedDataDictionary(), broker.getPersistenceKey());
        return (String) code.getFieldValueByAlias(ALIAS_REF_CODE_MESSAGE, dictionary);
    }

    /**
     * Gets the plain row.
     *
     * @param row ExportFormatRow
     * @return String
     */
    public String getPlainRow(ExportFormatRow row) {
        return getTransferObjectRecord(row).getPlainRow();
    }

    /**
     * Gets the record type.
     *
     * @param plainRow String
     * @return String
     */
    public static String getRecordType(String plainRow) {
        String recordType = plainRow.substring(1, 3);
        if (TransferObjectRecord.RECORD_TYPE_99.equals(recordType)) {
            recordType = recordType + plainRow.substring(3, 6).trim();
            if (!FLFasterExportConfiguration.s_genericRecordTypes.contains(recordType)) {
                recordType = TransferObjectRecord.RECORD_TYPE_99;
            }
        }
        return recordType;
    }

    /**
     * Gets the related bean.
     *
     * @param infos FieldValueInfo[]
     * @return X 2 base bean
     */
    public X2BaseBean getRelatedBean(FieldInfo... infos) {
        Class clazzToSearch = null;
        for (FieldInfo info : infos) {
            if (clazzToSearch == null) {
                clazzToSearch = info.getBaseClass();
            }
            if (!clazzToSearch.equals(info.getBaseClass())) {
                throw new X2RuntimeException();
            }
        }
        X2Criteria criteria = new X2Criteria();
        for (FieldInfo info : infos) {
            X2Criteria andCriteria = new X2Criteria();
            andCriteria.addEqualTo(info.getBeanPath(), info.getValue());
            criteria.addAndCriteria(andCriteria);
        }
        QueryByCriteria query = new QueryByCriteria(clazzToSearch, criteria);
        return getBroker().getBeanByQuery(query);
    }

    /**
     * Gets the request to reply.
     *
     * @param response StudentTransferObject
     * @param exportConfig FLFasterExportConfiguration
     * @return Student transfer object
     */
    public StudentTransferObject getRequestToReply(StudentTransferObject response,
                                                   FLFasterExportConfiguration exportConfig) {
        String statusToSearch = null;

        if (response.getTransferType().equals(TransferObject.TRANSFER_TYPE_RESPONSE)) {
            if (StringUtils.isEmpty(response.getStatus())) {
                statusToSearch = StudentTransferObject.STATUS_SENT;

            } else if (response.getStatus().equals(StudentTransferObject.STATUS_NEW)) {
                statusToSearch = StudentTransferObject.STATUS_INCOMING;
            } else if (response.getStatus().equals(StudentTransferObject.STATUS_MERGED)) {
                statusToSearch = StudentTransferObject.STATUS_RECEIVED;
            } else {
                return null;
            }
        } else {
            return null;
        }

        TransferObjectRecord header = response.getRecords().iterator().next();

        String recordType = header.getPlainRow().substring(1, 3);

        int addressedStartPosition = exportConfig.getFieldStartPosition(recordType, FIELD_ADDRESSED_DST_NUM);
        int addressedDstLength = exportConfig.getFieldLength(recordType, FIELD_ADDRESSED_DST_NUM);
        int addressedSklLength = exportConfig.getFieldLength(recordType, FIELD_ADDRESSED_SKL_NUM);
        int addressedInstEndPosition = addressedStartPosition + addressedDstLength + addressedSklLength;

        int sendingStartPosition = exportConfig.getFieldStartPosition(recordType, FIELD_SENDING_DST_NUM);
        int sendingDstLength = exportConfig.getFieldLength(recordType, FIELD_SENDING_DST_NUM);
        int sendingSklLength = exportConfig.getFieldLength(recordType, FIELD_SENDING_SKL_NUM);
        int sendingInstEndPosition = sendingStartPosition + sendingDstLength + sendingSklLength;

        String institutionValue = null;
        int institutionStartPosition = -1;
        int institutionEndPosition = -1;

        String plainRow = header.getPlainRow();

        if (StringUtils.isEmpty(response.getStatus())
                || response.getStatus().equals(StudentTransferObject.STATUS_MERGED)) {
            institutionValue = plainRow.substring(sendingStartPosition, sendingInstEndPosition);
            institutionStartPosition = addressedStartPosition;
            institutionEndPosition = addressedInstEndPosition;
        } else if (response.getStatus().equals(StudentTransferObject.STATUS_NEW)) {
            institutionValue = plainRow.substring(addressedStartPosition, addressedInstEndPosition);
            institutionStartPosition = sendingStartPosition;
            institutionEndPosition = sendingInstEndPosition;
        }

        FieldPlainRowInfo institution =
                new FieldPlainRowInfo(institutionStartPosition, institutionEndPosition, institutionValue);
        FieldPlainRowInfo studentId = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_STD_NUM_ID_FL);
        FieldPlainRowInfo appendage = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_APPENDAGE);
        FieldPlainRowInfo firstName = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_FIRST_NAME);
        FieldPlainRowInfo middleName = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_MIDDLE_NAME);
        FieldPlainRowInfo lastName = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_LAST_NAME);
        FieldPlainRowInfo gender = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_GENDER);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_ETHNICITY);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_RACE_AMERICAALASKAINDIAN);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_RACE_ASIAN);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_RACE_AFRICAN_BLACK);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_RACE_PACIFIC_ISLANDER);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_RACE_WHITE);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_RACIAL_ETHNIC_CAT);
        FieldPlainRowInfo dob = new FieldPlainRowInfo(exportConfig, plainRow, FIELD_DATE_OF_BIRTH);
        new FieldPlainRowInfo(exportConfig, plainRow, FIELD_HS_GRADUATION_DATE);

        ValuesToIdentifyPlainRow byStudentId = new ValuesToIdentifyPlainRow(institution, studentId);
        ValuesToIdentifyPlainRow byFullName =
                new ValuesToIdentifyPlainRow(institution, appendage, firstName, middleName, lastName);
        ValuesToIdentifyPlainRow byNameGenderDob =
                new ValuesToIdentifyPlainRow(institution, firstName, lastName, gender, dob);

        Collection<ValuesToIdentifyPlainRow> identifiers = Arrays.asList(byStudentId, byFullName, byNameGenderDob);

        StudentTransferObject request = findStudentTransfer(TransferObject.TRANSFER_TYPE_REQUEST, statusToSearch,
                (ValuesToIdentifyPlainRow[]) identifiers.toArray());

        return request;
    }

    /**
     * Gets the transfer object record.
     *
     * @param row ExportFormatRow
     * @return Transfer object record
     */
    public TransferObjectRecord getTransferObjectRecord(ExportFormatRow row) {
        TransferObjectRecord tor = null;
        for (Entry<String, TransferObject> entry : m_transferObjectsByOid.entrySet()) {
            Collection<StudentTransferObject> studentTransferObjects = entry.getValue().getStudentTransfers();
            tor = getTransferObjectRecord(row, studentTransferObjects);
        }
        if (tor == null) {
            X2Criteria toCriteria = new X2Criteria();
            DataDictionary dictionary = getDictionary(TransferObject.DDX_ID);
            DataDictionaryField fieldResultOid =
                    dictionary.findDataDictionaryFieldByAlias(TransferObject.ALIAS_TNR_RESULT_OID);
            toCriteria.addEqualTo(fieldResultOid.getJavaName(), row.getResultOid());
            QueryByCriteria toQuery = new QueryByCriteria(UserDefinedTableA.class, toCriteria);
            UserDefinedTableA uda = (UserDefinedTableA) getBroker().getBeanByQuery(toQuery);
            TransferObject to = findTransferObject(uda.getOid());
            Collection<StudentTransferObject> studentTransferObjects = to.getStudentTransfers();
            tor = getTransferObjectRecord(row, studentTransferObjects);
        }

        if (!tor.getExportFormatRowOid().equals(row.getOid())) {
            throw new X2RuntimeException();
        }
        return tor;
    }

    /**
     * Gets the transfer object record.
     *
     * @param row ExportFormatRow
     * @param studentTransferObjects List<StudentTransferObject>
     * @return Transfer object record
     */
    private TransferObjectRecord getTransferObjectRecord(ExportFormatRow row,
                                                         Collection<StudentTransferObject> studentTransferObjects) {
        for (StudentTransferObject sto : studentTransferObjects) {
            for (TransferObjectRecord record : sto.getRecords()) {
                if (record.getRecordSortOrder().equals(Integer.valueOf(row.getSortOrder().intValue()))
                        && row.getOid().equals(record.getExportFormatRowOid())) {
                    return record;
                }
            }
        }
        return null;
    }
}
