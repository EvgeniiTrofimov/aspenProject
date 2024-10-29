/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.md;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.PersonAddress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.sis.model.beans.SisAddress;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.expression.ExpressionUtils;
import com.x2dev.utils.expression.InvalidExpressionException;
import com.x2dev.utils.expression.InvalidTokenException;
import com.x2dev.utils.expression.Token;
import com.x2dev.utils.types.PlainDate;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.StringUtils;

/**
 * The Class StudentAddressImport.
 *
 * @author Follett Software Company
 * @copyright 2021
 */
public class StudentAddressImport extends TextImportJavaSource {

    private static final String COLUMN_STD_ADRS_VIEW = "ADRS_VIEW";

    private static final String EXPRESSION_ADDRESS_VIEW = "{addressLine01} {addressLine02}";

    private static final int INDEX_DATE_BEGIN = 2;
    private static final int INDEX_DATE_END = 3;
    private static final int INDEX_STD_ADRS_VIEW = 4;
    private static final int INDEX_STD_OID = 0;

    private static final String INPUT_LOG_ADDRESS_CREATES = "logAddressCreates";
    private static final String INPUT_LOG_ADDRESS_UPDATES = "logAddressUpdates";
    private static final String INPUT_PREVIEW_ONLY = "previewOnly";

    private static final String PAD_ADDRESS_TYPE_PHYSICAL = "Physical";
    private static final String PAD_JOIN_TYPE_ADDRESS = "Address";

    private List<String> m_addressesLog = new ArrayList<>();
    private Map<String, String> m_addressKeyToOidMap = new HashMap<>();
    private List<Token> m_addressViewTokens = null;
    private ModelBroker m_broker;
    private final SimpleDateFormat m_dateFormatter = new SimpleDateFormat("yyyyMMdd_hhmmss");
    private boolean m_logAddressCreates;
    private boolean m_logAddressUpdates;
    private boolean m_previewOnly;
    private List<String> m_validationErrors = new ArrayList<>();

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = new ModelBroker(getPrivilegeSet());
        }
        return m_broker;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        m_logAddressCreates = ((Boolean) getParameter(INPUT_LOG_ADDRESS_CREATES)).booleanValue();
        m_logAddressUpdates = ((Boolean) getParameter(INPUT_LOG_ADDRESS_UPDATES)).booleanValue();
        m_previewOnly = ((Boolean) getParameter(INPUT_PREVIEW_ONLY)).booleanValue();

        createAddressMap();
        super.importData(sourceFile);
    }

    /**
     * Import resource records.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        String stdAdrsView = record.get(INDEX_STD_ADRS_VIEW).trim().toUpperCase();
        if (!COLUMN_STD_ADRS_VIEW.equals(stdAdrsView)) {
            if (!m_addressKeyToOidMap.containsKey(stdAdrsView)) {
                logInvalidRecord(lineNumber, stdAdrsView);
                incrementSkipCount();
            } else {
                SisStudent student = getBroker().getBeanByOid(SisStudent.class, record.get(INDEX_STD_OID));
                String begin = record.get(INDEX_DATE_BEGIN);
                String end = record.get(INDEX_DATE_END);
                registerPersonAddress(stdAdrsView, student,
                        new PlainDate(m_dateFormatter.parse(begin)),
                        StringUtils.isEmpty(end) ? null : new PlainDate(m_dateFormatter.parse(end)), lineNumber);
            }
        }
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 5;
    }

    /**
     * Gets the import statistics.
     *
     * @return String builder
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#getImportStatistics()
     */
    @Override
    protected StringBuilder getImportStatistics() {
        StringBuilder buffer = super.getImportStatistics();
        buffer.append("\n");
        if (!m_validationErrors.isEmpty()) {
            buffer.append("VALIDATION ERRORS\n\n");
            m_validationErrors.forEach(buffer::append);
        }
        if (!m_addressesLog.isEmpty()) {
            buffer.append("ADDRESS LOG\n\n");
            m_addressesLog.forEach(buffer::append);
        }
        return buffer;
    }

    /**
     * Creates the address map.
     *
     * @throws InvalidExpressionException exception
     * @throws InvalidTokenException exception
     */
    private void createAddressMap() {


        X2Criteria criteria = new X2Criteria();
        BeanQuery query = new BeanQuery(SisAddress.class, criteria);
        query.addOrderByAscending(X2BaseBean.COL_LAST_MODIFIED_TIME);
        Collection<SisAddress> allAddresses = getBroker().getCollectionByQuery(query);
        for (SisAddress address : allAddresses) {
            String addressView = getAddressView(address);
            m_addressKeyToOidMap.put(addressView, address.getOid());
        }
    }

    /**
     * Find person addresses.
     *
     * @param psnOid String
     * @return Collection
     */
    private Collection<PersonAddress> findPersonAddresses(String psnOid) {
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(PersonAddress.COL_PERSON_OID, psnOid);
        criteria.addEqualTo(PersonAddress.COL_ADDRESS_TYPE, PAD_ADDRESS_TYPE_PHYSICAL);
        BeanQuery query = new BeanQuery(PersonAddress.class, criteria);
        return getBroker().getCollectionByQuery(query);
    }

    private String getAddressView(Address address) {
        String value = null;
        if (m_addressViewTokens == null) {
            try {
                m_addressViewTokens = ExpressionUtils.parseExpression(EXPRESSION_ADDRESS_VIEW, getLocale());
            } catch (InvalidExpressionException e) {
                throw new X2RuntimeException(e);
            }
        }
        try {
            value = ExpressionUtils.evaluateExpression(address, m_addressViewTokens);
        } catch (InvalidTokenException e) {
            throw new X2RuntimeException(e);
        }
        return value.trim().toUpperCase();
    }

    /**
     * Log address create.
     *
     * @param stdAdrsView String
     * @param student SisStudent
     */
    private void logAddressCreate(String stdAdrsView, SisStudent student) {
        incrementInsertCount();
        if (m_logAddressCreates) {
            m_addressesLog.add(String.format("'%s' address created for student %s %s\n", stdAdrsView,
                    student.getLocalId(), student.getNameView()));
        }
    }

    /**
     * Log address update.
     *
     * @param stdAdrsView String
     * @param student SisStudent
     */
    private void logAddressUpdate(String stdAdrsView, SisStudent student) {
        incrementUpdateCount();
        if (m_logAddressUpdates) {
            m_addressesLog.add(String.format("'%s' address updated for student %s %s\n", stdAdrsView,
                    student.getLocalId(), student.getNameView()));
        }
    }

    /**
     * Register person address.
     *
     * @param stdAdrsView String
     * @param student SisStudent
     * @param begin Date
     * @param end Date
     * @param lineNumber
     */
    private void registerPersonAddress(String stdAdrsView,
                                       SisStudent student,
                                       PlainDate begin,
                                       PlainDate end,
                                       int lineNumber) {
        String adrOid = m_addressKeyToOidMap.get(stdAdrsView);
        String psnOid = student.getPersonOid();
        Collection<PersonAddress> personAddresses = findPersonAddresses(psnOid);
        /*
         * Since there may be more than one address id with the same view expression, we need to
         * match based on the expression.
         * We will only match addresses that either have the same start date or when both new
         * address and discovered address are null
         * This is necessary because students move away from and back to the same address
         */
        PersonAddress personAddress =
                personAddresses.stream().filter(
                        pa -> stdAdrsView.equals(getAddressView(pa.getAddress())) &&
                                (begin.equals(pa.getStartDate()) || (end == null && pa.getEndDate() == null)))
                        .findFirst().orElse(null);
        boolean save = true;
        if (personAddress == null) {
            if (end == null) {
                // skip end records that do not match existing
                incrementSkipCount();
                save = false;
            } else {
                personAddress = X2BaseBean.newInstance(PersonAddress.class, getBroker().getPersistenceKey());
                personAddress.setActiveIndicator(false);
                personAddress.setAddressType(PAD_ADDRESS_TYPE_PHYSICAL);
                personAddress.setJoinType(PAD_JOIN_TYPE_ADDRESS);
                personAddress.setAddressOid(adrOid);
                personAddress.setPersistSharing(false);
                personAddress.setPersonOid(psnOid);
                logAddressCreate(stdAdrsView, student);
            }
        } else if ((end == null && personAddress.getEndDate() == null)
                || (end != null && end.equals(personAddress.getEndDate()))) {
            incrementSkipCount();
            save = false;
        } else {
            logAddressUpdate(stdAdrsView, student);
        }
        if (save && !m_previewOnly) {
            personAddress.setStartDate(begin);
            if (end != null) {
                personAddress.setEndDate(end);
            }
            List<ValidationError> errors = getBroker().saveBean(personAddress);
            if (errors != null && !errors.isEmpty()) {
                errors.forEach(error -> m_addressesLog.add(String.format("Line: %d Error:  %s\n", lineNumber,
                        student.getLocalId(), student.getNameView())));
            }
        }
    }
}
