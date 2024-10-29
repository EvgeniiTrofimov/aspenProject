/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.on;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.procedures.statereporting.common.FilterableFactory;
import com.x2dev.procedures.statereporting.common.FilterableFactory.Filterable;
import com.x2dev.procedures.statereporting.common.FilterableFactory.MultiLevelMap.ValueByKeyResolver;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.UserDefinedTableA;
import com.x2dev.sis.model.beans.UserDefinedTableB;
import com.x2dev.sis.model.beans.UserDefinedTableC;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.struts.util.MessageResources;

/**
 * The Class SpecialEquipmentAmountPPA.
 */
public class SpecialEquipmentAmountPPA extends ReportJavaSourceNet {


    private static final long serialVersionUID = 1L;

    private static class Record {
        private Map<Field, Object> m_fieldValuePairs = new HashMap<Field, Object>();

        private enum Field {
            //
            boardName,
            //
            boardNumber,
            //
            comments,
            //
            educationDirectorName,
            //
            incurredBetweenMentionedTime,
            //
            inEquipmentCost,
            //
            item,
            //
            noOfItems,
            //
            notProvidedByOSAPAC,
            //
            numberOfEquipmentIn,
            //
            numberOfEquipmentOut,
            //
            outEquipmentCost,
            //
            price,
            //
            regionalOffice,
            //
            schoolNumber,
            //
            seaMaintenanceAndTraining,
            //
            seaPPAPurchases,
            //
            signatureDay,
            //
            signatureMonth,
            //
            signatureYear,
            //
            totalExpenditures;
        }

        private static List<String> s_uniqueFields =
                Arrays.asList(Field.item.toString(),
                        Field.price.toString(),
                        Field.noOfItems.toString(),
                        Field.comments.toString(),
                        Field.boardNumber.toString(),
                        Field.boardName.toString());

        private static ValueByKeyResolver<Record> s_valueResolver = new ValueByKeyResolver<Record>() {
            @Override
            public Object getValue(String key, Record entity) {
                return entity.get(Field.valueOf(key));
            }
        };

        /**
         * Gets the.
         *
         * @param key Field
         * @return Object
         */
        Object get(Field key) {
            return m_fieldValuePairs.get(key);
        }

        /**
         * Sets the.
         *
         * @param field Field
         * @param value Object
         */
        void set(Field field, Object value) {
            m_fieldValuePairs.put(field, value);
        }
    }

    // Inventory DDX aliases
    // Inventory item
    private static final String ALIAS_INVENTORY_ITEM_SERIAL = "udc-spedinv-serial-no";
    private static final String ALIAS_INVENTORY_ITEM_BAR_CODE = "udc-spedinv-bar-code";

    // Inventory master
    private static final String ALIAS_INVENTORY_MASTER_ACTUAL_COST = "uda-spedinv-actual-cost";
    private static final String ALIAS_INVENTORY_MASTER_ADDITIONAL_INFORMATION = "uda-additional-info";
    private static final String ALIAS_INVENTORY_MASTER_APPROVAL_INFORMATION = "uda-spedinv-approval-date";
    private static final String ALIAS_INVENTORY_MASTER_ASSUMPTION_COST = "uda-spedinv-assumption-cost";
    private static final String ALIAS_INVENTORY_MASTER_CONSUMABLE = "uda-spedinv-consumable";
    private static final String ALIAS_INVENTORY_MASTER_DELIVERY_DATE = "uda-spedinv-delivery-date";
    private static final String ALIAS_INVENTORY_MASTER_DESCRIPTION = "uda-spedinv-description";
    private static final String ALIAS_INVENTORY_MASTER_IMAGING_DATE = "uda-spedinv-imaging-date";
    private static final String ALIAS_INVENTORY_MASTER_INVOICE = "uda-spedinv-invoice";
    private static final String ALIAS_INVENTORY_MASTER_MODEL = "uda-spedinv-model";
    private static final String ALIAS_INVENTORY_MASTER_MULTIPLE_ITEMS = "uda-spedinv-multiple-items";
    private static final String ALIAS_INVENTORY_MASTER_ORDER_DATE = "uda-spedinv-order-date";
    private static final String ALIAS_INVENTORY_MASTER_PRIMARY_APPROVAL_DATE = "uda-approval-date";
    private static final String ALIAS_INVENTORY_MASTER_PRIMARY_CATEGORY = "uda-spedinv-category";
    private static final String ALIAS_INVENTORY_MASTER_PROJECT_COST = "uda-spedinv-projected-cost";
    private static final String ALIAS_INVENTORY_MASTER_PURCHASE_ORDER = "uda-spedinv-po-number";
    private static final String ALIAS_INVENTORY_MASTER_REPAIR_LOG = "uda-repair-log";
    private static final String ALIAS_INVENTORY_MASTER_SEA = "uda-spedinv-sea";
    private static final String ALIAS_INVENTORY_MASTER_SHARED = "uda-spedinv-shared";
    private static final String ALIAS_INVENTORY_MASTER_STATUS = "uda-spedinv-status";
    private static final String ALIAS_INVENTORY_MASTER_STORAGE_DETAILS = "uda-storage-details";
    private static final String ALIAS_INVENTORY_MASTER_TRAINING = "uda-training";
    private static final String ALIAS_INVENTORY_MASTER_TRANSFER_INFO = "uda-transfer-info";
    private static final String ALIAS_INVENTORY_MASTER_UNIQUE_ID = "uda-unique-id";
    private static final String ALIAS_INVENTORY_MASTER_VENDOR = "uda-vendor";
    private static final String ALIAS_INVENTORY_MASTER_WARRANTY_EXP_DATE = "uda-warranty-exp-date";
    private static final String ALIAS_INVENTORY_MASTER_WARRANTY_VENDOR = "uda-warranty-vendor";

    // Inventory transaction
    private static final String ALIAS_INVENTORY_TRANSACTION_DATE = "udb-spedinv-transaction-date";
    private static final String ALIAS_INVENTORY_TRANSACTION_LOCATION = "udb-spedinv-location";
    private static final String ALIAS_INVENTORY_TRANSACTION_RETURN_DATE = "udb-spedinv-return-date";
    private static final String ALIAS_INVENTORY_TRANSACTION_RETURN_LOCATION = "udb-spedinv-return-date";
    private static final String ALIAS_INVENTORY_TRANSACTION_TYPE = "udb-spedinv-return-location";

    private static final String ALIAS_RCD_BOARD_NUMBER = "rcd-bsid-board-number";
    private static final String ALIAS_SKL_BSID_NUMBER = "all-skl-BSID";

    private static final String CONST_NUM_1 = "1";
    private static final String CONST_NUM_2 = "2";
    private static final String DDX_ID_ON_INVENTORY = "ON-SPED-INVENTORY";
    private static final String DDX_ID_REF_BSID_SCHOOL = "REF-School-BSID";
    private static final String DDX_OID_ON_INVENTORY = "ddxOnInventory";

    private static final String REF_TABLE_NAME_BSID_BOARD = "BSID - School Boards";
    private static final String REF_TABLE_NAME_BSID_SCHOOL = "BSID - Schools";
    private static final String REPORT_PARAM_PAGE_PARAMETER = "pageParam";

    private DataDictionary m_ddxInventory;
    private DictionaryExtractor m_dictExtractor;
    private StringBuilder m_errorsLog = new StringBuilder();
    private boolean m_isAllSchools = true;
    private Map<String, Filterable<ReferenceCode>> m_refCodesFilterables = new HashMap<>();
    private Map<String, List<Record>> m_seaPPARecords = new TreeMap<>();
    private Map<String, BigDecimal> m_totalPricesOfItems = new HashMap<>();
    private UserDataContainer m_userData;

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private Locale m_user_locale;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        Collection<UserDefinedTableC> inventoryItems = retrieveInventoryItemRecords();
        for (UserDefinedTableC inventoryItem : inventoryItems) {
            initInventoryRecord(inventoryItem);
        }

        Set<String> boardNamesNumbers = m_seaPPARecords.keySet();
        for (String boardNameNumber : boardNamesNumbers) {
            fillReportDate(grid, boardNameNumber, CONST_NUM_1);
            grid.set(Record.Field.totalExpenditures.toString(), m_totalPricesOfItems.get(boardNameNumber).toString());
            fillReportDate(grid, boardNameNumber, CONST_NUM_2);
        }

        grid.beforeTop();

        return grid;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        m_ddxInventory = getDictExtractor().getDictionary(DDX_ID_ON_INVENTORY);

        initializeLocalized();
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();

        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {

                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        loc.getLocale());
                // save the messages for that language
                resources.put(loc.getLocale(), messages);

                // populate the map of valid locales
                m_validLocales.put(loc.getName(), loc.getLocale());
                if (loc.getPrimaryIndicator()) {
                    m_defaultLocale = loc.getLocale();
                }
            }
        }

        if (m_defaultLocale == null) {
            m_defaultLocale = CONST_AMERICAN_ENGLISH_LOCALE;
        }
        addParameter(PARAM_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
        addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
        // Additional hooks for enhanced implementations
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
        // Comment line below if your numeric notation, currencies and others don't display as
        // expected
        addParameter(net.sf.jasperreports5.engine.JRParameter.REPORT_LOCALE, m_user_locale); // Only
                                                                                             // tested
                                                                                             // for
                                                                                             // JasperReports
                                                                                             // engine
                                                                                             // 5
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
        m_userData = userData;
        super.saveState(userData);

        try {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), userData.getLocale());
            m_user_locale = userData.getLocale();
        } catch (Exception e) {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
            m_user_locale = Locale.US;
        }
    }

    /**
     * Fills in report data of identified page
     *
     * @param grid
     * @param schoolName
     * @param pageIdentifier
     */
    private void fillReportDate(ReportDataGrid grid, String schoolName, String pageIdentifier) {
        for (Record record : m_seaPPARecords.get(schoolName)) {
            grid.append();
            for (Record.Field field : Record.Field.values()) {
                grid.set(field.toString(), record.m_fieldValuePairs.get(field));
            }
            grid.set(REPORT_PARAM_PAGE_PARAMETER, pageIdentifier);
        }
    }

    /**
     * Gets the board by school number.
     *
     * @param schoolNumber String
     * @return Reference code
     */
    private ReferenceCode getBoardBySchoolNumber(String schoolNumber) {
        ReferenceCode schoolRefCode =
                getRefCodesFilterable(REF_TABLE_NAME_BSID_SCHOOL).extractFirst(ReferenceCode.COL_CODE, schoolNumber);
        if (schoolRefCode == null) {
            throw new RuntimeException("Cannot find reference code for school by BSID " + schoolNumber);
        }
        String boardNumber = (String) schoolRefCode.getFieldValueByAlias(ALIAS_RCD_BOARD_NUMBER,
                getDictExtractor().getDictionary(DDX_ID_REF_BSID_SCHOOL));
        ReferenceCode boardRefCode =
                getRefCodesFilterable(REF_TABLE_NAME_BSID_BOARD).extractFirst(ReferenceCode.COL_CODE, boardNumber);
        if (boardRefCode == null) {
            throw new RuntimeException("Cannot find reference code for board by BSID " + boardNumber);
        }
        return boardRefCode;
    }

    /**
     * Gets the dict extractor.
     *
     * @return Dictionary extractor
     */
    private DictionaryExtractor getDictExtractor() {
        if (m_dictExtractor == null) {
            m_dictExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictExtractor;
    }

    /**
     * Gets the dictionary.
     *
     * @return Data dictionary
     */
    private DataDictionary getDictionary() {
        return DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
    }

    /**
     * Gets the ref codes filterable.
     *
     * @param refTableName String
     * @return Filterable
     */
    private Filterable<ReferenceCode> getRefCodesFilterable(String refTableName) {
        Filterable<ReferenceCode> refCodesFilterable = m_refCodesFilterables.get(refTableName);
        if (refCodesFilterable == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceTable.COL_USER_NAME, refTableName);
            ReferenceTable referenceTable =
                    getBroker().getBeanByQuery(new QueryByCriteria(ReferenceTable.class, criteria));
            if (referenceTable == null) {
                throw new RuntimeException("Cannot find reference table " + refTableName);
            }
            refCodesFilterable = FilterableFactory.create(referenceTable.getCodeMap().values(),
                    Arrays.asList(X2BaseBean.COL_OID),
                    FilterableFactory.Filterable.PredefinedResolver.X2BASE_BEAN);
            m_refCodesFilterables.put(refTableName, refCodesFilterable);
        }
        return refCodesFilterable;
    }

    /**
     * Gets the report date.
     *
     * @return Plain date
     */
    private PlainDate getReportDate() {
        return new PlainDate();
    }

    /**
     * Initializes related inventory record.
     */
    private void initInventoryRecord(UserDefinedTableC inventoryItem) {
        try {
            UserDefinedTableA inventoryMasterRecord = retrieveInventoryMasterRecord(inventoryItem);
            UserDefinedTableB inventoryTransaction = retrieveInventoryTransactionRecord(inventoryItem);
            if (inventoryMasterRecord != null && inventoryTransaction != null) {
                String region = "All";
                SisSchool school = inventoryTransaction.getSchool();
                String schoolNumber = (String) school.getFieldValueByAlias(ALIAS_SKL_BSID_NUMBER);
                String boardNumber = "####";
                String boardName = "Undetermined";
                if (!StringUtils.isEmpty(schoolNumber)) {
                    ReferenceCode board = getBoardBySchoolNumber(schoolNumber);
                    if (board != null) {
                        boardNumber = board.getCode();
                        boardName = board.getDescription();
                    }
                }
                String boardNameNumber = boardName + boardNumber;
                if (!m_seaPPARecords.containsKey(boardNameNumber)) {
                    m_seaPPARecords.put(boardNameNumber, new ArrayList<Record>());
                }
                Record record = new Record();
                Calendar reportDate = Calendar.getInstance();
                reportDate.setTime(getReportDate());

                record.set(Record.Field.regionalOffice, region);
                record.set(Record.Field.boardName, boardName);
                record.set(Record.Field.boardNumber, boardNumber);
                record.set(Record.Field.schoolNumber, schoolNumber);
                record.set(Record.Field.item,
                        inventoryMasterRecord.getFieldValueByAlias(ALIAS_INVENTORY_MASTER_DESCRIPTION, m_ddxInventory));
                record.set(Record.Field.educationDirectorName, "");
                record.set(Record.Field.incurredBetweenMentionedTime, Boolean.FALSE);
                record.set(Record.Field.inEquipmentCost, "");
                record.set(Record.Field.noOfItems, "");
                record.set(Record.Field.notProvidedByOSAPAC, Boolean.FALSE);
                record.set(Record.Field.numberOfEquipmentIn, "");
                record.set(Record.Field.numberOfEquipmentOut, "");
                record.set(Record.Field.outEquipmentCost, "");
                BigDecimal price = new BigDecimal((String) inventoryMasterRecord
                        .getFieldValueByAlias(ALIAS_INVENTORY_MASTER_PROJECT_COST, m_ddxInventory));
                record.set(Record.Field.price, price.toString());
                BigDecimal totalExpenditures = m_totalPricesOfItems.get(boardNameNumber);
                if (totalExpenditures == null) {
                    m_totalPricesOfItems.put(boardName + boardNumber, price);
                } else if (price != null) {
                    m_totalPricesOfItems.put(boardName + boardNumber, totalExpenditures.add(price));
                }

                record.set(Record.Field.seaPPAPurchases, "");
                record.set(Record.Field.signatureDay, reportDate.get(Calendar.DATE));
                record.set(Record.Field.signatureMonth, reportDate.get(Calendar.MONTH) + 1);
                record.set(Record.Field.signatureYear, reportDate.get(Calendar.YEAR));
                record.set(Record.Field.seaMaintenanceAndTraining,
                        inventoryMasterRecord.getFieldValueByAlias(ALIAS_INVENTORY_MASTER_TRAINING, m_ddxInventory));
                record.set(Record.Field.comments, inventoryMasterRecord.getFieldValueByAlias(
                        ALIAS_INVENTORY_MASTER_ADDITIONAL_INFORMATION, m_ddxInventory));
                m_seaPPARecords.get(boardNameNumber).add(record);
            }

        } catch (Exception e) {
            // TODO error logging
            logError(e.getMessage());
        }
    }

    /**
     * Log error.
     *
     * @param error String
     */
    private void logError(String error) {
        m_errorsLog.append(error);
        m_errorsLog.append("\n");
    }

    /**
     * Retrieves 'Inventory Item' records
     *
     * @param student
     * @return {@link Collection}<{@link UserDefinedTableC}>
     */
    private Collection<UserDefinedTableC> retrieveInventoryItemRecords() {
        X2Criteria inventoryItemsCriteria = new X2Criteria();
        inventoryItemsCriteria.addEqualTo(UserDefinedTableC.COL_EXTENDED_DATA_DICTIONARY_OID, DDX_OID_ON_INVENTORY);
        QueryByCriteria inventoryItemsQuery =
                new QueryByCriteria(UserDefinedTableC.class, inventoryItemsCriteria, true);
        Collection<UserDefinedTableC> inventoryItemRecords = getBroker().getCollectionByQuery(inventoryItemsQuery);
        return inventoryItemRecords;
    }

    /**
     * Retrieves 'Inventory Master' record related to
     * inventory item.
     *
     * @return {@link UserDefinedTableA}
     */
    private UserDefinedTableA retrieveInventoryMasterRecord(UserDefinedTableC inventoryItem) {
        //@formatter:off
        /*
        X2Criteria inventoryMasterCriteria = new X2Criteria();
        inventoryMasterCriteria.addEqualTo(X2BaseBean.COL_OID, inventoryItem.getUserDefinedTableAOid());
        QueryByCriteria inventoryMasterQuery =
                new QueryByCriteria(UserDefinedTableA.class, inventoryMasterCriteria, true);
        UserDefinedTableA inventoryMasterRecord = getBroker().getBeanByQuery(inventoryMasterQuery);
        */
        //@formatter:on
        UserDefinedTableA inventoryMasterRecord = inventoryItem.getUserDefinedTableA();
        return inventoryMasterRecord;
    }

    /**
     * Retrieves 'Inventory Transaction' record related to
     * inventory item.
     *
     * @return {@link UserDefinedTableB}
     */
    private UserDefinedTableB retrieveInventoryTransactionRecord(UserDefinedTableC inventoryItem) {
        //@formatter:off
        /*
        X2Criteria inventoryTransactionCriteria = new X2Criteria();
        inventoryTransactionCriteria.addEqualTo(UserDefinedTableB.COL_EXTENDED_DATA_DICTIONARY_OID,
                DDX_OID_ON_INVENTORY);
        inventoryTransactionCriteria.addEqualTo(UserDefinedTableB.COL_USER_DEFINED_TABLE_C_OID, inventoryItem.getOid());
        QueryByCriteria inventoryTransactionQuery =
                new QueryByCriteria(UserDefinedTableB.class, inventoryTransactionCriteria, true);
        UserDefinedTableB inventoryTransactionRecord = getBroker().getBeanByQuery(inventoryTransactionQuery);
        */
        //@formatter:on
        UserDefinedTableB inventoryTransactionRecord = null;
        DataDictionaryField returnDateField =
                m_ddxInventory.findDataDictionaryFieldByAlias(ALIAS_INVENTORY_TRANSACTION_RETURN_DATE);
        Collection<UserDefinedTableB> inventoryTransactionRecords = inventoryItem.getUserDefinedRecordsB(getBroker());
        for (UserDefinedTableB record : inventoryTransactionRecords) {
            String dateStr = (String) record.getFieldValueByBeanPath(returnDateField.getJavaName());
            if (!StringUtils.isEmpty(dateStr)) {
                inventoryTransactionRecord = record;
                break;
            }
        }
        return inventoryTransactionRecord;
    }
}
