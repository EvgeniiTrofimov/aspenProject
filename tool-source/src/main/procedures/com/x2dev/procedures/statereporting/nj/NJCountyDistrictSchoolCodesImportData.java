/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nj;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.UpdateQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataField;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.utils.X2BaseException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.poi37.poifs.filesystem.POIFSFileSystem;
import org.apache.poi37.ss.usermodel.Cell;
import org.apache.poi37.ss.usermodel.DataFormatter;
import org.apache.poi37.ss.usermodel.Row;
import org.apache.poi37.ss.usermodel.Sheet;
import org.apache.poi37.ss.usermodel.Workbook;
import org.apache.poi37.ss.usermodel.WorkbookFactory;

/**
 * Java source for the Waco student import. Each record has student, person, address,
 * potentially 2 contacts/student contacts/persons/addresses
 * This import will create or update and reuse those beans as necessary.
 *
 * @author Follett School Solutions
 */
public class NJCountyDistrictSchoolCodesImportData extends XlsImportJavaSource {
    private static final long serialVersionUID = 1L;

    // Aliases
    private static final String ALIAS_DESCRIPTION_2 = "Description_2";

    // Constant Values
    private static final String CONST_REF_TABLE_EXT_ID = "REF-DOE-LOC-CODES";
    private static final String CONST_REF_TABLE_IN_INPUT = "Ref-InInput";
    private static final String CONST_REF_TABLE_NAME = "County District School Codes";
    private static final String CONST_REF_TABLE_NOT_IN_INPUT = "Ref-NotInInput";

    // Input order. County Name is the second field but is unused at this time
    private static final int INDEX_COUNTY_CODE = 0;
    private static final int INDEX_DISTRICT_CODE = 2;
    private static final int INDEX_DISTRICT_NAME = 3;
    private static final int INDEX_SCHOOL_CODE = 4;
    private static final int INDEX_SCHOOL_NAME = 5;

    private static final int INPUT_FIELD_CT = 6;

    private ModelBroker m_modelBroker;
    private String m_orgOid;
    private List<DataField> m_refCodesDataFieldsList;
    private Map<String, ReferenceCode> m_refCodesMap;
    private Map<String, String> m_refCodesMatchStatusMap;
    private Map<String, String> m_refCodesNewValueMap;
    private DataDictionary m_rtbExtDictionary;
    private String m_rtbOid;

    private boolean m_success;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.x2dev.sis.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return INPUT_FIELD_CT;
    }

    /**
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.XlsImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        try {
            // First import Data
            /**
             * Import data.
             *
             * @param sourceFile File
             * @throws Exception exception
             * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
             */
            InputStream inputStream = null;

            try {
                inputStream = new FileInputStream(sourceFile.getPath());
            } catch (FileNotFoundException fnfe) {
                logInvalidRecord(0, "error.file.edit.notFound", sourceFile.getAbsolutePath());
            }

            if (inputStream != null) {
                try {
                    POIFSFileSystem fileSystem = new POIFSFileSystem(inputStream);
                    Workbook workbook = WorkbookFactory.create(fileSystem);
                    // HSSFWorkbook workbook = new HSSFWorkbook(fileSystem);
                    Sheet sheet = workbook.getSheetAt(0);
                    DataFormatter formatter = new DataFormatter();
                    int lineNumber = 1;
                    while (lineNumber <= sheet.getLastRowNum()) {
                        List<String> record = new ArrayList<String>(getFieldCount());

                        try {
                            Row row = sheet.getRow(lineNumber);
                            for (short i = 0; i < getFieldCount(); i++) {
                                Cell cell = row.getCell(i);
                                if (cell != null) {
                                    switch (cell.getCellType()) {
                                        case Cell.CELL_TYPE_NUMERIC:
                                            String value = formatter.formatCellValue(cell);
                                            record.add(value);
                                            break;

                                        case Cell.CELL_TYPE_STRING:
                                            record.add(cell.getRichStringCellValue().getString());
                                            break;

                                        default:
                                            record.add("");
                                            break;
                                    }
                                } else {
                                    record.add("");
                                }
                            }
                        } catch (Exception e) {
                            throw new X2BaseException((String) null, "Exception loading row " + (lineNumber + 1), e);
                        }

                        importRecord(record, lineNumber);
                        lineNumber++;
                    }
                } finally {
                    inputStream.close();
                }
            }
            // Disable Reference Code records that do not have a match in the input file
            for (String code : m_refCodesMap.keySet()) {
                if (m_refCodesMatchStatusMap.get(code).equals(CONST_REF_TABLE_NOT_IN_INPUT)) {
                    ReferenceCode rcd = m_refCodesMap.get(code);
                    // Add ReferenceCode if not found and then update values
                    if (rcd != null) {
                        rcd.setDisabledIndicator(true);
                        saveBeanSafely(0, "Error saving Reference Code Record", rcd, m_modelBroker);
                        String message = "Not in input file, Disabled for State Code " + code;
                        logToolMessage(Level.INFO, message, false);
                    }
                }
            }

            // Get fields that use the reference codes
            m_refCodesDataFieldsList = new ArrayList<DataField>();
            getRefCodesFieldsList();

            // Set fields that use the reference codes to the new value
            for (String code : m_refCodesNewValueMap.keySet()) {
                for (DataField dataField : m_refCodesDataFieldsList) {
                    String dbName = dataField.getDatabaseName();
                    String javaName = dataField.getJavaName();
                    String codeNew = m_refCodesNewValueMap.get(code);

                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(dbName, code);

                    UpdateQuery query =
                            new UpdateQuery(dataField.getDataTable().getDataClass(), criteria, javaName, codeNew);
                    m_modelBroker.executeUpdateQuery(query);
                }
            }
        } catch (Exception e) {
            logInvalidRecord(0, e.getMessage());
            AppGlobals.getLog().severe(" == Stack Trace: \n" + ExceptionUtils.getFullStackTrace(e));
            m_success = false;
            throw e;
        } finally {
            if (m_success) {
                m_modelBroker.commitTransaction();
            } else {
                AppGlobals.getLog().severe("Import FAILED, rolling back transaction");
                m_modelBroker.rollbackTransaction();
            }
        }
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.x2dev.sis.tools.imports.XlsImportJavaSource#importRecord(java.util.List, int)
     *
     *      Imports record to update reference table
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        // Fetch input fields
        String countyCode = record.get(INDEX_COUNTY_CODE);
        String districtCode = record.get(INDEX_DISTRICT_CODE);
        String districtName = record.get(INDEX_DISTRICT_NAME);
        String schoolCode = record.get(INDEX_SCHOOL_CODE);
        String schoolName = record.get(INDEX_SCHOOL_NAME);

        // Check if row is empty (by checking County Code/County Name) and return if it is
        if (StringUtils.isEmpty(countyCode)
                && StringUtils.isEmpty(districtCode)
                && StringUtils.isEmpty(schoolCode)) {
            return;
        }

        String inputKeyCode = countyCode.trim() + districtCode.trim() + schoolCode.trim();
        ReferenceCode rcd = m_refCodesMap.get(inputKeyCode);
        String message = "";
        String codeOldValue = "";

        // Add ReferenceCode if not found and then update values
        if (rcd == null) {
            message = "Added for State Code " + inputKeyCode;
            rcd = X2BaseBean.newInstance(ReferenceCode.class, m_modelBroker.getPersistenceKey());
            rcd.setReferenceTableOid(m_rtbOid);
            rcd.setStateCode(inputKeyCode);
            rcd.setOwnerOid(m_orgOid);
            rcd.setOwnerType(Ownable.OWNER_TYPE_ORG1);
        } else {
            message = "Updated for State Code " + inputKeyCode;
            codeOldValue = rcd.getCode();
        }
        rcd.setCode(inputKeyCode);
        rcd.setDescription(schoolName);

        String description2 =
                "District = " + districtCode + "-" + districtName + ", County = " + countyCode + ", School = "
                        + schoolCode;
        rcd.setFieldValueByAlias(ALIAS_DESCRIPTION_2, description2, m_rtbExtDictionary);

        // Load/Update but disable record if School Code is 000
        if (schoolCode.equals("000")) {
            rcd.setDisabledIndicator(true);
        } else {
            rcd.setDisabledIndicator(false);
        }

        // Save rcd record if the first with its key in the input file (to avoid duplicate
        // errors)
        String matchStatus = m_refCodesMatchStatusMap.get(inputKeyCode);
        // Condition for checking that it is the first
        if ((matchStatus != null) && (matchStatus.equals(CONST_REF_TABLE_IN_INPUT))) {
            incrementSkipCount();
        } else {
            saveBeanSafely(lineNumber, "Error saving Reference Code Record", rcd, m_modelBroker);
            logToolMessage(Level.INFO, message, false);

            // Update match status, code old/new values
            m_refCodesMatchStatusMap.put(inputKeyCode, CONST_REF_TABLE_IN_INPUT);
            if (StringUtils.isNotEmpty(codeOldValue)) {
                incrementUpdateCount();
                m_refCodesNewValueMap.put(codeOldValue, inputKeyCode);
            } else {
                incrementInsertCount();
            }
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        // initialize rtbOid for new Reference Codes
        X2Criteria criteriaRtb = new X2Criteria();
        criteriaRtb.addEqualTo(ReferenceTable.COL_USER_NAME, CONST_REF_TABLE_NAME);

        QueryByCriteria queryRtb = new QueryByCriteria(ReferenceTable.class, criteriaRtb);
        m_rtbOid = getBroker().getBeanByQuery(queryRtb).getOid();

        // initialize m_rtbExtDdx for Extended Data Dictionary
        X2Criteria criteriaDdx = new X2Criteria();
        criteriaDdx.addEqualTo(ExtendedDataDictionary.COL_ID, CONST_REF_TABLE_EXT_ID);

        QueryByCriteria queryDdx = new QueryByCriteria(ExtendedDataDictionary.class, criteriaDdx);
        ExtendedDataDictionary rtbDdx = (ExtendedDataDictionary) getBroker().getBeanByQuery(queryDdx);
        m_rtbExtDictionary = DataDictionary.getDistrictDictionary(rtbDdx, getBroker().getPersistenceKey());

        // Load map of County District School Codes
        m_refCodesMatchStatusMap = new HashMap<String, String>();
        loadCountyDistrictSchoolCodesMap();

        // initialize variables for update
        m_modelBroker = new ModelBroker(getPrivilegeSet());
        m_success = true;
        m_orgOid = getOrganization().getOid();
        m_refCodesNewValueMap = new HashMap<String, String>();

        // Start a transaction for updating the reference table, and rollback if we don't complete
        // to the last input record.
        m_modelBroker.beginTransaction();
    }

    /**
     * Get list of fields that use these reference codes.
     *
     * @return void
     * @throws X2BaseException exception
     */
    private void getRefCodesFieldsList() throws X2BaseException {
        // Load List
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(DataFieldConfig.COL_REFERENCE_TABLE_OID, m_rtbOid);

        QueryByCriteria query = new QueryByCriteria(DataFieldConfig.class, criteria);
        QueryIterator fields = getBroker().getIteratorByQuery(query);
        try {
            while (fields.hasNext()) {
                DataFieldConfig fdd = (DataFieldConfig) fields.next();
                DataField dataField = fdd.getDataField();
                if (dataField != null) {
                    m_refCodesDataFieldsList.add(dataField);
                }
            }
        } finally {
            fields.close();
        }
    }

    /**
     * Load map of County District School Codes.
     *
     * @throws X2BaseException exception
     */
    private void loadCountyDistrictSchoolCodesMap() throws X2BaseException {
        // Load Map
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.REL_REFERENCE_TABLE + PATH_DELIMITER +
                ReferenceTable.COL_USER_NAME, CONST_REF_TABLE_NAME);

        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
        m_refCodesMap = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 5000);

        // Initialize match status
        for (String code : m_refCodesMap.keySet()) {
            m_refCodesMatchStatusMap.put(code, CONST_REF_TABLE_NOT_IN_INPUT);
        }
    }

    /**
     * Checks the result of a saveBean call for errors, throws generic exception and logs if there
     * are errors.
     *
     * @param lineNumber - line number from input, 0 if no line number
     * @param errorMsg - message to display in the event of an error saving the bean
     * @param bean - the bean to save
     * @param broker - the broker for saving the bean
     * @throws Exception exception
     */
    private void saveBeanSafely(int lineNumber, String errorMsg, X2BaseBean bean, ModelBroker broker) throws Exception {
        if (bean.isDirty()) {
            Collection<ValidationError> valErrors = broker.saveBean(bean);
            if (valErrors.size() > 0) {
                StringBuffer errorCause = new StringBuffer();
                errorCause.append(errorMsg);
                errorCause.append(" : ");

                for (ValidationError valError : valErrors) {
                    errorCause.append(valError.toString());
                }

                Exception e = new Exception(errorMsg + " : " + errorCause.toString());
                // incrementSkipCount();
                throw e;
            }
        }
    }
}
