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
package com.x2dev.procedures.statereporting.uk;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */


import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Import procedure to import current discount code including the following
 * <li>Insert the new discount code
 * <li>Disable discount code no longer used.
 *
 * @author X2 Development Corporation
 */
@SuppressWarnings("unused")
public class DiscountCodeImportData extends TextImportJavaSource {
    private static final int INDEX_DISCOUNT_ID = 0;
    private static final int INDEX_DISCOUNT_CODE = 1;
    private static final int INDEX_DISCOUNT_DESCRIPTION = 2;
    private static final int INDEX_SSFT_2 = 3;
    private static final int INDEX_SSFT_1 = 4;
    private static final int INDEX_MAP = 5;
    private static final int INDEX_EFFECTIVE_FROM = 6;
    private static final int INDEX_EFFECTIVE_TO = 7;
    private static final int INDEX_LAST_UPDATED = 8;

    private Collection<String> m_currentDiscountCodes;
    private String m_discountRefTableOid;
    private Map<String, ReferenceCode> m_existingDiscountCodes;

    /**
     * Imports the data from the given file.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        m_currentDiscountCodes = new ArrayList<String>();

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField dictionaryField =
                dictionary.findDataDictionaryField(SchoolCourse.class.getName(), SchoolCourse.COL_DISCOUNT_CODE);

        if (dictionaryField.hasReferenceTable()) {
            m_discountRefTableOid = dictionaryField.getDataFieldConfig().getReferenceTableOid();
        }

        if (m_discountRefTableOid != null) {
            /*
             * Existing discount codes
             */
            Criteria refCodeCriteria = new Criteria();
            refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, m_discountRefTableOid);

            QueryByCriteria qanQuery = new QueryByCriteria(ReferenceCode.class, refCodeCriteria);
            m_existingDiscountCodes = getBroker().getMapByQuery(qanQuery, ReferenceCode.COL_CODE, 2000);

            super.importData(sourceFile);
            disableNonCurrentDiscountCodes(); // Disable non-current discount code.
        }
    }

    /**
     * Split line.
     *
     * @param line String
     * @param lineNumber int
     * @return List
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#splitLine(java.lang.String,
     *      int)
     */
    @Override
    protected List<String> splitLine(String line, int lineNumber) {
        List<String> record = new ArrayList<String>();
        StringBuilder recontructLine = new StringBuilder();
        if (lineNumber == 1) {
            // Handle the header row
            Collection<String> components = StringUtils.convertDelimitedStringToList(line, ',', true);
            for (String component : components) {
                if (recontructLine.length() > 0) {
                    recontructLine.append(",");
                }
                if (component.indexOf("\"") == -1) {
                    component = "\"" + component + "\"";
                }
                recontructLine.append(component);
            }

            record = super.splitLine(recontructLine.toString(), lineNumber);
        } else {
            line = line.replaceAll("\\\\", "/"); // replace the escape characters
            // Split it using double quotes first
            String[] temp = line.split("\"");
            if (temp.length == 1) {
                setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
                record = super.splitLine(line, lineNumber);
            } else {
                for (int x = 0; x < temp.length; x++) {
                    if (temp[x].startsWith(",")) {
                        temp[x] = temp[x].substring(1, temp[x].length());
                        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
                        record.addAll(super.splitLine(temp[x], lineNumber));
                    } else if (temp[x].endsWith(",")) {
                        temp[x] = temp[x].substring(0, temp[x].length() - 1);
                        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
                        record.addAll(super.splitLine(temp[x], lineNumber));
                    } else {
                        record.add(temp[x]);
                    }
                }
            }
        }
        return record;
    }

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 9;
    }

    /**
     * Import record.
     *
     * @param record List<String>
     * @param lineNumber int
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importRecord(java.util.List,
     *      int)
     */
    @Override
    protected void importRecord(List<String> record, int lineNumber) throws Exception {
        // Skip the head row
        if (lineNumber > 1) {
            String discountCode = record.get(INDEX_DISCOUNT_CODE);
            ReferenceCode discount = null;
            if (!m_existingDiscountCodes.containsKey(discountCode)) {
                discount = X2BaseBean.newInstance(ReferenceCode.class, getBroker().getPersistenceKey());
                incrementInsertCount();
            } else {
                discount = m_existingDiscountCodes.get(discountCode);
                incrementMatchCount();
            }
            discount.setCode(discountCode);
            discount.setDescription(record.get(INDEX_DISCOUNT_DESCRIPTION));
            discount.setDisabledIndicator(false);
            discount.setReferenceTableOid(m_discountRefTableOid);

            getBroker().saveBeanForced(discount);

            m_currentDiscountCodes.add(discountCode);
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#saveState(com.follett.fsc.core.k12.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
    }

    /**
     * Disable any non-current discount codes.
     */
    private void disableNonCurrentDiscountCodes() {
        for (String existingQanCode : m_existingDiscountCodes.keySet()) {
            if (!m_currentDiscountCodes.contains(existingQanCode)) {
                ReferenceCode discountCode = m_existingDiscountCodes.get(existingQanCode);
                discountCode.setDisabledIndicator(true);

                getBroker().saveBeanForced(discountCode);
            }
        }
    }
}
