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


import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.Ownable;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.RefQanCode;
import com.x2dev.sis.model.business.admin.SisReferenceManager;
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
 * Import procedure to import current QAN code including the following
 * <li>Insert the new QAN code
 * <li>Disable QAN code no longer used.
 *
 * @author X2 Development Corporation
 */
@SuppressWarnings("unused")
public class QanCodeImportData extends TextImportJavaSource {
    private static final int INDEX_QAN_ID = 0;
    private static final int INDEX_QAN_CODE = 1;
    private static final int INDEX_AB = 2;
    private static final int INDEX_QUAL_TYPE = 3;
    private static final int INDEX_MAP = 4;
    private static final int INDEX_DISC_CODE = 5;
    private static final int INDEX_TITLE = 6;
    private static final int INDEX_SHORT_TITLE = 7;
    private static final int INDEX_ACC_START_DATE = 8;
    private static final int INDEX_ACC_END_DATE = 9;
    private static final int INDEX_CER_END_DATE = 10;
    private static final int INDEX_APP_START_DATE = 11;
    private static final int INDEX_APP_END_DATE = 12;
    private static final int INDEX_SSFT_2 = 13;
    private static final int INDEX_SSFT_1 = 14;
    private static final int INDEX_NQF = 15;
    private static final int INDEX_EFFECTIVE_FROM = 16;
    private static final int INDEX_EFFECTIVE_TO = 17;
    private static final int INDEX_LAST_UPDATED = 18;

    private Collection<String> m_currentQanCodes;
    private Map<String, RefQanCode> m_existingQanCodes;

    /**
     * Gets the field count.
     *
     * @return int
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#getFieldCount()
     */
    @Override
    protected int getFieldCount() {
        return 19;
    }

    /**
     * Imports the data from the given file.
     *
     * @param sourceFile File
     * @throws Exception exception
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        m_currentQanCodes = new ArrayList<String>();

        /*
         * Existing QAN codes
         */
        Criteria qanCriteria =
                SisReferenceManager.getQanCodesCriteria(getOwnableCriteria(), getBroker().getPersistenceKey());
        QueryByCriteria qanQuery = new QueryByCriteria(RefQanCode.class, qanCriteria);
        m_existingQanCodes = getBroker().getMapByQuery(qanQuery, RefQanCode.COL_QAN_CODE, 2000);

        super.importData(sourceFile);

        disableNonCurrentQanCodes(); // Disable non-current discount code.
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
            String qanCode = record.get(INDEX_QAN_CODE);
            RefQanCode qan = null;
            if (!m_existingQanCodes.containsKey(qanCode)) {
                qan = X2BaseBean.newInstance(RefQanCode.class, getBroker().getPersistenceKey());
                incrementInsertCount();
            } else {
                qan = m_existingQanCodes.get(qanCode);
                incrementMatchCount();
            }
            qan.setQanCode(qanCode);
            qan.setAbIdentifier(record.get(INDEX_AB));
            qan.setDisabledIndicator(false);
            qan.setDiscountCode(record.get(INDEX_DISC_CODE));
            qan.setMap(record.get(INDEX_MAP));
            qan.setNqf(record.get(INDEX_NQF));
            qan.setOwnerOid(getOrganization().getRootOrganization().getOid());
            qan.setOwnerType(Ownable.OWNER_TYPE_ORG1);
            qan.setQualificationShortTitle(record.get(INDEX_SHORT_TITLE));
            qan.setQualificationTitle(record.get(INDEX_TITLE));
            qan.setQualificationType(record.get(INDEX_QUAL_TYPE));
            qan.setSsft1(record.get(INDEX_SSFT_1));
            qan.setSsft2(record.get(INDEX_SSFT_2));

            getBroker().saveBeanForced(qan);

            m_currentQanCodes.add(qanCode);
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
     * Disable any non-current QAN codes.
     */
    private void disableNonCurrentQanCodes() {
        for (String existingQanCode : m_existingQanCodes.keySet()) {
            if (!m_currentQanCodes.contains(existingQanCode)) {
                RefQanCode qan = m_existingQanCodes.get(existingQanCode);
                qan.setDisabledIndicator(true);

                getBroker().saveBeanForced(qan);
            }
        }
    }
}
