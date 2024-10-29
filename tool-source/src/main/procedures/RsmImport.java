/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2006 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.business.localization.LocalizationMessageResources;
import com.follett.fsc.core.k12.business.localization.LocalizationResource;
import com.follett.fsc.core.k12.tools.imports.TextImportJavaSource;
import com.x2dev.utils.StringUtils;
import java.io.File;
import java.util.List;

/**
 * Imports RSM data used for off-line translation.
 * The file must contain the following fields (in order):
 * <ol>
 * <li>Entity OID
 * <li>Locale
 * <li>RSM key
 * <li>Primary locale's value
 * <li>Translated value
 * </ol>
 *
 * @author X2 Development Corporation
 */
public class RsmImport extends TextImportJavaSource {
    private static final long serialVersionUID = 1L;

    private static final String DELETE_VALUE = "#DELETE#";

    /*
     * Record indexes
     */
    private static final int INDEX_OBJECT_OID = 0;
    private static final int INDEX_LOCALE = 1;
    private static final int INDEX_KEY = 2;
    private static final int INDEX_TRANSLATED_VALUE = 4;

    /**
     * Wrapping a transaction around the RSM import causes the RSM Update cache messaging events
     * to batch and fire to remote servers as a single collection rather than many individual
     * events. This prevents flooding of remote servers with multiple events which use database
     * connections.
     *
     * @param file File
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#afterImportData(java.io.File)
     */
    @Override
    protected void afterImportData(File file) {
        getBroker().commitTransaction();
        super.afterImportData(file);
    }

    /**
     * Wrapping a transaction around the RSM import causes the RSM Update cache messaging events
     * to batch and fire to remote servers as a single collection rather than many individual
     * events. This prevents flooding of remote servers with multiple events which use database
     * connections.
     *
     *
     * @param file File
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#beforeImportData(java.io.File)
     */
    @Override
    protected void beforeImportData(File file) {
        super.beforeImportData(file);
        getBroker().beginTransaction();
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
     * Import data.
     *
     * @param sourceFile File
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.imports.TextImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        setValueWrappingMode(VALUE_WRAPPING_MODE.NONE);
        setValueDelimiter('\t');
        setUseEscapes(false);

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
        String objectOid = record.get(INDEX_OBJECT_OID);
        String locale = record.get(INDEX_LOCALE);
        String key = record.get(INDEX_KEY);
        String translatedValue = record.get(INDEX_TRANSLATED_VALUE);

        if (StringUtils.isEmpty(locale) || StringUtils.isEmpty(key)) {
            incrementSkipCount();
            return;
        }

        incrementMatchCount();

        if (DELETE_VALUE.equals(translatedValue)) {
            translatedValue = null;
        }

        if (LocalizationCache.hasResources(getBroker().getPersistenceKey().getDeploymentId(), locale)) {
            LocalizationMessageResources resources =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), locale, true);

            if (resources.isPresent(key)) {
                String oldValue = "";
                try {
                    oldValue = resources.getMessage(key);

                    if (oldValue != null && oldValue.equals(translatedValue)) {
                        incrementSkipCount();
                    } else {
                        LocalizationResource resource =
                                new LocalizationResource(locale, key, objectOid, translatedValue);
                        LocalizationCache.setLocalizedResource(getBroker(), resource);
                        incrementUpdateCount();
                    }
                } catch (IllegalArgumentException ex) {
                    logInvalidRecord(lineNumber, ex.getLocalizedMessage());
                    incrementSkipCount();
                }
            } else {
                LocalizationResource resource = new LocalizationResource(locale, key, objectOid, translatedValue);
                LocalizationCache.setLocalizedResource(getBroker(), resource);
                if (!StringUtils.isEmpty(translatedValue)) {
                    incrementInsertCount();
                } else {
                    incrementSkipCount();
                }
            }
        } else {
            incrementSkipCount();
        }
    }
}
