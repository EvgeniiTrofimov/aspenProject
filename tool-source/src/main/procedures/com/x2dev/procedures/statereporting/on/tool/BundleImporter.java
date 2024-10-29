/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2022 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.tool;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryCache;
import com.follett.fsc.core.k12.tools.ToolCompiler;
import com.follett.fsc.core.k12.tools.bundle.InvalidToolBundleException;
import com.follett.fsc.core.k12.tools.bundle.ToolBundle;
import com.follett.fsc.core.k12.tools.imports.ImportJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.ExportFormatManager;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StreamUtils;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.apache.ojb.broker.query.Criteria;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class BundleImporter extends ImportJavaSource {

    /**
     * @see com.follett.fsc.core.k12.tools.imports.ImportJavaSource#importData(java.io.File)
     */
    @Override
    protected void importData(File sourceFile) throws Exception {
        try {
            FileInputStream stream = new FileInputStream(sourceFile);
            File tempFolder = null;
            byte[] fileBytes = StreamUtils.getInputStreamBytes(stream);

            ToolBundle bundle = new ToolBundle(fileBytes, tempFolder);
            bundle.setReplaceCustomFormats(true);
            bundle.setReplaceCustomInputDefinitions(true);
            bundle.setReplaceCustomJavaSources(true);
            bundle.setReplaceCustomXmlDefinitions(true);
            bundle.setReplaceCustomPortableObjects(true);
            bundle.setUpdateExistingTools(true);

            List<KeyValuePair> tools = bundle.create(tempFolder, getBroker(), getOrganization(), true);
            if (!CollectionUtils.isEmpty(tools)) {
                ToolCompiler compiler = new ToolCompiler(tempFolder, getBroker());
                int successCount = compiler.compileBundle(tools);

                Set<String> errors = compiler.getErrors();
                if (!CollectionUtils.isEmpty(errors)) {
                    // TODO: add error messages
                }

                // Look for Export Format Definitions in the bundle. If found:
                // 1. Validate and assign data dictionary fields.
                // 2. Reload the extended data dictionary.
                List<String> objectIds = new ArrayList<String>();
                for (KeyValuePair pair : tools) {
                    String definitionId = (String) pair.getKey();
                    objectIds.add(definitionId);
                }
                if (!objectIds.isEmpty()) {
                    Criteria criteria = new Criteria();
                    criteria.addIn(ExportFormatDefinition.COL_PROCEDURE_ID, objectIds);
                    BeanQuery query = new BeanQuery(ExportFormatDefinition.class, criteria, false);

                    int efdCount = 0;
                    try (QueryIterator queryIterator = getBroker().getIteratorByQuery(query)) {
                        while (queryIterator.hasNext()) {
                            ExportFormatDefinition def = (ExportFormatDefinition) queryIterator.next();

                            List<StateReportValidationError> efdErrors =
                                    ExportFormatManager.validateDefinition(def, getBroker(), true);
                            for (StateReportValidationError error : efdErrors) {
                                // TODO: add error messages
                            }
                            efdCount++;
                        }
                    }

                    if (efdCount > 0) {
                        DataDictionaryCache.clearExtendedDictionaries(getBroker().getPersistenceKey(), true);
                    }
                }
            }
        } catch (IOException ioe) {
            throw (ioe);
        } catch (InvalidToolBundleException ipbe) {
            throw (ipbe);
        }
    }

}
