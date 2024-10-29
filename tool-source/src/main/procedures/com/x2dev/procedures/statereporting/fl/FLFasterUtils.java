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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.Procedure;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DictionaryHelper;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.DefaultValue;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * @author Follett Software Company
 * @copyright 2018
 */
public class FLFasterUtils {
    /**
     * Static processing class used to process tool input and generate default elements.
     */
    public static class ToolInputProcessor {
        private static final String GROUP_ELEMENT = "group";
        private static final String INPUT_DEFAULT_VALUE_SOURCE_ATTRIB = "default-value-source";
        private static final String ROOT_ALLOW_SCHOOL_SELECT_ATTRIB = "allow-school-select";

        /**
         * This method will rebuild the ToolInput object from information in the
         * Tool.
         *
         * @param tool Tool
         * @param inputDefaults boolean
         * @param userData UserDataContainer
         * @param broker X2Broker
         * @param locale Locale
         * @return a fully populated ToolInput from the data in JobEntry.
         * @throws X2BaseException exception
         */
        public static ToolInput restoreToolInput(Tool tool,
                                                 boolean inputDefaults,
                                                 UserDataContainer userData,
                                                 X2Broker broker,
                                                 Locale locale)
                throws X2BaseException {
            ToolInput input = null;
            if (tool != null) {
                SAXBuilder builder = new SAXBuilder();

                // Parameter values defined in the Tool input.xml
                // Make a list of all params and params with default values if the
                // inputDefaults flag is set.
                Set<String> allParams = new HashSet<>();
                Map<String, DefaultValue> defaultParams = new HashMap<>();
                String inputDef = tool.getFullInputDefinition();
                if (!StringUtils.isEmpty(inputDef)) {
                    try {
                        Document inputDefDoc = builder.build(new ByteArrayInputStream(inputDef.getBytes()));
                        Element idRoot = inputDefDoc.getRootElement();
                        // See if the root element has attribute for "allow school selection"
                        Attribute schoolSelect = idRoot.getAttribute(ROOT_ALLOW_SCHOOL_SELECT_ATTRIB);
                        if (schoolSelect != null && schoolSelect.getValue().equalsIgnoreCase("true")) {
                            // "schoolOid" is a valid parameter.
                            allParams.add(ToolInput.SCHOOL_OID_PARAM);
                        }

                        parseInputElements(inputDefaults, allParams, defaultParams, idRoot);
                    } catch (JDOMException jde) {
                        // Ignore. The tool input template is bad somehow. We can't get the input
                        // defaults from it.
                    } catch (IOException ioe) {
                        // Ignore. The tool input template is bad somehow. We can't get the input
                        // defaults from it.
                    }
                }

                input = new ToolInput(tool, null, userData, locale);
                for (String key : defaultParams.keySet()) {
                    // From the DefaultValue, get the default object.
                    // Then try to convert it to a string with a converter.
                    DefaultValue dv = defaultParams.get(key);
                    if (dv != null) {
                        // Get the default value and convert it to String for display.
                        Object resValue = dv.resolve(userData, broker, locale);
                        String resString = null;
                        if (resValue instanceof String) {
                            resString = (String) resValue;
                        } else {
                            Converter someConverter =
                                    ConverterFactory.getConverterForClass(resValue.getClass().getName(), locale);
                            if (someConverter != null) {
                                resString = someConverter.javaToString(resValue);
                            } else {
                                resString = resValue.toString(); // what else?
                            }
                        }
                        input.setParameterAsString(key, resString);
                    }
                }
            }
            return input;
        }

        /**
         * Helper method to recursively parse input parameters from the root element.
         *
         * @param inputDefaults boolean
         * @param allParams Set<String>
         * @param defaultParams Map<String,DefaultValue>
         * @param root Element
         */
        private static void parseInputElements(boolean inputDefaults,
                                               Set<String> allParams,
                                               Map<String, DefaultValue> defaultParams,
                                               Element root) {
            for (Object obj : root.getChildren()) {
                Element child = (Element) obj;
                if (child.getName().equals(ToolInput.INPUT_ELEMENT)) {
                    allParams.add(child.getAttributeValue(ToolInput.INPUT_NAME_ATTRIB));

                    if (inputDefaults) {
                        Attribute defaultAttr = child.getAttribute(ToolInput.INPUT_DEFAULT_VALUE_ATTRIB);
                        Attribute defaultAttrSource = child.getAttribute(INPUT_DEFAULT_VALUE_SOURCE_ATTRIB);
                        if (defaultAttr != null) {
                            String dfltVal = defaultAttr.getValue();
                            String dfltValueSource = null;
                            if (defaultAttrSource != null) {
                                // This input has a default value with source.
                                // Add to the DefaultValue object for this value and source.
                                dfltValueSource = defaultAttrSource.getValue();
                            }
                            defaultParams.put(child.getAttributeValue(ToolInput.INPUT_NAME_ATTRIB),
                                    new DefaultValue(dfltVal, dfltValueSource));
                        }
                    }
                } else if (child.getName().equals(GROUP_ELEMENT)) {
                    parseInputElements(inputDefaults, allParams, defaultParams, child);
                }
            }
        }
    }

    private static final String ID_EXPORT = "EXP-FL-FST";
    private static final String INPUT_PARAM_CURRENT_SELECTION = "##current";

    private static final SimpleDateFormat s_dateInputFormat = new SimpleDateFormat("M/d/yy");

    /**
     * Creates the tool job for the export definition and loads default parameters for the export.
     *
     * @param exportDefinition ImportExportDefinition
     * @param userData
     * @param broker
     * @param locale
     * @param parameters
     * @param criteria
     * @return ToolJob
     * @throws Exception exception
     */
    public static ToolJob createToolJob(ImportExportDefinition exportDefinition,
                                        UserDataContainer userData,
                                        X2Broker broker,
                                        Locale locale,
                                        Map parameters,
                                        X2Criteria criteria)
            throws Exception {
        ToolInput exportInput =
                ToolInputProcessor.restoreToolInput(exportDefinition, true, userData, broker, locale);
        for (Object setItem : parameters.entrySet()) {
            Entry<String, Object> entry = (Entry<String, Object>) setItem;
            Object value = entry.getValue();
            if (value instanceof String) {
                exportInput.setParameterAsString(entry.getKey(), (String) entry.getValue());
            } else if (value instanceof Boolean) {
                exportInput.setParameterAsString(entry.getKey(), entry.getValue().toString());
            } else if (value instanceof Date) {
                exportInput.setParameterAsString(entry.getKey(), s_dateInputFormat.format(value));
            }
        }
        exportInput.setParameterAsString(INPUT_PARAM_CURRENT_SELECTION,
                getCurrentSelectionOids(userData, criteria, broker));

        userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);
        return ToolJob.createJob(exportDefinition, userData, tempFolder, false, locale);
    }

    /**
     * Creates the tool job for the procedure and loads default parameters for the export.
     *
     * @param procedure
     * @param parameters
     * @param userData
     * @param broker
     * @param locale
     *
     * @return ToolJob
     * @throws Exception exception
     */
    public static ToolJob createToolJob(Procedure procedure,
                                        Map<String, Object> parameters,
                                        UserDataContainer userData,
                                        X2Broker broker,
                                        Locale locale)
            throws Exception {
        ToolInput exportInput =
                ToolInputProcessor.restoreToolInput(procedure, true, userData, broker, locale);

        for (Entry<String, Object> parameter : parameters.entrySet()) {
            exportInput.setParameterAsString(parameter.getKey(), parameter.getValue().toString());
        }

        userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);
        return ToolJob.createJob(procedure, userData, tempFolder, false, locale);
    }

    /**
     * Gets the current selection oids.
     *
     * @param userData UserDataContainer
     * @param currentCriteria X2Criteria
     * @param broker X2Broker
     * @return String
     */
    public static String getCurrentSelectionOids(UserDataContainer userData,
                                                 X2Criteria currentCriteria,
                                                 X2Broker broker) {
        String selectionOids = null;
        SisStudent student = userData.getCurrentRecord(SisStudent.class);
        if (student != null) {
            selectionOids = student.getOid();
        }
        if (selectionOids == null) {
            X2Criteria criteria = currentCriteria;
            SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, criteria);
            Collection<String> oids = broker.getSubQueryCollectionByQuery(subQuery);

            for (String oid : oids) {
                if (selectionOids == null) {
                    selectionOids = oid;
                } else {
                    selectionOids = selectionOids + ";" + oid;
                }
            }
        }

        return selectionOids;
    }

    /**
     * Returns records for passed result handler.
     *
     * @param handler ResultHandler
     * @return int
     * @throws IOException Signals that an I/O exception has occurred.
     */
    public static List<String> getRecords(ResultHandler handler) throws IOException {
        ArrayList<String> rows = new ArrayList<>();
        FileInputStream result = new FileInputStream(handler.getFilePath());
        try (BufferedReader br = new BufferedReader(new InputStreamReader(result))) {
            String row = null;
            while ((row = br.readLine()) != null) {
                rows.add(row);
            }
        }

        return rows;
    }

    /**
     * Gets the actual records.
     *
     * @param studentOid String
     * @param recordType String
     * @param recordsType
     * @param userData UserDataContainer
     * @param broker X2Broker
     * @param locale Locale
     * @param parameters Map
     * @param currentCriteria X2Criteria
     * @return List
     * @throws Exception exception
     */
    public static Map<String, List<String>> getActualResponseRecords(String studentOid,
                                                                     Map<String, String> parameters,
                                                                     UserDataContainer userData,
                                                                     X2Broker broker,
                                                                     Locale locale)
            throws Exception {
        X2Criteria exportCriteria = new X2Criteria();
        exportCriteria.addEqualTo(ImportExportDefinition.COL_ID, ID_EXPORT);
        ImportExportDefinition exportDefinition = (ImportExportDefinition) broker
                .getBeanByQuery(new QueryByCriteria(ImportExportDefinition.class, exportCriteria));
        Map<String, List<String>> rowsByTypes = new HashMap<>();
        if (exportDefinition != null) {
            X2Criteria currentCriteria = new X2Criteria();
            currentCriteria.addEqualTo(X2BaseBean.COL_OID, studentOid);
            ToolJob exportJob = createToolJob(exportDefinition, userData, broker, locale, parameters, currentCriteria);
            try {
                exportJob.run();
            } finally {
                ResultHandler resultHandler = exportJob.getResultHandler();
                resultHandler.close();
                try {
                    if (exportJob.getStatus() == ToolJob.STATUS_SUCCESS) {
                        List<String> rowsList = FLFasterUtils.getRecords(exportJob.getResultHandler());
                        for (String row : rowsList) {
                            String rowType = row.substring(1, 3);
                            List<String> typeRows = rowsByTypes.get(rowType);
                            if (typeRows == null) {
                                typeRows = new ArrayList<String>();
                                rowsByTypes.put(rowType, typeRows);
                            }
                            typeRows.add(row);
                        }
                    } else {
                        throw new X2RuntimeException();
                    }
                } catch (FileNotFoundException fnfe) {
                    throw new X2BaseException(fnfe);
                } catch (IOException ioe) {
                    throw new X2BaseException(ioe);
                }
            }
        }
        return rowsByTypes;
    }

    public static void setFieldValueByBeanPath(X2BaseBean bean, String path, Object value, DataDictionary dictionary) {
        String id = DictionaryHelper.translateAlias(path, dictionary, true);
        ModelProperty property = new ModelProperty(SisStudent.class, id, dictionary);

        bean.setFieldValueByBeanPath(property.getBeanPath(), value);
    }
}
