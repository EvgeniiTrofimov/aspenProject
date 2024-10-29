/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.fl;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataTable;
import com.follett.fsc.core.k12.beans.ExportFormatDefinition;
import com.follett.fsc.core.k12.beans.ExportFormatRow;
import com.follett.fsc.core.k12.beans.ImportExportDefinition;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryRelationship;
import com.follett.fsc.core.k12.business.dictionary.ExtendedDictionaryAttributes;
import com.follett.fsc.core.k12.business.portability.BundledObjectImporter;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.DefaultValue;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.WebUtils;
import com.follett.fsc.core.utils.conf.ConfigUtils;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLExport;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLRelationshipKey;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLSurvey;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationError;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationProcessor;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FLValidationRule;
import com.x2dev.procedures.statereporting.fl.FLExportConfiguration.FL_EXPORT;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.FLRuleSetValidationProcessor;
import com.x2dev.procedures.statereporting.fl.FLValidationRuleSet.ValidationRule;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.X2RuntimeException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Paths;
import java.text.ParseException;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * The Class SetProcedure.
 */
public class SetProcedure extends ProcedureJavaSource {

    /**
     * The Class Fix.
     */
    protected abstract class Fix {
        private ExportFormatRow m_currentRow = null;

        public ExportFormatRow getCurrentRow() {
            return m_currentRow;
        }

        public void setCurrentRow(ExportFormatRow row) {
            m_currentRow = row;
        }

        /**
         * Fix error.
         *
         * @param bean X2BaseBean
         */
        protected abstract void fixError(X2BaseBean bean);
    }

    /**
     * The Class RuleWithFix.
     */
    protected class RuleWithFix {
        Fix m_fix;
        FLValidationProcessor m_processor;
        FLRuleSetValidationProcessor m_ruleSetProcessor;
        ValidationRule m_rule;



        /**
         * Instantiates a new rule with fix.
         *
         * @param rule ValidationRule
         * @param fix Fix
         */
        public RuleWithFix(ValidationRule rule, Fix fix) {
            m_rule = rule;
            m_fix = fix;
        }

        /**
         * Instantiates a new rule with fix.
         *
         * @param processor FLValidationProcessor
         * @param fix Fix
         */
        public RuleWithFix(FLValidationProcessor processor, Fix fix) {
            m_processor = processor;
            m_fix = fix;
        }

        /**
         * Instantiates a new rule with fix.
         *
         * @param processor FLValidationProcessor
         * @param fix Fix
         */
        public RuleWithFix(FLRuleSetValidationProcessor processor, Fix fix) {
            m_ruleSetProcessor = processor;
            m_fix = fix;
        }

        /**
         * Fix.
         *
         * @param bean X2BaseBean
         */
        private void fix(X2BaseBean bean) {
            m_fix.fixError(bean);
        }

        private void setCurrentRow(ExportFormatRow row) {
            m_fix.setCurrentRow(row);
        }

        /**
         * Validation failed.
         *
         * @param row ExportFormatRow
         * @return true, if successful
         */
        private boolean validationFailed(ExportFormatRow row) {
            if (m_rule != null) {
                return !FLValidationRuleSet.VALIDATION_STATUS.OK.equals(
                        m_rule.getValidationResult(m_helper, m_export, row).getStatus());
            } else if (m_processor != null) {
                return !m_processor.getValidationErrors(m_helper, m_export, row).isEmpty();
            } else if (m_ruleSetProcessor != null) {
                return !FLValidationRuleSet.VALIDATION_STATUS.OK.equals(
                        m_ruleSetProcessor.getValidationResult(m_helper, m_export, row).getStatus());
            }
            return false;
        }
    }

    /**
     * Static processing class used to process tool input and generate default elements.
     */
    private static class ToolInputProcessor {
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

    /**
     * The left square bracket ([) prefix character for identifying aliases in bean paths.
     * An Alias must be enclosed in square brackets.
     * <br>
     * [<i>alias</i>]
     */
    private static final String ALIAS_PREFIX_CHAR = "[";

    /**
     * The right square bracket (]) postfix character for identifying aliases in bean paths.
     * An Alias must be enclosed in square brackets.
     * <br>
     * [<i>alias</i>]
     */
    private static final String ALIAS_SUFFIX_CHAR = "]";

    private final static String PACKAGE_NAME = "com.x2dev.procedures.statereporting.fl";

    protected static final String PARAM_SURVEY_PERIOD = "surveyPeriod";

    protected FL_EXPORT m_export = null;

    private ModelBroker m_broker = null;
    private Map<String, List<String>> m_codesForFields = null;

    private Class m_classToFix = null;
    private FLExportConfiguration m_exportHelper = null;
    private ArrayList<FL_EXPORT> m_exportsToInclude = new ArrayList<>();
    private FLExportConfiguration m_helper = null;
    private Collection<StateReportValidationError> m_initErrors = new ArrayList<>();
    private List<ImportExportDefinition> m_exportDefinitionList = new ArrayList<>();
    private Map<String, DataDictionary> m_mapDataDictionary = new HashMap();
    private FLStateReportData m_reportData = null;
    private FLStudentHelper m_studentHelper = null;
    private FLSurvey m_survey;
    private File m_tempFolder = null;

    private UserDataContainer m_userData = null;
    private Map<String, ArrayList<RuleWithFix>> m_validationRules = new HashMap<>();

    /**
     * Adds the fixes by rule number.
     *
     * @param ruleNumber String
     * @param fixes ArrayList<RuleWithFix>
     */
    protected void addFixesByRuleNumber(String ruleNumber, ArrayList<RuleWithFix> fixes) {
        m_validationRules.put(ruleNumber, fixes);
    }

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        fixErrors();
    }

    /**
     * Gets the export.
     *
     * @return fl export
     */
    protected FL_EXPORT getExport() {
        return m_export;
    }

    /**
     * Gets the FL report data.
     *
     * @return FL state report data
     */
    protected FLStateReportData getFLReportData() {
        return m_reportData;
    }

    /**
     * Gets the helper.
     *
     * @return FL export configuration
     */
    protected FLExportConfiguration getHelper() {
        return m_helper;

    }

    /**
     * Gets the model broker.
     *
     * @return Model broker
     */
    protected ModelBroker getModelBroker() {
        return m_broker;
    }

    /**
     * Gets the random code for field.
     *
     * @param field DataDictionaryField
     * @return String
     */
    protected String getRandomCodeForField(DataDictionaryField field) {
        return getRandomCodeForField(field, null);
    }

    /**
     * Gets the random code for field.
     *
     * @param field DataDictionaryField
     * @return String
     */
    protected String getRandomCodeForField(DataDictionaryField field, String pattern) {
        if (m_codesForFields == null) {
            m_codesForFields = new HashMap<>();
        }
        List<String> codesForField = m_codesForFields.get(field.toString());
        if (codesForField == null) {
            codesForField = new ArrayList<String>();
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes();
                for (ReferenceCode code : codes) {
                    if (!code.getDisabledIndicator()) {
                        codesForField.add(code.getCode());
                    }
                }
                m_codesForFields.put(field.getJavaName(), codesForField);
            }
        }

        String randomCode = null;

        if (codesForField != null && codesForField.size() > 0) {
            List<String> filteredCodes = new ArrayList<>();
            if (pattern != null) {
                for (String code : codesForField) {
                    if (code.matches(pattern)) {
                        filteredCodes.add(code);
                    }
                }
                if (filteredCodes.size() == 0) {
                    throw new X2RuntimeException();
                }
            }
            if (pattern == null) {
                int randomCodeIndex = ThreadLocalRandom.current().nextInt(0, codesForField.size());
                randomCode = codesForField.get(randomCodeIndex);
            } else {
                int randomCodeIndex = ThreadLocalRandom.current().nextInt(0, filteredCodes.size());
                randomCode = filteredCodes.get(randomCodeIndex);
            }
        }

        return randomCode;
    }

    /**
     * Gets the random code for field.
     *
     * @param field DataDictionaryField
     * @return String
     */
    protected String getRandomCodeForFieldByPattern(DataDictionaryField field, String pattern) {
        String randomCode = null;
        while (randomCode == null || !randomCode.matches(pattern)) {
            randomCode = getRandomCodeForField(field, pattern);
        }
        return randomCode;
    }

    protected FLStudentHelper getStudentHelper() throws X2BaseException {
        if (m_studentHelper == null) {
            m_studentHelper = new FLStudentHelper(getFLReportData());
        }
        return m_studentHelper;
    }

    protected void importBundle(String fileName) throws Exception {
        File file = getFile(fileName);

        BundledObjectImporter bundledObjectImporter = new BundledObjectImporter(file, m_tempFolder,
                getUser().getOrganization1(), m_broker);
        bundledObjectImporter.importFiles(false);
    }

    protected void includeExports(List<FL_EXPORT> exports) {
        m_exportsToInclude.addAll(exports);
    }

    /**
     * Initialize.
     *
     * @param export FL_EXPORT
     * @param classToFix Class
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    protected void initialize(FL_EXPORT export, Class classToFix) throws X2BaseException {
        m_export = export;
        m_classToFix = classToFix;

        m_broker = new ModelBroker(getPrivilegeSet());

        initializeReportData();

        ReferenceCode surveyPeriodBean = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class,
                (String) getParameter(PARAM_SURVEY_PERIOD));
        String surveyCode = surveyPeriodBean.getCode();

        m_exportHelper = new FLExportConfiguration(getCurrentContext(), m_reportData.getSurveyPeriodCode(), m_broker);

        m_survey = m_exportHelper.getSurveyFromCode(surveyCode);

        m_tempFolder = new File(ConfigUtils.getTempFolder());

        m_helper = new FLExportConfiguration(getCurrentContext(), m_reportData.getSurveyPeriodCode(), m_broker);
    }

    protected void reloadHelper() throws X2BaseException {
        m_studentHelper = new FLStudentHelper(getFLReportData());
    }

    protected void runAndValidateExports() throws Exception {
        m_exportHelper = new FLExportConfiguration(getCurrentContext(), m_reportData.getSurveyPeriodCode(), m_broker);
        executeExport();
        executeValidation();
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
        super.saveState(userData);

        m_userData = userData;
    }

    /**
     * Creates the tool job for the export definition and loads default parameters for the export.
     *
     * @param exportDefinition ImportExportDefinition
     * @return ToolJob
     * @throws Exception exception
     */
    private ToolJob createToolJob(ImportExportDefinition exportDefinition) throws Exception {
        ToolInput exportInput =
                ToolInputProcessor.restoreToolInput(exportDefinition, true, m_userData, getBroker(), getLocale());

        exportInput.setParameterAsString(PARAM_SURVEY_PERIOD, (String) getParameter(PARAM_SURVEY_PERIOD));
        exportInput.setParameterAsString("saveResults", "true");
        exportInput.setParameterAsString("suppressOutput", "true");

        m_userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);
        return ToolJob.createJob(exportDefinition, m_userData, tempFolder, false, getLocale());
    }

    /**
     * Decode bean path.
     *
     * @param table the table
     * @param beanPath the bean path
     * @param dictionary the dictionary
     * @return the string
     */
    private String decodeBeanPath(DataTable table, String beanPath, DataDictionary dictionary) {
        boolean last = false;
        StringBuilder resultPath = new StringBuilder();
        List<String> pathElements =
                StringUtils.convertDelimitedStringToList(beanPath, ModelProperty.PATH_DELIMITER, true);
        for (String element : pathElements) {
            if (last) {
                throw new IllegalStateException(
                        "Bean path parsing error. There can be no elements beyond the first field element in the path.");
            } else if (element.startsWith(ALIAS_PREFIX_CHAR) && element.endsWith(ALIAS_SUFFIX_CHAR)) {
                // Alias lookup.
                String alias = element.substring(1, element.length() - 1);
                DataDictionaryField dataField = dictionary.findDataDictionaryFieldByAlias(alias);
                if (dataField == null) {
                    throw new IllegalStateException(
                            "Bean path parsing error. The alias " + alias + " was not found.");
                } else if (dataField != null && !dataField.getDataTable().equals(table)) {
                    throw new IllegalStateException("Bean path parsing error. The alias " + alias
                            + " was is not on table " + table.getDatabaseName());
                } else if (dataField != null) {
                    // Field element is valid. Add it and mark element as last.
                    if (resultPath.length() > 0) {
                        resultPath.append(ModelProperty.PATH_DELIMITER);
                    }
                    resultPath.append(dataField.getJavaName());
                    last = true;
                }
            } else {
                boolean found = false;
                List<DataDictionaryField> dFields = dictionary.getFieldsForContext(table);
                for (DataDictionaryField dField : dFields) {
                    if (dField.getJavaName().equals(element)) {
                        // Field element is valid. Add it and mark element as last.
                        if (resultPath.length() > 0) {
                            resultPath.append(ModelProperty.PATH_DELIMITER);
                        }
                        resultPath.append(element);
                        last = true;
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    List<DataDictionaryRelationship> dRelations = dictionary.getRelationshipsForContext(table);
                    for (DataDictionaryRelationship ddRelation : dRelations) {
                        if (ddRelation.getRelatedJavaName().equals(element)) {

                            if (!ddRelation.getRelatedRelationType()
                                    .equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                                throw new IllegalStateException(
                                        "Bean path parsing error. The relationship type for "
                                                + element + " is not 1 to 1 or 1 to N.");
                            }
                            // relationship Ok. continue path. set next table in path.
                            if (resultPath.length() > 0) {
                                resultPath.append(ModelProperty.PATH_DELIMITER);
                            }
                            resultPath.append(element);
                            table = ddRelation.getRelatedDataTable();
                            found = true;
                            break;
                        }
                    }
                }
            }
        }
        // Check the path ends in a field, not a relation.
        if (!last) {
            throw new IllegalStateException("Bean path parsing error. The bean path must end in a field element");
        }
        return resultPath.toString();
    }

    /**
     * Decode related table.
     *
     * @param table the table
     * @param relationshipPath the relationship path
     * @param dictionary the dictionary
     * @return the data table
     */
    private DataTable decodeRelatedTable(DataTable table, String relationshipPath, DataDictionary dictionary) {
        List<String> pathElements =
                StringUtils.convertDelimitedStringToList(relationshipPath, ModelProperty.PATH_DELIMITER, true);
        for (String element : pathElements) {
            boolean found = false;
            List<DataDictionaryRelationship> dRelations = dictionary.getRelationshipsForContext(table);
            for (DataDictionaryRelationship ddRelation : dRelations) {
                if (ddRelation.getRelatedJavaName().equals(element)) {

                    if (!ddRelation.getRelatedRelationType().equals(DataDictionaryRelationship.ONE_TYPE_CODE)) {
                        throw new IllegalStateException("Relation path parsing error. The relationship type for "
                                + element + " is not 1 to 1 or 1 to N.");
                    }
                    table = ddRelation.getRelatedDataTable();
                    found = true;
                    break;
                }
            }
            if (!found) {
                throw new IllegalStateException("Relation not found for "
                        + element + " from table " + table.getDatabaseName());
            }
        }
        return table;
    }

    /**
     * Executes the jobs selected.
     *
     * @throws Exception exception
     */
    private void executeExport() throws Exception {
        if (m_survey != null) {
            List<FLExport> exportList = m_survey.getExports();

            List<String> exportIds = new ArrayList<>();

            for (FLExport export : exportList) {
                if (export == m_export || m_exportsToInclude.contains(export)) {
                    exportIds.add(export.getExportId());
                }
            }

            X2Criteria criteria = new X2Criteria();
            criteria.addIn(ImportExportDefinition.COL_ID, exportIds);
            Map<String, ImportExportDefinition> mapExports = getBroker().getMapByQuery(
                    new BeanQuery(ImportExportDefinition.class, criteria), ImportExportDefinition.COL_ID, 30);

            for (String exportId : exportIds) {
                ImportExportDefinition exportDefinition = mapExports.get(exportId);
                if (exportDefinition != null) {
                    m_exportDefinitionList.add(exportDefinition);
                }
            }
        }

        Iterator<ImportExportDefinition> iterator = m_exportDefinitionList.iterator();
        List<KeyValuePair<ImportExportDefinition, ToolJob>> jobsList = new LinkedList();
        while (iterator.hasNext()) {
            ImportExportDefinition exportDefinition = iterator.next();
            ToolJob job = createToolJob(exportDefinition);
            jobsList.add(new KeyValuePair(exportDefinition, job));
        }

        for (KeyValuePair<ImportExportDefinition, ToolJob> item : jobsList) {
            ToolJob job = item.getValue();

            try {
                job.run();
            } finally {
                ResultHandler resultHandler = job.getResultHandler();
                resultHandler.close();
            }
        }
    }

    /**
     * Execute validation.
     *
     * @throws Exception exception
     */
    private void executeValidation() throws Exception {
        m_exportHelper.deleteValidationErrors();

        List<String> outputList = new ArrayList<>();
        if (m_survey != null) {
            for (FLExport export : m_survey.getExports()) {
                if (export == m_export || m_exportsToInclude.contains(export)) {
                    int validationErrorsTotal = 0;
                    List<FLValidationError> exportErrors = new ArrayList<>();
                    outputList.add("Export: " + export.name());
                    Collection<ExportFormatRow> exportFormatRows = m_exportHelper.getExportFormatRows(export);
                    for (ExportFormatRow row : exportFormatRows) {
                        for (FLValidationRule rule : export.getValidationRules()) {
                            List<FLValidationError> errors;
                            try {
                                errors = rule.getProcessor().getValidationErrors(m_exportHelper, export, row);
                            } catch (IllegalStateException e) {
                                StringBuilder message = new StringBuilder();
                                message.append("\n");
                                message.append("Validate Rule processing exception for export ");
                                message.append(export.name());
                                message.append(" rule # ");
                                message.append(rule.getRuleNumber());
                                message.append("\n");
                                message.append(e.getMessage());
                                message.append("\n");
                                throw new IllegalStateException(message.toString());
                            }
                            for (FLValidationError error : errors) {
                                error.setRuleNumber(rule.getRuleNumber());
                            }
                            exportErrors.addAll(errors);
                        }
                    }
                    m_exportHelper.persistValidationErrors(exportErrors);
                    validationErrorsTotal += exportErrors.size();
                    outputList.add("rows: " + exportFormatRows.size());
                    outputList.add("validation errors: " + validationErrorsTotal + "\n");
                }
            }
        }

        for (String comment : outputList) {
            PrintStream stream = new PrintStream(getResultHandler().getOutputStream());
            stream.print(comment + "\n");
        }
    }

    /**
     * Fix errors.
     *
     * @throws ParseException exception
     * @throws X2BaseException exception
     */
    private void fixErrors() throws ParseException, X2BaseException {
        Collection<ExportFormatRow> rows = m_helper.getExportFormatRows(m_export);
        if (rows == null) {
            throw new X2RuntimeException();
        }

        for (FLValidationRule rule : m_export.getValidationRules()) {
            for (ExportFormatRow row : rows) {
                List<FLValidationError> errors = rule.getProcessor().getValidationErrors(m_helper, m_export, row);

                if (errors != null && !errors.isEmpty()) {
                    makeAdjustment(rule, row);
                }
            }
        }
    }

    private String getAspenRootDirectory() {

        // This is temporary code until we have S-58463 completed. We used to need to load the
        // bootstrap dir in order to load files across projects, now we do not but the change is
        // very large right now, I've added a story to deal with this.
        File setProcedureFileLocation =
                new File(this.getClass().getProtectionDomain().getCodeSource().getLocation().getPath());
        // This means we loaded the file via a jar and not on the classpath
        if (setProcedureFileLocation.isFile()) {
            setProcedureFileLocation = setProcedureFileLocation.getParentFile();
        }

        // Jenkins hack: when we run a maven "deploy", it deploys the artifact in the local
        // .repository location. This gets loaded and we get the wrong path for the files. We are
        // only using this "bootstrap directory" to get the parent so we can traverse down to the
        // right location, but now that everything is in the right location we don't need this.
        // Anyway, this .repository code is a hack that allows us to get out of the repo and back to
        // the workspace on jenkins
        String workspaceDirectoryToFind = "aspen-bundles";
        if (setProcedureFileLocation.toString().contains(".repository")) {
            workspaceDirectoryToFind = ".repository";
        }

        File aspenCoreDirectory = getAspenBundleDirectoryParent(setProcedureFileLocation, workspaceDirectoryToFind);
        return Paths.get(aspenCoreDirectory.getParent()).toString();
    }

    private static File getAspenBundleDirectoryParent(File f, String parentToFind) {
        if (!f.exists()) {
            return null;
        }
        if (f.toString().endsWith(parentToFind + "/") || f.toString().endsWith(parentToFind)) {
            return f;
        }
        return getAspenBundleDirectoryParent(f.getParentFile(), parentToFind);
    }

    /**
     * Gets the export format dictionary.
     *
     * @param export the export
     * @return the export format dictionary
     */
    private DataDictionary getExportFormatDictionary(FL_EXPORT export) {
        if (!m_mapDataDictionary.containsKey(export.getCode())) {
            ExportFormatDefinition definition = m_helper.getExportFormatDefinition(export);
            ExtendedDictionaryAttributes extendedDictionary = definition.getExtendedDataDictionary();
            DataDictionary dictionary =
                    DataDictionary.getDistrictDictionary(extendedDictionary, m_broker.getPersistenceKey());
            m_mapDataDictionary.put(export.getCode(), dictionary);
        }
        return m_mapDataDictionary.get(export.getCode());
    }

    /**
     * Gets the file from package.
     *
     * @param fileName String
     * @return File
     * @throws Exception exception
     */
    private File getFile(String fileName) throws Exception {
        return new File(getFileLocationInPackage(fileName));
    }

    /**
     * Gets the file location in package.
     *
     * @param fileName String
     * @param packagePath String
     * @return String
     * @throws Exception exception
     */
    private String getFileLocationInPackage(String fileName) throws Exception {
        String aspenRootDirectory = getAspenRootDirectory();

        String p = Paths
                .get(aspenRootDirectory, "aspen-bundles", "src", "main", "procedures",
                        PACKAGE_NAME.replace(".", File.separator), fileName)
                .toString();

        if (!new File(p).exists()) {
            p = Paths
                    .get(aspenRootDirectory, "aspen-bundles", "src", "main",
                            "procedures.com.x2dev.procedures.sys.shared".replace(".", File.separator), fileName)
                    .toString();
            if (!new File(p).exists()) {
                p = null;
            }
        }
        return p;
    }

    /**
     * Gets the related bean from row.
     *
     * @param beanClass the bean class
     * @param export the export
     * @param row the row
     * @return the related bean from row
     * @throws X2BaseException the x 2 base exception
     */
    private X2BaseBean getRelatedBeanFromRow(Class beanClass, FL_EXPORT export, ExportFormatRow row)
            throws X2BaseException {
        X2BaseBean bean = null;
        FLRelationshipKey relationship = export.getRelationshipKeyByClassName(beanClass.getName());
        if (relationship != null) {
            String value = m_helper.getExportFormatRowFieldValue(row, export, relationship.getFieldName());
            if (!StringUtils.isEmpty(value)) {
                value = value.trim();
                DataDictionary dictionary = getExportFormatDictionary(export);
                DataTable table = dictionary.findDataDictionaryTableByClass(relationship.getQueryClass().getName())
                        .getSystemDataTable();
                String beanPath = decodeBeanPath(table, relationship.getBeanPath(), dictionary);
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(beanPath, value);
                BeanQuery query = new BeanQuery(relationship.getQueryClass(), criteria);
                bean = m_broker.getBeanByQuery(query);
                if (bean != null && !StringUtils.isEmpty(relationship.getRelatingPath())) {
                    table = dictionary.findDataDictionaryTableByClass(bean.getClass().getName())
                            .getSystemDataTable();
                    table = decodeRelatedTable(table, relationship.getRelatingPath(), dictionary);
                    beanPath = relationship.getRelatingPath() + ModelProperty.PATH_DELIMITER + X2BaseBean.COL_OID;
                    if (!beanClass.equals(table.getDataClass())) {
                        throw new IllegalStateException(
                                "Bean path parsing error. The relating path is class " + table.getDataClass().getName()
                                        + " but the expected class is " + beanClass.getName());
                    }
                    criteria = new X2Criteria();
                    value = (String) WebUtils.getProperty(bean, beanPath);
                    criteria.addEqualTo(beanPath, value);
                    query = new BeanQuery(beanClass, criteria);
                    bean = m_broker.getBeanByQuery(query);
                }
            }
        }
        return bean;
    }

    /**
     * Initialize report data.
     *
     * @throws X2BaseException exception
     */
    private void initializeReportData() throws X2BaseException {
        // Lookup State report source data procedure
        m_reportData =
                (FLStateReportData) StateReportData.getReportDataFromProcedure(m_export.getProcedureId(), getBroker(),
                        m_initErrors);
        if (m_reportData != null && m_initErrors.size() == 0) {
            try {
                // Initialize the report data object.
                m_reportData.setBroker(getBroker());
                m_reportData.setCurrentContext(getCurrentContext());
                m_reportData.setOrganization(getOrganization());
                m_reportData.setPrivilegeSet(getPrivilegeSet());
                m_reportData.setSchoolContext(isSchoolContext());
                m_reportData.setSchool(getSchool());
                m_reportData.setParameters(getParameters());
                m_reportData.setUser(getUser());
                m_reportData.initializeExport();
            } catch (X2BaseException x2be) {
                throw x2be;
            }

            m_initErrors.addAll(m_reportData.getSetupErrors());
        }
    }


    /**
     * Make adjustment.
     *
     * @param rule FLValidationRule
     * @param row ExportFormatRow
     * @throws X2BaseException exception
     */
    private void makeAdjustment(FLValidationRule rule, ExportFormatRow row) throws X2BaseException {
        Collection<RuleWithFix> rules = m_validationRules.get(rule.getRuleNumber());
        if (rules != null) {
            for (RuleWithFix ruleWithFix : rules) {
                if (ruleWithFix.validationFailed(row)) {
                    ruleWithFix.setCurrentRow(row);
                    ruleWithFix.fix(getRelatedBeanFromRow(m_classToFix, m_export, row));
                }
            }
        }
    }
}
