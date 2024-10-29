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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.DefaultValue;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValueTrio;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * The Class TNRestagingProcedure.
 *
 * @author Follett Software Company
 */
public class TNRestagingProcedure extends ProcedureJavaSource {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * Fields used in export operations.
     */
    public enum EXPORTS_FIELDS {
        ROW_010("EXP-TN-CTX", "TN 010", TAB_DISTR_CAL),
        // Comment to maintain format
        ROW_011("EXP-TN-CAL", "TN 011", TAB_DISTR_CAL),
        //
        ROW_016("EXP-TN-BST", "TN 016", TAB_BST),
        //
        ROW_015("EXP-TN-BUS", "TN 015", TAB_BUS),
        //
        ROW_020("EXP-TN-CAS", "TN 020", TAB_SKL_CAL),
        //
        ROW_021("EXP-TN-CASP", "TN 021", TAB_SKL_CAL),
        //
        ROW_022("EXP-TN-CSD", "TN 022", TAB_SKL_CAL),
        //
        ROW_030("EXP-TN-MST", "TN 030", TAB_CRS_MST),
        //
        ROW_031("EXP-TN-MSTS", "TN 031", TAB_CRS_MST),
        //
        ROW_040("EXP-TN-STD", "TN 040", TAB_STD),
        //
        ROW_041("EXP-TN-ENR", "TN 041", TAB_STD),
        //
        ROW_042("EXP-TN-PGMI", "TN 042", TAB_STD),
        //
        ROW_043("EXP-TN-STDG", "TN 043", TAB_STD),
        //
        ROW_044("EXP-TN-PGMC", "TN 044", TAB_STD_PGM),
        //
        ROW_045("EXP-TN-STR" + STRING_TAB_ID + TAB_STD, "TN 045", TAB_STD),
        //
        ROW_045_TAB_BUS("EXP-TN-STR" + STRING_TAB_ID + TAB_STD_BUS, "TN 045", TAB_STD_BUS),
        //
        ROW_046("EXP-TN-CND", "TN 046", TAB_STD),
        //
        ROW_047("EXP-TN-STDD", "TN 047", TAB_STD),
        //
        ROW_048("EXP-TN-SSC" + STRING_TAB_ID + TAB_STD, "TN 048", TAB_STD),
        //
        ROW_048_TAB_CRS_MST("EXP-TN-SSC" + STRING_TAB_ID + TAB_STD_MST, "TN 048", TAB_STD_MST),
        //
        ROW_049("EXP-TN-ATT", "TN 049", TAB_STD),
        //
        ROW_050("EXP-TN-ENRW", "TN 050", TAB_STD),
        //
        ROW_051("EXP-TN-STDE", "TN 051", TAB_STD),
        //
        ROW_052("EXP-TN-PGMM", "TN 052", TAB_STD),
        //
        ROW_080("EXP-TN-TRN", "TN 080", TAB_STD),
        //
        ROW_081("EXP-TN-TAI", "TN 081", TAB_STD),
        //
        ROW_082("EXP-TN-TAS", "TN 082", TAB_STD),
        //
        ROW_060("EXP-TN-STF", "TN 060", TAB_STF),
        //
        ROW_062("EXP-TN-SFP", "TN 062", TAB_STF),
        //
        ROW_063("EXP-TN-MTC" + STRING_TAB_ID + TAB_STF, "TN 063", TAB_STF),
        //
        ROW_063_TAB_CRS_MST("EXP-TN-MTC" + STRING_TAB_ID + TAB_STAFF_MST, "TN 063", TAB_STAFF_MST);

        private String m_exportId;
        private String m_exportStartName;
        private String m_tab;

        /**
         * Gets the id.
         *
         * @return String
         */
        public String getID() {
            return m_exportId;
        }

        /**
         * Gets the efr name.
         *
         * @return String
         */
        public String getEfrName() {
            return m_exportStartName;
        }

        /**
         * Gets the tab name.
         *
         * @return String
         */
        public String getTabName() {
            return m_tab;
        }

        /**
         * Instantiates a new exports fields.
         *
         * @param exportId String
         * @param exportStartName String
         * @param tab String
         */
        private EXPORTS_FIELDS(String exportId, String exportStartName, String tab) {
            m_exportId = exportId;
            m_exportStartName = exportStartName;
            m_tab = tab;
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
         * @param locale Locale
         * @return a fully populated ToolInput from the data in JobEntry.
         * @throws X2BaseException exception
         */
        public static ToolInput restoreToolInput(Tool tool,
                                                 boolean inputDefaults,
                                                 UserDataContainer userData,
                                                 Locale locale)
                throws X2BaseException {
            ToolInput input = null;
            if (tool != null) {
                SAXBuilder builder = new SAXBuilder();

                // Parameter values defined in the Tool input.xml
                // Make a list of all params and params with default values if the
                // inputDefaults flag is set.
                Set<String> allParams = new HashSet<String>();
                Map<String, DefaultValue> defaultParams = new HashMap<String, DefaultValue>();
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
                        Object resValue = dv.resolve(userData, locale);
                        String resString = null;
                        if (resValue instanceof String) {
                            resString = (String) resValue;
                        } else {
                            Converter someConverter =
                                    ConverterFactory.getConverterForClass(resValue.getClass().getName(),
                                            locale);
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
     * Aliases
     */
    private static final String ALIAS_SKL_CAL_INSTR_PGM_NUM = "DOE INSTRUCTIONAL PROGRAM";
    private static final String ALIAS_SKL_CAL_NUM = "DOE CALENDAR NUMBER";
    private static final String ALIAS_SKL_STATE_ID = "DOE SCHOOL STATE ID";
    private static final String ALIAS_STD_STATE_ID = "DOE EIS STATE ID";

    /*
     * Other constants
     */
    private static final String EMPTY_STRING = "";
    private static final String FIELD_RECORD_ID = "RECORD ID";
    private static final String FIELD_RECORD_TYPE = "RECORD TYPE";
    private static final String NONE_SELECTED = "none";
    private static final String IDS_DELIMITER = ";";
    private static final String PARAM_DATE_START = "startDate";
    private static final String PARAM_DELETE_ROWS_OIDS = "deleteRowsOids";
    private static final String PARAM_SUPPRESS_HEADING = "suppressHeading";
    private static final String PARAM_SUPPRESS_TRAILER = "suppressTrailer";
    private static final String RECORD_ALL = "All";
    private static final String RECORD_DELETE = "D";
    private static final String RECORD_GRAND_TOTAL_KEY = "Grand Total";
    private static final String RECORD_NEW = "N";
    private static final String STRING_TAB_ID = "_tabId:";

    private static final String REGEX_EFR_NAME = "TN %%% %%% Uploads for %%%%";

    /**
     * Input params
     */
    private static final String INPUT_PARAM_BUS = "buses";
    private static final String INPUT_PARAM_BUS_ALL = "allBuses";
    private static final String INPUT_PARAM_CAL_NUMS = "calendarNumbers";
    private static final String INPUT_PARAM_CLASSIFICATIONS = "classifications";
    private static final String INPUT_PARAM_EXCL_041_DELETES = "exclude041Deletes";
    private static final String INPUT_PARAM_CRS_SKL_OID = "schoolOidCourse";
    private static final String INPUT_PARAM_FROM_TAB = "fromTab";
    private static final String INPUT_PARAM_RESTRICT_CLASS = "restrictClassific";
    private static final String INPUT_PARAM_SCHOOL_CALS = "schoolCalendars";
    private static final String INPUT_PARAM_SECTIONS = "courseSections";
    private static final String INPUT_PARAM_STD_ALL = "##all";
    private static final String INPUT_PARAM_STD_LOCAL_ID = "localId";
    private static final String INPUT_PARAM_STD_SNAPSHOT = "##snapshot";
    private static final String INPUT_PARAM_STD_STATE_ID = "stateId";
    private static final String INPUT_PARAM_STD_QUERY_BY = "queryBy1";
    private static final String INPUT_PARAM_STD_QUERY_STRING = "queryString1";
    private static final String INPUT_PARAM_STF_OIDS = "staffsOids";
    private static final String INPUT_PARAM_STF_SKL_OID = "schoolOidStaff";

    /**
     * Keys' names.
     */
    private static final String KEY_NAME_AM_BUS_NUMBER = "AM BUS NUMBER";
    private static final String KEY_NAME_CAL_NUM = "CALENDAR NUMBER";
    private static final String KEY_NAME_CLASS_NUM = "LOCAL CLASS NUM";
    private static final String KEY_NAME_CLASS_NUMBER = "LOCAL CLASS NUMBER";
    private static final String KEY_NAME_CLASSIFICATION = "CLASSIFICATION";
    private static final String KEY_NAME_DISTR_ID = "DISTRICT ID";
    private static final String KEY_NAME_INSTR_PGM_NUM = "INSTR PROGRAM NUM";
    private static final String KEY_NAME_PM_BUS_NUMBER = "PM BUS NUMBER";
    private static final String KEY_NAME_SKL_ID = "SCHOOL ID";
    private static final String KEY_NAME_SKL_YEAR = "SCHOOL YEAR";
    private static final String KEY_NAME_STD_LOCAL_ID = "LOCAL STUDENT KEY";
    private static final String KEY_NAME_STF_ID = "LOCAL STAFF KEY";

    private static final String RESTAGING_EXPORT_ID = "EXP-TN-EIS";
    /**
     * Input Tabs
     */
    public static final String TAB_BST = "tabBst";
    public static final String TAB_BUS = "tabBus";
    public static final String TAB_CRS_MST = "tabCrsMst";
    public static final String TAB_DISTR_CAL = "tabDistr";
    public static final String TAB_STD = "tabStd";
    public static final String TAB_STF = "tabStf";
    public static final String TAB_SKL_CAL = "tabSklCal";

    /**
     * Additional Tabs
     */
    public static final String TAB_STAFF_MST = "tabStaffMst";
    public static final String TAB_STD_BUS = "tabStdBus";
    public static final String TAB_STD_PGM = "tabStdPgm";
    public static final String TAB_STD_MST = "tabStdMst";

    /*
     * Class members
     */
    private String m_contextOid;
    private DistrictSchoolYearContext m_currentContext;
    private PlainDate m_dateStart;
    private Map<String, Set<String>> m_deleteRowsOis;
    private Map<String, LinkedList<ExportFormatResult>> m_efrMap;
    private ArrayList<EXPORTS_FIELDS> m_efrNameByParams;
    private Collection<ImportExportDefinition> m_definitions;
    private Set<String> m_fromTab;
    private Map<String, Map<String, Integer>> m_prcMessage;
    private String m_recordId;
    private Boolean m_restrictTN044;
    private Map<String, Object> m_inputMap;
    private Collection<String> m_inputTabs;
    private HashMap<String, Set<String>> m_resultsMap;
    private ArrayList<String> m_tabsToUse;
    private UserDataContainer m_userData;

    /**
     * Gets the current context.
     *
     * @return District school year context
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCurrentContext()
     */
    @Override
    public DistrictSchoolYearContext getCurrentContext() {
        if (m_currentContext == null) {
            if (m_contextOid != null) {
                m_currentContext = (DistrictSchoolYearContext) getBroker().getBeanByOid(DistrictSchoolYearContext.class,
                        m_contextOid);
            } else {
                m_currentContext = super.getCurrentContext();
                m_contextOid = m_currentContext.getOid();
            }
        }
        return m_currentContext;
    }

    /**
     * 1)For each add record found that matches the selection, a delete record creates.
     * The results will be written to a new Export Format Results comment "Restaging".
     * 2)Executes the jobs selected.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        m_deleteRowsOis = new HashMap<String, Set<String>>();

        if (inputParametersOK()) {
            initializeAllEntities();

            performToolJobs(m_definitions);

            printOutputMessage();
        }

    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        m_inputTabs = new ArrayList<String>();
        m_inputTabs.add(TAB_BST);
        m_inputTabs.add(TAB_BUS);
        m_inputTabs.add(TAB_CRS_MST);
        m_inputTabs.add(TAB_DISTR_CAL);
        m_inputTabs.add(TAB_SKL_CAL);
        m_inputTabs.add(TAB_STD);
        m_inputTabs.add(TAB_STF);

        m_inputMap = getJob().getInput().generateInputMap();
        m_contextOid = (String) getParameter(ToolInput.CONTEXT_OID_PARAM);
        m_prcMessage = new HashMap<String, Map<String, Integer>>();
        m_dateStart = (PlainDate) getParameter(PARAM_DATE_START);
        if (m_dateStart == null) {
            m_dateStart = getCurrentContext().getStartDate();
        }
        m_restrictTN044 = (Boolean) getParameter(INPUT_PARAM_RESTRICT_CLASS);
        initializeFinalResultOids();
        initializeDefinitions();
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

        m_userData = userData;
    }

    /**
     * Adds the tab.
     *
     * @param field EXPORTS_FIELDS
     */
    private void addTab(EXPORTS_FIELDS field) {
        String tabId = field.getID().replaceAll(getDefinitionId(field) + STRING_TAB_ID, EMPTY_STRING);
        if (m_fromTab == null) {
            m_fromTab = new HashSet<String>();
        }
        if (m_inputTabs.contains(tabId)) {
            m_fromTab.add(field.getID());
        } else {
            m_fromTab.add(tabId);
        }
    }

    /**
     * Combines the input definition from the restaging input and the
     * export.
     *
     * @param toolInputDefinition String
     * @param exportCode ToolSourceCode
     * @return String
     */
    private String combineInputDefinition(String toolInputDefinition, ToolSourceCode exportCode) {
        String output = null;
        SAXBuilder builder = new SAXBuilder();
        XMLOutputter outputter = new XMLOutputter();

        Document mainInputDefinitionXML = null;
        Document inputDefinitionXML = null;

        try {
            mainInputDefinitionXML = builder.build(new ByteArrayInputStream(toolInputDefinition.getBytes()));
            inputDefinitionXML = builder.build(new ByteArrayInputStream(exportCode.getInputDefinition().getBytes()));
        } catch (JDOMException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        if (inputDefinitionXML != null && mainInputDefinitionXML != null) {
            Element rootElement = inputDefinitionXML.getRootElement();
            Collection<Element> mainElements = mainInputDefinitionXML.getRootElement().getChildren();

            for (Element element : mainElements) {
                Element newElement = (Element) element.clone();
                newElement.detach();
                rootElement.addContent(newElement);
            }

            Format format = Format.getPrettyFormat();
            format.setEncoding("UTF-8");
            outputter.setFormat(format);

            output = outputter.outputString(inputDefinitionXML);
        }
        return output;
    }

    /**
     * Creates the tool job for the export definition and loads default parameters for the export.
     *
     * @param exportDefinition ImportExportDefinition
     * @param includeHeading boolean
     * @param includeTrailer boolean
     * @return ToolJob
     * @throws Exception exception
     */
    private ToolJob createToolJob(ImportExportDefinition exportDefinition,
                                  boolean includeHeading,
                                  boolean includeTrailer)
            throws Exception {
        ToolInput exportInput = ToolInputProcessor.restoreToolInput(exportDefinition, true, m_userData, getLocale());

        // Set values to the export parameters from input of the procedure.
        Set<Entry<String, Object>> entrySet = m_inputMap.entrySet();
        for (Entry entry : entrySet) {
            exportInput.setParameterAsString((String) entry.getKey(), String.valueOf(entry.getValue()));
        }

        // Parameters will be corrected if this export is data that should be restaged with data
        // from other export.
        setParameters(exportInput, exportDefinition);

        if (!includeHeading) {
            exportInput.setParameterAsString(PARAM_SUPPRESS_HEADING, "true");
        }
        if (!includeTrailer) {
            exportInput.setParameterAsString(PARAM_SUPPRESS_TRAILER, "true");
        }

        m_userData.setToolInput(exportInput);

        File tempFolderRoot = AppGlobals.getRootTemporaryFolder();
        File tempFolder = FolderUtils.createUniqueFolder(tempFolderRoot);

        return ToolJob.createJob(exportDefinition, m_userData, tempFolder, false, getLocale());
    }

    /**
     * Gets the definition id.
     *
     * @param field EXPORTS_FIELDS
     * @return String
     */
    private String getDefinitionId(EXPORTS_FIELDS field) {

        String definitionId = field.getID();
        if (definitionId.contains(STRING_TAB_ID)) {
            int fromIndex = definitionId.indexOf(STRING_TAB_ID) + STRING_TAB_ID.length();
            String tabId = definitionId.substring(fromIndex);

            definitionId = definitionId.replaceAll(STRING_TAB_ID + tabId, EMPTY_STRING);
        }

        return definitionId;
    }

    /**
     * Get all ExportFromatRow of the given ExportFromatResult oid.
     *
     * @param resultOid String
     * @param fieldsToQuery ArrayList<HashMap<String,String>>
     * @param keysToPut HashMap<String,ArrayList<String>>
     * @return list of ExportFormatRow
     */
    private ArrayList<ExportFormatRow> getEFRowsResultOid(String resultOid,
                                                          ArrayList<HashMap<String, String>> fieldsToQuery,
                                                          HashMap<String, ArrayList<String>> keysToPut) {
        ArrayList<ExportFormatRow> rowsByEFR = new ArrayList<ExportFormatRow>();
        if (!StringUtils.isEmpty(resultOid)) {
            X2Criteria efwCriteria = new X2Criteria();
            efwCriteria.addEqualTo(ExportFormatRow.COL_RESULT_OID, resultOid);
            for (HashMap<String, String> mapToIterate : fieldsToQuery) {
                for (Map.Entry<String, String> entry : mapToIterate.entrySet()) {
                    String fieldKey = entry.getKey();
                    String fieldJavaName = entry.getValue();
                    ArrayList<String> keyValues = keysToPut.get(fieldKey);
                    if (keyValues != null && !keyValues.isEmpty()) {
                        X2Criteria efwAndCriteria = new X2Criteria();
                        efwAndCriteria.addIn(fieldJavaName, keyValues);
                        efwCriteria.addAndCriteria(efwAndCriteria);
                    }
                }
            }

            QueryByCriteria efwQuery = new QueryByCriteria(ExportFormatRow.class, efwCriteria);
            rowsByEFR.addAll(getBroker().getCollectionByQuery(efwQuery));
        }

        return rowsByEFR;
    }

    /**
     * Return Map of java names of ExportFormat fields with effKeyInd = 1 keyed on java name of
     * RECORD TYPE field.
     *
     * @param fields ArrayList<ExportFormatField>
     * @return Hash map
     */
    private HashMap<String, ArrayList<HashMap<String, String>>> getMapOfKeyIndexes(ArrayList<ExportFormatField> fields) {
        String javaName = null;
        DataFieldConfig fdd = null;
        HashMap<String, ArrayList<HashMap<String, String>>> fieldsMap =
                new HashMap<String, ArrayList<HashMap<String, String>>>();
        ArrayList<HashMap<String, String>> keyInds = new ArrayList<HashMap<String, String>>();
        if (fields != null) {
            Collections.sort(fields, new Comparator<ExportFormatField>() {
                @Override
                public int compare(ExportFormatField eff1, ExportFormatField eff2) {

                    return eff1.getPosition() - eff2.getPosition();
                }
            });

            for (ExportFormatField field : fields) {
                if (field.getKeyInd()) {
                    HashMap<String, String> mapToPut = new HashMap<String, String>();
                    if (field.getDataFieldConfig() != null && field.getDataFieldConfig().getDataField() != null) {
                        mapToPut.put(field.getName(), field.getDataFieldConfig().getDataField().getJavaName());
                        keyInds.add(mapToPut);
                    }
                }

                if (FIELD_RECORD_TYPE.equalsIgnoreCase(field.getName())) {
                    fdd = field.getDataFieldConfig();
                }

                if (FIELD_RECORD_ID.equalsIgnoreCase(field.getName())) {
                    if (field.getDataFieldConfig() != null && field.getDataFieldConfig().getDataField() != null) {
                        m_recordId = field.getDataFieldConfig().getDataField().getJavaName();
                    }
                }

            }
        }

        if (fdd != null && fdd.getDataField() != null) {
            javaName = fdd.getDataField().getJavaName();
            fieldsMap.put(javaName, keyInds);
        }

        return fieldsMap;
    }

    /**
     * Populate STD keys by selected BUS entities.
     * Anytime a bus is restaged, the student transportation records for that bus need to be
     * restaged as well.
     */
    private void init045FromBusEntities() {
        String[] busNumbersCodeOids =
                m_inputMap.get(INPUT_PARAM_BUS) != null ? ((String) m_inputMap.get(INPUT_PARAM_BUS)).split(",") : null;
        Boolean allBusses = (Boolean) getParameter(INPUT_PARAM_BUS_ALL);

        HashMap<String, ArrayList<String>> keyMapAm = new HashMap<String, ArrayList<String>>();
        HashMap<String, ArrayList<String>> keyMapPm = new HashMap<String, ArrayList<String>>();

        if (allBusses.booleanValue()) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STD_BUS)) {
                keyMapAm.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                keyMapAm.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());

                ArrayList<String> distrIds = keyMapAm.get(KEY_NAME_DISTR_ID);
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMapAm.get(KEY_NAME_SKL_YEAR);
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());
            }
        } else if (!allBusses.booleanValue() && busNumbersCodeOids != null && busNumbersCodeOids.length > 0
                && !StringUtils.isEmpty(busNumbersCodeOids[0])) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STD_BUS)) {
                keyMapAm.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                keyMapAm.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                keyMapAm.put(KEY_NAME_AM_BUS_NUMBER, new ArrayList<String>());

                keyMapPm.put(KEY_NAME_DISTR_ID, keyMapAm.get(KEY_NAME_DISTR_ID));
                keyMapPm.put(KEY_NAME_SKL_YEAR, keyMapAm.get(KEY_NAME_SKL_YEAR));
                keyMapPm.put(KEY_NAME_PM_BUS_NUMBER, keyMapAm.get(KEY_NAME_AM_BUS_NUMBER));

                ArrayList<String> distrIds = keyMapAm.get(KEY_NAME_DISTR_ID);
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMapAm.get(KEY_NAME_SKL_YEAR);
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                X2Criteria criteria = new X2Criteria();
                criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(busNumbersCodeOids));
                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
                Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

                ArrayList<String> busNumbsAm = keyMapAm.get(KEY_NAME_AM_BUS_NUMBER);
                for (ReferenceCode code : codes) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        busNumbsAm.add(code.getStateCode());
                    }
                }
            }
            // Create "D" records in cases when there are history records with selected bus numbers
            // in AM BUS NUMBER or/and
            // PM BUS NUMBER fields.
            if (!keyMapAm.isEmpty()) {
                iterateDeletes(TAB_STD_BUS, keyMapAm);
            }
            if (!keyMapPm.isEmpty()) {
                iterateDeletes(TAB_STD_BUS, keyMapPm);
            }
        } else {
            m_fromTab.remove(EXPORTS_FIELDS.ROW_045_TAB_BUS.getTabName());

            // We can't just delete all tab, because may exist some definitions that are still
            // needed for other tabs.
            ArrayList<String> savedDefinitionsIds = new ArrayList<String>();
            if (m_fromTab.contains(EXPORTS_FIELDS.ROW_045.getID())) {
                String definitionId = getDefinitionId(EXPORTS_FIELDS.ROW_045);
                savedDefinitionsIds.add(definitionId);
            }

            removeTab(TAB_STD_BUS, savedDefinitionsIds);
        }
    }

    /**
     * Populate keys of STD by selected CRS entities.
     */
    private void init048FromCrsEntities() {
        String[] schoolOids = m_inputMap.get(INPUT_PARAM_CRS_SKL_OID) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_CRS_SKL_OID)).split(",")
                : null;
        String[] cskOids = m_inputMap.get(INPUT_PARAM_SECTIONS) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_SECTIONS)).split(",")
                : null;
        Boolean wholeSchool = Boolean.FALSE;
        if (cskOids == null) {
            wholeSchool = Boolean.TRUE;
        }
        if (schoolOids != null && schoolOids.length > 0 && !StringUtils.isEmpty(schoolOids[0])) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STD_MST)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                if (wholeSchool.booleanValue()) {
                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);
                    for (String schoolOid : schoolOids) {
                        SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                        String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                        sklIds.add(sklStateId);
                    }

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());
                } else if (!wholeSchool.booleanValue() && cskOids != null) {
                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                    keyMap.put(KEY_NAME_CLASS_NUMBER, new ArrayList<String>());

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);

                    for (String schoolOid : schoolOids) {
                        SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                        String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                        sklIds.add(sklStateId);
                    }

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                    ArrayList<String> crsCodes = keyMap.get(KEY_NAME_CLASS_NUMBER);

                    for (String cskOid : cskOids) {
                        MasterSchedule mst = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, cskOid);
                        String localClassNum = EMPTY_STRING;
                        if (mst != null) {
                            localClassNum = mst.getCourseView();
                            if (!StringUtils.isEmpty(localClassNum) && !crsCodes.contains(localClassNum)) {
                                crsCodes.add(localClassNum);
                            }
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_STD_MST, keyMap);
                }
            }
        } else {
            m_fromTab.remove(EXPORTS_FIELDS.ROW_048_TAB_CRS_MST.getTabName());

            // We can't just delete all tab, because may exist some definitions that are still
            // needed for other tabs.
            ArrayList<String> savedDefinitionsIds = new ArrayList<String>();
            if (m_fromTab.contains(EXPORTS_FIELDS.ROW_048.getID())) {
                String definitionId = getDefinitionId(EXPORTS_FIELDS.ROW_048);
                savedDefinitionsIds.add(definitionId);
            }

            removeTab(TAB_STD_MST, savedDefinitionsIds);
        }
    }

    /**
     * Populate keys of STD by selected CRS entities.
     */
    private void init063FromCrsEntities() {
        String[] schoolOids = m_inputMap.get(INPUT_PARAM_CRS_SKL_OID) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_CRS_SKL_OID)).split(",")
                : null;
        String[] cskOids = m_inputMap.get(INPUT_PARAM_SECTIONS) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_SECTIONS)).split(",")
                : null;
        Boolean wholeSchool = Boolean.FALSE;
        if (cskOids == null) {
            wholeSchool = Boolean.TRUE;
        }
        if (schoolOids != null && schoolOids.length > 0 && !StringUtils.isEmpty(schoolOids[0])) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STAFF_MST)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                if (wholeSchool.booleanValue()) {
                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);
                    for (String schoolOid : schoolOids) {
                        SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                        String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                        sklIds.add(sklStateId);
                    }

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());
                } else if (!wholeSchool.booleanValue() && cskOids != null) {
                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                    keyMap.put(KEY_NAME_CLASS_NUM, new ArrayList<String>());

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);

                    for (String schoolOid : schoolOids) {
                        SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                        String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                        sklIds.add(sklStateId);
                    }

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                    ArrayList<String> crsCodes = keyMap.get(KEY_NAME_CLASS_NUM);

                    for (String cskOid : cskOids) {
                        MasterSchedule mst = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, cskOid);
                        String localClassNum = EMPTY_STRING;
                        if (mst != null) {
                            localClassNum = mst.getCourseView();
                            if (!StringUtils.isEmpty(localClassNum) && !crsCodes.contains(localClassNum)) {
                                crsCodes.add(localClassNum);
                            }
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_STAFF_MST, keyMap);
                }
            }
        } else {
            m_fromTab.remove(EXPORTS_FIELDS.ROW_063_TAB_CRS_MST.getTabName());

            // We can't just delete all tab, because may exist some definitions that are still
            // needed for other tabs.
            ArrayList<String> savedDefinitionsIds = new ArrayList<String>();
            if (m_fromTab.contains(EXPORTS_FIELDS.ROW_063.getID())) {
                String definitionId = getDefinitionId(EXPORTS_FIELDS.ROW_063);
                savedDefinitionsIds.add(definitionId);
            }

            removeTab(TAB_STAFF_MST, savedDefinitionsIds);
        }
    }

    /**
     * Populate keys of all selected entities.
     */
    private void initializeAllEntities() {
        if (!m_efrNameByParams.isEmpty()) {
            initializeDistrCalendarEntities();
            initializeBstEntities();
            initializeBusEntities();
            initializeSklCalendarEntities();
            initializeCrsSectionEntities();
            initializeStaffEntities();
            initializeStdEntities();
            initializeStdPgmEntities();
            if (m_fromTab != null && m_fromTab.contains(EXPORTS_FIELDS.ROW_045_TAB_BUS.getTabName())) {
                init045FromBusEntities();
            }

            if (m_fromTab != null && m_fromTab.contains(EXPORTS_FIELDS.ROW_048_TAB_CRS_MST.getTabName())) {
                init048FromCrsEntities();
            }

            if (m_fromTab != null && m_fromTab.contains(EXPORTS_FIELDS.ROW_063_TAB_CRS_MST.getTabName())) {
                init063FromCrsEntities();
            }
        }
    }

    /**
     * Populate keys of BST entities.
     */
    private void initializeBstEntities() {
        String busOids = (String) m_inputMap.get(INPUT_PARAM_BUS);
        Boolean allBusses = (Boolean) getParameter(INPUT_PARAM_BUS_ALL);

        HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

        if (allBusses.booleanValue() || !StringUtils.isEmpty(busOids)) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_BST)) {
                keyMap.put("District ID", new ArrayList<String>());
                keyMap.put("School Year", new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get("District ID");
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get("School Year");
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());
            }
        } else {
            removeTab(TAB_BST, null);
        }

        if (!keyMap.isEmpty()) {
            iterateDeletes(TAB_BST, keyMap);
        }
    }

    /**
     * Populate keys of the selected BUS entities.
     */
    private void initializeBusEntities() {
        String[] busNumbersCodeOids =
                m_inputMap.get(INPUT_PARAM_BUS) != null ? ((String) m_inputMap.get(INPUT_PARAM_BUS)).split(",") : null;
        Boolean allBusses = (Boolean) getParameter(INPUT_PARAM_BUS_ALL);

        HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

        if (allBusses.booleanValue()) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_BUS)) {
                keyMap.put("District ID", new ArrayList<String>());
                keyMap.put("School Year", new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get("District ID");
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get("School Year");
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());
            }
        } else if (!allBusses.booleanValue() && busNumbersCodeOids != null && busNumbersCodeOids.length > 0
                && !StringUtils.isEmpty(busNumbersCodeOids[0])) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_BUS)) {
                keyMap.put("District ID", new ArrayList<String>());
                keyMap.put("School Year", new ArrayList<String>());
                keyMap.put("Bus Number", new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get("District ID");
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get("School Year");
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                X2Criteria criteria = new X2Criteria();
                criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(busNumbersCodeOids));
                QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
                Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

                ArrayList<String> busNumbs = keyMap.get("Bus Number");
                for (ReferenceCode code : codes) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        busNumbs.add(code.getStateCode());
                    }
                }
            }
        } else {
            removeTab(TAB_BUS, null);
        }

        if (!keyMap.isEmpty()) {
            iterateDeletes(TAB_BUS, keyMap);
        }
    }

    /**
     * Populate keys of the selected CRS entities.
     */
    private void initializeCrsSectionEntities() {
        String[] schoolOids = m_inputMap.get(INPUT_PARAM_CRS_SKL_OID) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_CRS_SKL_OID)).split(",")
                : null;
        String[] cskOids = m_inputMap.get(INPUT_PARAM_SECTIONS) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_SECTIONS)).split(",")
                : null;
        Boolean wholeSchool = Boolean.FALSE;
        if (cskOids == null) {
            wholeSchool = Boolean.TRUE;
        }
        if (schoolOids != null && schoolOids.length > 0 && !StringUtils.isEmpty(schoolOids[0])) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_CRS_MST)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                if (wholeSchool.booleanValue()) {

                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);
                    for (String schoolOid : schoolOids) {
                        SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                        String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                        sklIds.add(sklStateId);
                    }

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());
                } else if (!wholeSchool.booleanValue() && cskOids != null) {
                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                    keyMap.put(KEY_NAME_CLASS_NUM, new ArrayList<String>());

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);

                    for (String schoolOid : schoolOids) {
                        SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                        String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);
                        sklIds.add(sklStateId);
                    }

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                    ArrayList<String> crsCodes = keyMap.get(KEY_NAME_CLASS_NUM);

                    for (String cskOid : cskOids) {
                        MasterSchedule mst = (MasterSchedule) getBroker().getBeanByOid(MasterSchedule.class, cskOid);
                        String localClassNum = EMPTY_STRING;
                        if (mst != null) {
                            localClassNum = mst.getCourseView();
                            if (!StringUtils.isEmpty(localClassNum) && !crsCodes.contains(localClassNum)) {
                                crsCodes.add(localClassNum);
                            }
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_CRS_MST, keyMap);
                }
            }
        } else {
            removeTab(TAB_CRS_MST, null);
        }
    }

    /**
     * Initialize selected definitions.
     */
    private void initializeDefinitions() {
        Set<String> definitionsIds = new HashSet<String>();

        for (EXPORTS_FIELDS fields : EXPORTS_FIELDS.values()) {
            if ((Boolean) getParameter(fields.getID()) == Boolean.TRUE) {
                // Some export can be run from several tabs with different entities of selection, so
                // some IDs of fields
                // can include information about ImportExportDefinition that should be used and from
                // which tabs
                // should be used selection for this export.
                String definitionId = getDefinitionId(fields);
                addTab(fields);
                definitionsIds.add(definitionId);
            }
        }

        X2Criteria criteria = new X2Criteria();
        criteria.addIn(ImportExportDefinition.COL_ID, definitionsIds);
        QueryByCriteria query = new QueryByCriteria(ImportExportDefinition.class, criteria);
        query.addOrderByAscending(ImportExportDefinition.COL_NAME);

        m_definitions = getBroker().getCollectionByQuery(query);
    }

    /**
     * Populate keys of the selected CAL entities.
     */
    private void initializeDistrCalendarEntities() {
        String[] calNumbers = m_inputMap.get(INPUT_PARAM_CAL_NUMS) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_CAL_NUMS)).split(",")
                : null;

        if (calNumbers != null && calNumbers.length > 0 && !StringUtils.isEmpty(calNumbers[0])) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_DISTR_CAL)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                keyMap.put(KEY_NAME_CAL_NUM, new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                ArrayList<String> calCodes = keyMap.get(KEY_NAME_CAL_NUM);

                for (String calNum : calNumbers) {
                    if (!StringUtils.isEmpty(calNum)) {
                        ReferenceCode rcd = (ReferenceCode) getBroker().getBeanByOid(ReferenceCode.class, calNum);
                        String code = rcd.getCode();
                        if (!StringUtils.isEmpty(code) && !calCodes.contains(code)) {
                            calCodes.add(rcd.getCode());
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_DISTR_CAL, keyMap);
                }
            }
        } else {
            removeTab(TAB_DISTR_CAL, null);
        }
    }

    /**
     * Get exports results which are needed to be checked for the creating deletes from input map.
     */
    private void initializeEfrNamesByParams() {
        m_efrNameByParams = new ArrayList<EXPORTS_FIELDS>();
        m_tabsToUse = new ArrayList<String>();
        for (EXPORTS_FIELDS field : EXPORTS_FIELDS.values()) {
            if ((Boolean) getParameter(field.getID()) == Boolean.TRUE) {
                m_efrNameByParams.add(field);
                String tabName = field.getTabName();
                if (!m_tabsToUse.contains(tabName)) {
                    m_tabsToUse.add(tabName);
                }
            }
        }
    }

    /**
     * Populate ExportFormatResult map keyed on name.
     */
    private void initializeEFResultsMap() {
        X2Criteria efrCriteria = new X2Criteria();
        efrCriteria.addLikeIgnoreCase(ExportFormatResult.COL_NAME, REGEX_EFR_NAME);

        Calendar cal = Calendar.getInstance();
        cal.setTime(getCurrentContext().getStartDate());
        efrCriteria.addGreaterOrEqualThan(ExportFormatResult.COL_RUN_DATE, cal.getTimeInMillis());

        cal.setTime(getCurrentContext().getEndDate());
        efrCriteria.addLessOrEqualThan(ExportFormatResult.COL_RUN_DATE, cal.getTimeInMillis());

        QueryByCriteria efrQuery = new QueryByCriteria(ExportFormatResult.class, efrCriteria);
        m_efrMap = getBroker().getGroupedCollectionByQuery(efrQuery, ExportFormatResult.COL_NAME, 1024);
    }

    /**
     * Populate list of ExportFromatResult oids which match input selection.
     */
    private void initializeFinalResultOids() {
        m_resultsMap = new HashMap<String, Set<String>>();
        if (m_efrMap == null) {
            initializeEFResultsMap();
        }

        if (m_efrNameByParams == null) {
            initializeEfrNamesByParams();
        }

        for (EXPORTS_FIELDS field : m_efrNameByParams) {

            for (Map.Entry<String, LinkedList<ExportFormatResult>> entry : m_efrMap.entrySet()) {
                if (entry.getKey().contains(field.getEfrName())) {
                    Set<String> results = m_resultsMap.get(field.getTabName());

                    if (results == null) {
                        results = new HashSet<String>();
                        m_resultsMap.put(field.getTabName(), results);
                    }

                    for (ExportFormatResult result : entry.getValue()) {
                        results.add(result.getOid());
                    }
                }
            }
        }
    }

    /**
     * Populate keys of the selected CAS entities.
     */
    private void initializeSklCalendarEntities() {
        String[] calNumbers =
                m_inputMap.get(INPUT_PARAM_SCHOOL_CALS) != null ? ((String) m_inputMap.get(INPUT_PARAM_SCHOOL_CALS))
                        .split(",") : null;

        if (calNumbers != null && calNumbers.length > 0 && !StringUtils.isEmpty(calNumbers[0])) {

            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_SKL_CAL)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                keyMap.put(KEY_NAME_CAL_NUM, new ArrayList<String>());
                keyMap.put(KEY_NAME_INSTR_PGM_NUM, new ArrayList<String>());
                keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                ArrayList<String> calCodes = keyMap.get(KEY_NAME_CAL_NUM);

                ArrayList<String> instrPgmCodes = keyMap.get(KEY_NAME_INSTR_PGM_NUM);

                ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);

                for (String calNum : calNumbers) {
                    if (!StringUtils.isEmpty(calNum)) {
                        SchoolCalendar cas = (SchoolCalendar) getBroker().getBeanByOid(SchoolCalendar.class, calNum);

                        String casNumber = (String) cas.getFieldValueByAlias(ALIAS_SKL_CAL_NUM);

                        if (!StringUtils.isEmpty(casNumber) && !calCodes.contains(casNumber)) {
                            calCodes.add(casNumber);
                        }

                        SisSchool school = (SisSchool) cas.getSchool();
                        String sklId = null;
                        if (school != null && !StringUtils
                                .isEmpty((sklId = (String) school.getFieldValueByAlias(ALIAS_SKL_STATE_ID)))) {
                            sklIds.add(sklId);
                        }

                        String instrPgmCode = (String) cas.getFieldValueByAlias(ALIAS_SKL_CAL_INSTR_PGM_NUM);

                        if (!StringUtils.isEmpty(instrPgmCode) && !instrPgmCodes.contains(instrPgmCode)) {
                            instrPgmCodes.add(instrPgmCode);
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_SKL_CAL, keyMap);
                }
            }
        } else {
            removeTab(TAB_SKL_CAL, null);
        }
    }

    /**
     * Populate keys of the selected STF entities.
     */
    private void initializeStaffEntities() {
        String schoolOid = (String) m_inputMap.get(INPUT_PARAM_STF_SKL_OID);
        String[] stfOids = m_inputMap.get(INPUT_PARAM_STF_OIDS) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_STF_OIDS)).split(",")
                : null;
        Boolean entireSchool = Boolean.FALSE;
        if (stfOids == null) {
            entireSchool = Boolean.TRUE;
        }
        if (!StringUtils.isEmpty(schoolOid)) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STF)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                if (entireSchool.booleanValue()) {

                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());

                    SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                    String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);
                    sklIds.add(sklStateId);

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                } else if (!entireSchool.booleanValue() && stfOids != null && stfOids.length > 0) {
                    keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_ID, new ArrayList<String>());
                    keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                    keyMap.put(KEY_NAME_STF_ID, new ArrayList<String>());

                    SisSchool skl = (SisSchool) getBroker().getBeanByOid(SisSchool.class, schoolOid);
                    String sklStateId = (String) skl.getFieldValueByAlias(ALIAS_SKL_STATE_ID);

                    ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                    distrIds.add(getOrganization().getId());

                    ArrayList<String> sklIds = keyMap.get(KEY_NAME_SKL_ID);
                    sklIds.add(sklStateId);

                    ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                    sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                    ArrayList<String> stfIds = keyMap.get(KEY_NAME_STF_ID);

                    for (String stfOid : stfOids) {
                        SisStaff stf = (SisStaff) getBroker().getBeanByOid(SisStaff.class, stfOid);

                        if (stfIds != null && stf != null && !stfIds.contains(stf.getLocalId())) {
                            stfIds.add(StringUtils.padLeft(stf.getLocalId(), 10, '0'));
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_STF, keyMap);
                }
            }
        } else {
            m_fromTab.remove(EXPORTS_FIELDS.ROW_063.getID());

            // We can't just delete all tab, because may exist some definitions that are still
            // needed for other tabs.
            ArrayList<String> savedDefinitionsIds = new ArrayList<String>();
            if (m_fromTab.contains(EXPORTS_FIELDS.ROW_063_TAB_CRS_MST.getTabName())) {
                String definitionId = getDefinitionId(EXPORTS_FIELDS.ROW_063_TAB_CRS_MST);
                savedDefinitionsIds.add(definitionId);
            }

            removeTab(TAB_STF, savedDefinitionsIds);
        }
    }

    /**
     * Populate keys of the selected STD entities.
     */
    private void initializeStdEntities() {
        String queryBy = (String) getParameter(INPUT_PARAM_STD_QUERY_BY);
        String queryString = (String) getParameter(INPUT_PARAM_STD_QUERY_STRING);

        if (!StringUtils.isEmpty(queryBy) && !NONE_SELECTED.equals(queryBy)) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STD)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                keyMap.put(KEY_NAME_STD_LOCAL_ID, new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                if (!StringUtils.isEmpty(queryString) && !INPUT_PARAM_STD_ALL.equals(queryBy)) {
                    ArrayList<String> stdLocalIds = keyMap.get(KEY_NAME_STD_LOCAL_ID);

                    if (INPUT_PARAM_STD_LOCAL_ID.equals(queryBy)) {
                        stdLocalIds.add(StringUtils.padLeft(queryString, 10, '0'));
                    }
                    if (INPUT_PARAM_STD_STATE_ID.equals(queryBy)) {
                        X2Criteria stdCriteria = new X2Criteria();
                        DataDictionary dictionary =
                                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_STATE_ID);
                        stdCriteria.addEqualTo(field.getJavaName(), queryString);
                        SisStudent std = (SisStudent) getBroker()
                                .getBeanByQuery(new QueryByCriteria(SisStudent.class, stdCriteria));

                        if (std != null) {
                            m_inputMap.put(INPUT_PARAM_STD_QUERY_STRING, std.getStateId());
                            stdLocalIds.add(StringUtils.padLeft(std.getLocalId(), 10, '0'));
                        }
                    }
                    if (INPUT_PARAM_STD_SNAPSHOT.equals(queryBy)) {
                        SubQuery recordSetSubquery =
                                ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
                        Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);

                        X2Criteria stdCriteria = new X2Criteria();
                        stdCriteria.addIn(X2BaseBean.COL_OID, objectOids);

                        Collection<SisStudent> stds =
                                getBroker().getCollectionByQuery(new QueryByCriteria(SisStudent.class, stdCriteria));

                        if (stds != null) {
                            for (SisStudent std : stds) {
                                stdLocalIds.add(StringUtils.padLeft(std.getLocalId(), 10, '0'));
                            }
                        }
                    }
                }
                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_STD, keyMap);
                }
            }
        } else {
            m_fromTab.remove(EXPORTS_FIELDS.ROW_045.getID());
            m_fromTab.remove(EXPORTS_FIELDS.ROW_048.getID());
            // We can't just delete all tab, because may exist some definitions that are still
            // needed for other tabs.
            ArrayList<String> savedDefinitionsIds = new ArrayList<String>();
            if (m_fromTab.contains(EXPORTS_FIELDS.ROW_045_TAB_BUS.getTabName())) {
                String definitionId = getDefinitionId(EXPORTS_FIELDS.ROW_045_TAB_BUS);
                savedDefinitionsIds.add(definitionId);
            }
            if (m_fromTab.contains(EXPORTS_FIELDS.ROW_048_TAB_CRS_MST.getTabName())) {
                String definitionId = getDefinitionId(EXPORTS_FIELDS.ROW_048_TAB_CRS_MST);
                savedDefinitionsIds.add(definitionId);
            }
            removeTab(TAB_STD, savedDefinitionsIds);
        }
    }

    /**
     * Populate keys of the selected STD_PGM entities.
     */
    private void initializeStdPgmEntities() {
        String queryBy = (String) getParameter(INPUT_PARAM_STD_QUERY_BY);
        String queryString = (String) getParameter(INPUT_PARAM_STD_QUERY_STRING);
        String[] classificCodeOids = m_inputMap.get(INPUT_PARAM_CLASSIFICATIONS) != null
                ? ((String) m_inputMap.get(INPUT_PARAM_CLASSIFICATIONS)).split(",")
                : null;

        boolean classificationSelected = classificCodeOids != null &&
                classificCodeOids.length > 0 &&
                !StringUtils.isEmpty(classificCodeOids[0]);

        if (!StringUtils.isEmpty(queryBy) && !NONE_SELECTED.equals(queryBy) &&
                (!m_restrictTN044.booleanValue() || classificationSelected)) {
            if (m_resultsMap != null && m_resultsMap.keySet().contains(TAB_STD_PGM)) {
                HashMap<String, ArrayList<String>> keyMap = new HashMap<String, ArrayList<String>>();

                keyMap.put(KEY_NAME_DISTR_ID, new ArrayList<String>());
                keyMap.put(KEY_NAME_SKL_YEAR, new ArrayList<String>());
                keyMap.put(KEY_NAME_STD_LOCAL_ID, new ArrayList<String>());
                keyMap.put(KEY_NAME_CLASSIFICATION, new ArrayList<String>());

                ArrayList<String> distrIds = keyMap.get(KEY_NAME_DISTR_ID);
                distrIds.add(getOrganization().getId());

                ArrayList<String> sklYears = keyMap.get(KEY_NAME_SKL_YEAR);
                sklYears.add(Integer.valueOf(getCurrentContext().getSchoolYear() - 1).toString());

                if (!StringUtils.isEmpty(queryString) && !INPUT_PARAM_STD_ALL.equals(queryBy)) {
                    ArrayList<String> stdLocalIds = keyMap.get(KEY_NAME_STD_LOCAL_ID);

                    if (INPUT_PARAM_STD_LOCAL_ID.equals(queryBy)) {
                        stdLocalIds.add(StringUtils.padLeft(queryString, 10, '0'));
                    }
                    if (INPUT_PARAM_STD_STATE_ID.equals(queryBy)) {
                        X2Criteria stdCriteria = new X2Criteria();
                        DataDictionary dictionary =
                                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
                        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(ALIAS_STD_STATE_ID);
                        stdCriteria.addEqualTo(field.getJavaName(), queryString);
                        SisStudent std = (SisStudent) getBroker()
                                .getBeanByQuery(new QueryByCriteria(SisStudent.class, stdCriteria));

                        if (std != null) {
                            m_inputMap.put(INPUT_PARAM_STD_QUERY_STRING, std.getStateId());
                            stdLocalIds.add(StringUtils.padLeft(std.getLocalId(), 10, '0'));
                        }
                    }
                    if (INPUT_PARAM_STD_SNAPSHOT.equals(queryBy)) {
                        SubQuery recordSetSubquery =
                                ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
                        Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);

                        X2Criteria stdCriteria = new X2Criteria();
                        stdCriteria.addIn(X2BaseBean.COL_OID, objectOids);

                        Collection<SisStudent> stds =
                                getBroker().getCollectionByQuery(new QueryByCriteria(SisStudent.class, stdCriteria));

                        if (stds != null) {
                            for (SisStudent std : stds) {
                                stdLocalIds.add(StringUtils.padLeft(std.getLocalId(), 10, '0'));
                            }
                        }
                    }
                }

                if (m_restrictTN044.booleanValue()) {
                    if (classificationSelected) {
                        X2Criteria criteria = new X2Criteria();
                        criteria.addIn(X2BaseBean.COL_OID, Arrays.asList(classificCodeOids));
                        QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, criteria);
                        Collection<ReferenceCode> codes = getBroker().getCollectionByQuery(query);

                        ArrayList<String> classificationCodes = keyMap.get(KEY_NAME_CLASSIFICATION);
                        for (ReferenceCode code : codes) {
                            if (!StringUtils.isEmpty(code.getStateCode())) {
                                classificationCodes.add(code.getStateCode());
                            }
                        }
                    }
                }

                if (!keyMap.isEmpty()) {
                    iterateDeletes(TAB_STD_PGM, keyMap);
                }
            }
        } else {
            removeTab(TAB_STD_PGM, null);
        }
    }

    /**
     * Input parameters OK.
     *
     * @return true, if successful
     */
    private boolean inputParametersOK() {
        boolean result = true;
        // Validate that snapshot has rows
        String queryBy = (String) getParameter(INPUT_PARAM_STD_QUERY_BY);
        String queryString = (String) getParameter(INPUT_PARAM_STD_QUERY_STRING);
        if (INPUT_PARAM_STD_SNAPSHOT.equals(queryBy)) {
            SubQuery recordSetSubquery =
                    ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool());
            Collection<String> objectOids = getBroker().getSubQueryCollectionByQuery(recordSetSubquery);
            if (objectOids == null || objectOids.isEmpty()) {
                logMessage("Snapshot invalid. Enter a snapshot containing at least one student.");
                result = false;
            }
        }
        return result;
    }

    /**
     * Populate list of oids of the ExportFormatRows to delete.
     *
     * @param tabName String
     * @param keysMap HashMap<String,ArrayList<String>>
     * @return Oids of records that will be deleted.
     */
    private void iterateDeletes(String tabName, HashMap<String, ArrayList<String>> keysMap) {
        Set<String> results = m_resultsMap.get(tabName);
        boolean excl041Del = false;
        if (results != null) {
            for (String resultOid : results) {
                Set<String> deleteRowsOids = new HashSet<String>();
                ExportFormatResult result =
                        (ExportFormatResult) getBroker().getBeanByOid(ExportFormatResult.class, resultOid);

                ExportFormatDefinition definition = null;
                String definitionOid = EMPTY_STRING;
                ArrayList<ExportFormatField> fields = new ArrayList<ExportFormatField>();
                if (result != null && (definitionOid = result.getDefinitionOid()) != null) {
                    definition = (ExportFormatDefinition) getBroker().getBeanByOid(ExportFormatDefinition.class,
                            definitionOid);
                    if (definition != null) {
                        fields.addAll(definition.getFields(getBroker()));
                    }
                }

                m_recordId = "";
                HashMap<String, ArrayList<HashMap<String, String>>> fieldsMap = getMapOfKeyIndexes(fields);
                Collection<ExportFormatRow> tempRows = null;
                if ((tempRows = result.getRows()) != null && tempRows.size() > 0) {
                    ExportFormatRow row = (ExportFormatRow) tempRows.toArray()[0];
                    if (row != null && !StringUtils.isEmpty(m_recordId)) {
                        String recordId = (String) row.getFieldValueByBeanPath(m_recordId);

                        if (!StringUtils.isEmpty(recordId)) {
                            String queryBy = (String) getParameter(INPUT_PARAM_STD_QUERY_BY);
                            boolean exclude041Deletes =
                                    ((Boolean) getParameter(INPUT_PARAM_EXCL_041_DELETES)).booleanValue();
                            boolean isCorrectQuery = INPUT_PARAM_STD_LOCAL_ID.equals(queryBy) ||
                                    INPUT_PARAM_STD_STATE_ID.equals(queryBy) ||
                                    INPUT_PARAM_STD_SNAPSHOT.equals(queryBy);

                            excl041Del = "041".equals(recordId) && isCorrectQuery && exclude041Deletes;

                            Map<String, Integer> countsMap = m_prcMessage.get(recordId);
                            Map<String, Integer> grandTotal = m_prcMessage.get(RECORD_GRAND_TOTAL_KEY);
                            if (countsMap == null) {
                                countsMap = new HashMap<String, Integer>();
                                countsMap.put(RECORD_DELETE, Integer.valueOf(0));
                                countsMap.put(RECORD_NEW, Integer.valueOf(0));
                                m_prcMessage.put(recordId, countsMap);
                            }

                            if (grandTotal == null) {
                                grandTotal = new HashMap<String, Integer>();
                                grandTotal.put(RECORD_ALL, Integer.valueOf(0));
                                m_prcMessage.put(RECORD_GRAND_TOTAL_KEY, grandTotal);
                            }
                        }
                    }
                }

                if (!excl041Del && !fieldsMap.isEmpty()) {
                    String recordTypeBeanPath = (String) fieldsMap.keySet().toArray()[0];

                    ArrayList<ExportFormatRow> rows =
                            getEFRowsResultOid(resultOid, fieldsMap.get(recordTypeBeanPath), keysMap);
                    Set<String> keyValues = new HashSet();

                    if (rows != null && !rows.isEmpty()) {
                        for (ExportFormatRow row : rows) {
                            String recordType = (String) row.getFieldValueByBeanPath(recordTypeBeanPath);
                            String recordId = (String) row.getFieldValueByBeanPath(m_recordId);

                            if (RECORD_NEW.equals(recordType)) {
                                StringBuilder keyValue = new StringBuilder();
                                for (HashMap<String, String> map : fieldsMap.get(recordTypeBeanPath)) {
                                    for (String path : map.values()) {
                                        keyValue.append((String) row.getFieldValueByBeanPath(path));
                                        keyValue.append("|");
                                    }
                                }

                                if (!keyValues.contains(keyValue.toString()) &&
                                        (m_deleteRowsOis.get(definition.getId()) == null ||
                                                !m_deleteRowsOis.get(definition.getId()).contains(row.getOid()))) {
                                    deleteRowsOids.add(row.getOid());
                                    keyValues.add(keyValue.toString());

                                    iterateTotals(recordId, RECORD_DELETE);
                                }
                            }
                        }
                    }
                }
                if (definition != null) {
                    Set<String> oids = m_deleteRowsOis.get(definition.getId());
                    if (oids != null) {
                        oids.addAll(deleteRowsOids);
                        m_deleteRowsOis.put(definition.getId(), oids);
                    } else {
                        m_deleteRowsOis.put(definition.getId(), deleteRowsOids);
                    }
                }
            }
        }
    }

    /**
     * Iterate totals for the output.
     *
     * @param recordId String
     * @param recordType String
     */
    private void iterateTotals(String recordId, String recordType) {
        if (!StringUtils.isEmpty(recordId)) {
            Map<String, Integer> countsMap = m_prcMessage.get(recordId);
            Map<String, Integer> grandTotal = m_prcMessage.get(RECORD_GRAND_TOTAL_KEY);
            if (countsMap == null) {
                countsMap = new HashMap<String, Integer>();
                countsMap.put(RECORD_DELETE, Integer.valueOf(0));
                countsMap.put(RECORD_NEW, Integer.valueOf(0));
                m_prcMessage.put(recordId, countsMap);
            }

            if (grandTotal == null) {
                grandTotal = new HashMap<String, Integer>();
                grandTotal.put(RECORD_ALL, Integer.valueOf(0));
                m_prcMessage.put(RECORD_GRAND_TOTAL_KEY, grandTotal);
            }

            int sum = countsMap.get(recordType).intValue() + 1;
            countsMap.put(recordType, Integer.valueOf(sum));
            int totalSum = grandTotal.get(RECORD_ALL).intValue() + 1;
            grandTotal.put(RECORD_ALL, Integer.valueOf(totalSum));

        }
    }

    /**
     * Run the tool jobs for the selected exports.
     *
     * @param definitions Collection<ImportExportDefinition>
     * @throws Exception exception
     */
    private void performToolJobs(Collection<ImportExportDefinition> definitions) throws Exception {
        boolean includeHeader = false;
        boolean includeFooter = false;

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ImportExportDefinition.COL_ID, RESTAGING_EXPORT_ID);
        QueryByCriteria query = new QueryByCriteria(ImportExportDefinition.class, criteria);
        ImportExportDefinition restagingExport = (ImportExportDefinition) getBroker().getBeanByQuery(query);

        if (restagingExport == null) {
            throw new ToolRunException(RESTAGING_EXPORT_ID + " is not found");
        }

        List<KeyValueTrio<ImportExportDefinition, ToolSourceCode, ToolJob>> jobsList = new LinkedList();
        try {
            for (ImportExportDefinition def : definitions) {
                ToolSourceCode cloneCode = null;
                cloneCode = (ToolSourceCode) restagingExport.getSourceCode().copyBean();
                cloneCode.setInputDefinition(
                        combineInputDefinition(getJob().getTool().getFullInputDefinition(), def.getSourceCode()));
                getBroker().saveBeanForced(cloneCode);

                ImportExportDefinition clone = def.clone();
                clone.setSourceCodeOid(cloneCode.getOid());
                ToolJob job = null;
                try {
                    job = createToolJob(clone, includeHeader, includeFooter);
                } catch (Exception e) {
                    if (cloneCode != null && !StringUtils.isEmpty(cloneCode.getOid())) {
                        getBroker().deleteBean(cloneCode);
                    }
                }
                jobsList.add(new KeyValueTrio(clone, cloneCode, job));
            }

            for (KeyValueTrio<ImportExportDefinition, ToolSourceCode, ToolJob> item : jobsList) {
                ToolJob job = item.getValue2();
                try {
                    job.run();
                } finally {
                    try {
                        if (job != null && job.getResultHandler() != null) {
                            BufferedReader reader =
                                    new BufferedReader(new FileReader(job.getResultHandler().getFilePath()));
                            try {
                                String line;
                                String code = null;
                                String recordType = null;

                                while ((line = reader.readLine()) != null) {
                                    if (line.length() >= 3) {
                                        if (code == null) {
                                            code = line.substring(0, 3);
                                        }
                                        recordType = line.substring(5, 6);

                                        if (!StringUtils.isEmpty(recordType) && RECORD_NEW.equals(recordType)) {
                                            iterateTotals(code, RECORD_NEW);
                                        }
                                    }
                                }
                            } finally {
                                reader.close();
                            }
                        }
                    } catch (FileNotFoundException fnfe) {
                        throw new X2BaseException(fnfe);
                    } catch (IOException ioe) {
                        throw new X2BaseException(ioe);
                    }
                }
            }
        } finally {
            for (KeyValueTrio<ImportExportDefinition, ToolSourceCode, ToolJob> listItem : jobsList) {
                if (listItem.getValue1() != null) {
                    getBroker().deleteBean(listItem.getValue1());
                }
            }
        }
    }

    /**
     * Print output message.
     */
    private void printOutputMessage() {
        if (m_prcMessage != null) {
            Set<String> sortedSet = new TreeSet<String>(m_prcMessage.keySet());

            Integer grandTotal = null;

            for (String recordId : sortedSet) {
                Map<String, Integer> valuesMap = m_prcMessage.get(recordId);

                if (!RECORD_GRAND_TOTAL_KEY.equals(recordId)) {
                    Integer deletesCount = valuesMap.get(RECORD_DELETE);
                    Integer newCount = valuesMap.get(RECORD_NEW);
                    Integer totalCount = Integer.valueOf(deletesCount.intValue() + newCount.intValue());
                    if (totalCount.intValue() > 0) {
                        logMessage("Total Records with Record Type = TN " + recordId + " - " + totalCount + ". Type " +
                                RECORD_DELETE + " - " + deletesCount + ", Type " + RECORD_NEW + " - " + newCount + ".");
                    }
                } else {
                    grandTotal = valuesMap.get(RECORD_ALL);
                }
            }

            logMessage("Grand Total: " + grandTotal + ".");
        }
    }

    /**
     * Removes the tab.
     *
     * @param tabName String
     * @param savedDefinitionIds Collection<String>
     */
    private void removeTab(String tabName, Collection<String> savedDefinitionIds) {
        for (EXPORTS_FIELDS fields : EXPORTS_FIELDS.values()) {
            if (fields.getTabName().equals(tabName)) {
                Iterator<ImportExportDefinition> iter = m_definitions.iterator();
                while (iter.hasNext()) {
                    ImportExportDefinition item = iter.next();
                    String fieldDefinitionId = getDefinitionId(fields);
                    if (fieldDefinitionId.equals(item.getId()) &&
                            (savedDefinitionIds == null ||
                                    !savedDefinitionIds.contains(item.getId()))) {
                        iter.remove();
                        break;
                    }
                }
            }
        }
    }

    /**
     * Set parameters.
     *
     * @param input ToolInput
     * @param exportDefinition ImportExportDefinition
     */
    private void setParameters(ToolInput input, ImportExportDefinition exportDefinition) {
        String definitionId = exportDefinition.getId();

        if (m_fromTab != null) {
            if (definitionId.equals(getDefinitionId(EXPORTS_FIELDS.ROW_045))) {
                boolean tn045RunFromStdTab = m_fromTab.contains(EXPORTS_FIELDS.ROW_045.getID());
                boolean tn045RunFromBusTab = m_fromTab.contains(EXPORTS_FIELDS.ROW_045_TAB_BUS.getTabName());

                if (tn045RunFromStdTab && tn045RunFromBusTab) {
                    input.setParameterAsString(INPUT_PARAM_FROM_TAB, EXPORTS_FIELDS.ROW_045_TAB_BUS.getTabName());
                } else if (!tn045RunFromStdTab && tn045RunFromBusTab) {
                    input.setParameterAsString(INPUT_PARAM_STD_QUERY_BY, INPUT_PARAM_STD_ALL);
                    input.setParameterAsString(INPUT_PARAM_FROM_TAB, EXPORTS_FIELDS.ROW_045_TAB_BUS.getTabName());
                }
            } else if (definitionId.equals(getDefinitionId(EXPORTS_FIELDS.ROW_048))) {
                boolean tn048RunFromStdTab = m_fromTab.contains(EXPORTS_FIELDS.ROW_048.getID());
                boolean tn048RunFromCrsTab = m_fromTab.contains(EXPORTS_FIELDS.ROW_048_TAB_CRS_MST.getTabName());

                if (tn048RunFromStdTab && tn048RunFromCrsTab) {
                    input.setParameterAsString(INPUT_PARAM_FROM_TAB, EXPORTS_FIELDS.ROW_048_TAB_CRS_MST.getTabName());
                } else if (!tn048RunFromStdTab && tn048RunFromCrsTab) {
                    input.setParameterAsString(INPUT_PARAM_STD_QUERY_BY, INPUT_PARAM_STD_ALL);
                    input.setParameterAsString(INPUT_PARAM_FROM_TAB, EXPORTS_FIELDS.ROW_048_TAB_CRS_MST.getTabName());
                }
            } else if (definitionId.equals(getDefinitionId(EXPORTS_FIELDS.ROW_063))) {
                boolean tn063RunFromCrsTab = m_fromTab.contains(EXPORTS_FIELDS.ROW_063_TAB_CRS_MST.getTabName());
                boolean tn063RunFromStaffTab = m_fromTab.contains(EXPORTS_FIELDS.ROW_063.getID());

                if (tn063RunFromStaffTab && tn063RunFromCrsTab) {
                    input.setParameterAsString(INPUT_PARAM_FROM_TAB, EXPORTS_FIELDS.ROW_063_TAB_CRS_MST.getTabName());
                } else if (!tn063RunFromStaffTab && tn063RunFromCrsTab) {
                    input.setParameterAsString(INPUT_PARAM_FROM_TAB, EXPORTS_FIELDS.ROW_063_TAB_CRS_MST.getTabName());
                    input.setParameterAsString(INPUT_PARAM_STF_OIDS, null);
                    input.setParameterAsString(INPUT_PARAM_STF_SKL_OID, null);
                }
            }
        }

        // Set delete records oids. Always will be overwritten, so default value is just dummy.
        String importExportDefId = exportDefinition.getId();
        importExportDefId = importExportDefId.replaceAll("EXP-", "EXPDATA-");
        // Must loop through m_deleteRowOids map to capture matches for all versions
        StringBuilder allDeleteOids = new StringBuilder();
        for (Entry<String, Set<String>> entry : m_deleteRowsOis.entrySet()) {
            if (entry.getKey().equals(importExportDefId) || entry.getKey().startsWith(importExportDefId + "-V")) {
                Set<String> deleteOids = entry.getValue();
                if (deleteOids != null && deleteOids.size() > 0) {
                    for (String deleteOid : deleteOids) {
                        if (allDeleteOids.length() > 0) {
                            allDeleteOids.append(IDS_DELIMITER);
                        }
                        allDeleteOids.append(deleteOid);
                    }
                }
            }
        }
        if (allDeleteOids.length() == 0) {
            input.setParameterAsString(PARAM_DELETE_ROWS_OIDS, null);
        } else {
            input.setParameterAsString(PARAM_DELETE_ROWS_OIDS, allDeleteOids.toString());
        }
    }
}
