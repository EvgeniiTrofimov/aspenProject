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
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.ResultHandler;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.lowagie.text.DocWriter;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Rectangle;
import com.lowagie.text.html.HtmlWriter;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfWriter;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.ByteArrayClassLoader;
import com.x2dev.utils.ByteArrayCompiler;
import com.x2dev.utils.DataGrid;
import com.x2dev.utils.FolderUtils;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.ThreadUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * The Class MaBoundReports.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaBoundReports extends ReportJavaSourceNet {
    private static final long serialVersionUID = 1L;
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    protected static final String SAVE_TO_DOCUMENTS_IEP_PARAM = "saveToDocumentsIEP";

    /**
     * Represent data for one formInstance<br>
     * formInstance can be null for blank case<br>
     * it has own javaSource .
     *
     * @author X2 Development Corporation
     */
    public class FormInstanceContainer {
        private static final String METHOD_NAME_GATHER_DATA = "gatherData";
        private static final String METHOD_NAME_INIT_BEAN_REPORT = "initBeanReport";

        private Object m_formDataSource = null;
        private Map<String, Object> m_defaultParam = null;
        private FormInstance m_formInstance = null;
        private Object m_javaSource = null;
        private ByteArrayInputStream m_jasperFormat = null;
        private X2BaseBean m_storageObject = null;

        /**
         * Instantiates a new form instance container.
         *
         * @param container ReportDataContainer
         * @param formInstance FormInstance
         * @throws ToolRunException exception
         */
        public FormInstanceContainer(ReportDataContainer container, FormInstance formInstance) throws ToolRunException {
            container.addFormInstanceContainer(this);
            m_formInstance = formInstance;
            if (formInstance != null && formInstance.getStorageObject() != null) {
                m_storageObject = formInstance.getStorageObject();
            } else {
                FormDefinition formDefinition =
                        container.getReportDataContainers().getFormDefinitionByFormId(container.getFormId());
                m_storageObject = createFormStorage(formDefinition);

            }
            m_defaultParam = container.getCopiedDefaultParam();
            initAndSetToolJavaSource(container);

            m_jasperFormat = new ByteArrayInputStream(container.getReport().getCompiledFormat());
        }

        /**
         * return filled JRDataSource for current formInstance .
         *
         * @param multipleNumber String
         * @return JR data source
         * @throws ToolRunException exception
         */
        public Object getDataSource(String multipleNumber) throws ToolRunException {
            Object classObject = m_javaSource;
            m_formDataSource = callReflection(METHOD_NAME_GATHER_DATA, classObject, new Class[] {},
                    new Object[] {});
            return m_formDataSource;
        }

        /**
         * get FormInstance.
         *
         * @return Form instance
         */
        public FormInstance getFormInstance() {
            return m_formInstance;
        }

        /**
         * get FormStorage.
         *
         * @return X 2 base bean
         */
        public X2BaseBean getFormStorage() {
            return m_storageObject;
        }

        /**
         * get Jasper Report Format.
         *
         * @return Byte array input stream
         */
        public ByteArrayInputStream getJasperFormat() {
            return m_jasperFormat;
        }

        /**
         * get Tool Java Source object.
         *
         * @return Object
         */
        public Object getJavaSource() {
            return m_javaSource;
        }

        /**
         * get report param.
         *
         * @return Map
         */
        public Map<String, Object> getMapParam() {
            return m_defaultParam;
        }

        /**
         * Creates the form storage.
         *
         * @param formDefinition FormDefinition
         * @return X2BaseBean
         */
        private X2BaseBean createFormStorage(FormDefinition formDefinition) {
            DataTableConfig tblConf = (DataTableConfig) getBroker().getBeanByOid(DataTableConfig.class,
                    formDefinition.getStorageDataTableConfigOid());
            Class storageClass = null;
            try {
                storageClass = Class.forName(tblConf.getDataTable().getClassName());
            } catch (ClassNotFoundException e) {
                AppGlobals.getLog().log(Level.SEVERE, e.toString());
            }
            X2BaseBean bean = X2BaseBean.newInstance(storageClass, getBroker().getPersistenceKey());
            if (bean instanceof DictionaryExtendable) {
                String dictionaryOidColumn = ((DictionaryExtendable) bean).getDictionaryOidColumn();
                bean.setFieldValueByBeanPath(dictionaryOidColumn,
                        m_formDefinition.getExtendedDataDictionaryOid());
            }
            return bean;
        }

        /**
         * initialize report Java Source and return it.
         *
         * @param container ReportDataContainer
         * @throws ToolRunException exception
         */
        private void initAndSetToolJavaSource(ReportDataContainer container) throws ToolRunException {
            Report report = container.getReport();
            Class toolObjectClass = getToolObjectClass(report);

            if (toolObjectClass != null) {
                Class[] classArgs = new Class[] {FormInstance.class, X2BaseBean.class, X2BaseBean.class,
                        DataDictionary.class, Map.class, Organization.class, DistrictSchoolYearContext.class,
                        X2Broker.class, String.class};
                Object classObject = newInstance(toolObjectClass);

                Object[] objectArgs = new Object[] {m_formInstance, m_storageObject, m_formInstance.getOwnerObject(),
                        container.getDataDictionary(), getMapParam(), getOrganization(), getCurrentContext(),
                        getBroker(), ((Report) getJob().getTool()).getEngineVersion()};


                callReflection(METHOD_NAME_INIT_BEAN_REPORT, classObject, classArgs, objectArgs);
                m_javaSource = classObject;
            }
        }


    }

    /**
     * The Class ReportDataContainer.
     */
    public class ReportDataContainer {
        private DataDictionary m_ddx = null;
        private Map<String, Object> m_defaultParam = null;
        private Map<String, Object> m_defaultToolInput = null;
        private String m_formId = null;
        private Map<String, Object> m_inputParams = new HashMap();
        private List<FormInstanceContainer> m_instsContainer = new ArrayList<FormInstanceContainer>();
        private Report m_containedReport = null;
        private ReportDataContainers m_reportDataContainers;
        private String m_reportId = null;

        /**
         * Instantiates a new report data container.
         *
         * @param reportDataContainers ReportDataContainers
         */
        public ReportDataContainer(ReportDataContainers reportDataContainers) {
            m_reportDataContainers = reportDataContainers;
        }

        /**
         * Adds the form instance container.
         *
         * @param formInstContainer FormInstanceContainer
         */
        public void addFormInstanceContainer(FormInstanceContainer formInstContainer) {
            m_instsContainer.add(formInstContainer);
        }

        /**
         * Gets the copied default param.
         *
         * @return Map
         */
        public Map<String, Object> getCopiedDefaultParam() {
            return m_defaultParam == null ? new HashMap<String, Object>() : new HashMap<String, Object>(m_defaultParam);
        }

        /**
         * Gets the data dictionary.
         *
         * @return Data dictionary
         */
        public DataDictionary getDataDictionary() {
            return m_ddx;
        }

        /**
         * Gets the default tool input.
         *
         * @return Map
         */
        public Map<String, Object> getDefaultToolInput() {
            return m_defaultToolInput;
        }

        /**
         * Gets the form id.
         *
         * @return String
         */
        public String getFormId() {
            return m_formId;
        }

        /**
         * Gets the input params.
         *
         * @return Map
         */
        public Map<String, Object> getInputParams() {
            return m_inputParams;
        }

        /**
         * Gets the list form instance container.
         *
         * @return List
         */
        public List<FormInstanceContainer> getListFormInstanceContainer() {
            return m_instsContainer;
        }

        /**
         * Gets the multiple number.
         *
         * @return String
         */
        public String getMultipleNumber() {
            return null;
        }

        /**
         * Gets the report.
         *
         * @return Report
         */
        public Report getReport() {
            return m_containedReport;
        }

        /**
         * Gets the report data containers.
         *
         * @return Report data containers
         */
        public ReportDataContainers getReportDataContainers() {
            return m_reportDataContainers;
        }

        /**
         * Gets the report id.
         *
         * @return String
         */
        public String getReportId() {
            return m_reportId;
        }

        /**
         * Sets the data dictionary.
         *
         * @param ddx void
         */
        public void setDataDictionary(DataDictionary ddx) {
            m_ddx = ddx;
        }

        /**
         * Sets the default param.
         *
         * @param defaultParam Map<String,Object>
         */
        public void setDefaultParam(Map<String, Object> defaultParam) {
            m_defaultParam = defaultParam;
        }

        /**
         * Sets the default tool input.
         *
         * @param defaultToolInput Map<String,Object>
         */
        public void setDefaultToolInput(Map<String, Object> defaultToolInput) {
            m_defaultToolInput = defaultToolInput;
        }

        /**
         * Sets the from id.
         *
         * @param formId void
         */
        public void setFromId(String formId) {
            m_formId = formId;
        }

        /**
         * Sets the report.
         *
         * @param report void
         */
        public void setReport(Report report) {
            m_containedReport = report;
        }

        /**
         * Sets the report id.
         *
         * @param reportId void
         */
        public void setReportId(String reportId) {
            m_reportId = reportId;
        }
    }

    /**
     * The Class ReportDataContainers.
     */
    public class ReportDataContainers extends ArrayList<ReportDataContainer> {
        private Map<String, FormDefinition> m_formDefinitionMap = null;
        private List<String> m_formIds = new ArrayList();
        private Map<String, Collection<FormInstance>> m_formInstancesMap = null;
        private X2BaseBean m_owner;
        private List<String> m_reportIds = new ArrayList();
        private Map<String, Report> m_reports = new HashMap();

        /**
         * Instantiates a new report data containers.
         *
         * @param owner X2BaseBean
         */
        public ReportDataContainers(X2BaseBean owner) {
            m_owner = owner;
        }

        /**
         * Adds the by id.
         *
         * @param reportId String
         * @param formId String
         * @param inputParams String
         * @param defaultToolInput Map<String,Object>
         * @return ReportDataContainer
         */
        public ReportDataContainer addById(String reportId,
                                           String formId,
                                           String inputParams,
                                           Map<String, Object> defaultToolInput) {
            ReportDataContainer container = new ReportDataContainer(this);
            container.setFromId(formId);
            container.setReportId(reportId);
            container.setDefaultToolInput(defaultToolInput);
            m_reportIds.add(reportId);
            m_formIds.add(formId);
            if (!StringUtils.isEmpty(inputParams)) {
                for (String key : Arrays.asList(inputParams.split("\\s*,\\s*"))) {
                    container.getInputParams().put(key, getParameter(key));
                }
            }
            add(container);
            return container;
        }

        /**
         * Gets the form definition by form id.
         *
         * @param formId String
         * @return Form definition
         */
        public FormDefinition getFormDefinitionByFormId(String formId) {
            FormDefinition formDefinition = null;
            if (m_formDefinitionMap != null) {
                formDefinition = m_formDefinitionMap.get(formId);
            }
            return formDefinition;
        }

        /**
         * Gets the by tool id.
         *
         * @param toolId String
         * @return Report data container
         */
        public ReportDataContainer getByToolId(String toolId) {
            return stream().filter(container -> toolId.equals(container.getReportId())).findAny().orElse(null);
        }

        /**
         * Inits the container resources.
         *
         * @throws ToolRunException exception
         */
        public void initContainerResources() throws ToolRunException {

            loadReports();
            loadFormInstanceAndDefinition();

            for (ReportDataContainer container : this) {

                String formId = container.getFormId();

                Map<String, Object> defaultParam = new HashMap<String, Object>();

                // getParameters() need for add param like "district" and other default param.
                // but in this implementation getParameters() return also unnecessary param from
                // current report
                // Now this does not prevent. If will - need to be changed
                defaultParam.putAll(container.getDefaultToolInput());
                defaultParam.putAll(getDefaultToolInputMapValues());
                defaultParam.putAll(container.getInputParams());

                container.setDefaultParam(defaultParam);

                FormDefinition formDefinition = getFormDefinitionByFormId(formId);
                ExtendedDataDictionary extendedDataDictionary = formDefinition.getExtendedDataDictionary();
                DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(extendedDataDictionary,
                        getBroker().getPersistenceKey());
                container.setDataDictionary(dataDictionary);

                List<FormInstance> allFormInstances = getFormInstanceByFormId(formId);
                // List<FormInstance> targetFormInstances = filterMultipleFormInstance(container,
                // allFormInstances,
                // container.getMultipleNumber(), getWorkflow());
                List<FormInstance> targetFormInstances = new ArrayList();
                if (!allFormInstances.isEmpty()) {
                    targetFormInstances.addAll(allFormInstances);
                }
                addPsuedoInstances(container, targetFormInstances);


                if (targetFormInstances.size() > 0) {
                    for (FormInstance formInstance : targetFormInstances) {
                        new FormInstanceContainer(container, formInstance);
                    }
                } else if (isBlank()) {
                    if (formDefinition != null) {
                        FormInstance instance =
                                X2BaseBean.newInstance(FormInstance.class, getBroker().getPersistenceKey());
                        instance.setFormDefinitionOid(formDefinition.getOid());
                        new FormInstanceContainer(container, instance);
                    }
                }
            }
        }

        /**
         * Gets the owner.
         *
         * @return X 2 base bean
         */
        X2BaseBean getOwner() {
            return m_owner;
        }

        /**
         * Gets the default tool input map values.
         *
         * @return Map
         */
        private Map<String, Object> getDefaultToolInputMapValues() {
            List<String> defaultToolInputKeys =
                    new ArrayList<String>(Arrays.asList("district",
                            "organization",
                            "school_name",
                            "school",
                            "currentContext",
                            "schoolContext",
                            "longDateFormat",
                            "shortDateFormat",
                            "timeFormat",
                            "timestampFormat",
                            "currentDate",
                            "currentDate",
                            "file",
                            "locale"));
            Map<String, Object> parentDefaultToolInputMapValues = getParameters();
            Map<String, Object> defaultToolInputMapValues = new HashMap<String, Object>();
            for (String key : defaultToolInputKeys) {
                Object value = parentDefaultToolInputMapValues.get(key);
                if (value != null) {
                    defaultToolInputMapValues.put(key, value);
                }
            }

            return defaultToolInputMapValues;

        }

        /**
         * Gets the form instance by form id.
         *
         * @param formId String
         * @return List
         */
        private List<FormInstance> getFormInstanceByFormId(String formId) {
            Collection<FormInstance> formInstances = null;
            if (m_formInstancesMap != null) {
                formInstances = m_formInstancesMap.get(formId);
            }
            List<FormInstance> formInstancesList =
                    formInstances == null ? new ArrayList<FormInstance>() : new ArrayList<FormInstance>(formInstances);

            return formInstancesList;
        }

        /**
         * Load form instance and definition.
         */
        private void loadFormInstanceAndDefinition() {

            if (getOwner() != null) {
                X2Criteria criteria = new X2Criteria();
                criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, getOwner().getOid());
                criteria.addIn(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                        m_formIds);
                QueryByCriteria byCriteria = new QueryByCriteria(FormInstance.class, criteria);
                m_formInstancesMap =
                        getBroker().getGroupedCollectionByQuery(byCriteria, FormInstance.REL_FORM_DEFINITION +
                                ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID, 50);

            }


            X2Criteria criteria = new X2Criteria();
            criteria.addIn(FormDefinition.COL_ID, m_formIds);
            QueryByCriteria byCriteria = new QueryByCriteria(FormDefinition.class, criteria);
            m_formDefinitionMap = getBroker().getMapByQuery(byCriteria, FormDefinition.COL_ID, 50);
        }

        /**
         * Load reports.
         */
        private void loadReports() {
            Map<String, Report> reportsMap = loadReportsByIds(m_reportIds);
            for (ReportDataContainer container : this) {
                String reportId = container.getReportId();
                Report report = reportsMap.get(reportId);
                container.setReport(report);
                m_reports.put(reportId, report);
            }
        }
    }



    /**
     * The Class ReportDataInfos.
     */
    public class ReportDataInfos extends ArrayList<ReportDataInfo> {

        /**
         * Adds the by id.
         *
         * @param reportId String
         * @param formId String
         * @param inputParams String
         * @return ReportDataInfo
         */
        public ReportDataInfo addById(String reportId, String formId, String inputParams) {
            ReportDataInfo info = new ReportDataInfo(reportId, formId, inputParams);
            return add(info) ? info : null;
        }

        /**
         * Inits the.
         *
         * @param userData UserDataContainer
         * @throws ToolRunException exception
         */
        public void init(UserDataContainer userData) throws ToolRunException {
            List<String> reportIds = stream().map(ReportDataInfo::getReportId).collect(Collectors.toList());
            Map<String, Report> map = loadReportsByIds(reportIds);
            for (ReportDataInfo info : this) {
                Report report = map.get(info.getReportId());
                Map<String, Object> values = getDefaultValuesFromInput(report, userData);
                if (values != null) {
                    info.setDefaultValues(values);
                }
            }
        }

    }


    /**
     * The Class ReportDataInfo.
     */
    public class ReportDataInfo implements Serializable {
        private Map<String, Object> m_defaultValues;
        private String m_formId;
        private String m_inputParams;
        private String m_reportId;

        /**
         * Instantiates a new report data info.
         *
         * @param reportId String
         * @param formId String
         * @param inputParams String
         */
        public ReportDataInfo(String reportId, String formId, String inputParams) {
            super();
            this.m_reportId = reportId;
            this.m_formId = formId;
            this.m_inputParams = inputParams;
        }

        /**
         * Gets the default values.
         *
         * @return the defaultValues
         */
        public Map<String, Object> getDefaultValues() {
            return m_defaultValues;
        }

        /**
         * Gets the form id.
         *
         * @return the formId
         */
        public String getFormId() {
            return m_formId;
        }

        /**
         * Gets the input params.
         *
         * @return the inputParams
         */
        public String getInputParams() {
            return m_inputParams;
        }

        /**
         * Gets the report id.
         *
         * @return the reportId
         */
        public String getReportId() {
            return m_reportId;
        }

        /**
         * Sets the default values.
         *
         * @param defaultValues Map<String,Object>
         */
        public void setDefaultValues(Map<String, Object> defaultValues) {
            m_defaultValues = defaultValues;
        }
    }

    public static final String INPUT_DEFAULT_VALUE_ATTRIB = "default-value";
    public static final String INPUT_ELEMENT = "input";
    public static final String INPUT_NAME_ATTRIB = "name";
    public static final String PARAM_FORM_DEFINITION_OID = "formDefinitionOid";
    public static final String PARAM_FORM_INSTANCE_OID = "formInstanceOid";

    private static final String METHOD_NAME_FILTER_MULTIPLE_FORM_INSTANCE = "filterMultipleFormInstance";
    private static final String POINT = ".";

    private FormDefinition m_formDefinition = null;
    private boolean m_isBlank = false;
    private Collection<X2BaseBean> m_ownerBeans;
    private ReportDataInfos m_reportDataInfos = new ReportDataInfos();
    private transient List<ReportDataContainers> m_reportsContainersList;
    private File m_saveToDocumentsResultFile = null;

    /**
     * Filter multiple form instance.
     *
     * @param container ReportDataContainer
     * @param formInstances List<FormInstance>
     * @param multipleNumber String
     * @param workflow Workflow
     * @return List
     * @throws ToolRunException exception
     */
    public List<FormInstance> filterMultipleFormInstance(ReportDataContainer container,
                                                         List<FormInstance> formInstances,
                                                         String multipleNumber,
                                                         Workflow workflow)
            throws ToolRunException {
        List<FormInstance> instances = null;
        Report report = container.getReport();
        Class toolObjectClass = getToolObjectClass(report);

        if (toolObjectClass != null) {
            Class[] classArgs = new Class[] {List.class, String.class, Workflow.class, X2Broker.class};
            Object classObject = newInstance(toolObjectClass);
            Object[] objectArgs = new Object[] {formInstances, multipleNumber, workflow, getBroker()};
            instances = (List<FormInstance>) callReflection(METHOD_NAME_FILTER_MULTIPLE_FORM_INSTANCE, classObject,
                    classArgs, objectArgs);

        }
        return instances == null ? new ArrayList<FormInstance>() : instances;

    }

    /**
     * Returns true if the form being printed is blank.
     *
     * @return boolean
     */
    public boolean isBlank() {
        return m_isBlank;
    }

    /**
     * Prints the blank if instance doesnt exist.
     *
     * @param container ReportDataContainer
     * @param multipleNumber String
     * @param workFlow Workflow
     * @return true, if successful
     * @throws ToolRunException exception
     */
    public boolean printBlankIfInstanceDoesntExist(ReportDataContainer container,
                                                   String multipleNumber,
                                                   Workflow workFlow)
            throws ToolRunException {
        return false;
    }

    /**
     * Adds the psuedo instances.
     *
     * @param container ReportDataContainer
     * @param targetFormInstances List<FormInstance>
     * @throws ToolRunException exception
     */
    protected void addPsuedoInstances(ReportDataContainer container, List<FormInstance> targetFormInstances)
            throws ToolRunException {
        // Do nothing
    }

    /**
     * Adds the report infos.
     *
     * @param infos ReportDataInfos
     */
    protected void addReportInfos(ReportDataInfos infos) {
        // add using override
    }

    /**
     * Adds the reports.
     *
     * @param reportsContainer ReportDataContainers
     */
    protected void addReports(ReportDataContainers reportsContainer) {
        for (ReportDataInfo info : m_reportDataInfos) {
            reportsContainer.addById(info.getReportId(), info.getFormId(), info.getInputParams(),
                    info.getDefaultValues());
        }
    }

    /**
     * Gather data.
     *
     * @return Object
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        m_reportsContainersList = new LinkedList();
        for (X2BaseBean owner : m_ownerBeans) {
            ReportDataContainers reportsContainer = new ReportDataContainers(owner);
            m_reportsContainersList.add(reportsContainer);

            addReports(reportsContainer);

            reportsContainer.initContainerResources();
        }

        ReportDataGrid grid = new ReportDataGrid();
        grid.beforeTop();
        return grid;
    }

    /**
     * Gets the default values from input.
     *
     * @param tool Tool
     * @param userData UserDataContainer
     * @return Map
     * @throws ToolRunException exception
     */
    protected Map<String, Object> getDefaultValuesFromInput(Tool tool, UserDataContainer userData)
            throws ToolRunException {

        ToolInput toolInput = null;
        try {
            toolInput = new ToolInput(tool, null, userData, getLocale());
        } catch (X2BaseException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.toString());
            throw new ToolRunException(e);
        }

        ToolSourceCode sourceCode = tool.getSourceCode();
        String inputDefinition = sourceCode.getInputDefinition();
        if (inputDefinition != null && inputDefinition.length() > 0) {
            try {
                SAXBuilder builder = new SAXBuilder();
                Document document = builder.build(new ByteArrayInputStream(inputDefinition.getBytes()));
                Element root = document.getRootElement();
                Iterator children = root.getChildren().iterator();
                while (children.hasNext()) {
                    Element element = (Element) children.next();
                    if (element.getName().equals(INPUT_ELEMENT)) {
                        String defaultValue = element.getAttributeValue(INPUT_DEFAULT_VALUE_ATTRIB);
                        String name = element.getAttributeValue(INPUT_NAME_ATTRIB);
                        if (defaultValue != null) {
                            toolInput.setParameterAsString(name, defaultValue);
                        }
                    }
                }
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.SEVERE, e.toString());
                throw new ToolRunException(e);
            }
        }

        Map<String, Object> returnMap = toolInput.generateInputMap();
        return returnMap;
    }


    /**
     * Gets the report data containers.
     *
     * @return Report data containers
     */
    protected List<ReportDataContainers> getReportDataContainersList() {
        return m_reportsContainersList;
    }

    /**
     * Load reports by ids.
     *
     * @param ids List<String>
     * @return Map
     */
    protected Map<String, Report> loadReportsByIds(List<String> ids) {
        X2Criteria criteria = new X2Criteria();
        criteria.addIn(Report.COL_ID, ids);
        QueryByCriteria byCriteria = new QueryByCriteria(Report.class, criteria);

        return getBroker().getMapByQuery(byCriteria, Report.COL_ID, 40);
    }

    /**
     * Publish results.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#publishResults()
     */
    @Override
    protected void publishResults() throws Exception {
        boolean contentsOutput = false;

        boolean saveToDocuments = getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM) != null &&
                ((Boolean) getParameter(SAVE_TO_DOCUMENTS_IEP_PARAM)).booleanValue();

        logDataPrepared();

        ResultHandler resultHandler = getJob().getResultHandler();
        resultHandler.open(getJob(), null);
        OutputStream pdfOutputStream = resultHandler.getOutputStream();
        if (saveToDocuments) {
            pdfOutputStream = prepareSaveToDocuments(pdfOutputStream);
        }
        try {
            com.lowagie.text.Document document = new com.lowagie.text.Document();
            DocWriter writer =
                    getReportWriter(document, pdfOutputStream);
            document.open();
            boolean pageOutput = false;
            try {
                PdfContentByte cb = getPDFWriterContent(writer);
                for (ReportDataContainers containers : getReportDataContainersList()) {
                    for (ReportDataContainer container : containers) {
                        for (FormInstanceContainer formInstContainer : container.getListFormInstanceContainer()) {
                            String engineVersion = container.getReport().getEngineVersion();
                            Object data = formInstContainer.getDataSource(container.getMultipleNumber());
                            /*
                             * Check to see if gatherData returned a structure that contains no
                             * data. If it does, discard it so that a JREmptyDataSource is created
                             * and used below
                             */
                            if (data != null && data instanceof DataGrid) {
                                DataGrid dataSource = (DataGrid) data;
                                if (dataSource.getRows().isEmpty()) {
                                    data = null;
                                }
                            }

                            if (data == null) {
                                if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
                                    data = new net.sf.jasperreports.engine.JREmptyDataSource(0);
                                } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
                                    data = new net.sf.jasperreports3.engine.JREmptyDataSource(0);
                                } else if (Report.REPORT_ENGINE_5_RELEASE.equals(engineVersion)) {
                                    data = new net.sf.jasperreports5.engine.JREmptyDataSource(0);
                                } else {
                                    throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00008", new Object[] {
                                            engineVersion
                                    });
                                }
                            } else {
                                contentsOutput = true;
                            }

                            ThreadUtils.checkInterrupt();

                            /*
                             * If the job has been aborted then don't bother filling the format or
                             * exporting the
                             * results.
                             */
                            if (getJob().getStatus() != ToolJob.STATUS_ABORT) {
                                ByteArrayOutputStream stream =
                                        getFormattedReport(container, formInstContainer, engineVersion, data);
                                if (stream.size() > 0) {
                                    addSubReportToReport(stream, writer, document, cb);
                                    pageOutput = true;
                                }
                            }
                        }
                    }
                }
                if (!contentsOutput) {
                    getResultHandler().setEmpty(true);
                }
            } finally {
                if (pageOutput) {
                    document.close();
                    if (getJob().getInput().getFormat() != ToolInput.HTML_FORMAT) {
                        writer.close();
                    }
                }
            }
        } finally {
            resultHandler.close();
        }
        if (saveToDocuments) {
            saveToDocuments(pdfOutputStream);
        }
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#saveState(com.follett.fsc.core.k12.web.UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        /*
         * Retrieve the root Form instance from the form OID parameter if one exists. Otherwise
         * attempt to find one in the list hierarchy.
         */
        String formInstanceOid = (String) getParameter(PARAM_FORM_INSTANCE_OID);
        FormInstance formInstance = null;
        if (formInstanceOid != null) {
            formInstance = (FormInstance) getBroker().getBeanByOid(FormInstance.class, formInstanceOid);
        } else {
            formInstance = userData.getCurrentRecord(FormInstance.class);
        }

        /*
         * See if the current record is from the owner class
         */
        X2BaseBean formOwner = null;
        if (formInstance != null) {
            m_formDefinition = formInstance.getFormDefinition();
            formOwner = formInstance.getOwnerObject();
            m_ownerBeans = new ArrayList();
            m_ownerBeans.add(formOwner);
        } else {
            String formDefinitionOid = (String) getParameter(PARAM_FORM_DEFINITION_OID);
            if (formDefinitionOid != null) {
                m_formDefinition = (FormDefinition) getBroker().getBeanByOid(FormDefinition.class, formDefinitionOid);

                Class ownerClass = m_formDefinition.getOwnerDataTableConfig().getDataTable().getDataClass();
                formOwner = (X2BaseBean) userData.getCurrentRecord(ownerClass);
                if (formOwner == null) {
                    ContextList currentList = userData.getCurrentList();
                    if (currentList != null) {
                        BeanQuery query = currentList.getQuery();
                        if (ownerClass.equals(query.getBaseClass())) {
                            m_ownerBeans = getBroker().getCollectionByQuery(query);
                        } else {
                            // Blank Blank form
                            m_isBlank = true;
                            m_ownerBeans = new ArrayList();
                            m_ownerBeans.add(null);
                        }
                    }
                } else {
                    // Blank Owner or current record form
                    m_isBlank = true;
                    String navId = userData.getCurrentNode().getId();
                    Object tool = getJob().getTool();
                    if (!StringUtils.isEmpty(navId) && tool != null && Report.class.isAssignableFrom(tool.getClass())) {
                        Report report = (Report) tool;
                        for (ToolNav item : report.getToolNavs()) {
                            if (navId.equals(item.getNavId())) {
                                // Current record form
                                m_isBlank = false;
                                break;
                            }
                        }
                    }
                    m_ownerBeans = new ArrayList();
                    m_ownerBeans.add(formOwner);
                }
            }
        }
        addReportInfos(m_reportDataInfos);
        m_reportDataInfos.init(userData);
    }

    /**
     * Adds sub-report to main report(according to report type - PDF/HTML).
     *
     * @param stream ByteArrayOutputStream
     * @param writer DocWriter
     * @param document Document
     * @param cb PdfContentByte
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void addSubReportToReport(ByteArrayOutputStream stream,
                                      DocWriter writer,
                                      com.lowagie.text.Document document,
                                      PdfContentByte cb)
            throws IOException {
        switch (getJob().getInput().getFormat()) {
            case ToolInput.HTML_FORMAT:
                ((HtmlWriter) writer).add(new String(stream.toByteArray()));
                break;
            case ToolInput.PDF_FORMAT:
                ByteArrayInputStream inputStream = new ByteArrayInputStream(stream.toByteArray());
                PdfReader pdfReader = new PdfReader(inputStream);
                try {
                    for (int i = 1; i <= pdfReader.getNumberOfPages(); i++) {
                        Rectangle currentRectangle = pdfReader.getPageSize(i);
                        document.setPageSize(currentRectangle);
                        document.newPage();
                        PdfImportedPage page = ((PdfWriter) writer).getImportedPage(pdfReader, i);
                        cb.addTemplate(page, 0, 0);
                    }
                } finally {
                    pdfReader.close();
                }
                break;
        }

    }

    /**
     * Call reflection.
     *
     * @param methodName String
     * @param classObject Object
     * @param classArgs Class[]
     * @param objectArgs Object[]
     * @return Object
     * @throws ToolRunException exception
     */
    private Object callReflection(String methodName, Object classObject, Class[] classArgs, Object[] objectArgs)
            throws ToolRunException {
        Object returnValue = null;
        Method method = null;
        Class toolObjectClass = null;
        if (classObject instanceof Class) {
            toolObjectClass = (Class) classObject;
        } else {
            toolObjectClass = classObject.getClass();
        }

        List<Exception> listException = new ArrayList<Exception>();
        int i = 0;
        for (Class cls = toolObjectClass; method == null && cls != Object.class; cls = cls.getSuperclass()) {
            i++;
            try {
                method = cls.getDeclaredMethod(methodName, classArgs);
            } catch (Exception e) {
                listException.add(e);
            }
        }
        if (i > 0 && listException.size() == i) {
            AppGlobals.getLog().log(Level.SEVERE, listException.get(0).toString() + " Method Name " + methodName);
            throw new ToolRunException(listException.get(0));
        }
        if (method != null) {
            try {
                method.setAccessible(true);
                returnValue = method.invoke(classObject, objectArgs);
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.SEVERE, e.toString() + " Method Name " + methodName);
                throw new ToolRunException(e);
            }
        }

        return returnValue;
    }

    /**
     * Exports report to stream according to selected format(engine version - 1).
     *
     * @param data Object
     * @param formInstContainer FormInstanceContainer
     * @param outputStream ByteArrayOutputStream
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportReportEngine1(Object data,
                                     FormInstanceContainer formInstContainer,
                                     ByteArrayOutputStream outputStream)
            throws net.sf.jasperreports.engine.JRException,
            IOException {

        net.sf.jasperreports.engine.JRDataSource dataSource =
                (net.sf.jasperreports.engine.JRDataSource) data;

        net.sf.jasperreports.engine.JasperPrint reportPrint =
                net.sf.jasperreports.engine.JasperFillManager.fillReport(
                        formInstContainer.getJasperFormat(),
                        formInstContainer.getMapParam(),
                        dataSource);
        if (reportPrint != null && reportPrint.getPages().size() > 0) {
            if (dataSource instanceof ReportDataGrid) {
                ((ReportDataGrid) dataSource).applyJasperPrintParameters(reportPrint);
            }
            switch (getJob().getInput().getFormat()) {
                case ToolInput.HTML_FORMAT:
                    net.sf.jasperreports.engine.export.JRHtmlExporter htmlExporter =
                            new net.sf.jasperreports.engine.export.JRHtmlExporter();
                    htmlExporter
                            .setParameter(
                                    net.sf.jasperreports.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN,
                                    false);
                    try (Writer writer = new OutputStreamWriter(outputStream)) {
                        htmlExporter.setParameter(net.sf.jasperreports.engine.JRExporterParameter.JASPER_PRINT,
                                reportPrint);
                        htmlExporter.setParameter(net.sf.jasperreports.engine.JRExporterParameter.OUTPUT_WRITER,
                                writer);
                        htmlExporter
                                .setParameter(
                                        net.sf.jasperreports.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN,
                                        false);
                        htmlExporter.exportReport();
                    }
                    break;
                case ToolInput.PDF_FORMAT:
                    net.sf.jasperreports.engine.JasperExportManager.exportReportToPdfStream(reportPrint,
                            outputStream);
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Exports report to stream according to selected format(engine version -3).
     *
     * @param data Object
     * @param formInstContainer FormInstanceContainer
     * @param outputStream ByteArrayOutputStream
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportReportEngine3(Object data,
                                     FormInstanceContainer formInstContainer,
                                     ByteArrayOutputStream outputStream)
            throws net.sf.jasperreports3.engine.JRException,
            IOException {

        net.sf.jasperreports3.engine.JRDataSource dataSource =
                (net.sf.jasperreports3.engine.JRDataSource) data;

        net.sf.jasperreports3.engine.JasperPrint reportPrint =
                net.sf.jasperreports3.engine.JasperFillManager.fillReport(
                        formInstContainer.getJasperFormat(),
                        formInstContainer.getMapParam(),
                        dataSource);
        if (reportPrint != null && reportPrint.getPages().size() > 0) {
            if (dataSource instanceof ReportDataGrid) {
                ((ReportDataGrid) dataSource).applyJasperPrintParameters(reportPrint);
            }
            switch (getJob().getInput().getFormat()) {
                case ToolInput.HTML_FORMAT:
                    net.sf.jasperreports3.engine.export.JRHtmlExporter htmlExporter =
                            new net.sf.jasperreports3.engine.export.JRHtmlExporter();
                    htmlExporter
                            .setParameter(
                                    net.sf.jasperreports3.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN,
                                    false);
                    try (Writer writer = new OutputStreamWriter(outputStream)) {
                        htmlExporter.setParameter(net.sf.jasperreports3.engine.JRExporterParameter.JASPER_PRINT,
                                reportPrint);
                        htmlExporter.setParameter(net.sf.jasperreports3.engine.JRExporterParameter.OUTPUT_WRITER,
                                writer);
                        htmlExporter
                                .setParameter(
                                        net.sf.jasperreports3.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN,
                                        false);
                        htmlExporter.exportReport();
                    }
                    break;
                case ToolInput.PDF_FORMAT:

                    net.sf.jasperreports3.engine.JasperExportManager.exportReportToPdfStream(reportPrint,
                            outputStream);
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Exports report to stream according to selected format(engine version - 5).
     *
     * @param data Object
     * @param formInstContainer FormInstanceContainer
     * @param outputStream ByteArrayOutputStream
     * @throws JRException exception
     * @throws IOException Signals that an I/O exception has occurred.
     */
    private void exportReportEngine5(Object data,
                                     FormInstanceContainer formInstContainer,
                                     ByteArrayOutputStream outputStream)
            throws net.sf.jasperreports5.engine.JRException,
            IOException {

        net.sf.jasperreports5.engine.JRDataSource dataSource =
                (net.sf.jasperreports5.engine.JRDataSource) data;

        net.sf.jasperreports5.engine.JasperPrint reportPrint =
                net.sf.jasperreports5.engine.JasperFillManager.fillReport(
                        formInstContainer.getJasperFormat(),
                        formInstContainer.getMapParam(),
                        dataSource);
        if (reportPrint != null && reportPrint.getPages().size() > 0) {
            if (dataSource instanceof ReportDataGrid) {
                ((ReportDataGrid) dataSource).applyJasperPrintParameters(reportPrint);
            }
            switch (getJob().getInput().getFormat()) {
                case ToolInput.HTML_FORMAT:
                    net.sf.jasperreports5.engine.export.JRHtmlExporter htmlExporter =
                            new net.sf.jasperreports5.engine.export.JRHtmlExporter();
                    htmlExporter
                            .setParameter(
                                    net.sf.jasperreports5.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN,
                                    false);
                    try (Writer writer = new OutputStreamWriter(outputStream)) {
                        htmlExporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.JASPER_PRINT,
                                reportPrint);
                        htmlExporter.setParameter(net.sf.jasperreports5.engine.JRExporterParameter.OUTPUT_WRITER,
                                writer);
                        htmlExporter
                                .setParameter(
                                        net.sf.jasperreports5.engine.export.JRHtmlExporterParameter.IS_USING_IMAGES_TO_ALIGN,
                                        false);
                        htmlExporter.exportReport();
                    }
                    break;
                case ToolInput.PDF_FORMAT:
                    net.sf.jasperreports5.engine.JasperExportManager.exportReportToPdfStream(reportPrint,
                            outputStream);
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * Gets the formatted report.
     *
     * @param container ReportDataContainer
     * @param formInstContainer FormInstanceContainer
     * @param engineVersion String
     * @param data Object
     * @return Byte array output stream
     * @throws X2BaseException exception
     */
    private ByteArrayOutputStream getFormattedReport(ReportDataContainer container,
                                                     FormInstanceContainer formInstContainer,
                                                     String engineVersion,
                                                     Object data)
            throws X2BaseException {
        ByteArrayOutputStream outputStream = null;
        try {
            outputStream = new ByteArrayOutputStream();
            if (Report.REPORT_ENGINE_1_RELEASE.equals(engineVersion)) {
                if (data instanceof net.sf.jasperreports.engine.JRDataSource) {
                    exportReportEngine1(data, formInstContainer, outputStream);
                } else {
                    throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                            data.getClass().toString(), engineVersion
                    });
                }
            } else if (Report.REPORT_ENGINE_3_RELEASE.equals(engineVersion)) {
                if (data instanceof net.sf.jasperreports3.engine.JRDataSource) {
                    exportReportEngine3(data, formInstContainer, outputStream);
                } else {
                    throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                            data.getClass().toString(), engineVersion
                    });
                }
            } else if (Report.REPORT_ENGINE_5_RELEASE.equals(engineVersion)) {
                if (data instanceof net.sf.jasperreports5.engine.JRDataSource) {
                    exportReportEngine5(data, formInstContainer, outputStream);
                } else {
                    throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00007", new Object[] {
                            data.getClass().toString(), engineVersion
                    });
                }
            } else {
                throw new X2BaseException(AppGlobals.getLogResources(), "TLS-00008", engineVersion);
            }
        } catch (Exception e) {
            throw new X2BaseException(e);
        }
        return outputStream;
    }

    /**
     * Gets report content if format is PDF.
     *
     * @param writer DocWriter
     * @return Pdf content byte
     */
    private PdfContentByte getPDFWriterContent(DocWriter writer) {
        if (getJob().getInput().getFormat() == ToolInput.PDF_FORMAT) {
            return ((PdfWriter) writer).getDirectContent();
        }
        return null;
    }

    /**
     * Retrieves appropriate documentWriter according to report format.
     *
     * @param document Document
     * @param outputStream OutputStream
     * @return Doc writer
     * @throws DocumentException exception
     */
    private DocWriter getReportWriter(com.lowagie.text.Document document, OutputStream outputStream)
            throws DocumentException {
        switch (getJob().getInput().getFormat()) {
            case ToolInput.PDF_FORMAT:
                return PdfWriter.getInstance(document, outputStream);
            case ToolInput.HTML_FORMAT:
                return HtmlWriter.getInstance(document, outputStream);
        }
        return null;
    }

    /**
     * Gets the tool object class.
     *
     * @param report Report
     * @return Class
     * @throws ToolRunException exception
     */
    private Class getToolObjectClass(Report report) throws ToolRunException {

        Class toolObjectClass = null;
        boolean localTempFolder = false;

        ToolSourceCode sourceCode = report.getSourceCode();
        KeyValuePair<String, String> classNamePair =
                ByteArrayCompiler.extractClassName(sourceCode.getSourceCode().getBytes());
        String packageName = classNamePair.getKey();
        String simpleClassName = classNamePair.getValue();

        String className = simpleClassName;
        if (packageName != null) {
            className = packageName + POINT + simpleClassName;
        }
        byte[] compileCode = report.getSourceCode().getCompiledCode();
        File tempFolder = null;
        ByteArrayClassLoader classLoader;
        try {
            tempFolder = FolderUtils.createUniqueFolder(AppGlobals.getRootTemporaryFolder());
            localTempFolder = true;
            classLoader = new ByteArrayClassLoader(Report.class.getClassLoader(),
                    compileCode,
                    getBroker().getPersistenceKey().getDeploymentId(),
                    null);
            toolObjectClass = classLoader.loadClass(className);
        } catch (Throwable t) {
            AppGlobals.getLog().log(Level.SEVERE, t.toString());
            throw new ToolRunException(t);
        } finally {
            if (localTempFolder && tempFolder != null && tempFolder.exists()) {
                FolderUtils.recursiveRemoveDir(tempFolder, true);
            }
        }
        return toolObjectClass;
    }

    /**
     * New instance.
     *
     * @param tclass Class
     * @return Object
     * @throws ToolRunException exception
     */
    private Object newInstance(Class tclass) throws ToolRunException {
        Object classObject = null;
        try {
            classObject = tclass.getDeclaredConstructor().newInstance();
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
                | NoSuchMethodException | SecurityException e) {
            AppGlobals.getLog().log(Level.SEVERE, e.toString());
            throw new ToolRunException(e);
        }
        return classObject;
    }

    /**
     * Prepare save to documents.
     *
     * @param outputStream OutputStream
     * @return OutputStream
     * @throws FileNotFoundException exception
     */
    private OutputStream prepareSaveToDocuments(OutputStream outputStream) throws FileNotFoundException {
        m_saveToDocumentsResultFile = new File(
                getJob().getTempFolder() + File.separator + System.currentTimeMillis()
                        + Integer.toString(getJob().getJobId()) + "report.pdf");

        @SuppressWarnings("resource")
        FileOutputStream foStream = new FileOutputStream(m_saveToDocumentsResultFile);
        OutputStream oss[] = new OutputStream[2];
        oss[0] = outputStream;
        oss[1] = foStream;
        DuplicateOutputStream dos = new DuplicateOutputStream(oss);
        return dos;
    }

    /**
     * Save to documents.
     *
     * @param pdfOutputStream OutputStream
     */
    private void saveToDocuments(OutputStream pdfOutputStream) {
        // Create a Student Document record with the PDF set as the file source.
        if (m_saveToDocumentsResultFile.isFile()) {
            Student student = null;
            Person person = null;
            School school = null;
            if (m_ownerBeans != null && m_ownerBeans.size() > 0) {
                X2BaseBean owner = m_ownerBeans.iterator().next();
                if (owner instanceof IepData) {
                    student = ((IepData) owner).getStudent();
                    person = student.getPerson();
                    school = student.getSchool();
                }
                if (student != null && person != null) {

                    // Prepare some global variables so the method createDocument() will work.
                    // Find or create the document record.
                    com.follett.fsc.core.k12.beans.Document document =
                            createDocumentIEP(true, person, student, school, m_saveToDocumentsResultFile);

                    getBroker().saveBeanForced(document);
                }
            }
            m_saveToDocumentsResultFile.delete();
        }
    }

    /**
     * Retrieve a list of existing Documents for the student and document name.
     *
     * @param personOid String
     * @param documentName String
     * @return Map
     */
    private Collection<com.follett.fsc.core.k12.beans.Document> getExistingDocumentsIEP(String personOid,
                                                                                        String documentName) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(com.follett.fsc.core.k12.beans.Document.COL_PERSON_OID, personOid);
        criteria.addEqualTo(com.follett.fsc.core.k12.beans.Document.COL_NAME, documentName);
        QueryByCriteria query = new QueryByCriteria(com.follett.fsc.core.k12.beans.Document.class, criteria);

        return getBroker().getCollectionByQuery(query);
    }

    /**
     * Builds the document name.
     *
     * @param reportName String
     * @param context DistrictSchoolYearContext
     * @param dictionary DataDictionary
     * @return String
     */
    private String buildDocumentNameIEP(String reportName,
                                        DistrictSchoolYearContext context,
                                        DataDictionary dictionary) {
        String documentName;

        if (reportName == null) {
            return null;
        }

        DataDictionaryField documentNameField = null;
        if (dictionary != null) {
            documentNameField =
                    dictionary.findDataDictionaryField(
                            com.follett.fsc.core.k12.beans.Document.class.getName(),
                            com.follett.fsc.core.k12.beans.Document.COL_NAME);
        }

        documentName = reportName;

        if (context != null && context.getContextId() != null) {
            documentName = documentName + "_" + context.getContextId();
        }

        if (documentNameField != null) {
            if (documentName != null && documentName.length() > documentNameField.getDatabaseLength()) {
                documentName = documentName.substring(0, documentNameField.getDatabaseLength());
            }
        }

        return documentName;
    }

    /**
     * Creates the document.
     *
     * @param overwriteExisting boolean
     * @param person Person
     * @param student Student
     * @param school School
     * @param resultFile File
     * @return Document
     * @see com.follett.fsc.core.k12.tools.reports.SaveToDocuments#createDocument(boolean,
     *      com.follett.fsc.core.k12.beans.Person, com.follett.fsc.core.k12.beans.Student,
     *      java.io.File)
     */
    private com.follett.fsc.core.k12.beans.Document createDocumentIEP(boolean overwriteExisting,
                                                                      Person person,
                                                                      Student student,
                                                                      School school,
                                                                      File resultFile) {
        com.follett.fsc.core.k12.beans.Document document = null;

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField documentTypeField = dictionary.findDataDictionaryField(
                com.follett.fsc.core.k12.beans.Document.class.getName(),
                com.follett.fsc.core.k12.beans.Document.COL_TYPE_CODE);

        String reportName = (getParameter(DOCUMENT_NAME_PARAM) != null
                ? (String) getParameter(DOCUMENT_NAME_PARAM)
                : "IEPReport");
        String documentName = buildDocumentNameIEP(reportName, getCurrentContext(), dictionary);
        String documentType =
                getParameter(DOCUMENT_TYPE_PARAM) != null ? (String) getParameter(DOCUMENT_TYPE_PARAM) : "IEP";

        String fileName = student.getNameView() + "_" + student.getOid() + "_" + documentName + "_" + new PlainDate()
                + FolderUtils.FILE_EXTENSION_SEPARATOR + "pdf";

        if (documentType != null && documentType.length() > documentTypeField.getDatabaseLength()) {
            documentType = documentType.substring(0, documentTypeField.getDatabaseLength());
        }

        if (overwriteExisting) {
            Collection<com.follett.fsc.core.k12.beans.Document> documents =
                    getExistingDocumentsIEP(person.getOid(), documentName);
            document = getMostRecentDocumentIEP(documents);
        }

        if (document == null) {
            document = X2BaseBean.newInstance(com.follett.fsc.core.k12.beans.Document.class,
                    getBroker().getPersistenceKey());
            document.setPersonOid(person.getOid());
            document.setName(documentName);
            document.setFormatCode("pdf");
            document.setFilename(fileName);
        }

        document.setBinaryFile(resultFile);
        document.setTypeCode(documentType);
        document.setFieldValueByAlias(ALIAS_UPLOAD_DATE, getPlainDate().toString());
        document.setFieldValueByAlias(ALIAS_SCHOOL_ID,
                school.getSchoolId());
        return document;
    }

    /**
     * Gets the most recent document.
     *
     * @param documents Collection<Document>
     * @return Document
     */
    private com.follett.fsc.core.k12.beans.Document getMostRecentDocumentIEP(Collection<com.follett.fsc.core.k12.beans.Document> documents) {
        com.follett.fsc.core.k12.beans.Document mostRecentDocument = null;
        com.follett.fsc.core.k12.beans.Document document = null;
        if (documents != null) {
            for (com.follett.fsc.core.k12.beans.Document doc : documents) {
                if (document == null) {
                    mostRecentDocument = doc;
                } else {
                    if (doc.getLastModifiedTime() > document.getLastModifiedTime()) {
                        mostRecentDocument = doc;
                    }
                }
            }
        }

        return mostRecentDocument;
    }

    /**
     * A duplicating output stream.
     * Anything written to the output stream will be written to
     * multiple underlying output streams.
     *
     * @author Follett Software Company
     * @copyright 2021
     */
    private static class DuplicateOutputStream extends FilterOutputStream {
        private OutputStream m_streams[];

        /**
         * Constructor.
         * Provides a list of output streams to be written to.
         *
         * @param streams OutputStream[]
         */
        public DuplicateOutputStream(OutputStream streams[]) {
            super(streams[0]);
            m_streams = streams;
        }

        /**
         * Write to all underlying output streams.
         *
         * @param i int
         * @throws IOException Signals that an I/O exception has occurred.
         * @see java.io.FilterOutputStream#write(int)
         */
        @Override
        public void write(int i) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(i);
            }
        }

        /**
         * Write to all underlying output streams.
         *
         * @param b byte[]
         * @throws IOException Signals that an I/O exception has occurred.
         * @see java.io.FilterOutputStream#write(byte[])
         */
        @Override
        public void write(byte[] b) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b);
            }
        }

        /**
         * Write to all underlying output streams.
         *
         * @param b byte[]
         * @param offset int
         * @param length int
         * @throws IOException Signals that an I/O exception has occurred.
         * @see java.io.FilterOutputStream#write(byte[], int, int)
         */
        @Override
        public void write(byte[] b, int offset, int length) throws IOException {
            for (OutputStream os : m_streams) {
                os.write(b, offset, length);
            }
        }

        /**
         * Flush all underlying output streams.
         *
         * @throws IOException Signals that an I/O exception has occurred.
         * @see java.io.FilterOutputStream#flush()
         */
        @Override
        public void flush() throws IOException {
            for (OutputStream os : m_streams) {
                os.flush();
            }
        }

        /**
         * Close all underlying output streams.
         *
         * @throws IOException Signals that an I/O exception has occurred.
         * @see java.io.FilterOutputStream#close()
         */
        @Override
        public void close() throws IOException {
            for (OutputStream os : m_streams) {
                os.close();
            }
        }
    }
}
