/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.il;

import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_DATA_SOURCE;
import static com.follett.fsc.core.k12.tools.reports.ReportConstants.FIELD_FORMAT;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataTableConfig;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.ToolSourceCode;
import com.follett.fsc.core.k12.beans.Workflow;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.Tool;
import com.follett.fsc.core.k12.tools.ToolInput;
import com.follett.fsc.core.k12.tools.ToolInputParameter;
import com.follett.fsc.core.k12.tools.ToolRunException;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.ByteArrayClassLoader;
import com.x2dev.utils.ByteArrayCompiler;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;

/**
 * Java source for a form-based report. All fields and aliases present on the form storage and owner
 * objects are available for use in the format. The storage and owner objects are retrieved and made
 * available by the superclass - <code>BaseFormReportJavaSource</code>.
 * <p>
 * In the report format, fields and aliases from the storage object can be accessed directly. To
 * retrieve values from the owner object, the prefix <b>"owner"</b> must be present on the field
 * or alias. See <code>SimpleFormDataSource</code> for more information.
 *
 * @author X2 Development Corporation
 */
public class BoundReports extends BaseFormReportJavaSource

{

    private static final String FIELD_PARAMETERS_MAP = "PARAMETERS_MAP";

    protected static String FORM_PREFIX = "SPED-IL-";

    public static final String INPUT_DEFAULT_VALUE_ATTRIB = "default-value";
    public static final String INPUT_ELEMENT = "input";
    public static final String INPUT_NAME_ATTRIB = "name";

    private static final String METHOD_NAME_FILTER_MULTIPLE_FORM_INSTANCE = "filterMultipleFormInstance";
    private static final String METHOD_NAME_PRINT_BLANK_IF_INSTANCE_DOESNT_EXIST = "printBlankIfInstanceDoesntExist";

    private static final String PARAM_CURRENT_OID = "currentOid";
    private static final String POINT = ".";
    private static final String PREFIX_IEP = "IEP";
    private static final String PREFIX_STD = "STD";
    protected static String REPORT_PREFIX = "SYS-SPED-IL-";


    private static final long serialVersionUID = 1L;

    private Workflow m_workflow = null;

    private ReportDataContainers m_reportsContainer = null;

    protected UserDataContainer m_userData = null;

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
        private static final String METHOD_NAME_PROCESS_MULTIPLE = "processMultiple";

        private JRDataSource m_dataSource = null;
        private Map<String, Object> m_dafaultParam = null;
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
            if (formInstance != null) {
                m_storageObject = formInstance.getStorageObject();
            } else {
                @SuppressWarnings("synthetic-access")
                FormDefinition formDefinition = m_reportsContainer.getFormDefinitionByFormId(container.getFormId());
                m_storageObject = createFormStorage(formDefinition);

            }
            m_dafaultParam = container.getCopiedDefaultParam();
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
        public JRDataSource getDataSource(String multipleNumber) throws ToolRunException {
            Class[] classArgs = new Class[] {String.class, Workflow.class};
            Object classObject = m_javaSource;
            Object[] objectArgs = new Object[] {multipleNumber, getWorkflow()};

            callReflection(METHOD_NAME_PROCESS_MULTIPLE, classObject, classArgs, objectArgs);
            m_dataSource = (JRDataSource) callReflection(METHOD_NAME_GATHER_DATA, classObject, new Class[] {},
                    new Object[] {});
            return m_dataSource;
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
            return m_dafaultParam;
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
                        DataDictionary.class, Map.class, X2Broker.class};
                Object classObject = newInstance(toolObjectClass);

                Object[] objectArgs = new Object[] {m_formInstance, m_storageObject, getFormOwner(),
                        container.getDataDictionary(), getMapParam(), getBroker()};

                callReflection(METHOD_NAME_INIT_BEAN_REPORT, classObject, classArgs, objectArgs);
                m_javaSource = classObject;
            }
        }


    }


    /**
     * Represent resources for tool input report
     * m_report - Report object
     * m_formId - form definition id
     * m_reportId - report Id
     * m_multipleNumber - multiple mode
     * m_dafaultParam - from toolInput
     * m_ddx - report Extended DataDictionayr
     * m_instsContainer - list FormInstanceContainer. FormInstanceContainer represent information
     * for one formInstance
     *
     * @author Follett Software Company
     *
     */
    public class ReportDataContainer {
        private Report m_report = null;
        private String m_formId = null;
        private String m_reportId = null;
        private String m_multipleNumber = null;
        private Map<String, Object> m_dafaultParam = null;
        private DataDictionary m_ddx = null;
        private List<FormInstanceContainer> m_instsContainer = new ArrayList<FormInstanceContainer>();

        /**
         * add FormInstanceContainer.
         *
         * @param formInstContainer FormInstanceContainer
         */
        public void addFormInstanceContainer(FormInstanceContainer formInstContainer) {
            m_instsContainer.add(formInstContainer);
        }

        /**
         * Gets the copied default param.
         *
         * @return Copied Default Param
         */
        public Map<String, Object> getCopiedDefaultParam() {
            return m_dafaultParam == null ? new HashMap<String, Object>() : new HashMap<String, Object>(m_dafaultParam);
        }

        /**
         * get DataDictionary .
         *
         * @return Data dictionary
         */
        public DataDictionary getDataDictionary() {
            return m_ddx;
        }

        /**
         * get Form Definition Id.
         *
         * @return String
         */
        public String getFormId() {
            return m_formId;
        }

        /**
         * Gets the list form instance container.
         *
         * @return all FormInstanceContainer belong to current workflow and form definition
         */
        public List<FormInstanceContainer> getListFormInstanceContainer() {
            return m_instsContainer;
        }

        /**
         * m_multiple mode.
         *
         * @return String
         */
        public String getMultipleNumber() {
            return m_multipleNumber;
        }

        /**
         *
         * @return Report
         */
        public Report getReport() {
            return m_report;
        }

        /**
         * Gets the report id.
         *
         * @return report Id
         */
        public String getReportId() {
            return m_reportId;
        }

        /**
         *
         * set Data Dictionary.
         *
         * @param ddx void
         */
        public void setDataDictionary(DataDictionary ddx) {
            m_ddx = ddx;
        }

        /**
         * set Default Param.
         *
         * @param dafaultParam Map<String,Object>
         */
        public void setDefaultParam(Map<String, Object> dafaultParam) {
            m_dafaultParam = dafaultParam;
        }

        /**
         * set From Definition Id.
         *
         * @param formId void
         */
        public void setFromId(String formId) {
            m_formId = formId;
        }

        /**
         * multiple Number mode.
         *
         * @param multipleNumber void
         */
        public void setMultipleNumber(String multipleNumber) {
            m_multipleNumber = multipleNumber;
        }

        /**
         * set Report.
         *
         * @param report void
         */
        public void setReport(Report report) {
            m_report = report;
        }

        /**
         * set Report Id.
         *
         * @param reportId void
         */
        public void setReportId(String reportId) {
            m_reportId = reportId;
        }
    }


    /**
     * List of ReportDataContainer with specific methods and members <br>
     * m_reportIds - list Report id <br>
     * m_formIds - list relevant Forms id <br>
     * m_reports map, key report ID, value -Report <br>
     * m_formInstancesMap map, key form Id, value Collection<FormInstance> from workflow where
     * FormInstance has the same formdefinition Id like key <br>
     * m_formDefinitionMap map, key form id, value FromDefinition <br>
     * one ReportDataContainer it is container for one reportId. <br>
     * one reportId has one <br>
     *
     * @author Follett Software Company
     *
     */
    public class ReportDataContainers extends ArrayList<ReportDataContainer> {
        private List<String> m_reportIds = new ArrayList<String>();
        private List<String> m_formIds = new ArrayList<String>();
        private Map<String, Report> m_reports = new HashMap<String, Report>();
        private Map<String, Collection<FormInstance>> m_formInstancesMap = null;
        private Map<String, FormDefinition> m_formDefinitionMap = null;

        /**
         * Instantiates a new report data containers.
         */
        public ReportDataContainers() {

        }

        /**
         * parse input param and add report id and multiple number.
         *
         * @param inputName should have next pattern REPORT_ID_MULT_*<br>
         *        _MULT_* can not be<br>
         */
        public void addByInputParam(String inputName) {

            ReportDataContainer container = new ReportDataContainer();
            String reportId = inputName.replaceFirst("_MULT.+", "");
            String multPart = inputName.replaceFirst(reportId, "");


            if (!StringUtils.isEmpty(multPart)) {
                String multNumber = multPart.replace("_MULT", "");
                if (!StringUtils.isEmpty(multNumber)) {
                    container.setMultipleNumber(multNumber);
                }
            }
            String formId = reportId.replaceFirst(REPORT_PREFIX, FORM_PREFIX);
            container.setFromId(formId);
            container.setReportId(reportId);
            m_reportIds.add(reportId);
            m_formIds.add(formId);
            add(container);
        }

        /**
         * Gets the form definition by form id.
         *
         * @param formId String
         * @return FormDefinition by formId
         */
        public FormDefinition getFormDefinitionByFormId(String formId) {
            FormDefinition formDefinition = null;
            if (m_formDefinitionMap != null) {
                formDefinition = m_formDefinitionMap.get(formId);
            }
            return formDefinition;
        }

        /**
         * init all container resources.
         *
         * @throws ToolRunException exception
         */
        public void initContainerResources() throws ToolRunException {

            loadReports();
            loadFormInstanceAndDefinition();

            for (ReportDataContainer container : this) {

                Report report = container.getReport();
                String formId = container.getFormId();

                Map<String, Object> dafaultParam = new HashMap<String, Object>();

                // getParameters() need for add param like "district" and other default param.
                // but in this implementation getParameters() return also unnecessary param from
                // current report
                // Now this does not prevent. If will - need to be changed
                dafaultParam.putAll(getDefaultValuesFromInput(report));
                dafaultParam.putAll(getDefaultToolInputMapValues());

                container.setDefaultParam(dafaultParam);

                FormDefinition formDefinition = getFormDefinitionByFormId(formId);
                ExtendedDataDictionary extendedDataDictionary = formDefinition.getExtendedDataDictionary();
                DataDictionary dataDictionary = DataDictionary.getDistrictDictionary(extendedDataDictionary,
                        getBroker().getPersistenceKey());
                container.setDataDictionary(dataDictionary);

                List<FormInstance> allFormInstances = getFormInstanceByFormId(formId);
                List<FormInstance> targetFormInstances = filterMultipleFormInstance(container, allFormInstances,
                        container.getMultipleNumber(), getWorkflow());

                if (targetFormInstances.size() > 0) {
                    for (FormInstance formInstance : targetFormInstances) {
                        new FormInstanceContainer(container, formInstance);

                    }
                } else if (printBlankIfInstanceDoesntExist(container, container.getMultipleNumber(), getWorkflow())) {
                    new FormInstanceContainer(container, null);
                }
            }
        }

        /**
         * Gets the default tool input map values.
         *
         * @return Map
         */
        private Map<String, Object> getDefaultToolInputMapValues() {
            List<String> defaultToolInputKeys =
                    new ArrayList<String>(Arrays.asList("district", "organization", "school_name",
                            "school", "currentContext", "schoolContext", "longDateFormat", "shortDateFormat" +
                                    "timeFormat",
                            "timestampFormat", "currentDate", "currentDate", "file", "locale"));
            @SuppressWarnings("synthetic-access")
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
         * load into map input param with default values<br>
         * key - input name, value - input default value.
         *
         * @param tool Tool
         * @return Map
         * @throws ToolRunException exception
         */
        private Map<String, Object> getDefaultValuesFromInput(Tool tool) throws ToolRunException {

            ToolInput toolInput = null;
            try {
                toolInput = new ToolInput(tool, null, m_userData, getLocale());
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
         * get Collection Form Instance from m_formInstances map <br>
         * key is Form Definition Id <br>
         * Initialize m_formInstances map before use this method <br>
         * .
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
         * initialize m_formInstances and m_formDefinition<br>
         * method using m_reports<br>
         * m_reports must be initialized<br>
         * .
         */
        private void loadFormInstanceAndDefinition() {

            if (getFormOwner() != null) {
                Criteria criteria = new Criteria();
                criteria.addEqualTo(FormInstance.COL_OWNER_OBJECT_OID, getFormOwner().getOid());
                criteria.addIn(FormInstance.REL_FORM_DEFINITION + ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID,
                        m_formIds);
                QueryByCriteria byCriteria = new QueryByCriteria(FormInstance.class, criteria);
                m_formInstancesMap =
                        getBroker().getGroupedCollectionByQuery(byCriteria, FormInstance.REL_FORM_DEFINITION +
                                ModelProperty.PATH_DELIMITER + FormDefinition.COL_ID, 50);

            }


            Criteria criteria = new Criteria();
            criteria.addIn(FormDefinition.COL_ID, m_formIds);
            QueryByCriteria byCriteria = new QueryByCriteria(FormDefinition.class, criteria);
            m_formDefinitionMap = getBroker().getMapByQuery(byCriteria, FormDefinition.COL_ID, 50);
        }



        /**
         * load m_reports map by m_reportIds.
         */
        private void loadReports() {
            Criteria criteria = new Criteria();
            criteria.addIn(Report.COL_ID, m_reportIds);
            QueryByCriteria byCriteria = new QueryByCriteria(Report.class, criteria);

            Map<String, Collection<Report>> reportsMap =
                    getBroker().getGroupedCollectionByQuery(byCriteria, Report.COL_ID, 40);
            for (ReportDataContainer container : this) {
                String reportId = container.getReportId();
                Collection<Report> reports = reportsMap.get(reportId);
                if (reports != null && !reports.isEmpty()) {
                    Report report = reports.iterator().next();
                    container.setReport(report);
                    m_reports.put(reportId, report);
                }
            }
        }

    }

    /**
     * call method filterMultipleFormInstance from report java class for current "container".
     *
     * @param container contain reports resources for current workflow for one form definition
     * @param formInstances list instance for current workflow for current container
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
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    public X2Broker getBroker() {
        return super.getBroker();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getLocale()
     */
    @Override
    public Locale getLocale() {
        // TODO Auto-generated method stub
        return super.getLocale();
    }

    /**
     * call method printBlankIfInstanceDoesntExist from report java class for current "container".
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
        Boolean returnValue = null;
        Report report = container.getReport();
        Class toolObjectClass = getToolObjectClass(report);

        if (toolObjectClass != null) {
            Class[] classArgs = new Class[] {String.class, Workflow.class, X2Broker.class};
            Object classObject = newInstance(toolObjectClass);
            Object[] objectArgs = new Object[] {multipleNumber, workFlow, getBroker()};
            returnValue = (Boolean) callReflection(METHOD_NAME_PRINT_BLANK_IF_INSTANCE_DOESNT_EXIST, classObject,
                    classArgs, objectArgs);
        }

        return returnValue == null ? false : returnValue.booleanValue();
    }


    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() throws Exception {
        ReportDataGrid grid = new ReportDataGrid();
        m_reportsContainer.initContainerResources();

        for (ReportDataContainer container : m_reportsContainer) {
            for (FormInstanceContainer formInstContainer : container.getListFormInstanceContainer()) {
                JRDataSource dataSource = formInstContainer.getDataSource(container.getMultipleNumber());

                if (dataSource != null && !dataSource.toString().equals("<<< NO RECORDS >>")) {
                    grid.append();
                    grid.set(FIELD_DATA_SOURCE, dataSource);
                    grid.set(FIELD_FORMAT, formInstContainer.getJasperFormat());
                    grid.set(FIELD_PARAMETERS_MAP, formInstContainer.getMapParam());

                }

            }
        }
        grid.beforeTop();
        return grid;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#initialize()
     */
    @Override
    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    protected void initialize() {
        initOwnerByParam(PARAM_CURRENT_OID);
        DataDictionary ddx = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        setDictionary(ddx);
    }

    /**
     * initialize Owner table <br>
     * in this case owner it is IepData<br>
     * .
     *
     * @param param <br>
     *        input name form INPUT DEFINITIONS<br>
     *        which contain currentDetail.oid from session<br>
     *        Work with SisStudent oid and IepData oid<br>
     */
    protected void initOwnerByParam(String param) {
        String currentOid = (String) getParameter(param);
        X2BaseBean owner = null;
        X2BaseBean currentRecord = m_userData.getCurrentRecord(IepData.class);
        if (currentRecord != null && currentRecord instanceof IepData) {
            owner = currentRecord;
        } else {
            if (currentOid.toUpperCase().startsWith(PREFIX_STD)) {
                SisStudent student = (SisStudent) getBroker().getBeanByOid(SisStudent.class, currentOid);
                for (IepData iepData : student.getIepData()) {
                    owner = iepData;
                }
            } else if (currentOid.toUpperCase().startsWith(PREFIX_IEP)) {
                owner = getBroker().getBeanByOid(IepData.class, currentOid);
            }
        }

        setFormOwner(owner);
    }

    /**
     * in this implementation logic parse input param. and add param name if value<br>
     * It find report param and put into ReportDataContainer if value is true for this param.<br>
     *
     * @param userData UserDataContainer
     */
    @Override
    protected void saveState(UserDataContainer userData) {
        m_reportsContainer = new ReportDataContainers();
        m_userData = userData;
        super.saveState(userData);
        ToolInput toolInput = userData.getToolInput();
        Iterator iterator = toolInput.getParameterNames().iterator();
        while (iterator.hasNext()) {
            String name = (String) iterator.next();
            Object tip = toolInput.getElement(name);

            if (tip != null) {
                Object value = toolInput.getParameterValue(name);

                if (tip instanceof ToolInputParameter) {
                    if (name != null && name.startsWith(REPORT_PREFIX) && value != null
                            && value.equals(Boolean.TRUE.toString())) {
                        m_reportsContainer.addByInputParam(name);

                    }
                }
            }
        }
    }

    /**
     * call method by reflection.
     *
     * @param methodName String
     * @param classObject Object
     * @param classArgs Class[]
     * @param objectArgs Object[]
     * @return Object
     * @throws ToolRunException exception
     */
    Object callReflection(String methodName, Object classObject, Class[] classArgs, Object[] objectArgs)
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
            AppGlobals.getLog().log(Level.SEVERE, listException.get(0).toString() + " Method Name" + methodName);
            throw new ToolRunException(listException.get(0));
        }
        if (method != null) {
            try {
                method.setAccessible(true);
                returnValue = method.invoke(classObject, objectArgs);
            } catch (Exception e) {
                AppGlobals.getLog().log(Level.SEVERE, e.toString() + " Method Name" + methodName);
                throw new ToolRunException(e);
            }
        }

        return returnValue;
    }

    /**
     * get tool object class from Report.
     *
     * @param report Report
     * @return Class
     * @throws ToolRunException exception
     */
    Class getToolObjectClass(Report report) throws ToolRunException {

        Class toolObjectClass = null;

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
        ByteArrayClassLoader classLoader;
        try {
            classLoader = new ByteArrayClassLoader(Report.class.getClassLoader(),
                    compileCode,
                    null, null);
            toolObjectClass = classLoader.loadClass(className);
        } catch (Throwable t) {
            AppGlobals.getLog().log(Level.SEVERE, t.toString());
            throw new ToolRunException(t);
        }
        return toolObjectClass;
    }

    /**
     * Gets the workflow.
     *
     * @return Workflow
     * @throws ToolRunException exception
     */
    Workflow getWorkflow() throws ToolRunException {
        X2BaseBean currentRecord = m_userData.getCurrentRecord(IepData.class);
        String iepDataOid = null;
        if (currentRecord != null && currentRecord instanceof IepData) {
            iepDataOid = currentRecord.getOid();
        } else if (getFormOwner() != null) {
            iepDataOid = getFormOwner().getOid();
        }


        if (m_workflow == null && iepDataOid != null) {

            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(Workflow.COL_OWNER_OID, iepDataOid);
            m_workflow = (Workflow) getBroker().getBeanByQuery(new QueryByCriteria(Workflow.class, criteria));

        }

        if (m_workflow == null) {
            try {
                iepDataOid.toString();
            } catch (NullPointerException e) {
                throw new ToolRunException(e);
            }
            try {
                m_workflow.getOid();
            } catch (NullPointerException e) {
                throw new ToolRunException(e);
            }
        }
        return m_workflow;
    }

    /**
     * create new Instance
     * require constructor without arguments.
     *
     * @param tclass Class
     * @return Object
     * @throws ToolRunException exception
     */
    Object newInstance(Class tclass) throws ToolRunException {
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


}
