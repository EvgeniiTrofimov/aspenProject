/*
 *
 */
package com.x2dev.reports.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.FormDefinition;
import com.follett.fsc.core.k12.beans.OrganizationLocale;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.ToolJob;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.reports.SimpleFormDataSource;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.utils.KeyValuePair;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.NotSerializableException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.struts.util.MessageResources;

/**
 * The Class IepFormData.
 */
public class IepListData extends ReportJavaSourceNet {

    private static final long serialVersionUID = 1L;

    private static final String NODE_ID_IEP_IEP_LIST = "iep.iep.list";
    private static final String NODE_ID_IEP_IEP_LIST_DETAIL = "iep.iep.list.detail";
    private static final String NODE_ID_STD_STD_LIST_IEP_DETAIL = "student.std.list.iep.detail";

    private static final String IEP_REPORT_ID = "SYS-SPED-MA-IEP-N";
    private static final String GOAL_REPORT_ID = "SYS-SPED-MA-IEP-GOALS";
    private static final String ADM1_REPORT_ID = "SYS-SPED-MA-ADM1";

    private static final String PARAM_IEP_REPORT_FORMAT = "iepReportFormat";
    private static final String PARAM_GOAL_REPORT_FORMAT = "goalsormat";

    private static final String PARAM_IEP_REPORT_DATA = "iepDataSource";
    private static final String PARAM_IEP_REPORT_PARAMETERS = "iepParameters";
    private static final String PARAM_GOALS_REPORT_DATA = "goalsDataSource";

    protected static final List<KeyValuePair<String, String>> SUB_REPORT_FORMATS = Arrays.asList(
            new KeyValuePair<String, String>(IEP_REPORT_ID, PARAM_IEP_REPORT_FORMAT),
            new KeyValuePair<String, String>(GOAL_REPORT_ID, PARAM_GOAL_REPORT_FORMAT));

    private List<IepData> m_currentIeps;
    private UserDataContainer m_userData;

    // Localization
    private Map<String, String> m_validLocales;
    private String m_defaultLocale; // Usually English
    private MessageResources m_default_message_resource;
    private Locale m_userLocale;
    private String m_userLocaleLanguage;
    private static final String PARAM_PREFIX = "prefix";
    private static final String PARAM_REPORT_LOCALE = "reportLocale";
    private static final String PARAM_LOCALES = "locales";
    private static final String PARAM_DEFAULT_LOCALE = "default_locale";
    private static final String PARAM_DEFAULT_LANGUAGE = "default_language";
    private static final String CONST_TOOLS_FOR_PREFIX = "tools.";
    private static final String CONST_AMERICAN_ENGLISH_LOCALE = "en_US";
    private static final String CONST_AMERICAN_ENGLISH_LANGUAGE = "English";

    private static final String RESPONSE_PAGE_ONLY_PARAM = "responseOnly";

    /**
     * The Class RewindableReportDataGrid, make all ReportDataGrid rewindable.
     */
    class RewindableReportDataGrid extends ReportDataGrid 
    implements net.sf.jasperreports5.engine.JRRewindableDataSource,
               net.sf.jasperreports6.engine.JRRewindableDataSource {
        /**
         * @see net.sf.jasperreports5.engine.JRRewindableDataSource#moveFirst()
         */
        @Override
        public void moveFirst() {
            super.beforeTop();
        }
    }

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        initSubReports();
        addParameter(RESPONSE_PAGE_ONLY_PARAM, Boolean.FALSE);
        Map<String, Object> initialParameters = new HashMap<String, Object>();
        initialParameters.putAll(getParameters());
        ReportDataGrid dataGrid = new RewindableReportDataGrid();
        BaseFormReportJavaSource formReportData = getIepReportTool();
        if (formReportData != null && m_currentIeps != null) {
            for (IepData iep : m_currentIeps) {
                SimpleFormDataSource dataSource = gatherReportForIep(iep, formReportData, initialParameters);
                Map<String, Object> iepParameters = new HashMap<String, Object>();
                iepParameters.putAll(formReportData.copyParameterMap(null));
                dataGrid.append();
                dataGrid.set(PARAM_IEP_REPORT_DATA, dataSource);
                dataGrid.set(PARAM_IEP_REPORT_PARAMETERS, iepParameters);
            }
        }
        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Save state.
     *
     * @param userData UserDataContainer
     * @throws X2BaseException
     *
     * @see com.x2dev.sis.tools.reports.BaseFormReportJavaSource#saveState(com.x2dev.sis.web.
     *      UserDataContainer)
     */
    @Override
    protected void saveState(UserDataContainer userData) throws X2BaseException {
        super.saveState(userData);
        List<String> listNodes =
                new ArrayList<String>(Arrays.asList(NODE_ID_IEP_IEP_LIST));
        List<String> detailsNodes =
                new ArrayList<String>(Arrays.asList(NODE_ID_IEP_IEP_LIST_DETAIL, NODE_ID_STD_STD_LIST_IEP_DETAIL));
        if (listNodes.contains(userData.getCurrentNode().getId())) {
            m_currentIeps = new ArrayList(userData.getCurrentList().getAllRecords());
        } else if (detailsNodes.contains(userData.getCurrentNode().getId())) {
            m_currentIeps = new ArrayList();
            IepData iep = (IepData) userData.getCurrentRecord(IepData.class);
            m_currentIeps.add(iep);
        }

        m_userData = userData;

        m_userLocale = userData.getLocale();
        m_userLocaleLanguage = userData.getLocale().getDisplayLanguage();
    }

    /**
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        // Enabling localization
        initializeLocalized();
    }

    /**
     * Initializes for localization.
     *
     * Adds the localization parameters
     * Populates the Valid Locales map
     */
    private void initializeLocalized() {
        Collection<OrganizationLocale> locales = getOrganization().getRootOrganization().getLocales();
        Map<String, MessageResources> resources = new HashMap<String, MessageResources>();
        m_validLocales = new HashMap<String, String>();

        if (m_userLocale != null) {
            m_default_message_resource =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey(), m_userLocale);
        } else {
            m_default_message_resource = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                    LocalizationCache.getCurrentLocale());
        }

        if (StringUtils.isBlank(m_userLocaleLanguage)) {
            m_userLocaleLanguage = LocalizationCache.getCurrentLocale().getDisplayLanguage();
        }


        for (OrganizationLocale loc : locales) {
            if (loc.getEnabledIndicator()) {
                MessageResources messages = LocalizationCache.getMessages(getBroker().getPersistenceKey(),
                        loc.getLocale());
                // save the messages for that language
                resources.put(loc.getLocale(), messages);

                // populate the map of valid locales
                m_validLocales.put(loc.getName(), loc.getLocale());
                if (loc.getPrimaryIndicator()) {
                    m_defaultLocale = loc.getLocale();
                }
            }
        }

        if (m_defaultLocale == null) {
            m_defaultLocale = CONST_AMERICAN_ENGLISH_LOCALE;
        }
        addParameter(PARAM_PREFIX, CONST_TOOLS_FOR_PREFIX + getJob().getTool().getOid() + ".");
        addParameter(PARAM_REPORT_LOCALE, m_default_message_resource);
        // Additional hooks for enhanced implementations
        addParameter(PARAM_LOCALES, resources);
        addParameter(PARAM_DEFAULT_LOCALE, CONST_AMERICAN_ENGLISH_LOCALE);
        addParameter(PARAM_DEFAULT_LANGUAGE, CONST_AMERICAN_ENGLISH_LANGUAGE);
    }

    /**
     * Set the IEP into the existing report instance, causing the report instance to re-initialize
     * with the new IEP and prepare to generate report data.
     *
     * We do this by using an existing public setter method on the core class to pass
     * the IEP OID into the report instance. The ON IEP report data class is set to find
     * this IEP OID and reinitialize with it.
     *
     * @param iepData
     * @param reportInstanace
     */
    private SimpleFormDataSource gatherReportForIep(IepData iepData, BaseFormReportJavaSource reportInstance, Map<String, Object> initialParameters) {
        FormDefinition formDef = X2BaseBean.newInstance(FormDefinition.class, getBroker().getPersistenceKey());
        formDef.setCategory(iepData.getOid());
        try {
			reportInstance.copyParameterMap(initialParameters);
		} catch (InstantiationException | IllegalAccessException | CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        reportInstance.setFormDefinition(formDef); // triggers re-initialization.
        SimpleFormDataSource dataSource = (SimpleFormDataSource) reportInstance.getDataSource(); // triggers re-generation of
        return dataSource;
    }

    /**
     * Load the IEP report data java class and return the initialized report instance.
     *
     * It must be returned as a generic BaseFormReportJavaSource because it will be
     * loaded in a separate classloader, so we cannot access the direct class through
     * current classloader.
     *
     * (I wanted to just include the source code in this module and instantiate it directly,
     * but we cannot get to all of the initialization methods.)
     *
     * @return ReportJavaSource
     */
    private BaseFormReportJavaSource getIepReportTool() {
        File tempFolder = m_userData.getUserTempFolder();
        if (tempFolder == null) {
            tempFolder = AppGlobals.getSecureRootDirectory(getOrganization(),
                    m_userData.getPersistenceKey().getDeploymentId());
        }

        // Load the IEP report tool.
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(Report.COL_ID, IEP_REPORT_ID);
        BeanQuery query = new BeanQuery(Report.class, criteria);
        Report iepReportTool = getBroker().getBeanByQuery(query);

        BaseFormReportJavaSource iepReportData = null;
        if (iepReportTool != null) {
            // Use a ToolJob to initialize the report.
            // We cannot access many of the initialization methods directly.
            // Only ToolJOb can do this.
            ToolJob iepReportJob = null;
            try {
                iepReportJob = ToolJob.createJob(iepReportTool, m_userData, tempFolder, false, getLocale());
            } catch (NotSerializableException | X2BaseException e) {
                addCustomErrorMessage("Unable to load report " + IEP_REPORT_ID + ": " + e.getMessage());
            }
            // Get the initialized report instance from the ToolJob.
            if (iepReportJob != null) {
            	iepReportData = (BaseFormReportJavaSource) iepReportJob.getToolJavaSource();
            }
        }
        return iepReportData;
    }

    /**
     * Load subreports into parameters.
     */
    private void initSubReports() {
        Report report = (Report) getJob().getTool();
        for (KeyValuePair<String, String> item : SUB_REPORT_FORMATS) {
            try {
                byte[] compiledFormat = ReportUtils.getReport(item.getKey(), getBroker()).getCompiledFormat();
                Object loadedJRReport = null;
                if (report.getEngineVersion().startsWith("5.5")) { 
                	loadedJRReport = net.sf.jasperreports5.engine.util.JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
                } else if (report.getEngineVersion().startsWith("6.19")) {
                	loadedJRReport = net.sf.jasperreports6.engine.util.JRLoader.loadObject(new ByteArrayInputStream(compiledFormat));
                }
                addParameter(item.getValue(), loadedJRReport);
            } catch (net.sf.jasperreports5.engine.JRException | net.sf.jasperreports6.engine.JRException e) {
                String message = "ERROR: Loading subreport for '" + item.getValue() + "' from report " + item.getKey();
                message += "\n" + e.getMessage();
                this.addCustomErrorMessage(message);
            }
        }
    }
}