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

import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Report;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.x2dev.procedures.sys.sped.ma.MaSpedAttribHelper;
import java.util.Locale;
import java.util.Map;

/**
 * The Class MaBeanReport.
 *
 * @author Follett Software Company
 * @copyright 2018
 */
public class MaBeanReport extends BaseFormReportJavaSource {
    private static final long serialVersionUID = 1L;
    /**
     * Report parameter indicating the report is not blank
     */
    public static final String PARAM_REPORT_IS_NOT_BLANK = "reportIsNotBlank";

    private static final String NO_PERFMON4J_INSTRUMENTATION = "";
    private static final String JR_PARAM_INIT_REPORTS = "initReport";

    protected X2Broker m_broker;
    protected DistrictSchoolYearContext m_currentContext = null;
    protected String m_engineVersion;
    protected FormInstance m_formInstance = null;
    protected Boolean m_initInstanse = Boolean.valueOf(false);
    protected Locale m_locLocale = null;
    protected Organization m_organization;
    protected Map<String, Object> m_params;

    /**
     * Gets the form instance.
     *
     * @return Form instance
     * @see com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource#getFormInstance()
     */
    @Override
    public FormInstance getFormInstance() {
        FormInstance formInstance = null;
        if (m_initInstanse.booleanValue()) {
            formInstance = m_formInstance;
        } else {
            formInstance = super.getFormInstance();
        }
        return formInstance;
    }

    /**
     * Inits the bean report.
     *
     * @param formInstance FormInstance
     * @param formStorage X2BaseBean
     * @param formOwner X2BaseBean
     * @param dictionary DataDictionary
     * @param params Map<String,Object>
     * @param organization Organization
     * @param currentContext DistrictSchoolYearContext
     * @param broker X2Broker
     * @param engineVersion String
     */
    public void initBeanReport(FormInstance formInstance,
                               X2BaseBean formStorage,
                               X2BaseBean formOwner,
                               DataDictionary dictionary,
                               Map<String, Object> params,
                               Organization organization,
                               DistrictSchoolYearContext currentContext,
                               X2Broker broker,
                               String engineVersion) {

        m_initInstanse = Boolean.TRUE;

        setFormStorage(formStorage);
        setFormOwner(formOwner);
        setDictionary(dictionary);
        setBroker(broker);
        m_params = params;
        addParameter(JR_PARAM_INIT_REPORTS, m_initInstanse);
        addParameter(PARAM_FORM_OWNER, formOwner);
        addParameter(PARAM_FORM_STORAGE, formStorage);
        addParameter(PARAM_DICTIONARY, dictionary);
        addParameter(PARAM_BLANK, Boolean.valueOf(isBlank()));
        m_formInstance = formInstance;
        m_organization = organization;
        m_currentContext = currentContext;
        m_engineVersion = engineVersion;
    }

    /**
     * Adds the parameter.
     *
     * @param key String
     * @param value Object
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#addParameter(java.lang.String,
     *      java.lang.Object)
     */
    @Override
    protected void addParameter(String key, Object value) {
        if (m_initInstanse.booleanValue()) {
            m_params.put(key, value);
        } else {
            super.addParameter(key, value);
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
        if (isBlank()) {
            return gatherEmptyReportData();
        }
        MaSpedAttribHelper helper = new MaSpedAttribHelper(getBroker(), true);
        return helper.getMaSpedDataSource(getFormStorage(), getFormOwner(), getDictionary(), getLocale());
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    @Override
    protected X2Broker getBroker() {
        if (m_broker == null) {
            m_broker = super.getBroker();
        }
        return m_broker;
    }

    /**
     * Gets the current context.
     *
     * @return District school year context
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getCurrentContext()
     */
    @Override
    protected DistrictSchoolYearContext getCurrentContext() {
        // TODO Auto-generated method stub
        return m_currentContext == null ? super.getCurrentContext() : m_currentContext;
    }

    /**
     * Gets the locale.
     *
     * @return Locale
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getLocale()
     */
    @Override
    protected Locale getLocale() {
        Locale locale = null;
        if (m_initInstanse.booleanValue()) {
            if (m_locLocale == null) {
                m_locLocale = Locale.getDefault();
            }
            locale = m_locLocale;
        } else {
            locale = super.getLocale();
        }
        return locale;
    }

    /**
     * Gets the organization.
     *
     * @return Organization
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getOrganization()
     */
    @Override
    protected Organization getOrganization() {
        return m_organization == null ? super.getOrganization() : m_organization;
    }

    /**
     * Gets the parameter.
     *
     * @param key String
     * @return Object
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getParameter(java.lang.String)
     */
    @Override
    protected Object getParameter(String key) {
        Object returnObject = null;
        if (m_initInstanse.booleanValue()) {
            returnObject = m_params.get(key);
        } else {
            returnObject = super.getParameter(key);
        }
        return returnObject;
    }

    /**
     * Gets the parameters.
     *
     * @return Map
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getParameters()
     */
    @Override
    protected Map getParameters() {
        Map returnMap;
        if (m_initInstanse.booleanValue()) {
            returnMap = m_params;
        } else {
            returnMap = super.getParameters();
        }
        return returnMap;
    }

    /**
     * Report is not blank.
     *
     * @return true, if successful
     */
    protected boolean reportIsNotBlank() {
        boolean iepNotBlankReport = true;

        if (getParameter(PARAM_REPORT_IS_NOT_BLANK) != null) {
            iepNotBlankReport = ((Boolean) getParameter(PARAM_REPORT_IS_NOT_BLANK)).booleanValue();
        }
        return iepNotBlankReport;
    }

    /**
     * Sets the broker.
     *
     * @param broker void
     */
    protected void setBroker(X2Broker broker) {
        m_broker = broker;
    }

    /**
     * Returns 'JREmptyDataSource' object according to report's engine version.
     *
     * @return Object
     */
    private Object gatherEmptyReportData() {
        if (m_engineVersion == null) {
            m_engineVersion = ((Report) getJob().getTool()).getEngineVersion();
        }
        if (Report.REPORT_ENGINE_1_RELEASE.equals(m_engineVersion)) {
            return new dori.jasper.engine.JREmptyDataSource();
        } else if (Report.REPORT_ENGINE_3_RELEASE.equals(m_engineVersion)) {
            return new net.sf.jasperreports3.engine.JREmptyDataSource();
        } else if (Report.REPORT_ENGINE_5_RELEASE.equals(m_engineVersion)) {
        	return new net.sf.jasperreports5.engine.JREmptyDataSource();
        } else if (Report.REPORT_ENGINE_6_RELEASE.equals(m_engineVersion)) {
            return new net.sf.jasperreports6.engine.JREmptyDataSource();
        }
        return new net.sf.jasperreports5.engine.JREmptyDataSource();
    }
}
