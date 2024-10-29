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
package com.x2dev.reports.statereporting.on.register;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.reports.Publishable;
import com.x2dev.procedures.statereporting.common.DictionaryExtractor;
import com.x2dev.reports.statereporting.on.OnLocalizedReport;
import com.x2dev.sis.model.beans.path.SisBeanPaths;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Map;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class OnRegisterReportBase extends OnLocalizedReport implements Publishable {

    protected static final String ALIAS_RCD_IMAGE_BASE64 = "all-rcd-ImageBase64";

    // OnSis Images code constants
    protected static final String ON_MINISTRY_CODE_LOGO = "OnMinistry";
    protected static final String ON_TRILLIUM_CODE_LOGO = "OnTrillium";
    protected static final String ON_BOARD_CODE_LOGO = "OnBoardLogo";

    // Reference table constants
    protected static final String REF_OID_ON_SIS_IMAGES = "rtbOnImage    ";
    protected static final String REPORT_LOGO = "logoOntario";

    protected DictionaryExtractor m_dictionaryExtractor;

    /**
     * Get logo image in base64 string format.
     *
     * @param imageCode RCD_CODE for image
     * @param broker the broker
     * @return String - base64 value
     */
    public static String getBase64ImageString(String imageCode, X2Broker broker) {
        String base64Image = null;
        Map<String, ReferenceCode> referenceCodeMap = getBase64ImageCodeMap(broker);
        ReferenceCode rcdBean = referenceCodeMap.get(imageCode);
        if (rcdBean != null) {
            base64Image = (String) rcdBean.getFieldValueByAlias(ALIAS_RCD_IMAGE_BASE64);
        }
        return base64Image;
    }

    /**
     * Get map of ReferenceCode bean keyed on ReferenceCode.COL_CODE for
     * ON_SIS_IMAGES reference
     *
     * @param broker the broker
     * @return Map<String, ReferenceCode>
     */
    private static Map<String, ReferenceCode> getBase64ImageCodeMap(X2Broker broker) {
        Map<String, ReferenceCode> referenceCodeMap;
        X2Criteria imageCriteria = new X2Criteria();
        imageCriteria.addEqualTo(SisBeanPaths.REF_CODE.referenceTableOid().getPath(),
                REF_OID_ON_SIS_IMAGES);
        BeanQuery imageQuery = new BeanQuery(ReferenceCode.class, imageCriteria);
        referenceCodeMap = broker.getMapByQuery(imageQuery, SisBeanPaths.REF_CODE.code().getPath(), 10);
        return referenceCodeMap;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDataBreakColumn()
     */
    @Override
    public String getDataBreakColumn() {
        throw new IllegalStateException("getDataBreakColumn must have override if publishing is required");
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getDescription(com.follett.fsc.core.k12.beans.X2BaseBean)
     */
    @Override
    public String getDescription(X2BaseBean bean) {
        throw new IllegalStateException("getDescription must have override if publishing is required");
    }

    /**
     * Gets the email address. Will never be published so returning null.
     *
     * @param person the person
     * @return the email address
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailAddress(com.follett.fsc.core.k12.beans.Person)
     */
    @Override
    public String getEmailAddress(Person person) {
        return null;
    }

    /**
     * Gets the email recipients. Will never be published so returning null.
     *
     * @param bean the bean
     * @return the email recipients
     * @see com.follett.fsc.core.k12.tools.reports.Publishable#getEmailRecipients(com.follett.fsc.core.k12.beans.X2BaseBean)
     */
    @Override
    public Collection<Person> getEmailRecipients(X2BaseBean bean) {
        Collection<Person> recipients = null;

        return recipients;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        // TODO Auto-generated method stub
        return null;
    }
    /**
     * Gets the dictionary extractor.
     *
     * @return Dictionary extractor
     */
    protected DictionaryExtractor getDictionaryExtractor() {
        if (m_dictionaryExtractor == null) {
            m_dictionaryExtractor = new DictionaryExtractor(getBroker());
        }
        return m_dictionaryExtractor;
    }
    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        // add extra report parameters
        addParameter(REPORT_LOGO,
                getBase64ImageString(ON_TRILLIUM_CODE_LOGO, getBroker()));
    }

}
