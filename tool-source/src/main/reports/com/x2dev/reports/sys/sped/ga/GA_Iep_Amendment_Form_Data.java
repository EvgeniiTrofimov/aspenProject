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
package com.x2dev.reports.sys.sped.ga;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.reports.BaseFormReportJavaSource;
import com.follett.fsc.core.k12.tools.reports.BeanCollectionDataSource;
import com.x2dev.sis.model.beans.IepAmendment;
import com.x2dev.sis.model.beans.IepAmendmentDetail;
import com.x2dev.sis.model.beans.IepData;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Java source for the IEP Amendment form.
 *
 * @author Follett Software Company
 */
public class GA_Iep_Amendment_Form_Data extends BaseFormReportJavaSource {
    /**
     * Report Constants
     */
    private static final long serialVersionUID = 1L;

    /**
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected Object gatherData() throws Exception {
        // TODO get name, school, etc
        // look for Rahul code.

        IepAmendment amendment = ((IepData) getFormOwner()).getIepAmendment();

        addParameter(PARAM_DICTIONARY, DataDictionary.getDistrictDictionary(
                amendment.getIepData().getExtendedDataDictionary(), getBroker().getPersistenceKey()));

        Collection<IepAmendmentDetail> amendmentDetail;

        if (isBlank()) {
            IepAmendmentDetail blankAmendmentDetail =
                    X2BaseBean.newInstance(IepAmendmentDetail.class, getBroker().getPersistenceKey());
            blankAmendmentDetail.setIepAmendmentOid(amendment.getOid());

            amendmentDetail = new ArrayList<IepAmendmentDetail>(3);
            for (int i = 0; i < 4; i++) {
                amendmentDetail.add(blankAmendmentDetail);
            }
        } else {
            Criteria criteria = new Criteria();
            criteria.addEqualTo(IepAmendmentDetail.COL_IEP_AMENDMENT_OID, amendment.getOid());

            QueryByCriteria query = new QueryByCriteria(IepAmendmentDetail.class, criteria);
            query.addOrderByAscending(IepAmendmentDetail.COL_SEQUENCE_NUMBER);

            amendmentDetail = getBroker().getCollectionByQuery(query);
        }

        BeanCollectionDataSource dataSource =
                new BeanCollectionDataSource(amendmentDetail, getDictionary(), getLocale());
        return dataSource;
    }
}
