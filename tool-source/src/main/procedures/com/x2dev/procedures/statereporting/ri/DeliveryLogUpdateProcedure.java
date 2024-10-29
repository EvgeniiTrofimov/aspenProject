/*
 * ====================================================================
 *
 * Follett School Solutions
 *
 * Copyright (c) Follett School Solutions
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett School Solutions.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepData.StatusCode;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepServiceLog;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.types.PlainDate;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Procedure for creating IEP_SERVICE_LOG records based on IEP_SERVICE data.
 *
 * @author Follett School Solutions
 */
public class DeliveryLogUpdateProcedure extends ProcedureJavaSource {
    private static final String DELIVERY_LOG_ALIAS = "cst-isv-del-log";
    private static final String MEETING_DAYS_ALIAS = "cst-isv-days-meet";

    private static final String EXTENDED_DICTIONARY_ID_PARAM = "extendedDictionary";

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {

        String extendedDictionaryId = (String) getParameter(EXTENDED_DICTIONARY_ID_PARAM);

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDictionaryId);

        QueryByCriteria query = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(query);

        DataDictionary dictionary = DataDictionary.getDistrictDictionary(extendedDictionary,
                getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryFieldByAlias(DELIVERY_LOG_ALIAS);

        PlainDate dateToday = new PlainDate();

        Criteria servicesCriteria = new Criteria();

        servicesCriteria.addLessOrEqualThan(IepService.COL_START_DATE, dateToday);
        servicesCriteria.addGreaterOrEqualThan(IepService.COL_END_DATE, dateToday);
        servicesCriteria.addEqualTo(field.getJavaName(), "1");
        servicesCriteria.addEqualTo(IepService.REL_IEP_DATA + ModelProperty.PATH_DELIMITER + IepData.COL_STATUS_CODE,
                new Integer(StatusCode.ACTIVE.ordinal()));
        String activeCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.SPED_ACTIVE_CODE);
        servicesCriteria.addEqualTo(
                IepService.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_SPED_STATUS_CODE, activeCode);

        QueryByCriteria servicesQuery = new QueryByCriteria(IepService.class, servicesCriteria);
        servicesQuery.addOrderByAscending(IepService.COL_STUDENT_OID);
        servicesQuery.addOrderByAscending(IepService.COL_SERVICE_MODE);

        QueryIterator servicesIterator = getBroker().getIteratorByQuery(servicesQuery);

        PlainDate serviceStartDate;
        PlainDate serviceEndDate;

        String iepServiceOid;
        String servicedays;
        String serviceDeliveryCode;
        String serviceProvider;
        String serviceType;

        String SimpleDayFormat = "EEEEEEEEEE";

        DateFormat dayFormat = new SimpleDateFormat(SimpleDayFormat);

        Date date = new Date();
        String dayToday = dayFormat.format(date);

        Arrays.asList(IepData.StatusCode.values());

        try {
            while (servicesIterator.hasNext()) {
                IepService service = (IepService) servicesIterator.next();
                service.getIepData();
                SisStudent student = service.getStudent();

                servicedays = (String) service.getFieldValueByAliasExtended(MEETING_DAYS_ALIAS, dictionary);
                serviceDeliveryCode = service.getServiceDeliveryCode();
                serviceProvider = service.getProviderCode();
                serviceStartDate = service.getStartDate();
                serviceEndDate = service.getEndDate();
                serviceType = service.getServiceType();

                Collection<PlainDate> sessionDates = null;

                sessionDates = CalendarManager.getInSessionDates(serviceStartDate, serviceEndDate, student,
                        getBroker());

                iepServiceOid = service.getOid();

                if (sessionDates.contains(dateToday) && servicedays != null && servicedays.contains(dayToday)) {

                    IepServiceLog newSet = X2BaseBean.newInstance(IepServiceLog.class, getBroker().getPersistenceKey());
                    newSet.setIepServiceOid(iepServiceOid);
                    newSet.setDate(dateToday);
                    newSet.setServiceDeliveryCode(serviceDeliveryCode);

                    Criteria servicesLogExistsCriteria = new Criteria();
                    servicesLogExistsCriteria.addEqualTo(IepServiceLog.COL_IEP_SERVICE_OID, iepServiceOid);
                    servicesLogExistsCriteria.addEqualTo(IepServiceLog.COL_DATE, dateToday);

                    if (serviceDeliveryCode != null) {
                        servicesLogExistsCriteria.addEqualTo(IepServiceLog.COL_SERVICE_DELIVERY_CODE,
                                serviceDeliveryCode);
                    } else {
                        servicesLogExistsCriteria.addIsNull(IepServiceLog.COL_SERVICE_DELIVERY_CODE);
                    }

                    servicesLogExistsCriteria.addIsNull(IepServiceLog.COL_THERAPY_CODE);
                    servicesLogExistsCriteria.addIsNull(IepServiceLog.COL_COMMENTS);

                    QueryByCriteria servicesLogExistsQuery = new QueryByCriteria(IepServiceLog.class,
                            servicesLogExistsCriteria);

                    int servicesLogExistsCount = getBroker().getCount(servicesLogExistsQuery);

                    if (servicesLogExistsCount == 0) {
                        getBroker().saveBeanForced(newSet);
                        logMessage("1 record was created for " + student.getNameView() + "; type = " + serviceType
                                + " provider = " + serviceProvider);
                    }
                }
            }
        } finally {
            servicesIterator.close();
        }
    }
}
