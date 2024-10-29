/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.sis.model.beans.IepServiceLog;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import com.x2dev.utils.types.PlainTime;
import java.util.List;


/**
 * The Class CreateIepSeviceLog is used to create service logs for a selection of sevices.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class CreateIepSeviceLog extends ProcedureJavaSource {
    private static final String ALIAS_LOG_CREATOR = "all-isl-svsLogCreator";
    private static final String ALIAS_ICD10 = "isvICD10";
    private static final String INPUT_PARAM_DATE = "date";
    private static final String INPUT_PARAM_DURATION = "duration";
    private static final String INPUT_PARAM_COPIES = "copies";
    private static final String INPUT_PARAM_ICD10_CODE = "icd10Code";
    private static final String INPUT_PARAM_SERVICE_DELIVERY_CODE = "serviceDeliveryCode";
    private static final String INPUT_PARAM_START_TIME = "startTime";
    private static final String INPUT_PARAM_THERAPY_CODE = "therapyCode";
    private static final String INPUT_PARAM_ACTIVITY = "activity";

    private String m_nameView;
    private PrivilegeSet m_privilegeSet;
    private X2Criteria m_criteria;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        X2Broker modelBroker = new ModelBroker(m_privilegeSet);
        ReferenceCode rcdServiceDelivery = null;
        PlainTime startPlainTime = null;
        PlainDate date = (PlainDate) getParameter(INPUT_PARAM_DATE);
        Integer duration = (Integer) getParameter(INPUT_PARAM_DURATION);
        Integer copies = (Integer) getParameter(INPUT_PARAM_COPIES);
        String serviceDeliveryRcdOid = (String) getParameter(INPUT_PARAM_SERVICE_DELIVERY_CODE);
        String therapyRcdOids = (String) getParameter(INPUT_PARAM_THERAPY_CODE);
        String icd10Code = (String) getParameter(INPUT_PARAM_ICD10_CODE);
        String startTime = (String) getParameter(INPUT_PARAM_START_TIME);
        String activity = (String) getParameter(INPUT_PARAM_ACTIVITY);

        if (!StringUtils.isEmpty(serviceDeliveryRcdOid)) {
            List<String> rcdOids = StringUtils.convertDelimitedStringToList(serviceDeliveryRcdOid, ",", true);
            if (rcdOids.size() > 0) {
                rcdServiceDelivery = getBroker().getBeanByOid(ReferenceCode.class, rcdOids.get(0));
            }
        }
        StringBuilder therapyCodes = new StringBuilder();
        if (!StringUtils.isEmpty(therapyRcdOids)) {
            List<String> rcdOids = StringUtils.convertDelimitedStringToList(therapyRcdOids, ",", true);
            for (String rcdOid : rcdOids) {
                ReferenceCode rcdTherapy = getBroker().getBeanByOid(ReferenceCode.class, rcdOid);
                if (rcdTherapy != null) {
                    if (therapyCodes.length() > 0) {
                        therapyCodes.append(",");
                    }
                    therapyCodes.append(rcdTherapy.getCode());
                }
            }
        }
        try {
            Converter timeConverter =
                    ConverterFactory.getConverterForClass(Converter.TIME_CONVERTER, getLocale());
            startPlainTime = (PlainTime) timeConverter.stringToJava(startTime);
        } catch (RuntimeException e) {
            // invalid time format.
            logMessage("Start time invalid format (Requires hh:mm:ss AM/PM): " + startTime);
        }

        if (copies == null || copies.intValue() < 1) {
            copies = Integer.valueOf(1);
        }
        if (m_criteria == null) {
            logMessage("No Iep Services selected");
        } else if (date == null) {
            logMessage("No date provided");
        } else if (duration == null) {
            logMessage("No duration provided");
        } else if (rcdServiceDelivery == null) {
            logMessage("No service delivery code provided");
        } else if (startPlainTime == null) {
            logMessage("No start time provided  (Requires hh:mm AM/PM)");
        } else if (icd10Code == null) {
            logMessage("No ICD 10 provided");
        } else {
            BeanQuery query = new BeanQuery(IepService.class, m_criteria);
            try (QueryIterator iterator = modelBroker.getIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    IepService bean = (IepService) iterator.next();

                    for (int copy = 0; copy < copies.intValue(); copy++) {
                        IepServiceLog log =
                                X2BaseBean.newInstance(IepServiceLog.class, modelBroker.getPersistenceKey());
                        log.setIepServiceOid(bean.getOid());

                        if (copy == 0) { // Only put date on first record. other copies no date.
                            log.setDate(date);
                        }
                        log.setStartTime(startPlainTime);
                        log.setServiceDeliveryCode(rcdServiceDelivery.getCode());
                        log.setTherapyCode(therapyCodes.toString());
                        log.setDuration(duration.intValue());
                        log.setComments(activity);
                        log.setFieldValueByAlias(ALIAS_ICD10, icd10Code);
                        log.setFieldValueByAlias(ALIAS_LOG_CREATOR, m_nameView);

                        List<ValidationError> errors = null;
                        if (copy == 0) {
                            errors = modelBroker.saveBean(log);
                        } else {
                            modelBroker.saveBeanForced(log); // Logs without dates must be forced.
                        }
                        if (errors == null || errors.isEmpty()) {
                            logMessage("Created Service Log " + Integer.toString(copy + 1) + " for " + bean.getOid());
                        } else {
                            for (ValidationError error : errors) {
                                String[] details = error.getDetails();
                                if (details != null) {
                                    for (int i = 0; i < details.length; i++) {
                                        String string = details[i];
                                        logMessage("Service Log Creation Failed for " + bean.getOid() + "--" + string);
                                    }
                                } else if (error.getCause() != null) {
                                    if (error.getCause() instanceof Exception) {
                                        logMessage("Service Log Creation Failed for " + bean.getOid() + "--"
                                                + ((Exception) error.getCause()).getMessage());
                                    } else {
                                        logMessage("Service Log Creation Failed for " + bean.getOid() + "--"
                                                + error.getCause().toString());
                                    }
                                } else {
                                    logMessage("Service Log Creation Failed for " + bean.getOid() + "-- error:"
                                            + Integer.toString(error.getType()));
                                }
                            }
                            break; // If first copy fails, skip other copies.
                        }
                    }
                }
            }
        }
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
        m_nameView = userData.getUser().getNameView();
        m_privilegeSet = userData.getPrivilegeSet();
        IepService currentService = userData.getCurrentRecord(IepService.class);
        if (currentService == null) {
            ContextList currentList = userData.getCurrentList();
            if (currentList != null) {
                BeanQuery query = currentList.getQuery();
                if (query.getBaseClass().equals(IepService.class)) {
                    m_criteria = getCurrentCriteria();
                }
            }
        } else {
            m_criteria = new X2Criteria();
            m_criteria.addEqualTo(X2BaseBean.COL_OID, currentService.getOid());
        }
    }
}
