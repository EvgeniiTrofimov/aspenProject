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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource;
import com.follett.fsc.core.k12.web.ContextList;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.IepService;
import com.x2dev.utils.X2BaseException;
import java.util.List;

/**
 * The Class ChangeIepServiceProvider is used to alter the service provider for one or more
 * services. It intentionally bypasses the protection in the UI preventing update of services once
 * the IEP is active.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class ChangeIepServiceProvider extends ProcedureJavaSource {
    private static final String INPUT_PARAM_STAFF = "staff";

    private IepService m_currentService;
    private UserDataContainer m_userData;

    /**
     * Execute.
     *
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.tools.procedures.ProcedureJavaSource#execute()
     */
    @Override
    protected void execute() throws Exception {
        X2Broker modelBroker = new ModelBroker(m_userData.getPrivilegeSet());
        X2Criteria criteria = null;
        if (m_currentService == null) {
            ContextList currentList = m_userData.getCurrentList();
            if (currentList != null) {
                BeanQuery query = currentList.getQuery();
                if (query.getBaseClass().equals(IepService.class)) {
                    criteria = getCurrentCriteria();
                }
            }
        } else {
            criteria = new X2Criteria();
            criteria.addEqualTo(X2BaseBean.COL_OID, m_currentService.getOid());
        }
        if (criteria == null) {
            logMessage("No Iep Services selected");
        } else {
            String staffOid = (String) getParameter(INPUT_PARAM_STAFF);
            BeanQuery query = new BeanQuery(IepService.class, criteria);
            try (QueryIterator iterator = modelBroker.getIteratorByQuery(query)) {
                while (iterator.hasNext()) {
                    IepService bean = (IepService) iterator.next();
                    bean.setStaffOid(staffOid);
                    List<ValidationError> errors = modelBroker.saveBean(bean);
                    if (errors == null || errors.isEmpty()) {
                        logMessage("Updated Service " + bean.getOid());
                    } else {
                        for (ValidationError error : errors) {
                            String[] details = error.getDetails();
                            for (int i = 0; i < details.length; i++) {
                                String string = details[i];
                                logMessage("Updated Service Failed " + bean.getOid() + "--" + string);
                            }
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
        m_userData = userData;
        m_currentService = userData.getCurrentRecord(IepService.class);
    }

}
