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
package com.x2dev.procedures.statereporting.va;

import static com.follett.fsc.core.k12.business.ValidationConstants.CUSTOM_ERROR;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * @author Follett Software Company
 * @copyright 2022
 */
public class VAConductTemplateProcedure implements DynamicFormProcedure {

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.fsc.core.k12.web.GenericDetail, com.follett.fsc.core.k12.web.UserDataContainer, com.follett.fsc.core.k12.business.ModelBroker)
     */
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        // No action
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(com.follett.fsc.core.k12.web.template.Template, com.follett.fsc.core.k12.web.ApplicationContext, com.follett.fsc.core.k12.business.dictionary.DataDictionary, com.follett.fsc.core.k12.business.PrivilegeSet, java.util.Locale)
     */
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        // No action
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(com.follett.fsc.core.k12.web.GenericDetail, java.lang.String, java.lang.String, com.follett.fsc.core.k12.web.UserDataContainer, com.follett.fsc.core.k12.web.template.Template, java.util.List)
     */
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        // No action
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.fsc.core.k12.web.GenericDetailForm, com.follett.fsc.core.k12.web.GenericDetail, com.follett.fsc.core.k12.web.UserDataContainer, com.follett.fsc.core.k12.business.ModelBroker)
     */
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        List<ValidationError> errors = null;
        boolean is99 = false;
        int count = 0;
        Class detailClass = detail.getRootClass();
        if (detailClass.equals(ConductIncident.class)) {
            // An incident detail.
            ChildDetailSet childSet = detail.getChildDetailSet("actions");
            count = childSet.getChildDetails().size();
            if (childSet != null) {
                for (GenericDetail child : childSet.getChildDetails()) {
                    String code = (String) child.getValue("actActionCode");
                    if ("99".equals(code)) {
                        is99 = true;
                    }
                }
            }
        } else if (detailClass.equals(ConductAction.class)) {
            String actionOid = detail.getOid();
            X2Criteria actionCriteria = new X2Criteria();
            actionCriteria.addEqualTo(X2BaseBean.COL_OID, actionOid);
            SubQuery actionQuery = new SubQuery(ConductAction.class, ConductAction.COL_INCIDENT_OID, actionCriteria);
            X2Criteria incidentCriteria = new X2Criteria();
            incidentCriteria.addIn(ConductAction.COL_INCIDENT_OID, actionQuery);
            SubQuery query = new SubQuery(ConductAction.class, ConductAction.COL_ACTION_CODE, incidentCriteria);
            Collection<Object> results = broker.getSubQueryCollectionByQuery(query);
            for (Object result : results) {
                count++;
                if ("99".equals(result)) {
                    is99 = true;
                }
            }
        }
        if (is99 && count > 1) {
            String errorMessage = "An action code of 99 can not have other actions in the same incident.";
            errors = new ArrayList<ValidationError>(); 
            errors.add(new ValidationError(CUSTOM_ERROR, null, errorMessage));
        }
            
        return errors;
    }
}
