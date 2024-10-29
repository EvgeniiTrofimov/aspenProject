/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationConstants;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class PAConductTemplateProcedure implements DynamicFormProcedure {

    private List<String> m_actionCodesS3S9 = Arrays.asList(new String[] {"S3", "S4", "S5", "S6", "S7", "S8", "S9"});
    private Map<String, String> m_incidentStateCodes;
    private Map<String, String> m_actionStateCodes;

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(com.follett.fsc.core.k12.web.template.Template,
     *      com.follett.fsc.core.k12.web.ApplicationContext,
     *      com.follett.fsc.core.k12.business.dictionary.DataDictionary,
     *      com.follett.fsc.core.k12.business.PrivilegeSet, java.util.Locale)
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
            throws X2BaseException {
        // TODO Auto-generated method stub

    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(com.follett.fsc.core.k12.web.GenericDetail,
     *      java.lang.String, java.lang.String, com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template, java.util.List)
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {

        loadReferenceCodes(userData);
        String btnValue = (String) detail.getValueByAlias("DOE ALT ED");
        String incidentCode = (String) detail.getValue("cndIncident");
        String incidentStateCode = m_incidentStateCodes.get(incidentCode);
        ChildDetailSet actionDetails = detail.getChildDetailSet("actions");
        Collection<GenericDetail> actions = actionDetails.getChildDetails();
        Boolean r1Exists = false;
        Boolean s3s9Exists = false;
        List<ValidationError> errors = new ArrayList<>();

        for (GenericDetail action : actions) {
            String actionCode = (String) action.getValue("actActionCode");
            String actionStateCode = m_actionStateCodes.get(actionCode);
            if (actionCode.equals("R1")) {
                r1Exists = true;
            }
            if (m_actionCodesS3S9.contains(actionStateCode)) {
                s3s9Exists = true;
            }
        }

        if (r1Exists && !btnValue.equals("true")) {
            errors.add(new ValidationError(ValidationConstants.CUSTOM_ERROR, null,
                    "If Alternative Education is false, then Remedial Actions cannot contain Alternative Education"));
        }

        if (!r1Exists && btnValue.equals("true")) {
            errors.add(new ValidationError(ValidationConstants.CUSTOM_ERROR, null,
                    "If Alternative Education is true, then Remedial Actions must contain Alternative Education"));
        }
        // if action state code is in S3-S9, then incident state code cannot be null.
        if (s3s9Exists && StringUtils.isEmpty(incidentStateCode)) {
            errors.add(new ValidationError(ValidationConstants.CUSTOM_ERROR, null,
                    "If Action code S3 - S9 are used, the Incident must be a state reportable incident code"));
        }

        return errors;
    }

    /**
     * Load maps of incident codes to state code and action codes to state code.
     *
     * @param userData
     */
    private void loadReferenceCodes(UserDataContainer userData) {
        if (m_actionStateCodes == null || m_incidentStateCodes == null) {
            ModelBroker broker = new ModelBroker(userData);
            DataDictionary dictionary = DataDictionary.getDistrictDictionary(userData);

            DataDictionaryField field = dictionary.findDataDictionaryField(ConductIncident.class.getName(),
                    ConductIncident.COL_INCIDENT_CODE);
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(broker);
                m_incidentStateCodes = new HashMap<String, String>();
                for (ReferenceCode code : codes) {
                    m_incidentStateCodes.put(code.getCode(), code.getStateCode());
                }
            }

            field = dictionary.findDataDictionaryField(ConductAction.class.getName(),
                    ConductAction.COL_ACTION_CODE);
            refTable = field.getReferenceTable();
            if (refTable != null) {
                Collection<ReferenceCode> codes = refTable.getReferenceCodes(broker);
                m_actionStateCodes = new HashMap<String, String>();
                for (ReferenceCode code : codes) {
                    m_actionStateCodes.put(code.getCode(), code.getStateCode());
                }
            }
        }
    }

}
