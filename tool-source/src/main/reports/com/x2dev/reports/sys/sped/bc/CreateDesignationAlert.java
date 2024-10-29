/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2014 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.reports.sys.sped.bc;

import com.follett.fsc.core.k12.beans.StudentAlert;
import com.follett.fsc.core.k12.beans.StudentAlert.AlertType;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Creates, updates, or deletes a student alert based on disabilities (designations) for BC.
 *
 * @author X2 Development Corporation
 */
public class CreateDesignationAlert implements DynamicFormProcedure {
    private static final String DESIGNATION_ALERT_ICON_PATH = "alertIcons/designation.png";
    private static final String DESIGNATION_ALERT_MESSAGE =
            "Student designation on file. Please go to the Student Services view and select IEP and Designations.";

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate(
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
                                                   UserDataContainer userData,
                                                   ModelBroker broker) {
        List<ValidationError> validationErrors = new ArrayList<ValidationError>();

        String disabilityOid = detail.getOid();
        if (!StringUtils.isEmpty(disabilityOid)) {
            IepDisability disability = (IepDisability) broker.getBeanByOid(IepDisability.class, disabilityOid);
            if (disability != null) {
                if (disability.getPrimaryIndicator()) {
                    validationErrors = createOrUpdateStudentAlert(disability.getStudentOid(), validationErrors, broker);
                } else {
                    validationErrors = deleteStudentAlert(disability.getStudentOid(), validationErrors, broker);
                }
            }
        }

        return validationErrors;
    }

    /**
     * Initialize template.
     *
     * @param template Template
     * @param applicationContext ApplicationContext
     * @param dictionary DataDictionary
     * @param privilegeSet PrivilegeSet
     * @param locale Locale
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate(
     *      com.follett.fsc.core.k12.web.template.Template,
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
        // Not needed
    }

    /**
     * Modify form.
     *
     * @param detail GenericDetail
     * @param key String
     * @param value String
     * @param userData UserDataContainer
     * @param template Template
     * @param errors List
     * @return Map
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm(
     *      com.follett.fsc.core.k12.web.GenericDetail, java.lang.String, java.lang.String,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.web.template.Template,
     *      java.util.List)
     */
    @Override
    public Map<String, Object> modifyForm(GenericDetail detail,
                                          String key,
                                          String value,
                                          UserDataContainer userData,
                                          Template template,
                                          List errors)
            throws X2BaseException {
        return null;
    }

    /**
     * Validate template.
     *
     * @param form GenericDetailForm
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate(
     *      com.follett.fsc.core.k12.web.GenericDetailForm,
     *      com.follett.fsc.core.k12.web.GenericDetail,
     *      com.follett.fsc.core.k12.web.UserDataContainer,
     *      com.follett.fsc.core.k12.business.ModelBroker)
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
                                                  GenericDetail detail,
                                                  UserDataContainer userData,
                                                  ModelBroker broker) {
        return null;
    }

    /**
     * Returns the student alert criteria.
     *
     * @param studentOid String
     * @return Criteria
     */
    private static Criteria getStudentAlertCriteria(String studentOid) {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentAlert.COL_STUDENT_OID, studentOid);
        criteria.addEqualTo(StudentAlert.COL_ALERT_TYPE, Integer.valueOf(AlertType.OTHER.ordinal()));
        criteria.addEqualTo(StudentAlert.COL_ICON_FILENAME, DESIGNATION_ALERT_ICON_PATH);

        return criteria;
    }

    /**
     * Creates a new student alert for primary disability if one wasn't already present, or, updates
     * existing alert
     * with designation.
     *
     * @param studentOid String
     * @param validationErrors List<ValidationError>
     * @param broker ModelBroker
     * @return List&lt;ValidationErrors&gt;
     */
    private List<ValidationError> createOrUpdateStudentAlert(String studentOid,
                                                             List<ValidationError> validationErrors,
                                                             ModelBroker broker) {
        Criteria criteria = getStudentAlertCriteria(studentOid);
        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);

        StudentAlert designationAlert = (StudentAlert) broker.getBeanByQuery(query);
        if (designationAlert == null) {
            designationAlert = X2BaseBean.newInstance(StudentAlert.class, broker.getPersistenceKey());
            designationAlert.setStudentOid(studentOid);
            designationAlert.setAlertType(AlertType.OTHER.ordinal());
            designationAlert.setIconFilename(DESIGNATION_ALERT_ICON_PATH);
        }

        designationAlert.setAlertDescription(DESIGNATION_ALERT_MESSAGE);

        if (designationAlert.isNew() || designationAlert.isDirty()) {
            validationErrors.addAll(broker.saveBean(designationAlert));
        }

        return validationErrors;
    }

    /**
     * Deletes old designation alert, if present.
     *
     * @param studentOid String
     * @param validationErrors List<ValidationError>
     * @param broker ModelBroker
     * @return List&lt;ValidationErrors&gt;
     */
    private List<ValidationError> deleteStudentAlert(String studentOid,
                                                     List<ValidationError> validationErrors,
                                                     ModelBroker broker) {
        Criteria criteria = getStudentAlertCriteria(studentOid);
        QueryByCriteria query = new QueryByCriteria(StudentAlert.class, criteria);

        StudentAlert oldAlert = (StudentAlert) broker.getBeanByQuery(query);
        if (oldAlert != null) {
            validationErrors.addAll(broker.deleteBean(oldAlert));
        }

        return validationErrors;
    }
}
