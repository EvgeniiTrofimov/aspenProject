/*
 * ====================================================================
 * 
 * Follett Software Company
 * 
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 * 
 * ====================================================================
 */
package com.x2dev.procedures.sys.sped.ri;

import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.PrivilegeSet;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure;
import com.follett.fsc.core.k12.web.ApplicationContext;
import com.follett.fsc.core.k12.web.ChildDetailSet;
import com.follett.fsc.core.k12.web.EmbeddedListDetailSet;
import com.follett.fsc.core.k12.web.GenericDetail;
import com.follett.fsc.core.k12.web.GenericDetailForm;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.forms.FormDetail;
import com.follett.fsc.core.k12.web.template.Template;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.IepMeeting;
import com.x2dev.sis.model.beans.IepMeetingAttendance;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * The Class IepRiProcedure.
 */
public class TeamDefinitionRiProcedure implements DynamicFormProcedure {

    private List<GenericDetail> m_newDetails = null;

    /**
     * After save template.
     *
     * @param detail GenericDetail
     * @param userData UserDataContainer
     * @param broker ModelBroker
     * @return List
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#afterSaveTemplate
     */
    @Override
    public List<ValidationError> afterSaveTemplate(GenericDetail detail,
            UserDataContainer userData,
            ModelBroker broker) {
        List<ValidationError> errors = new ArrayList<ValidationError>();

        FormDetail formDetail = null;
        FormInstance formInstance = null;
        if (detail instanceof FormDetail) {
            formDetail = (FormDetail) detail;
            formInstance = formDetail.getFormInstance();
        }
        String iepDataOid = null;
        String stdOid = null;
        List<String> meetingOids = new ArrayList<String>();
        if (formInstance != null) {
            IepData iepData = (IepData) formInstance.getOwner();
            iepDataOid = iepData.getOid();
            stdOid = iepData.getStudentOid();
            for (IepMeeting meeting : iepData.getIepMeeting()) {
                meetingOids.add(meeting.getOid());
            }
        }

        for (GenericDetail newDetail : m_newDetails) {
            String oid = newDetail.getOid();

            for (String meetingOid : meetingOids) {
                IepMeetingAttendance attendance =
                        X2BaseBean.newInstance(IepMeetingAttendance.class, broker.getPersistenceKey());
                attendance.setIepDataOid(iepDataOid);
                attendance.setIepMeetingOid(meetingOid);
                attendance.setPrintInvitationIndicator(true);
                attendance.setTeamMemberOid(oid);
                attendance.setStudentOid(stdOid);
                errors.addAll(broker.saveBean(attendance));
            }
        }
        return errors;
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#initializeTemplate
     */
    @Override
    public void initializeTemplate(Template template,
                                   ApplicationContext applicationContext,
                                   DataDictionary dictionary,
                                   PrivilegeSet privilegeSet,
                                   Locale locale)
                                           throws X2BaseException {
        // Nothing needed
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#modifyForm
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
     * @see com.follett.fsc.core.k12.tools.procedures.DynamicFormProcedure#validateTemplate
     */
    @Override
    public List<ValidationError> validateTemplate(GenericDetailForm form,
            GenericDetail detail,
            UserDataContainer userData,
            ModelBroker broker) {
        m_newDetails = new ArrayList<GenericDetail>();
        List<ValidationError> errors = new ArrayList<ValidationError>();
        if (detail instanceof FormDetail) {
            FormDetail formDetail = (FormDetail) detail;
            EmbeddedListDetailSet embeddedList = getEmbeddedListById("members", formDetail);
            if (embeddedList != null) {
                for (String childId : embeddedList.getChildIdsToSave()) {
                    GenericDetail childDetail = embeddedList.getChildDetail(childId);
                    if (childDetail.isNew()) {
                        m_newDetails.add(embeddedList.getChildDetail(childId));
                    }
                }
            }
        }
        return errors;
    }

    private EmbeddedListDetailSet getEmbeddedListById(String detailId, GenericDetail detail) {
        EmbeddedListDetailSet embeddedList = null;
        for (String childDetailSetId : detail.getChildDetailSetIds()) {
            ChildDetailSet childDetailSet = detail.getChildDetailSet(childDetailSetId);
            if (detailId.equals(childDetailSetId) && childDetailSet instanceof EmbeddedListDetailSet) {
                embeddedList = (EmbeddedListDetailSet) childDetailSet;
            }
        }
        return embeddedList;
    }
}
