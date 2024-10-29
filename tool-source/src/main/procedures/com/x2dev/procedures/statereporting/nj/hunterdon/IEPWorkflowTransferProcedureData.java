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
package com.x2dev.procedures.statereporting.nj.hunterdon;

import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.FormInstance;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.beans.WorkflowProgress;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.MessageProperties;
import com.follett.fsc.core.k12.business.ValidationError;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.IepData;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentSchedule;
import com.x2dev.sis.model.business.sped.SpedUtils;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehavior;
import com.x2dev.sis.model.business.sped.SpedWorkflowBehaviorFactory;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2010 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Defines special behavior that occurs on the special education IEP transfer workflow.
 * <p>
 * By default, all behavior is defined in the business layer. Custom workflow behavior can be
 * defined directly in this procedure. When adding behavior beyond what is defined in the business
 * layer, provide a detailed description of the changes made.
 *
 * @author X2 Development Corporation
 */
public class IEPWorkflowTransferProcedureData extends WorkflowProcedure {
    private static final long serialVersionUID = 1L;
    private SpedWorkflowBehavior m_behavior = null;
    private static final String ALIAS_ATTEND_NOT_EXEMPT = "iep-attend-not-exempt";
    private static final String ALIAS_CASE_MANAGER = "Case Manager";
    private static final String ALIAS_CREDHR_NOT_EXEMPT = "iep-credhr-not-exempt";
    private static final String ALIAS_HSPSRA_NOT_EXEMPT = "iep-hspsra-not-exempt";
    private static final String ALIAS_OTHER_NOT_EXEMPT = "iep-other-not-exempt";

    /**
     * Constructs a new SpedIepTransferProcedure.
     *
     * @param definition WorkflowDefinition
     * @param district Organization
     * @param user User
     * @param broker X2Broker
     * @param locale Locale
     */
    public IEPWorkflowTransferProcedureData(WorkflowDefinition definition, Organization district,
            User user, X2Broker broker, Locale locale) {
        super(definition, district, user, broker, locale);
        m_behavior = SpedWorkflowBehaviorFactory.getWorkflowBehavior(getOrganization());
    }

    /**
     * Implements the draft IEP using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementIep(WorkflowProgress progress) throws Exception {
        IepData iepOrg = (IepData) progress.getWorkflow().getOwner();
        PlainDate OrigNextEvaluationDate = iepOrg.getNextEvaluationDate();

        List<ValidationError> errors =
                m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        } else {
            IepData iep = (IepData) progress.getWorkflow().getOwner();
            // Re-set the Next Evaluation Date back to its original
            iep.setNextEvaluationDate(OrigNextEvaluationDate);
            getBroker().saveBeanForced(iep);
            // Update the Case Manager Name on the Student when the IEP gets saved.
            String staffName = iep.getStaff().getNameView();
            Student student = iep.getStudent();
            student.setFieldValueByAlias(ALIAS_CASE_MANAGER, staffName);
            getBroker().saveBeanForced(student);
        }
    }

    /**
     * Implements the draft IEP (with email notification) using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void executeImplementNotifyIep(WorkflowProgress progress) throws Exception {
        IepData iepOrg = (IepData) progress.getWorkflow().getOwner();
        PlainDate OrigNextEvaluationDate = iepOrg.getNextEvaluationDate();

        List<ValidationError> errors =
                m_behavior.executeImplementIep(progress, getOrganization(), getLocale(), getBroker());
        if (errors.size() > 0) {
            addValidationErrors(errors);
        } else {
            IepData iep = (IepData) progress.getWorkflow().getOwner();
            // Re-set the Next Evaluation Date back to its original
            iep.setNextEvaluationDate(OrigNextEvaluationDate);
            getBroker().saveBeanForced(iep);
            // Update the Case Manager Name on the Student when the IEP gets saved.
            String staffName = iep.getStaff().getNameView();
            Student student = iep.getStudent();
            student.setFieldValueByAlias(ALIAS_CASE_MANAGER, staffName);
            getBroker().saveBeanForced(student);

            List<String> recp = buildRecipientsList((IepData) progress.getWorkflow().getOwner(), iep.getStudent());
            String message = getOrganization().getName() + " is sending this electronic notification to inform you:\n"
                    + "\n"
                    + "The IEP belonging to " + student.getPerson().getFirstName() + " "
                    + student.getPerson().getLastName()
                    + " is now active. You are required to be familiar with the IEP and to assist the student in meeting the goals, including the implementation of any modifications specified in the student's IEP.\n"
                    + "\n"
                    + "Please take a few minutes to review the active IEP Plan at your earliest convenience.\n"
                    + "\n"
                    + "Thank you.";

            sendemail(recp, message);
        }
    }



    /**
     * Initialize owner.
     *
     * @param selectionBean X2BaseBean
     * @param formInstances Map<String,FormInstance>
     * @param date PlainDate
     * @param workflowDefinition WorkflowDefinition
     * @return X2BaseBean
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#initializeOwner(com.follett.fsc.
     *      core.k12.beans.X2BaseBean, java.util.Map, PlainDate,
     *      com.follett.fsc.core.k12.beans.WorkflowDefinition)
     */
    @Override
    public X2BaseBean initializeOwner(X2BaseBean selectionBean,
                                      Map<String, FormInstance> formInstances,
                                      PlainDate date,
                                      WorkflowDefinition workflowDefinition) {
        IepData iep = m_behavior.executeTransfer((SisStudent) selectionBean, formInstances, getLocale(), getBroker());

        // set default values for fields on extended dictionary
        DataDictionary dictionary = getDataDictionary();
        iep.setFieldValueByAlias(ALIAS_ATTEND_NOT_EXEMPT, BooleanAsStringConverter.TRUE, dictionary);
        iep.setFieldValueByAlias(ALIAS_CREDHR_NOT_EXEMPT, BooleanAsStringConverter.TRUE, dictionary);
        iep.setFieldValueByAlias(ALIAS_HSPSRA_NOT_EXEMPT, BooleanAsStringConverter.TRUE, dictionary);
        iep.setFieldValueByAlias(ALIAS_OTHER_NOT_EXEMPT, BooleanAsStringConverter.TRUE, dictionary);

        return iep;
    }

    /**
     * Owner exists before initiation.
     *
     * @return true, if successful
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure#ownerExistsBeforeInitiation()
     */
    @Override
    public boolean ownerExistsBeforeInitiation() {
        return false;
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementIep(progress, getOrganization(), getLocale(), getBroker());
    }

    /**
     * Rolls back changes made by the implement IEP method using the default sped workflow behavior.
     * During rollback, no notification is sent to the user.
     *
     * @param progress WorkflowProgress
     * @throws Exception exception
     * @see com.follett.fsc.core.k12.business.sped.SpedDefaultWorkflowBehavior
     */
    public void rollbackImplementNotifyIep(WorkflowProgress progress) throws Exception {
        m_behavior.rollbackImplementIep(progress, getOrganization(), getLocale(), getBroker());
    }


    /*
     * Effect: Sends e-mail to people within recipient list
     */

    /**
     * Sendemail.
     *
     * @param staffemaillist List<String>
     * @param message String
     */
    private void sendemail(List<String> staffemaillist, String message) {
        // WriteEmailManager manager = new WriteEmailManager(getOrganization(),
        // getCurrentOwnerOid(), getCurrentOwnerType(), getUser());
        WriteEmailManager manager = new WriteEmailManager(getOrganization());

        if (manager.connect()) {
            try {
                MessageProperties msg = new MessageProperties(null, null, staffemaillist, null, null, "IEP Alert",
                        message, "text/plain", null, null, null);
                manager.sendMail(msg);

            } finally {
                manager.disconnect();
            }
        }
    }

    /**
     * Helper method to retrieve the data dictionary.
     *
     * @return DataDictionary
     */
    private DataDictionary getDataDictionary() {
        ExtendedDataDictionary extendedDataDictionary = SpedUtils.getIepDictionary(getOrganization(), getBroker());
        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        return dictionary;
    }

    /**
     * Collects a list of recipients to receive an e-mail notification of an IEP or ED Plan
     * acceptance, including all current teachers and IEP/Ed Plan team members (which presumably
     * include the student's primary and possibly secondary contacts).
     *
     * @param iep IepData
     * @param student SisStudent
     * @return List<String>
     */
    private List<String> buildRecipientsList(IepData iep,
                                             SisStudent student) {
        List<String> recipientsList = new ArrayList<String>();

        // add teacher emails
        Collection<StudentSchedule> schedules = student.getStudentSchedules(getBroker());


        Collection<ScheduleTeacher> allStaff = null;
        for (StudentSchedule schedule : schedules) {
            if (schedule != null) {
                Schedule currentSchedule = schedule.getSchedule();
                if (currentSchedule != null
                        && schedule.getSection() != null
                        && currentSchedule.getDistrictContextOid().equals(getOrganization().getCurrentContextOid())) {
                    allStaff = schedule.getSection().getTeacherSections();

                    for (ScheduleTeacher allStaffPersons : allStaff) {
                        Person sPerson = allStaffPersons.getStaff().getPerson();
                        if (sPerson != null
                                && !StringUtils.isEmpty(sPerson.getEmail01())
                                && !recipientsList.contains(sPerson.getEmail01())) {
                            recipientsList.add(sPerson.getEmail01());
                        }
                    }
                }
            }
        }
        return recipientsList;
    }
}
