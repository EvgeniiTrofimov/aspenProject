/* #PROCEDURE-ID [SYS-SPED-RI-COMMON-W] */
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
package com.x2dev.procedures.sys.sped.ma;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.User;
import com.follett.fsc.core.k12.beans.WorkflowDefinition;
import com.follett.fsc.core.k12.business.EmailManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.TextBankUtils;
import com.follett.fsc.core.k12.business.WriteEmailManager;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.procedures.WorkflowProcedure;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.SpedNotificationManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 *
 * @author Follett Software Company
 * @copyright 2017
 */
public class SpedMAWorkflowCommonProcedure {

    @SuppressWarnings("unused")
    private WorkflowProcedure m_workflowProcedure = null;
    private X2Broker m_broker = null;
    @SuppressWarnings("unused")
    private Locale m_locale = null;
    @SuppressWarnings("unused")
    private User m_user = null;
    @SuppressWarnings("unused")
    private WorkflowDefinition m_workflowDefinition = null;
    private Organization m_district = null;

    /**
     *
     * @author Follett Software Company
     * @copyright 2017
     */
    class ExtendedSpedNotificationManager extends SpedNotificationManager {


        /**
         * Resource key for the e-mail subject label.
         */
        private static final String MAIL_SUBJECT_LABEL = "message.email.iep.subjectHeader";

        private WriteEmailManager m_emailManager = null;
        private IepData m_iep = null;


        /**
         * @param broker
         * @param district
         * @param iep
         */
        public ExtendedSpedNotificationManager(IepData iep) {
            super(m_broker, m_district, iep);

            m_iep = iep;

            m_emailManager = new WriteEmailManager(m_district);
        }


        /**
         *
         */
        @Override
        public boolean sendEmailNotification() throws Exception {
            SisStudent student = m_iep.getStudent();

            String subject = LocalizationCache.getMessages(m_broker.getPersistenceKey()).getMessage(MAIL_SUBJECT_LABEL,
                    student.getNameView());
            String message = PreferenceManager.getPreferenceValue(m_district,
                    SisPreferenceConstants.SPED_DEFAULT_IEP_EMAIL_BODY_TEXT);
            message = TextBankUtils.specifyNames(message, student.getPerson().getFirstName() + " "
                    + student.getPerson().getLastName());
            message = TextBankUtils.specifyDistrict(message, m_district.getName());

            List<String> recipientsList = buildRecipientsList2(m_iep, student);

            return m_emailManager.sendMassEmail(recipientsList, subject, message, null);

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
        private List<String> buildRecipientsList2(IepData iep, SisStudent student) {
            List<String> recipientsList = new ArrayList<String>();

            // add teacher emails
            X2Criteria staffCriteria = new X2Criteria();
            staffCriteria.addEqualTo(StudentSchedule.COL_STUDENT_OID, student.getOid());
            staffCriteria.addEqualToField(StudentSchedule.COL_SCHEDULE_OID,
                    StudentSchedule.REL_SCHEDULE + PATH_DELIMITER + Schedule.REL_SCHOOL + PATH_DELIMITER +
                            SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER
                            + SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID);
            staffCriteria.addNotEmpty(StudentSchedule.REL_SECTION + PATH_DELIMITER +
                    Section.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.REL_PERSON + PATH_DELIMITER +
                    SisPerson.COL_EMAIL01, m_broker.getPersistenceKey());

            SubQuery staffSubQuery = new SubQuery(StudentSchedule.class, StudentSchedule.REL_SECTION +
                    PATH_DELIMITER + Section.REL_PRIMARY_STAFF + PATH_DELIMITER + SisStaff.REL_PERSON +
                    PATH_DELIMITER + SisPerson.COL_EMAIL01, staffCriteria);

            recipientsList.addAll(m_broker.getSubQueryCollectionByQuery(staffSubQuery));

            // add IEP team member emails
            X2Criteria teamMemberCriteria = new X2Criteria();
            teamMemberCriteria.addEqualTo(IepTeamMember.COL_IEP_DATA_OID, iep.getOid());
            DataDictionary ddx = getDictionaryByExtendedDictionaryId(DDX_ID_SPED_MA_IEP);
            DataDictionaryField field = ddx.findDataDictionaryFieldByAlias(ALIAS_ITM_EXCLUDE_EMAIL);

            if (field != null)
            {
                teamMemberCriteria.addNotEqualTo(field.getJavaName(), BooleanAsStringConverter.TRUE);
            }
            teamMemberCriteria.addNotEmpty(IepTeamMember.REL_PERSON + PATH_DELIMITER +
                    SisPerson.COL_EMAIL01, m_broker.getPersistenceKey());

            SubQuery teamMemberSubQuery = new SubQuery(IepTeamMember.class, IepTeamMember.REL_PERSON +
                    PATH_DELIMITER + SisPerson.COL_EMAIL01, teamMemberCriteria);

            recipientsList.addAll(m_broker.getSubQueryCollectionByQuery(teamMemberSubQuery));

            /*
             * Exclude invalid emails.
             */
            if (!CollectionUtils.isEmpty(recipientsList)) {
                List<String> invalidEmails = new ArrayList<String>();
                for (String email : recipientsList) {
                    if (!EmailManager.validateEmailAddress(email)) {
                        invalidEmails.add(email);
                    }
                }

                recipientsList.removeAll(invalidEmails);
            }

            // add guidance counselor
            // TODO: add guidance counselor once it is a system field on Student table

            return recipientsList;
        }

    }

    private static final String ALIAS_ITM_EXCLUDE_EMAIL = "itm-exclude-email";

    private static final String DDX_ID_SPED_MA_IEP = "SPED-MA-IEP";


    /**
     *
     * @param procedure
     * @param definition
     * @param district
     * @param user
     * @param broker
     * @param locale
     */
    public SpedMAWorkflowCommonProcedure(WorkflowProcedure procedure, WorkflowDefinition definition,
                                         Organization district,
                                         User user, X2Broker broker, Locale locale) {

        m_workflowProcedure = procedure;
        m_workflowDefinition = definition;
        m_broker = broker;
        m_user = user;
        m_district = district;
        m_locale = locale;
    }

    /**
     * Gets the broker.
     *
     * @return X 2 broker
     * @see com.follett.fsc.core.k12.tools.ToolJavaSource#getBroker()
     */
    protected X2Broker getBroker() {
        return m_broker;
    }



    /**
     * use the same method on <code>SpedIlWorkflowCommonProcedure</code>.
     *
     * @param extendedDataDictionaryID String
     * @return Data dictionary
     */
    public DataDictionary getDictionaryByExtendedDictionaryId(String extendedDataDictionaryID) {
        DataDictionary returnValue = null;
        Criteria criteria = new Criteria();
        criteria.addEqualTo(ExtendedDataDictionary.COL_ID, extendedDataDictionaryID);
        QueryByCriteria byCriteria = new QueryByCriteria(ExtendedDataDictionary.class, criteria);
        ExtendedDataDictionary extendedDataDictionary = (ExtendedDataDictionary) getBroker().getBeanByQuery(byCriteria);
        returnValue = DataDictionary.getDistrictDictionary(extendedDataDictionary, getBroker().getPersistenceKey());
        return returnValue;
    }

}
