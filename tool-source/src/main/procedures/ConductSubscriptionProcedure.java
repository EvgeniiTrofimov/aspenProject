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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Subscription;
import com.follett.fsc.core.k12.beans.SubscriptionDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.ConductOffense;
import com.x2dev.sis.model.beans.SisStudent;
import java.text.SimpleDateFormat;
import org.apache.ojb.broker.query.Criteria;

/**
 * The attendance subscription procedure finds conduct incident records that have
 * been created since the last run and sends notifications to persons who
 * have subscribed to notifications.
 *
 * @author X2 Development Corporation
 */
public class ConductSubscriptionProcedure extends SubscriptionProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String TIME_FORMAT = "hh:mm aaa";

    /** A date formatter to present the date of the incident. */
    SimpleDateFormat m_dateFormat = null;
    SimpleDateFormat m_timeFormat = null;

    /**
     * Define the bean class for student conduct incident.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getBeanClass()
     */
    @Override
    protected Class getBeanClass() {
        return ConductIncident.class;
    }

    /**
     * Returns a formatted notification message for a student conduct record.
     *
     *
     * @param bean X2BaseBean
     * @param subscription Subscription
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getNotificationMessage(com.
     *      follett.fsc.core.k12.beans.X2BaseBean, com.follett.fsc.core.k12.beans.Subscription)
     */
    @Override
    protected String getNotificationMessage(X2BaseBean bean, Subscription subscription) {
        if (m_dateFormat == null) {
            m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
            m_timeFormat = new SimpleDateFormat(TIME_FORMAT);
        }

        ConductIncident incident = (ConductIncident) bean;

        // Build list of offense codes.
        StringBuilder codes = new StringBuilder();
        codes.append(incident.getIncidentCode());
        for (ConductOffense offense : incident.getConductOffenses()) {
            if (!offense.getPrimary()) {
                codes.append(",");
                codes.append(offense.getIncidentCode());
            }
        }

        String studentName = incident.getStudent().getNameView();
        String date = null;
        String time = null;
        String comment = null;
        if (incident.getIncidentDate() != null) {
            date = m_dateFormat.format(incident.getIncidentDate());
        }
        if (incident.getIncidentTime() != null) {
            time = m_timeFormat.format(incident.getIncidentTime());
        }
        if (incident.getDescription() != null) {
            comment = incident.getDescription();
        }
        String[] params = new String[] {studentName, codes.toString(), date, time, comment};
        String message = formatMessage("message.subscription.conduct.body", params);

        return message;
    }

    /**
     * Returns a query that selects the beans for the events that need notifications sent.
     * This query should use the studentQuery as a subquery to identify the students to be
     * included in the query.
     *
     *
     * @param studentOidSubQuery SubQuery
     * @return Criteria
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getObjectQuery(com.follett.
     *      fsc.core.framework.persistence.SubQuery)
     */
    @Override
    protected Criteria getObjectCriteria(SubQuery studentOidSubQuery) {
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        criteria.addEqualTo(ConductIncident.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.REL_SUBSCRIPTIONS + ModelProperty.PATH_DELIMITER +
                Subscription.REL_SUBSCRIPTION_DEFINITION + ModelProperty.PATH_DELIMITER +
                SubscriptionDefinition.COL_PROCEDURE_ID, m_subscriptionDefinition.getProcedureId());

        return criteria;
    }

    /**
     * Returns the student Oid from the student conduct record.
     *
     * @param bean X2BaseBean
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getStudentOid(com.follett.fsc
     *      .core.k12.beans.X2BaseBean)
     */
    @Override
    protected String getStudentOid(X2BaseBean bean) {
        return ((ConductIncident) bean).getStudentOid();
    }

}
