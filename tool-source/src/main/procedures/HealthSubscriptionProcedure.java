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
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure;
import com.x2dev.sis.model.beans.HealthLog;
import com.x2dev.sis.model.beans.HealthLogComplaint;
import com.x2dev.sis.model.beans.SisStudent;
import java.text.SimpleDateFormat;
import org.apache.ojb.broker.query.Criteria;

/**
 * The attendance subscription procedure finds health visit records that have
 * been created since the last run and sends notifications to persons who
 * have subscribed to notifications.
 *
 * @author X2 Development Corporation
 */
public class HealthSubscriptionProcedure extends SubscriptionProcedure {
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
     * Define the bean class for student health visit.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getBeanClass()
     */
    @Override
    protected Class getBeanClass() {
        return HealthLog.class;
    }

    /**
     * Returns a formatted notification message for a student health visit.
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

        HealthLog visit = (HealthLog) bean;

        // Build list of offense codes.
        StringBuilder codes = new StringBuilder();
        codes.append(visit.getPrimaryComplaintCode());
        for (HealthLogComplaint complaint : visit.getComplaints()) {
            if (!complaint.getPrimaryIndicator()) {
                codes.append(",");
                codes.append(complaint.getComplaintCode());
            }
        }

        String studentName = visit.getNameView();
        String date = null;
        String time = null;
        if (visit.getDate() != null) {
            date = m_dateFormat.format(visit.getDate());
        }
        if (visit.getTimeIn() != null) {
            time = m_timeFormat.format(visit.getTimeIn());
        }
        String comment = visit.getNotes();
        String[] params = new String[] {studentName, codes.toString(), date, time, comment};
        String message = formatMessage("message.subscription.health.body", params);

        return message;
    }

    /**
     * Returns a criteria that selects the beans for the events that need notifications sent.
     * This should use the studentQuery as a subquery to identify the students to be
     * included in the query.
     *
     *
     * @param personOidSubQuery SubQuery
     * @return Criteria
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getObjectQuery(com.follett.
     *      fsc.core.framework.persistence.SubQuery)
     */
    @Override
    protected Criteria getObjectCriteria(SubQuery personOidSubQuery) {
        Criteria attendanceCriteria = new Criteria();
        attendanceCriteria.addIn(HealthLog.COL_PERSON_OID, personOidSubQuery);

        return attendanceCriteria;
    }

    /**
     * Returns the student Oid from the student health visit.
     *
     * @param bean X2BaseBean
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getStudentOid(com.follett.fsc
     *      .core.k12.beans.X2BaseBean)
     */
    @Override
    protected String getStudentOid(X2BaseBean bean) {
        return ((HealthLog) bean).getPerson().getStudent().getOid();
    }

    /**
     * Overwritten to return person oid subquery for use in getObjectCriteria which is passed this
     * SubQuery.
     *
     * @return Sub query
     * @see com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getStudentSubQuery()
     */
    @Override
    protected SubQuery getStudentSubQuery() {
        SubQuery subscriptionQuery =
                new SubQuery(Subscription.class, Subscription.COL_STUDENT_OID, getSubscriptionCriteria());
        Criteria criteria = new Criteria();
        criteria.addIn(X2BaseBean.COL_OID, subscriptionQuery);
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(), SisStudent.COL_ENROLLMENT_STATUS));
        return new SubQuery(SisStudent.class, SisStudent.COL_PERSON_OID, criteria);
    }
}
