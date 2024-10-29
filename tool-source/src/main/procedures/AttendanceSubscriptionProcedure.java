/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2011 X2 Development Corporation.
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
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import java.text.SimpleDateFormat;
import org.apache.ojb.broker.query.Criteria;

/**
 * Procedure to notify subscribers of daily attendance records that have been created since the last
 * time this procedure was run.
 *
 * @author X2 Development Corporation
 */
public class AttendanceSubscriptionProcedure extends SubscriptionProcedure {

    /** The Constant serialVersionUID. */
    private static final long serialVersionUID = 1L;

    private static final String DATE_FORMAT = "MM/dd/yyyy";

    private SimpleDateFormat m_dateFormat = null;

    /**
     * Defines the bean class as StudentAttendance.
     *
     * @return Class
     * @see com.x2dev.sis.tools.procedures.SubscriptionProcedure#getBeanClass()
     */
    @Override
    protected Class getBeanClass() {
        return StudentAttendance.class;
    }

    /**
     * Returns a formatted notification message for a StudentAttendance bean.
     *
     *
     * @param bean X2BaseBean
     * @param subscription Subscription
     * @return String
     * @see
     *      com.x2dev.sis.tools.procedures.SubscriptionProcedure#getNotificationMessage(com.x2dev.sis.
     *      model.beans.X2BaseBean, com.x2dev.sis.model.beans.Subscription)
     */
    @Override
    protected String getNotificationMessage(X2BaseBean bean, Subscription subscription) {

        if (bean == null || !(bean instanceof StudentAttendance)) {
            this.logMessage("The bean for the StudentAttendance was either null or not a StudentAttendance");
            return formatMessage("error.exception", new String[] {});
        }

        StudentAttendance attendance = (StudentAttendance) bean;

        String studentName = attendance.getStudent().getNameView();
        String code = attendance.getCodeView();
        String reason = attendance.getReasonCode();
        String time = attendance.getTimeView();

        String date = getFormattedDate(attendance);

        String[] params = new String[] {studentName, code, reason, date, time};
        String message = formatMessage("message.subscription.attendance.body", params);

        return message;
    }


    /**
     * Returns a criteria that selects the StudentAttendance beans for the events that need
     * notifications sent.
     *
     *
     * @param studentOidSubQuery SubQuery
     * @return Criteria
     * @see com.x2dev.sis.tools.procedures.SubscriptionProcedure#getObjectQuery(com.x2dev.sis.ojb.
     *      SubQuery)
     */
    @Override
    protected Criteria getObjectCriteria(SubQuery studentOidSubQuery) {
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                StudentAttendance.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        criteria.addEqualTo(
                StudentAttendance.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_SUBSCRIPTIONS
                        + ModelProperty.PATH_DELIMITER + Subscription.REL_SUBSCRIPTION_DEFINITION
                        + ModelProperty.PATH_DELIMITER + SubscriptionDefinition.COL_PROCEDURE_ID,
                m_subscriptionDefinition.getProcedureId());

        return criteria;
    }

    /**
     * Returns the student OID from the StudentAttendance bean.
     *
     * @param bean X2BaseBean
     * @return String
     * @see
     *      com.x2dev.sis.tools.procedures.SubscriptionProcedure#getStudentOid(com.x2dev.sis.model.beans.
     *      X2BaseBean)
     */
    @Override
    protected String getStudentOid(X2BaseBean bean) {
        return ((StudentAttendance) bean).getStudentOid();
    }

    /**
     * Gets the formatted date
     *
     * @param attendance
     * @return String
     */
    private String getFormattedDate(StudentAttendance attendance) {
        String date = null;
        if (m_dateFormat == null) {
            m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        }
        if (attendance.getDate() != null) {
            date = m_dateFormat.format(attendance.getDate());
        }
        return date;
    }
}
