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
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.utils.types.PlainTime;
import java.text.SimpleDateFormat;
import org.apache.ojb.broker.query.Criteria;

/**
 * Procedure to notify subscribers of daily attendance records that have been created since the last
 * time this procedure was run.
 *
 * @author X2 Development Corporation
 */
public class PeriodAttendanceSubscriptionProcedure extends SubscriptionProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String TIME_FORMAT = "hh:mm aaa";

    /**
     * Defines the bean class as StudentPeriodAttendance.
     *
     * @return Class
     * @see com.x2dev.sis.tools.procedures.SubscriptionProcedure#getBeanClass()
     */
    @Override
    protected Class getBeanClass() {
        return StudentPeriodAttendance.class;
    }

    /**
     * Returns a formatted notification message for a StudentPeriodAttendance bean.
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

        if (bean == null || !(bean instanceof StudentPeriodAttendance)) {
            this.logMessage(
                    "The bean for the StudentPeriodAttendance was either null or not a StudentPeriodAttendance");
            return formatMessage("error.exception", new String[] {});
        }

        StudentPeriodAttendance attendance = (StudentPeriodAttendance) bean;


        String studentName = "";
        String courseDesc = "";
        String courseSection = "";

        if (attendance.getStudent() != null) {
            studentName = attendance.getStudent().getNameView();
        }

        MasterSchedule masterSchedule = attendance.getMasterSchedule();
        if (masterSchedule != null) {

            courseDesc = masterSchedule.getDescription();
            courseSection = masterSchedule.getCourseView();
        }

        String periodName = attendance.getPeriodView();
        String code = attendance.getCodeView();
        String reason = attendance.getReasonCode();
        String date = getFormattedDate(attendance);
        String time = getFormattedTime(attendance);

        String[] params = new String[] {studentName, periodName, courseDesc, courseSection, code, reason, date, time};
        String message = formatMessage("message.subscription.classattendance.body", params);

        return message;
    }

    /**
     * Returns a criteria that selects the StudentPeriodAttendance beans for the events that need
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
                StudentPeriodAttendance.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        criteria.addEqualTo(
                StudentPeriodAttendance.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.REL_SUBSCRIPTIONS
                        + ModelProperty.PATH_DELIMITER + Subscription.REL_SUBSCRIPTION_DEFINITION
                        + ModelProperty.PATH_DELIMITER + SubscriptionDefinition.COL_PROCEDURE_ID,
                m_subscriptionDefinition.getProcedureId());

        return criteria;
    }

    /**
     * Returns the student OID from the StudentPeriodAttendance bean.
     *
     * @param bean X2BaseBean
     * @return String
     * @see
     *      com.x2dev.sis.tools.procedures.SubscriptionProcedure#getStudentOid(com.x2dev.sis.model.beans.
     *      X2BaseBean)
     */
    @Override
    protected String getStudentOid(X2BaseBean bean) {
        return ((StudentPeriodAttendance) bean).getStudentOid();
    }

    /**
     * Gets the formatted date
     *
     * @param attendance
     * @return String user formatted date string
     */
    private String getFormattedDate(StudentPeriodAttendance attendance) {

        if (attendance.getDate() != null) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT);
            return dateFormat.format(attendance.getDate());
        }
        return null;
    }

    /**
     * Gets a user friendly view of the attendance time. Uses all the available times (time In /
     * time Out) if they aren't null
     *
     * @param attendance
     * @return String user formatted string for attendance record
     */
    private String getFormattedTime(StudentPeriodAttendance attendance) {

        PlainTime timeIn = attendance.getTimeIn();
        PlainTime timeOut = attendance.getTimeOut();

        SimpleDateFormat timeFormat = new SimpleDateFormat(TIME_FORMAT);

        if (timeIn != null && timeOut != null) {
            String[] params = new String[] {timeFormat.format(timeIn), timeFormat.format(timeOut)};
            return formatMessage("message.subscription.classattendance.body.timeViewInOut", params);
        }

        if (timeIn != null) {
            String[] params = new String[] {timeFormat.format(timeIn)};
            return formatMessage("message.subscription.classattendance.body.timeViewIn", params);
        }

        if (timeOut != null) {
            String[] params = new String[] {timeFormat.format(timeOut)};
            return formatMessage("message.subscription.classattendance.body.timeViewOut", params);
        }

        return null;
    }
}
