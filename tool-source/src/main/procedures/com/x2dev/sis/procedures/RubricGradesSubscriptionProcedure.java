/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2016 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.procedures;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.Subscription;
import com.follett.fsc.core.k12.beans.SubscriptionDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookScore;
import com.x2dev.sis.model.beans.ReportingStandardScore;
import com.x2dev.sis.model.beans.RubricRatingScale;
import com.x2dev.sis.model.beans.RubricRatingScalePoints;
import com.x2dev.sis.model.beans.SectionReportingStandard;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.LoggerUtils;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;

/**
 * The rubric grades subscription procedure finds reporting standard score records that
 * did not meet standards and sends notifications to persons who
 * have subscribed to notifications.
 *
 * @author Follett Software Company
 */
public class RubricGradesSubscriptionProcedure extends SubscriptionProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String DATE_FORMAT = "MM/dd/yyyy";

    /** A date formatter to present the date of the grade. */
    private SimpleDateFormat m_dateFormat = null;

    /**
     * Instantiates a new rubric grades subscription procedure.
     */
    public RubricGradesSubscriptionProcedure() {
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
    }

    /**
     * Gets the bean class.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getBeanClass()
     */
    @Override
    protected Class getBeanClass() {
        return ReportingStandardScore.class;
    }

    /**
     * Gets the notification message.
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
        try {
            ReportingStandardScore rsScore = (ReportingStandardScore) bean;
            SectionReportingStandard standard = rsScore.getSectionReportingStandard();
            RubricRatingScale scale = standard.getRubricRatingScale();
            RubricRatingScalePoints matchingPoint =
                    findMatchingRatingPoint(scale.getRatingScalePoints(), rsScore.getScore());
            if (matchingPoint != null && !matchingPoint.getMasteryIndicator()) {
                return getFinalMessage(rsScore, standard.getName(), matchingPoint);
            }
        } catch (NullPointerException e) {
            /*
             * ignore sending this message, as there is a something wrong with the grade book,
             * grade book column definition, master schedule, or dates, etc., that caused a problem
             * Log the issue.
             */
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
        }

        return null;
    }

    /**
     * Find the RubricRatingScalePoints object that matches the score.
     * 
     * @param points Set of points to search.
     * @param score Value to look for
     * @return Matching point, or null if not found
     */
    private RubricRatingScalePoints findMatchingRatingPoint(Collection<RubricRatingScalePoints> points, String score) {
        for (RubricRatingScalePoints point : points) {
            if (score.equals(point.getId())) {
                return point;
            }
        }
        return null;
    }

    /**
     * Gets the final message.
     *
     * @param rsScore ReportingStandardScore
     * @param standardName String
     * @param matchingPoint RubricRatingScalePoints
     * @return String
     */
    private String getFinalMessage(ReportingStandardScore rsScore,
                                   String standardName,
                                   RubricRatingScalePoints matchingPoint) {
        GradebookScore gbScore = rsScore.getGradebookScore();
        GradebookColumnDefinition colDef = gbScore.getColumnDefinition();
        String studentName = gbScore.getStudent().getNameView();
        String date = colDef.getDateDue() != null ? m_dateFormat.format(colDef.getDateDue()) : null;
        String course = colDef.getMasterSchedule().getCourseView();
        String descr = colDef.getMasterSchedule().getDescription();

        String[] params = new String[] {studentName, matchingPoint.getName(), standardName, date, descr, course};
        return formatMessage("message.subscription.rubric.grade.body", params);
    }

    /**
     * Gets the object criteria.
     *
     * @param studentOidSubQuery SubQuery
     * @return Criteria
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getObjectCriteria(com.follett
     *      .fsc.core.framework.persistence.SubQuery)
     */
    @Override
    protected Criteria getObjectCriteria(SubQuery studentOidSubQuery) {
        /*
         * The studentOidSubQuery is ignored in this query because using it is extremely slow. The
         * default query just filters students where the students status is "active". The Criteria
         * is built here directly using an inner join to the Student table from the GradebookScore
         * table
         */
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(
                StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                        ReportingStandardScore.REL_GRADEBOOK_SCORE + ModelProperty.PATH_DELIMITER
                                + GradebookScore.REL_STUDENT + ModelProperty.PATH_DELIMITER
                                + SisStudent.COL_ENROLLMENT_STATUS));
        criteria.addEqualTo(ReportingStandardScore.REL_GRADEBOOK_SCORE + ModelProperty.PATH_DELIMITER
                + GradebookScore.REL_COLUMN_DEFINITION + ModelProperty.PATH_DELIMITER
                + GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);

        criteria.addEqualTo(ReportingStandardScore.REL_GRADEBOOK_SCORE + ModelProperty.PATH_DELIMITER
                + GradebookScore.REL_STUDENT + ModelProperty.PATH_DELIMITER +
                SisStudent.REL_SUBSCRIPTIONS + ModelProperty.PATH_DELIMITER +
                Subscription.REL_SUBSCRIPTION_DEFINITION + ModelProperty.PATH_DELIMITER +
                SubscriptionDefinition.COL_PROCEDURE_ID, m_subscriptionDefinition.getProcedureId());

        return criteria;
    }

    /**
     * Gets the student oid.
     *
     * @param bean X2BaseBean
     * @return String
     * @see
     *      com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getStudentOid(com.follett.fsc
     *      .core.k12.beans.X2BaseBean)
     */
    @Override
    protected String getStudentOid(X2BaseBean bean) {
        GradebookScore gradebookScore = ((ReportingStandardScore) bean).getGradebookScore();
        return gradebookScore == null ? "" : gradebookScore.getStudentOid();
    }
}
