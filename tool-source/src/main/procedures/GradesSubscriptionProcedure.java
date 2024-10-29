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
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Subscription;
import com.follett.fsc.core.k12.beans.SubscriptionDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.sis.model.beans.GradeScale;
import com.x2dev.sis.model.beans.GradebookColumnDefinition;
import com.x2dev.sis.model.beans.GradebookScore;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.GradesManager;
import com.x2dev.utils.LoggerUtils;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;

/**
 * The grades subscription procedure finds gradbook score records that fell
 * short of the grade threshold and sends notifications to persons who
 * have subscribed to notifications.
 *
 * @author X2 Development Corporation
 */
public class GradesSubscriptionProcedure extends SubscriptionProcedure {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    private static final String DATE_FORMAT = "MM/dd/yyyy";

    /** A grades manager to convert and scale student grades. */
    private GradesManager m_gradeManager = null;

    /** A date formatter to present the date of the grade. */
    private SimpleDateFormat m_dateFormat = null;

    /** Error message for when the required underlying data for the gradebook score is missing */
    private static final String DATASTRUCTURE_ERROR_MESSAGE =
            "GradesSubscriptionProcedure could not send message for gradebook score: {0}.  Missing column def or master schedule.";

    /**
     * Gets the bean class.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#getBeanClass()
     */
    @Override
    protected Class getBeanClass() {
        return GradebookScore.class;
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
        String message = null;

        try {
            // Get a Grades manager if necessary.
            if (m_gradeManager == null) {
                m_gradeManager = new GradesManager(getBroker());
            }

            GradebookScore gbScore = (GradebookScore) bean;
            GradebookColumnDefinition colDef = gbScore.getColumnDefinition();
            if (colDef != null && colDef.getMasterSchedule() != null) {
                GradeScale scale = colDef.getGradeScale();

                BigDecimal gradeScore = m_gradeManager.getNumericValue(gbScore);

                if (gradeScore != null) {
                    double score = gradeScore.doubleValue();

                    if (colDef.getTotalPoints() != null) {
                        score = GradesManager.scale(score,
                                0.0,
                                colDef.getTotalPoints().doubleValue(),
                                0.0,
                                100.0,
                                null,
                                null);
                    } else if (scale != null) {
                        score = GradesManager.scale(score,
                                scale.getMinimumPoints().doubleValue(),
                                scale.getMaximumPoints().doubleValue(),
                                0,
                                100,
                                null,
                                null);
                    }

                    double percent = getGradeThreshold(subscription, scale);

                    if (!colDef.getSystemOnlyIndicator() && colDef
                            .getVisibilityType() == GradebookColumnDefinition.VisibilityCode.PUBLIC.ordinal()) {
                        if (score < percent) {
                            // Grade is below threshold. Build the message.
                            if (m_dateFormat == null) {
                                m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
                            }

                            String studentName = gbScore.getStudent().getNameView();
                            String code = gbScore.getScore();
                            String max = colDef.getTotalPoints().toString();
                            String date = null;

                            if (colDef.getDateDue() != null) {
                                date = m_dateFormat.format(colDef.getDateDue());
                            }

                            String course = colDef.getMasterSchedule().getCourseView();
                            String descr = colDef.getMasterSchedule().getDescription();

                            String[] params = new String[] {studentName, code, max, date, descr, course};
                            message = formatMessage("message.subscription.grade.body", params);
                        }
                    }
                }
            } else {
                AppGlobals.getLog().log(Level.SEVERE, DATASTRUCTURE_ERROR_MESSAGE, gbScore.getOid());
            }
        } catch (NullPointerException e) {
            /*
             * ignore sending this message, as there is a something wrong with the grade book,
             * grade book column definition, master schedule, or dates, etc., that caused a problem
             * Log the issue.
             */
            AppGlobals.getLog().log(Level.SEVERE, LoggerUtils.convertThrowableToString(e));
        }

        return message;
    }

    /**
     * Returns the grade threshold for the subscription.
     *
     * @param subscription Subscription
     * @param scale GradeScale
     * @return double
     */
    protected double getGradeThreshold(Subscription subscription, GradeScale scale) {
        // Get subscriber entered threshold.
        double percent = -1;
        try {
            String param = subscription.getParameter();
            if (param != null) {
                percent = Float.parseFloat(param);
            }
        } catch (NumberFormatException nfe) {
            // Invalid parameter or invalid score, no value. No message.
        }

        return percent;
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
         * default query just filters students where the student's status is 'active'. The Criteria
         * is built here directly using an inner join to the Student table from the GradebookScore
         * table
         */
        Criteria criteria = new Criteria();
        criteria.addAndCriteria(StudentManager.getActiveStudentStatusCriteria(getOrganization(),
                GradebookScore.REL_STUDENT + ModelProperty.PATH_DELIMITER + SisStudent.COL_ENROLLMENT_STATUS));
        criteria.addEqualTo(GradebookScore.REL_COLUMN_DEFINITION + ModelProperty.PATH_DELIMITER
                + GradebookColumnDefinition.COL_SYSTEM_ONLY_INDICATOR, Boolean.FALSE);
        X2Criteria nullCriteria = new X2Criteria();
        X2Criteria visCriterea = new X2Criteria();

        nullCriteria.addEmpty(GradebookScore.REL_COLUMN_DEFINITION + ModelProperty.PATH_DELIMITER
                + GradebookColumnDefinition.COL_VISIBILITY_TYPE, getBroker().getPersistenceKey());
        visCriterea.addNotEqualTo(GradebookScore.REL_COLUMN_DEFINITION + ModelProperty.PATH_DELIMITER
                        + GradebookColumnDefinition.COL_VISIBILITY_TYPE,
                Integer.valueOf(GradebookColumnDefinition.VisibilityCode.PRIVATE.ordinal()));
        visCriterea.addOrCriteria(nullCriteria);
        criteria.addAndCriteria(visCriterea);

        criteria.addEqualTo(GradebookScore.REL_STUDENT + ModelProperty.PATH_DELIMITER +
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
        return ((GradebookScore) bean).getStudentOid();
    }
}
