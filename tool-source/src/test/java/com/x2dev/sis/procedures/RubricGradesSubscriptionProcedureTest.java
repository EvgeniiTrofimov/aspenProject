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
package com.x2dev.sis.procedures;


import static mockit.Deencapsulation.invoke;
import static mockit.Deencapsulation.newInstance;
import static mockit.Deencapsulation.setField;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.BeanManager;
import com.follett.fsc.core.k12.beans.Subscription;
import com.follett.fsc.core.k12.beans.SubscriptionDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.ToolBroker;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.follett.fsc.core.utils.MockLocalization;
import com.x2dev.sis.model.beans.ReportingStandardScore;
import com.x2dev.sis.model.beans.RubricRatingScale;
import com.x2dev.sis.model.beans.RubricRatingScalePoints;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.ojb.broker.query.Criteria;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;
import mockit.Expectations;
import mockit.Mocked;
import mockit.NonStrictExpectations;
import mockit.Tested;

/**
 * The Class RubricGradesSubscriptionProcedureTest.
 */
@RunWith(Enclosed.class)
public class RubricGradesSubscriptionProcedureTest {
    // public RubricGradesSubscriptionProcedureTest()
    // {
    // super();
    // }

    /**
     * The Class Shared.
     */
    @Ignore("Shared class")
    public abstract static class Shared {
        @Mocked
        ToolBroker m_broker;

        @Tested
        RubricGradesSubscriptionProcedure m_proc;

        /**
         * Inits the broker.
         */
        protected void initBroker() {
            setField(m_proc, "m_broker", m_broker);
            setField(m_proc, "m_secondaryConnectionPreference", "false");
            setField(m_broker.getPersistenceKey(), "m_deploymentId", "");
        }
    }

    /**
     * The Class GetBeanClass.
     */
    public static class GetBeanClass extends Shared {

        /**
         * Checks for right class.
         */
        @Test
        public void hasRightClass() {
            Class beanClass = m_proc.getBeanClass();
            assertEquals("we use reporting standard score for this", ReportingStandardScore.class, beanClass);
        }
    }

    /**
     * The Class GetNotificationMessage.
     */
    public static class GetNotificationMessage extends Shared {

        /**
         * Catch null ref.
         *
         * @param logger Logger
         */
        @Test
        public void catchNullRef(@Mocked final Logger logger) {
            new NonStrictExpectations(AppGlobals.class) {
                {
                    AppGlobals.getLog();
                    result = logger;

                    logger.log(Level.SEVERE, anyString);
                    times = 1;
                }
            };
            invoke(m_proc, "getNotificationMessage", X2BaseBean.class, Subscription.class);
        }

        /**
         * Met mastery.
         *
         * @param reportingStandard ReportingStandardScore
         * @param subscription Subscription
         * @param matchingPoint RubricRatingScalePoints
         * @param scale RubricRatingScale
         */
        @Test
        public void metMastery(@Mocked final ReportingStandardScore reportingStandard,
                               @Mocked final Subscription subscription,
                               @Mocked final RubricRatingScalePoints matchingPoint,
                               @Mocked final RubricRatingScale scale) {
            mockIt(true, reportingStandard, matchingPoint, scale);
            String actual = invoke(m_proc, "getNotificationMessage", reportingStandard, subscription);
            assertEquals(null, actual);
        }

        /**
         * Did not meet mastery.
         *
         * @param reportingStandard ReportingStandardScore
         * @param subscription Subscription
         * @param matchingPoint RubricRatingScalePoints
         * @param scale RubricRatingScale
         */
        @Test
        public void didNotMeetMastery(@Mocked final ReportingStandardScore reportingStandard,
                                      @Mocked final Subscription subscription,
                                      @Mocked final RubricRatingScalePoints matchingPoint,
                                      @Mocked final RubricRatingScale scale) {
            mockIt(false, reportingStandard, matchingPoint, scale);
            String actual = invoke(m_proc, "getNotificationMessage", reportingStandard, subscription);
            assertEquals("final message", actual);
        }

        /**
         * Matching piont not found.
         *
         * @param reportingStandard ReportingStandardScore
         * @param subscription Subscription
         * @param scale RubricRatingScale
         */
        @Test
        public void matchingPiontNotFound(@Mocked final ReportingStandardScore reportingStandard,
                                          @Mocked final Subscription subscription,
                                          @Mocked final RubricRatingScale scale) {
            mockIt(true, reportingStandard, null, scale);
            String actual = invoke(m_proc, "getNotificationMessage", reportingStandard, subscription);
            assertEquals(null, actual);
        }

        /**
         * Mock it.
         *
         * @param hasMastery boolean
         * @param reportingStandard ReportingStandardScore
         * @param matchingPoint RubricRatingScalePoints
         * @param scale RubricRatingScale
         */
        private void mockIt(boolean hasMastery,
                            final ReportingStandardScore reportingStandard,
                            final RubricRatingScalePoints matchingPoint,
                            final RubricRatingScale scale) {
            final Collection<RubricRatingScalePoints> scalePoints = new ArrayList<RubricRatingScalePoints>();
            new NonStrictExpectations(m_proc) {
                {
                    reportingStandard.getScore();
                    result = "asdf";
                    reportingStandard.getSectionReportingStandard().getRubricRatingScale().getRatingScalePoints();
                    result = scalePoints;
                    reportingStandard.getSectionReportingStandard().getRubricRatingScale();
                    result = scale;
                    reportingStandard.getSectionReportingStandard().getName();
                    result = "";
                    invoke(m_proc, "findMatchingRatingPoint", scalePoints, "asdf");
                    result = matchingPoint;
                    Class[] parameterTypes =
                            {ReportingStandardScore.class, String.class, RubricRatingScalePoints.class};
                    invoke(m_proc, "getFinalMessage", parameterTypes, reportingStandard, "", matchingPoint);
                    result = "final message";
                }
            };

            if (matchingPoint != null) {
                final Boolean hasMasteryValue = Boolean.valueOf(hasMastery);
                new Expectations() {
                    {
                        matchingPoint.getMasteryIndicator();
                        result = hasMasteryValue;
                    }
                };
            }
        }
    }

    /**
     * The Class FindMatchingRatingPoint.
     */
    public static class FindMatchingRatingPoint extends Shared {

        /**
         * Not found.
         */
        @Test
        public void notFound() {
            assertNull(invoke(m_proc, "findMatchingRatingPoint", createPoints(), "not found"));
        }

        /**
         * Found.
         */
        @Test
        public void found() {
            RubricRatingScalePoints point = invoke(m_proc, "findMatchingRatingPoint", createPoints(), "b");
            assertEquals("b", point.getId());
        }

        /**
         * Creates the points.
         *
         * @return Collection
         */
        private Collection<RubricRatingScalePoints> createPoints() {
            RubricRatingScalePoints[] array = {createPoint("a"), createPoint("b"), createPoint("c")};
            return Arrays.asList(array);
        }

        /**
         * Creates the point.
         *
         * @param id String
         * @return RubricRatingScalePoints
         */
        private RubricRatingScalePoints createPoint(String id) {
            RubricRatingScalePoints scalePoint =
                    newInstance(RubricRatingScalePoints.class, m_broker.getPersistenceKey());
            setField(scalePoint, "id", id);
            return scalePoint;
        }
    }

    /**
     * The Class GetFinalMessage.
     */
    public static class GetFinalMessage extends Shared {

        /**
         * All values.
         *
         * @param reportingScore ReportingStandardScore
         * @param matchingPoint RubricRatingScalePoints
         */
        @Test
        public void allValues(@Mocked final ReportingStandardScore reportingScore,
                              @Mocked final RubricRatingScalePoints matchingPoint) {
            final PlainDate dateDue = new PlainDate(1000000);
            init(reportingScore, matchingPoint, dateDue);
            String expected = "student received a score of score for standard on 12/31/1969 for description course";
            String actual = invoke(m_proc, "getFinalMessage", reportingScore, "standard", matchingPoint);
            assertEquals(expected, actual);
        }

        /**
         * No date.
         *
         * @param reportingScore ReportingStandardScore
         * @param matchingPoint RubricRatingScalePoints
         */
        @Test
        public void noDate(@Mocked final ReportingStandardScore reportingScore,
                           @Mocked final RubricRatingScalePoints matchingPoint) {
            init(reportingScore, matchingPoint, null);
            String expected = "student received a score of score for standard on  for description course";
            String actual = invoke(m_proc, "getFinalMessage", reportingScore, "standard", matchingPoint);
            assertEquals(expected, actual);
        }

        /**
         * Inits the.
         *
         * @param reportingScore ReportingStandardScore
         * @param matchingPoint RubricRatingScalePoints
         * @param dateDue PlainDate
         */
        private void init(final ReportingStandardScore reportingScore,
                          final RubricRatingScalePoints matchingPoint,
                          final PlainDate dateDue) {
            new NonStrictExpectations() {
                {
                    reportingScore.getGradebookScore().getColumnDefinition().getDateDue();
                    result = dateDue;
                    reportingScore.getGradebookScore().getColumnDefinition().getMasterSchedule().getCourseView();
                    result = "course";
                    reportingScore.getGradebookScore().getColumnDefinition().getMasterSchedule().getDescription();
                    result = "description";
                    reportingScore.getGradebookScore().getStudent().getNameView();
                    result = "student";

                    matchingPoint.getName();
                    result = "score";
                }
            };
            initBroker();
            MockLocalization.mock("message.subscription.rubric.grade.body",
                    "{0} received a score of {1} for {2} on {3} for {4} {5}");
        }
    }

    /**
     * The Class GetObjectCriteria.
     */
    public static class GetObjectCriteria extends Shared {

        /**
         * Creates criteria.
         *
         * @param subscriptionDefinition SubscriptionDefinition
         */
        @Test
        public void createsCriteria(@Mocked final SubscriptionDefinition subscriptionDefinition) {
            new NonStrictExpectations(BeanManager.class) {
                {
                    BeanManager.getCoreClassTranslator().getConcreteType(ReportingStandardScore.class);
                    result = ReportingStandardScore.class;

                    subscriptionDefinition.getProcedureId();
                    result = "procid";
                }
            };
            setField(m_proc, "m_subscriptionDefinition", subscriptionDefinition);
            SubQuery query = new SubQuery(ReportingStandardScore.class, "col", new Criteria());
            Criteria criteria = invoke(m_proc, "getObjectCriteria", query);
            String expected =
                    "[[[gradebookScore.student.enrollmentStatus IN [null, null]]], gradebookScore.columnDefinition.systemOnlyIndicator = false, gradebookScore.student.subscriptions.subscriptionDefinition.procedureId = procid]\n";
            assertEquals(expected.trim(), criteria.toString().trim());
        }
    }

    /**
     * The Class GetStudentOid.
     */
    public static class GetStudentOid extends Shared {

        /**
         * Get the student oid associated with the reporting standard score.
         *
         * @param score ReportingStandardScore
         */
        @Test
        public void hasScore(final @Mocked ReportingStandardScore score) {
            new NonStrictExpectations() {
                {
                    score.getGradebookScore().getStudentOid();
                    result = "oid";
                }
            };
            String actual = invoke(m_proc, "getStudentOid", score);
            assertEquals("oid", actual);
        }

        /**
         * Get a valid value even if there is no gradebook score attached to the reporting standard
         * score.
         *
         * @param score ReportingStandardScore
         */
        @Test
        public void noScore(final @Mocked ReportingStandardScore score) {
            new NonStrictExpectations() {
                {
                    score.getGradebookScore();
                    result = null;
                }
            };
            String actual = invoke(m_proc, "getStudentOid", score);
            assertEquals("", actual);
        }
    }

}
