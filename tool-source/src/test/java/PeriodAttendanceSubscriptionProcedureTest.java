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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.Subscription;
import com.follett.fsc.core.k12.beans.SubscriptionDefinition;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.test.BeanStorageHelper;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.test.scenarios.ScheduleScenario;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.StudentPeriodAttendance;
import com.x2dev.utils.TimeUtils;
import com.x2dev.utils.types.PlainTime;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;
import mockit.Deencapsulation;

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

/**
 * The Class PeriodAttendanceSubscriptionProcedureTest.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
@RunWith(Enclosed.class)
public class PeriodAttendanceSubscriptionProcedureTest {

    /**
     * The Class Shared.
     */
    @Ignore("Shared class")
    public abstract static class Shared extends X2BaseTest {
        public final PeriodAttendanceSubscriptionProcedure proc = new PeriodAttendanceSubscriptionProcedure() {

            /**
             * @see com.follett.fsc.core.k12.tools.procedures.SubscriptionProcedure#formatMessage(java.lang.String,
             *      java.lang.Object[])
             */
            @Override
            protected String formatMessage(String messageKey, Object[] params) {
                StringBuilder sb = new StringBuilder();
                sb.append(messageKey + " [");
                for (Object o : params) {
                    sb.append(o + ",");
                }
                sb.append("]");
                return sb.toString();
            }

        };
        public UserDataContainer userData;
        public ModelBroker broker;
        public School school;
        public Person person;
        public Student student;
        public DistrictSchoolYearContext schoolYear;
        public ScheduleScenario scheduleScenario;

        /**
         * @see com.follett.fsc.core.k12.test.X2BaseTest#setUp()
         */
        @Override
        @Before
        public void setUp() throws Exception {
            super.setUp();

            userData = UserDataContainer.getInstance(getUser(), getPrivilegeSet(), "127.0.0.1", "", Locale.getDefault(),
                    false);
            broker = new ModelBroker(getPrivilegeSet());

            schoolYear = BeanStorageHelper.createSchoolYear(this, broker);
            school = BeanStorageHelper.createSchool(this, broker, schoolYear);
            person = BeanStorageHelper.createPerson(this, broker, "junitPerson-Joe");
            student = BeanStorageHelper.createStudent(this, broker, person, school);

            Person staffPerson = BeanStorageHelper.createPerson(this, broker, "staffPerson");
            Staff staff = BeanStorageHelper.createStaff(this, broker, "Joe", staffPerson.getOid(), school.getOid(),
                    userData.getOrganizationOid(), "MATH");

            scheduleScenario = BeanStorageHelper.createScheduleScenario(this, broker, userData.getOrganization(),
                    schoolYear, school, staff);
        }

        /**
         * @see com.follett.fsc.core.k12.test.X2BaseTest#tearDown()
         */
        @Override
        @After
        public void tearDown() throws Exception {
            super.tearDown();
        }

    }

    /**
     * The Class CleanupHeader.
     */
    public static class BeanClass {
        public final PeriodAttendanceSubscriptionProcedure proc = new PeriodAttendanceSubscriptionProcedure();

        /**
         * Test method for {@link PeriodAttendanceSubscriptionProcedure#getBeanClass()}.
         */
        @Test
        public void testGetBeanClass() {
            Assert.assertEquals(StudentPeriodAttendance.class, proc.getBeanClass());
        }
    }

    /**
     * The Class CleanupHeader.
     */
    public static class NotificationMessage extends Shared {

        String queryParams = "";
        StudentPeriodAttendance attendance;
        Subscription student_subscription;

        /**
         * @see PeriodAttendanceSubscriptionProcedureTest.Shared#setUp()
         */
        @Override
        @Before
        public void setUp() throws Exception {
            super.setUp();

            attendance = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);
            String studentName = attendance.getStudent().getNameView();

            String periodName = attendance.getPeriodView();
            String courseDesc = attendance.getMasterSchedule().getDescription();
            String courseSection = attendance.getMasterSchedule().getCourseView();
            String code = attendance.getCodeView();
            String reason = attendance.getReasonCode();
            String date = Deencapsulation.invoke(proc, "getFormattedDate", attendance);
            String time = Deencapsulation.invoke(proc, "getFormattedTime", attendance);

            queryParams = "[" + studentName + "," + periodName + "," + courseDesc + "," + courseSection + "," + code
                    + "," + reason + "," + date + "," + time + ",]";

            student_subscription = BeanStorageHelper.createSubscription(this, broker, student, person, null);
        }

        /**
         * Null attendance null subscription.
         */
        @Test
        public void nullAttendance_nullSubscription() {
            String msg = proc.getNotificationMessage(null, null);
            Assert.assertEquals("error.exception []", msg);
        }

        /**
         * Non null attendance null subscription.
         */
        @Test
        public void nonNullAttendance_nullSubscription() {
            String msg = proc.getNotificationMessage(attendance, null);
            Assert.assertEquals("message.subscription.classattendance.body " + queryParams, msg);
        }

        /**
         * Null attendance non null subscription.
         */
        @Test
        public void nullAttendance_nonNullSubscription() {
            String msg = proc.getNotificationMessage(null, student_subscription);
            Assert.assertEquals("error.exception []", msg);
        }

        /**
         * Not student period attendance.
         */
        @Test
        public void notStudentPeriodAttendance() {
            String msg = proc.getNotificationMessage(person, null);
            Assert.assertEquals("error.exception []", msg);
        }

        /**
         * Happy path.
         */
        @Test
        public void happyPath() {
            String msg = proc.getNotificationMessage(attendance, student_subscription);
            Assert.assertEquals("message.subscription.classattendance.body " + queryParams, msg);
        }
    }

    /**
     * The Class GetObjectCriteria.
     */
    public static class GetObjectCriteria extends Shared {

        SubscriptionDefinition subDef;
        Student student_a, student_b, student_c;

        /**
         * @see PeriodAttendanceSubscriptionProcedureTest.Shared#setUp()
         */
        @Override
        @Before
        public void setUp() throws Exception {
            super.setUp();

            subDef = BeanStorageHelper.createSubscriptionDefinition(this, broker);
            Deencapsulation.setField(proc, "m_subscriptionDefinition", subDef);

            BeanStorageHelper.createSubscription(this, broker, student, person, subDef.getOid());

            student_a = BeanStorageHelper.createStudent(this, broker, person, school);
            BeanStorageHelper.createSubscription(this, broker, student_a, person, subDef.getOid());
            student_a.setEnrollmentStatus("NOT Active");
            this.saveTemporaryBeanWithErrorOutput(broker, student_a);

            student_b = BeanStorageHelper.createStudent(this, broker, person, school);
            BeanStorageHelper.createSubscription(this, broker, student_b, person, subDef.getOid());

            student_c = BeanStorageHelper.createStudent(this, broker, person, school);

            Map existingPreferences = Deencapsulation.invoke(userData.getPreferenceSet(), "getPreferences");
            existingPreferences.put(SystemPreferenceDefinition.STUDENT_ACTIVE_CODE, "Active");
            existingPreferences.put(SystemPreferenceDefinition.STUDENT_ACTIVE_CODE_NO_PRIMARY, "ActiveNoPrim");
            Deencapsulation.setField(userData.getPreferenceSet(), "m_preferences", existingPreferences);

            Deencapsulation.setField(proc, "m_organization", userData.getOrganization());
        }

        /**
         * Test get object criteria.
         */
        @Test
        public void testGetObjectCriteria() {
            Assert.assertEquals(
                    "[[[student.enrollmentStatus IN [Active, Active No Primary]]], student.subscriptions.subscriptionDefinition.procedureId = "
                            + subDef.getProcedureId() + "]",
                    proc.getObjectCriteria(null).toString());
        }

        /**
         * Live test.
         */
        @Test
        public void liveTest() {

            StudentPeriodAttendance att_a = BeanStorageHelper.createPeriodAttendance(this, broker, student_a, school,
                    scheduleScenario.masterSchedule);
            StudentPeriodAttendance att_b = BeanStorageHelper.createPeriodAttendance(this, broker, student_b, school,
                    scheduleScenario.masterSchedule);
            StudentPeriodAttendance att_c = BeanStorageHelper.createPeriodAttendance(this, broker, student_c, school,
                    scheduleScenario.masterSchedule);
            StudentPeriodAttendance att_stu = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);

            // Make sure we have 4 attendance beans
            BeanQuery query = new BeanQuery(proc.getBeanClass(), new Criteria(), true, false);
            int total = broker.getCount(query);
            Assert.assertEquals(4, total);

            query = new BeanQuery(proc.getBeanClass(), proc.getObjectCriteria(null), true, false);
            Collection<StudentPeriodAttendance> beans = broker.getCollectionByQuery(query);

            // should NOT get "a" back because he isn't active or "c" because he has no subscripion,
            // should get both other kids back
            Assert.assertEquals(2, beans.size());

            // test to get student with enroll, with subscription
            Assert.assertTrue(beans.contains(att_stu));
            // test to get student with enroll, with subscription
            Assert.assertTrue(beans.contains(att_b));
            // test to get student withOUT enroll, with subscription
            Assert.assertFalse(beans.contains(att_a));
            // test to get student with enroll, not with subscription
            Assert.assertFalse(beans.contains(att_c));
        }
    }

    /**
     * The Class GetFormattedTime.
     */
    public static class GetFormattedTime extends Shared {

        PlainTime twelveThirty = TimeUtils.getTime("12:30:00 PM");
        PlainTime twelveThirtySix = TimeUtils.getTime("12:36:00 PM");

        /**
         * No time.
         */
        @Test
        public void noTime() {
            StudentPeriodAttendance att = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);
            att.setTimeIn(null);
            att.setTimeOut(null);
            Assert.assertNull(Deencapsulation.invoke(proc, "getFormattedTime", att));
        }

        /**
         * In only.
         */
        @Test
        public void inOnly() {
            StudentPeriodAttendance att = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);
            att.setTimeIn(twelveThirty);
            att.setTimeOut(null);

            String queryParams = "[12:30 PM,]";

            Assert.assertEquals("message.subscription.classattendance.body.timeViewIn " + queryParams,
                    Deencapsulation.invoke(proc, "getFormattedTime", att));
        }

        /**
         * Out only.
         */
        @Test
        public void outOnly() {
            StudentPeriodAttendance att = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);
            // Time is:
            att.setTimeOut(twelveThirtySix);
            att.setTimeIn(null);

            String queryParams = "[12:36 PM,]";

            Assert.assertEquals("message.subscription.classattendance.body.timeViewOut " + queryParams,
                    Deencapsulation.invoke(proc, "getFormattedTime", att));
        }

        /**
         * Both in and out.
         */
        @Test
        public void bothInAndOut() {
            StudentPeriodAttendance att = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);
            // Time is:
            att.setTimeIn(twelveThirty);
            att.setTimeOut(twelveThirtySix);

            String queryParams = "[12:30 PM,12:36 PM,]";

            Assert.assertEquals("message.subscription.classattendance.body.timeViewInOut " + queryParams,
                    Deencapsulation.invoke(proc, "getFormattedTime", att));
        }

    }

    /**
     * The Class GetStudentOid.
     */
    public static class GetStudentOid extends Shared {
        /**
         * Test method for
         * {@link PeriodAttendanceSubscriptionProcedure#getStudentOid(com.follett.fsc.core.k12.beans.X2BaseBean)}
         * .
         */
        @Test
        public void testGetStudentOid() {
            StudentPeriodAttendance attendance = BeanStorageHelper.createPeriodAttendance(this, broker, student, school,
                    scheduleScenario.masterSchedule);

            Assert.assertEquals(attendance.getStudentOid(), proc.getStudentOid(attendance));
        }
    }
}
