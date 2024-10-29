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
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.Subscription;
import com.follett.fsc.core.k12.beans.SubscriptionDefinition;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.test.BeanStorageHelper;
import com.follett.fsc.core.k12.test.X2BaseTest;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.x2dev.sis.model.beans.StudentAttendance;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import org.apache.ojb.broker.query.Criteria;
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
 * The Class AttendanceSubscriptionProcedureTest.
 *
 * @author Follett Software Company
 * @copyright 2017
 */
@RunWith(Enclosed.class)
public class AttendanceSubscriptionProcedureTest {

    /**
     * The Class Shared.
     */
    @Ignore("Shared class")
    public abstract static class Shared extends X2BaseTest {
        public final AttendanceSubscriptionProcedure proc = new AttendanceSubscriptionProcedure() {

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

            DistrictSchoolYearContext schoolYear = BeanStorageHelper.createSchoolYear(this, broker);
            school = BeanStorageHelper.createSchool(this, broker, schoolYear);
            person = BeanStorageHelper.createPerson(this, broker, "junitPerson");
            student = BeanStorageHelper.createStudent(this, broker, person, school);
        }

    }

    /**
     * The Class CleanupHeader.
     */
    public static class BeanClass {
        public final AttendanceSubscriptionProcedure proc = new AttendanceSubscriptionProcedure();

        /**
         * Test method for {@link AttendanceSubscriptionProcedure#getBeanClass()}.
         */
        @Test
        public void testGetBeanClass() {
            Assert.assertEquals(StudentAttendance.class, proc.getBeanClass());
        }
    }

    /**
     * The Class CleanupHeader.
     */
    public static class NotificationMessage extends Shared {

        String queryParams = "";
        StudentAttendance attendance;
        Subscription student_subscription;

        /**
         * @see AttendanceSubscriptionProcedureTest.Shared#setUp()
         */
        @Override
        @Before
        public void setUp() throws Exception {
            super.setUp();

            attendance = BeanStorageHelper.createDailyAttendance(this, broker, student, true, false, false, "", school);
            String studentName = attendance.getStudent().getNameView();
            String code = attendance.getCodeView();
            String reason = attendance.getReasonCode();
            String time = attendance.getTimeView();
            String date = Deencapsulation.invoke(proc, "getFormattedDate", attendance);

            queryParams = "[" + studentName + "," + code + "," + reason + "," + date + "," + time + ",]";

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
            Assert.assertEquals("message.subscription.attendance.body " + queryParams, msg);
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
         * Not student attendance.
         */
        @Test
        public void notStudentAttendance() {
            String msg = proc.getNotificationMessage(person, null);
            Assert.assertEquals("error.exception []", msg);
        }

        /**
         * Happy path.
         */
        @Test
        public void happyPath() {
            String msg = proc.getNotificationMessage(attendance, student_subscription);
            Assert.assertEquals("message.subscription.attendance.body " + queryParams, msg);
        }
    }

    /**
     * The Class GetObjectCriteria.
     */
    public static class GetObjectCriteria extends Shared {

        SubscriptionDefinition subDef;
        Student student_a, student_b, student_c;

        /**
         * @see AttendanceSubscriptionProcedureTest.Shared#setUp()
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

            StudentAttendance att_a =
                    BeanStorageHelper.createDailyAttendance(this, broker, student_a, true, false, false, "", school);
            StudentAttendance att_b =
                    BeanStorageHelper.createDailyAttendance(this, broker, student_b, true, false, false, "", school);
            StudentAttendance att_c =
                    BeanStorageHelper.createDailyAttendance(this, broker, student_c, true, false, false, "", school);
            StudentAttendance att_stu =
                    BeanStorageHelper.createDailyAttendance(this, broker, student, true, false, false, "", school);

            // Make sure we have 4 attendance beans
            BeanQuery query = new BeanQuery(proc.getBeanClass(), new Criteria(), true, false);
            int total = broker.getCount(query);
            Assert.assertEquals(4, total);

            query = new BeanQuery(proc.getBeanClass(), proc.getObjectCriteria(null), true, false);
            Collection<StudentAttendance> beans = broker.getCollectionByQuery(query);

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
     * The Class GetStudentOid.
     */
    public static class GetStudentOid extends Shared {
        /**
         * Test method for
         * {@link AttendanceSubscriptionProcedure#getStudentOid(com.follett.fsc.core.k12.beans.X2BaseBean)}
         * .
         */
        @Test
        public void testGetStudentOid() {
            StudentAttendance attendance =
                    BeanStorageHelper.createDailyAttendance(this, broker, student, true, false, false, "", school);

            Assert.assertEquals(attendance.getStudentOid(), proc.getStudentOid(attendance));
        }
    }
}
