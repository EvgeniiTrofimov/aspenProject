/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2020 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.statereporting.onsis;

import static com.x2dev.sis.model.beans.SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE;
import static com.x2dev.sis.model.beans.SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE;
import static com.x2dev.sis.model.beans.StudentEnrollment.ENTRY;
import static com.x2dev.sis.model.beans.StudentEnrollment.STATUS_CHANGE;
import static com.x2dev.sis.model.beans.StudentEnrollment.WITHDRAWAL;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.business.ValidationError;
import com.x2dev.procedures.statereporting.on.EnrollmentSpanHelper;
import com.x2dev.procedures.statereporting.on.EnrollmentSpanHelper.AnnualSpan;
import com.x2dev.procedures.statereporting.on.EnrollmentSpanHelper.ParentSpan;
import com.x2dev.procedures.statereporting.on.EnrollmentSpanHelper.SpanConfiguration;
import com.x2dev.procedures.statereporting.on.EnrollmentSpanHelper.SpanCriteria;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import org.junit.Test;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class EnrollmentSpanHelperTest extends OnsisBaseTest {
    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_Simple() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);
        PlainDate entryDate = setupInfo.enrollment.getEnrollmentDate();

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        Collection<AnnualSpan> spans = spanHelper.getEnrollmentSpans(setupInfo.student, null, getBroker());

        assertEquals(1, spans.size());

        Optional<AnnualSpan> optional = spans.stream().findFirst();
        assertTrue(optional.isPresent());

        AnnualSpan enrollmentSpan = optional.get();

        assertEquals(entryDate, enrollmentSpan.getFirstActiveInSessionDate());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_PriorYearEntry() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        PlainDate relativeDatePast = DateUtils.add(getContext().getStartDate(), -100);
        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, relativeDatePast, enrollmentCode,
                getActiveStatusCode(), updateStudent);

        PlainDate relativeDateThisYear = DateUtils.add(getContext().getStartDate(), 100);
        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, relativeDateThisYear, "Withdrawn",
                "Inactive", updateStudent);

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());

        assertEquals(2, spans.size());

        AnnualSpan thisYearSpan = spans.get(1);

        PlainDate lastActiveThisYear = relativeDateThisYear;
        if (!spanHelper.isMemberOnWithdrawal()) {
            lastActiveThisYear = DateUtils.add(relativeDateThisYear, -1);
        }
        forward = false;
        PlainDate expectedLastInSessionDate = findFirstInSessionDate(schoolCalendar, lastActiveThisYear, forward);

        assertEquals(firstInSessionDate, thisYearSpan.getFirstActiveInSessionDate());
        assertEquals(expectedLastInSessionDate, thisYearSpan.getLastActiveInSessionDate());

        /*
         * Filter by context
         */
        spanHelper = createOnsisSpanHelper();

        spans = spanHelper.getEnrollmentSpans(student, getContext(), null, getBroker());
        assertEquals(1, spans.size());
        assertEquals(getContext(), spans.get(0).getContext());

        spans = spanHelper.getEnrollmentSpans(student, getContext(-1), null, getBroker());
        assertEquals(1, spans.size());
        assertEquals(getContext(-1), spans.get(0).getContext());

        /*
         * Filter by context range
         */
        spanHelper = createOnsisSpanHelper();

        spans = spanHelper.getEnrollmentSpans(student, getContext().getSchoolYear(),
                getContext().getSchoolYear(), null, getBroker());
        assertEquals(1, spans.size());
        assertEquals(getContext(), spans.get(0).getContext());

        spans = spanHelper.getEnrollmentSpans(student, getContext(-1).getSchoolYear(),
                getContext(-1).getSchoolYear(), null, getBroker());
        assertEquals(1, spans.size());
        assertEquals(getContext(-1), spans.get(0).getContext());

        spans = spanHelper.getEnrollmentSpans(student, getContext(-1).getSchoolYear(),
                getContext().getSchoolYear(), null, getBroker());
        assertEquals(2, spans.size());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_FourSpans() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        PlainDate queryAsOfDate = null;
        boolean cutoffBeforeLastYear = false;
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper(queryAsOfDate, cutoffBeforeLastYear);

        /*
         * Setup: Create four spans from 3 years ago through the current year
         */
        boolean forward = true;
        DistrictSchoolYearContext contextYear1 = getContext(-3);
        PlainDate entryDateYear1 = contextYear1.getStartDate();

        SchoolCalendar schoolCalendarYear1 =
                findSchoolCalendar(contextYear1.getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate nextInSessionDateYear1 =
                findFirstInSessionDate(schoolCalendarYear1, entryDateYear1, forward);
        PlainDate lastInSessionDateYear1 =
                CalendarManager.getLastInSessionDate(schoolCalendarYear1, getBroker()).getDate();

        DistrictSchoolYearContext contextYear2 = getContext(-2);
        SchoolCalendar schoolCalendarYear2 =
                findSchoolCalendar(contextYear2.getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate firstInSessionDateYear2 =
                CalendarManager.getFirstInSessionDate(schoolCalendarYear2, getBroker()).getDate();
        PlainDate lastInSessionDateYear2 =
                CalendarManager.getLastInSessionDate(schoolCalendarYear2, getBroker()).getDate();

        DistrictSchoolYearContext contextYear3 = getContext(-1);
        SchoolCalendar schoolCalendarYear3 =
                findSchoolCalendar(contextYear3.getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate firstInSessionDateYear3 =
                CalendarManager.getFirstInSessionDate(schoolCalendarYear3, getBroker()).getDate();

        PlainDate withdrawalDate = DateUtils.add(getContext().getStartDate(), 70);
        PlainDate w1Date = DateUtils.add(withdrawalDate, -1);

        SchoolCalendar schoolCalendarYear4 =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate firstInSessionDateYear4 =
                findFirstInSessionDate(schoolCalendarYear4, getContext().getStartDate(), forward);
        forward = false;
        PlainDate expectedLastInSessionDate = findFirstInSessionDate(schoolCalendarYear4, w1Date, forward);

        /*
         * Create Entry enrollment three years back
         */
        boolean updateStudent = true;
        StudentEnrollment entry =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDateYear1, updateStudent);

        /*
         * Create Withdrawal enrollment this year
         */
        StudentEnrollment withdrawal =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, updateStudent);

        /*
         * Get Parent Span - one span covering all four years
         */
        List<ParentSpan> parentSpans = spanHelper.getParentSpans(student, null, getBroker());
        assertEquals(1, parentSpans.size());

        ParentSpan parentSpan = parentSpans.get(0);
        assertEquals(school, parentSpan.getSchool());
        assertEquals(student, parentSpan.getStudent());
        assertEquals(nextInSessionDateYear1, parentSpan.getFirstActiveInSessionDate());
        assertEquals(expectedLastInSessionDate, parentSpan.getLastActiveInSessionDate());
        assertFalse(parentSpan.isSecondary());
        assertEquals(2, parentSpan.getEnrollments().size());
        assertEquals(entry, parentSpan.getEnrollments().get(0));
        assertEquals(withdrawal, parentSpan.getEnrollments().get(1));

        /*
         * Get four child spans - one span per year
         */
        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(4, spans.size());

        /*
         * Check first child span
         */
        AnnualSpan firstSpan = spans.get(0);
        assertEquals(nextInSessionDateYear1, firstSpan.getFirstActiveInSessionDate());
        assertEquals(lastInSessionDateYear1, firstSpan.getLastActiveInSessionDate());
        assertTrue(firstSpan.isEntry());
        assertFalse(firstSpan.isWithdrawal());
        assertNull(firstSpan.getWithdrawalMemberDate());
        assertEquals(1, firstSpan.getAllEnrollmentsAscend().size());
        assertEquals(entry, firstSpan.getAllEnrollmentsAscend().get(0));
        assertEquals(1, firstSpan.getEnrollmentsInsideSpanAscend().size());
        assertEquals(entry, firstSpan.getEnrollmentsInsideSpanAscend().get(0));
        assertEquals(withdrawal, firstSpan.getDataStorageEnrollment());
        assertNull(firstSpan.getTerminatingEnrollment());

        /*
         * Check second child span
         */
        AnnualSpan secondSpan = spans.get(1);
        assertEquals(firstInSessionDateYear2, secondSpan.getFirstActiveInSessionDate());
        assertEquals(lastInSessionDateYear2, secondSpan.getLastActiveInSessionDate());
        assertFalse(secondSpan.isEntry());
        assertFalse(secondSpan.isWithdrawal());
        assertNull(secondSpan.getWithdrawalMemberDate());
        assertEquals(1, secondSpan.getAllEnrollmentsAscend().size());
        assertEquals(entry, firstSpan.getAllEnrollmentsAscend().get(0));
        assertEquals(0, secondSpan.getEnrollmentsInsideSpanAscend().size());
        assertEquals(withdrawal, secondSpan.getDataStorageEnrollment());
        assertNull(secondSpan.getTerminatingEnrollment());

        /*
         * Check last child span
         */
        AnnualSpan lastSpan = spans.get(spans.size() - 1);
        assertEquals(firstInSessionDateYear4, lastSpan.getFirstActiveInSessionDate());
        assertEquals(expectedLastInSessionDate, lastSpan.getLastActiveInSessionDate());
        assertEquals(2, lastSpan.getAllEnrollmentsAscend().size());
        assertEquals(entry, lastSpan.getAllEnrollmentsAscend().get(0));
        assertEquals(withdrawal, lastSpan.getAllEnrollmentsAscend().get(1));
        assertEquals(1, lastSpan.getEnrollmentsInsideSpanAscend().size());
        assertEquals(withdrawal, lastSpan.getEnrollmentsInsideSpanAscend().get(0));
        assertFalse(lastSpan.isEntry());
        assertTrue(lastSpan.isWithdrawal());
        assertEquals(withdrawal, lastSpan.getDataStorageEnrollment());
        assertEquals(withdrawal, lastSpan.getTerminatingEnrollment());


        /**
         * Re-pull spans using historical cutoff date
         */
        SpanConfiguration spanConfig = createOnsisSpanConfiguration(getOrganization());
        PlainDate historicalCutoffDate = contextYear2.getStartDate();
        spanConfig.historicalCutoffDate = historicalCutoffDate;
        spanHelper = new EnrollmentSpanHelper(spanConfig);

        parentSpans = spanHelper.getParentSpans(student, null, getBroker());
        assertEquals(1, parentSpans.size());

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(3, spans.size());

        spans = parentSpans.get(0).getAnnualSpans();
        assertEquals(3, spans.size());

        assertEquals(firstInSessionDateYear2, spans.get(0).getFirstActiveInSessionDate());
        assertEquals(firstInSessionDateYear3, spans.get(1).getFirstActiveInSessionDate());
        assertEquals(firstInSessionDateYear4, spans.get(2).getFirstActiveInSessionDate());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_PreReg() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);
        PlainDate expectedLastInSessionDate = findLastInSessionDate(schoolCalendar);

        /*
         * Create "E" PreReg near start of school year
         */
        PlainDate preRegDate = DateUtils.add(firstInSessionDate, 2);
        preRegDate = findFirstInSessionDate(schoolCalendar, preRegDate, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Registered";
        StudentEnrollment preRegRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, preRegDate,
                        enrollmentCode, getPreRegStatusCode(), updateStudent);

        /*
         * Create "S" Active 2 days after PreReg
         */
        PlainDate activeDate = DateUtils.add(preRegDate, 2);
        activeDate = findFirstInSessionDate(schoolCalendar, activeDate, forward);

        enrollmentCode = "Enrolled";
        StudentEnrollment activeRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, activeDate,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Expect one span with:
         * - firstActiveInSessionDate = activeDate (not preRegDate)
         * - lastActiveInSessionDate = ??
         * - 2 enrollment records [prereg, active]
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();
        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());

        assertEquals(1, spans.size());
        AnnualSpan span = spans.get(0);

        assertEquals(activeDate, span.getFirstActiveInSessionDate());
        assertEquals(2, span.getAllEnrollmentsAscend().size());
        assertEquals(preRegRecord.getOid(), span.getAllEnrollmentsAscend().get(0).getOid());
        assertEquals(activeRecord.getOid(), span.getAllEnrollmentsAscend().get(1).getOid());
        assertEquals(expectedLastInSessionDate, span.getLastActiveInSessionDate());
        assertTrue(span.isEntry());
    }

    /**
     * Enrollments:
     * E(PreReg) -- S (Active)
     *
     * Desired span:
     * [E,S] with First active date based on S record
     *
     * Rule: All trailing non-Active non-Withdrawals belong to the next span.
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_PreReg_BreakOnS() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);
        PlainDate expectedLastInSessionDate = findLastInSessionDate(schoolCalendar);

        /*
         * Create "E" PreReg near start of school year
         */
        PlainDate preRegDate = DateUtils.add(firstInSessionDate, 2);
        preRegDate = findFirstInSessionDate(schoolCalendar, preRegDate, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Registered";
        StudentEnrollment preRegRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, preRegDate,
                        enrollmentCode, getPreRegStatusCode(), updateStudent);

        /*
         * Create "S" Active 2 days after PreReg
         */
        PlainDate activeDate = DateUtils.add(preRegDate, 2);
        activeDate = findFirstInSessionDate(schoolCalendar, activeDate, forward);

        enrollmentCode = "Enrolled";
        StudentEnrollment activeRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, activeDate,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Expect one span with:
         * - firstActiveInSessionDate = activeDate (not preRegDate)
         * - lastActiveInSessionDate = ??
         * - 2 enrollment records [prereg, active]
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());

        assertEquals(1, spans.size());
        AnnualSpan preRegSpan = spans.get(0);

        assertEquals(activeDate, preRegSpan.getFirstActiveInSessionDate());
        assertEquals(expectedLastInSessionDate, preRegSpan.getLastActiveInSessionDate());

        assertEquals(2, preRegSpan.getAllEnrollmentsAscend().size());
        assertEquals(preRegRecord.getOid(), preRegSpan.getAllEnrollmentsAscend().get(0).getOid());
        assertEquals(activeRecord.getOid(), preRegSpan.getAllEnrollmentsAscend().get(1).getOid());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_TwoE_SameSchool() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        /*
         * Create "E" Active near start of school year
         */
        PlainDate entry1Date = DateUtils.add(firstInSessionDate, 2);
        entry1Date = findFirstInSessionDate(schoolCalendar, entry1Date, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entry1 =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry1Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "E" Active 2 days after first E record
         */
        PlainDate entry2Date = DateUtils.add(entry1Date, 2);
        entry2Date = findFirstInSessionDate(schoolCalendar, entry2Date, forward);

        StudentEnrollment entry2 =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry2Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Expect one span with:
         * - firstActiveInSessionDate = entry1Date (not entry2Date)
         * - 2 enrollment records [entry1, entry2]
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        AnnualSpan span = spans.get(0);

        assertEquals(entry1Date, span.getFirstActiveInSessionDate());
        assertEquals(2, span.getAllEnrollmentsAscend().size());
        assertEquals(entry1.getOid(), span.getAllEnrollmentsAscend().get(0).getOid());
        assertEquals(entry2.getOid(), span.getAllEnrollmentsAscend().get(1).getOid());
        // assertEquals(expectedLastInSessionDate, span.getLastActiveInSessionDate());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_MemberOnEntry() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        /*
         * Create "E" Active near start of school year
         */
        PlainDate entry1Date = DateUtils.add(firstInSessionDate, 2);
        entry1Date = findFirstInSessionDate(schoolCalendar, entry1Date, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry1 =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry1Date,
                enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Test: MemberOnEntry FALSE
         */
        setPreference(STUDENT_MEMBER_ON_ENTRY_DATE, Boolean.FALSE.toString());

        /*
         * Expect one span with:
         * - firstActiveInSessionDate = first inSession date AFTER entry1Date (not entry1Date)
         */
        PlainDate expectedFirstActiveDate = DateUtils.add(entry1Date, 1);
        expectedFirstActiveDate = findFirstInSessionDate(schoolCalendar, expectedFirstActiveDate, forward);

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        AnnualSpan span = spans.get(0);

        assertEquals(expectedFirstActiveDate, span.getFirstActiveInSessionDate());

        /*
         * Test: MemberOnEntry TRUE
         */
        setPreference(STUDENT_MEMBER_ON_ENTRY_DATE, Boolean.TRUE.toString());

        /*
         * Expect one span with:
         * - firstActiveInSessionDate = entry1Date
         */
        spanHelper = createOnsisSpanHelper();

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        span = spans.get(0);

        assertEquals(entry1Date, span.getFirstActiveInSessionDate());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_MemberOnWithdraw() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);
        PlainDate lastInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getEndDate(), false);

        /*
         * Create "E" Active near start of school year
         */
        PlainDate entry1Date = DateUtils.add(firstInSessionDate, 2);
        entry1Date = findFirstInSessionDate(schoolCalendar, entry1Date, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry1 =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry1Date,
                enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Sanity check: Check withdrawal values before creating the Withdrawal record
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        AnnualSpan span = spans.get(0);

        assertEquals(lastInSessionDate, span.getLastActiveInSessionDate());
        assertNull(span.getWithdrawalMemberDate());
        assertEquals(lastInSessionDate, span.getWithdrawalMemberOrLastActiveDate());
        assertNull(span.getTerminatingEnrollment());
        assertFalse(span.isWithdrawal());

        /*
         * Create "W" record after E
         */
        PlainDate withdrawDate = DateUtils.add(entry1Date, 2);
        withdrawDate = findFirstInSessionDate(schoolCalendar, withdrawDate, forward);
        enrollmentCode = "Transfer";

        StudentEnrollment withdrawal =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawDate,
                        enrollmentCode, "Inactive", updateStudent);

        /*
         * Test: MemberOnWithdrawal FALSE
         */
        setPreference(STUDENT_MEMBER_ON_WITHDRAWAL_DATE, Boolean.FALSE.toString());

        /*
         * Expect one span with:
         * - lastActiveInSessionDate = first inSession date BEFORE withdrawDate (not withdrawDate)
         */
        PlainDate dayBeforeW = DateUtils.add(withdrawDate, -1);
        PlainDate expectedLastActiveDate = findFirstInSessionDate(schoolCalendar, dayBeforeW, false);

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        span = spans.get(0);

        assertEquals(expectedLastActiveDate, span.getLastActiveInSessionDate());
        assertEquals(dayBeforeW, span.getWithdrawalMemberDate());
        assertEquals(dayBeforeW, span.getWithdrawalMemberOrLastActiveDate());
        assertEquals(withdrawal, span.getTerminatingEnrollment());
        assertTrue(span.isWithdrawal());

        /*
         * Test: MemberOnWithdrawal TRUE
         */
        setPreference(STUDENT_MEMBER_ON_WITHDRAWAL_DATE, Boolean.TRUE.toString());

        /*
         * Expect one span with:
         * - lastActiveInSessionDate = withdrawDate
         */
        spanHelper = createOnsisSpanHelper();

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        span = spans.get(0);

        assertEquals(withdrawDate, span.getLastActiveInSessionDate());
        assertEquals(withdrawDate, span.getWithdrawalMemberDate());
        assertEquals(withdrawDate, span.getWithdrawalMemberOrLastActiveDate());
        assertEquals(withdrawal, span.getTerminatingEnrollment());
        assertTrue(span.isWithdrawal());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_breakOnStatusChangeRecord() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);
        SisSchoolCalendarDate lastInSessionDate =
                CalendarManager.getLastInSessionDate(schoolCalendar, getBroker());

        /*
         * Create "E" Active near start of school year
         */
        PlainDate entry1Date = DateUtils.add(firstInSessionDate, 2);
        entry1Date = findFirstInSessionDate(schoolCalendar, entry1Date, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entry1 =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry1Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "S" record after E
         */
        PlainDate statusDate = DateUtils.add(entry1Date, 2);
        statusDate = findFirstInSessionDate(schoolCalendar, statusDate, forward);

        StudentEnrollment statusRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, statusDate,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Test 1 - SpanHelper with break on "S" enabled
         * Expect two spans as detailed below
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());

        /*
         * First span:
         */
        // getFirstActiveInSessionDate must return entry1Date
        AnnualSpan firstSpan = spans.get(0);
        assertEquals(entry1Date, firstSpan.getFirstActiveInSessionDate());

        // getLastActiveInSessionDate must return the in-session date before statusDate
        PlainDate expectedLastActiveInSessionDate = DateUtils.add(statusDate, -1);
        expectedLastActiveInSessionDate =
                findFirstInSessionDate(schoolCalendar, expectedLastActiveInSessionDate, false);
        assertEquals(expectedLastActiveInSessionDate, firstSpan.getLastActiveInSessionDate());

        // getEnrollments must include the E record then the S record.
        List<StudentEnrollment> enrollments = firstSpan.getAllEnrollmentsAscend();
        assertEquals(1, enrollments.size());
        assertEquals(entry1.getOid(), enrollments.get(0).getOid());

        // getFirstActiveEnrollment must return the E record
        StudentEnrollment firstActiveEnrollment = firstSpan.getFirstActiveEnrollment();
        assertEquals(entry1.getOid(), firstActiveEnrollment.getOid());

        /*
         * Second span:
         */
        // getFirstActiveInSessionDate must return statusDate
        AnnualSpan secondSpan = spans.get(1);
        assertEquals(statusDate, secondSpan.getFirstActiveInSessionDate());

        // getLastActiveInSessionDate must return last in session of the year
        assertEquals(lastInSessionDate.getDate(), secondSpan.getLastActiveInSessionDate());


        // getEnrollments must include only the S record.
        enrollments = secondSpan.getAllEnrollmentsAscend();
        assertEquals(1, enrollments.size());
        assertEquals(statusRecord.getOid(), enrollments.get(0).getOid());

        // getFirstActiveEnrollment must return the S record
        firstActiveEnrollment = secondSpan.getFirstActiveEnrollment();
        assertEquals(statusRecord.getOid(), firstActiveEnrollment.getOid());


        /*
         * Test 2 - SpanHelper with break on "S" disabled
         *
         * Expect one span.
         * Span must include the E record and the S record
         */
        SpanConfiguration spanConfig = createOnsisSpanConfiguration(getOrganization());
        spanConfig.statusBreak = false;
        spanHelper = new EnrollmentSpanHelper(spanConfig);

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());

        // getFirstActiveInSessionDate must return entry1Date
        firstSpan = spans.get(0);
        assertEquals(entry1Date, firstSpan.getFirstActiveInSessionDate());

        // getLastActiveInSessionDate must return last in session date of the year
        assertEquals(lastInSessionDate.getDate(), firstSpan.getLastActiveInSessionDate());

        // getEnrollments must include the E record then the S record.
        enrollments = firstSpan.getAllEnrollmentsAscend();
        assertEquals(2, enrollments.size());
        assertEquals(entry1.getOid(), enrollments.get(0).getOid());
        assertEquals(statusRecord.getOid(), enrollments.get(1).getOid());

        // getFirstActiveEnrollment must return the E record
        firstActiveEnrollment = firstSpan.getFirstActiveEnrollment();
        assertEquals(entry1.getOid(), firstActiveEnrollment.getOid());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_statusChangeSpanCanAccessFutureWithdrawal() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        /*
         * Create "E" Active near start of school year
         */
        PlainDate entry1Date = DateUtils.add(firstInSessionDate, 2);
        entry1Date = findFirstInSessionDate(schoolCalendar, entry1Date, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entry1 =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry1Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "S" record after E
         */
        PlainDate statusDate = DateUtils.add(entry1Date, 2);
        statusDate = findFirstInSessionDate(schoolCalendar, statusDate, forward);
        PlainDate dayBeforeStatus = DateUtils.add(statusDate, -1);
        PlainDate lastInSessionBeforeStatus = findFirstInSessionDate(schoolCalendar, dayBeforeStatus, false);

        StudentEnrollment statusRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, statusDate,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create W record after S
         */
        PlainDate withdrawalDate = DateUtils.add(statusDate, 2);
        withdrawalDate = findFirstInSessionDate(schoolCalendar, withdrawalDate, forward);
        StudentEnrollment withdrawalRecord =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate,
                        enrollmentCode, "Inactive", updateStudent);

        /*
         * SpanHelper with break on "S" enabled
         * Expect two spans broken by the S record.
         * Expect the W record
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());

        /*
         * First span:
         */
        // getFirstActiveInSessionDate must return entry1Date
        AnnualSpan firstSpan = spans.get(0);
        assertEquals(entry1Date, firstSpan.getFirstActiveInSessionDate());

        // getEnrollments must include only the E record
        // and MUST NOT include the S or W record
        List<StudentEnrollment> enrollments = firstSpan.getAllEnrollmentsAscend();
        assertEquals(1, enrollments.size());
        assertEquals(entry1.getOid(), enrollments.get(0).getOid());

        // getFirstActiveEnrollment must return the E record
        StudentEnrollment firstActiveEnrollment = firstSpan.getFirstActiveEnrollment();
        assertEquals(entry1.getOid(), firstActiveEnrollment.getOid());

        // getLastActiveInSessionDate must return lastInSessionOnBeforeStatus
        assertEquals(lastInSessionBeforeStatus, firstSpan.getLastActiveInSessionDate());

        // getDataStorageEnrollment MUST return the W record even though it's in the future
        StudentEnrollment futureWRecord = firstSpan.getDataStorageEnrollment();
        assertNotNull(futureWRecord);
        assertEquals(withdrawalRecord.getOid(), futureWRecord.getOid());

        /*
         * Second span:
         */
        // getFirstActiveInSessionDate must return firstInSessionOnAfterStatus
        AnnualSpan secondSpan = spans.get(1);
        assertEquals(statusDate, secondSpan.getFirstActiveInSessionDate());

        // getEnrollments must include the S record and the W record.
        enrollments = secondSpan.getAllEnrollmentsAscend();
        assertEquals(2, enrollments.size());
        assertEquals(statusRecord.getOid(), enrollments.get(0).getOid());
        assertEquals(withdrawalRecord.getOid(), enrollments.get(1).getOid());

        // getFirstActiveEnrollment must return the S record
        firstActiveEnrollment = secondSpan.getFirstActiveEnrollment();
        assertEquals(statusRecord.getOid(), firstActiveEnrollment.getOid());

        // getDataStorageEnrollment MUST return the W record
        futureWRecord = firstSpan.getDataStorageEnrollment();
        assertEquals(withdrawalRecord.getOid(), futureWRecord.getOid());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_ignoreFteRecord() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        /*
         * Create "E" Active near start of school year
         */
        PlainDate entry1Date = DateUtils.add(firstInSessionDate, 2);
        entry1Date = findFirstInSessionDate(schoolCalendar, entry1Date, forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry1 =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry1Date,
                enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create FTE "S" record after E
         */
        PlainDate fteDate = DateUtils.add(entry1Date, 2);
        fteDate = findFirstInSessionDate(schoolCalendar, fteDate, forward);
        enrollmentCode = "Internal Transfer";

        StudentEnrollment fteStatus =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, fteDate,
                        FTE_ENROLLMENT_CODE, getActiveStatusCode(), updateStudent);

        fteStatus.setOriginatingClass("com.x2dev.procedures.statereporting.on.OnsisDmFte");
        getBroker().saveBeanForced(fteStatus);

        /*
         * Test 1 - SpanHelper with break on "S"
         * but NO FTE record filter.
         * Expect two spans.
         */
        SpanConfiguration spanConfig = createOnsisSpanConfiguration(getOrganization());
        spanConfig.ignoreForSpansCallback = null;
        EnrollmentSpanHelper spanHelper = new EnrollmentSpanHelper(spanConfig);

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());
        assertEquals(entry1Date, spans.get(0).getFirstActiveInSessionDate());
        assertEquals(fteDate, spans.get(1).getFirstActiveInSessionDate());

        /*
         * Test 2 - SpanHelper with break on "S" and provide FTE record filter.
         * Expect one span.
         */
        spanHelper = createOnsisSpanHelper();

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());
        assertEquals(entry1Date, spans.get(0).getFirstActiveInSessionDate());
    }

    /**
     * When calculating getLastActiveInSessionDate,
     * the starting date for scanning for last in-session date is either:
     * 1. from the W record adjusted by memberOnWithdrawal,
     * 2. or from the S record if breaking on S.
     *
     * If the scanning start date is on/after the district start date
     * but before the school's first in-session date,
     * the scanning start date should be downgraded to the last in-session date of the prior year,
     * and getLastActiveInSessionDate should return that date.
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_DowngradeWithdrawalBeforeSchoolStart() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        PlainDate currentYearContextStartDate = getContext().getStartDate();
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate = findFirstInSessionDate(schoolCalendar, currentYearContextStartDate, forward);

        // This test requires that the first in-session date is after district context start date
        // because the W date must be on/after district start date and before first in-session.
        // If memberOnWithdraw is false,
        // then the W date must be strictly after district start date).
        assertTrue("setup", currentYearContextStartDate.before(firstInSessionDate));

        PlainDate entryDateLastYear = DateUtils.add(getContext().getStartDate(), -100);
        boolean updateStudent = true;
        StudentEnrollment entry =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDateLastYear, updateStudent);

        PlainDate withdrawalDate = DateUtils.add(firstInSessionDate, -1);
        // withdrawalDate = findFirstInSessionDate(schoolCalendar, withdrawalDate, false);

        // Setup: verify districtStartDate < withdrawalDate < firstInSessionDate
        assertNotNull(withdrawalDate);
        assertTrue(currentYearContextStartDate.before(withdrawalDate));
        assertTrue(withdrawalDate.before(firstInSessionDate));

        StudentEnrollment withdrawal =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, updateStudent);

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());

        /*
         * Expect:
         * One span in prior year
         * - getLastActiveInSessionDate returns prior year's last in-session date
         * - the W record should be the last record in getEnrollments
         * - getDataStorageEnrollment returns the W record
         */
        assertEquals(1, spans.size());

        SchoolCalendar priorYearCalendar =
                findSchoolCalendar(getContext(-1).getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate lastYearLastInSessionDate =
                findFirstInSessionDate(priorYearCalendar, getContext(-1).getEndDate(), false);

        AnnualSpan lastYearSpan = spans.get(0);
        assertNotNull(lastYearSpan);
        assertEquals(lastYearLastInSessionDate, lastYearSpan.getLastActiveInSessionDate());

        List<StudentEnrollment> spanEnrollments = lastYearSpan.getAllEnrollmentsAscend();
        assertEquals(2, spanEnrollments.size());
        assertEquals(entry.getOid(), spanEnrollments.get(0).getOid());
        assertEquals(withdrawal.getOid(), spanEnrollments.get(1).getOid());

        StudentEnrollment dataStorageEnrollment = lastYearSpan.getDataStorageEnrollment();
        assertNotNull(dataStorageEnrollment);
        assertEquals(withdrawal.getOid(), dataStorageEnrollment.getOid());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_ConcurrentSimple() throws Exception {
        /*
         * Set up student
         */
        SisSchool highSchool = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        PlainDate currentYearContextStartDate = getContext().getStartDate();
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), highSchool.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate = findFirstInSessionDate(schoolCalendar, currentYearContextStartDate, forward);
        boolean updateStudent = true;
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, firstInSessionDate, updateStudent);
        PlainDate lastInSessionDate = findFirstInSessionDate(schoolCalendar, getContext().getEndDate(), false);

        /*
         * Add concurrent enrollment
         */
        SisSchool middleSchool = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);
        HashMap<String, Object> colValues = new HashMap<>();
        PlainDate startDate = getContext().getStartDate();

        colValues.put(StudentSchool.COL_ATTENDANCE_MANAGEMENT_TYPE,
                Integer.valueOf(StudentSchool.AttendanceManagementType.NONE.ordinal()));
        colValues.put(StudentSchool.COL_DISTRICT_CONTEXT_OID, getOrganization().getCurrentContextOid());
        // PlainDate endDate = getDistrictContext().getEndDate();
        // colValues.put(StudentSchool.COL_END_DATE, endDate);
        colValues.put(StudentSchool.COL_SCHOOL_OID, middleSchool.getOid());
        colValues.put(StudentSchool.COL_START_DATE, startDate);
        colValues.put(StudentSchool.COL_STUDENT_OID, student.getOid());
        colValues.put(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

        StudentSchool concurrent = (StudentSchool) buildTestBean(StudentSchool.class, getBroker(), colValues);

        // Setting this to null so that it bypassed required field validation
        concurrent.setEndDate(null);
        getBroker().saveBeanForced(concurrent);

        /*
         * Check the spans
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());

        AnnualSpan enrSpan = spans.get(0);
        AnnualSpan concurrentSpan = spans.get(1);

        assertTrue(!enrSpan.isSecondary());
        assertEquals(firstInSessionDate, enrSpan.getFirstActiveInSessionDate());
        assertEquals(lastInSessionDate, enrSpan.getLastActiveInSessionDate());

        assertTrue(concurrentSpan.isSecondary());
        assertEquals(firstInSessionDate, concurrentSpan.getFirstActiveInSessionDate());
        assertEquals(lastInSessionDate, concurrentSpan.getLastActiveInSessionDate());


        /*
         * Vary the start and end dates
         */
        PlainDate newStartDate = DateUtils.add(firstInSessionDate, 3);
        newStartDate = findFirstInSessionDate(schoolCalendar, newStartDate, forward);

        PlainDate newEndDate = DateUtils.add(lastInSessionDate, -3);
        newEndDate = findFirstInSessionDate(schoolCalendar, newEndDate, false);

        concurrent.setStartDate(newStartDate);
        concurrent.setEndDate(newEndDate);
        List<ValidationError> errors = getBroker().saveBean(concurrent);
        assertEquals(0, errors.size());

        /*
         * Check the spans
         */
        spanHelper = createOnsisSpanHelper();

        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());
        concurrentSpan = spans.get(1);

        assertTrue(concurrentSpan.isSecondary());
        assertEquals(newStartDate, concurrentSpan.getFirstActiveInSessionDate());
        assertEquals(newEndDate, concurrentSpan.getLastActiveInSessionDate());

        /*
         * Check that queryAsOfDate causes future span end date to return null
         */
        PlainDate queryAsOfDate1 = DateUtils.add(newEndDate, -5);
        boolean cutoffBeforeLastYear = false;
        spanHelper = createOnsisSpanHelper(queryAsOfDate1, cutoffBeforeLastYear);


        spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());
        concurrentSpan = spans.get(1);

        assertTrue(concurrentSpan.isSecondary());
        assertEquals(newStartDate, concurrentSpan.getFirstActiveInSessionDate());
        assertNull(concurrentSpan.getLastActiveInSessionDate());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_selectOverlappingRange() throws Exception {
        /*
         * Test EnrollmentSpanHelper->
         * public List<EnrollmentSpan> getEnrollmentSpans(SisStudent student,
         * PlainDate overlappingStartDate,
         * PlainDate overlappingEndDate)
         */
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         *
         * Set up one span from Day 30 thru Day 60
         */
        PlainDate currentYearContextStartDate = getContext().getStartDate();
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate = findFirstInSessionDate(schoolCalendar, currentYearContextStartDate, forward);

        // This test requires that the first in-session date is after district context start date
        // because the W date must be on/after district start date and before first in-session.
        // If memberOnWithdraw is false,
        // then the W date must be strictly after district start date).
        assertTrue("setup", currentYearContextStartDate.before(firstInSessionDate));

        PlainDate entryDate = DateUtils.add(getContext().getStartDate(), 30);
        entryDate = findFirstInSessionDate(schoolCalendar, entryDate, forward);
        boolean updateStudent = true;
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDate, updateStudent);

        PlainDate withdrawalDate = DateUtils.add(entryDate, 30);
        withdrawalDate = findFirstInSessionDate(schoolCalendar, withdrawalDate, false);

        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, "Withdrawn",
                "Inactive", updateStudent);

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        /*
         * Note:Square brackets [ ] represent query range
         */

        /*
         * Test:
         * --[--]--E--W--
         */
        List<AnnualSpan> spans;
        List<ValidationError> errors = new ArrayList<>();

        PlainDate overlappingEndDate = DateUtils.add(entryDate, -7);
        PlainDate overlappingStartDate = DateUtils.add(overlappingEndDate, -7);
        spans = spanHelper.getEnrollmentSpansActiveIn(student, overlappingStartDate, overlappingEndDate, errors,
                getBroker());
        assertTrue(errors.isEmpty());
        assertEquals(0, spans.size());

        /*
         * Test:
         * --E--W--[--]--
         */
        overlappingStartDate = DateUtils.add(withdrawalDate, 7);
        overlappingEndDate = DateUtils.add(overlappingStartDate, 7);
        spans = spanHelper.getEnrollmentSpansActiveIn(student, overlappingStartDate, overlappingEndDate, errors,
                getBroker());
        assertTrue(errors.isEmpty());
        assertEquals(0, spans.size());

        /*
         * Test:
         * --[--E--]--W--
         */
        overlappingStartDate = DateUtils.add(entryDate, -7);
        overlappingEndDate = DateUtils.add(entryDate, 7);
        spans = spanHelper.getEnrollmentSpansActiveIn(student, overlappingStartDate, overlappingEndDate, errors,
                getBroker());
        assertEquals(1, spans.size());

        /*
         * Test:
         * --E--[--W--]--
         */
        overlappingStartDate = DateUtils.add(entryDate, 7);
        overlappingEndDate = DateUtils.add(withdrawalDate, 7);
        spans = spanHelper.getEnrollmentSpansActiveIn(student, overlappingStartDate, overlappingEndDate, errors,
                getBroker());
        assertEquals(1, spans.size());

        /*
         * Test:
         * --E--[--]--W--
         */
        overlappingStartDate = DateUtils.add(entryDate, 7);
        overlappingEndDate = DateUtils.add(withdrawalDate, -7);
        spans = spanHelper.getEnrollmentSpansActiveIn(student, overlappingStartDate, overlappingEndDate, errors,
                getBroker());
        assertEquals(1, spans.size());

        /*
         * Test:
         * --[--E--W--]--
         */
        overlappingStartDate = DateUtils.add(entryDate, -7);
        overlappingEndDate = DateUtils.add(withdrawalDate, 7);
        spans = spanHelper.getEnrollmentSpansActiveIn(student, overlappingStartDate, overlappingEndDate, errors,
                getBroker());
        assertEquals(1, spans.size());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_QueryAsOfDate() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * Setup: Create 3 spans from 2 years ago through current year
         */
        boolean forward = true;
        DistrictSchoolYearContext contextYear1 = getContext(-2);
        PlainDate entryDateYear1 = contextYear1.getStartDate();

        SchoolCalendar schoolCalendarYear1 =
                findSchoolCalendar(contextYear1.getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate nextInSessionDateYear1 =
                findFirstInSessionDate(schoolCalendarYear1, entryDateYear1, forward);
        SisSchoolCalendarDate lastInSessionDateYear1 =
                CalendarManager.getLastInSessionDate(schoolCalendarYear1, getBroker());

        DistrictSchoolYearContext contextYear2 = getContext(-1);
        SchoolCalendar schoolCalendarYear2 =
                findSchoolCalendar(contextYear2.getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        SisSchoolCalendarDate firstInSessionDateYear2 =
                CalendarManager.getFirstInSessionDate(schoolCalendarYear2, getBroker());
        // SisSchoolCalendarDate lastInSessionDateYear2 =
        // CalendarManager.getLastInSessionDate(schoolCalendarYear2, getBroker());

        PlainDate withdrawalDate = DateUtils.add(getContext().getStartDate(), 70);

        SchoolCalendar schoolCalendarYear3 =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        // PlainDate firstInSessionDateYear3 =
        findFirstInSessionDate(schoolCalendarYear3, getContext().getStartDate(), forward);
        forward = false;
        // PlainDate expectedLastInSessionDate = findFirstInSessionDate(schoolCalendarYear3,
        // withdrawalDate, forward);

        /*
         * Create Entry record two years back
         */
        boolean updateStudent = true;
        StudentEnrollment entry =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, nextInSessionDateYear1,
                        updateStudent);

        /*
         * Create Withdrawal record this year
         */
        StudentEnrollment withdrawal =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, updateStudent);


        /*
         * Create a SpanHelper that queries as of the middle of Year 1
         */
        PlainDate queryAsOfDate1 = DateUtils.add(nextInSessionDateYear1, 21);
        boolean cutoffBeforeLastYear = false;
        EnrollmentSpanHelper spanHelper1 = createOnsisSpanHelper(queryAsOfDate1, cutoffBeforeLastYear);

        /*
         * Get Parent Span - one span covering all years (up to queryAsOfDate)
         */
        List<ParentSpan> parentSpans1 = spanHelper1.getParentSpans(student, null, getBroker());
        assertEquals(1, parentSpans1.size());
        ParentSpan parentSpan1 = parentSpans1.get(0);
        assertEquals(1, parentSpan1.getEnrollments().size());
        assertEquals(entry, parentSpan1.getEnrollments().get(0));
        assertNull(parentSpan1.getLastActiveInSessionDate());

        /*
         * Get child spans - one span per year
         */
        List<AnnualSpan> spans1 = spanHelper1.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans1.size());

        /*
         * Check first child span
         */
        AnnualSpan firstSpan1 = spans1.get(0);
        assertEquals(nextInSessionDateYear1, firstSpan1.getFirstActiveInSessionDate());
        assertNull(firstSpan1.getLastActiveInSessionDate());
        assertTrue(firstSpan1.isEntry());
        assertFalse(firstSpan1.isWithdrawal());
        assertNull(firstSpan1.getWithdrawalMemberDate());
        assertEquals(1, firstSpan1.getAllEnrollmentsAscend().size());
        assertEquals(entry, firstSpan1.getAllEnrollmentsAscend().get(0));
        assertEquals(1, firstSpan1.getEnrollmentsInsideSpanAscend().size());
        assertEquals(entry, firstSpan1.getEnrollmentsInsideSpanAscend().get(0));
        assertEquals(withdrawal, firstSpan1.getDataStorageEnrollment());
        assertNull(firstSpan1.getTerminatingEnrollment());



        /*
         * Create a SpanHelper that queries as of the middle of Year 2
         */
        PlainDate queryAsOfDate2 = DateUtils.add(firstInSessionDateYear2.getDate(), 21);
        EnrollmentSpanHelper spanHelper2 = createOnsisSpanHelper(queryAsOfDate2, cutoffBeforeLastYear);

        /*
         * Get Parent Span - one span covering all years (up to queryAsOfDate)
         */
        List<ParentSpan> parentSpans2 = spanHelper2.getParentSpans(student, null, getBroker());
        assertEquals(1, parentSpans2.size());
        ParentSpan parentSpan2 = parentSpans2.get(0);
        assertEquals(1, parentSpan2.getEnrollments().size());
        assertEquals(entry, parentSpan2.getEnrollments().get(0));
        assertNull(parentSpan2.getLastActiveInSessionDate());

        /*
         * Get two child spans - one span per year
         */
        List<AnnualSpan> spans2 = spanHelper2.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans2.size());

        /*
         * Check first child span
         */
        AnnualSpan firstSpan2 = spans2.get(0);
        assertEquals(nextInSessionDateYear1, firstSpan2.getFirstActiveInSessionDate());
        assertEquals(lastInSessionDateYear1.getDate(), firstSpan2.getLastActiveInSessionDate());
        assertTrue(firstSpan2.isEntry());
        assertFalse(firstSpan2.isWithdrawal());
        assertNull(firstSpan2.getWithdrawalMemberDate());
        assertEquals(1, firstSpan2.getAllEnrollmentsAscend().size());
        assertEquals(entry, firstSpan2.getAllEnrollmentsAscend().get(0));
        assertEquals(1, firstSpan2.getEnrollmentsInsideSpanAscend().size());
        assertEquals(entry, firstSpan2.getEnrollmentsInsideSpanAscend().get(0));
        assertEquals(withdrawal, firstSpan2.getDataStorageEnrollment());
        assertNull(firstSpan2.getTerminatingEnrollment());

        /*
         * Check second child span
         */
        AnnualSpan secondSpan2 = spans2.get(1);
        assertEquals(firstInSessionDateYear2.getDate(), secondSpan2.getFirstActiveInSessionDate());
        assertNull(secondSpan2.getLastActiveInSessionDate());
        assertEquals(1, secondSpan2.getAllEnrollmentsAscend().size());
        assertEquals(entry, secondSpan2.getAllEnrollmentsAscend().get(0));
        assertEquals(0, secondSpan2.getEnrollmentsInsideSpanAscend().size());
        assertEquals(withdrawal, secondSpan2.getDataStorageEnrollment());
        assertNull(secondSpan2.getTerminatingEnrollment());
    }

    /**
     *
     * Test that the parentSpan lastActiveDateInSessionDate returns:
     * - null if the last (child) AnnualSpan is not terminated by a W record
     * - else lastActiveDateInSessionDate of the last (child) AnnualSpan
     *
     * @throws Exception
     */
    @Test
    public void test_parentSpanLastActiveDateIsNullOrChildLastActive() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        /*
         * Setup: Create span from last year through the current year
         */
        DistrictSchoolYearContext contextLastYear = getContext(-1);
        PlainDate entryDateLastYear = contextLastYear.getStartDate();

        /*
         * Create Entry enrollment last year
         */
        boolean updateStudent = true;
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDateLastYear, updateStudent);

        /*
         * Get Parent Span - one span covering two years
         */
        List<ValidationError> errors = new ArrayList<>();
        List<ParentSpan> parentSpans = spanHelper.getParentSpans(student, errors, getBroker());
        assertEquals(1, parentSpans.size());
        assertEquals(2, parentSpans.get(0).getAnnualSpans().size());

        /*
         * Expect parentSpan lastActiveDate == null
         */
        assertNull(parentSpans.get(0).getLastActiveInSessionDate());

        /*
         * Create Withdrawal this year
         */
        PlainDate withdrawalDate = DateUtils.add(getContext().getStartDate(), 70);
        PlainDate w1Date = DateUtils.add(withdrawalDate, -1);

        SchoolCalendar schoolCalendarThisYear =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = false;
        PlainDate expectedLastInSessionDate = findFirstInSessionDate(schoolCalendarThisYear, w1Date, forward);

        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, updateStudent);
        spanHelper = createOnsisSpanHelper();

        /*
         * Expect parentSpan lastActiveDate == actual date
         * Expect childSpan lastActiveDate == actual date
         */
        parentSpans = spanHelper.getParentSpans(student, errors, getBroker());
        assertEquals(1, parentSpans.size());
        assertEquals(2, parentSpans.get(0).getAnnualSpans().size());
        assertEquals(expectedLastInSessionDate, parentSpans.get(0).getLastActiveInSessionDate());
        assertEquals(expectedLastInSessionDate,
                parentSpans.get(0).getAnnualSpans().get(1).getLastActiveInSessionDate());

        /*
         * Get spans with queryAsOfDate before Withdrawal
         *
         * Expect parentSpan lastActiveDate == null
         * Expect childSpan lastActiveDate == null
         */
        PlainDate queryAsOfDate = DateUtils.add(expectedLastInSessionDate, -3);
        boolean cutoffBeforeLastYear = true;
        spanHelper = createOnsisSpanHelper(queryAsOfDate, cutoffBeforeLastYear);

        parentSpans = spanHelper.getParentSpans(student, errors, getBroker());
        assertEquals(1, parentSpans.size());
        assertEquals(2, parentSpans.get(0).getAnnualSpans().size());
        assertNull(parentSpans.get(0).getLastActiveInSessionDate());
        assertNull(parentSpans.get(0).getAnnualSpans().get(1).getLastActiveInSessionDate());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_getStudents() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * Setup: Create span that begins first in-session day this year
         * and has a W date on today.
         *
         * getStudents using lastActive: student does not qualify
         * getStudents using firstInactive: student qualifies
         */
        boolean forward = true;
        DistrictSchoolYearContext thisYear = getContext();

        SchoolCalendar schoolCalendarYear1 =
                findSchoolCalendar(thisYear.getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate firstInSessionDateYear1 =
                findFirstInSessionDate(schoolCalendarYear1, thisYear.getStartDate(), forward);

        PlainDate withdrawalDate = new PlainDate();

        /*
         * Create Entry record
         */
        boolean updateStudent = true;
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, firstInSessionDateYear1,
                updateStudent);

        /*
         * Create Withdrawal record today
         */
        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, updateStudent);


        /*
         * Create a SpanHelper
         */
        EnrollmentSpanHelper spanHelper1 = createOnsisSpanHelper();

        /*
         * getStudents using W date as start of range
         * using lastActive: student does not qualify
         */
        SpanCriteria spanCriteria = new EnrollmentSpanHelper.SpanCriteria();
        spanCriteria.limitingStudentOids = Arrays.asList(student.getOid());
        spanCriteria.schoolOids = Arrays.asList(school.getOid());
        //TODO: Ken Bakke
        //spanCriteria.setUseLastActiveEndDate();
        spanCriteria.setSpanOverlap(withdrawalDate, DateUtils.add(withdrawalDate, 7));

        List<ValidationError> errors = new ArrayList<>();
        Collection<SisStudent> students = spanHelper1.getStudents(spanCriteria, errors, getBroker());

        assertEquals(0, errors.size());
        assertEquals(0, students.size());


        /*
         * getStudents using firstInactive: student does qualify
         */
        //TODO: Ken Bakke
        //spanCriteria.setUseFirstInactiveEndDate();

        students = spanHelper1.getStudents(spanCriteria, errors, getBroker());

        assertEquals(0, errors.size());
        assertEquals(1, students.size());
    }

    /*
     * Test on-the-fly swap E/W records that are reversed based on just timestamp.
     *
     * Condition:
     * 1. W (sklA) and E (sklB) record on same date, with W timestamp > E timestamp, but
     * different schoolOids
     *
     * Further validation:
     * 1. Next record after W is for sklB
     * 2. Prev record before E is for sklA
     *
     * Then swap so E timestamp > W timestamp
     */
    @Test
    public void spanHelperTest_badData_EWtimestamps() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollments (newest down to oldest)
         *
         * W HS
         * <time elapses>
         * W elem - these two are same date
         * E HS - these two are same date
         * <time elapses>
         * E elem
         *
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entryElem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, ENTRY, firstInSessionDate, enrollmentCode,
                        getActiveStatusCode(), updateStudent);

        PlainDate sharedDate = DateUtils.add(firstInSessionDate, 5);
        StudentEnrollment entryHS =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, sharedDate, enrollmentCode,
                        getActiveStatusCode(), updateStudent);

        StudentEnrollment withdrawalElem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, WITHDRAWAL, sharedDate, "Withdrawn",
                        "Inactive", updateStudent);
        withdrawalElem.setTimestamp(entryHS.getTimestamp() + 100);
        saveDirtyBean(withdrawalElem);

        PlainDate withdrawalHsDate = DateUtils.add(sharedDate, 2);
        StudentEnrollment withdrawalHS =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalHsDate, "Withdrawn",
                        "Inactive", updateStudent);

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());

        assertEquals(2, spans.size());


        AnnualSpan spanElem = spans.get(0);
        assertEquals(DEFAULT_SKL_ELEM_SCHOOL_OID, spanElem.getSchool().getOid());
        assertTrue(spanElem.getFirstActiveInSessionDate().equals(firstInSessionDate));
        assertTrue(!spanElem.getLastActiveInSessionDate().after(sharedDate));
        assertEquals(2, spanElem.getAllEnrollmentsAscend().size());
        assertEquals(entryElem, spanElem.getFirstActiveEnrollment());
        assertEquals(withdrawalElem, spanElem.getTerminatingEnrollment());
        assertEquals(entryElem, spanElem.getAllEnrollmentsAscend().get(0));
        assertEquals(withdrawalElem, spanElem.getAllEnrollmentsAscend().get(1));

        AnnualSpan spanHs = spans.get(1);
        assertEquals(DEFAULT_SKL_SECONDARY_SCHOOL_OID, spanHs.getSchool().getOid());
        assertTrue(!spanHs.getFirstActiveInSessionDate().before(sharedDate));
        assertTrue(spanHs.getLastActiveInSessionDate().after(sharedDate));
        assertEquals(2, spanHs.getAllEnrollmentsAscend().size());
        assertEquals(entryHS, spanHs.getFirstActiveEnrollment());
        assertEquals(withdrawalHS, spanHs.getTerminatingEnrollment());
        assertEquals(entryHS, spanHs.getAllEnrollmentsAscend().get(0));
        assertEquals(withdrawalHS, spanHs.getAllEnrollmentsAscend().get(1));
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_badData_TwoE_DiffSchool() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_ELEM_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        /*
         * Create "E" Active at start of school year
         */
        PlainDate entry1Date = firstInSessionDate;
        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entryElem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, ENTRY, entry1Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "E" Active in DIFFERENT school shortly after first E record
         */
        PlainDate entry2Date = findFirstInSessionDate(schoolCalendar, DateUtils.add(entry1Date, 5), forward);
        PlainDate expectedSpan1LastActive =
                findFirstInSessionDate(schoolCalendar, DateUtils.add(entry2Date, -1), false);

        StudentEnrollment entryHs =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry2Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Expect two spans, first span is terminated by the second E record.
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(2, spans.size());

        AnnualSpan elemSpan = spans.get(0);
        assertEquals(entry1Date, elemSpan.getFirstActiveInSessionDate());
        assertEquals(1, elemSpan.getAllEnrollmentsAscend().size());
        assertEquals(entryElem.getOid(), elemSpan.getAllEnrollmentsAscend().get(0).getOid());
        assertEquals(entry2Date, elemSpan.getFirstInactiveInSessionDate());
        assertEquals(expectedSpan1LastActive, elemSpan.getLastActiveInSessionDate());
        assertEquals(entryHs, elemSpan.getTerminatingEnrollment());

        AnnualSpan hsSpan = spans.get(1);
        assertTrue(!hsSpan.getFirstActiveInSessionDate().before(entry2Date));
        assertEquals(1, hsSpan.getAllEnrollmentsAscend().size());
        assertEquals(entryHs.getOid(), hsSpan.getAllEnrollmentsAscend().get(0).getOid());
    }

    /**
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_badData_InvalidSpanDiffSchoolTerminatesPrevE() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_ELEM_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        /*
         * Create "E" Active at start of school year
         */
        PlainDate entry1Date = firstInSessionDate;
        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entryElem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, ENTRY, entry1Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "E" Inactive in DIFFERENT school shortly after first E record
         */
        PlainDate entry2Date = findFirstInSessionDate(schoolCalendar, DateUtils.add(entry1Date, 5), forward);
        StudentEnrollment entryHs =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entry2Date,
                        enrollmentCode, "Inactive", updateStudent);
        PlainDate expectedSpan1LastActive =
                findFirstInSessionDate(schoolCalendar, DateUtils.add(entry2Date, -1), false);

        /*
         * Expect one span terminated by the second E record.
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(1, spans.size());

        AnnualSpan elemSpan = spans.get(0);
        assertEquals(entry1Date, elemSpan.getFirstActiveInSessionDate());
        assertEquals(1, elemSpan.getAllEnrollmentsAscend().size());
        assertEquals(entryElem.getOid(), elemSpan.getAllEnrollmentsAscend().get(0).getOid());
        assertEquals(entry2Date, elemSpan.getFirstInactiveInSessionDate());
        assertEquals(expectedSpan1LastActive, elemSpan.getLastActiveInSessionDate());
        assertEquals(entryHs, elemSpan.getTerminatingEnrollment());
    }

    @Test
    public void spanHelperTest_badData_ZeroDaySpan() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8");

        /*
         * StudentEnrollments (newest down to oldest)
         * W elem - these two are same date
         * E elem - these two are same date
         */
        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDate =
                findFirstInSessionDate(schoolCalendar, getContext().getStartDate(), forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entryElem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, ENTRY, firstInSessionDate, enrollmentCode,
                        getActiveStatusCode(), updateStudent);

        StudentEnrollment withdrawalElem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, WITHDRAWAL, firstInSessionDate, "Withdrawn",
                        "Inactive", updateStudent);
        withdrawalElem.setTimestamp(entryElem.getTimestamp() + 100);
        saveDirtyBean(withdrawalElem);

        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());

        assertEquals(0, spans.size());
    }

    /**
     * Test that a lone S record between spans, in a different school,
     * is correctly ignored
     *
     * W 06/20/2008 SchoolA Inactive
     * E 03/07/2008 SchoolA Active
     * S 10/04/2007 SchoolB Active <==== S record, between spans, different school
     * W 10/18/2006 SchoolA Inactive
     * E 01/09/2006 SchoolA Active
     *
     * @throws Exception
     */
    @Test
    public void spanHelperTest_badData_Srecord_wrongSchool_between_spans() throws Exception {
        /*
         * Set up data
         */
        SisSchool elemSchool = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        SisStudent student = createStudent(lastName, firstName, DEFAULT_SKL_ELEM_SCHOOL_OID, "8");

        /*
         * StudentEnrollment
         */
        DistrictSchoolYearContext lastYearContext = getContext(-1);
        SchoolCalendar schoolCalendarElemLastYear =
                findSchoolCalendar(lastYearContext.getOid(), elemSchool.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate firstInSessionDateElem =
                findFirstInSessionDate(schoolCalendarElemLastYear, lastYearContext.getStartDate(), forward);

        /*
         * Create "E" Active at start of last year
         */
        PlainDate entry1Date = firstInSessionDateElem;
        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        StudentEnrollment entry1Elem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, ENTRY, entry1Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "W" Inactive shortly after
         */
        PlainDate withdrawal1Date = DateUtils.add(entry1Date, 7);
        StudentEnrollment withdrawal1Elem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, WITHDRAWAL, withdrawal1Date, "Withdrawn",
                        "Inactive", updateStudent);
        withdrawal1Elem.setTimestamp(entry1Elem.getTimestamp() + 100);
        saveDirtyBean(withdrawal1Elem);

        /*
         * Create lone "S" record at start of this year in different school
         */
        SisSchool highSchool = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        SchoolCalendar schoolCalendarHS =
                findSchoolCalendar(getContext().getOid(), highSchool.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        PlainDate firstInSessionDateHs =
                findFirstInSessionDate(schoolCalendarHS, getContext().getStartDate(), forward);
        PlainDate sRecordDate = firstInSessionDateHs;

        StudentEnrollment sRecordHs =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, sRecordDate,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "E" Active this year AFTER the S record
         */
        PlainDate entry2Date = DateUtils.add(sRecordDate, 7);
        StudentEnrollment entry2Elem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, ENTRY, entry2Date,
                        enrollmentCode, getActiveStatusCode(), updateStudent);

        /*
         * Create "W" Inactive shortly after
         */
        PlainDate withdrawal2Date = DateUtils.add(entry2Date, 7);
        StudentEnrollment withdrawal2Elem =
                createEnrollment(student, DEFAULT_SKL_ELEM_SCHOOL_OID, WITHDRAWAL, withdrawal2Date, "Withdrawn",
                        "Inactive", updateStudent);
        withdrawal2Elem.setTimestamp(entry2Elem.getTimestamp() + 100);
        saveDirtyBean(withdrawal2Elem);



        /*
         * Expect two spans, first span is terminated by the second E record.
         */
        EnrollmentSpanHelper spanHelper = createOnsisSpanHelper();

        List<AnnualSpan> spans = spanHelper.getEnrollmentSpans(student, null, getBroker());
        assertEquals(3, spans.size());

        AnnualSpan hsSpan = spans.get(1);
        assertEquals(sRecordHs, hsSpan.getAllEnrollmentsAscend().get(0));
        assertEquals(1, hsSpan.getAllEnrollmentsAscend().size());
        assertEquals(entry2Elem, hsSpan.getTerminatingEnrollment());
    }
}
