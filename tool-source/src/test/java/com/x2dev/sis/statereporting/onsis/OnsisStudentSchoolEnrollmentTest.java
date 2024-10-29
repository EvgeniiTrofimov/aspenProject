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

import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_ENR_GRADE_LEVEL;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.ALIAS_STD_OEN;
import static com.x2dev.sis.model.beans.StudentEnrollment.ENTRY;
import static com.x2dev.sis.model.beans.StudentEnrollment.STATUS_CHANGE;
import static com.x2dev.sis.model.beans.StudentEnrollment.WITHDRAWAL;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.SubmissionType;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.types.PlainDate;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnsisStudentSchoolEnrollmentTest extends OnsisBaseTest {
    @Test
    public void OnsisStudentSchoolEnrollmentTest_Root() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);
        PlainDate entryDate = setupInfo.enrollment.getEnrollmentDate();
        String entryDateStr = getOnsisHelper().getOnsisDateFormat().format(entryDate);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARELEM2;
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo.student.getOid();
        String staffOid = setupInfo.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        /*
         * Expected output
         */
        String classCode = setupInfo.course.getNumber() + "-" + setupInfo.section.getSectionNumber();
        String gradeLevel = setupInfo.student.getGradeLevel();
        String expectedEnrollment = "<STUDENT_SCHOOL_ENROLMENT>\n" +
                "    <ACTION>ADD</ACTION>\n" +
                "    <GRADE_TYPE>" + gradeLevel + "</GRADE_TYPE>\n" +
                "    <CLASS_CODE>" + classCode + "</CLASS_CODE>\n" +
                "    <LANGUAGE_TYPE>E</LANGUAGE_TYPE>\n" +
                "    <STU_BRD_RES_STAT_TYPE/>\n" +
                "    <ATTENDANCE_TYPE/>\n" +
                "    <EDP_TYPE/>\n" +
                "    <RESIDENCE_STATUS_TYPE/>\n" +
                "    <CURRENT_RESIDENCE_COUNTRY/>\n" +
                "    <CURRENT_RESIDENCE_PROVINCE/>\n" +
                "    <INDIGENOUS_SELF_IDENTIFICATION/>\n" +
                "    <STUDENT_MOBILITY_TYPE/>\n" +
                "    <COUNTRY_TYPE/>\n" +
                "    <PROVINCE_STATE_TYPE/>\n" +
                "    <LANGUAGE_TYPE_PREVIOUS_SCH/>\n" +
                "    <STUDENT_MOBILITY_TYPE_EXIT/>\n" +
                "    <COUNTRY_TYPE_EXIT/>\n" +
                "    <PROVINCE_STATE_TYPE_EXIT/>\n" +
                "    <POSTAL_AREA_TYPE/>\n" +
                "    <COUNTRY_TYPE_BIRTH/>\n" +
                "    <PROVINCE_STATE_TYPE_BIRTH/>\n" +
                "    <COUNTRY_TYPE_PERM/>\n" +
                "    <PROVINCE_STATE_TYPE_PERM/>\n" +
                "    <YEAR_OF_ENTRY_TO_CANADA/>\n" +
                "    <MAIN_SCHOOL_FLAG>T</MAIN_SCHOOL_FLAG>\n" +
                "    <SCHOOL_STUDENT_NUMBER/>\n" +
                "    <FRENCH_ADMISSION_APPROVAL_DATE/>\n" +
                "    <LOCAL_SCHOOL_PROGRAM_FLAG/>\n" +
                "    <FTE>0.00</FTE>\n" +
                "    <SPECIAL_EDUCATION_FLAG>F</SPECIAL_EDUCATION_FLAG>\n" +
                "    <ENROLMENT_START_DATE>" + entryDateStr + "</ENROLMENT_START_DATE>\n" +
                "</STUDENT_SCHOOL_ENROLMENT>";

        /*
         * Perform the export
         */
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                staffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Expect STUDENT_SCHOOL_ENROLMENT
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo.schoolBSID + "']";
        String pathToStudent = "STUDENT[OEN='" + setupInfo.cleanOen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT[ENROLMENT_START_DATE='" + entryDateStr + "']";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrolmentElem = assertElement(studentElem, pathToEnrolment);
        assertSubset(expectedEnrollment, enrolmentElem);
    }

    /**
     *
     * Test that a 0-day span doesn't export
     * except when it's in the CSV extract.
     *
     * I.e., a W record in the submission period but before an in-session day.
     *
     * @throws Exception
     */
    @Test
    public void OnsisStudentSchoolEnrollmentTest_SuppressZeroDaySpan() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        String oen = generateCleanOEN();
        SisStudent student = createStudentWithProperties(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8",
                ALIAS_STD_OEN, oen);

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
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDateLastYear, updateStudent);

        PlainDate withdrawalDate = DateUtils.add(firstInSessionDate, -1);
        // withdrawalDate = findFirstInSessionDate(schoolCalendar, withdrawalDate, false);

        // Setup: verify districtStartDate < withdrawalDate < firstInSessionDate
        assertNotNull(withdrawalDate);
        assertTrue(currentYearContextStartDate.before(withdrawalDate));
        assertTrue(withdrawalDate.before(firstInSessionDate));

        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawalDate, updateStudent);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_OCTSEC1;
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = student.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        /*
         * Perform the export - without enrolment in CSV
         */
        String nullStaffOid = null;
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Expect no STUDENT_SCHOOL_ENROLMENT
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_SECONDARY_BSID + "']";
        String pathToStudent = "STUDENT[OEN='" + oen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        assertNoElement(schoolElem, pathToStudent);

        /*
         * Add Enrolment to CSV
         */
        enrolmentsCsv = createEnrolmentCsv(submissionTypeCode, DEFAULT_ACADEMIC_YEAR, oen, entryDateLastYear,
                DEFAULT_SKL_SECONDARY_BSID, school.getName());

        /*
         * Perform the export - with enrolment in CSV
         */
        nullStaffOid = null;
        xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Expect one STUDENT_SCHOOL_ENROLMENT
         * with startDate = last year
         * and endDate = W-1
         */

        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, pathToData);
        schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);

        String entryDateStr = getOnsisHelper().getOnsisDateFormat().format(entryDateLastYear);
        pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT[ENROLMENT_START_DATE='" + entryDateStr + "']";

        PlainDate expectedEndDate = DateUtils.add(withdrawalDate, -1);
        String endDateStr = getOnsisHelper().getOnsisDateFormat().format(expectedEndDate);
        Element enrolmentElem = assertElement(studentElem, pathToEnrolment);
        assertContent(enrolmentElem, "ENROLMENT_END_DATE", endDateStr);
    }


    /**
     *
     * Test that a StudentSchool span gets Grade Type (level)
     * from the primary span's StudentSchoolEnrollment record.
     *
     * @throws Exception
     */
    @Test
    public void OnsisStudentSchoolEnrollmentTest_SecondarySpanGetsGradeTypeFromPrimarySpan() throws Exception {
        /*
         * Set up data
         */
        SisSchool primarySchool = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(primarySchool, null, isPrimary, null);
        StudentEnrollment entryRecord = setupInfo.enrollment;
        SisStudent student = setupInfo.student;
        String oen = setupInfo.cleanOen;

        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARSEC1;
        SubmissionType submissionType = getSubmissionType(submissionTypeCode);

        SisSchool secondSchool = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);

        /*
         * Set up the primary enrollment:
         * - E record before reporting period with Grade Level 09
         * - S record during reporting period with Grade Level 10
         * - S record after reporting period with Grade Level 11
         * - Student record Grade Level 12
         */
        entryRecord.setFieldValueByAlias(ALIAS_ENR_GRADE_LEVEL, "09");
        saveDirtyBean(entryRecord);

        PlainDate sRecordDuringDate = submissionType.getPeriodStartDate();
        boolean updateStudent = false;
        StudentEnrollment sRecordDuring =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, sRecordDuringDate,
                        updateStudent);
        sRecordDuring.setFieldValueByAlias(ALIAS_ENR_GRADE_LEVEL, "10");
        saveDirtyBean(sRecordDuring);

        PlainDate sRecordAfterDate = DateUtils.add(submissionType.getPeriodEndDate(), 1);
        StudentEnrollment sRecordAfter =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, STATUS_CHANGE, sRecordAfterDate,
                        updateStudent);
        sRecordAfter.setFieldValueByAlias(ALIAS_ENR_GRADE_LEVEL, "11");
        saveDirtyBean(sRecordAfter);

        student.setGradeLevel("12");
        saveDirtyBean(student);


        /*
         * Create a secondary enrollment that overlaps current context
         */
        // StudentSchool ssk =
        createSecondaryEnrollment(student, secondSchool);


        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(primarySchool.getOid(), secondSchool.getOid());
        String studentOid = student.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        String expectedStateCode = lookupStateCodeByAlias("10", ALIAS_ENR_GRADE_LEVEL);

        /*
         * Perform the export
         */
        String nullStaffOid = null;
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML for the primary enrollment
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToPrimarySchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_SECONDARY_BSID + "']";
        String pathToStudent = "STUDENT[OEN='" + oen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element primarySchoolElem = assertElement(dataElem, pathToPrimarySchool);
        Element primaryStudent = assertElement(primarySchoolElem, pathToStudent);
        Element primaryEnrolmentElem = assertElement(primaryStudent, pathToEnrolment);
        assertContent(primaryEnrolmentElem, "GRADE_TYPE", expectedStateCode);

        /*
         * Check the output XML for the secondary enrollment
         */
        String pathToSecondSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_ELEM_BSID + "']";

        Element secondSchoolElem = assertElement(dataElem, pathToSecondSchool);
        Element secondStudent = assertElement(secondSchoolElem, pathToStudent);
        Element secondEnrolmentElem = assertElement(secondStudent, pathToEnrolment);
        assertContent(secondEnrolmentElem, "GRADE_TYPE", expectedStateCode);

        /*
         * Remove the Grade 10 S record and verify it pulls Grade 09
         * from the Entry record before the reporting period.
         *
         * Code "09" on the record translates to "9" state value
         */
        getBroker().deleteBean(sRecordDuring);
        expectedStateCode = lookupStateCodeByAlias("09", ALIAS_ENR_GRADE_LEVEL);

        /*
         * Perform the export
         */
        xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML for the primary enrollment
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, pathToData);
        primarySchoolElem = assertElement(dataElem, pathToPrimarySchool);
        primaryStudent = assertElement(primarySchoolElem, pathToStudent);
        primaryEnrolmentElem = assertElement(primaryStudent, pathToEnrolment);
        assertContent(primaryEnrolmentElem, "GRADE_TYPE", expectedStateCode);

        /*
         * Check the output XML for the secondary enrollment
         */
        secondSchoolElem = assertElement(dataElem, pathToSecondSchool);
        secondStudent = assertElement(secondSchoolElem, pathToStudent);
        secondEnrolmentElem = assertElement(secondStudent, pathToEnrolment);
        assertContent(secondEnrolmentElem, "GRADE_TYPE", expectedStateCode);
    }

    /**
     * Enrollment End Date:
     *
     * Always return span's LastActiveInSession Date UNLESS:
     * - "Future"
     * - Condition: span LastActiveInSession > End Date
     * - Return: null
     *
     * - "Summer Withdraw needs to report"
     * - Condition: Span's LastActiveInSessionDate is < Report Start
     * - Return: W-1 date (assuming W exists and W-1 >= Start Date; else null)
     *
     * @return
     * @throws Exception
     */
    @Test
    public void OnsisStudentSchoolEnrollmentTest_SummerWithdrawalEndDate() throws Exception {
        /*
         * Set up enrollment:
         * E last year
         * W dated 7/2 (note mbrOnWith = false)
         *
         * Expect:
         * Exported EnrolmentEndDate == W-1 (7/1)
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        String oen = generateCleanOEN();
        SisStudent student = createStudentWithProperties(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8",
                ALIAS_STD_OEN, oen);

        /*
         * StudentEnrollment
         */
        PlainDate entryDateLastYear = DateUtils.add(getContext().getStartDate(), -100);
        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDateLastYear, enrollmentCode,
                getActiveStatusCode(), updateStudent);

        String submissionTypeCode = SUBMISSION_TYPE_CODE_OCTSEC1;
        SubmissionType submissionType = getSubmissionType(submissionTypeCode);

        PlainDate withdrawDate = DateUtils.add(submissionType.getPeriodStartDate(), 1);
        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawDate, "Withdrawn",
                "Inactive", updateStudent);

        /*
         * Define CSV import files
         * Add Enrolment to CSV
         */
        String enrolmentsCsv = createEnrolmentCsv(submissionTypeCode, DEFAULT_ACADEMIC_YEAR, oen, entryDateLastYear,
                DEFAULT_SKL_SECONDARY_BSID, school.getName());

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = student.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        /*
         * Perform the export - without enrolment in CSV
         */
        String nullStaffOid = null;
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Exported EnrolmentEndDate == W-1 (7/1)
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_SECONDARY_BSID + "']";
        String pathToStudent = "STUDENT[OEN='" + oen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrollmentElem = assertElement(studentElem, pathToEnrolment);

        String endDateStr = getOnsisHelper().getOnsisDateFormat().format(submissionType.getPeriodStartDate());

        assertContent(enrollmentElem, "ENROLMENT_END_DATE", endDateStr);
    }

    /**
     * Test that a span that enters and withdraws during the period
     * returns End Date that is LastActiveInSessionDate
     * rather than W-1 date.
     *
     * @throws Exception
     */
    @Test
    public void OnsisStudentSchoolEnrollmentTest_EnterWithdrawDuringRpt_LastActive() throws Exception {
        /*
         * Set up enrollment:
         * E == first in-session day during the report period e.g. 9/3/2019
         * W == after Entry date during the report period BUT W-1 must not be in-session
         *
         * Expect:
         * Exported EnrolmentEndDate < W-1 date (should be last in-session day BEFORE W-1 date)
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        String oen = generateCleanOEN();
        SisStudent student = createStudentWithProperties(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8",
                ALIAS_STD_OEN, oen);

        /*
         * StudentEnrollment
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_OCTSEC1;
        SubmissionType submissionType = getSubmissionType(submissionTypeCode);

        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate entryDate = findFirstInSessionDate(schoolCalendar, submissionType.getPeriodStartDate(), forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDate, enrollmentCode,
                getActiveStatusCode(), updateStudent);

        PlainDate fiveDaysAfterEntry = DateUtils.add(entryDate, 5);
        PlainDate withdrawMinusOne = findFirstNotInSessionDate(schoolCalendar, fiveDaysAfterEntry, forward);
        PlainDate withdrawDate = DateUtils.add(withdrawMinusOne, 1);

        forward = false;
        PlainDate lastActiveDate = findFirstInSessionDate(schoolCalendar, withdrawMinusOne, forward);

        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawDate, "Withdrawn",
                "Inactive", updateStudent);

        /*
         * Define CSV import files
         * Add Enrolment to CSV
         */
        String enrolmentsCsv = createEnrolmentCsv(submissionTypeCode, DEFAULT_ACADEMIC_YEAR, oen, entryDate,
                DEFAULT_SKL_SECONDARY_BSID, school.getName());

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = student.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        /*
         * Perform the export - without enrolment in CSV
         */
        String nullStaffOid = null;
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Exported EnrolmentEndDate < W-1 date (should be last in-session day BEFORE W-1 date)
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_SECONDARY_BSID + "']";
        String pathToStudent = "STUDENT[OEN='" + oen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrollmentElem = assertElement(studentElem, pathToEnrolment);

        String endDateStr = getOnsisHelper().getOnsisDateFormat().format(lastActiveDate);
        assertContent(enrollmentElem, "ENROLMENT_END_DATE", endDateStr);
    }

    /**
     * Test that EnrolmentEndDate is null for a W record in the future (beyond Report End Date)
     *
     * @throws Exception
     */
    @Test
    public void OnsisStudentSchoolEnrollmentTest_FutureEndDate() throws Exception {
        /*
         * Set up enrollment:
         * E == first school in-session day e.g. 9/3/2019
         * W == well AFTER report period end date
         *
         * Expect:
         * Exported EnrolmentEndDate should empty
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        String oen = generateCleanOEN();
        SisStudent student = createStudentWithProperties(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8",
                ALIAS_STD_OEN, oen);

        /*
         * StudentEnrollment
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_OCTSEC1;
        SubmissionType submissionType = getSubmissionType(submissionTypeCode);

        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate entryDate = findFirstInSessionDate(schoolCalendar, submissionType.getPeriodStartDate(), forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDate, enrollmentCode,
                getActiveStatusCode(), updateStudent);

        PlainDate withdrawDate = DateUtils.add(submissionType.getPeriodEndDate(), 5);

        // StudentEnrollment withdrawal =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawDate, "Withdrawn",
                "Inactive", updateStudent);

        /*
         * Define CSV import files
         * Add Enrolment to CSV
         */
        String enrolmentsCsv = createEnrolmentCsv(submissionTypeCode, DEFAULT_ACADEMIC_YEAR, oen, entryDate,
                DEFAULT_SKL_SECONDARY_BSID, school.getName());

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = student.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        /*
         * Perform the export - without enrolment in CSV
         */
        String nullStaffOid = null;
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Exported EnrolmentEndDate NOT present
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_SECONDARY_BSID + "']";
        String pathToStudent = "STUDENT[OEN='" + oen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrollmentElem = assertElement(studentElem, pathToEnrolment);

        assertNoElement(enrollmentElem, "ENROLMENT_END_DATE");
    }

    /**
     * In the June submission, where the last in-session date is before report end date,
     * an unterminated span should not export an end date.
     *
     * And if a span terminates beyond the report end date, it should not export an end date.
     *
     * If a span terminates within the report period,
     * it should export the W-1 date backed to last in-session date.
     *
     * @throws Exception
     */
    @Test
    public void OnsisStudentSchoolEnrollmentTest_JuneEndDate() throws Exception {
        /*
         * Set up enrollment:
         * E == first school in-session day e.g. 9/3/2019
         * W == well AFTER report period end date
         *
         * Expect:
         * Exported EnrolmentEndDate should empty
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_SECONDARY_SCHOOL_OID);
        String lastName = "UtxStudent";
        String firstName = "Barry";
        String oen = generateCleanOEN();
        SisStudent student = createStudentWithProperties(lastName, firstName, DEFAULT_SKL_SECONDARY_SCHOOL_OID, "8",
                ALIAS_STD_OEN, oen);

        /*
         * StudentEnrollment
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_JUNELEM3;
        SubmissionType submissionType = getSubmissionType(submissionTypeCode);

        SchoolCalendar schoolCalendar =
                findSchoolCalendar(getContext().getOid(), school.getOid(), DEFAULT_ONSIS_CALENDAR_ID);
        boolean forward = true;
        PlainDate entryDate = findFirstInSessionDate(schoolCalendar, submissionType.getPeriodStartDate(), forward);

        boolean updateStudent = true;
        String enrollmentCode = "Enrolled";
        // StudentEnrollment entry =
        createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, ENTRY, entryDate, enrollmentCode,
                getActiveStatusCode(), updateStudent);

        /*
         * Withdraw on the report end date.
         * Expect: export span end date backed to last in-session date.
         */
        PlainDate lastSchoolInSessionDate = findFirstInSessionDate(schoolCalendar, getContext().getEndDate(), false);
        assertTrue("setup", lastSchoolInSessionDate.before(submissionType.getPeriodEndDate()));

        PlainDate withdrawDate = submissionType.getPeriodEndDate();

        StudentEnrollment withdrawal =
                createEnrollment(student, DEFAULT_SKL_SECONDARY_SCHOOL_OID, WITHDRAWAL, withdrawDate, "Withdrawn",
                        "Inactive", updateStudent);

        /*
         * Define CSV import files
         * Add Enrolment to CSV
         */
        String enrolmentsCsv = createEnrolmentCsv(submissionTypeCode, DEFAULT_ACADEMIC_YEAR, oen, entryDate,
                DEFAULT_SKL_SECONDARY_BSID, school.getName());

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = student.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT;

        /*
         * Perform the export - without enrolment in CSV
         */
        String nullStaffOid = null;
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Exported EnrolmentEndDate is school last in-session date
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + DEFAULT_SKL_SECONDARY_BSID + "']";
        String pathToStudent = "STUDENT[OEN='" + oen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrollmentElem = assertElement(studentElem, pathToEnrolment);

        String endDateStr = getOnsisHelper().getOnsisDateFormat().format(lastSchoolInSessionDate);
        assertContent(enrollmentElem, "ENROLMENT_END_DATE", endDateStr);



        /**
         * Move the Withdraw beyond the report end date.
         * Expect: export span end date is null (NOT backed up to last in-session date)
         */
        withdrawDate = DateUtils.add(submissionType.getPeriodEndDate(), 4);
        withdrawal.setEnrollmentDate(withdrawDate);
        saveDirtyBean(withdrawal);

        /*
         * Perform the export
         */
        xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                nullStaffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Exported EnrolmentEndDate is school last in-session date
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, pathToData);
        schoolElem = assertElement(dataElem, pathToSchool);
        studentElem = assertElement(schoolElem, pathToStudent);
        enrollmentElem = assertElement(studentElem, pathToEnrolment);

        assertNoElement(enrollmentElem, "ENROLMENT_END_DATE");
    }
}
