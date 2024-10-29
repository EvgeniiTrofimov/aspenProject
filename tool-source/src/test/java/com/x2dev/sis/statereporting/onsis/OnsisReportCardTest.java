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

import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG;
import static com.x2dev.sis.model.beans.StudentEnrollment.WITHDRAWAL;
import com.x2dev.procedures.statereporting.on.OnsisHelpersContainer.SubmissionType;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.converters.BooleanAsStringConverter;
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
public class OnsisReportCardTest extends OnsisBaseTest {
    @Test
    public void OnsisReportCardTest_altReportCard() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);
        PlainDate entryDate = setupInfo.enrollment.getEnrollmentDate();
        String entryDateStr = getOnsisHelper().getOnsisDateFormat().format(entryDate);
        assertAlias(ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG);
        assertAlias(ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG);

        assertEquals(BooleanAsStringConverter.TRUE,
                lookupStateCodeByAlias("Yes", ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG));
        assertEquals(BooleanAsStringConverter.TRUE,
                lookupStateCodeByAlias("Yes", ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG));


        /*
         * ReportCard requires a Withdrawal during the submission period
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARELEM2;
        SubmissionType submissionType = getSubmissionType(submissionTypeCode);
        PlainDate withdrawalDate = DateUtils.add(submissionType.getPeriodEndDate(), -3);

        boolean updateStudent = true;
        StudentEnrollment withdrawal =
                createEnrollment(setupInfo.student, school.getOid(), WITHDRAWAL, withdrawalDate, updateStudent);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createEnrolmentCsv(submissionTypeCode, DEFAULT_ACADEMIC_YEAR,
                setupInfo.cleanOen, entryDate, DEFAULT_SKL_ELEM_BSID, school.getName());

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo.student.getOid();
        String staffOid = setupInfo.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_REPORT_CARD;

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
         * Expect no REPORT_CARD
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo.schoolBSID + "']";
        String pathToStudent = "STUDENT[OEN='" + setupInfo.cleanOen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT[ENROLMENT_START_DATE='" + entryDateStr + "']";
        String pathToReportCard = "REPORT_CARD";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrolmentElem = assertElement(studentElem, pathToEnrolment);

        assertNoElement(enrolmentElem, pathToReportCard);


        /*
         * Flag altReportCard on Student
         */
        setupInfo.student.setFieldValueByAlias(ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG, "Yes");
        saveDirtyBean(setupInfo.student);
        assertEquals(BooleanAsStringConverter.TRUE,
                lookupStateCodeByAlias(setupInfo.student, ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG));

        /*
         * Expected output
         */
        String currentGradeLevel = setupInfo.student.getGradeLevel();
        Integer nextGradeLevel = Integer.valueOf(currentGradeLevel) + 1;
        String expectedReportCard =
                "<REPORT_CARD>" +
                        "  <ACTION>ADD</ACTION>" +
                        "  <GRADE_IN_SEPTEMBER>" + nextGradeLevel + "</GRADE_IN_SEPTEMBER>" +
                        "  <TERM>" +
                        "    <ACTION>ADD</ACTION>" +
                        "    <TERM_CODE>1</TERM_CODE>" +
                        "    <DAYS_ABSENT>0.00</DAYS_ABSENT>" +
                        "    <TIMES_LATE>0</TIMES_LATE>" +
                        "    <ALTERNATIVE_REPORT_CARD_FLAG>T</ALTERNATIVE_REPORT_CARD_FLAG>" +
                        "  </TERM>" +
                        "</REPORT_CARD>";

        /*
         * Perform the export
         */
        xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                staffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Expect REPORT_CARD
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, pathToData);
        schoolElem = assertElement(dataElem, pathToSchool);
        studentElem = assertElement(schoolElem, pathToStudent);
        enrolmentElem = assertElement(studentElem, pathToEnrolment);

        assertHasChildSubset(expectedReportCard, enrolmentElem);


        /*
         * Clear altReportCard on Student
         * Flag altReportCard on W record
         */
        setupInfo.student.setFieldValueByAlias(ALIAS_STD_ELEM_ALT_REPORT_CARD_FLAG, null);
        saveDirtyBean(setupInfo.student);
        withdrawal.setFieldValueByAlias(ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG, "Yes");
        saveDirtyBean(withdrawal);
        assertEquals(BooleanAsStringConverter.TRUE,
                lookupStateCodeByAlias(withdrawal, ALIAS_ENR_ELEM_ALT_REPORT_CARD_FLAG));

        /*
         * Perform the export
         */
        xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                staffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         *
         * Expect REPORT_CARD
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, pathToData);
        schoolElem = assertElement(dataElem, pathToSchool);
        studentElem = assertElement(schoolElem, pathToStudent);
        enrolmentElem = assertElement(studentElem, pathToEnrolment);

        assertHasChildSubset(expectedReportCard, enrolmentElem);
    }

    @Test
    public void OnsisReportCardTest_ReportCard_ActionDelete() throws Exception {
        /*
         * Set up data
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, DEFAULT_SKL_ELEM_SCHOOL_OID);
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);
        PlainDate entryDate = setupInfo.enrollment.getEnrollmentDate();
        String entryDateStr = getOnsisHelper().getOnsisDateFormat().format(entryDate);

        /*
         * ReportCard requires a Withdrawal during the submission period
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARELEM2;

        /*
         * Define CSV import file to test Action=Delete
         */
        String reportCardCsv = new ReportCardCsvBuilder()
                .h1(DEFAULT_ACADEMIC_YEAR, headerForSubmission(submissionTypeCode))
                .h2(setupInfo.schoolBSID, school.getName())
                .dt(setupInfo.cleanOen)
                .build();

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo.student.getOid();
        String staffOid = setupInfo.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_STUDENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_ROOT
                + XML_TAGS_STUDENT_SCHOOL_ENROLMENT_REPORT_CARD;

        /*
         * Perform the export
         */
        String xmlOutputStr = performExport(submissionTypeCode,
                schoolOids,
                studentOid,
                staffOid,
                exportTags,
                reportCardCsv);

        /*
         * Check the output XML
         * Expect REPORT_CARD with ACTION=DELETE
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo.schoolBSID + "']";
        String pathToStudent = "STUDENT[OEN='" + setupInfo.cleanOen + "']";
        String pathToEnrolment = "STUDENT_SCHOOL_ENROLMENT[ENROLMENT_START_DATE='" + entryDateStr + "']";
        String pathToReportCard = "REPORT_CARD";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);
        Element studentElem = assertElement(schoolElem, pathToStudent);
        Element enrolmentElem = assertElement(studentElem, pathToEnrolment);
        Element reportCard = assertElement(enrolmentElem, pathToReportCard);
        assertContent(reportCard, "ACTION", "DELETE");
    }
}
