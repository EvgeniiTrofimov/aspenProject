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

import static com.x2dev.procedures.statereporting.on.OnsisAssignedSubject.OnsisAssignedSubjectEntity.DEFAULT_GRADE_FLAG;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_CRS_COURSE_CODE_TYPE;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_CRS_MINISTRY_COURSE_CODE;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_MST_CLASS_TYPE;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.ALIAS_MST_LANGUAGE_OF_INSTRUCTION;
import static com.x2dev.procedures.statereporting.on.OnsisStateReportData.ALIAS_MTC_REPORT_SPECIALTY_SUBJECT;
import static com.x2dev.procedures.statereporting.on.OnsisConstants.COURSE_CODE_TYPE_HOMEROOM;
import com.x2dev.procedures.statereporting.on.OnsisStateReportData;
import com.x2dev.sis.model.beans.Course;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnsisSchoolEducatorAssignmentTest extends OnsisBaseTest {

    /*
     * Tests to write:
     * - Toggle date ranges on MTC
     */

    /**
     * Test business rules for <CLASS_ASSIGNMENT>
     *
     * @throws Exception
     */
    @Test
    public void testSchoolEducatorAssignment_ClassAssignment_Elem() throws Exception {
        testSchoolEducatorAssignment_ClassAssignment(SUBMISSION_TYPE_CODE_OCTELEM2);
    }

    @Test
    public void testSchoolEducatorAssignment_ClassAssignment_Secondary() throws Exception {
        testSchoolEducatorAssignment_ClassAssignment(SUBMISSION_TYPE_CODE_OCTSEC1);
    }

    /**
     *
     * @param submissionTypeCode
     * @throws Exception
     */
    public void testSchoolEducatorAssignment_ClassAssignment(String submissionTypeCode) throws Exception {
        /*
         * StaffPosition:
         * - isTeacher true (teachingTypeCode = B or T)
         *
         * 1) If NOT Early Childhood Educator (SFP JobCode != "ECE" or "LCE") --> IsPrimary/Homeroom
         * must be true
         * 2) OR IF ECE: getClassAssignments
         *
         * Plus Filter out “NE” (External Educator) ClassType as state code from:
         * 1. ALIAS_MST_CLASS_TYPE
         * 2. ALIAS_CRS_CLASS_TYPE
         * 3. DCC if ALIAS_CRS_COURSE_CODE_TYPE==DCC
         *
         * Section must have at least one student enrolled in it for a <CLASS_ASSIGNMENT> to export.
         * Student must also be enrolled in the school
         */
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, getDefaultSchoolOid(submissionTypeCode));
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);
        String classCode = setupInfo.course.getNumber() + "-" + setupInfo.section.getSectionNumber();
        String elemSubjTypeCode1 = CODE_ELEM_SUBJECT_TYPE_HEALTH_EDUC;

        if (!isElem(submissionTypeCode)) {
            /*
             * Set Course values for Secondary export
             */
            Course course1 = setupInfo.course;
            course1.setFieldValueByAlias(ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE, elemSubjTypeCode1);
            course1.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, "MDC");
            course1.setFieldValueByAlias(ALIAS_CRS_MINISTRY_COURSE_CODE, "X");
            saveDirtyBean(course1);
        }

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo.student.getOid();
        String staffOid = setupInfo.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_SCHOOL_EDUCATOR_ASSIGNMENT;

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
         * Expect CLASS_ASSIGNMENT
         */
        String pathToData = "/ONSIS_BATCH_FILE/DATA";
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo.schoolBSID + "']";
        String pathToSea = "SCHOOL_EDUCATOR_ASSIGNMENT[MEN='" + setupInfo.staffMen + "']";
        String pathClassAssignment = "CLASS_ASSIGNMENT[CLASS_CODE='" + classCode + "']";

        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, pathToData);
        Element schoolElem = assertElement(dataElem, pathToSchool);

        System.out.println(domToXml(schoolElem));

        Element sea = assertElement(schoolElem, pathToSea);
        assertElement(sea, pathClassAssignment);

        /*
         * Set ClassType to "NE (External Educator)
         */
        setupInfo.section.setFieldValueByAlias(ALIAS_MST_CLASS_TYPE, CODE_CLASS_TYPE_EXTERNAL_EDUCATOR);
        saveDirtyBean(setupInfo.section);

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
         * Expect CLASS_ASSIGNMENT
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, pathToData);
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertNoElement(sea, pathClassAssignment);
    }

    /**
     * Test that a single <SCHOOL_EDUCATOR_ASSIGNMENT> can contain
     * both CLASS_AASSIGNMENT and ASSIGNED_SUBJECT
     *
     * @throws Exception
     */
    @Test
    public void testSchoolEducatorAssignment_BothClassAndSubject() throws Exception {
        /*
         * Setup Staff/Course/Section/ScheduleTeacher
         *
         * StaffPosition:
         * - isTeacher true (teachingTypeCode = B or T)
         * - Not Early Childhood Educator (SFP JobCode != "ECE" or "LCE")
         *
         * Course:
         * - Master type "Class"
         * - Course delivery type "Day School" (Day)
         * - Course Code Type "Homeroom"
         *
         * Section
         * - Primary Staff: this SisStaff
         * - Schedule Term -> Code "FY"
         * - Language of instruction not empty (for ASSIGNED_SUBJECT)
         * (Note, Renfrew ignores LangOfInstr on Course bean)
         *
         * ScheduleTeacher
         * - Is Primary Teacher = true
         * - Report specialty subject 1 "true" (for ASSIGNED_SUBJECT)
         *
         * Section must have at least one student enrolled in it for a <CLASS_ASSIGNMENT> to export.
         * Student must also be enrolled in the school
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARELEM2;
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, getDefaultSchoolOid(submissionTypeCode));
        boolean isPrimary = true;
        SetupInfo setupInfoClass = basicSetup(school, null, isPrimary, null);

        /*
         * Setup ScheduleTeacher for ASSIGNED_SUBJECT using same Staff & Student
         */
        SetupInfo setupInfoSubject = basicSetup(school, setupInfoClass.staff, isPrimary, setupInfoClass.student);

        String elemSubjTypeCode = CODE_ELEM_SUBJECT_TYPE_HEALTH_EDUC;
        String elemSubjTypeOnsis = ONSIS_ELEM_SUBJECT_TYPE_HEALTH_EDUC;
        Course courseForSubj = setupInfoSubject.course;
        courseForSubj.setFieldValueByAlias(ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE, elemSubjTypeCode);
        courseForSubj.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, COURSE_CODE_TYPE_HOMEROOM);
        saveDirtyBean(courseForSubj);

        String langTypeCode = CODE_LANG_OF_INSTR_BOTH;
        String langTypeOnsis = ONSIS_LANG_OF_INSTR_BOTH;
        MasterSchedule sectionForSubj = setupInfoSubject.section;
        sectionForSubj.setFieldValueByAlias(ALIAS_MST_LANGUAGE_OF_INSTRUCTION, langTypeCode);
        saveDirtyBean(sectionForSubj);

        ScheduleTeacher mtcForSubj = setupInfoSubject.scheduleTeacher;
        mtcForSubj.setFieldValueByAlias(ALIAS_MTC_REPORT_SPECIALTY_SUBJECT, BooleanAsStringConverter.TRUE);
        saveDirtyBean(mtcForSubj);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define expected output
         */
        String positionType = DEFAULT_SFP_POSITION_TYPE;
        String classCode = setupInfoClass.course.getNumber() + "-" + setupInfoClass.section.getSectionNumber();
        String crsGradeLevelOnsis = DEFAULT_CRS_GRADE_LEVEL_ONSIS;
        String expectedSeaContent =
                "<SCHOOL_EDUCATOR_ASSIGNMENT>" +
                        "  <ACTION>ADD</ACTION>" +
                        "  <MEN>" + setupInfoClass.staffMen + "</MEN>" +
                        "  <POSITION_TYPE>" + positionType + "</POSITION_TYPE>" +
                        "  <CLASS_ASSIGNMENT>" +
                        "    <ACTION>ADD</ACTION>" +
                        "    <CLASS_CODE>" + classCode + "</CLASS_CODE>" +
                        "  </CLASS_ASSIGNMENT>" +
                        "  <ASSIGNED_SUBJECT>\n" +
                        "    <ACTION>ADD</ACTION>\n" +
                        "    <ELEMENTARY_SUBJECT_TYPE>" + elemSubjTypeOnsis + "</ELEMENTARY_SUBJECT_TYPE>\n" +
                        "    <LANGUAGE_TYPE>" + langTypeOnsis + "</LANGUAGE_TYPE>\n" +
                        "    <GRADE_FLAG>G</GRADE_FLAG>\n" +
                        "    <NUMBER_OF_CLASSES>1</NUMBER_OF_CLASSES>\n" +
                        "    <ASSIGNED_GRADE>\n" +
                        "      <ACTION>ADD</ACTION>\n" +
                        "      <GRADE_TYPE>" + crsGradeLevelOnsis + "</GRADE_TYPE>\n" +
                        "    </ASSIGNED_GRADE>\n" +
                        "  </ASSIGNED_SUBJECT>" +
                        "</SCHOOL_EDUCATOR_ASSIGNMENT>";

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfoClass.student.getOid();
        String staffOid = setupInfoClass.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_SCHOOL_EDUCATOR_ASSIGNMENT;

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
         */
        Document document = xmlToDom(xmlOutputStr);

        // <DATA>
        Element dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        assertContent(dataElem, "SCHOOL_SUBMISSION/SUBMISSION_PERIOD_TYPE", submissionTypeCode);

        // <SCHOOL>
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfoClass.schoolBSID + "']";
        Element schoolElem = assertElement(dataElem, pathToSchool);
        assertContent(schoolElem, "CLEAR_PENDING_AREA", "Y");

        // <SCHOOL_EDUCATOR_ASSIGNMENT>
        String pathToSea = "SCHOOL_EDUCATOR_ASSIGNMENT[MEN='" + setupInfoClass.staffMen + "']";
        Element sea = assertElement(schoolElem, pathToSea);
        // System.out.println(domToXml(sea));

        /*
         * Approach 1: Check example XML above is found as a subset
         */
        assertSubset(expectedSeaContent, sea);

        /*
         * Approach 2: Verify individual asserts on paths and content
         */
        assertContent(sea, "ACTION", "ADD");
        assertContent(sea, "MEN", setupInfoClass.staffMen);
        assertContent(sea, "POSITION_TYPE", positionType);
        assertContent(sea, "CORE_FLAG", "T");

        // <CLASS_ASSIGNMENT>
        String pathClassAssignment = "CLASS_ASSIGNMENT[CLASS_CODE='" + classCode + "']";
        assertElement(sea, pathClassAssignment);

        // <ASSIGNED_SUBJECT>
        String pathExpectedSubject1 = "ASSIGNED_SUBJECT[ELEMENTARY_SUBJECT_TYPE='" + elemSubjTypeOnsis
                + "' and LANGUAGE_TYPE='" + langTypeOnsis
                + "' and GRADE_FLAG='" + DEFAULT_GRADE_FLAG + "']";
        Element assignedSubject = assertElement(sea, pathExpectedSubject1);

        String pathExpectedGrade1 = "ASSIGNED_GRADE[GRADE_TYPE='" + crsGradeLevelOnsis + "']";
        assertElement(assignedSubject, pathExpectedGrade1);


        /*
         * Test that a Core (Homeroom) teacher does not report ASSIGNED_SUBJECTs
         * if the MTC isn't flagged as Specialty Subject.
         *
         * Turn off [all-mtc-ReportSpecialtySubject]
         * --> AsignedSubject should no longer export
         */
        mtcForSubj.setFieldValueByAlias(ALIAS_MTC_REPORT_SPECIALTY_SUBJECT, null);
        saveDirtyBean(mtcForSubj);

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
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertNoElement(sea, "ASSIGNED_SUBJECT");
    }

    /**
     * Business rule:
     * If this is a core teacher i.e. isPrimary on at least one section
     * (which for Elementary must also have CourseCodeType "Homeroom")
     * only publish ScheduleTeacher with [all-mtc-ReportSpecialtySubject]
     *
     * ELSE if teacher doesn't have any Homeroom sections on which they are Primary,
     * then export all their ScheduleTeacher records
     * that qualify.
     *
     * Initially set Course1 as Homeroom / isPrimary
     * and Course2 not Homeroom,
     * --> expect just the Homeroom subject to publish.
     *
     * Then remove Homeroom from Course1
     * --> expect both subjects to publish.
     *
     * @throws Exception
     */
    @Test
    public void testSchoolEducatorAssignment_AllSubjectsIfNoPrimaryHR() throws Exception {
        /*
         * Setup two Course/Section/MTC:
         * - First set as Homeroom
         * - Second not Homeroom
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARELEM2;
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, getDefaultSchoolOid(submissionTypeCode));
        boolean isPrimary = true;
        SetupInfo setupInfo1 = basicSetup(school, null, isPrimary, null);
        SetupInfo setupInfo2 = basicSetup(school, setupInfo1.staff, isPrimary, setupInfo1.student);

        String elemSubjTypeCode1 = CODE_ELEM_SUBJECT_TYPE_HEALTH_EDUC;
        String elemSubjTypeOnsis1 = ONSIS_ELEM_SUBJECT_TYPE_HEALTH_EDUC;
        String elemSubjTypeCode2 = CODE_ELEM_SUBJECT_TYPE_PHYSICAL_EDUC;
        String elemSubjTypeOnsis2 = ONSIS_ELEM_SUBJECT_TYPE_PHYSICAL_EDUC;
        String langTypeOnsis = DEFAULT_MST_LANG_OF_INSTRUCTION_ONSIS;

        Course course1 = setupInfo1.course;
        Course course2 = setupInfo2.course;
        course1.setFieldValueByAlias(ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE, elemSubjTypeCode1);
        course2.setFieldValueByAlias(ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE, elemSubjTypeCode2);

        // Only Course1 should remain as Homeroom CourseCodeType
        course1.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, COURSE_CODE_TYPE_HOMEROOM);
        course2.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, null);

        saveDirtyBean(course1);
        saveDirtyBean(course2);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo1.student.getOid();
        String staffOid = setupInfo1.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_SCHOOL_EDUCATOR_ASSIGNMENT;

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
         */
        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo1.schoolBSID + "']";
        Element schoolElem = assertElement(dataElem, pathToSchool);
        String pathToSea = "SCHOOL_EDUCATOR_ASSIGNMENT[MEN='" + setupInfo1.staffMen + "']";
        Element sea = assertElement(schoolElem, pathToSea);
        // System.out.println(domToXml(sea));

        /*
         * Verify individual asserts on paths and content
         * Expect no <ASSIGNED_SUBJECT>
         * Because if this is a core (Homeroom) teacher,
         * only publish ScheduleTeacher with [all-mtc-ReportSpecialtySubject]
         */
        String pathExpectedSubject1 = "ASSIGNED_SUBJECT[ELEMENTARY_SUBJECT_TYPE='" + elemSubjTypeOnsis1
                + "' and LANGUAGE_TYPE='" + langTypeOnsis
                + "' and GRADE_FLAG='" + DEFAULT_GRADE_FLAG + "']";
        String pathExpectedSubject2 = "ASSIGNED_SUBJECT[ELEMENTARY_SUBJECT_TYPE='" + elemSubjTypeOnsis2
                + "' and LANGUAGE_TYPE='" + langTypeOnsis
                + "' and GRADE_FLAG='" + DEFAULT_GRADE_FLAG + "']";
        assertNoElement(sea, pathExpectedSubject1);
        assertNoElement(sea, pathExpectedSubject2);

        /*
         * Set Specialty Subject on ScheduleTeacher 2
         * Expect subject2 to publish
         */
        ScheduleTeacher scheduleTeacher2 = setupInfo2.scheduleTeacher;
        scheduleTeacher2.setFieldValueByAlias(ALIAS_MTC_REPORT_SPECIALTY_SUBJECT, BooleanAsStringConverter.TRUE);
        saveDirtyBean(scheduleTeacher2);

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
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo1.schoolBSID + "']";
        schoolElem = assertElement(dataElem, pathToSchool);
        pathToSea = "SCHOOL_EDUCATOR_ASSIGNMENT[MEN='" + setupInfo1.staffMen + "']";
        sea = assertElement(schoolElem, pathToSea);
        // System.out.println(domToXml(sea));

        assertNoElement(sea, pathExpectedSubject1);
        assertElement(sea, pathExpectedSubject2);

        /*
         * Reset Specialty Subject on ScheduleTeacher 2.
         * Remove Homeroom from Course1
         * --> expect both subjects to publish.
         */
        scheduleTeacher2.setFieldValueByAlias(ALIAS_MTC_REPORT_SPECIALTY_SUBJECT, null);
        saveDirtyBean(scheduleTeacher2);
        course1.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, null);
        saveDirtyBean(course1);

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
         * Expect both subjects
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        // System.out.println(domToXml(sea));

        assertElement(sea, pathExpectedSubject1);
        assertElement(sea, pathExpectedSubject2);
    }

    /**
     * Test that a Secondary export doesn't require CourseCodeType=Homeroom
     *
     *
     * @throws Exception
     */
    @Test
    public void testSchoolEducatorAssignment_SecondaryDoesntRequireHoomroom() throws Exception {
        /*
         * Setup a Course/Section/MTC NOT as Homeroom
         *
         * For a secondary export:
         * - all-crs-MinistryCourseCode must not be empty
         * - all-crs-CourseCodeType state code must be LDC, MDC or DCC
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARSEC1;
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, getDefaultSchoolOid(submissionTypeCode));
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);

        String elemSubjTypeCode1 = CODE_ELEM_SUBJECT_TYPE_HEALTH_EDUC;
        String elemSubjTypeOnsis1 = ONSIS_ELEM_SUBJECT_TYPE_HEALTH_EDUC;
        String langTypeOnsis = DEFAULT_MST_LANG_OF_INSTRUCTION_ONSIS;

        /*
         * Set Course values for Secondary export
         */
        Course course1 = setupInfo.course;
        course1.setFieldValueByAlias(ALIAS_CRS_ELEMENTARY_SUBJECT_TYPE, elemSubjTypeCode1);
        course1.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, "MDC");
        course1.setFieldValueByAlias(ALIAS_CRS_MINISTRY_COURSE_CODE, "X");
        saveDirtyBean(course1);

        /*
         * Because this teacher isPrimary, need to set Specialty Subject = true to export a Subject.
         * Note, Homeroom not required b/c export is Secondary)
         */
        ScheduleTeacher scheduleTeacher = setupInfo.scheduleTeacher;
        scheduleTeacher.setFieldValueByAlias(ALIAS_MTC_REPORT_SPECIALTY_SUBJECT, BooleanAsStringConverter.TRUE);
        saveDirtyBean(scheduleTeacher);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo.student.getOid();
        String staffOid = setupInfo.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_SCHOOL_EDUCATOR_ASSIGNMENT;

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
         */
        Document document = xmlToDom(xmlOutputStr);
        Element dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo.schoolBSID + "']";
        Element schoolElem = assertElement(dataElem, pathToSchool);
        String pathToSea = "SCHOOL_EDUCATOR_ASSIGNMENT[MEN='" + setupInfo.staffMen + "']";
        Element sea = assertElement(schoolElem, pathToSea);
        // System.out.println(domToXml(sea));

        /*
         * Verify individual asserts on paths and content
         * Expect no <ASSIGNED_SUBJECT>
         * Because if this is a core (Homeroom) teacher,
         * only publish ScheduleTeacher with [all-mtc-ReportSpecialtySubject]
         */
        String pathExpectedSubject1 = "ASSIGNED_SUBJECT[ELEMENTARY_SUBJECT_TYPE='" + elemSubjTypeOnsis1
                + "' and LANGUAGE_TYPE='" + langTypeOnsis
                + "' and GRADE_FLAG='" + DEFAULT_GRADE_FLAG + "']";
        assertElement(sea, pathExpectedSubject1);
    }

    /**
     * Test Core Teacher Flag
     *
     * @throws Exception
     */
    @Test
    public void testSchoolEducatorAssignment_CoreTeacherFlag() throws Exception {
        /*
         * Setup Staff/Course/Section/ScheduleTeacher
         *
         * StaffPosition:
         * - Must be isTeacher (teachingTypeCode = B or T)
         * - Must not be Early Childhood Educator (SFP JobCode != "ECE" or "LCE")
         * - SFP must be in date
         * - Must have at least one section of interest for which teacher isPrimary
         * (filtered by CourseCodeType=Homeroom for elementary, else all sections)
         *
         */
        String submissionTypeCode = SUBMISSION_TYPE_CODE_MARELEM2;
        SisSchool school = getBroker().getBeanByOid(SisSchool.class, getDefaultSchoolOid(submissionTypeCode));
        boolean isPrimary = true;
        SetupInfo setupInfo = basicSetup(school, null, isPrimary, null);

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String enrolmentsCsv = createDummyCsv(submissionTypeCode);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(school.getOid());
        String studentOid = setupInfo.student.getOid();
        String staffOid = setupInfo.staff.getOid();
        String exportTags = XML_TAGS_SCHOOL_ROOT
                + XML_TAGS_SCHOOL_EDUCATOR_ASSIGNMENT;

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
         */
        Document document = xmlToDom(xmlOutputStr);

        // <DATA>
        Element dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        assertContent(dataElem, "SCHOOL_SUBMISSION/SUBMISSION_PERIOD_TYPE", submissionTypeCode);

        // <SCHOOL>
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + setupInfo.schoolBSID + "']";
        Element schoolElem = assertElement(dataElem, pathToSchool);
        assertContent(schoolElem, "CLEAR_PENDING_AREA", "Y");

        // <SCHOOL_EDUCATOR_ASSIGNMENT>
        String pathToSea = "SCHOOL_EDUCATOR_ASSIGNMENT[MEN='" + setupInfo.staffMen + "']";
        Element sea = assertElement(schoolElem, pathToSea);
        // System.out.println(domToXml(sea));

        /*
         * Verify individual asserts on paths and content
         */
        assertContent(sea, "ACTION", "ADD");
        assertContent(sea, "MEN", setupInfo.staffMen);
        assertContent(sea, "CORE_FLAG", "T");


        /*
         * Remove Homeroom CourseCodeType
         */
        Course courseForSubj = setupInfo.course;
        String oldCourseCodeType = (String) courseForSubj.getFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE);
        courseForSubj.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, null);
        saveDirtyBean(courseForSubj);

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
         * Check the output XML:
         *
         * Verify <CORE_FLAG> is F
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertContent(sea, "CORE_FLAG", "F");

        /*
         * Restore Homeroom CourseCodeType
         */
        courseForSubj.setFieldValueByAlias(ALIAS_CRS_COURSE_CODE_TYPE, oldCourseCodeType);
        saveDirtyBean(courseForSubj);

        /*
         * Set SFP JobCode to ECE
         */
        StaffPosition sfp = setupInfo.staffPosition;

        String oldJobCode = sfp.getJobCode();
        sfp.setJobCode("ECE");
        saveDirtyBean(sfp);

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
         * Check the output XML:
         *
         * Verify <CORE_FLAG> is F
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertContent(sea, "CORE_FLAG", "F");

        /*
         * Restore SFP JobCode.
         * Change TeachingTypeCode so isTeacher = FALSE (not B or T)
         */
        String oldTeachingTypeCode =
                (String) sfp.getFieldValueByAlias(OnsisStateReportData.StaffHelper.ALIAS_SFP_TEACHING_TYPE);
        setupInfo.staffPosition.setJobCode(oldJobCode);
        saveDirtyBean(setupInfo.staffPosition);

        /*
         * Check the output XML:
         *
         * Verify <CORE_FLAG> is F
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertContent(sea, "CORE_FLAG", "F");

        /*
         * Restore SFP TeachingTypeCode.
         */
        sfp.setFieldValueByAlias(OnsisStateReportData.StaffHelper.ALIAS_SFP_TEACHING_TYPE, oldTeachingTypeCode);
        saveDirtyBean(setupInfo.staffPosition);

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
         * Check the output XML:
         *
         * Verify <CORE_FLAG> is back to T
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertContent(sea, "CORE_FLAG", "T");

        /*
         * Change MTC so IsPrimary is false
         */
        setupInfo.section.setPrimaryStaffOid(null);
        getBroker().saveBeanForced(setupInfo.section);

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
         * Check the output XML:
         *
         * Verify <CORE_FLAG> is F
         */
        document = xmlToDom(xmlOutputStr);
        dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        schoolElem = assertElement(dataElem, pathToSchool);
        sea = assertElement(schoolElem, pathToSea);
        assertContent(sea, "CORE_FLAG", "F");
    }
}
