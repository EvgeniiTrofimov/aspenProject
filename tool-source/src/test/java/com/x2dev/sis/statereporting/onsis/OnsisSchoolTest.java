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

import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Follett Software Company
 * @copyright 2020
 */
public class OnsisSchoolTest extends OnsisBaseTest {

    @Test
    public void testSchoolRoot() throws Exception {
        /*
         * Set up data
         */
        String schoolId = DEFAULT_SKL_ELEM_BSID;

        /*
         * Define CSV import files
         * (requires one with academic year)
         */
        String submissionType = SUBMISSION_TYPE_CODE_MARELEM2;
        String enrolmentsCsv = createDummyCsv(submissionType);

        String expectedSchoolContent =
                "<SCHOOL>" +
                        "  <SCHOOL_NUMBER>" + schoolId + "</SCHOOL_NUMBER>" +
                        "  <CLEAR_PENDING_AREA>Y</CLEAR_PENDING_AREA>" +
                        "</SCHOOL>";
        Element expectedSchoolElem = xmlToElem(expectedSchoolContent);

        /*
         * Define export scope: school(s), student, and XML tags
         */
        List<String> schoolOids = Arrays.asList(DEFAULT_SKL_ELEM_SCHOOL_OID);
        String studentOid = null;
        String staffOid = null;
        String exportTags = XML_TAGS_SCHOOL_ROOT;

        /*
         * Perform the export
         */
        String xmlOutputStr = performExport(submissionType,
                schoolOids,
                studentOid,
                staffOid,
                exportTags,
                enrolmentsCsv);

        /*
         * Check the output XML
         */
        System.out.println(xmlOutputStr);
        Document document = xmlToDom(xmlOutputStr);

        // <DATA><SCHOOL_SUBMISSION>
        Element dataElem = assertElement(document, "/ONSIS_BATCH_FILE/DATA");
        assertContent(dataElem, "SCHOOL_SUBMISSION/SUBMISSION_PERIOD_TYPE", submissionType);
        Element submissionElem = assertElement(dataElem, "SCHOOL_SUBMISSION");

        /*
         * Approach 1: Check example XML above is found as a subset
         */
        assertHasChildSubset(expectedSchoolElem, submissionElem);

        /*
         * Approach 2: Verify individual asserts on paths and content
         */
        // <SCHOOL>
        String pathToSchool = "SCHOOL_SUBMISSION/SCHOOL[SCHOOL_NUMBER='" + schoolId + "']";
        Element actualSchoolElem = assertElement(dataElem, pathToSchool);
        System.out.println(domToXml(actualSchoolElem));
        assertContent(actualSchoolElem, "CLEAR_PENDING_AREA", "Y");
    }
}
