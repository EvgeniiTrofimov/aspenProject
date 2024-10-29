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
package com.x2dev.sis.model.business.health.immunizations;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.x2dev.sis.model.business.health.RuleSet;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.types.PlainDate;
import java.io.InputStream;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import org.junit.Test;

/**
 * The Class ImmunizationRuleTestMasterTest.
 */
public class ImmunizationRuleTestMasterTest extends ImmunizationRuleTestMaster {

    public void setup() {
        this.setup(null);
    }

    public void setup(String today) {
        super.setup(today, "/com/x2dev/sis/model/business/health/immunizations/TestRule.xml");
    }

    /**
     * Tests for ImmunizationRuleTestMaster.
     */

    @Test
    public void testSetupPopulatesXML() {
        assertNull(m_xmlStream);
        setup();
        assertNotNull(m_xmlStream);
    }

    /**
     * Test parse rule.
     */
    @Test
    public void testParseRule() {
        setup();

        InputStream xml = null;
        RuleSet rule;

        // A null xml file should return an error
        try {
            rule = parseRule(xml);
            fail("Should have errored out!");
        } catch (AssertionError e) {
            // This is what we want
        }

        // A bad xml file should return null
        try {
            InputStream badXmlFile = ImmunizationRuleTestMasterTest.class.getResourceAsStream(
                    "/com/x2dev/sis/model/business/health/immunizations/ImmunizationRuleTestMaster.java");
            rule = parseRule(badXmlFile);
            fail("Should have errored out!");
        } catch (AssertionError e) {
            // This is what we want
        }

        // Should get something back in the event we have a good xml file
        xml = ImmunizationRuleTestMasterTest.class
                .getResourceAsStream("/com/x2dev/sis/model/business/health/immunizations/TestRule.xml");
        rule = parseRule(xml);
        assertNotNull(rule);
    }

    /**
     * Test parse string into student and doses.
     */
    @Test
    public void testParseStringIntoStudentAndDoses() {
        // DOB only
        setStudentDOBandDosesString("1983-11-28");
        assertNull(getDoses());
        parseStudentAndDoses();
        assertNotNull(getDoses());
        assertEquals(0, getDoses().size());
        // DOB and two doses
        setStudentDOBandDosesString("1983-11-28, 1983-12-01, 1984-01-02");
        ArrayList<String> testString =
                StringUtils.convertDelimitedStringToList("1983-11-28, 1983-12-01, 1984-01-02", ',');
        parseStudentAndDoses();
        assertNotNull(getDoses());
        assertEquals(0, m_student.getPerson().getDob().compareTo(new PlainDate(Date.valueOf(testString.get(0)))));
        assertEquals(2, getDoses().size());
        assertEquals(0, getDoses().get(0).getDate().compareTo(new PlainDate(Date.valueOf(testString.get(1).trim()))));
        assertEquals(0, getDoses().get(1).getDate().compareTo(new PlainDate(Date.valueOf(testString.get(2).trim()))));
    }

    /**
     * Test grade level data for default student.
     */
    @Test
    public void testGradeLevelDataForDefaultStudent() {
        setup();
        setStudentDOBandDosesString("1983-11-28, 1983-12-01, 1984-01-02");
        parseStudentAndDoses();
        Map<Integer, String> historyForStudent = m_studentHistory.getHistoryForStudent(m_student.getOid());

        assertEquals("-1", historyForStudent.get(Integer.valueOf(1988)));
        assertEquals("00", historyForStudent.get(Integer.valueOf(1989)));
        // Assume when student is 7 they are in 1st grade
        assertEquals("01", historyForStudent.get(Integer.valueOf(1990)));
        assertEquals("02", historyForStudent.get(Integer.valueOf(1991)));
        assertEquals("03", historyForStudent.get(Integer.valueOf(1992)));
        assertEquals("04", historyForStudent.get(Integer.valueOf(1993)));
        assertEquals("05", historyForStudent.get(Integer.valueOf(1994)));
        assertEquals("06", historyForStudent.get(Integer.valueOf(1995)));
        assertEquals("07", historyForStudent.get(Integer.valueOf(1996)));
        assertEquals("08", historyForStudent.get(Integer.valueOf(1997)));
        assertEquals("09", historyForStudent.get(Integer.valueOf(1998)));
        assertEquals("10", historyForStudent.get(Integer.valueOf(1999)));
        assertEquals("11", historyForStudent.get(Integer.valueOf(2000)));
        assertEquals("12", historyForStudent.get(Integer.valueOf(2001)));
    }

    /**
     * Test get school year array.
     */
    @Test
    public void testGetSchoolYearArray() {
        setup();
        setStudentDOBandDosesString("1983-11-28");
        parseStudentAndDoses();
        Collection<DistrictSchoolYearContext> schoolYears = m_studentHistory.getSchoolYears();
        assertNotNull(schoolYears);
        // TODO this *might* be ok returning one... need to write additional tests around code for
        // ImmunizationRule.java line 500
        assertEquals(1, schoolYears.size());
    }

    /**
     * Test get students grade level.
     */
    @Test
    public void testGetStudentsGradeLevel() {
        setup();
        setStudentDOBandDosesString("1983-11-28, 1983-12-01, 1984-01-02");
        parseStudentAndDoses();
        assertEquals(Integer.valueOf(12), m_studentHistory.getNumericGradeLevel("12"));
        assertEquals(Integer.valueOf(7), m_studentHistory.getNumericGradeLevel("7"));
    }

    /**
     * Test get students grade level pre K.
     */
    @Test
    public void testGetStudentsGradeLevelPreK() {
        setup("2013-11-05");
        setStudentDOBandDosesString("1983-11-28, 1983-12-01, 1984-01-02");
        parseStudentAndDoses();
        assertEquals(Integer.valueOf(12), m_studentHistory.getNumericGradeLevel("12"));
        assertEquals(Integer.valueOf(7), m_studentHistory.getNumericGradeLevel("7"));
        assertEquals(Integer.valueOf(-1), m_studentHistory.getNumericGradeLevel("PK"));
    }

    /**
     * Test get students school year date.
     */
    @Test
    public void testGetStudentsSchoolYearDate() {
        // Given a student who is older than 18 their last school year will be in the past
        setup("2012-11-01");
        setStudentDOBandDosesString("1983-11-28, 1983-12-01, 1984-01-02");
        parseStudentAndDoses();
        assertEquals(new PlainDate(Date.valueOf("2001-09-01")), m_schoolYear.getStartDate());
        assertEquals(new PlainDate(Date.valueOf("2002-06-01")), m_schoolYear.getEndDate());
        // given a student still in school their last school year will be the present
        setStudentDOBandDosesString("2000-11-28, 1983-12-01, 1984-01-02");
        parseStudentAndDoses();
        assertEquals(new PlainDate(Date.valueOf("2012-09-01")), m_schoolYear.getStartDate());
        assertEquals(new PlainDate(Date.valueOf("2013-06-01")), m_schoolYear.getEndDate());
    }

    /**
     * Test setup student based on grade level.
     */
    @Test
    public void testSetupStudentBasedOnGradeLevel() {
        setup("2013-11-01");
        parseIntIntoStudentAndDoses(7, new int[] {10, 20});
        assertEquals(Integer.valueOf(7), m_studentGradeLevel);
        // Two doses, one 10 days after their dob, and one 20 days after dose one
        assertEquals(new PlainDate(Date.valueOf("2000-11-11")), getDoses().get(0).getDate());
        assertEquals(new PlainDate(Date.valueOf("2000-12-01")), getDoses().get(1).getDate());
    }

    /**
     * Test setup student based on grade level month issue.
     */
    @Test
    public void testSetupStudentBasedOnGradeLevelMonthIssue() {
        setup("2013-01-01");
        parseIntIntoStudentAndDoses(7, new int[] {10, 20});
        assertEquals(Integer.valueOf(7), m_studentGradeLevel);
        assertEquals(new PlainDate(Date.valueOf("2000-01-11")), getDoses().get(0).getDate());
        assertEquals(new PlainDate(Date.valueOf("2000-01-31")), getDoses().get(1).getDate());
    }

    /**
     * Test student is setup properly with grade level.
     */
    @Test
    public void testStudentIsSetupProperlyWithGradeLevel() {
        setup("2013-11-05");
        parseIntIntoStudentAndDoses(7, new int[] {});
        assertEquals("7", m_testStudent.getGradeLevel());
    }
}
