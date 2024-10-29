/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2011 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.model.business.health.immunizations;

import org.junit.Test;

/**
 * Unit tests for the max/min DOB attributes on immunization compliance rule sets.
 *
 * @author mmastrangelo
 */
public class ImmunizationRuleRulesetMaxMinDobTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/sis/model/business/health/immunizations/Immunizationrule-RulesetMaxMinDob.xml");


        /*
         * <compliance-rule>
         * <rule-set min-dob="2005-01-01">
         * <dose-total count="2"/>
         * </rule-set>
         * <rule-set min-dob="2010-05-15" max-dob="2012-05-15">
         * <dose-total count="4"/>
         * </rule-set>
         * </compliance-rule>
         */
    }

    /**
     * Test min dob.
     */
    @Test
    public void testMinDob() {
        setup("2014-07-26");

        // Student with DOB 2007-07-01 is in 1st grade based on test assumptions
        setStudentDOBandDosesString("2004-12-31, 2006-07-15");
        evaluateStudentFromMStudentInformation(true); // Should pass; no rule covers DOB <
        // 2005-01-01

        setStudentDOBandDosesString("2005-01-01, 2006-07-15");
        evaluateStudentFromMStudentInformation(false); // Should fail; not enough doses

        setStudentDOBandDosesString("2005-01-01, 2006-07-15, 2006-08-11");
        evaluateStudentFromMStudentInformation(true); // Should pass
    }

    /**
     * Test min max dob.
     */
    @Test
    public void testMinMaxDob() {
        setup("2014-07-26");

        setStudentDOBandDosesString("2010-05-14, 2010-07-15, 2010-08-11");
        evaluateStudentFromMStudentInformation(true); // Should pass

        setStudentDOBandDosesString("2010-05-15, 2010-07-15, 2010-08-11");
        evaluateStudentFromMStudentInformation(false); // Should fail, not enuogh doses

        setStudentDOBandDosesString("2010-05-15, 2010-07-15, 2010-08-11, 2011-07-15, 2011-08-11");
        evaluateStudentFromMStudentInformation(true); // Should pass

        setStudentDOBandDosesString("2012-05-16, 2010-07-15, 2010-08-11");
        evaluateStudentFromMStudentInformation(true); // Should pass; beyond max dob for 4 dose rule
    }
}
