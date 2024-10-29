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

import org.junit.Test;


/**
 * The Class ImmunizationRuleNYHepBTest.
 */
public class ImmunizationRuleNYHepBTest extends ImmunizationRuleTestMaster {


    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/ny/Immunizationrule-hepb.xml");
    }

    private void setup() {
        this.setup(null);
    }

    /**
     * PreK k-12 Foot Note
     * State Req. 3 doses 3 doses* "Hepatitis B � For students in grades 7-12, 3 doses of
     * Recombivax HB or Engerix-B are required, except for those students who received 2 doses of
     * adult hepatitis
     * B vaccine (Recombivax) which are recommended for children 11-15 years old."
     *
     * CDC Req. Birth, 1-2 Months, 6-18 Months Catch up only Clear 3 doses, and handle case when
     * child is 11-15 and needs only 2 doses?
     */

    @Test
    public void testHepBFirstDose() {
        setup();
        setStudentDOBandDosesString("2008-11-02, 2008-11-15");
        evaluateStudentFromMStudentInformation(false); // Should not pass, three doses required
    }

    /**
     * Test hep B second dose.
     */
    @Test
    public void testHepBSecondDose() {
        setup();
        setStudentDOBandDosesString("2008-11-02, 2008-11-15, 2009-01-15");
        evaluateStudentFromMStudentInformation(false); // Should not pass, three doses required
    }

    /**
     * Test hep B third dose.
     */
    @Test
    public void testHepBThirdDose() {
        setup();
        setStudentDOBandDosesString("2008-11-02, 2008-11-15, 2009-01-15, 2010-01-15");
        evaluateStudentFromMStudentInformation(true); // Should pass, Three doses are all we require
    }
}
