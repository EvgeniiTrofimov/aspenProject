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
 * The Class ImmunizationRuleVATDAPTest.
 */
public class ImmunizationRuleVATDAPTest extends ImmunizationRuleTestMaster {
    private static final String TODAY = "2014-09-15";

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/va/Immunizationrule-tdap.xml");
    }

    /**
     * Test student got dosesin prek and elem.
     */
    @Test
    public void testStudentGotDosesinPrekAndElem() {
        setup(TODAY);
        parseDoses(TODAY, new int[] {365 * 5}); // Setup one dose in prek (one year old), two in
        // k-12 (8 and 9 years old)
        evaluateStudentFromMStudentInformation(true); // Should pass, three acceptable doses
    }
}
