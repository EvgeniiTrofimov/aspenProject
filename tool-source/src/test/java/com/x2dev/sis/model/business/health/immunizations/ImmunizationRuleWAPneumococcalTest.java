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
 * The Class ImmunizationRuleWAPneumococcalTest.
 */
public class ImmunizationRuleWAPneumococcalTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/procedures/statereporting/wa/Immunizationrule-pneumococcal.xml");
    }

    /**
     * Test ticket T 20125072.
     */
    @Test
    public void testTicketT20125072() {
        // T20125072 - WA Immunizations: Student showing compliant when no doses of anything are
        // entered
        // 8/10/1999
        setup("2013-10-23");
        setStudentDOBandDosesString("1999-08-10");
        evaluateStudentFromMStudentInformation(true);
        // Correct, students only need this in preK
    }
}
