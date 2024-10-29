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
 * Unit tests for the allowed-series attribute on immunization compliance dose rules.
 *
 * @author mmastrangelo
 */
public class ImmunizationRuleDoseAllowedSeriesTest extends ImmunizationRuleTestMaster {

    public void setup(String today) {
        super.setup(today, "/com/x2dev/sis/model/business/health/immunizations/Immunizationrule-DoseAllowedSeries.xml");
    }

    /**
     * Test dose sequence.
     */
    @Test
    public void testDoseSequence() {
        setup("2014-07-26");

        setStudentDOBandDosesString("2002-11-02, 2004-09-15:DTaP, 2005-09-15:DTaP, 2006-09-15:XXX, 2006-11-15:Tdap");
        evaluateStudentFromMStudentInformation(true);

        setStudentDOBandDosesString("2002-11-02, 2004-09-15:DTP, 2005-09-15:DTaP, 2006-09-15:XYZ, 2006-11-15:Tdap");
        evaluateStudentFromMStudentInformation(true);

        setStudentDOBandDosesString("2002-11-02, 2004-09-15:Tdap, 2005-09-15:DTaP, 2006-09-15:XYZ, 2006-11-15:Tdap");
        evaluateStudentFromMStudentInformation(false);
    }
}
