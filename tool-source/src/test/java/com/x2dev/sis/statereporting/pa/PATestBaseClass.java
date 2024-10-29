/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.sis.statereporting.pa;

import com.follett.fsc.core.k12.test.X2StateReportTest;

/**
 * The Class FLStateReportTest.
 */
public class PATestBaseClass extends X2StateReportTest {

    private static boolean s_applicationInitFailure = false;
    private static boolean s_initialized = false;

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#isInitFailed()
     */
    @Override
    protected boolean isInitFailed() {
        return s_applicationInitFailure;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#isInitialized()
     */
    @Override
    protected boolean isInitialized() {
        return s_initialized;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#setInitialized(boolean)
     */
    @Override
    protected void setInitialized(boolean isInitialized) {
        s_initialized = isInitialized;
    }

    /**
     * @see com.follett.fsc.core.k12.test.X2StateReportTest#setInitFailed(boolean)
     */
    @Override
    protected void setInitFailed(boolean isInitFailed) {
        s_applicationInitFailure = isInitFailed;
    }
}
