/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2021 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.common.ToolsSharedContainer.FieldRetriever;
import com.x2dev.utils.X2BaseException;
import java.util.Arrays;
import java.util.Map;

/**
 * @author Follett Software Company
 * @copyright 2021
 */
public class OnsisSchoolSubmission extends OnsisStateReportData {
    public static final String ELEMENT_SCHOOL = "SCHOOL";
    public static final String ELEMENT_STUDENT = "STUDENT";


    public static class SchoolSubmissionEntity extends OnsisStateReportEntity {
        public SchoolSubmissionEntity() {
            // dynamic instantiation
        }

        public String getPeriodCode() {
            return getReportData().getGlobalData().getSubmissionType().getSubmissionPeriodCode();
        }
    }

    public static final String ONSIS_TOPIC = "SCHOOL_SUBMISSION";

    /**
     * Builds the beans.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#buildBeans()
     */
    @Override
    public void buildBeans() throws X2BaseException {
        setBeans(Arrays.asList(getGlobalData().getOrganizationToolBean()));
    }

    /**
     * Gets the calcs.
     *
     * @return Map
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#getCalcs()
     */
    @Override
    public Map<String, FieldRetriever> getCalcs() {
        Map<String, FieldRetriever> calcs = super.getCalcs();
        calcs.put(OnsisRetrieverFromCsv.CALC_ID, new OnsisRetrieverFromCsv());
        return calcs;
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(SchoolSubmissionEntity.class);
    }
}
