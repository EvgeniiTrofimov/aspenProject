/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2002-2017 Follett Software Company.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.on.revised;

import com.x2dev.procedures.statereporting.on.revised.OnBeans.OnOrganization;

/**
 * The Class OnsisValidationFileOenData.
 */
public class OnsisValidationFileOenData extends OnsisStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * The Class OenSchoolDetailsEntity.
     */
    public static class OenSchoolDetailsEntity extends OnsisStateReportEntity {

        /**
         * Instantiates a new oen school details entity.
         */
        public OenSchoolDetailsEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            OnOrganization currentOrganization = (OnOrganization) getBean();
            StringBuilder entityName = new StringBuilder();
            entityName.append("OEN Details");
            entityName.append(currentOrganization.getName());

            return entityName.toString();
        }
    }

    /**
     * @see com.x2dev.procedures.statereporting.on.OnsisStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        setEntityClass(OenSchoolDetailsEntity.class);
    }
}
