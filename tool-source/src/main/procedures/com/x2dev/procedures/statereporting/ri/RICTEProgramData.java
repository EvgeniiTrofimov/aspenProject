/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2018 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.x2dev.sis.model.beans.GraduationProgram;
import com.x2dev.utils.X2BaseException;
import java.util.HashSet;
import java.util.Set;


/**
 * RI state report for Class Roster export. This class implements the data
 * export for the RI Class Roster export.
 *
 * @author X2 Development Corporation
 */
public class RICTEProgramData extends CTEStateReportData {

    /**
     * Implementation of StateReportEntity to be used by the Class Roster export. This
     * must be a public static inner class with a public no argument constructor
     * so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class RICTEProgramEntity extends CTEEmptyEntity {

        /**
         * Empty no argument constructor for dynamic instantiation.
         */
        public RICTEProgramEntity() {
            // Empty no argument constructor for dynamic instantiation.
        }

        /**
         * initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean)
                throws X2BaseException {
            super.intitialize(data, bean);
            GraduationProgram graduationProgram = (GraduationProgram) bean;
            RICTEProgramData prgData = (RICTEProgramData) data;
            if (!prgData.m_exportedBeans.contains(graduationProgram.getOid())) {
                prgData.m_exportedBeans.add(graduationProgram.getOid());
                setRowCount(1);
            } else {
                setRowCount(0);
            }
        }
    }

    protected Set<String> m_exportedBeans = new HashSet<>();

    /**
     * Initialize.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initialize()
     */
    @Override
    protected void initialize() {
        super.initialize();
    }

    /**
     * Initialize entity class.
     *
     * @see com.x2dev.procedures.statereporting.ri.CTEStateReportData#initializeEntityClass()
     */
    @Override
    protected void initializeEntityClass() {
        m_cteEntityClass = RICTEProgramEntity.class;
    }
}
