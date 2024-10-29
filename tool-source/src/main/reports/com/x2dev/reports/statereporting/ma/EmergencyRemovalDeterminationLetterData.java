/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2003 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.reports.statereporting.ma;

import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.X2BaseException;
import java.util.Collection;
import java.util.Iterator;

/**
 * Data source for the "Suspension Notice" report.
 *
 * @author X2 Development Corporation
 */
public class EmergencyRemovalDeterminationLetterData extends Chapter222Data {
    private static final String FIELD_ACTION = "action";
    private static final String FIELD_ACTION_CODE = "actionCode";
    private static final String FIELD_ADDRESS = "address";
    private static final String FIELD_COPY = "copy";
    private static final String FIELD_PRINCIPAL = "principal";
    private static final String FIELD_STUDENT = "student";
    private static final String FIELD_NOTICE_DATE = "noticeDate";
    private static final String FIELD_DELIVERY_METHOD = "methodOfDelivery";

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#initialize()
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        loadConductActionCodes();
    }

    /**
     * Append copy to grid.
     *
     * @param dataGrid ReportDataGrid
     * @param action ConductAction
     * @param copy int
     * @see Chapter222Data#appendCopyToGrid(com.follett.fsc.core.k12.tools.reports.ReportDataGrid,
     *      com.x2dev.sis.model.beans.ConductAction, int)
     */
    @Override
    protected void appendCopyToGrid(ReportDataGrid dataGrid, ConductAction action, int copy) {
        SisStudent student = action.getStudent();

        dataGrid.append();
        dataGrid.set(FIELD_ACTION, action);
        dataGrid.set(FIELD_STUDENT, student);
        dataGrid.set(FIELD_COPY, Integer.valueOf(copy));
        dataGrid.set(FIELD_ADDRESS, student.getPerson().getResolvedMailingAddress());
        dataGrid.set(FIELD_ACTION_CODE, getActionCode(action.getIncident().getIncidentCode()));
        dataGrid.set(FIELD_PRINCIPAL, getPrincipal(student.getSchool()));
        dataGrid.set(FIELD_NOTICE_DATE, action.getIncident().getFieldA024());
        dataGrid.set(FIELD_DELIVERY_METHOD, action.getIncident().getFieldB013());

        if (m_multipleMailings && copy == PARENT_COPY && student.getPerson().getAge() < 18) {
            Collection<StudentContact> contacts = m_studentContacts.get(student.getOid());
            if (contacts != null) {
                Iterator contactIterator = contacts.iterator();
                while (contactIterator.hasNext()) {
                    StudentContact contact = (StudentContact) contactIterator.next();
                    if (contact.getConductMailingIndicator() && contact.getLivesWithIndicator()) {
                        dataGrid.append();
                        dataGrid.set(FIELD_ACTION, action);
                        dataGrid.set(FIELD_STUDENT, student);
                        dataGrid.set(FIELD_COPY, Integer.valueOf(copy));
                        dataGrid.set(FIELD_ADDRESS, contact.getContact().getPerson().getResolvedMailingAddress());
                        dataGrid.set(FIELD_ACTION_CODE, getActionCode(action.getIncident().getIncidentCode()));
                        dataGrid.set(FIELD_PRINCIPAL, getPrincipal(student.getSchool()));
                    }
                }
            }
        }
    }
}
