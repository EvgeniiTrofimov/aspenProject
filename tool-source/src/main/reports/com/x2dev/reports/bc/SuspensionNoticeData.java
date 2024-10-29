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
package com.x2dev.reports.bc;
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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Data source for the "Suspension Notice" report.
 *
 * @author X2 Development Corporation
 */
public class SuspensionNoticeData extends SecondaryStudentDataSource {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * Name for the "action code" report parameter.
     */
    public static final String ACTION_CODE_PARAM = "actionCode";

    /**
     * Name for the enumerated "copies" report parameter.
     */
    public static final String COPIES_PARAM = "copies";

    /**
     * Name for the "date" report parameter.
     */
    public static final String DATE_PARAM = "date";

    /**
     * Name for the "multiple mailings" report parameter. The value is a Boolean.
     */
    public static final String MULTIPLE_MAILINGS_PARAM = "multipleMailings";

    /**
     * Name for the "query by" report parameter.
     */
    public static final String QUERY_BY_PARAM = "queryBy";

    /*
     * Grid fields
     */
    public static final String FIELD_ACTION = "action";
    public static final String FIELD_ADDRESS = "address";
    public static final String FIELD_COPY = "copy";

    // Values for the COPIES_PARAM parameter
    public static final int ALL_COPIES = 3;
    public static final int PARENT_COPY = 0;
    public static final int SCHOOL_COPY = 1;
    public static final int STUDENT_SERVICES_COPY = 2;

    private boolean m_multipleMailings;
    private Map m_studentContacts;

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid dataGrid = new ReportDataGrid(500, 2);

        String actionCode = (String) getParameter(ACTION_CODE_PARAM);
        int copy = ((Integer) getParameter(COPIES_PARAM)).intValue();

        PlainDate date = (PlainDate) getParameter(DATE_PARAM);
        m_multipleMailings = ((Boolean) getParameter(MULTIPLE_MAILINGS_PARAM)).booleanValue();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(ConductAction.COL_ACTION_CODE, actionCode);

        int queryBy = ((Integer) getParameter(QUERY_BY_PARAM)).intValue();
        switch (queryBy) {
            case 0: // Current Selection
                Criteria incidentCriteria = getCurrentCriteria();
                SubQuery sub = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
                criteria.addIn(ConductAction.COL_INCIDENT_OID, sub);
                break;

            default: // Date
                criteria.addEqualTo(ConductAction.REL_INCIDENT + "." +
                        ConductIncident.COL_INCIDENT_DATE, date);
                break;
        }

        if (isSchoolContext()) {
            if (hasSpecifiedCriteria()) {
                criteria.addAndCriteria(getStudentObjectCriteria(ConductAction.COL_STUDENT_OID,
                        ConductAction.REL_STUDENT,
                        ConductAction.COL_SCHOOL_OID));
            } else {
                criteria.addEqualTo(ConductAction.COL_SCHOOL_OID, getSchool().getOid());
            }
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(ConductAction.class));
        }

        QueryByCriteria query = new QueryByCriteria(ConductAction.class, criteria);

        query.addOrderByAscending(ConductAction.REL_STUDENT + "." + SisStudent.COL_NAME_VIEW);
        query.addOrderByAscending(ConductAction.REL_INCIDENT + "." + ConductIncident.COL_INCIDENT_DATE);

        if (m_multipleMailings) {
            loadMailingContacts();
        }

        QueryIterator actions = getBroker().getIteratorByQuery(query);
        try {
            while (actions.hasNext()) {
                ConductAction action = (ConductAction) actions.next();

                if (copy == ALL_COPIES) {
                    for (int i = 0; i < ALL_COPIES; i++) {
                        appendCopyToGrid(dataGrid, action, i);
                    }
                } else {
                    appendCopyToGrid(dataGrid, action, copy);
                }
            }
        } finally {
            actions.close();
        }

        dataGrid.beforeTop();
        return dataGrid;
    }

    /**
     * Appends the passed information to the data grid. Also appends alternate contact copies if the
     * copy is for the parent.
     *
     * @param dataGrid ReportDataGrid
     * @param action ConductAction
     * @param copy int
     */
    private void appendCopyToGrid(ReportDataGrid dataGrid, ConductAction action, int copy) {
        dataGrid.append();
        dataGrid.set(FIELD_ACTION, action);
        dataGrid.set(FIELD_COPY, Integer.valueOf(copy));
        dataGrid.set(FIELD_ADDRESS,
                action.getStudent().getPerson().getResolvedMailingAddress());


        if (m_multipleMailings && copy == PARENT_COPY) {
            Collection contacts =
                    (Collection) m_studentContacts.get(action.getStudentOid());
            if (contacts != null) {
                Iterator contactIterator = contacts.iterator();
                while (contactIterator.hasNext()) {
                    StudentContact contact = (StudentContact) contactIterator.next();

                    dataGrid.append();
                    dataGrid.set(FIELD_ACTION, action);
                    dataGrid.set(FIELD_COPY, Integer.valueOf(copy));
                    dataGrid.set(FIELD_ADDRESS,
                            contact.getContact().getPerson().getResolvedMailingAddress());
                }
            }
        }
    }

    /**
     * Loads the mailing contacts for students into a Map of StudentContacts keyed to student OID.
     */
    private void loadMailingContacts() {
        Criteria criteria = new Criteria();
        criteria.addEqualTo(StudentContact.COL_CONDUCT_MAILING_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(StudentContact.COL_LIVES_WITH_INDICATOR, Boolean.TRUE);

        if (isSchoolContext()) {
            criteria.addEqualTo(StudentContact.REL_STUDENT + "." +
                    SisStudent.COL_SCHOOL_OID, getSchool().getOid());

            /*
             * Include secondary students if needed
             */
            if ((getParameter(SECONDARY_STUDENT_PRIMARY_SCHOOL) != null &&
                    ((Boolean) getParameter(SECONDARY_STUDENT_PRIMARY_SCHOOL)).booleanValue()) ||
                    (getParameter(SECONDARY_STUDENT_PRIMARY_SCHOOL) != null &&
                            ((Boolean) getParameter(SECONDARY_STUDENT_PRIMARY_SCHOOL)).booleanValue())) {
                Criteria secondaryCriteria = new Criteria();
                secondaryCriteria.addEqualTo(StudentSchool.COL_SCHOOL_OID, getSchool().getOid());
                secondaryCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
                secondaryCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));

                SubQuery secondaryStudent = new SubQuery(StudentSchool.class,
                        StudentSchool.COL_STUDENT_OID, secondaryCriteria);

                Criteria secondaryContactCriteria = new Criteria();
                secondaryContactCriteria.addIn(StudentContact.COL_STUDENT_OID, secondaryStudent);

                criteria.addOrCriteria(secondaryContactCriteria);
            }
        } else {
            criteria.addAndCriteria(getOrganizationCriteria(StudentContact.class));
        }

        QueryByCriteria query = new QueryByCriteria(StudentContact.class, criteria);

        m_studentContacts = getBroker().getGroupedCollectionByQuery(query,
                StudentContact.COL_STUDENT_OID, 2000);
    }
}
