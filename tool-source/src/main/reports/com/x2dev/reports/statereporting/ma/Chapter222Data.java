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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.reports.ReportDataGrid;
import com.follett.fsc.core.k12.tools.reports.SecondaryStudentDataSource;
import com.x2dev.sis.model.beans.ConductAction;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.utils.types.PlainDate;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import net.sf.jasperreports.engine.JRDataSource;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class Chapter222Data.
 */
public abstract class Chapter222Data extends SecondaryStudentDataSource {
    /**
     * Appends the passed information to the data grid. Also appends alternate contact copies if the
     * copy is for the parent.
     *
     * @param dataGrid ReportDataGrid
     * @param action ConductAction
     * @param copy int
     */
    abstract protected void appendCopyToGrid(ReportDataGrid dataGrid, ConductAction action, int copy);

    protected static final String INPUT_PARAM_ACTION_CODE = "actionCode";
    protected static final String INPUT_PARAM_COPIES = "copies";
    protected static final String INPUT_PARAM_DATE = "date";
    protected static final String INPUT_PARAM_MULTIPLE_MAILINGS = "multipleMailings";
    protected static final String INPUT_PARAM_QUERY_BY = "queryBy";
    protected static final String INPUT_PARAM_SIGNATURE_BY = "letterSignatureBy";

    // The actionCode parameter value, which means that all the records should be displayed
    protected static final String PARAM_ANY_CODE = "Any code";

    // Values for the COPIES_PARAM parameter
    protected static final int ALL_COPIES = 3;
    protected static final int PARENT_COPY = 0;
    protected static final int SCHOOL_COPY = 1;
    protected static final int STUDENT_SERVICES_COPY = 2;

    protected Map<String, ReferenceCode> m_conductAction;
    protected Map<String, Collection<StudentContact>> m_studentContacts;
    protected Map<String, Map<String, Collection<PlainDate>>> m_schoolDays;
    protected boolean m_multipleMailings;
    protected Map<String, String> m_contactPriorityRelationship = new HashMap<String, String>();

    /**
     * Gather data.
     *
     * @return JRDataSource
     * @see com.follett.fsc.core.k12.tools.reports.ReportJavaSourceNet#gatherData()
     */
    @Override
    protected JRDataSource gatherData() {
        ReportDataGrid dataGrid = new ReportDataGrid(500, 2);

        String actionCode = (String) getParameter(INPUT_PARAM_ACTION_CODE);
        int copy = ((Integer) getParameter(INPUT_PARAM_COPIES)).intValue();

        PlainDate date = (PlainDate) getParameter(INPUT_PARAM_DATE);
        m_multipleMailings = ((Boolean) getParameter(INPUT_PARAM_MULTIPLE_MAILINGS)).booleanValue();

        Criteria criteria = new Criteria();
        if (!PARAM_ANY_CODE.equals(actionCode)) {
            criteria.addEqualTo(ConductAction.COL_ACTION_CODE, actionCode);
        }

        int queryBy = ((Integer) getParameter(INPUT_PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 0: // Current Selection
                Criteria incidentCriteria = getCurrentCriteria();
                SubQuery sub = new SubQuery(ConductIncident.class, X2BaseBean.COL_OID, incidentCriteria);
                criteria.addIn(ConductAction.COL_INCIDENT_OID, sub);
                break;

            default: // Date
                criteria.addEqualTo(ConductAction.REL_INCIDENT + PATH_DELIMITER +
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
     * Initialize the Conduct Action reference codes.
     */
    protected void loadConductActionCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField conductionActionCodeField =
                dictionary.findDataDictionaryField(ConductIncident.class.getName(),
                        ConductIncident.COL_INCIDENT_CODE);


        ReferenceTable refTable = conductionActionCodeField.getReferenceTable();
        if (refTable == null) {
            m_conductAction = new HashMap<String, ReferenceCode>();
        } else {
            m_conductAction = refTable.getCodeMap(getBroker());
        }
    }

    /**
     * Load school days.
     */
    /*
     * Loads in session days for current school and current context.
     */
    protected void loadSchoolDays() {
        m_schoolDays = new HashMap<String, Map<String, Collection<PlainDate>>>();

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());

        if (isSchoolContext()) {
            criteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                    SchoolCalendar.COL_SCHOOL_OID,
                    getSchool().getOid());
        }

        QueryByCriteria query = new QueryByCriteria(SchoolCalendarDate.class, criteria);
        query.addOrderByAscending(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_SCHOOL_OID);
        query.addOrderByAscending(SchoolCalendarDate.REL_SCHOOL_CALENDAR + PATH_DELIMITER +
                SchoolCalendar.COL_CALENDAR_ID);
        query.addOrderByAscending(SchoolCalendarDate.COL_DATE);

        QueryIterator iterator = null;
        try {
            iterator = getBroker().getIteratorByQuery(query);
            while (iterator.hasNext()) {
                SchoolCalendarDate csd = (SchoolCalendarDate) iterator.next();
                SchoolCalendar cas = csd.getSchoolCalendar();
                String schoolOid = cas.getSchoolOid();
                String calendarId = cas.getCalendarId();

                // Ensure all levels of map
                Map<String, Collection<PlainDate>> schoolCalendars = m_schoolDays.get(schoolOid);
                if (schoolCalendars == null) {
                    schoolCalendars = new HashMap<String, Collection<PlainDate>>();
                    m_schoolDays.put(schoolOid, schoolCalendars);
                }

                Collection<PlainDate> schoolDays = schoolCalendars.get(calendarId);
                if (schoolDays == null) {
                    schoolDays = new LinkedList<PlainDate>();
                    schoolCalendars.put(calendarId, schoolDays);
                }

                // Add school-day
                if (csd.getInSessionIndicator()) {
                    schoolDays.add(csd.getDate());
                }
            }
        } finally {
            if (iterator != null) {
                iterator.close();
            }
        }
    }

    /**
     * Returns action description by reference code
     * Method loadConductActionCodes should be performed first.
     *
     * @param referenceCode String
     * @return description
     */
    protected String getActionCode(String referenceCode) {
        if (m_conductAction.containsKey(referenceCode)) {
            return m_conductAction.get(referenceCode).getDescription();
        }
        return referenceCode;
    }

    /**
     * Identify if specified day is in session for calendar that corresponds to specified student.
     * Method loadSchoolDays should be performed first.
     *
     * @param student SisStudent
     * @param day PlainDate
     * @return Boolean - TRUE/FALSE if the day is/isn't in-session;
     *         null if day is out of calendar ranges or if no calendar found
     */
    protected Boolean isSchoolDay(SisStudent student, PlainDate day) {
        String schoolOid = student.getSchoolOid();
        String calendarCode = student.getCalendarCode();

        if (m_schoolDays.containsKey(schoolOid)) {
            Map<String, Collection<PlainDate>> schoolCalendars = m_schoolDays.get(schoolOid);
            if (schoolCalendars.containsKey(calendarCode)) {
                List<PlainDate> schoolDays = (List<PlainDate>) schoolCalendars.get(calendarCode);
                if (!(schoolDays.isEmpty() ||
                        schoolDays.get(0).after(day) ||
                        schoolDays.get(schoolDays.size() - 1).before(day))) {
                    return Boolean.valueOf(schoolDays.contains(day));
                }
            }
        }
        return null;
    }

    /**
     * Returns SisPerson of principal or vice principal depending on school the input parameter
     * "letterSignatureBy".
     *
     * @param school SisSchool
     * @return Sis person
     */
    protected SisPerson getPrincipal(SisSchool school) {
        SisPerson person = null;

        String signer = (String) getParameter(INPUT_PARAM_SIGNATURE_BY);
        if ("Vice Principal".equals(signer)) {
            person = school.getAdministrator2() == null ? null : school.getAdministrator2().getPerson();
            return person;
        }
        person = school.getAdministrator1() == null ? null : school.getAdministrator1().getPerson();
        return person;
    }
}
