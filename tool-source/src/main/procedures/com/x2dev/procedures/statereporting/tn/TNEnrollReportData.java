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
package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.web.AppGlobals;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStudentEnrollmentSpan;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.business.schedule.ScheduleManager;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * The Class TNEnrollReportData.
 */
public class TNEnrollReportData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = ""; 
    protected static final String ALIAS_DAY_EVENT_TYPE_2 = "DOE DAY EVENT TYPE 2";
    protected static final String ALIAS_DAY_EVENT_TYPE_3 = "DOE DAY EVENT TYPE 3";

    private static final String DAY_EVENT_CALENDAR_START = "CS";

    protected String m_fieldDayEventType2;
    protected String m_fieldDayEventType3;

    private Collection<String> m_calendarStartDateCodes;
    private Map<String, PlainDate> m_firstDatesOfSchools;
    private ScheduleManager m_scheduleMgr;

    /**
     * Determine the enrollment date that should be used as the start date for
     * this student. The default case is the later of the first in-session date for the school
     * and the span first active date.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    @Override
    protected PlainDate determineEnrollmentDate(SisStudent student, TNStudentEnrollmentSpan span) {

        String calendarCode = getStdCalendarId(student);
        String key = calendarCode + span.getSchool().getOid();

        if (m_firstDatesOfSchools == null) {
            m_firstDatesOfSchools = new HashMap<String, PlainDate>();
        }

        PlainDate firstInSessionDate = null;
        if (m_firstDatesOfSchools.containsKey(key)) {
            firstInSessionDate = m_firstDatesOfSchools.get(key);
        } else {
            firstInSessionDate = getFirstCalendarDay(span.getSchool(), calendarCode);
            if (firstInSessionDate == null) {
                // try to use most common calendar
                if (span.getSchool().getActiveSchedule() != null) {
                    if (m_scheduleMgr == null) {
                        m_scheduleMgr = new ScheduleManager(getBroker());
                    }
                    calendarCode = m_scheduleMgr.getMostCommonCalendar(span.getSchool().getActiveSchedule(), null);
                    firstInSessionDate = getFirstCalendarDay(span.getSchool(), calendarCode);
                }
            }
            if (firstInSessionDate == null) {
                firstInSessionDate = getCurrentContext().getStartDate();
                AppGlobals.getLog().log(Level.WARNING, "First In-Session date not found for school " +
                        span.getSchool().getName() + " and calendar code " + calendarCode);
            }
            m_firstDatesOfSchools.put(key, firstInSessionDate);
        }

        PlainDate enrDate = span.getFirstActiveEnrollment().getEnrollmentDate();
        if (enrDate != null && enrDate.after(firstInSessionDate)) {
            firstInSessionDate = enrDate;
        }

        return firstInSessionDate;
    }


    /**
     * Determine the withdrawal date that should be used as the exit date for
     * this student. The default case is the enrollment date from the first inactive enrollment.
     *
     * @param student SisStudent
     * @param span TNStudentEnrollmentSpan
     * @return PlainDate
     */
    @Override
    protected PlainDate determineWithdrawalDate(SisStudent student, TNStudentEnrollmentSpan span) {
        return span.getFirstInactiveEnrollment() == null ? null : span.getFirstInactiveEnrollment().getEnrollmentDate();
    }

    /**
     * method for getting calendars for all schools for given context.
     *
     * @throws X2BaseException exception
     */

    /*
     * Initialize the data module. Initialize necessary working resources. Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();
        m_fieldDayEventType2 = translateAliasToJavaName(ALIAS_DAY_EVENT_TYPE_2, true);
        m_fieldDayEventType3 = translateAliasToJavaName(ALIAS_DAY_EVENT_TYPE_3, true);
    }

    /**
     * Get collection of calendar date event codes corresponding to given state reference code.
     *
     * @return Collection of program codes
     */
    private Collection<String> getCalendarStartDateCodes() {
        if (m_calendarStartDateCodes == null) {
            X2Criteria criteria = new X2Criteria();
            DataDictionaryField field =
                    getDataDictionaryField(SisSchoolCalendarDate.class, SisSchoolCalendarDate.COL_SCHEDULE_DAY_TYPE);
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            criteria.addBeginsWith(ReferenceCode.COL_STATE_CODE, DAY_EVENT_CALENDAR_START);
            String[] columns = new String[] {ReferenceCode.COL_CODE};
            ReportQueryByCriteria query = new ReportQueryByCriteria(ReferenceCode.class, columns, criteria);

            m_calendarStartDateCodes = new ArrayList<String>();
            ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
            try {
                if (iterator.hasNext()) {
                    Object[] record = (Object[]) iterator.next();
                    String code = (String) record[0];
                    m_calendarStartDateCodes.add(code);
                }
            } finally {
                iterator.close();
            }
        }
        return m_calendarStartDateCodes;
    }

    /**
     * Returns the first calendar start date for a particlar calendar.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private PlainDate getFirstCalendarDay(SisSchool school, String calendar) {
        PlainDate firstDate = null;

        Criteria criteria = new Criteria();
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_SCHOOL_OID,
                school.getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_DISTRICT_CONTEXT_OID,
                getCurrentContext().getOid());
        criteria.addEqualTo(SisSchoolCalendarDate.REL_SCHOOL_CALENDAR + "." + SchoolCalendar.COL_CALENDAR_ID, calendar);

        Criteria criteriaOr = new Criteria();

        Criteria criteriaEvent = new Criteria();
        criteriaEvent.addIn(SisSchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, getCalendarStartDateCodes());
        criteriaOr.addOrCriteria(criteriaEvent);

        criteriaEvent = new Criteria();
        criteriaEvent.addIn(m_fieldDayEventType2, getCalendarStartDateCodes());
        criteriaOr.addOrCriteria(criteriaEvent);

        criteriaEvent = new Criteria();
        criteriaEvent.addIn(m_fieldDayEventType3, getCalendarStartDateCodes());
        criteriaOr.addOrCriteria(criteriaEvent);

        criteria.addAndCriteria(criteriaOr);

        QueryByCriteria calendarQuery = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
        calendarQuery.addOrderByAscending(SisSchoolCalendarDate.COL_DATE);
        QueryIterator calendars = null;
        try {
            calendars = getBroker().getIteratorByQuery(calendarQuery);
            if (calendars.hasNext()) {
                SisSchoolCalendarDate calendarDate = (SisSchoolCalendarDate) calendars.next();
                firstDate = calendarDate.getDate();
            }
        } finally {
            if (calendars != null) {
                calendars.close();
            }
        }

        return firstDate;
    }
}
