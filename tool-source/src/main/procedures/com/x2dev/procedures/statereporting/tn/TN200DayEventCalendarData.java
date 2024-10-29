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

package com.x2dev.procedures.statereporting.tn;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for calendar day export.
 */
public class TN200DayEventCalendarData extends TNStateReportData {
    /**
     * Entity class for calendar day export.
     *
     */
    public static class TN200DayEventCalendarEntity extends TNStateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Class for encapsulation EventType and EventDuration.
         */
        public class EventTypeDuration {
            private String eventDuration;
            private String eventType;

            /**
             * Instantiates a new event type duration.
             *
             * @param EventType String
             * @param EventDuration String
             */
            public EventTypeDuration(String EventType, String EventDuration) {
                this.eventType = EventType;
                this.eventDuration = EventDuration;
            }

            /**
             * Gets the event duration.
             *
             * @return String
             */
            public String getEventDuration() {
                return eventDuration;
            }

            /**
             * Gets the event type.
             *
             * @return String
             */
            public String getEventType() {
                return eventType;
            }
        }

        /**
         * Entity instance variables.
         */
        private List<EventTypeDuration> m_dayEvents;


        /**
         * Instantiates a new TN 200 day event calendar entity.
         */
        public TN200DayEventCalendarEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current date type.
         *
         * @return Event type duration
         */
        public EventTypeDuration getCurrentDateType() {
            return m_dayEvents.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolCalendarDate date = (SchoolCalendarDate) getBean();
            return "District calendar. Id: " + date.getSchoolCalendar().getCalendarId() + ". Date: " + date.toString();
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc
         *      .core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            TN200DayEventCalendarData tnData = (TN200DayEventCalendarData) data;

            m_dayEvents = new ArrayList<EventTypeDuration>();

            SchoolCalendarDate day = (SchoolCalendarDate) getBean();

            List<String> stateCodeEvents = getDayEvents(tnData, day);
            if (!stateCodeEvents.isEmpty()) {
                for (String stateCodeEvent : stateCodeEvents) {
                    m_dayEvents.add(parsRefCode(stateCodeEvent));
                }
                setRowCount(m_dayEvents.size());
            } else {
                setRowCount(0);
            }

            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Gets the day events.
         *
         * @param tnData TN200DayEventCalendarData
         * @param day SchoolCalendarDate
         * @return List
         */
        private List<String> getDayEvents(TN200DayEventCalendarData tnData, SchoolCalendarDate day) {
            List<String> stateCodeEvents = new ArrayList<String>();
            String code1 = tnData.lookupStateValue(SchoolCalendarDate.class, SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE,
                    day.getScheduleDayType());
            if (!StringUtils.isEmpty(code1)) {
                stateCodeEvents.add(code1);
            }
            String code2 = tnData.lookupStateValue(SchoolCalendarDate.class, tnData.m_fieldEvent2,
                    (String) day.getFieldValueByBeanPath(tnData.m_fieldEvent2));
            if (!StringUtils.isEmpty(code2)) {
                stateCodeEvents.add(code2);
            }
            String code3 = tnData.lookupStateValue(SchoolCalendarDate.class, tnData.m_fieldEvent3,
                    (String) day.getFieldValueByBeanPath(tnData.m_fieldEvent3));
            if (!StringUtils.isEmpty(code3)) {
                stateCodeEvents.add(code3);
            }
            String code4 = tnData.lookupStateValue(SchoolCalendarDate.class, tnData.m_fieldEvent4,
                    (String) day.getFieldValueByBeanPath(tnData.m_fieldEvent4));
            if (!StringUtils.isEmpty(code4)) {
                stateCodeEvents.add(code4);
            }
            return stateCodeEvents;
        }

        /**
         * Parser for code. code sample: AD-033
         *
         * @param code String
         * @return EventTypeDuration
         */
        private EventTypeDuration parsRefCode(String code) {
            if (code == null || code.length() < 4) {
                return new EventTypeDuration(code, "100");
            }

            String eventType = code.substring(0, 2);
            String eventDur = code.substring(code.length() - 3, code.length());
            int intCode = Integer.parseInt(eventDur);
            return new EventTypeDuration(eventType, Integer.toString(intCode));
        }
    }

    /**
     * Field retriever for EVENT DATE field.
     */
    protected class RetrieveEventDate implements FieldRetriever {
        public static final String CALC_ID = "CAL_CALC_EVENTDAY";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SchoolCalendarDate distrDate = (SchoolCalendarDate) entity.getBean();
            return distrDate.getDate();
        }
    }

    /**
     * Field retriever for EVENT DURATION field.
     */
    protected class RetrieveEventDuration implements FieldRetriever {
        public static final String CALC_ID = "CAL_CALC_EVENTDURTN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TN200DayEventCalendarEntity tnEntity = (TN200DayEventCalendarEntity) entity;
            return tnEntity.getCurrentDateType().getEventDuration();
        }
    }

    /**
     * Field retriever for EVENT TYPE field.
     */
    protected class RetrieveEventType implements FieldRetriever {
        public static final String CALC_ID = "CAL_CALC_EVENTTYPE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TN200DayEventCalendarEntity tnEntity = (TN200DayEventCalendarEntity) entity;
            return tnEntity.getCurrentDateType().getEventType();
        }
    }

    /**
     * Field retriever for SCHOOL DAY TYPE field.
     */
    protected class RetrieveSchoolDayType implements FieldRetriever {
        public static final String CALC_ID = "CAL_CALC_SKLDAYTYPE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SchoolCalendarDate distrDate = (SchoolCalendarDate) entity.getBean();
            return (distrDate.getInSessionIndicator()) ? "I" : "N";
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {
        public static final String CALC_ID = "CAL_CALC_SCHOOLYEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TN200DayEventCalendarData tnData = (TN200DayEventCalendarData) data;
            return tnData.m_SchoolYear;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */

    private static final String ALIAS_EVENT_TYPE2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_EVENT_TYPE3 = "DOE DAY EVENT TYPE 3";
    private static final String ALIAS_EVENT_TYPE4 = "all-csd-CalendarEventType4";
    private static final String ALIAS_CALENDAR_NUMBER = "DOE CALENDAR NUMBER";

    private static final String DISTRICT_SCHOOL_CODE = "dist";

    private static final String PARAM_CALENDAR_NUMBERS = "calendarNumbers";

    protected Collection<String> m_calendarNumbers;
    protected String m_calendarNumbersCodesOids;
    protected String m_fieldEvent2;
    protected String m_fieldEvent3;
    protected String m_fieldEvent4;
    protected String m_fieldCalendarNumber;

    /**
     * Instance variables.
     */
    protected String m_SchoolYear;

    /**
     * @see com.x2dev.procedures.statereporting.tn.TNStateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query to load.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        super.initialize();

        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();
        m_calendarNumbersCodesOids = (String) getParameter(PARAM_CALENDAR_NUMBERS);
        if (m_calendarNumbersCodesOids != null) {
            loadCalendarNumbers();
        }
        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeCriteriaAndQuery();
        setEntityClass(TN200DayEventCalendarEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        initFieldRetrievers();
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private final String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Register custom field Retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();

        calcs.put(RetrieveSchoolYear.CALC_ID, new RetrieveSchoolYear());
        calcs.put(RetrieveEventDate.CALC_ID, new RetrieveEventDate());
        calcs.put(RetrieveEventType.CALC_ID, new RetrieveEventType());
        calcs.put(RetrieveSchoolDayType.CALC_ID, new RetrieveSchoolDayType());
        calcs.put(RetrieveEventDuration.CALC_ID, new RetrieveEventDuration());

        super.addCalcs(calcs);
    }

    /**
     * Create query based on the current context.
     */
    private void initializeCriteriaAndQuery() {
        X2Criteria distSchoolCriteria = new X2Criteria();
        distSchoolCriteria.addEqualTo(School.COL_SCHOOL_ID, DISTRICT_SCHOOL_CODE);
        SubQuery schoolSubQuery = new SubQuery(School.class, X2BaseBean.COL_OID, distSchoolCriteria);

        X2Criteria distrCalCriteria = new X2Criteria();
        distrCalCriteria.addIn(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_SCHOOL_OID, schoolSubQuery);
        if (m_calendarNumbers != null) {
            distrCalCriteria.addIn(
                    SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER + m_fieldCalendarNumber,
                    m_calendarNumbers);
        }
        distrCalCriteria.addEqualTo(SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER +
                SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_contextOid);

        QueryByCriteria schoolCalendarDateQuery = new QueryByCriteria(SchoolCalendarDate.class, distrCalCriteria, true);
        schoolCalendarDateQuery.addOrderBy(
                SchoolCalendarDate.REL_SCHOOL_CALENDAR + ModelProperty.PATH_DELIMITER + m_fieldCalendarNumber, true);
        schoolCalendarDateQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);
        setQuery(schoolCalendarDateQuery);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_SchoolYear = getCurentSchoolYear();
        m_fieldCalendarNumber = translateAliasToJavaName(ALIAS_CALENDAR_NUMBER, true);
        m_fieldEvent2 = translateAliasToJavaName(ALIAS_EVENT_TYPE2, true);
        m_fieldEvent3 = translateAliasToJavaName(ALIAS_EVENT_TYPE3, true);
        m_fieldEvent4 = translateAliasToJavaName(ALIAS_EVENT_TYPE4, true);
    }


    /**
     * Load calendar numbers.
     */
    private void loadCalendarNumbers() {
        m_calendarNumbers = new ArrayList<String>();

        DataDictionaryField field = getDataDictionaryField(SchoolCalendar.class, m_fieldCalendarNumber);
        if (field != null && field.hasReferenceTable()) {
            Collection<ReferenceCode> calendarCodes = field.getReferenceTable().getReferenceCodes();
            for (ReferenceCode calendarCode : calendarCodes) {
                if (m_calendarNumbersCodesOids.contains(calendarCode.getOid())) {
                    m_calendarNumbers.add(calendarCode.getCode());
                }
            }
        }
    }
}
