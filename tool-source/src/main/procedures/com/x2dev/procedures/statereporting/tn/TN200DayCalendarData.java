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
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Class for student extract export.
 */
public class TN200DayCalendarData extends TNStateReportData {
    /**
     * Entity class for staff member export.
     *
     */
    public static class TN200DayCalendarEntity extends TNStateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Class EventFactory for managing Events.
         */
        public class EventFactory {
            private Map<String, EventRule> m_daysType = new HashMap<String, EventRule>();

            /**
             * Instantiates a new event factory.
             *
             * @param rules List<String>
             */
            public EventFactory(List<String> rules) {
                for (String rule : rules) {
                    int separatorIndex = rule.indexOf(EVENT_RULE_DELIMITER);
                    String ruleCode = rule.substring(0, separatorIndex);
                    String isFullDay = rule.substring(separatorIndex + 1);
                    m_daysType.put(ruleCode, new EventRule(BooleanAsStringConverter.TRUE.equals(isFullDay)));
                }
            }

            /**
             * returns count of days with type = code.
             *
             * @param mainCode String
             * @return int
             */
            public int getCount(String mainCode) {
                if (m_daysType.get(mainCode) != null) {
                    return m_daysType.get(mainCode).isFullDay() ? (m_daysType.get(mainCode).getCount() + 50) / 100
                            : m_daysType.get(mainCode).getCount();
                }
                return 0;
            }

            /**
             * Gets the event rule.
             *
             * @param mainCode String
             * @return Event rule
             */
            public EventRule getEventRule(String mainCode) {
                return m_daysType.get(mainCode);
            }

            /**
             * Inc count.
             *
             * @param evCode EventCode
             */
            public void incCount(EventCode evCode) {
                if (evCode == null) {
                    return;
                }

                String mainCode = evCode.getMainCode();
                if (StringUtils.isEmpty(mainCode)) {
                    return;
                }

                EventRule evRule = getEventRule(mainCode);

                if (evRule == null) {
                    evRule = new EventRule(false);
                    m_daysType.put(mainCode, evRule);
                }

                m_daysType.get(mainCode).incCount(evCode.getSubCode());
            }

            /**
             * Checks if is validation error rule violation.
             *
             * @param evCode EventCode
             * @return true, if is validation error rule violation
             */
            public boolean isValidationErrorRuleViolation(EventCode evCode) {
                if (evCode == null) {
                    return false;
                }

                EventRule evRule = getEventRule(evCode.getMainCode());

                if (evRule == null) {
                    return false;
                }

                if (evRule.isFullDay() && evCode.getSubCode() < 100) {
                    return true;
                }

                return false;
            }

            /**
             * Parser for code. code sample: AD-033
             *
             * @param code String
             * @return EventCode
             */
            public EventCode parsCode(String code) {
                int value = 100;

                try {
                    if (code == null) {
                        return null;
                    }

                    if (code.length() < 4) {
                        return new EventCode(code, 100);
                    }

                    String mainCode = code.substring(0, 2);
                    String subCode = code.substring(code.length() - 3, code.length());
                    try {
                        value = Integer.parseInt(subCode);
                    } catch (NumberFormatException e) {
                        // do nothing
                    }
                    return new EventCode(mainCode, value);
                } catch (Exception e) {
                    return null;
                }
            }
        }

        /**
         * Class for encapsulation EventMainCode and EventSubCode.
         */
        protected class EventCode {
            private String m_maincode;
            private int m_subCode;

            /**
             * Instantiates a new event code.
             *
             * @param mainCode String
             * @param subCode int
             */
            public EventCode(String mainCode, int subCode) {
                m_maincode = mainCode;
                m_subCode = subCode;
            }

            /**
             * Gets the main code.
             *
             * @return String
             */
            public String getMainCode() {
                return m_maincode;
            }

            /**
             * Gets the sub code.
             *
             * @return int
             */
            public int getSubCode() {
                return m_subCode;
            }
        }

        /**
         * Class rule description for Event.
         */
        protected class EventRule {
            private boolean isFullDay;
            private int m_count = 0;

            /**
             * Instantiates a new event rule.
             *
             * @param isFullDay boolean
             */
            public EventRule(boolean isFullDay) {
                this.isFullDay = isFullDay;
            }

            /**
             * returns count of days.
             *
             * @return int
             */
            public int getCount() {
                return m_count;
            }

            /**
             * Inc count.
             *
             * @param increment int
             */
            public void incCount(int increment) {
                m_count += increment;
            }

            /**
             * Checks if is full day.
             *
             * @return true, if is full day
             */
            public boolean isFullDay() {
                return isFullDay;
            }
        }

        /**
         * Entity instance variables.
         */
        EventFactory m_eventFactory;

        protected int m_instructDaysNumber = 0;
        protected TreeMap<PlainDate, List> m_eventsInYear;

        /**
         * Instantiates a new TN 200 day calendar entity.
         */
        public TN200DayCalendarEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolCalendar schoolCalendar = (SchoolCalendar) getBean();
            return "District calendar. Id: " + schoolCalendar.getCalendarId();
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            TN200DayCalendarData tnData = (TN200DayCalendarData) data;
            m_eventFactory = new EventFactory(tnData.m_eventRules);
            m_eventsInYear = new TreeMap<PlainDate, List>();

            SchoolCalendar distCalendar = (SchoolCalendar) bean;
            Collection<SchoolCalendarDate> days = distCalendar.getSchoolCalendarDates(data.getBroker());

            for (SchoolCalendarDate day : days) {
                List<String> stateCodeEvents = getDayEvents(tnData, day);
                if (!stateCodeEvents.isEmpty()) {
                    List<EventCode> eventsInDay = new ArrayList<EventCode>();
                    for (String stateCodeEvent : stateCodeEvents) {
                        EventCode evCode = m_eventFactory.parsCode(stateCodeEvent);
                        if (evCode != null) {
                            m_eventFactory.incCount(evCode);
                            eventsInDay.add(evCode);
                        }
                    }
                    if (!eventsInDay.isEmpty()) {
                        m_eventsInYear.put(day.getDate(), eventsInDay);
                    }
                }

                if (day.getInSessionIndicator()) {
                    m_instructDaysNumber += 1;
                }
            }
            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Gets the day events.
         *
         * @param tnData TN200DayCalendarData
         * @param day SchoolCalendarDate
         * @return List
         */
        private List<String> getDayEvents(TN200DayCalendarData tnData, SchoolCalendarDate day) {
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
            return stateCodeEvents;
        }

        /**
         * Gets the instruct days counter.
         *
         * @return Integer
         */
        public Integer getInstructDaysCounter() {
            return Integer.valueOf(m_instructDaysNumber);
        }
    }

    /**
     * Field retriever for Day Type Count.
     */
    protected class RetrieveDayTypeCount implements FieldRetriever {
        public static final String CALC_ID = "CALC_DAY_TYPE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TN200DayCalendarEntity seEntity = (TN200DayCalendarEntity) entity;
            String param = (String) field.getParameter();
            if (!StringUtils.isEmpty(param)) {
                int count = seEntity.m_eventFactory.getCount(param);
                return Integer.valueOf(count);
            }
            StateReportValidationError error = new StateReportValidationError(entity, field, "Parameter is missed",
                    "This field requires calculation parameter to retrieve value.");
            entity.addRetrievalError(field.getFieldId(), error);
            return Integer.valueOf(0);
        }
    }

    /**
     * Field retriever for Instructional Days field.
     */
    protected class RetrieveInstructionalDays implements FieldRetriever {
        public static final String CALC_ID = "CTX_CALC_INSTRDAYS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TN200DayCalendarEntity seEntity = (TN200DayCalendarEntity) entity;
            return seEntity.getInstructDaysCounter();
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {
        public static final String CALC_ID = "CTX_CALC_SCHOOLYEAR";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TN200DayCalendarData seData = (TN200DayCalendarData) data;
            return seData.m_SchoolYear;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String EVENT_RULE_DELIMITER = ":";

    private static final String ALIAS_EVENT_TYPE2 = "DOE DAY EVENT TYPE 2";
    private static final String ALIAS_EVENT_TYPE3 = "DOE DAY EVENT TYPE 3";
    private static final String ALIAS_CALENDAR_NUMBER = "DOE CALENDAR NUMBER";
    private static final String ALIAS_SCHEDULED_DAYS = "DOE SCHEDULED DAYS";
    private static final String ALIAS_DISTRICT_SI_STOCKPILE = "DOE DISTRICT SI STOCKPILE";
    private static final String ALIAS_DISTRICT_SP_STOCKPILE = "DOE SP STOCKPILE";

    private static final String INPUT_EVENT_RULES = "eventRules";

    private static final String EVENT_RULES_SEPARATOR = ";";
    private static final String DISTRICT_SCHOOL_CODE = "dist";

    private static final String PARAM_CALENDAR_NUMBERS = "calendarNumbers";

    /**
     * Instance variables.
     */
    protected String m_calendarNumbersCodesOids;
    protected String m_SchoolYear;
    protected String m_fieldEvent2;
    protected String m_fieldEvent3;
    protected String m_fieldCalendarNumber;
    protected String m_fieldScheduledDays;
    protected String m_fieldSiStockpile;
    protected String m_fieldSpStockpile;
    protected Collection<String> m_calendarNumbers;
    protected List<String> m_eventRules;

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
        m_calendarNumbersCodesOids = (String) getParameter(PARAM_CALENDAR_NUMBERS);
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();
        if (m_calendarNumbersCodesOids != null) {
            loadCalendarNumbers();
        }
        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeCriteriaAndQuery();
        setEntityClass(TN200DayCalendarEntity.class);

        // Add any necessary FieldRetrievers an Field Validators
        initFieldRetrievers();
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

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     * 
     * @return string representation of school year
     */
    private String getCurentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Register custom field Retrievers.
     */
    private void initFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(RetrieveSchoolYear.CALC_ID, new RetrieveSchoolYear());
        calcs.put(RetrieveDayTypeCount.CALC_ID, new RetrieveDayTypeCount());
        calcs.put(RetrieveInstructionalDays.CALC_ID, new RetrieveInstructionalDays());
        super.addCalcs(calcs);
    }

    /**
     * Initialize criteria and query.
     */
    private void initializeCriteriaAndQuery() {
        X2Criteria distSchoolCriteria = new X2Criteria();
        distSchoolCriteria.addEqualTo(School.COL_SCHOOL_ID, DISTRICT_SCHOOL_CODE);
        SubQuery schoolSubQuery = new SubQuery(School.class, X2BaseBean.COL_OID, distSchoolCriteria);

        X2Criteria distrCalCriteria = new X2Criteria();
        distrCalCriteria.addIn(SchoolCalendar.COL_SCHOOL_OID, schoolSubQuery);
        if (m_calendarNumbers != null) {
            distrCalCriteria.addIn(m_fieldCalendarNumber, m_calendarNumbers);
        }
        distrCalCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_contextOid);

        QueryByCriteria calendarQuery = new QueryByCriteria(SchoolCalendar.class, distrCalCriteria, true);
        setQuery(calendarQuery);
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_SchoolYear = getCurentSchoolYear();
        m_fieldCalendarNumber = translateAliasToJavaName(ALIAS_CALENDAR_NUMBER, true);
        m_fieldEvent2 = translateAliasToJavaName(ALIAS_EVENT_TYPE2, true);
        m_fieldEvent3 = translateAliasToJavaName(ALIAS_EVENT_TYPE3, true);
        m_fieldScheduledDays = translateAliasToJavaName(ALIAS_SCHEDULED_DAYS, true);
        m_fieldSiStockpile = translateAliasToJavaName(ALIAS_DISTRICT_SI_STOCKPILE, true);
        m_fieldSpStockpile = translateAliasToJavaName(ALIAS_DISTRICT_SP_STOCKPILE, true);

        m_eventRules = new ArrayList<String>();
        String eventRules = (String) getParameter(INPUT_EVENT_RULES);
        if (!StringUtils.isEmpty(eventRules)) {
            m_eventRules.addAll(StringUtils.convertDelimitedStringToList(eventRules, EVENT_RULES_SEPARATOR));
        }
    }

}
