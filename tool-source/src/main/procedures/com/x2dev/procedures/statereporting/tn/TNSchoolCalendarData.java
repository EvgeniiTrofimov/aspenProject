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
package com.x2dev.procedures.statereporting.tn;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNSchoolCalendarData.TNInstructionalProgramRPEntity.ReportPeriod;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisSchoolCalendarDate;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * The Class TNSchoolCalendarData.
 */
/*
 * Export procedure Code Template
 */
public class TNSchoolCalendarData extends TNStateReportData {
    /**
     * Implementation of StateReportEntity to be used by the TNSchoolCalendar
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     */
    public static class TNInstructionalProgramICEntity extends TNStateReportEntity {
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TNInstructionalProgramICEntity() {} // Empty constructor.

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolCalendar schoolCalendar = (SchoolCalendar) getBean();
            String name = schoolCalendar.getSchool().getName() + "[" + schoolCalendar.getCalendarId() + "]";

            return name;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            TNSchoolCalendarData tnData = (TNSchoolCalendarData) data;
            tnData.addEntityRowsCount(getRowCount());
        }
    }

    /**
     * Implementation of StateReportEntity to be used by the TNSchoolCalendar
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     */
    public static class TNInstructionalProgramRPEntity extends TNStateReportEntity {

        /**
         * The Class ReportPeriod.
         */
        public static class ReportPeriod {
            protected PlainDate dateBegin;

            private PlainDate dateEnd;
            private String groupNum;

            /**
             * Instantiates a new report period.
             *
             * @param groupNum String
             * @param dateBegin PlainDate
             * @param dateEnd PlainDate
             */
            public ReportPeriod(String groupNum, PlainDate dateBegin, PlainDate dateEnd) {
                this.groupNum = groupNum;
                this.dateBegin = dateBegin;
                this.dateEnd = dateEnd;
            }

            /**
             * Gets the date begin.
             *
             * @return Plain date
             */
            public PlainDate getDateBegin() {
                return dateBegin;
            }

            /**
             * Gets the date end.
             *
             * @return Plain date
             */
            public PlainDate getDateEnd() {
                return dateEnd;
            }

            /**
             * Gets the group number.
             *
             * @return String
             */
            public String getGroupNumber() {
                return groupNum;
            }

            /**
             * Checks if is include date.
             *
             * @param checkDate PlainDate
             * @return true, if is include date
             */
            public boolean isIncludeDate(PlainDate checkDate) {
                if (!dateBegin.after(checkDate) && !dateEnd.before(checkDate)) {
                    return true;
                }
                return false;
            }
        }

        /**
         * Constants: Aliases, Fields, IDs, Parameters
         */
        public static final String KEY_DATE_BEGIN = "DATE_BAGIN";
        public static final String KEY_DATE_END = "DATE_END";
        public static final String KEY_GROUP_NUMBER = "GROUP_NUM";

        private TNSchoolCalendarData m_data;

        /**
         * Instance variables.
         */
        protected ArrayList<ReportPeriod> m_coveredPeriods = new ArrayList<ReportPeriod>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TNInstructionalProgramRPEntity() {} // Empty constructor.

        /**
         * Resets current ReportPeriod.
         *
         * @return Report period
         */
        public ReportPeriod getCurrentReportPeriod() {
            return m_coveredPeriods.get(getCurrentRow());
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
            String name = schoolCalendar.getSchool().getName() + "[" + schoolCalendar.getCalendarId() + "]";

            return name;
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
            m_data = (TNSchoolCalendarData) data;
            X2Criteria criteria = getCalDateCriteria();

            QueryByCriteria query = new QueryByCriteria(SisSchoolCalendarDate.class, criteria);
            query.addOrderBy(SisSchoolCalendarDate.COL_DATE, true);
            QueryIterator iter = data.getBroker().getIteratorByQuery(query);

            parsQueryCalendar(iter);

            setRowCount(m_coveredPeriods.size());

            TNSchoolCalendarData tnData = (TNSchoolCalendarData) data;
            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Function for building custom SisSchoolCalendarDate criteria.
         *
         * @return X 2 criteria
         */
        private X2Criteria getCalDateCriteria() {
            SchoolCalendar schoolCalendar = (SchoolCalendar) getBean();
            X2Criteria criteria = new X2Criteria();
            criteria.addEqualTo(SisSchoolCalendarDate.COL_SCHOOL_CALENDAR_OID, schoolCalendar.getOid());
            criteria.addEqualTo(SisSchoolCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.TRUE);
            return criteria;
        }

        /**
         * Parsers QueryIterator and add ReportPeriod in list if this Period is covered by calendar
         * dates.
         *
         * @param iter QueryIterator
         */
        private void parsQueryCalendar(QueryIterator iter) {
            try {
                int periodCounter = 1;
                int dayCounter = 1;
                PlainDate periodStartDate = null;
                while (iter.hasNext()) {
                    SisSchoolCalendarDate schoolCalendarDate = (SisSchoolCalendarDate) iter.next();
                    if (dayCounter == 1) {
                        periodStartDate = schoolCalendarDate.getDate();
                    } else if (dayCounter == REPORT_PERIOD_LENGTH || !iter.hasNext()) {
                        m_coveredPeriods.add(new ReportPeriod(String.valueOf(periodCounter), periodStartDate,
                                schoolCalendarDate.getDate()));
                        if (m_data.m_maxPeriod != null && periodCounter == m_data.m_maxPeriod.intValue()) {
                            break;
                        }
                        periodCounter++;
                        dayCounter = 1;
                        continue;
                    }
                    dayCounter++;
                }
            } finally {
                iter.close();
            }
        }
    }

    /**
     * Implementation of StateReportEntity to be used by the TNSchoolCalendar
     * export. This must be a public static inner class with a public no
     * argument constructor so it can be instantiated through reflection.
     */
    public static class TNSchoolDaysEntity extends TNStateReportEntity {
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
        private List<EventTypeDuration> m_dateTypes;
        private SchoolCalendarDate m_schoolCalendarDate;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public TNSchoolDaysEntity() {} // Empty constructor.

        /**
         * Gets the current date type.
         *
         * @return Event type duration
         */
        public EventTypeDuration getCurrentDateType() {
            return m_dateTypes.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SchoolCalendar schoolCalendar = ((SchoolCalendarDate) getBean()).getSchoolCalendar();
            String entityName = schoolCalendar.getSchool().getName() + "[" + schoolCalendar.getCalendarId() + "]";

            if (getSchoolCalendarDate() != null) {
                entityName += "\nSchoolCalendarDate: " + getSchoolCalendarDate().getDate();
            }
            if (getCurrentDateType() != null) {
                entityName += "\nEventType: " + getCurrentDateType().getEventType();
            }

            return entityName;
        }

        /**
         * Gets the school calendar date.
         *
         * @return School calendar date
         */
        public SchoolCalendarDate getSchoolCalendarDate() {
            return m_schoolCalendarDate;
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
            m_dateTypes = new ArrayList<EventTypeDuration>();
            setRowCount(0);
            m_schoolCalendarDate = (SchoolCalendarDate) getBean();

            String eventTypeCode1 = getRefCode(data, m_schoolCalendarDate.getScheduleDayType());
            String eventTypeCode2 = getRefCodeThroughSklCalDate(data, m_schoolCalendarDate, ALIAS_INST_DAYTYPE2);
            String eventTypeCode3 = getRefCodeThroughSklCalDate(data, m_schoolCalendarDate, ALIAS_INST_DAYTYPE3);
            String eventTypeCode4 = getRefCodeThroughSklCalDate(data, m_schoolCalendarDate, ALIAS_INST_DAYTYPE4);

            if (eventTypeCode1 != null) {
                setRowCount(getRowCount() + 1);
                m_dateTypes.add(parsRefCode(eventTypeCode1));
            }

            if (eventTypeCode2 != null) {
                setRowCount(getRowCount() + 1);
                m_dateTypes.add(parsRefCode(eventTypeCode2));
            }

            if (eventTypeCode3 != null) {
                setRowCount(getRowCount() + 1);
                m_dateTypes.add(parsRefCode(eventTypeCode3));
            }

            if (eventTypeCode4 != null) {
                setRowCount(getRowCount() + 1);
                m_dateTypes.add(parsRefCode(eventTypeCode4));
            }
            TNSchoolCalendarData tnData = (TNSchoolCalendarData) data;
            tnData.addEntityRowsCount(getRowCount());
        }

        /**
         * Gets the ref code.
         *
         * @param data StateReportData
         * @param value String
         * @return String
         */
        private String getRefCode(StateReportData data, String value) {
            if (value == null) {
                return null;
            }

            String ret = data.lookupReferenceCodeByBeanPath(SisSchoolCalendarDate.class,
                    SisSchoolCalendarDate.COL_SCHEDULE_DAY_TYPE,
                    value, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            return ret;
        }

        /**
         * Gets the ref code through skl cal date.
         *
         * @param data StateReportData
         * @param sklCalDate SchoolCalendarDate
         * @param alias String
         * @return String
         */
        private String getRefCodeThroughSklCalDate(StateReportData data, SchoolCalendarDate sklCalDate, String alias) {
            if (sklCalDate == null) {
                return null;
            }

            String eventType = (String) sklCalDate.getFieldValueByAlias(alias);

            return getRefCode(data, eventType);
        }

        /**
         * Parser for code. code sample: AD-0033
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
     * Field retriever for Block Schedule field.
     */
    protected class RetrieveBlockSchedule implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SchoolCalendar schoolCalendar = (SchoolCalendar) entity.getBean();
            Object o = schoolCalendar.getFieldValueByAlias("DOE SPRING BLOCK DATE");
            return (o == null || ((String) o).equalsIgnoreCase("")) ? "N" : "Y";
        }

    }

    /**
     * Field retriever for EVENT DURATION field.
     */
    protected class RetrieveEventDuration implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNSchoolDaysEntity seEntity = (TNSchoolDaysEntity) entity;
            return seEntity.getCurrentDateType().getEventDuration();
        }
    }

    /**
     * Field retriever for EVENT TYPE field.
     */
    protected class RetrieveEventType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNSchoolDaysEntity seEntity = (TNSchoolDaysEntity) entity;
            return seEntity.getCurrentDateType().getEventType();
        }
    }

    /**
     * Field retriever for Report Period Begin Date field.
     */
    protected class RetrieveRepPerBeginDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNInstructionalProgramRPEntity seEntity = (TNInstructionalProgramRPEntity) entity;
            ReportPeriod rp = seEntity.getCurrentReportPeriod();
            if (rp != null) {
                return rp.getDateBegin();
            }
            return null;
        }
    }

    /**
     * Field retriever for Report Period End Date field.
     */
    protected class RetrieveRepPerEndDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNInstructionalProgramRPEntity seEntity = (TNInstructionalProgramRPEntity) entity;
            ReportPeriod rp = seEntity.getCurrentReportPeriod();
            if (rp != null) {
                return rp.getDateEnd();
            }
            return null;
        }
    }

    /**
     * Field retriever for Report Period Number field.
     */
    protected class RetrieveRepPerNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNInstructionalProgramRPEntity seEntity = (TNInstructionalProgramRPEntity) entity;
            ReportPeriod rp = seEntity.getCurrentReportPeriod();
            if (rp != null) {
                return rp.getGroupNumber();
            }
            return null;
        }
    }

    /**
     * Field retriever for SCHOOL DAY TYPE field.
     */
    protected class RetrieveSchoolDayType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SchoolCalendarDate sklCalDate = (SchoolCalendarDate) entity.getBean();
            return (sklCalDate == null) ? null : ((sklCalDate.getInSessionIndicator()) ? "I" : "N");
        }
    }

    /**
     * Field retriever for School Year field.
     */
    protected class RetrieveSchoolYear implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNSchoolCalendarData seData = (TNSchoolCalendarData) data;
            return seData.m_schoolYear;
        }
    }

    /**
     * Validate Instructional Program Number, not to be equal "00".
     * It is default value set then Instructional Program Number is missed.
     */
    protected class ValidateInstrpgmNumber implements FieldValidator {
        public static final String VAL_ID = "VAL_INSTRPGM";
        private static final String INSTPGM_EMPTY_ERROR = "Instructional Program is empty.";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection errors = new ArrayList<StateReportValidationError>();
            SchoolCalendar schoolCalendar = null;
            if (entity.getBean() instanceof SchoolCalendar) {
                schoolCalendar = (SchoolCalendar) entity.getBean();
            } else if (entity.getBean() instanceof SchoolCalendarDate) {
                schoolCalendar = ((SchoolCalendarDate) entity.getBean()).getSchoolCalendar();
            }
            if (schoolCalendar != null) {
                String casInstrPgmNum = (String) schoolCalendar.getFieldValueByBeanPath(m_fieldCasInstPgm);
                if (StringUtils.isEmpty(casInstrPgmNum)) {
                    TNStateReportData tnData = (TNStateReportData) data;
                    String message =
                            "Value not specified. Set to " + field.getDefaultValue() + " by default. Context Id:" +
                                    tnData.getCurrentContext().getContextId() + ". School:" +
                                    schoolCalendar.getSchool().getName() +
                                    ". Calendar code:" + schoolCalendar.getCalendarId();
                    StateReportValidationError error =
                            new StateReportValidationError(entity, field, INSTPGM_EMPTY_ERROR, message);
                    errors.add(error);
                }
            }
            return errors;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_CAS_INSTR_PGM = "DOE INSTRUCTIONAL PROGRAM";
    protected static final String ALIAS_GRADUATION = "DOE GRADUATION";
    protected static final String ALIAS_INST_CAL = "DOE INSTRUCTIONAL CALENDAR";
    protected static final String ALIAS_INST_DAYTYPE2 = "DOE DAY EVENT TYPE 2";
    protected static final String ALIAS_INST_DAYTYPE3 = "DOE DAY EVENT TYPE 3";
    protected static final String ALIAS_INST_DAYTYPE4 = "all-csd-CalendarEventType4";
    protected static final String ALIAS_SCHOOL_STATE_ID = "DOE SCHOOL STATE ID";

    protected static final String CALC_ID_CAS_CALC_BLOCKSCHL = "CAS_CALC_BLOCKSCHL";
    protected static final String CALC_ID_CASP_CALC_REPPERBGN = "CASP_CALC_REPPERBGN";
    protected static final String CALC_ID_CASP_CALC_REPPEREND = "CASP_CALC_REPPEREND";
    protected static final String CALC_ID_CASP_CALC_REPPERNUM = "CASP_CALC_REPPERNUM";
    protected static final String CALC_ID_CSD_CALC_EVENTDUR = "CSD_CALC_EVENTDUR";
    protected static final String CALC_ID_CSD_CALC_EVENTTYPE = "CSD_CALC_EVENTTYPE";
    protected static final String CALC_ID_CSD_CALC_SKLDAYTYPE = "CSD_CALC_SKLDAYTYPE";
    protected static final String CALC_ID_SCHOOLYEAR = "CAS_CALC_SCHOOLYEAR";

    private static final String DISTRICT_LEVEL_RECORD = "9999";

    private static final String PARAM_MAX_PERIODS_ALLOWED = "maxPeriodsAllowed";
    protected static final int REPORT_PERIOD_LENGTH = 20;

    /**
     * Entity instance variables.
     */
    protected String m_fieldCasInstPgm;
    protected String m_fieldSchoolStateId;
    protected String m_fieldInstDayType2 = null;
    protected String m_fieldInstDayType3 = null;
    protected String m_fieldInstDayType4 = null;
    protected Integer m_maxPeriod;
    protected String m_programCode;
    protected String m_schoolYear;

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

        initializeFields();

        if (getSetupErrors().size() != 0) {
            return;
        }

        initializeCriteriaAndQuery();
        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Gets the cas oids.
     *
     * @return oids of selected calendar schools.
     */
    private Collection<String> getCasOids() {
        String casOidsParamValue = (String) getParameter(PARAM_SCHOOL_CALENDARS);
        Collection<String> casOidsCollection = null;
        if (casOidsParamValue != null) {
            casOidsCollection = Arrays.asList(casOidsParamValue.split(","));
        }
        return casOidsCollection;
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
     * Gets the school calendar criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getSchoolCalendarCriteria() {
        X2Criteria schoolCriteria = new X2Criteria();

        schoolCriteria.addEqualTo(SchoolCalendar.COL_DISTRICT_CONTEXT_OID, m_contextOid);
        if (getCasOids() != null) {
            schoolCriteria.addIn(X2BaseBean.COL_OID, getCasOids());
        }

        if (isSchoolContext()) {
            schoolCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            schoolCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        schoolCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + m_fieldSchoolStateId,
                DISTRICT_LEVEL_RECORD);
        schoolCriteria.addNotEmpty(SchoolCalendar.REL_SCHOOL + PATH_DELIMITER + m_fieldSchoolStateId,
                getBroker().getPersistenceKey());

        applyInputCriteria(schoolCriteria, false, null);

        return schoolCriteria;
    }

    /**
     * Gets the school calendar date criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getSchoolCalendarDateCriteria() {

        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addIn(SchoolCalendarDate.COL_SCHOOL_CALENDAR_OID,
                new SubQuery(SchoolCalendar.class, X2BaseBean.COL_OID, getSchoolCalendarCriteria()));

        X2Criteria orcriteria = new X2Criteria();
        orcriteria.addNotEmpty(SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, getBroker().getPersistenceKey());
        X2Criteria orcriteria1 = new X2Criteria();
        orcriteria1.addNotEmpty(m_fieldInstDayType2, getBroker().getPersistenceKey());
        X2Criteria orcriteria2 = new X2Criteria();
        orcriteria2.addNotEmpty(m_fieldInstDayType3, getBroker().getPersistenceKey());
        orcriteria.addOrCriteria(orcriteria1);
        orcriteria.addOrCriteria(orcriteria2);
        schoolCriteria.addAndCriteria(orcriteria);

        return schoolCriteria;
    }

    /**
     * Initialize criteria and query.
     */
    private void initializeCriteriaAndQuery() {
        X2Criteria schoolCalendarCriteria = getSchoolCalendarCriteria();
        QueryByCriteria schoolCalendarQuery = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria, true);
        setQuery(schoolCalendarQuery);

        if (m_programCode.equalsIgnoreCase("020")) {
            initializeSchoolCalendarCriteriaAndQuery();
            setEntityClass(TNInstructionalProgramICEntity.class);
        }

        if (m_programCode.equalsIgnoreCase("021")) {
            initializeSchoolCalendarCriteriaAndQuery();
            setEntityClass(TNInstructionalProgramRPEntity.class);
        }

        if (m_programCode.equalsIgnoreCase("022")) {
            initializeSchoolCalendarDateCriteriaAndQuery();
            setEntityClass(TNSchoolDaysEntity.class);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_fieldSchoolStateId = translateAliasToJavaName(ALIAS_SCHOOL_STATE_ID, true);
        m_fieldCasInstPgm = translateAliasToJavaName(ALIAS_CAS_INSTR_PGM, true);
        m_fieldInstDayType2 = translateAliasToJavaName(ALIAS_INST_DAYTYPE2, true);
        m_fieldInstDayType3 = translateAliasToJavaName(ALIAS_INST_DAYTYPE3, true);
        m_fieldInstDayType4 = translateAliasToJavaName(ALIAS_INST_DAYTYPE4, true);
        m_programCode = (String) getParameter(PARAM_PROGRAM_CODE);
        m_maxPeriod = (Integer) getParameter(PARAM_MAX_PERIODS_ALLOWED);
        m_schoolYear = getCurentSchoolYear();
    }

    /**
     * Initialize school calendar criteria and query.
     */
    private void initializeSchoolCalendarCriteriaAndQuery() {
        X2Criteria schoolCalendarCriteria = getSchoolCalendarCriteria();
        QueryByCriteria schoolCalendarQuery = new QueryByCriteria(SchoolCalendar.class, schoolCalendarCriteria, true);
        setQuery(schoolCalendarQuery);
    }

    /**
     * Initialize school calendar date criteria and query.
     */
    private void initializeSchoolCalendarDateCriteriaAndQuery() {
        X2Criteria schoolCalendarDateCriteria = getSchoolCalendarDateCriteria();
        QueryByCriteria schoolCalendarDateQuery =
                new QueryByCriteria(SchoolCalendarDate.class, schoolCalendarDateCriteria, true);
        schoolCalendarDateQuery.addOrderBy(SchoolCalendarDate.COL_DATE, true);
        setQuery(schoolCalendarDateQuery);
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();

        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());

        calcs.put(CALC_ID_CAS_CALC_BLOCKSCHL, new RetrieveBlockSchedule());
        calcs.put(CALC_ID_CASP_CALC_REPPEREND, new RetrieveRepPerEndDate());
        calcs.put(CALC_ID_CASP_CALC_REPPERNUM, new RetrieveRepPerNumber());
        calcs.put(CALC_ID_CASP_CALC_REPPERBGN, new RetrieveRepPerBeginDate());
        calcs.put(CALC_ID_CSD_CALC_EVENTTYPE, new RetrieveEventType());
        calcs.put(CALC_ID_CSD_CALC_EVENTDUR, new RetrieveEventDuration());
        calcs.put(CALC_ID_CSD_CALC_SKLDAYTYPE, new RetrieveSchoolDayType());

        super.addCalcs(calcs);
    }

    /**
     * Register custom field Validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();

        validators.put(ValidateInstrpgmNumber.VAL_ID, new ValidateInstrpgmNumber());

        super.addValidators(validators);
    }
}
