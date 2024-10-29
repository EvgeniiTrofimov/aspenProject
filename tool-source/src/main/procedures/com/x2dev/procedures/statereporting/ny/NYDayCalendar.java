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
package com.x2dev.procedures.statereporting.ny;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.SchoolCalendar;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * New York state procedure for Attendance Codes export
 * This procedure has the Organization table as the root table because we have to report all the
 * attendance codes that are used by the district. Please check with Mark Wiley for further
 * questions.
 *
 * @author X2 Development Corporation
 */
public class NYDayCalendar extends StateReportData {
    /**
     * Entity class for Attendance Codes export.
     *
     * @author X2 Development Corporation
     */

    public static class NYDayCalendarEntity extends StateReportEntity {
        /**
         * Local variables for reporting information.
         */
        SchoolCalendarDate[] m_dates;
        NYDayCalendar m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public NYDayCalendarEntity() {
            // no argument constructor
        }

        /**
         * Initialize the data module.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            m_data = (NYDayCalendar) data;
            SchoolCalendar calendar = (SchoolCalendar) bean;

            Collection<SchoolCalendarDate> dates = calendar.getSchoolCalendarDates();
            m_dates = new SchoolCalendarDate[m_data.m_numDays];
            Iterator<SchoolCalendarDate> iterator = dates.iterator();
            while (iterator.hasNext()) {
                SchoolCalendarDate date = iterator.next();
                Calendar cal = Calendar.getInstance();
                cal.setTime(date.getDate());

                int index = m_data.dayIndex(cal.getTimeInMillis());
                m_dates[index] = date;

            }

            // There are 365 elements in the array, but the calendar does not fill them. This then
            // allows us to fill in all of the gaps in the calendar to report 365 rows.
            setRowCount(m_data.m_numDays);
        }

        /**
         * Will return null if this is not a day already in the calendar.
         *
         * @return School calendar date
         */
        public SchoolCalendarDate getCalendarDate() {
            return m_dates[getCurrentRow()];
        }

    }

    /**
     * Lookup the day exists in the calendar, otherwise sends out a generic one.
     */
    protected class RetrieveDayType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String dayType = null;
            NYDayCalendarEntity dcEntity = (NYDayCalendarEntity) entity;
            SchoolCalendarDate calDate = dcEntity.getCalendarDate();
            if (calDate != null) {
                dayType = calDate.getScheduleDayType();
                dayType = lookupStateValue(SchoolCalendarDate.class, SchoolCalendarDate.COL_SCHEDULE_DAY_TYPE, dayType);
                if (StringUtils.isEmpty(dayType)) {
                    if (calDate.getInSessionIndicator()) {
                        dayType = "Instructional day";
                    }
                }
            }
            if (StringUtils.isEmpty(dayType)) {
                dayType = "Other";
            }
            return dayType;
        }
    }

    /**
     * Retrieve the Calendar Date.
     */
    protected class RetrieveDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            NYDayCalendarEntity dcEntity = (NYDayCalendarEntity) entity;
            SchoolCalendarDate calDate = dcEntity.getCalendarDate();
            if (calDate != null) {
                return calDate.getDate();
            }

            // Need to change this so that we get the current year, then get the next year if we go
            // over the first day of year offset
            Calendar cal = Calendar.getInstance();
            cal.setTime(dcEntity.m_data.m_firstDayOfYear.getTime());
            cal.add(Calendar.DATE, dcEntity.getCurrentRow());

            return getPrintableDateFromCalendar(cal);
        }
    }

    /**
     * The Class ValidateDateType.
     */
    private class ValidateDateType implements FieldValidator {

        private static final String VAL_CALC_ID = "VAL_DAY_TYPE";
        protected List<String> validValues =
                new ArrayList<String>(Arrays.asList("Instructional day", "Teacher only day", "Holiday",
                        "Make-up day", "Weather day", "Late Arrv/Early Dism", "Emergency day", "Strike", "Other"));

        /**
         * Instantiates a new validate date type.
         */
        public ValidateDateType() {
            // TODO Auto-generated constructor stub
        }

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && !validValues.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " valid values are " + validValues,
                        field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }


    /**
     * The Class ValidateSchoolDate.
     */
    private class ValidateSchoolDate implements FieldValidator {
        private static final String DASH = "-";
        private static final String FIELD_LOCATION_GRADE_LEVEL = "Location Grade Level";
        private static final String FIELD_SCHOOL_DATE = "School Date";
        private static final String FIELD_LOCATION_CODE = "Location Code";
        private static final String FIELD_DISTRICT_CODE = "District Code";
        private static final String FIELD_SCHOOL_YEAR_DATE = "School Year Date";
        private static final String VAL_CALC_ID = "VAL_SCHOOL_DATE";
        protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        private List<String> m_uniqueRecords = new ArrayList<String>();

        /**
         * Instantiates a new validate school date.
         */
        public ValidateSchoolDate() {
            // TODO Auto-generated constructor stub
        }


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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            StringBuilder builder = new StringBuilder();
            builder.append(entity.getFieldValue(FIELD_DISTRICT_CODE) + DASH);
            builder.append(entity.getFieldValue(FIELD_LOCATION_CODE) + DASH);
            builder.append(entity.getFieldValue(FIELD_SCHOOL_YEAR_DATE) + DASH);
            builder.append(entity.getFieldValue(FIELD_SCHOOL_DATE) + DASH);
            builder.append(entity.getFieldValue(FIELD_LOCATION_GRADE_LEVEL));
            String key = builder.toString();
            if (m_uniqueRecords.contains(key)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Duplicate Calendar record found for the same date, location and State grade level - ", key));
            } else {
                m_uniqueRecords.add(key);
            }

            try {
                Calendar schoolDateCalendar = getCalendarDate(value);
                Integer selectedYear = getSelectedSchoolYear();


                if (schoolDateCalendar != null && selectedYear != null) {
                    int endYear = selectedYear.intValue();
                    int startYear = endYear - 1;
                    PlainDate currentDate = new PlainDate(schoolDateCalendar.getTime());
                    PlainDate startDate = new PlainDate(getCalendarDate("" + startYear + "-07-01").getTime());
                    PlainDate endDate = new PlainDate(getCalendarDate("" + endYear + "-06-30").getTime());
                    if (currentDate.before(startDate) || currentDate.after(endDate)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId()
                                        + " must be in the July1 - June 30 time frame of the user selected school year ",
                                field.getFieldId() + STYLE_BOLD + m_dateFormat.format(schoolDateCalendar.getTime())
                                        + STYLE_END +
                                        " selected year " + STYLE_BOLD + endYear + STYLE_END));

                    }


                }
                Calendar schoolYearCalendar = getCalendarDate(entity.getFieldValue(FIELD_SCHOOL_YEAR_DATE));

                if (schoolYearCalendar != null && selectedYear != null) {
                    if (selectedYear.intValue() != schoolYearCalendar.get(Calendar.YEAR)) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId()
                                        + "must equal year that the user selects in the School Year dropdown box",
                                field.getFieldId() + STYLE_BOLD + m_dateFormat.format(schoolYearCalendar.getTime())
                                        + STYLE_END +
                                        " selected year " + STYLE_BOLD + selectedYear + STYLE_END));
                    }
                }

            } catch (ParseException e) {
                //
            }

            return errors;
        }


        /**
         * Gets the calendar date.
         *
         * @param date String
         * @return Calendar
         * @throws ParseException exception
         */
        private Calendar getCalendarDate(String date) throws ParseException {
            Calendar returnCalendar = null;
            if (!StringUtils.isEmpty(date)) {
                returnCalendar = Calendar.getInstance();
                returnCalendar.setTime(m_dateFormat.parse(date));
            }
            return returnCalendar;
        }

        /**
         * Gets the selected school year.
         *
         * @return Integer
         */
        private Integer getSelectedSchoolYear() {
            Integer schoolYear = null;
            try {
                String schoolYearOid = (String) getParameter(PARAM_SCHOOL_YEAR);
                DistrictSchoolYearContext context = null;
                if (!StringUtils.isEmpty(schoolYearOid)) {
                    context = (DistrictSchoolYearContext) getBroker()
                            .getBeanByOid(DistrictSchoolYearContext.class, schoolYearOid);

                    if (context != null) {
                        schoolYear = Integer.valueOf(context.getSchoolYear());
                    }
                }

            } catch (NumberFormatException nfe) {
                // Invalid number passed in
            }
            return schoolYear;
        }

    }

    /**
     * Print date from calendar.
     *
     * @param cal Calendar
     * @return String
     */
    protected String getPrintableDateFromCalendar(Calendar cal) {
        String date = "";
        Calendar currentCal = Calendar.getInstance();
        currentCal.setTimeInMillis(cal.getTimeInMillis());
        date += currentCal.get(Calendar.YEAR) + "-";
        date += currentCal.get(Calendar.MONTH) + 1 > 9 ? (currentCal.get(Calendar.MONTH) + 1) + "-"
                : "0" + (currentCal.get(Calendar.MONTH) + 1) + "-";
        date += currentCal.get(Calendar.DAY_OF_MONTH) > 9 ? currentCal.get(Calendar.DAY_OF_MONTH) + ""
                : "0" + currentCal.get(Calendar.DAY_OF_MONTH);

        return date;
    }

    /**
     * Local variables for reporting information.
     */

    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    protected static final String PARAM_REMOVE_HEADER = "removeHeader";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_SCHOOL_YEAR = "schoolYear";

    private static final long MILLIS_PER_DAY = 24 * 3600 * 1000;

    protected Calendar m_firstDayOfYear;
    protected int m_numDays;
    protected Integer m_schoolYear;
    protected boolean m_removeHeaderIndicator;
    protected boolean m_excludeSchoolIndicator;

    /**
     * Overrides getHeading Method. The reason is so that the user can decide if the header is
     * included or not at runtime.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        if (m_removeHeaderIndicator) {
            return null;
        }

        return super.getHeading();
    }

    /**
     * Initializes the data module.
     */
    @Override
    public void initialize() {
        initializeFields();

        m_firstDayOfYear = Calendar.getInstance();
        setFirstDayOfDistrict(m_firstDayOfYear);
        Calendar lastDayOfYear = Calendar.getInstance();
        setLastDayOfDistrict(lastDayOfYear);
        m_numDays = dayIndex(lastDayOfYear.getTimeInMillis()) + 1;

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Create Query
            X2Criteria calCriteria = getCalendarCriteria();
            QueryByCriteria query = new QueryByCriteria(SchoolCalendar.class, calCriteria);
            setQuery(query);

            setEntityClass(NYDayCalendarEntity.class);

            HashMap calcs = new HashMap<String, FieldRetriever>();
            HashMap validators = new HashMap<String, FieldRetriever>();
            calcs.put("CAL-DATE", new RetrieveDate());
            calcs.put("CAL-DAY", new RetrieveDayType());
            super.addCalcs(calcs);

            validators.put(ValidateDateType.VAL_CALC_ID, new ValidateDateType());
            validators.put(ValidateSchoolDate.VAL_CALC_ID, new ValidateSchoolDate());
            super.addValidators(validators);

        }
    }

    /**
     * Day index.
     *
     * @param time long
     * @return int
     */
    int dayIndex(long time) {
        long diff = time - m_firstDayOfYear.getTimeInMillis();
        return (int) Math.round(diff / ((double) MILLIS_PER_DAY));
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_removeHeaderIndicator = false;
        if (getParameter(PARAM_REMOVE_HEADER) != null) {
            m_removeHeaderIndicator = ((Boolean) getParameter(PARAM_REMOVE_HEADER)).booleanValue();
        }

        m_excludeSchoolIndicator = ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue();

        // int currentSchoolYear = getOrganization().getCurrentContext().getSchoolYear();
        // m_schoolYear = Integer.valueOf(currentSchoolYear - 1);
        Integer schoolYear = null;
        try {
            String schoolYearOid = (String) getParameter(PARAM_SCHOOL_YEAR);
            DistrictSchoolYearContext context = null;
            if (!StringUtils.isEmpty(schoolYearOid)) {
                context = (DistrictSchoolYearContext) getBroker()
                        .getBeanByOid(DistrictSchoolYearContext.class, schoolYearOid);

                if (context != null) {
                    schoolYear = Integer.valueOf(context.getSchoolYear());
                }
            }

        } catch (NumberFormatException nfe) {
            // Invalid number passed in
        }
        if (schoolYear != null) {
            m_schoolYear = Integer.valueOf(schoolYear.intValue() - 1);
        } else {
            m_schoolYear = Integer.valueOf(getOrganization().getCurrentContext().getSchoolYear());
        }
    }

    /**
     * Get Calendar Criteria
     *
     * return X2Criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getCalendarCriteria() {
        X2Criteria calCriteria = new X2Criteria();
        if (isSchoolContext()) {
            calCriteria.addEqualTo(SchoolCalendar.COL_SCHOOL_OID, getSchool().getOid());
        }
        if (m_excludeSchoolIndicator) {
            String schoolExclude = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
            calCriteria.addNotEqualTo(SchoolCalendar.REL_SCHOOL + ModelProperty.PATH_DELIMITER + schoolExclude,
                    BooleanAsStringConverter.TRUE);
        }

        calCriteria.addEqualTo(SchoolCalendar.REL_DISTRICT_CONTEXT + ModelProperty.PATH_DELIMITER
                + DistrictSchoolYearContext.COL_SCHOOL_YEAR, Integer.valueOf(m_schoolYear.intValue() + 1));

        return calCriteria;
    }

    /**
     * Externalized representation of the 07/01/(School Year).
     *
     * @param cal void
     */
    private void setFirstDayOfDistrict(Calendar cal) {
        cal.set(Calendar.YEAR, m_schoolYear.intValue());
        cal.set(Calendar.MONTH, 6);
        cal.set(Calendar.DAY_OF_MONTH, 1);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
    }

    /**
     * Externalized representation of the 06/30/(School Year + 1).
     *
     * @param cal void
     */
    private void setLastDayOfDistrict(Calendar cal) {
        cal.set(Calendar.YEAR, m_schoolYear.intValue() + 1);
        cal.set(Calendar.MONTH, 5);
        cal.set(Calendar.DAY_OF_MONTH, 30);
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
    }
}
