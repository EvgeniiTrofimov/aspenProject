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
package com.x2dev.procedures.statereporting.ma;

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DistrictCalendar;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.StaffCalendar;
import com.follett.fsc.core.k12.beans.StaffCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.exports.ExportJavaSource;
import com.follett.fsc.core.k12.tools.reports.ReportUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.ConductIncident;
import com.x2dev.sis.model.beans.RefAttendanceStaff;
import com.x2dev.sis.model.beans.SisDistrictSchoolYearContext;
import com.x2dev.sis.model.beans.SisPerson;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.sis.model.beans.StaffDegree;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.sis.model.business.CalendarManager;
import com.x2dev.utils.CollectionUtils;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Massachusetts state report for EPIMS Staff export.
 * This class implements the data export for Mass EPIMS staff export.
 *
 * @author X2 Development Corporation
 */
public class EpimsStaff extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the EPIMS Staff export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class EPIMSEntity extends StateReportEntity {
        /**
         * Degrees held by the staff member.
         */
        String[] m_degreeValues;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public EPIMSEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Filter staff if the hire date is empty.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;
            SisStaff staffMember = (SisStaff) getBean();
            if (staffMember.getHireDate() == null) {
                FieldDefinition field = getData().getFieldDefinition(SR_11_HIRE_DATE);
                error = new StateReportValidationError(this, field, "Hire date is empty", "");
            }
            return error;
        }

        /**
         * Return an identifying name for this entity for reporting purposes.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStaff staff = (SisStaff) getBean();

            String name = staff.getNameView() +
                    " [ID: " + staff.getLocalId() +
                    ", MEPID: " + staff.getStateId() +
                    (getData().isSchoolContext() ? ", SCHOOL: " + staff.getSchool().getName() : "") +
                    "]";
            return name;
        }

        /**
         * Get degree values stored by the retriever.
         *
         * @return String[]
         */
        public String[] getDegreeValues() {
            return m_degreeValues;
        }

        /**
         * If the update parameter is set, save some calculated values back to the staff record.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            Boolean updateRecords = (Boolean) getData().getParameter(PARAM_UPDATE_RECORDS);
            Boolean includeAttendance = (Boolean) getData().getParameter(PARAM_INCLUDE_ATTENDANCE);
            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (updateRecords != null && updateRecords.booleanValue()) {
                try {
                    FieldDefinition field = getData().getFieldDefinition(SR_08_RACE);
                    PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(SR_08_RACE));

                    /*
                     * If the include attendance flag is set, the field definitions will have been
                     * loaded
                     * and we can save the values.
                     */
                    if (includeAttendance != null && includeAttendance.booleanValue()) {
                        field = getData().getFieldDefinition(SR_36_DAYS_PRESENT);
                        PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(SR_36_DAYS_PRESENT));

                        field = getData().getFieldDefinition(SR_37_DAYS_EXPECTED);
                        PropertyUtils.setProperty(getBean(), field.getBeanPath(), getFieldValue(SR_37_DAYS_EXPECTED));
                    }

                    if (getBean().isDirty()) {
                        getData().getBroker().saveBeanForced(getBean());
                    }
                } catch (IllegalAccessException e) {
                    // conversion errors. Cannot save staff.
                } catch (InvocationTargetException e) {
                    // conversion errors. Cannot save staff.
                } catch (NoSuchMethodException e) {
                    // conversion errors. Cannot save staff.
                }
            }
        }

        /**
         * Set staff degree values calculated by retriever.
         *
         * @param degreeValues void
         */
        public void setDegreeValues(String[] degreeValues) {
            m_degreeValues = degreeValues;
        }
    }

    /*
     * SR field alias constants.
     */
    protected static final String SR_01_MEPID = "SR01";
    protected static final String SR_02_LOCALID = "SR02";
    protected static final String SR_03_LICENSE = "SR03";
    protected static final String SR_04_FIRST_NAME = "SR04";
    protected static final String SR_05_MIDDLE_NAME = "SR05";
    protected static final String SR_06_LAST_NAME = "SR06";
    protected static final String SR_07_BIRTH_DATE = "SR07";
    protected static final String SR_08_RACE = "SR08";
    protected static final String SR_09_STATUS = "SR09";
    protected static final String SR_10_EXIT_REASON = "SR10";
    protected static final String SR_11_HIRE_DATE = "SR11";
    protected static final String SR_12_SALARY_SOURCE_1 = "SR12";
    protected static final String SR_13_PERCENT_SALARY_SOURCE_1 = "SR13";
    protected static final String SR_14_SALARY_SOURCE_2 = "SR14";
    protected static final String SR_15_PERCENT_SALARY_SOURCE_2 = "SR15";
    protected static final String SR_16_SALARY_SOURCE_3 = "SR16";
    protected static final String SR_17_PERCENT_SALARY_SOURCE_3 = "SR17";
    protected static final String SR_18_DEGREE_TYPE_1 = "SR18";
    protected static final String SR_19_DEGREE_INST_1 = "SR19";
    protected static final String SR_20_DEGREE_SUBJECT_1 = "SR20";
    protected static final String SR_21_DEGREE_TYPE_2 = "SR21";
    protected static final String SR_22_DEGREE_INST_2 = "SR22";
    protected static final String SR_23_DEGREE_SUBJECT_2 = "SR23";
    protected static final String SR_24_DEGREE_TYPE_3 = "SR24";
    protected static final String SR_25_DEGREE_INST_3 = "SR25";
    protected static final String SR_26_DEGREE_SUBJECT_3 = "SR26";
    protected static final String SR_27_EXIT_DATE = "SR27";
    protected static final String SR_28_PROFESSIONAL_STATUS = "SR28";
    protected static final String SR_29_OVERALL_EVALUATION = "SR29";
    protected static final String SR_30_STANDARD_EVALUATION_1 = "SR30";
    protected static final String SR_31_STANDARD_EVALUATION_2 = "SR31";
    protected static final String SR_32_STANDARD_EVALUATION_3 = "SR32";
    protected static final String SR_33_STANDARD_EVALUATION_4 = "SR33";
    protected static final String SR_34_IMPACT_ON_STUDENT = "SR34";
    protected static final String SR_35_EDUCATOR_EVALUATION_PLAN = "SR35-STF";
    protected static final String SR_36_DAYS_PRESENT = "SR36-STF";
    protected static final String SR_36_ADJUSTED_DAYS_PRESENT = "SR36-STF ADJUSTED";
    protected static final String SR_37_DAYS_EXPECTED = "SR37-STF";
    protected static final String SR_37_ADJUSTED_DAYS_EXPECTED = "SR37-STF ADJUSTED";
    protected static final String SR_38_BEGINNER_EDUCATOR_IND = "SR38-STF";
    private static final String ALIAS_SKL_SIF_DISTRICT_ID = "skl-sif-district-id";
    private static final String SEPARATOR_COMMA = ",";

    // ---------------------------------------------------------------------------------------------
    // --------------------------------------------------------------------- X2-specific code below
    // ---------------------------------------------------------------------------------------------

    /**
     * Name for the "calculate totals" parameter. The value is a Boolean.
     */
    public static final String PARAM_CALCULATE_TOTALS = "calculateTotals";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String PARAM_EXIT_AFTER_DATE = "exitAfterDate";

    /**
     * Name for the "include ratings" parameter. The value is a Boolean.
     */
    public static final String PARAM_INCLUDE_RATINGS = "includeRatings";

    /**
     * Name for the "include attendance" parameter. The value is a Boolean.
     */
    public static final String PARAM_INCLUDE_ATTENDANCE = "includeAttendance";

    /**
     * Name for the report date parameter. The corresponding values is a PlainDate object.
     */
    public static final String PARAM_REPORT_DATE = "reportDate";

    /**
     * Name for the enumerated "selection" parameter. The value is an Integer.
     */
    public static final String PARAM_QUERY_BY = "queryBy";

    /**
     * Name for the "selection value" parameter. The value is an String.
     */
    public static final String PARAM_QUERY_STRING = "queryString";

    /**
     * Name for the enumerated "sort" parameter. The value is an Integer.
     */
    public static final String PARAM_SORT = "sort";

    /**
     * Name for the "update staff records" parameter. The value is a Boolean.
     */
    public static final String PARAM_UPDATE_RECORDS = "updateRecords";

    /**
     * SR09 "Exited" state code
     */
    public static final String SR09_EXITED_CODE = "04";

    /*
     * Field alias for export header
     */
    protected static final String DOE_DISTRICT_ID = "DOE District ID";

    public static final String PARAM_INCLUDE_SIF_SCHOOL = "includeSifSchoolId";


    /*
     * The DOE uses an arbitrary code for its combinations of races / ethnicities. X2 instead uses
     * a bitmap with the following values:
     *
     * - WHITE: 1
     * - BLACK: 2
     * - ASIAN: 4
     * - AMERICAN INDIAN: 8
     * - PACIFIC ISLANDER: 16
     * - HISPANIC: 32 (this value is actually used as a bitmask by the DOE)
     *
     * The X2 bitmap is translated to a state code using the following array.
     *
     * (DOE values from http://www.doe.mass.edu/infoservices/data/guides/masscodes.html)
     */
    protected static final int[] STATE_RACE_CODES = new int[]
    // X2 Values: { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    // 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 }
    {0, 1, 2, 6, 3, 7, 10, 16, 4, 8, 11, 17, 13, 19, 23, 26, 5, 9, 12, 18, 14, 29, 22, 26, 15, 21, 24, 27, 25, 28, 30,
            31};

    /*
     * Other internal constants
     */
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String DATE_FORMAT = "MM/dd/yyyy";
    protected static final String NO_MIDDLE_NAME = "NMN";
    protected static final String REGEX_ALPHANUMERIC = "[A-Za-z0123456789]*";
    protected static final String REGEX_ALPHANUMERIC_HYPHEN = "[-A-Za-z0-9]*";
    protected static final String REGEX_NAME = "[-'. A-Za-z0123456789]*";
    protected static final String REGEX_MNAME = "[-' A-Za-z0123456789]*";
    protected static final String REGEX_NUMERIC = "[0123456789]*";
    protected static final String REGEX_NUMERIC_DECIMAL = "[.0123456789]*";
    protected static final String REF_CODE_STAFF_ATTENDANCE_REASONS = "rtbSfaReason";
    protected static final String REF_CODE_STAFF_LEAVE_REASONS = "rtbLeaveReason";
    protected static final String STAFF_ATTENDANCE_CODE_ABSENT = "A";
    protected static final String STAFF_ATTENDANCE_REASON_00 = "00";
    protected static final String STAFF_ATTENDANCE_REASON_01 = "01";
    protected static final String STAFF_ATTENDANCE_REASON_02 = "02";

    protected boolean m_calculateTotals;
    protected DateFormat m_dateFormat;
    protected DecimalFormat m_decimalFormat;
    protected Map<String, Collection<StaffDegree>> m_degreesMap;
    protected PlainDate m_exitAfterDate;
    protected String m_fieldExitDate;
    protected Pattern m_illegalNameCharacters;
    protected Boolean m_includeAttendance;
    protected Boolean m_includeRatings;
    protected Map<String, Collection<StaffCertification>> m_licensesMap;
    protected Map<String, Collection<Race>> m_raceCodeMap;
    protected PlainDate m_reportDate;
    protected Date m_reportDateMax;
    protected Date m_reportDateMin;
    protected boolean m_updateRecords;
    protected Collection<DistrictCalendar> m_districtCalendarDateList = new ArrayList<DistrictCalendar>();
    protected Map<String, String> m_calendarIdMap = new HashMap<String, String>();
    protected Map<String, Collection<StaffCalendarDate>> m_calendarDateMap =
            new HashMap<String, Collection<StaffCalendarDate>>();
    protected Map<String, Collection<StaffAttendance>> m_attendanceList =
            new HashMap<String, Collection<StaffAttendance>>();
    protected Map<String, Collection<StaffLeave>> m_leaveList = new HashMap<String, Collection<StaffLeave>>();
    protected Map<String, RefAttendanceStaff> m_refAttMap;
    protected Map<String, String> m_staffAttReasonsCodesMap = new HashMap<String, String>();
    protected Map<String, String> m_staffLeaveReasonsCodesMap = new HashMap<String, String>();
    private String m_sklDstrIdField = null;
    private List<String> m_includeSifSchoolIds = null;

    /**
     * Retrieves the beginner educator indicator.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveBegginerInd implements FieldRetriever {
        private static final String YES = "01";
        private static final String NO = "00";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            Boolean value = (Boolean) data.getPropertyAsJavaType(entity.getBean(), field.getBeanPath());

            return value != null && value.booleanValue() ? YES : NO;
        }
    }

    /**
     * Retrieve a Staff's Days Expected to be in Attendance.
     * 
     * @author X2 Development Corporation
     */
    protected class RetrieveDaysExpected implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            SisStaff staff = (SisStaff) entity.getBean();
            String staffOid = staff.getOid();

            String value = (String) staff.getFieldValueByAlias(SR_37_ADJUSTED_DAYS_EXPECTED);

            // Check adjusted days expected value first. If it is null, empty or zero then take it
            // form days expected.
            if (!StringUtils.isEmpty(value) && StringUtils.isNumeric(value) && Integer.parseInt(value) > 0) {
                // Remove leading zeros
                int valueInt = Integer.parseInt(value);
                value = Integer.valueOf(valueInt).toString();
            } else {
                String calendarId = staff.getCalendarId();
                PlainDate staffHireDate = staff.getHireDate();
                PlainDate staffExitDate = null;
                String exitDateStr = (String) staff.getFieldValueByAlias(SR_27_EXIT_DATE);
                if (exitDateStr != null) {
                    staffExitDate = DateUtils.getDate(exitDateStr);
                }

                int daysExpected = 0;

                Collection<StaffAttendance> staffAttendances = m_attendanceList.get(staffOid);
                Map<PlainDate, StaffAttendance> staffAttendanceMap = new HashMap();
                if (staffAttendances != null) {
                    for (StaffAttendance item : staffAttendances) {
                        staffAttendanceMap.put(item.getDate(), item);
                    }
                }
                Collection<StaffLeave> staffLeaves = m_leaveList.get(staffOid);

                // If the Staff doesn't have a StaffCalendar assign then use the DistrictCalendar
                if (calendarId != null && m_calendarIdMap.get(calendarId) != null) {
                    String calendarOid = m_calendarIdMap.get(calendarId);
                    Collection<StaffCalendarDate> staffCalendarDates = m_calendarDateMap.get(calendarOid);

                    for (StaffCalendarDate staffCalendarDate : staffCalendarDates) {
                        PlainDate calendarDate = staffCalendarDate.getDate();

                        boolean include = includeDate(calendarDate, staffHireDate, staffExitDate, staffAttendanceMap,
                                staffLeaves, false);

                        if (include) {
                            daysExpected++;
                        }
                    }
                } else {
                    for (DistrictCalendar districtCalendar : m_districtCalendarDateList) {
                        PlainDate calendarDate = districtCalendar.getDate();

                        boolean include = includeDate(calendarDate, staffHireDate, staffExitDate, staffAttendanceMap,
                                staffLeaves, false);

                        if (include) {
                            daysExpected++;
                        }
                    }
                }

                value = Integer.valueOf(daysExpected).toString();
            }

            return value;
        }

    }

    /**
     * Retrieve a Staff's Days Present.
     * 
     * @author X2 Development Corporation
     */
    protected class RetrieveDaysPresent implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            SisStaff staff = (SisStaff) entity.getBean();

            String value = (String) staff.getFieldValueByAlias(SR_36_ADJUSTED_DAYS_PRESENT);

            // Check adjusted days present value first. If it is null, empty or zero then take it
            // form days present.
            if (!StringUtils.isEmpty(value) && StringUtils.isNumeric(value) && Integer.parseInt(value) > 0) {
                // Remove leading zeros
                int valueInt = Integer.parseInt(value);
                value = Integer.valueOf(valueInt).toString();
            } else {
                String staffOid = staff.getOid();
                String calendarId = staff.getCalendarId();
                PlainDate staffHireDate = staff.getHireDate();
                PlainDate staffExitDate = null;
                String exitDateStr = (String) staff.getFieldValueByAlias(SR_27_EXIT_DATE);
                if (exitDateStr != null) {
                    staffExitDate = DateUtils.getDate(exitDateStr);
                }
                int daysPresent = 0;

                Collection<StaffAttendance> staffAttendances = m_attendanceList.get(staffOid);
                Map<PlainDate, StaffAttendance> staffAttendanceMap = new HashMap();
                if (staffAttendances != null) {
                    for (StaffAttendance item : staffAttendances) {
                        staffAttendanceMap.put(item.getDate(), item);
                    }
                }
                Collection<StaffLeave> staffLeaves = m_leaveList.get(staffOid);

                // If the Staff doesn't have a StaffCalendar assign then use the DistrictCalendar
                if (calendarId != null && m_calendarIdMap.get(calendarId) != null) {
                    String calendarOid = m_calendarIdMap.get(calendarId);
                    Collection<StaffCalendarDate> staffCalendarDates = m_calendarDateMap.get(calendarOid);

                    for (StaffCalendarDate staffCalendarDate : staffCalendarDates) {
                        PlainDate calendarDate = staffCalendarDate.getDate();

                        boolean include = includeDate(calendarDate, staffHireDate, staffExitDate, staffAttendanceMap,
                                staffLeaves, true);

                        if (include) {
                            daysPresent++;
                        }
                    }
                } else {
                    for (DistrictCalendar districtCalendar : m_districtCalendarDateList) {
                        PlainDate calendarDate = districtCalendar.getDate();

                        boolean include = includeDate(calendarDate, staffHireDate, staffExitDate, staffAttendanceMap,
                                staffLeaves, true);

                        if (include) {
                            daysPresent++;
                        }
                    }
                }

                value = Integer.valueOf(daysPresent).toString();
            }

            return value;
        }

    }

    /**
     * Returns the 9 degrees values that should be exported for this staff member.
     * The nine values are three sets of:
     * (Type
     * ,Institution
     * ,Subject)
     * The fieldDefinition must select which value to use by including
     * an Integer(1) - Integer(9) in the FieldDefinition.object value.
     */
    protected class RetrieveDegreeValues implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            SisStaff staff = (SisStaff) entity.getBean();
            String value = null;
            String[] values = ((EPIMSEntity) entity).getDegreeValues();
            Integer index = (Integer) field.getParameter();
            String SR03 = entity.getFieldValue(SR_03_LICENSE);

            // If the values have not already been retrieved, get them
            // and store them in the entity.
            if (values == null) {
                values = new String[9];
                /*
                 * Initialize the array with the default values
                 */
                for (int i = 0; i < 3; i++) {
                    values[i * 3] = "000";
                    values[i * 3 + 1] = "0000";
                    values[i * 3 + 2] = "00";
                }

                /*
                 * Populate up to 3 degrees if the staff does not have an ELAR license.
                 */
                if (SR03 == null || SR03.equals("00") || SR03.equals("01")) {
                    Collection<StaffDegree> degrees = m_degreesMap.get(staff.getOid());
                    if (degrees != null) {
                        int i = 0;
                        Iterator iterator = degrees.iterator();
                        while (iterator.hasNext() && i < 3) {
                            StaffDegree degree = (StaffDegree) iterator.next();

                            values[i * 3] = lookupStateValue(StaffDegree.class, StaffDegree.COL_TYPE,
                                    (String) getProperty(degree, StaffDegree.COL_TYPE));
                            values[i * 3 + 1] = lookupStateValue(StaffDegree.class, StaffDegree.COL_INSTITUTION,
                                    (String) getProperty(degree, StaffDegree.COL_INSTITUTION));
                            values[i * 3 + 2] = lookupStateValue(StaffDegree.class, StaffDegree.COL_DEGREE,
                                    (String) getProperty(degree, StaffDegree.COL_DEGREE));

                            i++;
                        }
                    }
                }
                ((EPIMSEntity) entity).setDegreeValues(values);
            }

            // Get the correct value from the array.
            if (index != null && index.intValue() >= 1 && index.intValue() <= 9) {
                value = values[index.intValue() - 1];
            }
            return value;
        }
    }

    /**
     * Return an exit reason, only if the status is exited.
     * Otherwise return null and get the default value.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitReason implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String SR09 = entity.getFieldValue(SR_09_STATUS);
            if (SR09.equals(SR09_EXITED_CODE)) {
                value = (String) getProperty(entity.getBean(), field.getBeanPath());
            }
            return value;
        }
    }

    /**
     * Returns the primary license number for the given staff member.
     *
     */
    protected class RetrieveLicense implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            SisStaff staff = (SisStaff) entity.getBean();
            String licenseNumber = null;

            Collection<StaffCertification> licenses = m_licensesMap.get(staff.getOid());
            if (!CollectionUtils.isEmpty(licenses)) {
                licenseNumber = licenses.iterator().next().getCertificationNumber();
                if (licenses.size() > 1) {
                    StateReportValidationError error = new StateReportValidationError(entity, field,
                            "Multiple licenses SR03 marked as primary",
                            "Using: " + STYLE_BOLD + licenseNumber + STYLE_END);
                    entity.addRetrievalError(field.getFieldId(), error);
                }
            }
            return licenseNumber;
        }
    }

    /**
     * Returns the properly formatted salary percentage for the staff/property combination. This
     * method handles values that are formatted as characters, integers, or decimals.
     *
     */
    protected class RetrievePercentage implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String rawValue = (String) getProperty(entity.getBean(), field.getBeanPath());

            String percentage = null;

            if (!StringUtils.isEmpty(rawValue)) {
                int decimalIndex = rawValue.indexOf('.');
                int percentIndex = rawValue.indexOf('%');

                double decimal = 0.0;
                try {
                    if (percentIndex != -1) {
                        /*
                         * Raw value: 80%, 5%, 33.3%, etc.
                         */
                        decimal = Double.parseDouble(rawValue.substring(0, percentIndex)) / 100.0;
                    } else if (decimalIndex != -1) {
                        /*
                         * Raw value: 0.5, .25, .333, etc.
                         */
                        decimal = Double.parseDouble(rawValue);
                    } else {
                        /*
                         * Raw value: 10, 005, 33, etc.
                         */
                        decimal = Double.parseDouble(rawValue) / 100.0;
                    }
                } catch (NumberFormatException nfe) {
                    StateReportValidationError error =
                            new StateReportValidationError(entity, field, "Invalid percent value", rawValue);

                    entity.addRetrievalError(field.getFieldId(), error);
                }

                if (decimal >= 1.0) {
                    percentage = "1.00";
                } else {
                    percentage = m_decimalFormat.format(decimal);

                    // Assure min length of 2
                    if (percentage.length() == 1) {
                        percentage = "0" + percentage;
                    }
                }
            } else {
                percentage = "00";
            }

            return percentage;
        }

    }

    /**
     * Returns the state equivalent race code for the given staff.
     */
    protected class RetrieveRaceCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            SisStaff staff = (SisStaff) entity.getBean();
            String raceCode = "";

            if (m_calculateTotals) {
                /*
                 * Calculate the local (X2) bitmap value and then translate it to the corresponding
                 * DOE
                 * code. See comments at the top of this class in the "user-referable code" section.
                 */
                int stateRaceCode = 0;
                Collection<Race> raceBeans = m_raceCodeMap.get(staff.getPersonOid());
                if (!CollectionUtils.isEmpty(raceBeans)) {
                    int localRaceCode = 0;

                    for (Race raceBean : raceBeans) {
                        String stateCode = lookupStateValue(Race.class, Race.COL_RACE_CODE, raceBean.getRaceCode());
                        if (StringUtils.isInteger(stateCode)) {
                            localRaceCode += Integer.parseInt(stateCode);
                        }
                    }

                    if (localRaceCode >= 0 && localRaceCode <= STATE_RACE_CODES.length) {
                        stateRaceCode = STATE_RACE_CODES[localRaceCode];
                    }

                    if (staff.getPerson().getHispanicLatinoIndicator()) {
                        stateRaceCode += 32;
                    }

                    raceCode = StringUtils.padLeft(String.valueOf(stateRaceCode), 2, '0');
                }
            } else {
                raceCode = (String) WebUtils.getProperty(staff, field.getBeanPath());
            }

            return raceCode;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * Validates the birthdate format and age.
     * Must be 16 years of age before reporting date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateBirthDate implements FieldValidator {

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

            // Check format.
            Date bday = null;
            if (!StringUtils.isEmpty(value)) {
                try {
                    bday = m_dateFormat.parse(value);
                } catch (ParseException e) {
                    // Fails to parse. Error.
                    errors.add(new StateReportValidationError(entity, field,
                            "Date of birth SR07 format error", "SR07=" + STYLE_BOLD + value + STYLE_END));
                }
            }
            // Check age
            if (bday != null && m_reportDateMax.before(bday)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Reporting age SR07 must be 16 years or older", "SR07=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate degree type goes with institution and subject.
     * This validator is used for three fields. FieldDefintion.parameter
     * must be an Integer(1), Integer(4) and Integer(7) to indicate which field.
     * (This matches the numbering pattern for the retriever.)
     *
     * Some degree types require institution and subjects.
     * Some degree types can not have institution or subject.
     * Degree fields must be filled in order.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateDegreeType implements FieldValidator {

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
            int index = ((Integer) field.getParameter()).intValue();
            String[] dTypes = null;
            String type = null;
            String typeId = null;
            String institute = null;
            String instituteId = null;
            String subject = null;
            String subjectId = null;
            if (index == 1) {
                type = entity.getFieldValue(SR_18_DEGREE_TYPE_1);
                typeId = data.getFieldDefinition(SR_18_DEGREE_TYPE_1).getFieldId();
                institute = entity.getFieldValue(SR_19_DEGREE_INST_1);
                instituteId = data.getFieldDefinition(SR_19_DEGREE_INST_1).getFieldId();
                subject = entity.getFieldValue(SR_20_DEGREE_SUBJECT_1);
                subjectId = data.getFieldDefinition(SR_20_DEGREE_SUBJECT_1).getFieldId();

                // validate institution and subject go with degree type.
                // validate institution and subject go with degree type.
                if (type != null && type.matches("00[09]|01[013]")
                        && (!"0000".equals(institute) || !"00".equals(subject))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Degree type " + typeId + " cannot have institute " +
                                    instituteId + " or subject " + subjectId,
                            typeId + "=" + STYLE_BOLD + type + STYLE_END + ", " +
                                    instituteId + "=" + STYLE_BOLD + institute + STYLE_END + ", " +
                                    subjectId + "=" + STYLE_BOLD + subject + STYLE_END));
                } else if (type != null && !type.matches("00[089]|01[0123]")
                        && ("0000".equals(institute) || "00".equals(subject))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Degree type " + typeId + " requires institute " +
                                    instituteId + " and subject " + subjectId,
                            typeId + "=" + STYLE_BOLD + type + STYLE_END + ", " +
                                    instituteId + "=" + STYLE_BOLD + institute + STYLE_END + ", " +
                                    subjectId + "=" + STYLE_BOLD + subject + STYLE_END));
                }
            } else if (index == 4) {
                type = entity.getFieldValue(SR_21_DEGREE_TYPE_2);
                typeId = data.getFieldDefinition(SR_21_DEGREE_TYPE_2).getFieldId();
                institute = entity.getFieldValue(SR_22_DEGREE_INST_2);
                instituteId = data.getFieldDefinition(SR_22_DEGREE_INST_2).getFieldId();
                subject = entity.getFieldValue(SR_23_DEGREE_SUBJECT_2);
                subjectId = data.getFieldDefinition(SR_23_DEGREE_SUBJECT_2).getFieldId();
            } else if (index == 7) {
                type = entity.getFieldValue(SR_24_DEGREE_TYPE_3);
                typeId = data.getFieldDefinition(SR_24_DEGREE_TYPE_3).getFieldId();
                institute = entity.getFieldValue(SR_25_DEGREE_INST_3);
                instituteId = data.getFieldDefinition(SR_25_DEGREE_INST_3).getFieldId();
                subject = entity.getFieldValue(SR_26_DEGREE_SUBJECT_3);
                subjectId = data.getFieldDefinition(SR_26_DEGREE_SUBJECT_3).getFieldId();
                dTypes = new String[3];
                dTypes[0] = entity.getFieldValue(SR_18_DEGREE_TYPE_1);
                dTypes[1] = entity.getFieldValue(SR_21_DEGREE_TYPE_2);
                dTypes[2] = type;
            }

            if (index == 4 || index == 7) {
                // validate institution and subject go with degree type.
                if (type != null && type.matches("00[09]|01[0123]")
                        && (!"0000".equals(institute) || !"00".equals(subject))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Degree type " + typeId + " cannot have institute " +
                                    instituteId + " or subject " + subjectId,
                            typeId + "=" + STYLE_BOLD + type + STYLE_END + ", " +
                                    instituteId + "=" + STYLE_BOLD + institute + STYLE_END + ", " +
                                    subjectId + "=" + STYLE_BOLD + subject + STYLE_END));
                } else if (type != null && !type.matches("00[09]|01[0123]")
                        && ("0000".equals(institute) || "00".equals(subject))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Degree type " + typeId + " requires institute " +
                                    instituteId + " and subject " + subjectId,
                            typeId + "=" + STYLE_BOLD + type + STYLE_END + ", " +
                                    instituteId + "=" + STYLE_BOLD + institute + STYLE_END + ", " +
                                    subjectId + "=" + STYLE_BOLD + subject + STYLE_END));
                }
            }

            // validate fill order.
            if (index == 7) {
                if (("000".equals(dTypes[0]) && !"000".equals(dTypes[1])) ||
                        ("000".equals(dTypes[0]) && !"000".equals(dTypes[2])) ||
                        ("000".equals(dTypes[1]) && !"000".equals(dTypes[2]))) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Degree types must be filled in order", "Degree 1 first, then 2 and 3"));
                }
            }
            return errors;
        }
    }

    /**
     * Validate reason for exit against employment status.
     * Exit reason must accompany states exited.
     * Exit reason many not exist for other statuses.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateExitReason implements FieldValidator {

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
            String SR09 = entity.getFieldValue(SR_09_STATUS);
            // exit and exit reason.
            if ("04".equals(SR09) && "00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Status SR09 exited requires Exit reason SR10",
                        "SR09=" + STYLE_BOLD + SR09 + STYLE_END + ", SR10=" + STYLE_BOLD + value + STYLE_END));
            } else if (!"04".equals(SR09) && !"00".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Status SR09 not exited cannot have Exit reason SR10",
                        "SR09=" + STYLE_BOLD + SR09 + STYLE_END + ", SR10=" + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }
    }

    /**
     * Validate the hire date is before the reporting date.
     * Staff must be age 16 to 82 on hire date.
     * Hire date must be before report date.
     * No more than 82 years of service allowed.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateHireDate implements FieldValidator {

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
            // Check format.
            String SR07 = entity.getFieldValue(SR_07_BIRTH_DATE);
            Date hday = null;
            Date bday = null;
            if (!StringUtils.isEmpty(SR07)) {
                try {
                    bday = m_dateFormat.parse(SR07);
                } catch (ParseException e) {
                    // Fails to parse. will be reported elsewhere.
                }
            }
            if (!StringUtils.isEmpty(value)) {
                try {
                    hday = m_dateFormat.parse(value);
                } catch (ParseException e) {
                    // Fails to parse. Error.
                    errors.add(new StateReportValidationError(entity, field,
                            "Date of hire SR11 format error", "SR11=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            // check hire date age.
            if (bday != null) {
                Calendar cal = Calendar.getInstance();
                cal.setTime(bday);
                cal.add(Calendar.YEAR, +16);
                Date hireDateMin = cal.getTime(); // 16 years old (> 16), min for hire age.
                cal.add(Calendar.YEAR, +67);
                Date hireDateMax = cal.getTime(); // 82 years old (< 83), max for hire age.
                if (hireDateMax.before(hday) || hireDateMin.after(hday)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Age SR07 on Hire date SR11 must be 16 - 82 years",
                            "SR07=" + STYLE_BOLD + SR07 + STYLE_END + ", SR11=" + STYLE_BOLD + value + STYLE_END));
                }
            }

            // Check hire date before report date
            if (hday != null && m_reportDate.before(hday)) {
                errors.add(new StateReportValidationError(entity, field,
                        "Date of hire SR11 must be before reporting date", "SR11=" + STYLE_BOLD + value + STYLE_END));
            }
            // Check hire date no more than 82 years of service
            if (hday != null && m_reportDateMin.after(hday)) {
                errors.add(new StateReportValidationError(entity, field,
                        "No more than 82 years of service", "SR11=" + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate salary percentages with salary sources.
     * this can be applied to three fields.
     * FieldDefinition.parameter must be the value
     * Integer(1) - Integer(3) to indicate
     * which to validate.
     * Percents must accompany a salary source value.
     * Each percent number must be between 0 and 1.
     * Total of three must be between 0 and 1.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateSalaryPercent implements FieldValidator {

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
            int index = ((Integer) field.getParameter()).intValue();
            float[] salPct = new float[3];
            String salSource = null;
            String salSourceId = null;
            try {
                if (index == 1) {
                    salSource = entity.getFieldValue(SR_12_SALARY_SOURCE_1);
                    salSourceId = data.getFieldDefinition(SR_12_SALARY_SOURCE_1).getFieldId();
                    salPct[index - 1] = m_decimalFormat.parse(value).floatValue();
                } else if (index == 2) {
                    salSource = entity.getFieldValue(SR_14_SALARY_SOURCE_2);
                    salSourceId = data.getFieldDefinition(SR_14_SALARY_SOURCE_2).getFieldId();
                    salPct[index - 1] = m_decimalFormat.parse(value).floatValue();
                } else if (index == 3) {
                    salSource = entity.getFieldValue(SR_16_SALARY_SOURCE_3);
                    salSourceId = data.getFieldDefinition(SR_16_SALARY_SOURCE_3).getFieldId();
                    salPct[0] = m_decimalFormat.parse(entity.getFieldValue(SR_13_PERCENT_SALARY_SOURCE_1)).floatValue();
                    salPct[1] = m_decimalFormat.parse(entity.getFieldValue(SR_15_PERCENT_SALARY_SOURCE_2)).floatValue();
                    salPct[2] = m_decimalFormat.parse(entity.getFieldValue(SR_17_PERCENT_SALARY_SOURCE_3)).floatValue();
                }
            } catch (ParseException e) {
                // percent parse exception, will be reported elsewhere.
            }

            // Validate percent range.
            if (salPct[index - 1] > 1.0 || salPct[index - 1] < 0.0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Salary percent " + field.getFieldId() + " out of range (0.0 - 1.0)",
                        field.getFieldId() + "=" + STYLE_BOLD + value + STYLE_END));
            }

            // Salary source goes with salary percentage.
            if ("000".equals(salSource) && salPct[index - 1] != 0.0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Source " + salSourceId + " cannot have percent " + field.getFieldId(),
                        salSourceId + "=" + STYLE_BOLD + salSource + STYLE_END + ", " + field.getFieldId() + "="
                                + STYLE_BOLD + value + STYLE_END));
            } else if (!"000".equals(salSource) && salPct[index - 1] == 0.0) {
                errors.add(new StateReportValidationError(entity, field,
                        "Source " + salSourceId + " must have percent " + field.getFieldId(),
                        salSourceId + "=" + STYLE_BOLD + salSource + STYLE_END + ", " + field.getFieldId() + "="
                                + STYLE_BOLD + value + STYLE_END));
            }

            // Check sum of percents is less or equal to 100%.
            // Check sources are filled from 1 to 3.
            if (index == 3) {
                float total = salPct[0] + salPct[1] + salPct[2];
                if (total > 1.0) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Salary source percentages add to greater than 100%",
                            "Total=" + STYLE_BOLD + Float.toString(total) + STYLE_END));
                }
                if ((salPct[0] == 0 && salPct[1] > 0) ||
                        (salPct[0] == 0 && salPct[2] > 0) ||
                        (salPct[1] == 0 && salPct[2] > 0)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Salary source must be filled in order", "Source 1 first, then 2 and 3"));
                }
            }
            return errors;
        }
    }

    /**
     * Gets the bean class.
     *
     * @return Class
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getBeanClass()
     */
    @Override
    public Class getBeanClass() {
        return SisStaff.class;
    }

    /**
     * Gets the export title.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "EPIMS Staff Roster";
    }

    /**
     * Returns heading text to include at the top of the export file.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        // EX: EPIMS,STAFF_ROSTER,02170000
        StringBuilder sb = new StringBuilder();
        sb.append("EPIMS,STAFF_ROSTER,");

        String code = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        if (StringUtils.isEmpty(code)) {
            code = "[INSERT DISTRICT ID HERE]";
            addSetupError("Using a placeholder for the district ID.", "Set the '" + DOE_DISTRICT_ID
                    + "' alias in the Data Dictionary and update that field with the correct ID.");
        } else {
            sb.append(code);
        }

        sb.append(ExportJavaSource.FORMAT_EOL_WINDOWS);

        return sb.toString();
    }

    /**
     * Turn off header row in export file.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Turn off value wrappers in the export file.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getUseValueWrappers()
     */
    @Override
    public boolean getUseValueWrappers() {
        return false;
    }

    /**
     * Initialize.
     *
     * @see
     *      com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(com.follett.fsc.core.
     *      k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_sklDstrIdField = translateAliasToJavaName(ALIAS_SKL_SIF_DISTRICT_ID, true);
        // Load Input Definition Parameters
        String includeIds = (String) getParameter(PARAM_INCLUDE_SIF_SCHOOL);
        if (!StringUtils.isEmpty(includeIds)) {
            List<String> rcdOids = new ArrayList<String>(Arrays.asList(includeIds.split(SEPARATOR_COMMA)));
            X2Criteria sifDistrIdCriteria = new X2Criteria();
            sifDistrIdCriteria.addIn(X2BaseBean.COL_OID, rcdOids);
            QueryByCriteria byCriteria = new QueryByCriteria(ReferenceCode.class, sifDistrIdCriteria);
            Collection<ReferenceCode> refCodes = getBroker().getCollectionByQuery(byCriteria);
            m_includeSifSchoolIds = new ArrayList();
            for (ReferenceCode code : refCodes) {
                m_includeSifSchoolIds.add(code.getCode());
            }
        }
        /*
         * Get core parameters
         */
        m_calculateTotals = ((Boolean) getParameter(PARAM_CALCULATE_TOTALS)).booleanValue();
        m_includeRatings = ((Boolean) getParameter(PARAM_INCLUDE_RATINGS));
        m_includeAttendance = ((Boolean) getParameter(PARAM_INCLUDE_ATTENDANCE));
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_exitAfterDate = (PlainDate) getParameter(PARAM_EXIT_AFTER_DATE);

        /*
         * Set up formatters and reference lookup tables
         */
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_decimalFormat = new DecimalFormat();
        m_decimalFormat.setMinimumIntegerDigits(0);
        m_decimalFormat.setMaximumFractionDigits(3);
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        m_fieldExitDate = translateAliasToJavaName(SR_27_EXIT_DATE, true);

        /*
         * Set up some date limits for age limit comparisons.
         */
        Calendar cal = Calendar.getInstance();
        cal.setTime(m_reportDate);
        cal.add(Calendar.YEAR, -16);
        m_reportDateMax = cal.getTime(); // 16 years old (> 16), min for employment reporting.
        cal.add(Calendar.YEAR, -67);
        m_reportDateMin = cal.getTime(); // 82 years of service (< 83), max for hire date.

        /*
         * Define all export fields.
         */
        ArrayList<FieldDefinition> fieldDefinitions = new ArrayList<FieldDefinition>(26);
        fieldDefinitions.add(getSR01_mepid());
        fieldDefinitions.add(getSR02_localId());
        fieldDefinitions.add(getSR03_license());
        fieldDefinitions.add(getSR04_firstName());
        fieldDefinitions.add(getSR05_middleName());
        fieldDefinitions.add(getSR06_lastName());
        fieldDefinitions.add(getSR07_birthDate());
        fieldDefinitions.add(getSR08_raceCode());
        fieldDefinitions.add(getSR09_status());
        fieldDefinitions.add(getSR10_exitReason());
        fieldDefinitions.add(getSR11_hireDate());
        fieldDefinitions.add(getSR12_salarySource1());
        fieldDefinitions.add(getSR13_salaryPercent1());
        fieldDefinitions.add(getSR14_salarySource2());
        fieldDefinitions.add(getSR15_salaryPercent2());
        fieldDefinitions.add(getSR16_salarySource3());
        fieldDefinitions.add(getSR17_salaryPercent3());
        fieldDefinitions.add(getSR18_degreeType1());
        fieldDefinitions.add(getSR19_degreeInstitution1());
        fieldDefinitions.add(getSR20_degreeSubject1());
        fieldDefinitions.add(getSR21_degreeType2());
        fieldDefinitions.add(getSR22_degreeInstitution2());
        fieldDefinitions.add(getSR23_degreeSubject2());
        fieldDefinitions.add(getSR24_degreeType3());
        fieldDefinitions.add(getSR25_degreeInstitution3());
        fieldDefinitions.add(getSR26_degreeSubject3());
        fieldDefinitions.add(getSR27_exitDate());
        if (m_includeRatings != null && m_includeRatings.booleanValue()) {
            fieldDefinitions.add(getSR28_professionalStatus());
            fieldDefinitions.add(getSR29_overallEvaluation());
            fieldDefinitions.add(getSR30_evaluationRating1());
            fieldDefinitions.add(getSR31_evaluationRating2());
            fieldDefinitions.add(getSR32_evaluationRating3());
            fieldDefinitions.add(getSR33_evaluationRating4());
            fieldDefinitions.add(getSR34_impactOnStudents());
            fieldDefinitions.add(getSR35_educatorEvaluationPlan());

        }
        if (m_includeAttendance != null && m_includeAttendance.booleanValue()) {
            fieldDefinitions.add(getSR36_daysPresent());
            fieldDefinitions.add(getSR37_daysExpected());
        }
        fieldDefinitions.add(getSR38_beginnerEducatorInd());

        setFieldDefinitions(fieldDefinitions);

        if (getSetupErrors().size() == 0) {
            /*
             * Define export query for staff records.
             */
            X2Criteria staffCriteria = getStaffCriteria();
            QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);

            int sort = ((Integer) getParameter(PARAM_SORT)).intValue();
            switch (sort) {
                case 1: // Local ID
                    staffQuery.addOrderByAscending(SisStaff.COL_LOCAL_ID);
                    break;

                case 2: // MEPID
                    staffQuery.addOrderByAscending(SisStaff.COL_STATE_ID);
                    break;

                default:
                    staffQuery.addOrderByAscending(SisStaff.COL_NAME_VIEW);
                    break;
            }

            // Load Maps and lists used in the retrievers
            loadMaps(staffCriteria);

            // Set the query to be used for student selection.
            setQuery(staffQuery);
            setEntityClass(EPIMSEntity.class);

            /*
             * Load auxiliary data in maps keyed on staff/person OID for all staff included in the
             * export.
             */
            int count = getBroker().getCount(staffQuery);
            SubQuery staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);
            SubQuery personSubQuery = new SubQuery(SisStaff.class, SisStaff.COL_PERSON_OID, staffCriteria);

            /*
             * Map 1: Degrees (use the three most recent)
             */
            Criteria orCriteria = new Criteria();
            orCriteria.addIsNull(StaffDegree.COL_DATE);

            Criteria dateCriteria = new Criteria();
            dateCriteria.addLessOrEqualThan(StaffDegree.COL_DATE, m_reportDate);
            dateCriteria.addOrCriteria(orCriteria);

            Criteria degreeCriteria = new Criteria();
            degreeCriteria.addIn(StaffDegree.COL_STAFF_OID, staffSubQuery);
            degreeCriteria.addAndCriteria(dateCriteria);

            QueryByCriteria degreeQuery = new QueryByCriteria(StaffDegree.class, degreeCriteria);
            degreeQuery.addOrderByDescending(StaffDegree.COL_DATE);
            m_degreesMap = getBroker().getGroupedCollectionByQuery(degreeQuery, StaffDegree.COL_STAFF_OID, count);

            /*
             * Map 2: Primary licenses
             *
             * We get the most recent license in case there are multiple flagged as primary
             */
            Criteria licenseCriteria = new Criteria();
            licenseCriteria.addIn(StaffCertification.COL_STAFF_OID, staffSubQuery);
            licenseCriteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, Boolean.TRUE);

            QueryByCriteria licenseQuery = new QueryByCriteria(StaffCertification.class, licenseCriteria);
            licenseQuery.addOrderByDescending(StaffCertification.COL_ISSUE_DATE);
            m_licensesMap =
                    getBroker().getGroupedCollectionByQuery(licenseQuery, StaffCertification.COL_STAFF_OID, count);

            /*
             * Map 3: Race codes
             */
            Criteria raceCriteria = new Criteria();
            raceCriteria.addIn(Race.COL_PERSON_OID, personSubQuery);

            QueryByCriteria raceQuery = new QueryByCriteria(Race.class, raceCriteria);
            m_raceCodeMap = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, count);
        }
    }

    /**
     * Build Field definition for SR 01, staff state identifier (MEPID).
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR01_mepid() {
        FieldDefinition field = new FieldDefinition(SR_01_MEPID,
                SisStaff.COL_STATE_ID,
                null, false, 8, 8, REGEX_NUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 02, staff local identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR02_localId() {
        FieldDefinition field = new FieldDefinition(SR_02_LOCALID,
                SisStaff.COL_LOCAL_ID,
                "0", false, 1, 20, REGEX_ALPHANUMERIC_HYPHEN,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 03, staff certification license.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR03_license() {
        FieldDefinition field = new FieldDefinition(SR_03_LICENSE,
                SisStaff.REL_CERTIFICATIONS + ModelProperty.PATH_DELIMITER
                        + StaffCertification.COL_CERTIFICATION_NUMBER,
                "00", false, 2, 20, REGEX_ALPHANUMERIC, null,
                new RetrieveLicense(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 04, staff first name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR04_firstName() {
        FieldDefinition field = new FieldDefinition(SR_04_FIRST_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_FIRST_NAME,
                null, false, 1, 30, REGEX_NAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 05, staff middle name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR05_middleName() {
        FieldDefinition field = new FieldDefinition(SR_05_MIDDLE_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_MIDDLE_NAME,
                NO_MIDDLE_NAME, false, 1, 30, REGEX_MNAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 06, staff last name.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR06_lastName() {
        FieldDefinition field = new FieldDefinition(SR_06_LAST_NAME,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_LAST_NAME,
                null, false, 1, 30, REGEX_NAME,
                null, new RetrieveStripNameChar(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 07, staff date of birth.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR07_birthDate() {
        FieldDefinition field = new FieldDefinition(SR_07_BIRTH_DATE,
                SisStaff.REL_PERSON + ModelProperty.PATH_DELIMITER + SisPerson.COL_DOB,
                null, false, 10, 10, null,
                m_dateFormat, null,
                new ValidateBirthDate(), null);
        return field;
    }

    /**
     * Build Field definition for SR 08, race code.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR08_raceCode() {
        FieldDefinition field = new FieldDefinition(SR_08_RACE,
                translateAliasToJavaName(SR_08_RACE, true),
                null, false, 2, 2, "[012345][0123456789]|6[0123]", null,
                new RetrieveRaceCode(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 09, employment status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR09_status() {
        FieldDefinition field = new FieldDefinition(SR_09_STATUS,
                translateAliasToJavaName(SR_09_STATUS, true),
                null, true, 2, 2, "0[1234]", null,
                null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 10, exit reason.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR10_exitReason() {
        FieldDefinition field = new FieldDefinition(SR_10_EXIT_REASON,
                translateAliasToJavaName(SR_10_EXIT_REASON, true),
                "00", true, 2, 2, "0[0123456789]|10", null,
                new RetrieveExitReason(), new ValidateExitReason(), null);
        return field;
    }

    /**
     * Build Field definition for SR 11, hire date.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR11_hireDate() {
        FieldDefinition field = new FieldDefinition(SR_11_HIRE_DATE, SisStaff.COL_HIRE_DATE,
                null, false, 10, 10, null,
                m_dateFormat, null,
                new ValidateHireDate(), null);
        return field;
    }

    /**
     * Build Field definition for SR 12, salary source 1.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR12_salarySource1() {
        FieldDefinition field = new FieldDefinition(SR_12_SALARY_SOURCE_1,
                translateAliasToJavaName(SR_12_SALARY_SOURCE_1, true),
                "000", true, 3, 3, REGEX_ALPHANUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 13, salary percent 1.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR13_salaryPercent1() {
        FieldDefinition field = new FieldDefinition(SR_13_PERCENT_SALARY_SOURCE_1,
                translateAliasToJavaName(SR_13_PERCENT_SALARY_SOURCE_1, true),
                null, false, 2, 5, REGEX_NUMERIC_DECIMAL,
                null, new RetrievePercentage(),
                new ValidateSalaryPercent(), Integer.valueOf(1));
        return field;
    }

    /**
     * Build Field definition for SR 14, salary source 2.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR14_salarySource2() {
        FieldDefinition field = new FieldDefinition(SR_14_SALARY_SOURCE_2,
                translateAliasToJavaName(SR_14_SALARY_SOURCE_2, true),
                "000", true, 3, 3, REGEX_ALPHANUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 15, salary percent 2.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR15_salaryPercent2() {
        FieldDefinition field = new FieldDefinition(SR_15_PERCENT_SALARY_SOURCE_2,
                translateAliasToJavaName(SR_15_PERCENT_SALARY_SOURCE_2, true),
                null, false, 2, 5, REGEX_NUMERIC_DECIMAL,
                null, new RetrievePercentage(),
                new ValidateSalaryPercent(), Integer.valueOf(2));
        return field;
    }

    /**
     * Build Field definition for SR 16, salary source 3.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR16_salarySource3() {
        FieldDefinition field = new FieldDefinition(SR_16_SALARY_SOURCE_3,
                translateAliasToJavaName(SR_16_SALARY_SOURCE_3, true),
                "000", true, 3, 3, REGEX_ALPHANUMERIC,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 17, salary percent 3.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR17_salaryPercent3() {
        FieldDefinition field = new FieldDefinition(SR_17_PERCENT_SALARY_SOURCE_3,
                translateAliasToJavaName(SR_17_PERCENT_SALARY_SOURCE_3, true),
                null, false, 2, 5, REGEX_NUMERIC_DECIMAL,
                null, new RetrievePercentage(),
                new ValidateSalaryPercent(), Integer.valueOf(3));
        return field;
    }

    /**
     * Build Field definition for SR 18, degree type 1.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(1) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR18_degreeType1() {
        FieldDefinition field = new FieldDefinition(SR_18_DEGREE_TYPE_1,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_TYPE,
                null, false, 3, 3, "00[0123456789]|01[01234]",
                null, new RetrieveDegreeValues(),
                new ValidateDegreeType(), Integer.valueOf(1));
        return field;
    }

    /**
     * Build Field definition for SR 19, degree institution 1.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(2) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR19_degreeInstitution1() {
        FieldDefinition field = new FieldDefinition(SR_19_DEGREE_INST_1,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_INSTITUTION,
                null, false, 4, 4, REGEX_NUMERIC,
                null, new RetrieveDegreeValues(), null, Integer.valueOf(2));
        return field;
    }

    /**
     * Build Field definition for SR 20, degree subject 1.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(3) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR20_degreeSubject1() {
        FieldDefinition field = new FieldDefinition(SR_20_DEGREE_SUBJECT_1,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_DEGREE,
                null, false, 2, 2, REGEX_ALPHANUMERIC,
                null, new RetrieveDegreeValues(), null, Integer.valueOf(3));
        return field;
    }

    /**
     * Build Field definition for SR 21, degree type 2.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(4) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR21_degreeType2() {
        FieldDefinition field = new FieldDefinition(SR_21_DEGREE_TYPE_2,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_TYPE,
                null, false, 3, 3, "00[0123456789]|01[01234]",
                null, new RetrieveDegreeValues(),
                new ValidateDegreeType(), Integer.valueOf(4));
        return field;
    }

    /**
     * Build Field definition for SR 22, degree institution 2.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(5) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR22_degreeInstitution2() {
        FieldDefinition field = new FieldDefinition(SR_22_DEGREE_INST_2,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_INSTITUTION,
                null, false, 4, 4, REGEX_NUMERIC,
                null, new RetrieveDegreeValues(), null, Integer.valueOf(5));
        return field;
    }

    /**
     * Build Field definition for SR 23, degree subject 3.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(6) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR23_degreeSubject2() {
        FieldDefinition field = new FieldDefinition(SR_23_DEGREE_SUBJECT_2,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_DEGREE,
                null, false, 2, 2, REGEX_ALPHANUMERIC,
                null, new RetrieveDegreeValues(), null, Integer.valueOf(6));
        return field;
    }

    /**
     * Build Field definition for SR 24, degree type 3.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(7) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR24_degreeType3() {
        FieldDefinition field = new FieldDefinition(SR_24_DEGREE_TYPE_3,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_TYPE,
                null, false, 3, 3, "00[0123456789]|01[01234]",
                null, new RetrieveDegreeValues(),
                new ValidateDegreeType(),
                Integer.valueOf(7));
        return field;
    }

    /**
     * Build Field definition for SR 25, degree institution 3.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(8) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR25_degreeInstitution3() {
        FieldDefinition field = new FieldDefinition(SR_25_DEGREE_INST_3,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_INSTITUTION,
                null, false, 4, 4, REGEX_NUMERIC,
                null, new RetrieveDegreeValues(), null,
                Integer.valueOf(8));
        return field;
    }

    /**
     * Build Field definition for SR 26, degree subject 3.
     * Degree information is retrieved all together. To get the part this
     * field needs, include Integer(9) in FieldDefinition.object.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR26_degreeSubject3() {
        FieldDefinition field = new FieldDefinition(SR_26_DEGREE_SUBJECT_3,
                SisStaff.REL_DEGREES + ModelProperty.PATH_DELIMITER + StaffDegree.COL_DEGREE,
                null, false, 2, 2, REGEX_ALPHANUMERIC,
                null, new RetrieveDegreeValues(), null,
                Integer.valueOf(9));
        return field;
    }

    /**
     * Build Field definition for SR 27, Exit date.
     * The date the staff member exited the district.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR27_exitDate() {
        FieldDefinition field = new FieldDefinition(SR_27_EXIT_DATE,
                m_fieldExitDate,
                "NA", false, 2, 10, null,
                m_dateFormat, null, null, null);

        DataDictionaryField dictField = getDataDictionaryField(SisStaff.class, m_fieldExitDate);
        SystemStringConverter converter = null;
        if (dictField.isString()) {
            Converter baseConverter = ConverterFactory.getConverterForClass(
                    dictField.getEffectiveJavaType(), Locale.getDefault(), dictField.isString());
            if (baseConverter instanceof SystemStringConverter) {
                converter = (SystemStringConverter) baseConverter;
            }
        }

        field.setConverter(converter);
        return field;
    }

    /**
     * Build Field definition for SR 28, Educators Professional Teacher Status.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR28_professionalStatus() {
        FieldDefinition field = new FieldDefinition(SR_28_PROFESSIONAL_STATUS,
                translateAliasToJavaName(SR_28_PROFESSIONAL_STATUS, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 29, Overall Annual Evaluation Rating.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR29_overallEvaluation() {
        FieldDefinition field = new FieldDefinition(SR_29_OVERALL_EVALUATION,
                translateAliasToJavaName(SR_29_OVERALL_EVALUATION, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 30, Evaluation Rating 1.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR30_evaluationRating1() {
        FieldDefinition field = new FieldDefinition(SR_30_STANDARD_EVALUATION_1,
                translateAliasToJavaName(SR_30_STANDARD_EVALUATION_1, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 31, Evaluation Rating 2.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR31_evaluationRating2() {
        FieldDefinition field = new FieldDefinition(SR_31_STANDARD_EVALUATION_2,
                translateAliasToJavaName(SR_31_STANDARD_EVALUATION_2, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 32, Evaluation Rating 3.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR32_evaluationRating3() {
        FieldDefinition field = new FieldDefinition(SR_32_STANDARD_EVALUATION_3,
                translateAliasToJavaName(SR_32_STANDARD_EVALUATION_3, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 34, Evaluation Rating 4.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR33_evaluationRating4() {
        FieldDefinition field = new FieldDefinition(SR_33_STANDARD_EVALUATION_4,
                translateAliasToJavaName(SR_33_STANDARD_EVALUATION_4, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 34, Evaluation Rating 4.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR34_impactOnStudents() {
        FieldDefinition field = new FieldDefinition(SR_34_IMPACT_ON_STUDENT,
                translateAliasToJavaName(SR_34_IMPACT_ON_STUDENT, true),
                "00", true, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 35, Educator Evaluation Plan.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR35_educatorEvaluationPlan() {
        FieldDefinition field = new FieldDefinition(SR_35_EDUCATOR_EVALUATION_PLAN,
                String.valueOf(LABEL_PREFIX_CHAR),
                "99", false, 2, 2, null,
                null, null, null, null);
        return field;
    }

    /**
     * Build Field definition for SR 36, Days Present.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR36_daysPresent() {
        FieldDefinition field = new FieldDefinition(SR_36_DAYS_PRESENT,
                translateAliasToJavaName(SR_36_DAYS_PRESENT, true),
                null, false, 1, 5, null,
                null, new RetrieveDaysPresent(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 37, Days Expected To Be in Attendance.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR37_daysExpected() {
        FieldDefinition field = new FieldDefinition(SR_37_DAYS_EXPECTED,
                translateAliasToJavaName(SR_37_DAYS_EXPECTED, true),
                null, false, 1, 5, null,
                null, new RetrieveDaysExpected(), null, null);
        return field;
    }

    /**
     * Build Field definition for SR 38, Beginner Educator Identifier.
     *
     * @return a FieldDefinition
     */
    protected FieldDefinition getSR38_beginnerEducatorInd() {
        FieldDefinition field = new FieldDefinition(SR_38_BEGINNER_EDUCATOR_IND,
                translateAliasToJavaName(SR_38_BEGINNER_EDUCATOR_IND, true),
                null, false, 2, 2, null,
                null, new RetrieveBegginerInd(), null, null);
        return field;
    }

    /**
     * Returns the criteria that returns all staff members that should be included in the export.
     *
     * @return X2Criteria
     */
    private X2Criteria getStaffCriteria() {
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(SisStaff.COL_ORGANIZATION1_OID, getOrganization().getOid());

        if (isSchoolContext()) {
            criteria.addEqualTo(SisStaff.COL_SCHOOL_OID, getSchool().getOid());
        } else if (m_includeSifSchoolIds != null) {
            criteria.addIn(ConductIncident.REL_SCHOOL + ModelProperty.PATH_DELIMITER
                    + m_sklDstrIdField, m_includeSifSchoolIds);
        }

        // staff Status must not be empty.
        criteria.addNotEmpty(getFieldDefinition(SR_09_STATUS).getBeanPath(), getBroker().getPersistenceKey());

        // Select staff who enter before report date and exit after report date.
        criteria.addLessOrEqualThan(SisStaff.COL_HIRE_DATE, m_reportDate);

        X2Criteria criteria2 = new X2Criteria();
        X2Criteria criteria3 = new X2Criteria();
        criteria2.addIsNull(m_fieldExitDate);
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd");
        criteria3.addGreaterOrEqualThan(m_fieldExitDate, dateFormat.format(m_exitAfterDate));
        criteria2.addOrCriteria(criteria3);
        criteria.addAndCriteria(criteria2);

        String queryString = (String) getParameter(PARAM_QUERY_STRING);
        int queryBy = ((Integer) getParameter(PARAM_QUERY_BY)).intValue();
        switch (queryBy) {
            case 1: // Bargaining unit
                criteria.addEqualTo(SisStaff.COL_BARGAINING_UNIT, queryString);
                break;

            case 2: // Local ID
                criteria.addPatternMatch(SisStaff.COL_LOCAL_ID, queryString, false);
                break;

            case 3: // MEPID
                criteria.addPatternMatch(SisStaff.COL_STATE_ID, queryString, false);
                break;

            case 4: // Snapshot
                criteria.addIn(X2BaseBean.COL_OID,
                        ReportUtils.getRecordSetSubQuery(queryString, getUser(), getSchool()));
                break;

            default:
                // Take all staff in the district
                break;
        }

        return criteria;
    }

    /**
     * Load maps of supporting data.
     *
     * @param staffCriteria Criteria
     */
    private void loadMaps(Criteria staffCriteria) {
        String contextOid = getCurrentContext().getOid();

        Criteria districtCalendarDateCriteria = new Criteria();
        districtCalendarDateCriteria.addEqualTo(DistrictCalendar.COL_DISTRICT_CONTEXT_OID, contextOid);
        districtCalendarDateCriteria.addEqualTo(DistrictCalendar.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));
        QueryByCriteria districtCalendarDateQuery =
                new QueryByCriteria(DistrictCalendar.class, districtCalendarDateCriteria);
        districtCalendarDateQuery.addOrderByAscending(DistrictCalendar.COL_DATE);

        m_districtCalendarDateList = getBroker().getCollectionByQuery(districtCalendarDateQuery);


        SubQuery staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);

        Criteria calendarCriteria = new Criteria();
        String currentContextOid = getCurrentContext().getOid();
        if (m_reportDate != null) {
            SisDistrictSchoolYearContext dateContext =
                    CalendarManager.getDistrictContext(m_reportDate, getOrganization().getOid(), getBroker());
            if (dateContext != null) {
                currentContextOid = dateContext.getOid();
            }
        }

        calendarCriteria.addEqualTo(StaffCalendar.COL_DISTRICT_CONTEXT_OID, currentContextOid);

        QueryByCriteria calendarQuery = new QueryByCriteria(StaffCalendar.class, calendarCriteria);
        Collection<StaffCalendar> calendars = getBroker().getCollectionByQuery(calendarQuery);
        for (StaffCalendar staffCalendar : calendars) {
            m_calendarIdMap.put(staffCalendar.getCalendarId(), staffCalendar.getOid());
        }


        Criteria calendarDateCriteria = new Criteria();
        calendarDateCriteria.addEqualTo(StaffCalendarDate.COL_IN_SESSION_INDICATOR, Boolean.valueOf(true));
        QueryByCriteria calendarDateQuery = new QueryByCriteria(StaffCalendarDate.class, calendarDateCriteria);
        calendarDateQuery.addOrderByAscending(StaffCalendarDate.COL_STAFF_CALENDAR_OID);
        calendarDateQuery.addOrderByAscending(StaffCalendarDate.COL_DATE);

        m_calendarDateMap = getBroker().getGroupedCollectionByQuery(calendarDateQuery,
                StaffCalendarDate.COL_STAFF_CALENDAR_OID, 64);


        // Load the refAttMap (Reference Attendance Student Code)
        Criteria criteria = new Criteria();
        QueryByCriteria query = new QueryByCriteria(RefAttendanceStaff.class, criteria);

        m_refAttMap = getBroker().getMapByQuery(query, RefAttendanceStaff.COL_ATTENDANCE_CODE, 30);


        // Load Staff Attendance Reason Codes
        X2Criteria staffAttCodesCriteria = new X2Criteria();
        staffAttCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_STAFF_ATTENDANCE_REASONS);
        BeanQuery staffAttCodesQuery = new BeanQuery(ReferenceCode.class, staffAttCodesCriteria);
        staffAttCodesQuery.addOrderByAscending(ReferenceCode.COL_CODE);
        Collection<ReferenceCode> staffAttCodesList = getBroker().getCollectionByQuery(staffAttCodesQuery);

        for (ReferenceCode referenceCode : staffAttCodesList) {
            String stateCode = referenceCode.getStateCode();
            if (stateCode == null) {
                stateCode = STAFF_ATTENDANCE_REASON_00;
            }
            m_staffAttReasonsCodesMap.put(referenceCode.getCode(), stateCode);
        }


        // Load Staff Leave Reason Codes
        X2Criteria staffLeaveCodesCriteria = new X2Criteria();
        staffLeaveCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, REF_CODE_STAFF_LEAVE_REASONS);
        BeanQuery staffLeaveCodesQuery = new BeanQuery(ReferenceCode.class, staffLeaveCodesCriteria);
        staffLeaveCodesQuery.addOrderByAscending(ReferenceCode.COL_CODE);
        Collection<ReferenceCode> staffLeaveCodesList = getBroker().getCollectionByQuery(staffLeaveCodesQuery);

        for (ReferenceCode referenceCode : staffLeaveCodesList) {
            String stateCode = referenceCode.getStateCode();
            if (stateCode == null) {
                stateCode = STAFF_ATTENDANCE_REASON_00;
            }
            m_staffLeaveReasonsCodesMap.put(referenceCode.getCode(), stateCode);
        }


        // Get Staff Attendance
        Criteria attendanceCriteria = new X2Criteria();
        attendanceCriteria.addIn(StaffAttendance.COL_STAFF_OID, staffSubQuery);
        // attendanceCriteria.addEqualTo(StaffAttendance.COL_REASON, Boolean.valueOf(true));

        BeanQuery attendanceQuery = new BeanQuery(StaffAttendance.class, attendanceCriteria);
        attendanceQuery.addOrderByAscending(StaffAttendance.COL_STAFF_OID);
        attendanceQuery.addOrderByAscending(StaffAttendance.COL_REASON);
        attendanceQuery.addOrderByAscending(StaffAttendance.COL_DATE);

        m_attendanceList = getBroker().getGroupedCollectionByQuery(attendanceQuery, StaffAttendance.COL_STAFF_OID, 64);


        // Get Staff Leaves
        X2Criteria leaveCriteria = new X2Criteria();
        leaveCriteria.addIn(StaffLeave.COL_STAFF_OID, staffSubQuery);

        BeanQuery leaveQuery = new BeanQuery(StaffLeave.class, leaveCriteria);
        leaveQuery.addOrderByAscending(StaffLeave.COL_STAFF_OID);
        leaveQuery.addOrderByAscending(StaffLeave.COL_REASON_CODE);
        leaveQuery.addOrderByAscending(StaffLeave.COL_START_DATE);

        m_leaveList = getBroker().getGroupedCollectionByQuery(leaveQuery, StaffLeave.COL_STAFF_OID, 64);
    }

    /**
     * Logic to determine if a calendar Date should be included .
     *
     * @param calendarDate PlainDate
     * @param staffHireDate PlainDate
     * @param staffExitDate PlainDate
     * @param staffAttendances Map<PlainDate,StaffAttendance>
     * @param staffLeaves Collection<StaffLeave>
     * @param isPresent boolean
     * @return boolean
     */
    boolean includeDate(PlainDate calendarDate,
                        PlainDate staffHireDate,
                        PlainDate staffExitDate,
                        Map<PlainDate, StaffAttendance> staffAttendances,
                        Collection<StaffLeave> staffLeaves,
                        boolean isPresent) {
        boolean include = true;

        if (staffHireDate != null && calendarDate.before(staffHireDate)) {
            include = false;
        } else if (staffExitDate != null && (calendarDate.after(staffExitDate) || calendarDate.equals(staffExitDate))) {
            include = false;
        } else if (calendarDate.after(m_reportDate)) {
            include = false;
        } else {
            if (staffAttendances != null) {
                StaffAttendance staffAttendance = staffAttendances.get(calendarDate);
                if (staffAttendance != null) {
                    boolean isAbsent =
                            staffAttendance.getReferenceAttendance() == null ? isAbsent(staffAttendance.getCode())
                                    : staffAttendance.getReferenceAttendance().getAbsentIndicator();
                    String stateCode = m_staffAttReasonsCodesMap.get(staffAttendance.getReason());

                    if (calendarDate.equals(staffAttendance.getDate()) && isAbsent) {
                        if (STAFF_ATTENDANCE_REASON_02.equals(stateCode)
                                || (isPresent && STAFF_ATTENDANCE_REASON_01.equals(stateCode))) {
                            include = false;
                        }
                    }
                }
            }

            if (include && staffLeaves != null) {
                for (StaffLeave staffLeave : staffLeaves) {
                    PlainDate startDate = staffLeave.getStartDate();
                    PlainDate endDate = staffLeave.getEndDate();
                    if ((calendarDate.equals(startDate) || calendarDate.after(startDate))
                            && (calendarDate.equals(endDate) || calendarDate.before(endDate))) {
                        String stateCode = m_staffLeaveReasonsCodesMap.get(staffLeave.getReasonCode());

                        if (STAFF_ATTENDANCE_REASON_02.equals(stateCode)
                                || (isPresent && STAFF_ATTENDANCE_REASON_01.equals(stateCode))) {
                            include = false;
                        }
                    }
                }
            }
        }

        return include;
    }

    /**
     * Check if a Staff Attendance Code is considered an absence.
     *
     * @param attendanceCode String
     * @return boolean
     */
    boolean isAbsent(String attendanceCode) {
        boolean isAbsentCode = false;
        if (m_refAttMap != null) {
            RefAttendanceStaff refAttStf = m_refAttMap.get(attendanceCode);
            if (refAttStf != null && refAttStf.getAbsentIndicator()) {
                isAbsentCode = true;
            }
        }

        return isAbsentCode;
    }

}
