/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2019 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.il;

import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.tuple.Pair;



/**
 * IL Student Attendance File Format data.
 *
 * @author Follett Software Company
 * @copyright 2019
 */
public class ILStudentMonthlyAtt extends StateReportData {

    /**
     * The Class ILAttendanceRecord.
     *
     * @author Follett Software Company
     */
    public class ILAttendanceRecord {

        private Double m_daysAbsExcused;
        private Double m_daysAbsUnexcused;
        private Double m_daysHomebound;
        private Double m_daysHospitalized;
        private Double m_daysPresent;
        private PlainDate m_enrDate;
        private String m_month;
        private String m_sklCodeHome;
        private String m_sklCodeServing;

        /**
         * Instantiates a new IL attendance record.
         */
        ILAttendanceRecord() {
            super();
        }

        /**
         * Gets the days abs excused.
         *
         * @return the m_daysAbsExcused
         */
        public Double getDaysAbsExcused() {
            return m_daysAbsExcused;
        }

        /**
         * Gets the days abs unexcused.
         *
         * @return the m_daysAbsUnexcused
         */
        public Double getDaysAbsUnexcused() {
            return m_daysAbsUnexcused;
        }

        /**
         * Gets the days homebound.
         *
         * @return the m_daysHomebound
         */
        public Double getDaysHomebound() {
            return m_daysHomebound;
        }

        /**
         * Gets the days hospitalized.
         *
         * @return the m_daysHospitalized
         */
        public Double getDaysHospitalized() {
            return m_daysHospitalized;
        }

        /**
         * Gets the days present.
         *
         * @return the m_daysPresent
         */
        public Double getDaysPresent() {
            return m_daysPresent;
        }

        /**
         * Gets the enr date.
         *
         * @return the m_enrDate
         */
        public PlainDate getEnrDate() {
            return m_enrDate;
        }

        /**
         * Gets the month.
         *
         * @return the m_month
         */
        public String getMonth() {
            return m_month;
        }

        /**
         * Gets the skl code home.
         *
         * @return the m_sklCodeHome
         */
        public String getSklCodeHome() {
            return m_sklCodeHome;
        }

        /**
         * Gets the skl code serving.
         *
         * @return the m_sklCodeServing
         */
        public String getSklCodeServing() {
            return m_sklCodeServing;
        }

        /**
         * Sets the days abs excused.
         *
         * @param daysAbsExcused void
         */
        public void setDaysAbsExcused(Double daysAbsExcused) {
            this.m_daysAbsExcused = daysAbsExcused;
        }

        /**
         * Sets the days abs unexcused.
         *
         * @param daysAbsUnexcused void
         */
        public void setDaysAbsUnexcused(Double daysAbsUnexcused) {
            this.m_daysAbsUnexcused = daysAbsUnexcused;
        }

        /**
         * Sets the days homebound.
         *
         * @param daysHomebound void
         */
        public void setDaysHomebound(Double daysHomebound) {
            this.m_daysHomebound = daysHomebound;
        }

        /**
         * Sets the days hospitalized.
         *
         * @param daysHospitalized void
         */
        public void setDaysHospitalized(Double daysHospitalized) {
            this.m_daysHospitalized = daysHospitalized;
        }

        /**
         * Sets the days present.
         *
         * @param daysPresent void
         */
        public void setDaysPresent(Double daysPresent) {
            this.m_daysPresent = daysPresent;
        }

        /**
         * Sets the enr date.
         *
         * @param enrDate void
         */
        public void setEnrDate(PlainDate enrDate) {
            this.m_enrDate = enrDate;
        }

        /**
         * Sets the month.
         *
         * @param month void
         */
        public void setMonth(String month) {
            this.m_month = month;
        }

        /**
         * Sets the skl code home.
         *
         * @param sklCodeHome void
         */
        public void setSklCodeHome(String sklCodeHome) {
            this.m_sklCodeHome = sklCodeHome;
        }

        /**
         * Sets the skl code serving.
         *
         * @param sklCodeServing void
         */
        public void setSklCodeServing(String sklCodeServing) {
            this.m_sklCodeServing = sklCodeServing;
        }
    }

    /**
     * Entity class for IL Student Attendance File Format data.
     *
     * @author Follett Software Company
     */
    public static class ILStudentMonthlyAttEntity extends StateReportEntity {
        List<ILAttendanceRecord> m_records = null;

        /**
         * Initialize student, calculate membership and attendance counts.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_records = new ArrayList<>();
            Map<String, ILAttendanceRecord> tempRecordsMap = new HashMap<>();
            SisStudent student = (SisStudent) bean;
            ILStudentMonthlyAtt attData = (ILStudentMonthlyAtt) data;
            List<StudentAttendance> attItems = attData.m_studentHelper.getStudentAttendances(student.getOid());
            Collection<StudentEnrollmentSpan> spans = attData.m_studentHelper.getStudentEnrollmentSpans(student, true);
            for (Integer month : attData.m_datesForMonthMap.keySet()) {
                Pair<PlainDate, PlainDate> datesForMonth = attData.m_datesForMonthMap.get(month);
                for (StudentEnrollmentSpan span : spans) {
                    if (span.getFirstInactiveEnrollment() != null) {
                        String enrCode = span.getFirstInactiveEnrollment().getEnrollmentCode();
                        if (!StringUtils.isEmpty(enrCode)) {
                            String enrCodeState = data.lookupStateValue(StudentEnrollment.class,
                                    StudentEnrollment.COL_ENROLLMENT_CODE, enrCode);
                            if (!StringUtils.isEmpty(enrCodeState) && "99".equals(enrCodeState)) {
                                continue;
                            }
                        }
                    }
                    if (!(span.getLastActiveDate() == null
                            && span.getFirstActiveEnrollment().getEnrollmentDate().before(span.getFirstActiveDate()))
                            && !span.getFirstActiveEnrollment().getEnrollmentDate().after(datesForMonth.getRight())) {
                        Integer membershipDays =
                                attData.getMembershipDays(datesForMonth.getLeft(), datesForMonth.getRight(),
                                        student.getCalendarCode(),
                                        span);
                        if (membershipDays == null) {
                            continue;
                        }

                        double absExcused = .0;
                        double absUnexcused = .0;
                        double homebound = .0;
                        double hospitalized = .0;
                        if (attItems != null) {
                            for (StudentAttendance attItem : attItems) {
                                if (attItem.getSchoolOid().equals(span.getSchool().getOid())) {
                                    if (!attItem.getDate().before(datesForMonth.getLeft())
                                            && !attItem.getDate().after(datesForMonth.getRight())) {
                                        if (attData.m_isAttV2) {
                                            if (addPortionAbs(data, attItem, ATT_REASON_HOMEBOUND)) {
                                                homebound +=
                                                        attItem.getPortionAbsent() != null
                                                                ? attItem.getPortionAbsent().doubleValue()
                                                                : 1.0;
                                                continue;

                                            }
                                            if (addPortionAbs(data, attItem, ATT_REASON_HOSPITALIZED)) {
                                                hospitalized +=
                                                        attItem.getPortionAbsent() != null
                                                                ? attItem.getPortionAbsent().doubleValue()
                                                                : 1.0;
                                                continue;

                                            }
                                        }
                                        if (attItem.getExcusedIndicator()) {
                                            absExcused +=
                                                    attItem.getPortionAbsent() != null
                                                            ? attItem.getPortionAbsent().doubleValue()
                                                            : 1.0;
                                        } else {
                                            absUnexcused +=
                                                    attItem.getPortionAbsent() != null
                                                            ? attItem.getPortionAbsent().doubleValue()
                                                            : 1.0;
                                        }

                                    }
                                }
                            }
                        }
                        double daysPresent =
                                membershipDays.intValue() - absExcused - absUnexcused - homebound - hospitalized;
                        ILAttendanceRecord recordToAdd = attData.new ILAttendanceRecord();
                        if (daysPresent < 0) {
                            daysPresent = .0;
                        }
                        recordToAdd.setDaysPresent(Double.valueOf(daysPresent));
                        recordToAdd.setDaysAbsExcused(absExcused);
                        recordToAdd.setDaysAbsUnexcused(absUnexcused);
                        recordToAdd.setDaysHomebound(homebound);
                        recordToAdd.setDaysHospitalized(hospitalized);
                        StudentEnrollment enrToOperate = span.getFirstActiveEnrollment();
                        PlainDate enrDate = enrToOperate.getEnrollmentDate();
                        PlainDate startCtxDate = attData.getCurrentContext().getStartDate();
                        PlainDate enrDateToSet = !enrDate.after(startCtxDate) ? startCtxDate : enrDate;
                        recordToAdd.setEnrDate(enrDateToSet);
                        String recordKey = enrDateToSet.toString();
                        int monthToSetInt = month + 1;
                        String monthToSetStr =
                                monthToSetInt < 10 ? "0" + String.valueOf(monthToSetInt)
                                        : String.valueOf(monthToSetInt);
                        recordToAdd.setMonth(monthToSetStr);
                        recordKey = recordKey + month;
                        String sklCodeHome = getSklCodeHome(enrToOperate, attData, datesForMonth.getRight());
                        recordToAdd.setSklCodeHome(sklCodeHome);
                        recordKey = recordKey + sklCodeHome;
                        String sklCodeServ =
                                getSklCodeServing(enrToOperate, sklCodeHome, attData, datesForMonth.getRight());
                        recordToAdd.setSklCodeServing(sklCodeServ);
                        recordKey = recordKey + sklCodeServ;
                        if (!tempRecordsMap.containsKey(recordKey)) {
                            tempRecordsMap.put(recordKey, recordToAdd);
                        } else {
                            ILAttendanceRecord recordToIncrement = tempRecordsMap.get(recordKey);
                            recordToIncrement.setDaysAbsExcused(absExcused);
                            recordToIncrement.setDaysAbsUnexcused(absUnexcused);
                            recordToIncrement.setDaysHomebound(homebound);
                            recordToIncrement.setDaysHospitalized(hospitalized);
                            recordToIncrement.setDaysPresent(daysPresent);
                        }
                    }
                }
            }
            if (!tempRecordsMap.isEmpty()) {
                m_records.addAll(tempRecordsMap.values());
            }
            setRowCount(m_records.size());
            attData.m_totalCount += m_records.size();
        }

        /**
         * Gets current record.
         *
         * @return Demo dataset
         */
        public ILAttendanceRecord getCurrentRecord() {
            return m_records.get(getCurrentRow());
        }

        /**
         * Adds the portion abs.
         *
         * @param data StateReportData
         * @param att StudentAttendance
         * @param reason String
         * @return true, if successful
         */
        private boolean addPortionAbs(StateReportData data, StudentAttendance att, String reason) {
            boolean addPortion = false;
            if (att.getReasonCode() != null && reason.equals(data.lookupStateValue(StudentAttendance.class,
                    StudentAttendance.COL_REASON_CODE, att.getReasonCode()))) {
                addPortion = true;
            }
            return addPortion;
        }

        /**
         * Gets the skl code home.
         *
         * @param primaryEnrollment StudentEnrollment
         * @param data ILStudentMonthlyAtt
         * @param date PlainDate
         * @return String
         */
        private String getSklCodeHome(StudentEnrollment primaryEnrollment,
                                      ILStudentMonthlyAtt data,
                                      PlainDate date) {
            SisSchool primarySchool = primaryEnrollment.getSchool();
            String codeForNonFte = null;
            StudentEnrollment overridenEnr = null;
            // RCDTS Homeschool will ALWAYS be the student's primary school of enrollment.
            if (BooleanAsStringConverter.TRUE
                    .equals(primarySchool.getFieldValueByBeanPath(data.m_fieldSklNonCalcFte))) {
                if (StudentEnrollment.YOG_CHANGE.equals(primaryEnrollment.getEnrollmentType()) ||
                        StudentEnrollment.STATUS_CHANGE.equals(primaryEnrollment.getEnrollmentType())) {
                    overridenEnr = data.m_studentHelper.getEnrollmentForDate(
                            primaryEnrollment.getStudent().getOid(), date, StudentEnrollment.ENTRY);
                }
                if (overridenEnr != null) {
                    codeForNonFte = (String) overridenEnr.getFieldValueByBeanPath(data.m_fieldEnrSklHome);
                    if (!StringUtils.isEmpty(codeForNonFte)) {
                        codeForNonFte =
                                data.lookupStateValue(StudentEnrollment.class, data.m_fieldEnrSklHome,
                                        codeForNonFte);
                    }
                }
                if (StringUtils.isEmpty(codeForNonFte)) {
                    codeForNonFte = (String) primaryEnrollment.getFieldValueByBeanPath(data.m_fieldEnrSklHome);
                    if (!StringUtils.isEmpty(codeForNonFte)) {
                        codeForNonFte =
                                data.lookupStateValue(StudentEnrollment.class, data.m_fieldEnrSklHome,
                                        codeForNonFte);
                    }
                }
            }
            return !StringUtils.isEmpty(codeForNonFte) ? codeForNonFte
                    : (String) primaryEnrollment.getSchool().getFieldValueByBeanPath(data.m_fieldSklCode);
        }

        /**
         * Gets the skl code serving.
         *
         * @param primaryEnrollment StudentEnrollment
         * @param sklCodeHome String
         * @param data ILStudentMonthlyAtt
         * @param date PlainDate
         * @return String
         */
        private String getSklCodeServing(StudentEnrollment primaryEnrollment,
                                         String sklCodeHome,
                                         ILStudentMonthlyAtt data,
                                         PlainDate date) {
            String servingSchool = null;
            if (!BooleanAsStringConverter.TRUE
                    .equals(primaryEnrollment.getSchool().getFieldValueByBeanPath(data.m_fieldSklNonCalcFte))) {
                servingSchool = sklCodeHome;
            } else {
                if (StudentEnrollment.YOG_CHANGE.equals(primaryEnrollment.getEnrollmentType())
                        || StudentEnrollment.STATUS_CHANGE.equals(primaryEnrollment.getEnrollmentType())) {
                    StudentEnrollment overridenEnr =
                            data.m_studentHelper.getEnrollmentForDate(primaryEnrollment.getStudent().getOid(),
                                    date, StudentEnrollment.ENTRY);
                    if (overridenEnr != null) {
                        servingSchool = (String) overridenEnr.getFieldValueByBeanPath(data.m_fieldEnrSklService);
                        if (!StringUtils.isEmpty(servingSchool)) {
                            servingSchool =
                                    data.lookupStateValue(StudentEnrollment.class, data.m_fieldEnrSklService,
                                            servingSchool);
                        }
                    }
                }
            }
            return !StringUtils.isEmpty(servingSchool) ? servingSchool
                    : (String) primaryEnrollment.getSchool().getFieldValueByBeanPath(data.m_fieldSklCode);
        }
    }

    /**
     * Retrieve all data from ILAttenanceRecord for the export.
     *
     * @author Follett Software Company
     */
    protected class RetrieveAttData implements FieldRetriever {

        public static final String CAL_ID = "ATT-DATA";
        public static final String CAL_PARAM_ABS_EXC = "ABS_EXCUSED";
        public static final String CAL_PARAM_ABS_UNEXC = "ABS_UNEXCUSED";
        public static final String CAL_PARAM_DAYS_HOMEBOUND = "DAYS_HOMEBOUND";
        public static final String CAL_PARAM_DAYES_HOSPITALIZED = "DAYES_HOSPITALIZED";
        public static final String CAL_PARAM_ENR_DATE = "ENR_DATE";
        public static final String CAL_PARAM_MONTH = "MONTH";
        public static final String CAL_PARAM_PRESENT = "PRESENT";
        public static final String CAL_PARAM_SKL_HOME = "SKL_HOME";
        public static final String CAL_PARAM_SKL_SERV = "SKL_SERV";
        public static final String CAL_PARAM_YEAR = "YEAR";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            ILStudentMonthlyAttEntity attEntity = (ILStudentMonthlyAttEntity) entity;
            ILAttendanceRecord currentRecord = attEntity.getCurrentRecord();
            ILStudentMonthlyAtt attData = (ILStudentMonthlyAtt) data;
            Object value = null;
            if (CAL_PARAM_PRESENT.equals(param)) {
                value = currentRecord.getDaysPresent();
            } else if (CAL_PARAM_MONTH.equals(param)) {
                value = currentRecord.getMonth();
            } else if (CAL_PARAM_ABS_EXC.equals(param)) {
                value = currentRecord.getDaysAbsExcused();
            } else if (CAL_PARAM_ABS_UNEXC.equals(param)) {
                value = currentRecord.getDaysAbsUnexcused();
            } else if (CAL_PARAM_ENR_DATE.equals(param)) {
                value = currentRecord.getEnrDate();
            } else if (CAL_PARAM_YEAR.equals(param)) {
                String monthStr = currentRecord.getMonth();
                if (StringUtils.startsWith(monthStr, "0", true) && monthStr.length() > 1) {
                    monthStr = monthStr.substring(1);
                }
                Integer month = Integer.valueOf(monthStr);
                int year = getCurrentContext().getSchoolYear();
                if (month.intValue() >= 8 && month.intValue() <= 12) {
                    year--;
                }
                value = Integer.valueOf(year);
            } else if (CAL_PARAM_SKL_HOME.equals(param)) {
                value = currentRecord.getSklCodeHome();
            } else if (CAL_PARAM_SKL_SERV.equals(param)) {
                value = currentRecord.getSklCodeServing();
            }
            if (attData.m_isAttV2) {
                if (CAL_PARAM_DAYS_HOMEBOUND.equals(param)) {
                    value = currentRecord.getDaysHomebound();
                } else if (CAL_PARAM_DAYES_HOSPITALIZED.equals(param)) {
                    value = currentRecord.getDaysHospitalized();
                }
            }
            return value;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author Follett Software Company
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        public static final String CAL_ID = "ATT-CLEAN";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field)
                throws X2BaseException {
            String value = (String) getProperty(entity.getBean(), field.getBeanPath());
            Matcher matcher = ILLEGAL_NAME_CHARACTERS.matcher(value);
            return matcher.replaceAll("");
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_ENR_SKL_SERVICE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_SKL_NON_CALC_FTE = "DOE NON-CALCULATING FTE";
    protected static final String ALIAS_SKL_SCHOOL_ID = "DOE SCHOOL ID";

    /**
     * Input parameters.
     */
    public static final String INPUT_PARAM_MONTH_0 = "0";
    public static final Pattern ILLEGAL_NAME_CHARACTERS = Pattern.compile("[^-A-z ]");
    public static final String INPUT_PARAM_ATT_V2 = "isAttV2";

    /**
     * Other constants
     */
    public static final String ATT_REASON_HOMEBOUND = "Homebound";
    public static final String ATT_REASON_HOSPITALIZED = "Hospitalized";
    public static final String DEFAULT_CALENDAR_ID = "Standard";


    /**
     * Instance variables.
     */
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected Map<Integer, Pair<PlainDate, PlainDate>> m_datesForMonthMap;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSklHome;
    protected String m_fieldEnrSklService;
    protected String m_fieldSklCode;
    protected String m_fieldSklNonCalcFte;
    protected boolean m_isAttV2;
    protected PlainDate m_reportDate;
    protected StudentHistoryHelper m_studentHelper;
    protected int m_totalCount;

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        String exportType = m_isAttV2 ? "Student Attendance V2" : "Student Attendance";
        heading.append(exportType);
        heading.append(',');
        heading.append(m_totalCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }

    /**
     * Initialize the export.
     * Prepare the StudentHistoryHelper and retrievers.
     */
    @Override
    public void initialize() {
        initializeFields();
        initializeMonthDatesMap();

        // Prepare the StudentHistoryHelper.
        m_studentHelper = new StudentHistoryHelper(this);
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                getCurrentContext().getStartDate());
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.FALSE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_YOG,
                Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS,
                Boolean.TRUE);

        // Prepare the StateReportData.
        setQuery(m_studentHelper.getStudentQuery(false));
        setEntityClass(ILStudentMonthlyAttEntity.class);

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> retrievers = new HashMap<String, FieldRetriever>();
        retrievers.put(RetrieveAttData.CAL_ID, new RetrieveAttData());
        retrievers.put(RetrieveStripNameChar.CAL_ID, new RetrieveStripNameChar());
        super.addCalcs(retrievers);
    }

    /**
     * Return the number of days the student is in membership in the specified enrollment span.
     * <br>
     * This uses first and last active dates, the school calendar and student calendar ID to
     * identify membership days.
     *
     * @param startPeriodDate PlainDate
     * @param endPeriodDate PlainDate
     * @param calCode String
     * @param span StudentEnrollmentSpan
     * @return int
     */
    protected Integer getMembershipDays(PlainDate startPeriodDate,
                                        PlainDate endPeriodDate,
                                        String calCode,
                                        StudentEnrollmentSpan span) {
        Integer value = null;
        // Get the in session days for the school and calendar.
        Set<PlainDate> insessionDates = m_studentHelper.getCalendarDays(span.getSchool(), calCode);
        if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(calCode)) {
            insessionDates = m_studentHelper.getCalendarDays(span.getSchool(), DEFAULT_CALENDAR_ID);
        }
        if (insessionDates == null) {
            insessionDates = m_studentHelper.getCalendarDays(span.getSchool(), StudentHistoryHelper.CALENDAR_ANY);
        }
        if (insessionDates != null) {
            ArrayList<PlainDate> sortedDates = new ArrayList<>();
            sortedDates.addAll(insessionDates);
            Collections.sort(sortedDates);
            int membershipDays = 0;
            if (span.getLastActiveDate() != null && sortedDates != null && !sortedDates.isEmpty()
                    && !span.getLastActiveDate().after(sortedDates.iterator().next())) {
                return null;
            }
            PlainDate startDateToOperate = null;
            PlainDate endDateToOperate = null;
            PlainDate spanStartDate = span.getFirstActiveDate();
            if (span.getLastActiveDate() == null
                    || (span.getLastActiveDate().after(endPeriodDate)
                            && spanStartDate.before(endPeriodDate))) {
                endDateToOperate = endPeriodDate;
            } else if (span.getLastActiveDate().before(endPeriodDate)) {
                endDateToOperate = span.getLastActiveDate();
            }
            if (!spanStartDate.after(startPeriodDate)) {
                startDateToOperate = startPeriodDate;
            } else if (!spanStartDate.after(endPeriodDate)) {
                startDateToOperate = spanStartDate;
            }
            if (startDateToOperate != null && endDateToOperate != null) {
                for (PlainDate date : insessionDates) {
                    if (!date.before(startDateToOperate) && !date.after(endDateToOperate)
                            && DateUtils.isBetween(date, startPeriodDate, endPeriodDate)) {
                        membershipDays++;
                    }
                }
            }
            if (membershipDays > 0) {
                value = Integer.valueOf(membershipDays);
            }
        }
        return value;
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Initialize fields.
     */
    private void initializeFields() {
        // Get user parameters.
        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        m_fieldSklNonCalcFte = translateAliasToJavaName(ALIAS_SKL_NON_CALC_FTE, true);
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
        m_fieldEnrSklService = translateAliasToJavaName(ALIAS_ENR_SKL_SERVICE, true);
        m_fieldSklCode = translateAliasToJavaName(ALIAS_SKL_SCHOOL_ID, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_isAttV2 = getParameter(INPUT_PARAM_ATT_V2) == null ? false
                : ((Boolean) getParameter(INPUT_PARAM_ATT_V2)).booleanValue();
    }

    /**
     * Initialize month dates map by selected parameters. If not selected January for default.
     */
    private void initializeMonthDatesMap() {
        m_datesForMonthMap = new HashMap<>();
        Integer month = null;
        boolean monthSelected = false;
        for (int i = 0; i <= Calendar.DECEMBER; ++i) {
            if (getParameter(String.valueOf(i)) != null && ((Boolean) getParameter(String.valueOf(i))).booleanValue()) {
                monthSelected = true;
                month = Integer.valueOf(i);
                populateDates(month);
            }
        }
        if (m_datesForMonthMap.isEmpty() && !monthSelected) {
            month = Integer.valueOf(0);
            populateDates(month);
        }
    }

    /**
     * Populate dates according to the selected month.
     *
     * @param month Integer
     */
    private void populateDates(Integer month) {
        int year = getCurrentContext().getSchoolYear();
        if (month.intValue() >= Calendar.AUGUST) {
            year--;
        }
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(new PlainDate());
        calendar.set(year, month, 1);
        PlainDate date = new PlainDate(calendar.getTime());
        PlainDate startDateOfMonth = DateUtils.getFirstOfMonth(date.getTime());
        PlainDate endDateOfMonth = DateUtils.getLastOfMonth(date.getTime());
        if (startDateOfMonth.before(m_reportDate)) {
            if (endDateOfMonth.before(m_reportDate)) {
                m_datesForMonthMap.put(month,
                        Pair.of(startDateOfMonth, endDateOfMonth));
            } else {
                m_datesForMonthMap.put(month,
                        Pair.of(startDateOfMonth, m_reportDate));
            }
        }
    }
}
