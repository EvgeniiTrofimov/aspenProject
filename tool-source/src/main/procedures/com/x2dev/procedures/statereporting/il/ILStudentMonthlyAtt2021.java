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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
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
public class ILStudentMonthlyAtt2021 extends StateReportData {

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
        private Double m_daysPresentInPerson;
        private Double m_daysPresentRemote;
        private Double m_daysMembership;
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
         * Gets the days abs.
         *
         * @return the absences
         */
        public Double getDaysAbs() {
            return m_daysAbsExcused + m_daysAbsUnexcused + m_daysHomebound + m_daysHospitalized;
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
         * Gets the membership days.
         *
         * @return the m_daysPresent
         */
        public Double getMembershipDays() {
            return m_daysMembership;
        }

        /**
         * Gets the days present in person.
         *
         * @return the m_daysPresent
         */
        public Double getDaysPresentInPerson() {
            return m_daysPresentInPerson;
        }

        /**
         * Gets the days present remote.
         *
         * @return the m_daysPresent
         */
        public Double getDaysPresentRemote() {
            return m_daysPresentRemote;
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
         * Sets the membership days.
         *
         * @param membership
         */
        public void setMembershipDays(Double membership) {
            this.m_daysMembership = membership;
        }

        /**
         * Sets the days present in person.
         *
         * @param daysPresent void
         */
        public void setDaysPresentInPerson(Double daysPresent) {
            this.m_daysPresentInPerson = daysPresent;
        }

        /**
         * Sets the days present remote.
         *
         * @param daysPresent void
         */
        public void setDaysPresentRemote(Double daysPresent) {
            this.m_daysPresentRemote = daysPresent;
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
        ILStudentMonthlyAtt2021 m_data = null;
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
            m_data = (ILStudentMonthlyAtt2021) data;
            Collection<StudentEnrollmentSpan> spans = m_data.m_studentHelper.getStudentEnrollmentSpans(student, true);
            for (Integer month : m_data.m_datesForMonthMap.keySet()) {
                Pair<PlainDate, PlainDate> datesForMonth = m_data.m_datesForMonthMap.get(month);
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
                            && !span.getFirstActiveEnrollment().getEnrollmentDate().after(datesForMonth.getRight())
                            && !span.getFirstActiveDate().equals(span.getLastActiveDate())
                            && StringUtils.isEqual(
                                    (String) span.getSchool().getFieldValueByBeanPath(m_data.m_fieldDoeExcludeSkl),
                                    BooleanAsStringConverter.FALSE)
                            && (m_data.getSchool() == null
                                    || m_data.getSchool().getOid().equals(span.getSchool().getOid()))) {
                        Integer membershipDays =
                                m_data.getMembershipDays(datesForMonth.getLeft(), datesForMonth.getRight(),
                                        student.getCalendarCode(),
                                        span);
                        if (membershipDays == null) {
                            continue;
                        }

                        double remoteDays = .0;
                        double absExcused = .0;
                        double absUnexcused = .0;
                        double homebound = .0;
                        double hospitalized = .0;
                        for (StudentAttendance attItem : span.getStudentAttendance()) {
                            if (attItem.getSchoolOid().equals(span.getSchool().getOid())) {
                                if (!attItem.getDate().before(datesForMonth.getLeft())
                                        && !attItem.getDate().after(datesForMonth.getRight())) {
                                    if (isVirtual(attItem)) {
                                        if (attItem.getAbsentIndicator() == false) {
                                            remoteDays += 1.0;
                                        }
                                    } else {
                                        if (addPortionAbs(data, attItem, ATT_REASON_HOMEBOUND)) {
                                            homebound += 1.0;
                                            continue;

                                        }
                                        if (addPortionAbs(data, attItem, ATT_REASON_HOSPITALIZED)) {
                                            hospitalized += 1.0;
                                            continue;

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
                        double daysPresentInPerson =
                                membershipDays.intValue() - remoteDays - absExcused - absUnexcused - homebound
                                        - hospitalized;
                        ILAttendanceRecord recordToAdd = m_data.new ILAttendanceRecord();
                        if (daysPresentInPerson < 0) {
                            daysPresentInPerson = .0;
                        }
                        recordToAdd.setDaysPresentInPerson(Double.valueOf(daysPresentInPerson));
                        recordToAdd.setDaysPresentRemote(remoteDays);
                        recordToAdd.setMembershipDays(Double.valueOf(membershipDays));
                        recordToAdd.setDaysAbsExcused(absExcused);
                        recordToAdd.setDaysAbsUnexcused(absUnexcused);
                        recordToAdd.setDaysHomebound(homebound);
                        recordToAdd.setDaysHospitalized(hospitalized);
                        StudentEnrollment enrToOperate = span.getFirstActiveEnrollment();
                        PlainDate enrDate = enrToOperate.getEnrollmentDate();
                        PlainDate startCtxDate = m_data.getCurrentContext().getStartDate();
                        PlainDate enrDateToSet = !enrDate.after(startCtxDate) ? startCtxDate : enrDate;
                        recordToAdd.setEnrDate(enrDateToSet);
                        int monthToSetInt = month + 1;
                        String monthToSetStr =
                                monthToSetInt < 10 ? "0" + String.valueOf(monthToSetInt)
                                        : String.valueOf(monthToSetInt);
                        recordToAdd.setMonth(monthToSetStr);
                        String sklCodeHome = getSklCodeHome(enrToOperate, m_data);
                        recordToAdd.setSklCodeHome(sklCodeHome);
                        String sklCodeServ = getSklCodeServing(enrToOperate, m_data);
                        recordToAdd.setSklCodeServing(sklCodeServ);
                        m_records.add(recordToAdd);
                    }
                }
            }
            if (!tempRecordsMap.isEmpty()) {
                m_records.addAll(tempRecordsMap.values());
            }
            setRowCount(m_records.size());
            m_data.m_totalCount += m_records.size();
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
        private String getSklCodeHome(StudentEnrollment enrollment, ILStudentMonthlyAtt2021 data) {
            String value = null;
            String sklCode = (String) enrollment.getFieldValueByBeanPath(data.m_fieldEnrSklHome);
            if (!StringUtils.isEmpty(sklCode)) {
                value = data.lookupStateValue(StudentEnrollment.class, data.m_fieldEnrSklHome, sklCode);
            }
            if (StringUtils.isEmpty(value)) {
                value = (String) enrollment.getSchool().getFieldValueByBeanPath(data.m_fieldSklCode);
            }
            return value;
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
        private String getSklCodeServing(StudentEnrollment enrollment, ILStudentMonthlyAtt2021 data) {
            String value = null;
            String sklCode = (String) enrollment.getFieldValueByBeanPath(data.m_fieldEnrSklService);
            if (!StringUtils.isEmpty(sklCode)) {
                value = data.lookupStateValue(StudentEnrollment.class, data.m_fieldEnrSklService, sklCode);
            }
            if (StringUtils.isEmpty(value)) {
                value = (String) enrollment.getSchool().getFieldValueByBeanPath(data.m_fieldSklCode);
            }
            return value;
        }

        /**
         * Determine if attendance record has a virtual code.
         *
         * @param attendance StudentAttendance
         * @return True is either other code 1 or 2 are flagged as isVirtual
         */
        private boolean isVirtual(StudentAttendance attendance) {
            String other1 = attendance.getOtherCode();
            String other2 = attendance.getOtherCode02();
            String isVirtual = null;

            if (!StringUtils.isEmpty(other1)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other1);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.getIsVirtualField());
                }
            }

            if (!StringUtils.isEmpty(other2) && StringUtils.isEmpty(isVirtual)) {
                RefAttendanceStudent refCode = m_data.m_refAttendanceMap.get(other2);
                if (refCode != null) {
                    isVirtual = (String) refCode.getFieldValueByBeanPath(m_data.getIsVirtualField());
                }
            }

            return BooleanAsStringConverter.TRUE.equals(isVirtual);
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
        public static final String CAL_PARAM_INPERSON = "INPERSON";
        public static final String CAL_PARAM_REMOTE = "REMOTE";
        public static final String CAL_PARAM_ELEARN = "ELEARN";
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
            Object value = null;
            if (CAL_PARAM_INPERSON.equals(param)) {
                if (m_remoteLearning == true) {
                    value = new Double(0);
                } else {
                    value = currentRecord.getDaysPresentInPerson();
                }
            } else if (CAL_PARAM_REMOTE.equals(param)) {
                if (m_remoteLearning == true) {
                    value = currentRecord.getMembershipDays() - currentRecord.getDaysAbs();
                } else {
                    value = currentRecord.getDaysPresentRemote();
                }
            } else if (CAL_PARAM_ELEARN.equals(param)) {
                value = new Double(0);
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
            if (CAL_PARAM_DAYS_HOMEBOUND.equals(param)) {
                value = currentRecord.getDaysHomebound();
            } else if (CAL_PARAM_DAYES_HOSPITALIZED.equals(param)) {
                value = currentRecord.getDaysHospitalized();
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
    protected static final String ALIAS_DOE_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_RAT_VIRTUAL_FLAG = "all-rat-IsVirtual";

    /**
     * Input parameters.
     */
    public static final String INPUT_PARAM_MONTH_0 = "0";
    public static final Pattern ILLEGAL_NAME_CHARACTERS = Pattern.compile("[^-A-z ]");
    public static final String INPUT_PARAM_REMOTE_LEARNING = "remoteLearning";

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
    protected Map<String, RefAttendanceStudent> m_refAttendanceMap = null;
    protected String m_fieldDistrictCode;
    protected String m_fieldDoeExcludeSkl;
    protected String m_fieldEnrSklHome;
    protected String m_fieldEnrSklService;
    protected String m_fieldSklCode;
    protected String m_fieldSklNonCalcFte;
    protected String m_fieldRatIsVirtual;

    protected boolean m_remoteLearning;
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
        heading.append("Student Attendance");
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
        initializeRefAttendanceMap();

        // Prepare the StudentHistoryHelper.
        m_studentHelper = new StudentHistoryHelper(this);
        m_studentHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE,
                getCurrentContext().getStartDate());
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_studentHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_ATTENDANCE, Boolean.TRUE);
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
     * Gets the checks if is virtual field.
     *
     * @return String
     */
    public String getIsVirtualField() {
        if (m_fieldRatIsVirtual == null) {
            m_fieldRatIsVirtual = translateAliasToJavaName(ALIAS_RAT_VIRTUAL_FLAG, true);
        }

        return m_fieldRatIsVirtual;
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
        m_remoteLearning = getParameter(INPUT_PARAM_REMOTE_LEARNING) == null ? false
                : ((Boolean) getParameter(INPUT_PARAM_REMOTE_LEARNING)).booleanValue();
        m_fieldDoeExcludeSkl = translateAliasToJavaName(ALIAS_DOE_EXCLUDE_SKL, true);
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
     * Initialize a map of Daily Attendance Reference Codes.
     */
    private void initializeRefAttendanceMap() {
        DataDictionaryField isVirtualField =
                getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_RAT_VIRTUAL_FLAG);
        X2Criteria criteria = new X2Criteria();

        criteria.addEqualTo(RefAttendanceStudent.COL_ATTENDANCE_TYPE, RefAttendanceStudent.ATTENDANCE_TYPE_DAILY);
        criteria.addEqualTo(RefAttendanceStudent.COL_CODE_TYPE, RefAttendanceStudent.TYPE_OTHER_CODE);
        if (isVirtualField != null) {
            criteria.addEqualTo(isVirtualField.getJavaName(), BooleanAsStringConverter.TRUE);
        }

        BeanQuery query = new BeanQuery(RefAttendanceStudent.class, criteria);

        m_refAttendanceMap = getBroker().getMapByQuery(query, RefAttendanceStudent.COL_ATTENDANCE_CODE, 8);
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