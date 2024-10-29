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
package com.x2dev.procedures.statereporting.ri;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.OrganizationDefinition;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.RefAttendanceStudent;
import com.x2dev.sis.model.beans.SisOrganization;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentAttendanceTime;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Rhode Island state report for Attendance.
 * This class implements the data export for RI Attendance export.
 *
 * @author X2 Development Corporation
 */
public class RIAttendance extends RIStateReportData {

    /**
     * The Class AttendanceEntity.
     */
    public static class AttendanceEntity extends StateReportEntity {

        private List<AttendanceRecord> m_records = new ArrayList<>();

        /*
         * Cached values for retrievers to share.
         */

        /**
         * Instantiates a new attendance entity.
         */
           /*
            * Public no argument constructor for dynamic instantiation.
            */
        public AttendanceEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Initialize the entity for the student bean provided.
         * This method finds the student schedule and student schedule change records for the
         * student
         * and generates a list of reportable schedule items.
         * The entity can produce multiple rows from these results.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#intitialize(com.x2dev.sis.tools.
         *      stateexports.StateReportData, com.x2dev.sis.model.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            RIAttendance attData = (RIAttendance) data;
            SisStudent std = (SisStudent) bean;
            String stdOid = std.getOid();
            Collection<StudentAttendance> attendances = attData.m_attMapByStdOid.get(stdOid);
            Collection<AttendanceRecord> recordsToExport = new ArrayList<>();
            Set<PlainDate> datesIncluded = new HashSet<>();
            Collection<StudentEnrollmentSpan> spans = attData.m_stdHelper.getStudentEnrollmentSpans(std, true)
                    .stream()
                    .filter(span -> span.getSchool() != null
                            && (StringUtils.isEmpty(attData.m_sklOidToReport)
                                    || span.getSchool().getOid().equals(attData.m_sklOidToReport))
                            && PreferenceManager
                                    .getPreferenceValue(data.getOrganization(),
                                            SystemPreferenceDefinition.STUDENT_ACTIVE_CODE)
                                    .equals(span.getFirstActiveEnrollment().getStatusCode())
                            && !BooleanAsStringConverter.TRUE.equals(span.getFirstActiveEnrollment()
                                    .getSchool().getFieldValueByBeanPath(attData.m_excludeSchoolEnrField))
                            && !BooleanAsStringConverter.TRUE.equals(span.getFirstActiveEnrollment()
                                    .getFieldValueByBeanPath(attData.m_fieldEnrExclude)))
                    .collect(Collectors.toList());
            if (spans != null && !spans.isEmpty()) {
                if (attendances != null) {
                    Collection<StudentAttendance> filteredAtts = new ArrayList<StudentAttendance>();
                    for (StudentAttendance attToCheck : attendances) {
                        for (StudentEnrollmentSpan spanToCheck : spans) {
                            if (attToCheck.getSchoolOid()
                                    .equals(spanToCheck.getFirstActiveEnrollment().getSchool().getOid())
                                    && !spanToCheck.getFirstActiveEnrollment().getEnrollmentDate()
                                            .after(attToCheck.getDate())
                                    && (spanToCheck.getFirstInactiveEnrollment() == null
                                            || !spanToCheck.getFirstInactiveEnrollment().getEnrollmentDate()
                                                    .before(attToCheck.getDate()))) {
                                filteredAtts.add(attToCheck);
                                break;
                            }
                        }
                    }
                    for (StudentAttendance att : filteredAtts) {
                        String codeView = att.getCodeView();
                        String codeReason = att.getReasonCode();
                        String otherCode = null;
                        PlainDate attDate = att.getDate();
                        if (!StringUtils.isEmpty(codeReason)
                                && !attData.m_attendanceReasonCodes.containsKey(codeReason)) {
                            continue;
                        } else if (!att.getAbsentIndicator() && !att.getDismissedIndicator()
                                && !att.getTardyIndicator()) {
                            otherCode = att.getOtherCode();
                        }
                        if (!StringUtils.isEmpty(otherCode)) {
                            codeView = otherCode;
                        } else if (att.getAbsentIndicator() && StringUtils.isEmpty(codeReason)) {
                            codeView = "A";
                        }
                        if (att.getTardyIndicator()) {
                            boolean isExcused = att.getExcusedIndicator();
                            if (att.getAttendanceTimes() != null && !att.getAttendanceTimes().isEmpty()) {
                                StudentAttendanceTime atm = att.getAttendanceTimes().stream()
                                        .filter(stdAtm -> stdAtm.getArrivalIndicator())
                                        .findFirst().orElse(null);
                                if (atm != null) {
                                    isExcused = atm.getExcusedIndicator();
                                }
                            }
                            AttendanceRecord attRecord =
                                    attData.new AttendanceRecord(attDate, "T", codeReason, isExcused);
                            recordsToExport.add(attRecord);
                        }
                        if (att.getDismissedIndicator()) {
                            boolean isExcused = att.getExcusedIndicator();
                            if (att.getAttendanceTimes() != null && !att.getAttendanceTimes().isEmpty()) {
                                StudentAttendanceTime atm = att.getAttendanceTimes().stream()
                                        .filter(stdAtm -> !stdAtm.getArrivalIndicator())
                                        .findFirst().orElse(null);
                                if (atm != null) {
                                    isExcused = atm.getExcusedIndicator();
                                }
                            }
                            AttendanceRecord attRecord =
                                    attData.new AttendanceRecord(attDate, "D", codeReason, isExcused);
                            recordsToExport.add(attRecord);
                        }
                        if (!att.getTardyIndicator() && !att.getDismissedIndicator()) {
                            AttendanceRecord attRecord =
                                    attData.new AttendanceRecord(attDate, codeView, codeReason,
                                            att.getExcusedIndicator());
                            recordsToExport.add(attRecord);
                        }
                        datesIncluded.add(attDate);
                    }
                }

                if (attData.m_includePresentRecords) {
                    String calendarCode = std.getCalendarCode();
                    String alternateCodeView = attData.m_inputPresentCode;
                    Set<PlainDate> membDates = attData.getMembershipDaysForCalendar(std.getSchool(), calendarCode)
                            .stream()
                            .filter(membDate -> DateUtils.isBetween(membDate, attData.m_startDate, attData.m_endDate))
                            .collect(Collectors.toSet());
                    if (membDates != null && !membDates.isEmpty()) {
                        for (PlainDate date : membDates) {
                            if (!datesIncluded.contains(date)) {
                                for (StudentEnrollmentSpan span : spans) {
                                    PlainDate enrDate = span.getFirstActiveEnrollment().getEnrollmentDate();
                                    PlainDate wDate = null;
                                    if (span.getFirstInactiveEnrollment() != null) {
                                        wDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
                                    }
                                    if (!enrDate.after(date) && (wDate == null || !wDate.before(date))) {
                                        StudentEnrollment stdEnr =
                                                attData.m_stdHelper.getEnrollmentForDate(stdOid, date, "E");
                                        if (stdEnr != null && stdEnr.getSchool() != null
                                                && !BooleanAsStringConverter.TRUE.equals(stdEnr.getSchool()
                                                        .getFieldValueByBeanPath(attData.m_excludeSchoolEnrField))
                                                && !BooleanAsStringConverter.TRUE.equals(stdEnr
                                                        .getFieldValueByBeanPath(attData.m_fieldEnrExclude))) {
                                            AttendanceRecord attRecord =
                                                    attData.new AttendanceRecord(date, alternateCodeView, null, null);
                                            recordsToExport.add(attRecord);
                                            datesIncluded.add(date);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            m_records.addAll(recordsToExport);
            Collections.sort(m_records);
            setRowCount(m_records.size());
        }

        /**
         * Returns an entity name for the validation report.
         *
         * @return String
         */
        @Override
        public String getEntityName() {
            AttendanceRecord attRecord = getCurrentAttRecord();
            SisStudent student = (SisStudent) getBean();

            StringBuilder entityName = new StringBuilder();
            entityName.append(attRecord.getAttDate().toString());
            entityName.append(" LASID: ");
            entityName.append(student.getLocalId());
            entityName.append(" Code: ");
            entityName.append(attRecord.getCodeView());
            entityName.append(" Reason: ");
            entityName.append(attRecord.getReasonCode() != null ? attRecord.getReasonCode() : "__dummy__");

            return entityName.toString();
        }

        /**
         * Check if the attendance entry is reportable.
         * Reportable codes have state code values.
         *
         * @return StateReportValidationError
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = super.filterEntity();
            RIAttendance attData = (RIAttendance) getData();
            String stdOid = getBean().getOid();
            AttendanceRecord attRecord = getCurrentAttRecord();
            PlainDate attDate = attRecord.getAttDate();
            String codeAtt = attRecord.getCodeView();
            String codeReason = attRecord.getReasonCode();
            String stdExcEnr = BooleanAsStringConverter.FALSE;
            boolean correctCodeView = false;
            if (attData.m_includePresentRecords) {
                correctCodeView = !StringUtils.isEmpty(codeAtt);
            } else {
                correctCodeView = true;
            }

            boolean correctCodeReason = false;
            if (attData.m_includePresentRecords) {
                correctCodeReason = codeReason != null ? attData.m_attendanceReasonCodes.containsKey(codeReason) : true;
            } else {
                correctCodeReason = true;
            }
            if (!(correctCodeView || correctCodeReason)) {
                error = new StateReportValidationError(this,
                        getData().getFieldDefinition("ATTEND_TYPE"),
                        "Non-reportable attendance entry", null);
                return error;
            }
            if (attDate != null && !StringUtils.isEmpty(stdOid)) {
                StudentEnrollment stdEnr =
                        attData.m_stdHelper.getEnrollmentForDate(stdOid, attDate, "E");
                if (stdEnr != null) {
                    stdExcEnr = (String) stdEnr.getFieldValueByAlias(DOE_EXLUDE_ENR);
                    if (BooleanAsStringConverter.TRUE.equals(stdExcEnr)) {
                        error = new StateReportValidationError(this, getData().getFieldDefinition("ATTEND_TYPE"),
                                "Non-reportable attendance entry", null);
                    }
                }
            }
            return error;
        }

        /**
         * Gets the current att record.
         *
         * @return Attendance record
         */
        public AttendanceRecord getCurrentAttRecord() {
            return m_records.get(getCurrentRow());
        }

        /**
         * Override toString to return identifying information.
         *
         * @return String
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return getEntityName();
        }
    }


    /**
     * The Class AttendanceRecord.
     */
    public class AttendanceRecord implements Comparable<AttendanceRecord> {
        PlainDate m_attDate;
        String m_codeView;
        Boolean m_excused;
        String m_reasonCode;

        /**
         * Instantiates a new attendance record.
         *
         * @param attDate PlainDate
         * @param codeView String
         * @param reasonCode String
         * @param excused Boolean
         */
        AttendanceRecord(PlainDate attDate, String codeView, String reasonCode, Boolean excused) {
            super();
            this.m_attDate = attDate;
            this.m_codeView = codeView;
            this.m_excused = excused;
            this.m_reasonCode = reasonCode;
        }

        /**
         * Gets the att date.
         *
         * @return the m_attDate
         */
        public PlainDate getAttDate() {
            return m_attDate;
        }


        /**
         * Gets the code view.
         *
         * @return the m_codeView
         */
        public String getCodeView() {
            return m_codeView;
        }

        /**
         * Gets the excused.
         *
         * @return Boolean
         */
        public Boolean getExcused() {
            return m_excused;
        }

        /**
         * Gets the reason code.
         *
         * @return the m_reasonCode
         */
        public String getReasonCode() {
            return m_reasonCode;
        }

        /**
         * Compare to.
         *
         * @param o AttendanceRecord
         * @return int
         * @see java.lang.Comparable#compareTo(java.lang.Object)
         */
        @Override
        public int compareTo(AttendanceRecord o) {
            int retValue = m_attDate.compareTo(o.getAttDate());
            if (retValue == 0) {
                retValue = m_codeView.compareTo(o.getCodeView());
            }
            return retValue;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "Date: [" + getAttDate() + "] Code: [" + getCodeView() + "] Reason: [" + getReasonCode()
                    + "] Excused: [" + getExcused() + "]";
        }

    }

    /**
     * Retrieve the attendance type code from the attendance code or reason code.
     */
    protected class RetrieveAttendanceInfo implements FieldRetriever {
        private static final String CALC_ID = "ATTENDANCE";
        private static final String CALC_PARAM_DATE = "ATT_DATE";
        private static final String CALC_PARAM_TYPE = "ATT_TYPE";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {

            String calcParam = (String) field.getParameter();
            Object value = null;
            AttendanceEntity attEntity = (AttendanceEntity) entity;
            AttendanceRecord currentRecord = attEntity.getCurrentAttRecord();
            if (CALC_PARAM_DATE.equals(calcParam)) {
                value = DATE_FORMAT.format(currentRecord.getAttDate());

            } else if (CALC_PARAM_TYPE.equals(calcParam)) {
                String stateCode = null;
                String attendanceCode = currentRecord.getCodeView();
                String reasonCode = currentRecord.getReasonCode();
                if (m_attendanceTypeCodes.containsKey(attendanceCode)) {
                    stateCode = m_attendanceTypeCodes.get(attendanceCode);
                }
                // if attendance reason is setup, use the state code here
                if (m_attendanceReasonCodes.containsKey(reasonCode)) {
                    stateCode = m_attendanceReasonCodes.get(reasonCode);
                } else if (StringUtils.isEmpty(reasonCode)) {
                    if (currentRecord.getExcused() != null) {
                        if ("A".equals(attendanceCode) && !currentRecord.getExcused().booleanValue()) {
                            stateCode = "2K";
                        } else if ("T".equals(attendanceCode) && !currentRecord.getExcused().booleanValue()) {
                            stateCode = "3K";
                        } else if ("D".equals(attendanceCode) && !currentRecord.getExcused().booleanValue()) {
                            stateCode = "4K";
                        } else if ("A".equals(attendanceCode) && currentRecord.getExcused().booleanValue()) {
                            stateCode = "2L";
                        } else if ("T".equals(attendanceCode) && currentRecord.getExcused().booleanValue()) {
                            stateCode = "3L";
                        } else if ("D".equals(attendanceCode) && currentRecord.getExcused().booleanValue()) {
                            stateCode = "4L";
                        }
                    } else if (m_includePresentRecords) {
                        stateCode = attendanceCode;
                    }
                }
                value = stateCode;
            }
            return value;
        }
    }


    /**
     * Retrieve the school id based on primary school for enrollment for the incident date.
     */
    protected class RetrieveId implements FieldRetriever {
        private static final String CALC_ID = "CODE-RETRIEVER";
        private static final String CALC_PARAM_CODE_DISTR = "CODE_DISTR";
        private static final String CALC_PARAM_CODE_SKL = "CODE_SKL";

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data,
                                    StateReportEntity entity,
                                    FieldDefinition field) {
            String idToReturn = null;
            AttendanceRecord currentRecord = ((AttendanceEntity) entity).getCurrentAttRecord();
            PlainDate attDate = currentRecord.getAttDate();
            String stdOid = entity.getBean().getOid();
            String param = (String) field.getParameter();
            if (attDate != null && !StringUtils.isEmpty(stdOid)) {
                StudentEnrollment stdEnr = m_stdHelper.getEnrollmentForDate(stdOid, attDate, "E");
                SisSchool skl = null;
                if (stdEnr != null && (skl = stdEnr.getSchool()) != null) {
                    if (CALC_PARAM_CODE_DISTR.equals(param)) {
                        String adjustedDistrCode = (String) skl.getFieldValueByBeanPath(m_sklFieldAdjDistr);
                        idToReturn = !StringUtils.isEmpty(adjustedDistrCode)
                                ? adjustedDistrCode
                                : (String) skl.getOrganization1().getFieldValueByBeanPath(m_districtIdField);

                    } else if (CALC_PARAM_CODE_SKL.equals(param)) {
                        idToReturn = (String) skl.getFieldValueByBeanPath(m_sklIdField);
                    }
                }
            }
            return idToReturn;
        }
    }

    /**
     * Constants
     */
    private static final String ALIAS_ENR_EXCLUDE = "DOE EXCLUDE ENR";
    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("MM/dd/yyyy");
    private static final String DEFAULT_CALENDAR_ID = "Standard";
    private static final String DOE_DISTRICT_ID = "State RIDE District Code";
    private static final String DOE_EXLUDE_ENR = "DOE EXCLUDE ENR";
    private static final String DOE_EXCLUDE_FROM_ATTENDANCE_RPT = "DOE EXCLUDE FROM ATTENDANCE RPT";
    private static final String EXCLUDE_ALIAS_STD = "DOE EXCLUDE STD";
    private static final String ORGANIZATION_PARAM = "orgOid";
    private static final String RI_ATTENDANCE_STATE_CODE = "RI Attendance State Code";

    /**
     * Input parameters/
     */
    public static final String INPUT_PARAM_ATT_CODE = "codePresent";
    public static final String INPUT_PARAM_END_DATE = "attendanceEndDate";
    public static final String INPUT_PARAM_PRESENT_REC = "generatePresent";
    public static final String INPUT_PARAM_SASID_STD_ONLY = "sasidStudentsOnly";
    public static final String INPUT_PARAM_START_DATE = "attendanceStartDate";

    /**
     * Class members.
     */
    protected Map<String, Collection<StudentAttendance>> m_attMapByStdOid;
    protected HashMap<String, String> m_attendanceReasonCodes;
    protected HashMap<String, String> m_attendanceTypeCodes;
    protected Iterator m_dataIterator;
    protected PlainDate m_endDate;
    protected String m_excludeSchoolEnrField;
    protected String m_excludeStdField;
    protected String m_fieldEnrExclude;
    protected boolean m_includePresentRecords = false;
    protected String m_inputPresentCode;
    protected Map<String, Set<PlainDate>> m_membershipDates = new HashMap<>();
    protected String m_orgFieldStr;
    protected String m_orgOid;
    protected String m_riStateCode;
    protected boolean m_sasidStudentOnly;
    protected String m_sklOidToReport;
    protected PlainDate m_startDate;
    protected StudentHistoryHelper m_stdHelper;

    /**
     * Get the heading from the export format, and add the district code to it.
     *
     * @return String
     * @see com.x2dev.sis.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        StringBuilder sb = new StringBuilder(super.getHeading());

        String code = (String) getOrganization().getFieldValueByAlias(DOE_DISTRICT_ID);
        if (StringUtils.isEmpty(code)) {
            code = "[INSERT DISTRICT ID HERE]";
            addSetupError("Using a placeholder for the district ID.",
                    "Set the " + STYLE_BOLD + DOE_DISTRICT_ID + STYLE_END +
                            " alias in the Data Dictionary and update that field with the correct ID.");
        } else {
            sb.append(code);
        }

        return sb.toString();
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException exception
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.x2dev.sis.model.business.X2Broker)
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();

        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);
        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);

        if (m_startDate == null) {
            m_startDate = new PlainDate();
        }
        if (m_endDate == null) {
            m_endDate = new PlainDate();
        }

        if (isSchoolContext()) {
            m_sklOidToReport = getSchool().getOid();
        } else {
            m_sklOidToReport = "";
        }

        m_stdHelper = new StudentHistoryHelper(this);
        m_stdHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL, Boolean.TRUE);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_endDate);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_stdHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_SPAN_BREAK_ON_STATUS,
                Boolean.TRUE);

        m_attendanceTypeCodes = new HashMap<String, String>();
        m_attendanceReasonCodes = new HashMap<String, String>();

        // this is the State Code alias on the 'Reference Code - Student Attendance' table
        // optional in case the district is reporting state codes via the attendance reasons
        m_riStateCode = translateAliasToJavaName(RI_ATTENDANCE_STATE_CODE, false);
        m_excludeStdField = translateAliasToJavaName(EXCLUDE_ALIAS_STD, false);
        m_excludeSchoolEnrField = translateAliasToJavaName(DOE_EXCLUDE_FROM_ATTENDANCE_RPT, false);
        m_fieldEnrExclude = translateAliasToJavaName(ALIAS_ENR_EXCLUDE, true);

        m_sasidStudentOnly = true;
        Boolean sasidStudentOnly = (Boolean) getParameter(INPUT_PARAM_SASID_STD_ONLY);
        if (sasidStudentOnly != null) {
            m_sasidStudentOnly = sasidStudentOnly.booleanValue();
        }
        m_includePresentRecords = getParameter(INPUT_PARAM_PRESENT_REC) != null
                && ((Boolean) getParameter(INPUT_PARAM_PRESENT_REC)).booleanValue();
        if (m_includePresentRecords) {
            m_inputPresentCode = (String) getParameter(INPUT_PARAM_ATT_CODE);
            if (StringUtils.isEmpty(m_inputPresentCode)) {
                addSetupError("Missing input.", "Code for Present records is required if it is enabled.");
            } else if (m_inputPresentCode.length() > 2) {
                m_inputPresentCode = m_inputPresentCode.substring(0, 2);
            }
        }
        // Get organization Oid and level and field.
        m_orgOid = (String) getParameter(ORGANIZATION_PARAM);
        SisOrganization organization = null;
        OrganizationDefinition def = null;
        if (!StringUtils.isEmpty(m_orgOid)) {
            organization = (SisOrganization) getBroker().getBeanByOid(SisOrganization.class, m_orgOid);
            if (organization != null) {
                def = organization.getOrganizationDefinition();
                m_orgFieldStr = "organization" + Integer.toString(def.getLevel() + 1) + "Oid";
            }
        }

        /*
         * Build maps of attendance codes and reasons.
         */
        Criteria refCodesCriteria = getAttendanceReferenceCodesCriteria();
        QueryByCriteria refCodesQuery = new QueryByCriteria(RefAttendanceStudent.class, refCodesCriteria);
        QueryIterator iterator = getBroker().getIteratorByQuery(refCodesQuery);

        while (iterator.hasNext()) {
            RefAttendanceStudent refCode = (RefAttendanceStudent) iterator.next();

            if (refCode != null) {
                m_attendanceTypeCodes.put(refCode.getAttendanceCode(),
                        (String) refCode.getFieldValueByBeanPath(m_riStateCode));
            }
        }

        Criteria attendanceReasonRefCodeCriteria = getAttendanceReasonCodesCriteria();
        if (!attendanceReasonRefCodeCriteria.isEmpty()) {
            QueryByCriteria reasonCodesQuery = new QueryByCriteria(ReferenceCode.class,
                    attendanceReasonRefCodeCriteria);
            QueryIterator iterator2 = getBroker().getIteratorByQuery(reasonCodesQuery);
            while (iterator2.hasNext()) {
                ReferenceCode attendanceReasonRefCode = (ReferenceCode) iterator2.next();
                if (attendanceReasonRefCode != null) {
                    m_attendanceReasonCodes.put(attendanceReasonRefCode.getCode(),
                            attendanceReasonRefCode.getStateCode());
                }
            }
        }

        if (m_attendanceTypeCodes.size() == 0 && m_attendanceReasonCodes.size() == 0) {
            addSetupError("Missing state codes",
                    "No state codes are set on the student attendance reference table OR the student attendance reason reference table ");
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            // If "Student must have SASID" is checked, get only students with non-empty state ids
            if (m_sasidStudentOnly) {
                m_stdHelper.getStudentCriteria().addNotEmpty(SisStudent.COL_STATE_ID, getBroker().getPersistenceKey());
            }

            Criteria studentAttendanceCriteria = getStudentAttendanceCriteria();
            QueryByCriteria studentAttendanceQuery = new QueryByCriteria(StudentAttendance.class,
                    studentAttendanceCriteria);
            studentAttendanceQuery.addOrderBy(StudentAttendance.COL_DATE, true);
            m_attMapByStdOid = getBroker().getGroupedCollectionByQuery(studentAttendanceQuery,
                    StudentAttendance.COL_STUDENT_OID, 1024);

            // Set the query to be used for student selection.
            setQuery(m_stdHelper.getStudentQuery(true));
            setEntityClass(AttendanceEntity.class);

            // Build maps of retriever functions and validator functions
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveAttendanceInfo.CALC_ID, new RetrieveAttendanceInfo());
            calcs.put(RetrieveId.CALC_ID, new RetrieveId());
            HashMap validators = new HashMap<String, FieldRetriever>();

            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#close()
     */
    @Override
    public void close() {
        // nothing needed for collection iterator
    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#next()
     */
    @Override
    public StateReportEntity next() throws X2BaseException {
        StateReportEntity entity = null;

        // See if the current entity can iterate more. If not, abandon it.
        if (m_currentEntity != null) {
            if (m_currentEntity.getCurrentRow() + 1 < m_currentEntity.getRowCount()) {
                m_currentEntity.setCurrentRow(m_currentEntity.getCurrentRow() + 1);
                entity = m_currentEntity;
            } else {
                m_currentEntity = null;
            }
        }

        /*
         * If multiple queries are used, and the current query/iterator is exhausted,
         * open the next available query/iterator.
         */
        boolean iteratorHasNext = false;
        if (m_dataIterator != null) {
            iteratorHasNext = m_dataIterator.hasNext();
        }
        /*
         * If the last entity was exhausted, get another from the iterator.
         * Entities may generate zero rows, if so skip them and get the
         * next from the iterator until it is exhausted too.
         */
        while (entity == null && getEntityClass() != null && m_dataIterator != null && iteratorHasNext) {
            X2BaseBean bean = (X2BaseBean) m_dataIterator.next();
            if (bean != null) {
                try {
                    entity = (StateReportEntity) getEntityClass().getDeclaredConstructor().newInstance();
                } catch (ClassCastException | InstantiationException | IllegalAccessException | IllegalArgumentException
                        | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                    throw new X2BaseException(e);
                }

                if (entity != null) {
                    entity.intitialize(this, bean);
                    if (entity.getRowCount() > 0) {
                        entity.setCurrentRow(0);
                        m_currentEntity = entity;
                    } else {
                        entity = null;
                    }
                }
            }

            iteratorHasNext = m_dataIterator.hasNext();
        }

        // Get the correct fields set for this entity/iteration based on the entity requested
        // definition Id.
        if (entity != null) {
            String fieldKey = entity.getCurrentFormatDefinitionId();
            m_currentFieldDefinitions = m_loadedFieldDefinitions.get(fieldKey);
            if (m_currentFieldDefinitions == null) {
                m_currentFieldDefinitions = new ArrayList<FieldDefinition>();
            }
        }

        return entity;

    }

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#open()
     */
    @Override
    public boolean open() {
        QueryByCriteria query = getQuery();
        if (query == null) {
            return false;
        }
        m_dataIterator = getBroker().getCollectionByQuery(query).iterator();
        return (m_dataIterator != null);
    }

    /**
     * Returns criteria for the attendance reason reference codes.
     *
     * @return Criteria
     */
    private Criteria getAttendanceReasonCodesCriteria() {
        String reasonCode = StudentAttendance.COL_REASON_CODE;
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(StudentAttendance.class.getName(), reasonCode);

        Criteria refTableCriteria = new Criteria();
        if (field.hasReferenceTable()) {
            String referenceTableOid = field.getDataFieldConfig().getReferenceTableOid();

            refTableCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            refTableCriteria.addNotNull(ReferenceCode.COL_STATE_CODE);
            refTableCriteria.addNotEqualTo(ReferenceCode.COL_STATE_CODE, "");
        }

        return refTableCriteria;
    }

    /**
     * Returns criteria for the attendance code reference codes.
     *
     * @return Criteria
     */
    private Criteria getAttendanceReferenceCodesCriteria() {
        X2Criteria criteria = new X2Criteria();

        // there is not state code column on the Reference Code - Student Attendance table
        String stateCodeColumn = translateAliasToJavaName(RI_ATTENDANCE_STATE_CODE, true);
        criteria.addNotNull(stateCodeColumn);

        return criteria;
    }

    /**
     * Gets the membership days for calendar.
     *
     * @param school SisSchool
     * @param calendarCode String
     * @return Sets the
     */
    private Set<PlainDate> getMembershipDaysForCalendar(SisSchool school,
                                                        String calendarCode) {
        String mapKey = school.getOid() + calendarCode;
        Set<PlainDate> datesToReturn = m_membershipDates.get(mapKey);
        if (datesToReturn == null) {
            datesToReturn = new HashSet<>();
            m_membershipDates.put(mapKey, datesToReturn);
            Set<PlainDate> insessionDates = m_stdHelper.getCalendarDays(school, calendarCode);
            if (insessionDates == null && !DEFAULT_CALENDAR_ID.equals(calendarCode)) {
                insessionDates = m_stdHelper.getCalendarDays(school, DEFAULT_CALENDAR_ID);
            }
            if (insessionDates != null) {
                for (PlainDate date : insessionDates) {
                    if (DateUtils.isBetween(date, m_startDate, m_endDate)) {
                        datesToReturn.add(date);
                    }
                }
            }
        }
        return datesToReturn;
    }

    /**
     * Returns a criteria for the attendance records to export.
     *
     * @return Criteria
     */
    private Criteria getStudentAttendanceCriteria() {
        X2Criteria userCriteria = new X2Criteria();
        userCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, m_startDate);
        userCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, m_endDate);
        if (isSchoolContext()) {
            userCriteria.addEqualTo(StudentAttendance.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            if (m_orgFieldStr != null && !StringUtils.isEmpty(m_orgOid)) {
                userCriteria.addEqualTo(StudentAttendance.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_orgFieldStr,
                        m_orgOid);
            }
            userCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            userCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        // Check exclude student from state reporting if flag is true.
        if (!StringUtils.isEmpty(m_excludeSchoolEnrField)) {
            userCriteria.addNotEqualTo(StudentAttendance.REL_SCHOOL + PATH_DELIMITER +
                    m_excludeSchoolEnrField,
                    BooleanAsStringConverter.TRUE);
        }
        // Check exclude enr student from state reporting if flag is true.
        if (!StringUtils.isEmpty(m_excludeStdField)) {
            userCriteria.addNotEqualTo(StudentAttendance.REL_STUDENT + PATH_DELIMITER +
                    m_excludeStdField,
                    BooleanAsStringConverter.TRUE);
        }
        return userCriteria;
    }
}
