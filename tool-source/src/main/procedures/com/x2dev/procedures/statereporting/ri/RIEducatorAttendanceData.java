/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2016 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.ri;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.StaffCalendar;
import com.follett.fsc.core.k12.beans.StaffCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffAttendance;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.sis.model.beans.StaffLeave;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;


/**
 * Data module for RI Educator Attendance Export.
 *
 * @author Follett Software Company
 */
public class RIEducatorAttendanceData extends StateReportData {

    /**
     * Entity class for RI Educator Attendance Export.
     *
     * @author Follett Software Company
     */
    public static class RIEducatorAttendanceEntity extends StateReportEntity {
        @SuppressWarnings("unused")
        private static final String NO_PERFMON4J_INSTRUMENTATION = "";
        private RIEducatorAttendanceData m_data;
        private ArrayList<RowData> m_rows;
        private SisStaff m_staff;

        /**
         * Instantiates a new RI educator attendance entity.
         */
        public RIEducatorAttendanceEntity() {
            // Needed for dynamic instantiation.
        }

        /**
         * Gets the entity name.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            return getEntityName(m_staff);
        }

        /**
         * Gets the entity name.
         *
         * @param staff SisStaff
         * @return String
         */
        public static String getEntityName(SisStaff staff) {
            return "Staff name: " + staff.getNameView() + ", " +
                    "Local ID: " + staff.getLocalId();
        }

        /**
         * Returns current row.
         *
         * @return RowData
         */
        public RowData getRow() {
            return m_rows.get(getCurrentRow());
        }

        /**
         * Intitialize.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_data = (RIEducatorAttendanceData) data;
            m_staff = (SisStaff) bean;

            // Only staff records where stfSklOID is populated AND the skl.{State School ID] is not
            // blank/null,
            // shall be included in the export. If stfSklOID is not populated, we need show error
            // messages for qualifying
            // records. If stfSklOID is populated and skl.{State School ID] is blank/null, exclude
            // records.
            if (!StringUtils.isEmpty(m_staff.getSchoolOid()) &&
                    StringUtils
                            .isEmpty((String) m_staff.getSchool().getFieldValueByBeanPath(m_data.m_fieldStateSklId))) {
                setRowCount(0);
                return;
            }

            RIEducatorAttendanceData eaData = (RIEducatorAttendanceData) data;

            m_rows = eaData.getDataForStaff(m_staff);

            if (!addErrors()) {
                setRowCount(m_rows.size());
            } else {
                setRowCount(0);
            }
        }

        /**
         * Add errors if school oid is not populated. Returns true if school oid is empty.
         *
         * @return StateReportValidationError
         */
        private boolean addErrors() {
            boolean errorsAdded = false;
            if (StringUtils.isEmpty(m_staff.getSchoolOid())) {
                ArrayList<RowData> rows = m_data.getDataForStaff(m_staff);
                if (!rows.isEmpty()) {
                    for (int i = 0; i < rows.size(); i++) {
                        StateReportValidationError error = null;
                        RowData currentRow = rows.get(i);
                        Object mainInstance = currentRow.getMainInstance();
                        if (mainInstance instanceof StaffAttendance) {
                            error = new StateReportValidationError(getEntityName(), "",
                                    "Date of Absence: " + ((StaffAttendance) mainInstance).getDate(),
                                    "Staff members is missing school assigned. "
                                            + "Staff members without a school assignment will not be included in the export.");
                        } else {
                            error = new StateReportValidationError(getEntityName(), "",
                                    "Start Date of Leave range: " + ((StaffLeave) mainInstance).getStartDate(),
                                    "Staff members is missing school assigned. "
                                            + "Staff members without a school assignment will not be included in the export.");
                        }
                        m_data.m_errors.add(error);
                    }
                }
                errorsAdded = true;
            }
            return errorsAdded;
        }
    }

    /**
     * Retrieves data using RowData.
     *
     * @author Follett Software Company
     */
    public class AttendanceRetriever implements FieldRetriever {
        public static final String CALC_ID = "ATTENDANCE";

        private static final String CALC_PARAM_DATE = "DATE";
        private static final String CALC_PARAM_FIVEDAYS = "FIVEDAYS";
        private static final String CALC_PARAM_SCHOOL = "SCHOOL";
        private static final String CALC_PARAM_TEACHING = "TEACHING";
        private static final String CALC_PARAM_TYPE = "TYPE";

        private static final String TEACHER_TITLE_STATE_CODE = "T";

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
            Object value = null;

            RIEducatorAttendanceEntity eaEntity = (RIEducatorAttendanceEntity) entity;
            SisStaff staff = (SisStaff) entity.getBean();

            RowData row = eaEntity.getRow();
            String parameter = (String) field.getParameter();
            String staffTypeCode = EMPTY_STRING;

            switch (parameter) {
                case CALC_PARAM_DATE:
                    value = row.getAttendanceDate();
                    break;
                case CALC_PARAM_SCHOOL:
                    SisSchool school = row.getSchool();
                    if (school == null) {
                        school = staff.getSchool();
                    }
                    if (school != null) {
                        value = school.getFieldValueByAlias(ALIAS_SCHOOL_STATE_ID);
                    }
                    break;
                case CALC_PARAM_TEACHING:
                    if (staff.getStaffType() != null) {
                        if (m_staffType.containsKey(staff.getStaffType())) {
                            staffTypeCode = m_staffType.get(staff.getStaffType());
                            if (staffTypeCode != null && staffTypeCode.equals(TEACHER_TITLE_STATE_CODE)) {
                                value = "Y";
                            } else {
                                value = "N";
                            }
                        }
                    }
                    break;
                case CALC_PARAM_TYPE:
                    value = row.getAttendanceType();
                    value = lookupReferenceCodeByAlias(ALIAS_ATT_ABSENSE_TYPE, (String) value,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    if (StringUtils.isEmpty((String) value)) {
                        StateReportValidationError error = null;
                        Object mainInstance = row.getMainInstance();

                        if (mainInstance instanceof StaffAttendance) {
                            error = new StateReportValidationError(entity, field,
                                    "Date of Absence: " + ((StaffAttendance) mainInstance).getDate(),
                                    "Missing Absence Type");
                        }
                        if (mainInstance instanceof StaffLeave) {
                            error = new StateReportValidationError(entity, field,
                                    "Start Date for Leave: " + ((StaffLeave) mainInstance).getStartDate(),
                                    "Missing Leave Type");
                        }

                        entity.addRetrievalError(field.getFieldId(), error);
                    }
                    break;
                case CALC_PARAM_FIVEDAYS:
                    value = eaEntity.getRow().getPreApprGr5Days();
                    break;
                default:
                    break;
            }

            return value;
        }
    }

    /**
     * Retrieves DISTCODE.
     *
     * @author Follett Software Company
     */
    public class DistcodeRetriever implements FieldRetriever {
        public static final String CALC_ID = "DISTCODE";

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
            String value = null;

            SisStaff staff = (SisStaff) entity.getBean();
            SisSchool school = staff.getSchool();

            if (school != null) {
                value = (String) school.getFieldValueByAlias(ALIAS_ADJUSTED_DISTRICT);
                if (StringUtils.isEmpty(value)) {
                    value = (String) staff.getOrganization1().getFieldValueByAlias(ALIAS_REPORTING_DISTRICT_CODE);
                }
            }

            return value;
        }
    }

    /**
     * Class to store next row data:
     * 1) TEACHING (DOE TEACHER || DOE TEACHER OF STUDENTS)
     * 2) ATTEND_DATE (sfaDate || slfStartDate-slfEndDate)
     * 3) ATTEND_TYPE (sfa.DOE STAFF ABSENCE TYPE || sfl.DOE STAFF LEAVE TYPE)
     * 4) PREAPPR_GRTR_FIVEDAYS (sfa.DOE PREAPPROVED > 5 DAYS || sfl.DOE LEAVE PREAPPROVED > 5 DAYS)
     *
     * @author Follett Software Company
     */
    protected static class RowData {
        /**
         * Returns RowData instance created based on passed StaffAttendance.
         *
         * @param att StaffAttendance
         * @return RowData
         */
        public static RowData getInstance(StaffAttendance att) {
            return new RowData(att);
        }

        /**
         * Returns list if RowData instances created based on passed StaffLeave and list of dates.
         *
         * @param leave StaffLeave
         * @param dates List<PlainDate>
         * @return List
         */
        public static List<RowData> getInstances(StaffLeave leave, List<PlainDate> dates) {
            List<RowData> list = new ArrayList<RowData>();

            for (PlainDate date : dates) {
                list.add(new RowData(leave, date));
            }

            return list;
        }

        private PlainDate m_attendDate;
        private String m_attendType;
        private boolean m_preApprGr5Days;
        private Object m_object;
        private SisSchool m_school;

        /**
         * Instantiates a new row data.
         *
         * @param obj Object
         */
        private RowData(Object obj) {
            m_object = obj;
        }

        /**
         * Instantiates a new row data.
         *
         * @param att StaffAttendance
         */
        private RowData(StaffAttendance att) {
            this((Object) att);

            m_attendDate = att.getDate();
            m_attendType = (String) att.getFieldValueByAlias(ALIAS_ATT_ABSENSE_TYPE);
            m_preApprGr5Days = BooleanAsStringConverter.TRUE.equals(att.getFieldValueByAlias(ALIAS_ATT_PREAPPR));
            m_school = att.getSchool();
        }

        /**
         * Instantiates a new row data.
         *
         * @param leave StaffLeave
         * @param date PlainDate
         */
        private RowData(StaffLeave leave, PlainDate date) {
            this(leave);

            m_attendDate = date;
            m_attendType = (String) leave.getFieldValueByAlias(ALIAS_LEAVE_ABSENSE_TYPE);
            m_preApprGr5Days = BooleanAsStringConverter.TRUE.equals(leave.getFieldValueByAlias(ALIAS_LEAVE_PREAPPR));
            m_school = leave.getSchool();
        }

        /**
         * Hash code.
         *
         * @return int
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return new HashCodeBuilder(17, 31).append(m_attendDate).append(m_attendType).append(m_preApprGr5Days)
                    .toHashCode();
        }


        /**
         * Returns value for ATTEND_DATE.
         *
         * @return PlainDate
         */
        public PlainDate getAttendanceDate() {
            return m_attendDate;
        }

        /**
         * Returns value for ATTEND_TYPE.
         *
         * @return String
         */
        public String getAttendanceType() {
            return m_attendType;
        }

        /**
         * Returns value for PREAPPR_GRTR_FIVEDAYS.
         *
         * @return Boolean
         */
        public Boolean getPreApprGr5Days() {
            return Boolean.valueOf(m_preApprGr5Days);
        }

        /**
         * Returns instance that was used to create the RowData.
         *
         * @return String
         */
        public Object getMainInstance() {
            return m_object;
        }

        /**
         * Gets the school.
         *
         * @return Sis school
         */
        public SisSchool getSchool() {
            return m_school;
        }

        /**
         * Equals.
         *
         * @param obj Object
         * @return true, if successful
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof RowData)) {
                return false;
            }
            if (obj == this) {
                return true;
            }

            RowData rhs = (RowData) obj;
            return new EqualsBuilder().append(m_attendDate, rhs.m_attendDate).append(m_attendType, rhs.m_attendType)
                    .append(m_preApprGr5Days, rhs.m_preApprGr5Days).isEquals();
        }
    }

    protected PlainDate m_endDate = null;
    protected ArrayList<StateReportValidationError> m_errors = new ArrayList<StateReportValidationError>();
    protected String m_fieldStateSklId;
    protected PlainDate m_startDate = null;

    private static final String ALIAS_ADJUSTED_DISTRICT = "DOE ADJUSTED DISTRICT";
    private static final String ALIAS_ADMIN_IN_SESSION = "DOE ADMINISTRATORS IN SESSION";
    private static final String ALIAS_ATT_ABSENSE_TYPE = "DOE STAFF ABSENCE TYPE";
    private static final String ALIAS_ATT_EXCLUDE = "DOE EXCLUDE ATTENDANCE";
    private static final String ALIAS_ATT_PREAPPR = "DOE PREAPPROVED > 5 DAYS";
    private static final String ALIAS_EXCLUDE_STAFF = "DOE EXCLUDE STF";
    private static final String ALIAS_LEAVE_ABSENSE_TYPE = "DOE STAFF LEAVE TYPE";
    private static final String ALIAS_LEAVE_EXCLUDE = "DOE EXCLUDE LEAVE";
    private static final String ALIAS_LEAVE_PREAPPR = "DOE LEAVE PREAPPROVED > 5 DAYS";
    private static final String ALIAS_REPORTING_DISTRICT_CODE = "RI Reporting District Code";
    private static final String ALIAS_SCHOOL_STATE_ID = "State School Id";
    private static final String ALIAS_STF_CERT_ID = "DOE CERTID";
    private static final String ALIAS_TEACHER_IN_SESSION = "DOE TEACHERS IN SESSION";
    private static final String STAFF_TYPE_DB_NAME = "STF_STAFF_TYPE";

    private static final String INPUT_PARAM_END_DATE = "endDate";
    private static final String INPUT_PARAM_START_DATE = "startDate";

    /**
     * Members
     */
    private Map<String, ArrayList<PlainDate>> m_calendarPossibleDays = new HashMap<String, ArrayList<PlainDate>>();
    private String m_fieldAdminDay;
    private String m_fieldExcludeAtt;
    private String m_fieldExcludeLeave;
    private String m_fieldExcludeStaff;
    private String m_fieldStfCertId;
    private String m_fieldTeacherDay;
    private Map<String, Map<String, Collection<StaffAttendance>>> m_staffAttendances;
    private Map<String, String> m_staffType;
    private Map<String, Collection<StaffCertification>> m_staffCertifications;
    private Map<String, Map<String, Collection<StaffLeave>>> m_staffLeaves;
    private Map<String, StaffLeave> m_leavesOids;

    /**
     * Initialize.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {
        translateAliases();
        initStaffType();

        m_endDate = (PlainDate) getParameter(INPUT_PARAM_END_DATE);
        if (m_endDate == null) {
            m_endDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
        m_startDate = (PlainDate) getParameter(INPUT_PARAM_START_DATE);

        // Determine dates where possible leaves and attendances by calendar id.
        X2Criteria staffCalDatesCriteria = new X2Criteria();
        /*
         * Dates that are not marked as DOE TEACHERS IN SESSION or DOE ADMINISTRATORS IN SESSION =
         * True, shall not
         * have records generated even if there is an absence or leave record in the database.
         */
        X2Criteria teachOrAdminCriteria = new X2Criteria();
        if (!StringUtils.isEmpty(m_fieldTeacherDay)) {
            teachOrAdminCriteria.addEqualTo(m_fieldTeacherDay, BooleanAsStringConverter.TRUE);
        }
        X2Criteria adminCriteria = new X2Criteria();
        if (!StringUtils.isEmpty(m_fieldAdminDay)) {
            adminCriteria.addEqualTo(m_fieldAdminDay, BooleanAsStringConverter.TRUE);
        }
        teachOrAdminCriteria.addOrCriteria(adminCriteria);
        staffCalDatesCriteria.addAndCriteria(teachOrAdminCriteria);

        /*
         * Dates should be within date range.
         */
        staffCalDatesCriteria.addGreaterOrEqualThan(StaffCalendarDate.COL_DATE, m_startDate);
        staffCalDatesCriteria.addLessOrEqualThan(StaffCalendarDate.COL_DATE, m_endDate);
        String[] columns =
                {StaffCalendarDate.REL_STAFF_CALENDAR + ModelProperty.PATH_DELIMITER + StaffCalendar.COL_CALENDAR_ID,
                        StaffCalendarDate.COL_DATE};
        ReportQueryByCriteria possibDatesQuery =
                new ReportQueryByCriteria(StaffCalendarDate.class, columns, staffCalDatesCriteria);

        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(possibDatesQuery);
        try {
            while (iterator.hasNext()) {
                Object[] items = (Object[]) iterator.next();
                String calendarId = (String) items[0];
                PlainDate date = new PlainDate((Date) items[1]);

                ArrayList<PlainDate> dates = m_calendarPossibleDays.get(calendarId);
                if (dates == null) {
                    dates = new ArrayList<PlainDate>();
                    m_calendarPossibleDays.put(calendarId, dates);
                }
                dates.add(date);
            }
        } finally {
            iterator.close();
        }

        // Determine staff attendances.
        X2Criteria attCriteria = new X2Criteria();
        attCriteria.addGreaterOrEqualThan(StaffAttendance.COL_DATE, m_startDate);
        attCriteria.addLessOrEqualThan(StaffAttendance.COL_DATE, m_endDate);
        attCriteria.addNotEqualTo(m_fieldExcludeAtt, BooleanAsStringConverter.TRUE);
        QueryByCriteria attQuery = new QueryByCriteria(StaffAttendance.class, attCriteria);
        String[] attColumns = {StaffAttendance.REL_STAFF + ModelProperty.PATH_DELIMITER + SisStaff.COL_SCHOOL_OID,
                StaffAttendance.COL_STAFF_OID};
        int[] attSizes = {1, 2};
        m_staffAttendances = getBroker().getGroupedCollectionByQuery(attQuery, attColumns, attSizes);

        // Determine staff leaves.
        X2Criteria leavesCriteria = new X2Criteria();
        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(StaffLeave.COL_END_DATE, m_startDate);
        X2Criteria endDateIsEmpty = new X2Criteria();
        endDateIsEmpty.addEmpty(StaffLeave.COL_END_DATE, getBroker().getPersistenceKey());
        endDateCriteria.addOrCriteria(endDateIsEmpty);
        leavesCriteria.addAndCriteria(endDateCriteria);
        leavesCriteria.addLessOrEqualThan(StaffLeave.COL_START_DATE, m_endDate);
        leavesCriteria.addNotEqualTo(m_fieldExcludeLeave, BooleanAsStringConverter.TRUE);
        QueryByCriteria leavesQuery = new QueryByCriteria(StaffLeave.class, leavesCriteria);
        String[] leaveColumns = {StaffLeave.REL_STAFF + ModelProperty.PATH_DELIMITER + SisStaff.COL_SCHOOL_OID,
                StaffLeave.COL_STAFF_OID};
        int[] leaveSizes = {1, 1};
        m_staffLeaves = getBroker().getGroupedCollectionByQuery(leavesQuery, leaveColumns, leaveSizes);
        m_leavesOids = getBroker().getMapByQuery(leavesQuery, X2BaseBean.COL_OID, 1);

        // Determine staff criteria.
        X2Criteria staffCriteria = new X2Criteria();
        X2Criteria staffInCriteria = new X2Criteria();
        SubQuery leavesSubQuery = new SubQuery(StaffLeave.class, StaffLeave.COL_STAFF_OID, leavesCriteria);
        staffInCriteria.addIn(X2BaseBean.COL_OID, leavesSubQuery);
        X2Criteria orInCriteria = new X2Criteria();
        SubQuery attSubQuery = new SubQuery(StaffAttendance.class, StaffAttendance.COL_STAFF_OID, attCriteria);
        orInCriteria.addIn(X2BaseBean.COL_OID, attSubQuery);
        staffInCriteria.addOrCriteria(orInCriteria);
        staffCriteria.addAndCriteria(staffInCriteria);
        staffCriteria.addNotEmpty(m_fieldStfCertId, getBroker().getPersistenceKey());
        staffCriteria.addNotEqualTo(m_fieldExcludeStaff, BooleanAsStringConverter.TRUE);
        applyInputCriteria(staffCriteria, true, null);
        /*
         * If there is no staff calendar and the user attempts to run the export,
         * the export shall return "The results were empty".
         */
        X2Criteria staffCalendarCriteria = new X2Criteria();
        SubQuery staffCalendarsSubQuery =
                new SubQuery(StaffCalendar.class, StaffCalendar.COL_CALENDAR_ID, staffCalendarCriteria);
        staffCriteria.addIn(SisStaff.COL_CALENDAR_ID, staffCalendarsSubQuery);

        // Determine staff certification criteria.
        X2Criteria staffCertifications = new X2Criteria();
        X2Criteria endDateCertCriteria = new X2Criteria();
        endDateCertCriteria.addGreaterOrEqualThan(StaffCertification.COL_EXPIRATION_DATE, m_startDate);
        X2Criteria endDateCertIsEmpty = new X2Criteria();
        endDateCertIsEmpty.addEmpty(StaffCertification.COL_EXPIRATION_DATE, getBroker().getPersistenceKey());
        endDateCertCriteria.addOrCriteria(endDateCertIsEmpty);
        staffCertifications.addAndCriteria(endDateCertCriteria);
        staffCertifications.addLessOrEqualThan(StaffCertification.COL_ISSUE_DATE, m_endDate);
        SubQuery staffSubQuery = new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria);
        staffCertifications.addIn(StaffCertification.COL_STAFF_OID, staffSubQuery);
        QueryByCriteria certQuery = new QueryByCriteria(StaffCertification.class, staffCertifications);
        m_staffCertifications = getBroker().getGroupedCollectionByQuery(certQuery, StaffCertification.COL_STAFF_OID, 1);

        setEntityClass(RIEducatorAttendanceEntity.class);
        QueryByCriteria staffQuery = new QueryByCriteria(SisStaff.class, staffCriteria);
        setQuery(staffQuery);

        Map<String, FieldRetriever> calcsMap = new HashMap<String, FieldRetriever>();
        calcsMap.put(AttendanceRetriever.CALC_ID, new AttendanceRetriever());
        calcsMap.put(DistcodeRetriever.CALC_ID, new DistcodeRetriever());
        addCalcs(calcsMap);
    }

    /**
     * Post process.
     *
     * @return Collection
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#postProcess()
     */
    @Override
    public Collection<StateReportValidationError> postProcess() {
        // No staff calendar defined.
        QueryByCriteria staffCalendarQuery = new QueryByCriteria(StaffCalendar.class, new X2Criteria());
        Collection<StaffCalendar> staffCalendar = getBroker().getCollectionByQuery(staffCalendarQuery);
        if (staffCalendar.isEmpty()) {
            StateReportValidationError error = new StateReportValidationError("", "", "", "No staff calendar defined.");
            m_errors.add(error);
        }

        // No Administrators in session days defined on staff calendar.
        X2Criteria cfdAdminCriteria = new X2Criteria();
        cfdAdminCriteria.addEqualTo(m_fieldAdminDay, BooleanAsStringConverter.TRUE);
        cfdAdminCriteria.addEqualTo(StaffCalendarDate.REL_STAFF_CALENDAR + ModelProperty.PATH_DELIMITER +
                StaffCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        QueryByCriteria cfdAdminQuery = new QueryByCriteria(StaffCalendarDate.class, cfdAdminCriteria);
        Collection<StaffCalendarDate> adminDates = getBroker().getCollectionByQuery(cfdAdminQuery);
        if (adminDates.isEmpty()) {
            StateReportValidationError error = new StateReportValidationError("", "", "",
                    "No Administrators in session days defined on staff calendar.");
            m_errors.add(error);
        }

        // No Teachers in session days defined on staff calendar.
        X2Criteria cfdTeachCriteria = new X2Criteria();
        cfdTeachCriteria.addEqualTo(m_fieldTeacherDay, BooleanAsStringConverter.TRUE);
        cfdTeachCriteria.addEqualTo(StaffCalendarDate.REL_STAFF_CALENDAR + ModelProperty.PATH_DELIMITER +
                StaffCalendar.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        QueryByCriteria cfdTeacherQuery = new QueryByCriteria(StaffCalendarDate.class, cfdTeachCriteria);
        Collection<StaffCalendarDate> teacherDates = getBroker().getCollectionByQuery(cfdTeacherQuery);
        if (teacherDates.isEmpty()) {
            StateReportValidationError error = new StateReportValidationError("", "", "",
                    "No Teachers in session days defined on staff calendar.");
            m_errors.add(error);
        }

        return m_errors;
    }

    /**
     * Returns certification for passed staff based on passed date.
     *
     * @param staff SisStaff
     * @param date PlainDate
     * @return StaffCertification
     */
    protected StaffCertification getCertification(SisStaff staff, PlainDate date) {
        StaffCertification certification = null;

        Collection<StaffCertification> certifications = m_staffCertifications.get(staff.getOid());

        if (certifications != null) {
            for (StaffCertification currentCert : certifications) {
                PlainDate issueDate = currentCert.getIssueDate();
                PlainDate expirationDate = currentCert.getExpirationDate();

                if (!date.before(issueDate) && (expirationDate == null || !date.after(expirationDate))) {
                    certification = currentCert;
                }
            }
        }

        return certification;
    }

    /**
     * Returns list of RowData by staff oid.
     *
     * @param staff SisStaff
     * @return ArrayList<RowData>
     */
    protected ArrayList<RowData> getDataForStaff(SisStaff staff) {
        ArrayList<RowData> data = new ArrayList<RowData>();

        Map<String, Collection<StaffAttendance>> staffAttendancesMap = m_staffAttendances.get(staff.getSchoolOid());
        Map<String, Collection<StaffLeave>> staffLeavesMap = m_staffLeaves.get(staff.getSchoolOid());

        Map<PlainDate, Collection<StaffAttendance>> dateAttendancesMap =
                new HashMap<PlainDate, Collection<StaffAttendance>>();
        Map<String, Collection<PlainDate>> leaveDatesMap = new HashMap<String, Collection<PlainDate>>();

        String calendarId = staff.getCalendarId();
        ArrayList<PlainDate> possibleDates = m_calendarPossibleDays.get(calendarId);

        // Try handle attendances.
        if (staffAttendancesMap != null) {
            Collection<StaffAttendance> attendances = staffAttendancesMap.get(staff.getOid());
            if (attendances != null) {
                for (StaffAttendance currentAtt : attendances) {
                    PlainDate attDate = currentAtt.getDate();

                    if (possibleDates != null && possibleDates.contains(attDate)) {
                        RowData rowData = RowData.getInstance(currentAtt);

                        if (!data.contains(rowData)) {
                            data.add(rowData);
                        }

                        // Add to the map to handle overlap errors
                        Collection<StaffAttendance> attendancesForDate = dateAttendancesMap.get(attDate);
                        if (attendancesForDate == null) {
                            attendancesForDate = new ArrayList<StaffAttendance>();
                            dateAttendancesMap.put(attDate, attendancesForDate);
                        }
                        attendancesForDate.add(currentAtt);
                    }
                }
            }
        }

        // Try handle leaves.
        if (staffLeavesMap != null) {
            Collection<StaffLeave> leaves = staffLeavesMap.get(staff.getOid());
            if (leaves != null) {
                for (StaffLeave leave : leaves) {
                    PlainDate leaveStart = leave.getStartDate();
                    PlainDate leaveEnd = leave.getEndDate();

                    if (possibleDates != null) {
                        ArrayList<PlainDate> leaveDates = new ArrayList<PlainDate>();
                        for (PlainDate date : possibleDates) {
                            if (!date.before(leaveStart) && (leaveEnd == null || !date.after(leaveEnd))) {
                                leaveDates.add(date);
                            }
                        }
                        if (!leaveDates.isEmpty()) {
                            List<RowData> leavesRowData = RowData.getInstances(leave, leaveDates);

                            for (RowData rowData : leavesRowData) {
                                if (!data.contains(rowData)) {
                                    data.add(rowData);
                                }
                            }

                            // Add to the map to handle overlap errors
                            leaveDatesMap.put(leave.getOid(), leaveDates);
                        }
                    }
                }
            }
        }

        // Handle duplicated attendances
        for (Entry<PlainDate, Collection<StaffAttendance>> entry : dateAttendancesMap.entrySet()) {
            if (entry.getValue().size() > 1) {
                StateReportValidationError error =
                        new StateReportValidationError(RIEducatorAttendanceEntity.getEntityName(staff),
                                "", "Date of Absence: " + entry.getKey(), "Duplicate Records");
                m_errors.add(error);
            }
        }

        // Handle overlapping leaves
        Collection<String> handledLeaves = new ArrayList<String>();
        for (Entry<String, Collection<PlainDate>> outerEntry : leaveDatesMap.entrySet()) {
            handledLeaves.add(outerEntry.getKey());
            for (Entry<String, Collection<PlainDate>> innerEntry : leaveDatesMap.entrySet()) {
                if (!handledLeaves.contains(innerEntry.getKey())) {
                    if (!Collections.disjoint(outerEntry.getValue(), innerEntry.getValue())) {
                        StateReportValidationError error =
                                new StateReportValidationError(RIEducatorAttendanceEntity.getEntityName(staff), "",
                                        "Start Dates for Leave ranges: "
                                                + m_leavesOids.get(outerEntry.getKey()).getStartDate() + " and " +
                                                m_leavesOids.get(innerEntry.getKey()).getStartDate(),
                                        "Overlapping or duplicate leave records.");
                        m_errors.add(error);
                    }
                }
            }
        }

        // Handle overlapping attendances and leaves
        for (Entry<PlainDate, Collection<StaffAttendance>> attendanceEntry : dateAttendancesMap.entrySet()) {
            for (Entry<String, Collection<PlainDate>> leaveEntry : leaveDatesMap.entrySet()) {
                if (leaveEntry.getValue().contains(attendanceEntry.getKey())) {
                    StateReportValidationError error =
                            new StateReportValidationError(RIEducatorAttendanceEntity.getEntityName(staff), "",
                                    "Date of Absence: " + attendanceEntry.getKey() + ", Start Date of Leave range: " +
                                            m_leavesOids.get(leaveEntry.getKey()).getStartDate(),
                                    "Overlapping attendance and leave records.");
                    m_errors.add(error);
                }
            }
        }

        return data;
    }

    /**
     * Load map with staff type codes.
     */
    private void initStaffType() {
        m_staffType = new HashMap<String, String>();

        DataDictionary dictionary =
                DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());

        DataDictionaryField staffTypeField = dictionary.findDataDictionaryFieldByDatabaseName(STAFF_TYPE_DB_NAME);

        if (staffTypeField.hasReferenceTable()) {
            for (ReferenceCode code : staffTypeField.getReferenceTable().getReferenceCodes(getBroker())) {
                m_staffType.put(code.getCode(), code.getStateCode());
            }
        }
    }

    /**
     * Validates aliases, add setup error if alias is not configured.
     */
    private void translateAliases() {
        m_fieldAdminDay = translateAliasToJavaName(ALIAS_ADMIN_IN_SESSION, true);
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STAFF, true);
        m_fieldStfCertId = translateAliasToJavaName(ALIAS_STF_CERT_ID, true);
        m_fieldStateSklId = translateAliasToJavaName(ALIAS_SCHOOL_STATE_ID, true);
        m_fieldTeacherDay = translateAliasToJavaName(ALIAS_TEACHER_IN_SESSION, true);
        m_fieldExcludeAtt = translateAliasToJavaName(ALIAS_ATT_EXCLUDE, true);
        m_fieldExcludeLeave = translateAliasToJavaName(ALIAS_LEAVE_EXCLUDE, true);

        translateAliasToJavaName(ALIAS_ADJUSTED_DISTRICT, true);
        translateAliasToJavaName(ALIAS_ATT_ABSENSE_TYPE, true);
        translateAliasToJavaName(ALIAS_ATT_PREAPPR, true);
        translateAliasToJavaName(ALIAS_LEAVE_ABSENSE_TYPE, true);
        translateAliasToJavaName(ALIAS_LEAVE_PREAPPR, true);
    }
}
