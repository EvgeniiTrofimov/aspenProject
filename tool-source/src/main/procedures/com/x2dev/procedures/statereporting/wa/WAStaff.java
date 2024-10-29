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

package com.x2dev.procedures.statereporting.wa;

// TODO: further staff filter (Type? Schedule?)

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Staff;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SchoolCourse;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.Section;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import java.text.SimpleDateFormat;
import java.util.*;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Staff File.
 *
 * @author X2 Development Corporation
 */
public class WAStaff extends StateReportData {
    /**
     * Entity class for Staff export.
     *
     */
    public static class StaffEntity extends StateReportEntity {
        private WAStaff m_data;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StaffEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the certification.
         *
         * @return the certification number of the first primary certification for this staff
         */
        public String getCertification() {
            String certification = null;
            SisStaff staff = (SisStaff) getBean();
            List<StaffCertification> certifications = m_data.m_staffCertificationMap.get(staff.getOid());
            if (certifications != null) {
                Iterator<StaffCertification> iterator = certifications.iterator();
                if (iterator.hasNext()) {
                    StaffCertification cert = iterator.next();
                    certification = cert.getCertificationNumber();
                }
            }
            return certification != null && certification.length() > 0 ? certification : "UNK";
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStaff staff = (SisStaff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] ";
            return name;
        }

        /**
         * Gets the staff code.
         *
         * @return the concatenated staff code for the CEDARS report
         */
        public String getStaffCode() {
            SisStaff staff = (SisStaff) getBean();

            String type = (String) staff.getFieldValueByAlias(ALIAS_STAFF_TYPE);
            type = getData().lookupReferenceCodeByAlias(ALIAS_STAFF_TYPE, type,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            String cert = (String) staff.getFieldValueByAlias(ALIAS_CERTIFICATION);
            cert = getData().lookupReferenceCodeByAlias(ALIAS_CERTIFICATION, cert,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            if (cert == null && type != null && type.length() >= 3) {
                cert = type.substring(2, 3);
            }
            if (cert == null) {
                cert = "0";
            }

            String staffType = null;
            if (type != null && type.length() > 1) {
                staffType = type.substring(0, 2) + cert;
            }

            return staffType;
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
            m_data = (WAStaff) data;

            // If the staff does not have a scheduled class, do not report.
            if (!m_data.m_staffWithSchedules.contains(bean.getOid())
                    && !m_data.m_staffWithHomerooms.contains(bean.getOid())) {
                setRowCount(0);
            }
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
     * Class implements procedures for calculated fields .
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveReportValue implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            StaffEntity myEntity = (StaffEntity) entity; // cast bean and data to correct types

            if (PARAM_STAFF_TYPE.equals(param)) {
                return myEntity.getStaffCode();
            } else if (param.equals(WAStaff.PARAM_CERTIFICATE)) {
                return myEntity.getCertification();
            }
            return null;
        }
    }

    /**
     * Custom validation procedures implementation.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateReportValue implements FieldValidator {
        private static final String FIELD_STAFF_TYPE_CODE = "Staff Type Code";
        private List<String> m_staffNotReqSert = new ArrayList<String>(Arrays.asList("910", "913", "990", "993"));
        SimpleDateFormat m_formatter = new SimpleDateFormat("MM/dd/yyyy");
        Calendar m_currentDate = new GregorianCalendar();

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
            StaffEntity staff = (StaffEntity) entity; // cast bean and data to correct types
            String param = (String) field.getParameter();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            String certNum = value;

            if (param.equals(WAStaff.PARAM_CERTIFICATE)) {
                String staffCode = entity.getFieldValue(FIELD_STAFF_TYPE_CODE);
                // Element F05
                // If staff type code, reported in Element F04 - Staff Type Code, contains a valid
                // value of 910, 913, 990 or 993 then a certification number is not required.
                if (StringUtils.isEmpty(certNum) && !StringUtils.isEmpty(staffCode)) {
                    if (!(m_staffNotReqSert.contains(staffCode))) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " can be empty only if " + FIELD_STAFF_TYPE_CODE + " is "
                                        + m_staffNotReqSert,
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END + "; " +
                                        FIELD_STAFF_TYPE_CODE + " = " + STYLE_BOLD + staffCode + STYLE_END));
                    }
                }


                // Element F09
                // The date of birth provided must calculate an age of at least 18 years.

                String birthDate = staff.getFieldValue(FIELD_BIRTHDATE);
                if (StringUtils.isEmpty(birthDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            FIELD_BIRTHDATE + " should not be empty",
                            "Birth Date should be set"));
                } else {
                    try {

                        Date date = m_formatter.parse(birthDate);
                        Calendar gc = new GregorianCalendar();
                        gc.setTime(date);
                        gc.add(Calendar.YEAR, 18);
                        if (gc.compareTo(m_currentDate) > 0) {
                            errors.add(new StateReportValidationError(entity, field,
                                    FIELD_BIRTHDATE + " is incorrect",
                                    "The age must exceed 18 years"));
                        }
                    } catch (Exception ex) {
                        errors.add(new StateReportValidationError(entity, field,
                                FIELD_BIRTHDATE + " has incorrect format",
                                "Birth date format unexpected: " + birthDate));
                    }
                }

            }
            return errors;
        }

    }

    /*
     * Constants: Aliases, Parameters, Fields.
     */
    private static final String ALIAS_CERTIFICATION = "DOE CERTIFICATION";
    private static final String ALIAS_EXCLUDE_CRS = "DOE EXCLUDE CRS";
    private static final String ALIAS_EXCLUDE_MST = "DOE EXCLUDE MST";
    private static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    private static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    private static final String ALIAS_STAFF_TYPE = "DOE STAFF TYPE";

    private static final String FIELD_BIRTHDATE = "Birth Date";

    private static final String PARAM_CERTIFICATE = "CERTIFICATE";
    private static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    private static final String PARAM_PRIMARY_ONLY = "primaryOnly";
    private static final String PARAM_STAFF_TYPE = "TYPE";

    /*
     * Instance variables
     */
    protected String m_excludeSchool;
    protected String m_fieldExcludeStaff;
    protected String m_fieldExcludeCourse;
    protected String m_fieldExcludeSection;
    protected Boolean m_primaryOnly;
    protected Map<String, List<StaffCertification>> m_staffCertificationMap;
    protected Set<String> m_staffWithHomerooms;
    protected Set<String> m_staffWithSchedules;

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for staff to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        /*
         * Get core parameters
         */
        initializeFields();


        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.

            X2Criteria staffCriteria = new X2Criteria();
            // add standard staff exclude indicator
            staffCriteria.addNotEqualTo(m_fieldExcludeStaff, BooleanAsStringConverter.TRUE);

            // add criteria from input definition
            applyInputCriteria(staffCriteria, true, null);
            if (!isSchoolContext()) {
                staffCriteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                        Boolean.TRUE);
                staffCriteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                        Boolean.TRUE);
            }

            if (getParameter(PARAM_EXCLUDE_SCHOOL) != null
                    && ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
                staffCriteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                        BooleanAsStringConverter.TRUE);
            }

            // create query - use the appropriate class
            QueryByCriteria query = new QueryByCriteria(Staff.class, staffCriteria);
            applyInputSort(query, null);

            setQuery(query);
            setEntityClass(StaffEntity.class);

            // load mapes of supporting data for staff.
            loadStaffMaps(staffCriteria);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("STF-TYPE-CERT", new RetrieveReportValue());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("STF-CERT", new ValidateReportValue());
            super.addValidators(validators);
        }
    }

    /**
     * Loads all of the javanames from the aliases, throws setup errors if there are any.
     */
    private void initializeFields() {
        m_fieldExcludeCourse = translateAliasToJavaName(ALIAS_EXCLUDE_CRS, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
        m_fieldExcludeStaff = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_fieldExcludeSection = translateAliasToJavaName(ALIAS_EXCLUDE_MST, true);
        m_primaryOnly = (Boolean) getParameter(PARAM_PRIMARY_ONLY);
    }

    /**
     * Load maps of supporting data for staff.
     *
     * @param staffCriteria X2Criteria
     */
    private void loadStaffMaps(X2Criteria staffCriteria) {
        // load certification map
        X2Criteria certificationCriteria = new X2Criteria();
        certificationCriteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, Boolean.TRUE);
        SubQuery staffSubQuery = new SubQuery(Staff.class, X2BaseBean.COL_OID, staffCriteria);
        certificationCriteria.addIn(StaffCertification.COL_STAFF_OID, staffSubQuery);
        QueryByCriteria certificationQuery = new QueryByCriteria(StaffCertification.class, certificationCriteria);
        certificationQuery.addOrderBy(StaffCertification.COL_STAFF_OID, true);
        m_staffCertificationMap =
                getBroker().getGroupedCollectionByQuery(certificationQuery, StaffCertification.COL_STAFF_OID, 500);

        // Load map of staff with scheduled classes.
        X2Criteria scheduledStaffCriteria = new X2Criteria();

        scheduledStaffCriteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                        MasterSchedule.COL_SCHEDULE_OID);

        // "Class" type classes.
        scheduledStaffCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                Section.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.COL_MASTER_TYPE,
                SchoolCourse.MASTER_TYPE_CLASS);

        // Exclude flags.
        scheduledStaffCriteria.addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStaff,
                BooleanAsStringConverter.TRUE);
        scheduledStaffCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER + m_fieldExcludeSection,
                BooleanAsStringConverter.TRUE);//
        scheduledStaffCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHOOL_COURSE + PATH_DELIMITER +
                SchoolCourse.REL_COURSE + PATH_DELIMITER + m_fieldExcludeCourse, BooleanAsStringConverter.TRUE);

        // Check the primary only parameter.
        if (m_primaryOnly != null && m_primaryOnly.booleanValue()) {
            scheduledStaffCriteria.addEqualTo(ScheduleTeacher.COL_PRIMARY_TEACHER_INDICATOR, Boolean.TRUE);//
        }

        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null
                && ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            scheduledStaffCriteria
                    .addNotEqualTo(ScheduleTeacher.REL_STAFF + PATH_DELIMITER + Staff.REL_SCHOOL + PATH_DELIMITER +
                            m_excludeSchool,
                            BooleanAsStringConverter.TRUE);
            scheduledStaffCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    Section.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                    BooleanAsStringConverter.TRUE);
        }

        applyInputCriteria(scheduledStaffCriteria, false, ScheduleTeacher.REL_STAFF);

        if (isSchoolContext()) {
            scheduledStaffCriteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else {
            scheduledStaffCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);

            scheduledStaffCriteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }
        SubQuery scheduledStaffQuery =
                new SubQuery(ScheduleTeacher.class, ScheduleTeacher.COL_STAFF_OID, scheduledStaffCriteria);
        scheduledStaffQuery.addGroupBy(ScheduleTeacher.COL_STAFF_OID);
        m_staffWithSchedules = new HashSet();
        m_staffWithSchedules.addAll(getBroker().getSubQueryCollectionByQuery(scheduledStaffQuery));

        X2Criteria homeroomCriteria = new X2Criteria();
        homeroomCriteria.addNotEmpty(Staff.COL_HOMEROOM, getBroker().getPersistenceKey());
        homeroomCriteria.addNotEqualTo(m_fieldExcludeStaff, BooleanAsStringConverter.TRUE);

        // add criteria from input definition
        applyInputCriteria(homeroomCriteria, true, null);
        if (!isSchoolContext()) {
            homeroomCriteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + School.COL_INACTIVE_INDICATOR,
                    Boolean.TRUE);
            homeroomCriteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + School.COL_ARCHIVE_INDICATOR,
                    Boolean.TRUE);
        }

        if (getParameter(PARAM_EXCLUDE_SCHOOL) != null
                && ((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            homeroomCriteria.addNotEqualTo(Staff.REL_SCHOOL + PATH_DELIMITER + m_excludeSchool,
                    BooleanAsStringConverter.TRUE);
        }

        SubQuery homeroomStaffQuery = new SubQuery(Staff.class, X2BaseBean.COL_OID, homeroomCriteria);
        m_staffWithHomerooms = new HashSet();
        m_staffWithHomerooms.addAll(getBroker().getSubQueryCollectionByQuery(homeroomStaffQuery));
    }
}
