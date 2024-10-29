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
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.*;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNMultiYearHelper.Strategy;
import com.x2dev.procedures.statereporting.tn.TNEnrollmentHelper.TNStaffMultiYearHelper;
import com.x2dev.sis.model.beans.MasterSchedule;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.ScheduleTeacher;
import com.x2dev.sis.model.beans.SchoolScheduleContext;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStaff;
import com.x2dev.sis.model.beans.StaffCertification;
import com.x2dev.sis.model.beans.StaffPosition;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * Class for student extract export.
 */
public class TNStaffMemberData extends TNStateReportData {
    private static final String NO_PERFMON4J_INSTRUMENTATION = "";

    /**
     * A container class for staff information.
     *
     * @author X2 Development Corporation
     */
    protected static class StaffInfo {
        private String m_instructionalProgram;
        private School m_school;
        private Staff m_staff;

        /**
         * Instantiates a new staff info.
         *
         * @param school School
         * @param staff Staff
         * @param instructionalProgram String
         */
        public StaffInfo(School school, Staff staff, String instructionalProgram) {
            m_school = school;
            m_instructionalProgram = instructionalProgram;
            m_staff = staff;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            return obj instanceof StaffInfo && m_school.equals(((StaffInfo) obj).m_school) &&
                    m_instructionalProgram.equals(((StaffInfo) obj).m_instructionalProgram) &&
                    m_staff.equals(((StaffInfo) obj).m_staff);
        }

        /**
         * Gets the instructional program.
         *
         * @return the instructionalProgram
         */
        public String getInstructionalProgram() {
            return m_instructionalProgram;
        }

        /**
         * Gets the school.
         *
         * @return the school
         */
        public School getSchool() {
            return m_school;
        }

        /**
         * Gets the staff.
         *
         * @return the staff
         */
        public Staff getStaff() {
            return m_staff;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            return m_school.hashCode() + m_instructionalProgram.hashCode() + m_staff.hashCode();
        }
    }

    /**
     * Entity class for staff member export.
     *
     */
    public static class TNStaffMemberEntity extends TNStateReportEntity {

        private TNStaffMemberData m_data = null;
        private ArrayList<StaffInfo> m_staffInfo = null;

        /**
         * Instantiates a new TN staff member entity.
         */
        public TNStaffMemberEntity() {
            // Public no argument constructor for dynamic instantiation.
        }

        /**
         * Retrieve the current StaffInfo this entity is on.
         *
         * @return current staff info
         */
        public StaffInfo getCurrentStaffInfo() {
            return m_staffInfo.get(getCurrentRow());
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            Staff staff = (Staff) getBean();
            String name = staff.getNameView() +
                    " [LASID: " + staff.getLocalId() +
                    ", SASID: " + staff.getStateId() +
                    "] ";

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

            m_data = (TNStaffMemberData) data;
            Staff staff = (Staff) bean;

            m_staffInfo = new ArrayList<TNStaffMemberData.StaffInfo>();

            if (m_data.m_stfSklMap.containsKey(staff.getOid())) {
                m_staffInfo.addAll(m_data.m_stfSklMap.get(staff.getOid()));
            }

            setRowCount(m_staffInfo.size());
            m_data.addEntityRowsCount(getRowCount());
        }
    }

    /**
     *
     * Field retriever for SSN.
     * This retriever normalizes SSN value to format 999999999 (nine digits without any other
     * characters)
     *
     */
    protected class FieldRetrieverSSN implements FieldRetriever {
        protected static final String STF_CALC_ID = "STF_CALC_SSN";

        private static final String CALC_PARAM_PREVSSN = "PREVSSN";
        private static final String CALC_PARAM_SSN = "SSN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberEntity seEntity = (TNStaffMemberEntity) entity;
            Staff staff = (Staff) seEntity.getBean();

            String param = (String) field.getParameter();

            String ssn = null;

            if (CALC_PARAM_SSN.equalsIgnoreCase(param)) {
                Person psn = staff.getPerson();
                ssn = (psn == null)
                        ? ""
                        : psn.getPersonId();
            }

            if (CALC_PARAM_PREVSSN.equalsIgnoreCase(param)) {
                ssn = (String) staff.getPerson().getFieldValueByBeanPath(m_fieldDoePreviousSSN);
            }

            if (StringUtils.isEmpty(ssn)) {
                return "";
            }

            return ssn.replaceAll("([^\\d]?)", "");
        }

    }

    /**
     * Validates if Staff has position in School Association if exist.
     */
    protected class FieldValidatorPosition implements FieldValidator {
        protected static final String VAL_ID = "STF_SFP";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            SisStaff stf = (SisStaff) entity.getBean();
            TNStaffMemberEntity tnEntity = (TNStaffMemberEntity) entity;
            TNStaffMemberData seData = (TNStaffMemberData) data;
            StaffInfo info = tnEntity.getCurrentStaffInfo();
            School skl = info.getSchool();
            Collection<StaffPosition> sfpList = null;
            if (seData.m_stfPositionMap.containsKey(stf.getOid())) {
                sfpList = seData.m_stfPositionMap.get(stf.getOid());
            }
            if (skl.getOid().equals(stf.getSchoolOid())) {
                // check current school position
                boolean hasPosition = false;
                if (sfpList != null) {
                    for (StaffPosition sfp : sfpList) {
                        if (sfp.getSchoolOid().equals(stf.getSchoolOid())) {
                            hasPosition = true;
                            break;
                        }
                    }
                }
                if (!hasPosition) {
                    errors.add(new StateReportValidationError(entity, field, "Staff Position Record Missing",
                            "Teacher should have a position in current school: \""
                                    + stf.getSchool().getName() + "\""));
                }
            } else {
                if (seData.m_sklAssociationMap.containsKey(stf.getOid())) {
                    Collection<StaffSchoolAssociation> sfsList = seData.m_sklAssociationMap.get(stf.getOid());
                    for (StaffSchoolAssociation sfs : sfsList) {
                        String sfsSklOid = sfs.getSchoolOid();
                        if (sfsSklOid.equals(skl.getOid())) {
                            boolean hasPosition = false;
                            if (sfpList != null) {
                                for (StaffPosition sfp : sfpList) {
                                    if (sfp.getSchoolOid().equals(sfsSklOid)) {
                                        hasPosition = true;
                                        break;
                                    }
                                }
                            }
                            if (!hasPosition) {
                                errors.add(
                                        new StateReportValidationError(entity, field, "Staff Position Record Missing",
                                                "Teacher should have a position in school: \""
                                                        + sfs.getSchool().getName() + "\""));
                            }
                        }
                    }
                }
            }
            return errors;
        }
    }

    /**
     * Field validator for SSN.
     * Validates SSN against non valid formats
     * Valid ssn format: 999999999
     */
    protected class FieldValidatorSSN implements FieldValidator {
        protected static final String STF_VAL_ID = "STF_VAL_SSN";
        private static final String patternSSN = "^[0-9]{9}$|^$";

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();

            if (value != null && !value.matches(patternSSN)) {
                errors.add(new StateReportValidationError(entity, field, "Invalid format",
                        "SSN value must be 9 digits long - currently[" +
                                value + "]"));
            }

            return errors;
        }

    }

    /**
     * Field retriever for Race indicator.
     */
    protected class RetrieveEthnicity implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            TNStaffMemberEntity seEntity = (TNStaffMemberEntity) entity;

            Staff staff = (Staff) seEntity.getBean();
            Person person = staff.getPerson();

            if (person == null) {
                return "";
            }
            return (person.getHispanicLatinoIndicator()) ? "H" : "N";
        }
    }

    /**
     * Field retriever for Instructional program field.
     */
    protected class RetrieveInstProgram implements FieldRetriever {
        public static final String TN_CALC_INSTPGM_ID = "TN_INSTRPGM_STF";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberEntity tnEntity = (TNStaffMemberEntity) entity;
            TNStaffMemberData tnData = (TNStaffMemberData) data;
            StaffInfo info = tnEntity.getCurrentStaffInfo();

            String value = null;
            if (!StringUtils.isEmpty(info.getInstructionalProgram())) {
                value = info.getInstructionalProgram();
            } else {
                SisStaff staff = (SisStaff) tnEntity.getBean();
                String calendarId = (String) m_multiYearHelper.getFieldValueByBeanPath(staff, SisStaff.COL_CALENDAR_ID);
                String calendarCode = !StringUtils.isEmpty(calendarId) ? calendarId
                        : CALENDAR_ID_STANDARD;
                SisSchool school = m_multiYearHelper.getSchool(staff);

                String key = makeCalendarLookupKey(tnData.m_contextOid, school.getOid(), calendarCode);
                if (tnData.m_calendarOids.containsKey(key)) {
                    value = tnData.m_calendarOids.get(key);
                } else {
                    String message =
                            "Value not specified. Set to " + field.getDefaultValue() + " by default. Context:" +
                                    tnData.getCurrentContext().getContextId() + ". School:" + school.getName() +
                                    ". Calendar code:" + calendarCode;
                    StateReportValidationError error =
                            new StateReportValidationError(entity, field, "Instructional Program is empty.", message);
                    entity.addRetrievalError(field.getFieldId(), error);
                }
            }

            return value;
        }

    }

    /**
     * Field retriever for Licensure Check field.
     */
    protected class RetrieveLicensureCheck implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberEntity seEntity = (TNStaffMemberEntity) entity;
            Staff stf = (Staff) seEntity.getBean();

            String value = (String) m_multiYearHelper.getFieldValueByBeanPath(stf,
                    translateAliasToJavaName(ALIAS_LIC_CHECK, false));
            String refCode = lookupReferenceCodeByAlias(ALIAS_LIC_CHECK, value,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            return refCode;
        }
    }

    /**
     * Field retriever for Licensure Number field
     *
     * Use the sfcCertNumber field and sfcPrimaryInd = true to determine which Teacher License
     * Number to use.
     *
     */
    protected class RetrieveLicensureNumber implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberEntity seEntity = (TNStaffMemberEntity) entity;
            Staff staff = (Staff) seEntity.getBean();
            TNStaffMemberData seData = (TNStaffMemberData) data;
            Collection<StaffCertification> staffCertifications = seData.m_stfCertificationMap.get(staff.getOid());
            String licensure = null;

            if (staffCertifications != null) {
                if (staffCertifications.size() > 1) {
                    StateReportValidationError error = new StateReportValidationError(seEntity, field,
                            "Staff must have only 1 primary certification.",
                            "Staff's name = " + STYLE_BOLD + staff.getNameView() + STYLE_END);
                    seEntity.addRetrievalError(field.getFieldId(), error);
                }

                StaffCertification stf = seData.m_stfCertificationMap.get(staff.getOid()).iterator().next();

                // Use the sfcCertNumber field and sfcPrimaryInd = true
                // to determine which Teacher License Number to use.

                boolean isPrimary = stf.getPrimaryIndicator();
                if (isPrimary) {
                    licensure = stf.getCertificationNumber();
                }
            }
            return licensure;
        }
    }

    /**
     * Field retriever for School.
     */
    protected class RetrieveSchool implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberEntity scge = (TNStaffMemberEntity) entity;
            StaffInfo info = scge.getCurrentStaffInfo();
            return info.getSchool().getFieldValueByBeanPath(m_fieldStateSchoolId);
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
            TNStaffMemberData seData = (TNStaffMemberData) data;
            return seData.m_schoolYear;
        }
    }

    /**
     * Field retriever for Staff Status field.
     */
    protected class RetrieveStaffStatus implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            TNStaffMemberEntity seEntity = (TNStaffMemberEntity) entity;
            Staff stf = (Staff) seEntity.getBean();
            String value = (String) m_multiYearHelper.getFieldValueByBeanPath(stf,
                    translateAliasToJavaName(ALIAS_STF_STATUS, false));
            String refCode = lookupReferenceCodeByAlias(ALIAS_STF_STATUS, value,
                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            return refCode;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    protected static final String ALIAS_STF_STATUS = "DOE EMPLOY STATUS";
    protected static final String ALIAS_DOE_PREV_SSN = "DOE PREVIOUS SSN";
    protected static final String ALIAS_EXCLUDE_STF = "DOE EXCLUDE STF";
    protected static final String ALIAS_EXIT_DATE = "DOE EXIT DATE";
    protected static final String ALIAS_LIC_CHECK = "DOE LICENSURE CHECK";
    protected static final String ALIAS_STAFF_INSTR_PRGM = "DOE INSTRUCTIONAL PROGRAM STF";

    private static final String CALC_ID_ETHNICITY = "STF_CALC_ETHNICITY";
    private static final String CALC_ID_LICCHECK = "STF_CALC_LICCHECK";
    private static final String CALC_ID_SCHOOLYEAR = "STF_CALC_SCHOOLYEAR";
    private static final String CALC_ID_SCHOOL = "STF_CALC_SCHOOL";
    private static final String CALC_ID_STFSTATUS = "STF_CALC_STFSTATUS";

    /**
     * Instance variables.
     */
    protected String m_activeStatus;
    protected String m_fieldDoePreviousSSN;
    protected String m_fieldExcludeStf;
    protected String m_fieldStaffInstructionalProgram;
    protected Map<String, Collection<Race>> m_races;
    protected String m_schoolYear;
    protected Map<String, Collection<ScheduleTeacher>> m_schTeacherMap;
    protected Map<String, Collection<StaffSchoolAssociation>> m_sklAssociationMap;
    protected Map<String, Collection<StaffCertification>> m_stfCertificationMap;
    protected Map<String, Collection<StaffPosition>> m_stfPositionMap;
    protected Map<String, Collection<StaffInfo>> m_stfSklMap;

    TNStaffMultiYearHelper m_multiYearHelper;

    private Boolean m_paramEntireSchool;
    private String m_paramSchoolOidStaff;
    private String m_paramStaffsOids;

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
    public void initialize() throws X2BaseException {
        super.initialize();
        // insure that all aliases and any other resources needed to operate the
        // report are available in the database.
        // setup error is created if the alias is not found.
        initializeFields();
        m_multiYearHelper = new TNStaffMultiYearHelper(getOrganization(), getCurrentContext(), getBroker());
        m_paramSchoolOidStaff = (String) getParameter("schoolOidStaff");
        m_paramStaffsOids = (String) getParameter("staffsOids");
        m_paramEntireSchool = Boolean.FALSE;
        if (StringUtils.isEmpty(m_paramStaffsOids)) {
            m_paramEntireSchool = Boolean.TRUE;
        }
        getCalendarsForContextOid(m_contextOid);

        if (getSetupErrors().size() != 0) {
            return;
        }

        if (getCurrentContext().getSchoolYear() > 2017) {
            setExportVersion(3);
        }

        X2Criteria criteria = getStaffCertificationCriteria();

        QueryByCriteria staffCertQuery = new QueryByCriteria(StaffCertification.class, criteria);
        staffCertQuery.addOrderBy(X2BaseBean.COL_LAST_MODIFIED_TIME, false);
        m_stfCertificationMap =
                getBroker().getGroupedCollectionByQuery(staffCertQuery, StaffCertification.COL_STAFF_OID, 1);

        X2Criteria staffCriteria = new X2Criteria();
        staffCriteria.addIn(X2BaseBean.COL_OID, m_stfCertificationMap.keySet());

        applyInputCriteria(staffCriteria, true, null);
        m_multiYearHelper.adjustCriteria(staffCriteria, Strategy.NOT_EMPTY, Staff.COL_SCHOOL_OID,
                getBroker().getPersistenceKey());

        String fieldLicCheck = translateAliasToJavaName(ALIAS_LIC_CHECK, true);
        X2Criteria stfOrCriteria = new X2Criteria();
        stfOrCriteria.addNotEmpty(SisStaff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldStateSchoolId,
                getBroker().getPersistenceKey());
        m_multiYearHelper.adjustCriteria(stfOrCriteria, Strategy.EQUAL_TO, fieldLicCheck, "N");
        if (!StringUtils.isEmpty(m_fieldExcludeStf)) {
            m_multiYearHelper.adjustCriteria(stfOrCriteria, Strategy.NOT_EQUAL_TO, m_fieldExcludeStf,
                    BooleanAsStringConverter.TRUE);
        }
        if (isSchoolContext()) {
            m_multiYearHelper.adjustCriteria(stfOrCriteria, Strategy.EQUAL_TO, Staff.COL_SCHOOL_OID,
                    getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            m_multiYearHelper.adjustCriteria(stfOrCriteria, Strategy.EQUAL_TO, Staff.COL_SCHOOL_OID,
                    m_paramSchoolOidStaff);
        }

        if (m_paramStaffsOids != null && m_paramEntireSchool.booleanValue() == false) {
            String[] staffOids = m_paramStaffsOids.split(",");
            Collection<String> collection = Arrays.asList(staffOids);
            m_multiYearHelper.adjustCriteria(stfOrCriteria, Strategy.IN, X2BaseBean.COL_OID, collection);
        }

        applyInputCriteria(stfOrCriteria, true, null);

        staffCriteria.addOrCriteria(stfOrCriteria);
        staffCriteria.addNotEmpty(SisStaff.REL_SCHOOL + ModelProperty.PATH_DELIMITER + m_fieldStateSchoolId,
                getBroker().getPersistenceKey());
        QueryByCriteria staffQuery = new QueryByCriteria(Staff.class, staffCriteria);
        applyInputSort(staffQuery, null);

        setQuery(staffQuery);
        setEntityClass(TNStaffMemberEntity.class);

        initializeStfSklMap(staffCriteria);

        // load race
        SubQuery raceSubquery = new SubQuery(Staff.class, Staff.COL_PERSON_OID, staffCriteria);
        X2Criteria raceCriteria = new X2Criteria();
        raceCriteria.addIn(Race.COL_PERSON_OID, raceSubquery);
        BeanQuery raceQuery = new BeanQuery(Race.class, raceCriteria);
        raceQuery.addOrderBy(Race.COL_PERSON_OID, true);
        m_races = getBroker().getGroupedCollectionByQuery(raceQuery, Race.COL_PERSON_OID, 1000);

        registerFieldRetrievers();
        registerFieldValidators();
    }

    /**
     * Adds the school with default.
     *
     * @param sklCollection Set<StaffInfo>
     * @param staff Staff
     * @param school School
     */
    private void addSchoolWithDefault(Set<StaffInfo> sklCollection, Staff staff, School school) {
        boolean found = false;
        for (StaffInfo item : sklCollection) {
            if (item.getSchool().getOid().equals(school.getOid())) {
                found = true;
            }
        }
        if (!found) {
            sklCollection.add(new StaffInfo(school, staff, ""));
        }
    }

    /**
     * return a set of active schools.
     *
     * @return Sets the
     */
    private Set<String> getActiveSchools() {
        Set<String> activeSchools = new HashSet();
        X2Criteria criteria = new X2Criteria();
        criteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
        criteria.addNotEmpty(m_fieldStateSchoolId, getBroker().getPersistenceKey());
        if (isSchoolContext()) {
            criteria.addEqualTo(X2BaseBean.COL_OID, getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            criteria.addEqualTo(X2BaseBean.COL_OID, m_paramSchoolOidStaff);
        }
        ReportQueryByCriteria query =
                new ReportQueryByCriteria(SisSchool.class, new String[] {X2BaseBean.COL_OID}, criteria);
        ReportQueryIterator iterator = getBroker().getReportQueryIteratorByQuery(query);
        try {
            while (iterator.hasNext()) {
                Object[] row = (Object[]) iterator.next();
                String schoolOid = (String) row[0];
                activeSchools.add(schoolOid);
            }
        } finally {
            iterator.close();
        }
        return activeSchools;
    }

    /**
     * Method for implementing business rule for schoolYear
     * (CTX_SCHOOL_YEAR - 1) where reporting date falls within `CTX_START_DATE` and `CTX_END_DATE`.
     * else CTX_SCHOOL_YEAR
     *
     * @return string representation of school year
     */
    private String getCurrentSchoolYear() {
        return Integer.toString(getCurrentContext().getSchoolYear() - 1);
    }

    /**
     * Gets the instructional program.
     *
     * @param item StaffPosition
     * @return String
     */
    private String getInstructionalProgram(StaffPosition item) {
        String value = (String) item.getFieldValueByBeanPath(m_fieldStaffInstructionalProgram);
        value = lookupReferenceCodeByBeanPath(StaffPosition.class, m_fieldStaffInstructionalProgram, value,
                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
        return StringUtils.unNullify(value);
    }

    /**
     * Function for building custom Staff criteria.
     *
     * @return X 2 criteria
     */
    private X2Criteria getStaffCertificationCriteria() {
        X2Criteria staffCertificationCriteria = new X2Criteria();
        staffCertificationCriteria.addEqualTo(StaffCertification.COL_PRIMARY_INDICATOR, "1");
        staffCertificationCriteria.addNotEmpty(
                StaffCertification.REL_STAFF + PATH_DELIMITER + SisStaff.REL_SCHOOL + PATH_DELIMITER
                        + m_fieldStateSchoolId,
                getBroker().getPersistenceKey());

        if (!StringUtils.isEmpty(m_fieldExcludeStf)) {
            m_multiYearHelper.adjustCriteria(staffCertificationCriteria, Strategy.NOT_EQUAL_TO,
                    StaffCertification.REL_STAFF + PATH_DELIMITER + m_fieldExcludeStf, BooleanAsStringConverter.TRUE);
        }
        if (isSchoolContext()) {
            m_multiYearHelper.adjustCriteria(staffCertificationCriteria, Strategy.EQUAL_TO,
                    StaffCertification.REL_STAFF + PATH_DELIMITER + Staff.COL_SCHOOL_OID, getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            X2Criteria sfpCriteria = new X2Criteria();
            sfpCriteria.addEqualTo(StaffPosition.COL_SCHOOL_OID, m_paramSchoolOidStaff);
            X2Criteria stfCriteria = new X2Criteria();
            stfCriteria.addEqualTo(StaffPosition.REL_STAFF + ModelProperty.PATH_DELIMITER + SisStaff.COL_SCHOOL_OID,
                    m_paramSchoolOidStaff);
            sfpCriteria.addOrCriteria(stfCriteria);
            SubQuery subQuery = new SubQuery(StaffPosition.class, StaffPosition.COL_STAFF_OID, sfpCriteria);
            m_multiYearHelper.adjustCriteria(staffCertificationCriteria, Strategy.IN, StaffCertification.COL_STAFF_OID,
                    subQuery);
        }
        if (m_paramStaffsOids != null && m_paramEntireSchool.booleanValue() == false) {
            String[] staffOids = m_paramStaffsOids.split(",");
            Collection<String> collection = Arrays.asList(staffOids);
            staffCertificationCriteria.addIn(StaffCertification.REL_STAFF + PATH_DELIMITER + X2BaseBean.COL_OID,
                    collection);
        }
        return staffCertificationCriteria;
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_activeStatus = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STAFF_ACTIVE_CODE);
        m_schoolYear = getCurrentSchoolYear();
        m_fieldExcludeStf = translateAliasToJavaName(ALIAS_EXCLUDE_STF, true);
        m_fieldDoePreviousSSN = translateAliasToJavaName(ALIAS_DOE_PREV_SSN, true);
        m_fieldStaffInstructionalProgram = translateAliasToJavaName(ALIAS_STAFF_INSTR_PRGM, true);
        if (getParameter(PARAM_REPORT_DATE) != null) {
            m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        }
    }

    /**
     * Initialize map of Schedule Teacher keyed on stfOid
     * It should give additional schools staff is working in.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeSchTeacherMap(X2Criteria staffCriteria) {

        X2Criteria criteria = new X2Criteria();
        // All queries using REL_ACTIVE_SCHOOL_SCHED can only be used when the selected context is
        // the current context.
        // If this is not the case, Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS must be used.
        if (m_contextOid.equals(getOrganization().getCurrentContextOid())) {
            criteria.addEqualToField(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    SisSchool.REL_ACTIVE_SCHOOL_SCHED + PATH_DELIMITER +
                    SchoolScheduleContext.COL_ACTIVE_SCHEDULE_OID,
                    ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                            MasterSchedule.COL_SCHEDULE_OID);
        } else {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + ModelProperty.PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER +
                    SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID,
                    m_contextOid);
        }
        criteria.addIn(ScheduleTeacher.COL_STAFF_OID,
                new SubQuery(Staff.class, X2BaseBean.COL_OID, staffCriteria));

        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        criteria.addNotEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        criteria.addNotEmpty(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                Schedule.REL_SCHOOL + PATH_DELIMITER +
                SisSchool.COL_ARCHIVE_INDICATOR, getBroker().getPersistenceKey());

        if (isSchoolContext()) {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    X2BaseBean.COL_OID, getSchool().getOid());
        } else if (!StringUtils.isEmpty(m_paramSchoolOidStaff)) {
            criteria.addEqualTo(ScheduleTeacher.REL_SECTION + PATH_DELIMITER +
                    MasterSchedule.REL_SCHEDULE + PATH_DELIMITER +
                    Schedule.REL_SCHOOL + PATH_DELIMITER +
                    X2BaseBean.COL_OID, m_paramSchoolOidStaff);
        }

        QueryByCriteria query = new QueryByCriteria(ScheduleTeacher.class, criteria);

        m_schTeacherMap = getBroker().getGroupedCollectionByQuery(query, ScheduleTeacher.COL_STAFF_OID, 1024);

    }

    /**
     * Initialize map of Staff Position keyed on stfOid
     * It should give additional schools staff is working in.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeStfPositionMap(X2Criteria staffCriteria) {
        X2Criteria staffPositionCriteria = new X2Criteria();
        staffPositionCriteria.addIn(StaffPosition.COL_STAFF_OID,
                new SubQuery(Staff.class, X2BaseBean.COL_OID, staffCriteria));

        PlainDate startDate = getCurrentContext().getStartDate();
        PlainDate endDate = getCurrentContext().getEndDate();

        X2Criteria criteriaStartDate = new X2Criteria();
        criteriaStartDate.addLessOrEqualThan(StaffPosition.COL_START_DATE, endDate);
        staffPositionCriteria.addAndCriteria(criteriaStartDate);

        X2Criteria criteriaEndDate = new X2Criteria();
        criteriaEndDate.addGreaterOrEqualThan(StaffPosition.COL_END_DATE, startDate);
        X2Criteria criteriaEndDateIsNull = new X2Criteria();
        criteriaEndDateIsNull.addIsNull(StaffPosition.COL_END_DATE);
        criteriaEndDate.addOrCriteria(criteriaEndDateIsNull);
        staffPositionCriteria.addAndCriteria(criteriaEndDate);

        staffPositionCriteria.addNotEqualTo(
                StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        staffPositionCriteria.addNotEqualTo(StaffPosition.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_ARCHIVE_INDICATOR,
                Boolean.TRUE);
        staffPositionCriteria.addNotEmpty(StaffPosition.REL_SCHOOL + PATH_DELIMITER + m_fieldStateSchoolId,
                getBroker().getPersistenceKey());

        QueryByCriteria query = new QueryByCriteria(StaffPosition.class, staffPositionCriteria);

        m_stfPositionMap = getBroker().getGroupedCollectionByQuery(query, StaffPosition.COL_STAFF_OID, 1024);
    }

    /**
     * Initialize map of StaffSchoolAssociations
     * It should give additional schools staff is working in.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeStfSchoolAssociationMap(X2Criteria staffCriteria) {
        X2Criteria sfsCriteria = new X2Criteria();
        sfsCriteria.addIn(StaffSchoolAssociation.COL_STAFF_OID,
                new SubQuery(SisStaff.class, X2BaseBean.COL_OID, staffCriteria));
        sfsCriteria.addEqualTo(StaffSchoolAssociation.COL_DISTRICT_CONTEXT_OID, getCurrentContext().getOid());
        QueryByCriteria query = new QueryByCriteria(StaffSchoolAssociation.class, sfsCriteria);
        m_sklAssociationMap = getBroker().getGroupedCollectionByQuery(query, StaffPosition.COL_STAFF_OID, 1024);
    }

    /**
     * Initialize map keyed on stfOid with all possible schools staff works in.
     *
     * @param staffCriteria X2Criteria
     */
    private void initializeStfSklMap(X2Criteria staffCriteria) {

        initializeSchTeacherMap(staffCriteria);
        initializeStfPositionMap(staffCriteria);
        initializeStfSchoolAssociationMap(staffCriteria);
        Set<String> activeSchools = getActiveSchools();
        m_stfSklMap = new HashMap<String, Collection<StaffInfo>>();
        QueryByCriteria query = new QueryByCriteria(Staff.class, staffCriteria);
        QueryIterator staffs = getBroker().getIteratorByQuery(query);
        try {
            while (staffs.hasNext()) {
                SisStaff stf = (SisStaff) staffs.next();
                String stfOid = stf.getOid();
                Set<StaffInfo> sklCollection = new HashSet<StaffInfo>();
                if (m_stfPositionMap.containsKey(stfOid)) {
                    for (StaffPosition item : m_stfPositionMap.get(stfOid)) {
                        if (activeSchools.contains(item.getSchoolOid())) {
                            String instructionalProgram = getInstructionalProgram(item);
                            sklCollection.add(new StaffInfo(item.getSchool(), stf, instructionalProgram));
                        }
                    }
                }
                if (m_schTeacherMap.containsKey(stfOid)) {
                    for (ScheduleTeacher teacher : m_schTeacherMap.get(stfOid)) {
                        if (activeSchools.contains(m_multiYearHelper.getFieldValueByBeanPath(teacher.getStaff(),
                                SisStaff.COL_SCHOOL_OID))) {
                            addSchoolWithDefault(sklCollection, stf, teacher.getSection().getSchedule().getSchool());
                        }

                    }
                }

                String schoolOid = (String) m_multiYearHelper.getFieldValueByBeanPath(stf, SisStaff.COL_SCHOOL_OID);
                if (!StringUtils.isEmpty(schoolOid) && !activeSchools.contains(schoolOid) &&
                        (m_paramSchoolOidStaff == null || m_paramSchoolOidStaff == schoolOid)) {
                    addSchoolWithDefault(sklCollection, stf, m_multiYearHelper.getSchool(stf));
                }

                m_stfSklMap.put(stfOid, sklCollection);
            }
        } finally {
            if (staffs != null) {
                staffs.close();
            }
        }
    }

    /**
     * Register custom field Retrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CALC_ID_ETHNICITY, new RetrieveEthnicity());
        calcs.put(RetrieveInstProgram.TN_CALC_INSTPGM_ID, new RetrieveInstProgram());
        calcs.put(CALC_TYPE_RACE, new RaceRetriever(m_races));
        calcs.put(CALC_ID_SCHOOLYEAR, new RetrieveSchoolYear());
        calcs.put(CALC_ID_SCHOOL, new RetrieveSchool());
        calcs.put(CALC_ID_LICCHECK, new RetrieveLicensureCheck());
        calcs.put(CALC_ID_STFSTATUS, new RetrieveStaffStatus());
        calcs.put(FieldRetrieverSSN.STF_CALC_ID, new FieldRetrieverSSN());
        calcs.put("STF_CALC_LICNUM", new RetrieveLicensureNumber());
        calcs.put(RetrieveTruncatedNames.TN_CALC_NAME, new RetrieveTruncatedNames());
        super.addCalcs(calcs);
    }

    /**
     * method for initialization of field validators.
     */
    private void registerFieldValidators() {
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put(FieldValidatorSSN.STF_VAL_ID, new FieldValidatorSSN());
        validators.put(FieldValidatorPosition.VAL_ID, new FieldValidatorPosition());
        super.addValidators(validators);
    }

}
