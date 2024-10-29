/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2009 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

package com.x2dev.procedures.statereporting.nh;

import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Address;
import com.follett.fsc.core.k12.beans.Organization;
import com.follett.fsc.core.k12.beans.Person;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.SchoolCalendarDate;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.Schedule;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.business.EnrollmentManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;

/**
 * This class implements the data export for New Hampshire's i4see Enrollment export.
 *
 * @author X2 Development Corporation
 */
public class NHI4SeeEnrollmentCATE extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the I4SeeEnrollment export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class I4SeeEntity extends StateReportEntity {
        private NHI4SeeEnrollmentCATE m_data;

        /**
         * List of StudentProgramParticipation that reflect this students participation in CATE.
         */
        private List<StudentProgramParticipation> m_programs;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public I4SeeEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Check enrollment membership count and membership days parameter to determine if the
         * student should be reported.
         *
         * @return StateReportValidationError
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#filterEntity()
         */
        @Override
        public StateReportValidationError filterEntity() {
            StateReportValidationError error = null;

            FieldDefinition field = m_data.getFieldDefinition(NAME_ATTENDANCE_DAYS_S1);

            /*
             * Get membership days parameter
             */
            double membershipCountAsDouble = 0;

            /*
             * Get membership count
             */
            String membershipCount = m_data.getMembershipDays(this, SEMESTER.FY, false);

            if (membershipCount != null) {
                try {
                    membershipCountAsDouble = Double.parseDouble(membershipCount);
                } catch (NumberFormatException nfe) {
                    // invalid format, will be reported elsewhere.
                }
            }

            // check enrollment count and membership days parameter.
            if ((m_requireMemberDay && membershipCountAsDouble > 0) || !m_requireMemberDay) {
                // No filtering.
            } else {
                // Student filtered.
                error = new StateReportValidationError(this, field, "0 member days - excluded from export", "");
            }

            return error;
        }

        /**
         * Returns the StudentProgramParticipation that represents the current report row.
         *
         * @return StudentProgramParticipation
         */
        public StudentProgramParticipation getCurrentProgram() {
            StudentProgramParticipation program = null;
            int index = getCurrentRow();
            if (index >= 0 && index < m_programs.size()) {
                program = m_programs.get(index);
            }
            return program;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            SisStudent student = (SisStudent) getBean();
            String name = student.getNameView() +
                    " [LASID: " + student.getLocalId() +
                    ", SASID: " + student.getStateId() +
                    (m_data.isSchoolContext() ? ", SCHOOL: " + student.getSchool().getName() : "") +
                    "]";

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

            SisStudent student = (SisStudent) bean;
            m_data = (NHI4SeeEnrollmentCATE) data;
            m_programs = m_data.getProgram(student.getOid());

            setRowCount(m_programs.size());
        }

        /**
         * If update calculated fields is set, save new values into the bean.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (m_updateRecords) {
                try {

                    PropertyUtils.setProperty(getBean(), m_data.m_studentDaysInAttendanceS1,
                            getFieldValue(NAME_ATTENDANCE_DAYS_S1));
                    PropertyUtils.setProperty(getBean(), m_data.m_studentDaysInAttendanceS2,
                            getFieldValue(NAME_ATTENDANCE_DAYS_S2));

                    if (getBean().isDirty()) {
                        m_data.getBroker().saveBeanForced(getBean());
                    }
                } catch (IllegalAccessException e) {
                    // conversion errors. Cannot save student.
                } catch (InvocationTargetException e) {
                    // conversion errors. Cannot save student.
                } catch (NoSuchMethodException e) {
                    // conversion errors. Cannot save student.
                }
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
     * The Enum SEMESTER.
     */
    public enum SEMESTER {
        FY, S1, S2
    }

    /**
     * The Class Retrieve133PostalCode.
     */
    protected class Retrieve133PostalCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            Person person = student.getPerson();
            Address address = person.getPhysicalAddress();

            value = address != null ? address.getPostalCode() : "";

            return value;
        }
    }

    /**
     * Returns the entry code for the student.
     */
    protected class Retrieve230EntryDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Date entryExitDate = null;

            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            if (program != null) {
                entryExitDate = program.getStartDate();
            }

            return entryExitDate;
        }
    }

    /**
     * Returns the entry code for the student.
     */
    protected class Retrieve240EntryCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            String code = null;
            String stateCode = null;

            if (program != null) {
                code = (String) program.getFieldValueByBeanPath(m_progCateEntryCodeField);

                if (code != null) {
                    stateCode = lookupStateValue(StudentProgramParticipation.class, m_progCateEntryCodeField, code);

                    if (stateCode == null) {
                        stateCode = code;
                    }
                }
            }

            return stateCode;
        }
    }

    /**
     * Returns the exit date for the student.
     */
    protected class Retrieve250ExitDate implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            PlainDate programExitDate = null;

            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();
            SisStudent student = (SisStudent) entity.getBean();

            if (program != null) {
                programExitDate = program.getEndDate();
                if (programExitDate != null) {
                    return programExitDate;
                }
                // Default to last in-session date of student calendar for EOY. If no calendar
                // exists, use district end date for current school year.
                // These values are lazy loaded into m_lastInSessionDateMap using the format
                // HashMap<schoolOid, HashMap<calendarId, PlainDate>>
                else if (m_reportType.intValue() == 1 && programExitDate == null) {
                    PlainDate lastInSessionDay = null;

                    String calendarId = student.getCalendarCode();
                    if (m_lastInSessionDateMap.get(student.getSchoolOid()) != null
                            && m_lastInSessionDateMap.get(student.getSchoolOid()).get(calendarId) != null) {
                        programExitDate = m_lastInSessionDateMap.get(student.getSchoolOid()).get(calendarId);
                    } else {
                        Map<String, Set<PlainDate>> calendarDatesMap =
                                m_enrollManager.getCalendarLookup(student.getSchool(), m_schoolYearStartDate,
                                        m_endOfYearDate, getCurrentContext().getOid());
                        for (PlainDate date : calendarDatesMap.get(calendarId)) {
                            if (lastInSessionDay == null || lastInSessionDay.before(date)) {
                                lastInSessionDay = date;
                            }
                        }
                        if (lastInSessionDay != null) {
                            programExitDate = lastInSessionDay;
                        } else {
                            programExitDate = m_endOfYearDate;
                        }

                        // Add schoolOid to map if it doesn't already exist
                        if (!m_lastInSessionDateMap.containsKey(student.getSchoolOid())) {
                            HashMap<String, PlainDate> lastDateByCalendar = new HashMap<String, PlainDate>();
                            m_lastInSessionDateMap.put(student.getSchoolOid(), lastDateByCalendar);
                        }

                        HashMap<String, PlainDate> lastDateByCalendar =
                                m_lastInSessionDateMap.get(student.getSchoolOid());
                        if (!lastDateByCalendar.containsKey(calendarId)) {
                            lastDateByCalendar.put(calendarId, programExitDate);
                        }
                    }
                }
            }
            return programExitDate;
        }
    }

    /**
     * Returns the exit code for the student.
     */
    protected class Retrieve260ExitCode implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            String code = null;
            String stateCode = null;

            if (program != null) {
                code = (String) program.getFieldValueByBeanPath(m_progCateExitCodeField);

                if (code != null) {
                    stateCode = lookupStateValue(StudentProgramParticipation.class, m_progCateExitCodeField, code);

                    if (stateCode == null) {
                        stateCode = code;
                    }
                }
            }

            if (m_reportType.intValue() == 1 && StringUtils.isEmpty(stateCode)) {
                stateCode = "W31"; // default value for Student is Continuing used for EOY records
            }

            return stateCode;
        }
    }


    /**
     * Retrieve days in S1 attendance for the student. If the count is zero, use the 555
     * placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1764DaysInAttendanceS1 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String attendanceCount = null;
            attendanceCount = getMembershipDays((I4SeeEntity) entity, SEMESTER.S1, true);
            return attendanceCount;
        }
    }

    /**
     * Retrieve days in S2 attendance for the student. If the count is zero, use the 555
     * placeholder.
     *
     * @author X2 Development Corporation
     */
    protected class Retrieve1765DaysInAttendanceS2 implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String attendanceCount = null;
            attendanceCount = getMembershipDays((I4SeeEntity) entity, SEMESTER.S2, true);
            return attendanceCount;
        }
    }

    /**
     * Returns fields from the newest CATE Student Program Participation record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgramParticipationFields implements FieldRetriever {
        private final List fields = Arrays.asList(
                ALIAS_I4SEE_1500_SAU_NUMBER_SEND, // 0
                ALIAS_I4SEE_1510_DISTRICT_NUMBER_SEND, // 1
                ALIAS_I4SEE_1520_SCHOOL_NUMBER_SEND, // 2
                ALIAS_I4SEE_1600_SAU_NUMBER_RCV, // 3
                ALIAS_I4SEE_1610_DISTRICT_NUMBER_RCV // 4
        );

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            I4SeeEntity i4seeEntity = (I4SeeEntity) entity;
            String value = null;
            String parameter = (String) field.getParameter();


            StudentProgramParticipation program = i4seeEntity.getCurrentProgram();

            if (program != null) {
                if (CALC_PARAM_I4SEE_1500.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1500_SAU_NUMBER_SEND);
                } else if (CALC_PARAM_I4SEE_1510.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1510_DISTRICT_NUMBER_SEND);
                } else if (CALC_PARAM_I4SEE_1520.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1520_SCHOOL_NUMBER_SEND);
                } else if (CALC_PARAM_I4SEE_1600.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1600_SAU_NUMBER_RCV);
                } else if (CALC_PARAM_I4SEE_1610.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1610_DISTRICT_NUMBER_RCV);
                } else if (CALC_PARAM_I4SEE_1710.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1710_PRIMARY_PROGRAM_ID);
                } else if (CALC_PARAM_I4SEE_1720.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1720_PROGRAM_COMPLETER);
                } else if (CALC_PARAM_I4SEE_1730.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_1730_TRAN_MODE);
                } else if (CALC_PARAM_I4SEE_162.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_162_PRIMARY_PROGRAM_FLAG);
                } else if (CALC_PARAM_I4SEE_163.equals(parameter)) {
                    value = (String) program.getFieldValueByAlias(ALIAS_I4SEE_163_TSA_FLAG);
                }

                /*
                 * If the data wasn't found in the Program record, go to the primary school and get
                 * it there
                 */
                if (value == null) {
                    value = getValueFromPrimarySchool(entity, field);
                }
            }

            return value;
        }

        /**
         * Gets the value from primary school.
         *
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return String
         * @throws X2BaseException exception
         */
        private String getValueFromPrimarySchool(StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool school = student.getSchool();
            String parameter = (String) field.getParameter();

            if (parameter != null && fields.contains(parameter)) {
                Organization organization = school.getOrganization1();

                switch (fields.indexOf(parameter)) {
                    case 0: // I4SEE_1500_SAU_NUMBER_SEND
                    case 3: // I4SEE_1600_SAU_NUMBER_RCV
                        value = (String) organization.getFieldValueByBeanPath(m_orgSauNumberField);
                        break;

                    case 1: // I4SEE_1510_DISTRICT_NUMBER_SEND
                    case 4: // I4SEE_1610_DISTRICT_NUMBER_RCV
                        value = (String) organization.getFieldValueByBeanPath(m_orgDistrictNumberField);
                        break;

                    case 2: // I4SEE_1520_SCHOOL_NUMBER_SEND
                        value = (String) school.getFieldValueByBeanPath(m_schoolIdField);
                        break;
                    default:
                        break;
                }
            }

            return value;
        }
    }

    /**
     * The Class RetrieveStudent.
     */
    protected class RetrieveStudent implements FieldRetriever {

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.
         *      core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            String parameter = (String) field.getParameter();
            SisStudent student = (SisStudent) entity.getBean();

            if (CALC_PARAM_I4SEE_160.equals(parameter)) {
                value = (String) student.getFieldValueByAlias(ALIAS_I4SEE_160_DISPLACED_HOMEMAKER);

            } else if (CALC_PARAM_I4SEE_161.equals(parameter)) {
                value = (String) student.getFieldValueByAlias(ALIAS_I4SEE_161_SINGLE_PARENT);
            }

            if (StringUtils.isEmpty(value) && m_reportType.intValue() == 1) // if EOY default to "N"
            {
                value = "N";
            }

            return value;
        }
    }

    /**
     * Validate entry code.
     *
     */
    protected class Validate240EntryCode implements FieldValidator {

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

            if (!VALID_ENTRY_STATUS_CODES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field, field.getFieldId() +
                        " is not valid",
                        field.getFieldId() + " = " + value));
            }
            return errors;
        }
    }

    /**
     * Validate exit code.
     */
    protected class Validate260ExitCode implements FieldValidator {

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

            String exitDate = entity.getFieldValue(NAME_I4SEE_250_EXIT_DATE);
            SisStudent student = (SisStudent) entity.getBean();
            if (StringUtils.isEmpty(value) && m_reportType.intValue() == 1) // Exit code is required
                                                                            // for EOY collection
            {
                String errorId = "Exit Date is required";
                String errorMessage = "Exit Date = " + exitDate;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            if (m_reportType.intValue() == 0 && // Only throw this validation error for BOY/MOY,
                                                // both fields are required for EOY
                    (!StringUtils.isEmpty(exitDate) && StringUtils.isEmpty(value))
                    || (StringUtils.isEmpty(exitDate) && !StringUtils.isEmpty(value))) {
                String errorId = "Exit Code and Exit Date must either both be empty, or both be populated";
                String errorMessage = "Exit Date = " + exitDate + ", Exit Code = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }

            if (CODE_ENROLLMENT_W11.equals(value)) {
                String promotedInd = (String) student.getFieldValueByAlias(ALIAS_I4SEE_510_PROMOTED_IND);

                if (!CODE_PROMOTED_IND_COMPLETED.equals(promotedInd)) {
                    String errorId = "Exit Code is not valid for Promoted Indicator " + promotedInd;
                    String errorMessage = "Exit Code = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }

                String gradeLevel = (String) student.getFieldValueByAlias(ALIAS_I4SEE_400_GRADE_LEVEL);

                if (!(CODE_GRADE_LEVEL_11.equals(gradeLevel) || CODE_GRADE_LEVEL_12.equals(gradeLevel))) {
                    String errorId = "Exit Code is not valid for Grade Level " + gradeLevel;
                    String errorMessage = "Exit Code = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            if ((value != null) && value.matches("W2[0123456789]")) {
                String gradeLevel = (String) student.getFieldValueByAlias(ALIAS_I4SEE_400_GRADE_LEVEL);

                if (!(CODE_GRADE_LEVEL_9.equals(gradeLevel) || CODE_GRADE_LEVEL_10.equals(gradeLevel) ||
                        CODE_GRADE_LEVEL_11.equals(gradeLevel) || CODE_GRADE_LEVEL_12.equals(gradeLevel))) {
                    String errorId = "Exit Code is not valid for Grade Level " + gradeLevel;
                    String errorMessage = "Exit Code = " + value;
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
                if (!StringUtils.isEmpty(exitDate)) {
                    try {
                        PlainDate exDate = (PlainDate) m_dateFormat.parse(exitDate);
                        if (student.getPerson() != null) {
                            int ageAsOfExitDate = student.getPerson().getAgeAsOfDate(exDate);
                            if (ageAsOfExitDate < 16) {
                                String errorId = "Exit Code is invalid for student's age as of exit date";
                                String errorMessage = "Exit Code = " + value + ", Exit Date = " + exitDate + ", Age = "
                                        + Integer.toString(ageAsOfExitDate);
                                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                            }
                        }
                    } catch (ParseException pe) {
                        // unable to format date
                    }

                }
            }
            if (!StringUtils.isEmpty(value) && !VALID_EXIT_CODE_VALUES.contains(value)) {
                String errorId = "Exit Code is not valid";
                String errorMessage = "Exit Code = " + value;
                errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
            }
            if ("W9".equals(value) && student.getPerson() != null) {
                int studentsAge = student.getPerson().getAge();
                if (studentsAge > 17) {
                    String errorId = "Exit Code is invalid for student's age";
                    String errorMessage = "Exit Code = " + value + ", Age = " + Integer.toString(studentsAge);
                    errors.add(new StateReportValidationError(entity, field, errorId, errorMessage));
                }
            }

            return errors;
        }
    }

    /**
     * Validate CATE Enrollment Status.
     */
    protected class Validate1700CATEEnrollmentStatus implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!VALID_1700_ENROLLMENT_STATUS_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " is not a valid code.",
                        field.getFieldId() + " = " + value));

            }

            return errors;
        }
    }

    /**
     * Validate that the field is not blank for EOY collection only.
     */
    protected class ValidateEOYNotBlank implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (StringUtils.isEmpty(value) && m_reportType.intValue() == 1) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " is required",
                        field.getFieldId() + " = " + value));
            }

            return errors;
        }
    }

    /**
     * Validate that the Telephone value is either blank or 7-14 characters.
     */
    protected class ValidateTelephoneLength implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!StringUtils.isEmpty(value) && (value.length() < 7 || value.length() > 14)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " must be blank or between 7-14 characters",
                        field.getFieldId() + " = " + value));
            }
            return errors;
        }
    }

    /**
     * Validate Tran Mode.
     */
    protected class ValidateTranMode implements FieldValidator {

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
         * @see
         *      com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett
         *      .fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection getFieldValidation(StateReportData data,
                                             StateReportEntity entity,
                                             FieldDefinition field,
                                             String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            if (!VALID_TRAN_MODE_VALUES.contains(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        field.getFieldId() + " is not a valid code",
                        field.getFieldId() + " = " + value));
            }

            return errors;
        }
    }

    /**
     * Input Definition Parameters
     */
    public static final String CALCULATE_TOTALS_PARAM = "calculateTotals";
    public static final String INCLUDE_INACTIVE_STUDENTS_PARAM = "includeInactive";
    public static final String INCLUDE_STUDENT_NAMES_PARAM = "includeStudentName";
    public static final String MOY_DATE_PARAM = "moyDate";
    public static final String REPORT_DATE_PARAM = "reportDate";
    public static final String REPORT_TYPE_PARAM = "reportType";
    public static final String REQUIRE_MEMBER_DAY_PARAM = "requireMemberDay";
    public static final String SUMMER_END_DATE_PARAM = "summerEndDate";
    public static final String SUMMER_START_DATE_PARAM = "summerStartDate";
    public static final String UPDATE_RECORDS_PARAM = "updateRecords";

    /**
     * Retriever Parameters
     */
    protected static final String CALC_ID_I4SEE_133_POSTAL_CODE = "I4SEE133";
    protected static final String CALC_ID_I4SEE_1764_ATTENDANCE_DAYS_S1 = "I4SEE1764";
    protected static final String CALC_ID_I4SEE_1765_ATTENDANCE_DAYS_S2 = "I4SEE1765";
    protected static final String CALC_ID_I4SEE_230_ENTRY_DATE = "I4SEE230";
    protected static final String CALC_ID_I4SEE_240_ENTRY_CODE = "I4SEE240";
    protected static final String CALC_ID_I4SEE_250_EXIT_DATE = "I4SEE250";
    protected static final String CALC_ID_I4SEE_260_EXIT_CODE = "I4SEE260";
    protected static final String CALC_ID_I4SEE_400_GRADE_LEVEL = "I4SEE400";
    protected static final String CALC_ID_I4SEE_PROGRAM = "PROGRAM";
    protected static final String CALC_ID_I4SEE_STUDENT = "STUDENT";

    protected static final String CALC_PARAM_I4SEE_1500 = "i4see 1500";
    protected static final String CALC_PARAM_I4SEE_1510 = "i4see 1510";
    protected static final String CALC_PARAM_I4SEE_1520 = "i4see 1520";
    protected static final String CALC_PARAM_I4SEE_1600 = "i4see 1600";
    protected static final String CALC_PARAM_I4SEE_1610 = "i4see 1610";
    protected static final String CALC_PARAM_I4SEE_1710 = "i4see 1710";
    protected static final String CALC_PARAM_I4SEE_1720 = "i4see 1720";
    protected static final String CALC_PARAM_I4SEE_1730 = "i4see 1730";
    protected static final String CALC_PARAM_I4SEE_160 = "i4see 160";
    protected static final String CALC_PARAM_I4SEE_161 = "i4see 161";
    protected static final String CALC_PARAM_I4SEE_162 = "i4see 162";
    protected static final String CALC_PARAM_I4SEE_163 = "i4see 163";

    protected static final String VAL_ID_EOY_NO_BLANKS = "EOYNOTBLANK";
    protected static final String VAL_ID_I4SEE_240_ENTRY_CODE = "I4SEE240";
    protected static final String VAL_ID_I4SEE_260_EXIT_CODE = "I4SEE260";
    protected static final String VAL_ID_I4SEE_1700_CATE_ENROLLMENT = "I4SEE1700";
    protected static final String VAL_ID_TEL_LENGTH = "TEL-LENGTH";
    protected static final String VAL_ID_TRAN_MODE = "VAL-TRAN";

    /*
     * Aliases
     */
    // ORGANIZATION
    private static final String ALIAS_I4SEE_040_DISTRICT_NUMBER = "i4see 040";
    private static final String ALIAS_I4SEE_030_SAU_NUMBER = "i4see 030";

    // SCHOOL
    private static final String ALIAS_SCHOOL_EXCLUDE = "i4see EXCLUDE SCHOOL";
    private static final String ALIAS_I4SEE_050_SCHOOL_ID = "i4see 050";

    // STUDENT
    private static final String ALIAS_ADJUSTED_SCHOOL_NUMBER = "i4see Adj School Number";
    private static final String ALIAS_I4SEE_CATE_STATUS = "i4see CATE Status";
    private static final String ALIAS_STUDENT_EXCLUDE = "I4SEE EXCLUDE";
    private static final String ALIAS_I4SEE_160_DISPLACED_HOMEMAKER = "i4see 160";
    private static final String ALIAS_I4SEE_161_SINGLE_PARENT = "i4see 161";
    private static final String ALIAS_I4SEE_162_PRIMARY_PROGRAM_FLAG = "i4see 162";
    private static final String ALIAS_I4SEE_163_TSA_FLAG = "i4see 163";
    private static final String ALIAS_I4SEE_400_GRADE_LEVEL = "i4see 400";
    private static final String ALIAS_I4SEE_500_HOMELESS_CD = "i4see 500";
    private static final String ALIAS_I4SEE_510_PROMOTED_IND = "i4see 510";
    private static final String ALIAS_I4SEE_580_RESIDENTIAL_HOME = "i4see 580";
    private static final String ALIAS_I4SEE_610_PORT_GRAD = "i4see 610";
    private static final String ALIAS_I4SEE_620_DIPLOMA_TYPE = "i4see 620";
    private static final String ALIAS_I4SEE_1764_ATTENDANCE_S1 = "i4see 1764";
    private static final String ALIAS_I4SEE_1765_ATTENDANCE_S2 = "i4see 1765";
    private static final String ALIAS_I4SEE_STATUS = "i4see Status";

    // STUDENT_PROGRAM_PARTICIPATION
    private static final String ALIAS_I4SEE_230_CATE_ENTRY_DATE = "i4see 230 CATE";
    private static final String ALIAS_I4SEE_240_CATE_ENTRY_CODE = "i4see 240 CATE";
    private static final String ALIAS_I4SEE_250_CATE_EXIT_DATE = "i4see 250 CATE";
    private static final String ALIAS_I4SEE_260_CATE_EXIT_CODE = "i4see 260 CATE";
    private static final String ALIAS_I4SEE_1500_SAU_NUMBER_SEND = "i4see 1500";
    private static final String ALIAS_I4SEE_1510_DISTRICT_NUMBER_SEND = "i4see 1510";
    private static final String ALIAS_I4SEE_1520_SCHOOL_NUMBER_SEND = "i4see 1520";
    private static final String ALIAS_I4SEE_1600_SAU_NUMBER_RCV = "i4see 1600";
    private static final String ALIAS_I4SEE_1610_DISTRICT_NUMBER_RCV = "i4see 1610";
    private static final String ALIAS_I4SEE_1710_PRIMARY_PROGRAM_ID = "i4see 1710";
    private static final String ALIAS_I4SEE_1720_PROGRAM_COMPLETER = "i4see 1720";
    private static final String ALIAS_I4SEE_1730_TRAN_MODE = "i4see 1730";
    private static final String ALIAS_I4SEE_CATE_CONTEXT_ID = "i4see CATE CONTEXT";

    /**
     * Field Names
     */
    private static final String NAME_I4SEE_250_EXIT_DATE = "Exit Date";
    private static final String NAME_ATTENDANCE_DAYS_S1 = "Attendance Days S1";
    private static final String NAME_ATTENDANCE_DAYS_S2 = "Attendance Days S2";

    /*
     * Other internal constants
     */
    private static final String DATE_FORMAT = "MM/dd/yyyy";
    private static final String PARAM_STATUS_NOT_REPORT = "Do not report";
    private static final String PARAM_STATUS_REPORT = "Report";

    public static final Integer REPORT_TYPE_BOY = Integer.valueOf(0);
    public static final Integer REPORT_TYPE_EOY = Integer.valueOf(1);

    private static final String CODE_GRADE_LEVEL_11 = "11";
    private static final String CODE_GRADE_LEVEL_12 = "12";
    private static final String CODE_GRADE_LEVEL_10 = "10";
    private static final String CODE_GRADE_LEVEL_9 = "9";

    private static final String CODE_PROMOTED_IND_COMPLETED = "3";

    private static final String CODE_ENROLLMENT_W11 = "W11";

    private static final String CODE_CATE_PROGRAM_CODE = "CATE";

    private static final String STUDENT_NAME = "name view";

    protected static final Collection VALID_1700_ENROLLMENT_STATUS_VALUES = Arrays.asList("1", "4", "9", "18", "22");
    protected static final Collection VALID_ENTRY_STATUS_CODES =
            Arrays.asList("E1", "E2", "R1", "R2", "R3", "R4", "R5", "R6",
                    "R7", "R8", "R12", "R15", "V1", "V2", "V3", "V4");
    protected static final Collection VALID_TRAN_MODE_VALUES = Arrays.asList("2", "6", "8");
    protected static final Collection VALID_EXIT_CODE_VALUES =
            Arrays.asList("W1", "W2", "W3", "W4", "W5", "W6", "W8", "W9",
                    "W10", "W11", "W12", "W14", "W15", "W20", "W21",
                    "W22", "W23", "W24", "W25", "W26", "W27", "W28",
                    "W29", "W31", "W32", "W33", "W34", "W35", "W36",
                    "W37", "W38", "W39", "W40");

    private static final String OFF_TRACK_CODE = "OFTR";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */

    protected boolean m_calculateTotals = false;
    protected boolean m_includeInactiveStudents = false;
    protected boolean m_includeStudentNames = false;
    protected Converter m_integerConverter;
    protected EnrollmentManager m_enrollManager;
    protected HashMap m_schoolsToCalendars = new HashMap();

    protected Integer m_reportType;
    protected Map<String, List<StudentProgramParticipation>> m_existingPrograms;
    protected Map<String, Schedule> m_scheduleMap = new HashMap();
    protected Map<String, SchoolCalendarDate> m_schoolCalendarDateMap = new HashMap();
    protected Map<String, SisSchool> m_schoolMap;
    protected HashMap<String, HashMap<String, PlainDate>> m_lastInSessionDateMap; // lazy loaded for
                                                                                  // each schoolOid
                                                                                  // and calendarId
                                                                                  // ..
                                                                                  // HashMap<schoolOid,
                                                                                  // HashMap<calendarId,
                                                                                  // PlainDate>>
    protected PlainDate m_middleOfYearDate;
    protected PlainDate m_reportDate;
    protected PlainDate m_schoolYearStartDate;
    protected PlainDate m_endOfYearDate;
    protected PlainDate m_summerEndDate;
    protected PlainDate m_summerStartDate;
    protected Set m_firstDayMembers;
    protected SimpleDateFormat m_dateFormat;
    protected static boolean m_requireMemberDay = false;
    protected static boolean m_updateRecords = false;
    protected String m_conductSuspensionLengthField;
    protected String m_orgDistrictNumberField;
    protected String m_orgSauNumberField;
    protected String m_progCateContextIdField;
    protected String m_progCateEntryCodeField;
    protected String m_progCateEntryDateField;
    protected String m_progCateExitCodeField;
    protected String m_progCateExitDateField;
    protected String m_schoolExcludeField;
    protected String m_schoolIdField;
    protected String m_studentAdjustedSchoolCodeField;
    protected String m_studentCateStatusField;
    protected String m_studentDaysInAttendanceS1;
    protected String m_studentDaysInAttendanceS2;
    protected String m_studentDiplomaTypeField;
    protected String m_studentExcludeField;
    protected String m_studentHomelessCodeField;
    protected String m_studentPostGradField;
    protected String m_studentPromotedIndField;
    protected String m_studentReportStatusField;
    protected String m_studentResidentialHomeField;
    protected StudentHistoryHelper m_helper;

    /**
     * Returns the class of the base bean used for this export.
     *
     * @return Class
     */
    @Override
    public Class getBeanClass() {
        return SisStudent.class;
    }

    /**
     * Returns the title of the report for report headings.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getExportTitle()
     */
    @Override
    public String getExportTitle() {
        return "I4SEE CATE ENROLLMENT";
    }

    /**
     * Sets the header row display mode in the export.
     *
     * @return boolean
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getIncludeHeaderRow()
     */
    @Override
    public boolean getIncludeHeaderRow() {
        return false;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        initializeFields();

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Load initialization data
             */

            loadCATEStudentProgramParticipation();
            /*
             * Build query object that will be used to retrieve export students.
             */
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_schoolYearStartDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_SCHOOL,
                    isSchoolContext() ? Boolean.TRUE : Boolean.FALSE);
            if (m_includeInactiveStudents) {
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_INCLUDE_INACTIVE, Boolean.TRUE);
            }

            X2Criteria inclusionCriteria1 = new X2Criteria();
            inclusionCriteria1.addNotEqualTo(m_studentExcludeField, BooleanAsStringConverter.TRUE);
            inclusionCriteria1.addNotEqualTo(m_studentReportStatusField, PARAM_STATUS_NOT_REPORT);
            X2Criteria inclusionCriteria2 = new X2Criteria();
            inclusionCriteria2.addEqualTo(m_studentCateStatusField, PARAM_STATUS_REPORT);
            X2Criteria inclusionCriteria = new X2Criteria();
            inclusionCriteria.addOrCriteria(inclusionCriteria1);
            inclusionCriteria.addOrCriteria(inclusionCriteria2);
            m_helper.getStudentCriteria().addAndCriteria(inclusionCriteria);

            loadSchools();

            loadActiveSchedules();

            /*
             * Add the Student Name field if requested
             */
            if (m_includeStudentNames) {
                getFieldDefinitions().add(0, getName());
            }

            /*
             * Set up Retrievers and Validators
             */
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(CALC_ID_I4SEE_PROGRAM, new RetrieveProgramParticipationFields());
            calcs.put(CALC_ID_I4SEE_230_ENTRY_DATE, new Retrieve230EntryDate());
            calcs.put(CALC_ID_I4SEE_240_ENTRY_CODE, new Retrieve240EntryCode());
            calcs.put(CALC_ID_I4SEE_250_EXIT_DATE, new Retrieve250ExitDate());
            calcs.put(CALC_ID_I4SEE_260_EXIT_CODE, new Retrieve260ExitCode());
            calcs.put(CALC_ID_I4SEE_133_POSTAL_CODE, new Retrieve133PostalCode());
            calcs.put(CALC_ID_I4SEE_1764_ATTENDANCE_DAYS_S1, new Retrieve1764DaysInAttendanceS1());
            calcs.put(CALC_ID_I4SEE_1765_ATTENDANCE_DAYS_S2, new Retrieve1765DaysInAttendanceS2());
            calcs.put(CALC_ID_I4SEE_STUDENT, new RetrieveStudent());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldRetriever>();
            validators.put(VAL_ID_I4SEE_240_ENTRY_CODE, new Validate240EntryCode());
            validators.put(VAL_ID_I4SEE_260_EXIT_CODE, new Validate260ExitCode());
            validators.put(VAL_ID_TRAN_MODE, new ValidateTranMode());
            validators.put(VAL_ID_EOY_NO_BLANKS, new ValidateEOYNotBlank());
            validators.put(VAL_ID_TEL_LENGTH, new ValidateTelephoneLength());
            super.addValidators(validators);

            setQuery(m_helper.getStudentQuery(false));
            setEntityClass(I4SeeEntity.class);
        }
    }

    /**
     * Returns the number of days the student has been a member during a particular semester
     * from the start of school to the report date.
     *
     * @param i4see I4SeeEntity
     * @param semester - S1, S2, or FY
     * @param attendance boolean
     * @return String
     */
    protected String getMembershipDays(I4SeeEntity i4see, SEMESTER semester, boolean attendance) {
        String count = null;
        SisStudent student = (SisStudent) i4see.getBean();

        // Check the active schedule for the school.
        SisSchool school = m_schoolMap.get(student.getSchoolOid());
        Schedule schedule = null;

        if (school != null) {
            schedule = m_scheduleMap.get(school.getOid());
            if (schedule != null) {
                try {
                    StudentProgramParticipation program = i4see.getCurrentProgram();
                    PlainDate startDate = m_scheduleMap.get(student.getSchoolOid()).getStartDate();
                    PlainDate endDate;

                    X2Criteria criteria = new X2Criteria();
                    criteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                    criteria.addEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);

                    X2Criteria attCriteria = new X2Criteria();
                    attCriteria.addEqualTo(StudentAttendance.COL_STUDENT_OID, student.getOid());
                    attCriteria.addNotEqualTo(StudentAttendance.COL_OTHER_CODE, OFF_TRACK_CODE);
                    attCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);

                    /*
                     * Set the starting date of the date range
                     */

                    if (program != null && program.getStartDate().after(startDate)) {
                        startDate = program.getStartDate();
                    }

                    // If the value being fetched is the number of days for the 2nd semester
                    // then set the startDate to the midterm date.
                    // MOY date is the first date in the second semester.
                    if (semester.equals(SEMESTER.S2) && m_middleOfYearDate.after(startDate)) {
                        startDate = m_middleOfYearDate;
                    }

                    criteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);
                    attCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, startDate);

                    /*
                     * Set the ending date of the date range
                     */

                    if (program != null && program.getEndDate() != null) {
                        endDate = program.getEndDate();
                    } else {
                        endDate = m_reportDate;
                    }

                    // If the value being fetched is the number of days for the 1st semester
                    // then set the endDate to the midterm date.
                    // MOY date is the first date in the second semester.
                    if (semester.equals(SEMESTER.S1) && m_middleOfYearDate != null
                            && !m_middleOfYearDate.after(endDate)) {
                        endDate = DateUtils.add(m_middleOfYearDate, -1);
                    }

                    if (endDate.after(m_reportDate)) {
                        endDate = m_reportDate;
                    }

                    criteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);
                    attCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, endDate);

                    /*
                     * Query student attendance
                     */

                    QueryByCriteria query = new QueryByCriteria(StudentAttendance.class, criteria);
                    int offTrackDays = getBroker().getCount(query);

                    float membership = m_enrollManager.getMembershipTotal(student,
                            getCalendarDays(m_schoolMap.get(student.getSchoolOid()), student.getCalendarCode()),
                            true,
                            startDate,
                            endDate,
                            null) - offTrackDays;

                    // If requesting attendance instead of membership, subtract out absent total.
                    if (attendance) {
                        ReportQueryByCriteria reportQuery = new ReportQueryByCriteria(StudentAttendance.class,
                                new String[] {"sum(ATT_PORTION_ABSENT)"}, attCriteria);
                        ReportQueryIterator reportIterator = getBroker().getReportQueryIteratorByQuery(reportQuery);
                        try {
                            if (reportIterator.hasNext()) {
                                Object[] results = (Object[]) reportIterator.next();
                                Number absenses = (Number) results[0];
                                if (absenses != null) {
                                    membership -= absenses.floatValue();
                                }
                            }
                        } finally {
                            reportIterator.close();
                        }
                    }

                    count = Integer.toString((int) membership);
                } catch (Exception e) {
                    addSetupError("Could not calculate membership",
                            "Student: " + student.getNameView() + ", Exception\n\t" + e.getMessage());
                }
            }
        }
        return count;
    }

    /**
     * Returns a list of student program participation, ordered newest to oldest.
     *
     * @param studentOid String
     * @return LinkedList&lt;StudentProgramParticipation&gt;
     */
    protected List<StudentProgramParticipation> getProgram(String studentOid) {
        List<StudentProgramParticipation> programs = m_existingPrograms.get(studentOid);

        if (programs == null) {
            programs = new ArrayList<StudentProgramParticipation>();
        }

        return programs;
    }

    /**
     * Returns the days-in-session for the given school and calendar combination.
     * If a schedule does not yet exist, null is returned.
     *
     * @param school SisSchool
     * @param calendar String
     * @return Set of PlainDate objects
     */
    private Set<PlainDate> getCalendarDays(SisSchool school, String calendar) {
        Set<PlainDate> calendarDays = null;

        if (!m_schoolsToCalendars.containsKey(school.getOid())) {
            // If a schedule has been defined...
            if (m_scheduleMap.get(school.getOid()) != null) {
                PlainDate startDate = getSchoolScheduleStartDate(school.getOid());
                Map calendarData = m_enrollManager.getCalendarLookup(school, startDate, m_reportDate);
                m_schoolsToCalendars.put(school.getOid(), calendarData);
            } else {
                m_schoolsToCalendars.put(school.getOid(), new HashMap<String, PlainDate>());
            }
        }

        calendarDays = (Set) ((Map) m_schoolsToCalendars.get(school.getOid())).get(calendar);

        return calendarDays;
    }

    /**
     * Build Field definition for the student name view.
     *
     * @return a FieldDefinition
     */
    private FieldDefinition getName() {
        FieldDefinition field = new FieldDefinition(STUDENT_NAME, SisStudent.COL_NAME_VIEW,
                null, false, 1, 32, null,
                null, null, null, null);

        return field;
    }

    /**
     * Get the School Schedule Start Date.
     *
     * @param schoolOid String
     * @return PlainDate
     */
    private PlainDate getSchoolScheduleStartDate(String schoolOid) {
        PlainDate startDate = null;

        if (m_scheduleMap.containsKey(schoolOid)) {
            startDate = m_scheduleMap.get(schoolOid).getStartDate();
        }

        return startDate;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        // System Parameters
        m_dateFormat = new SimpleDateFormat(DATE_FORMAT);
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_endOfYearDate = getOrganization().getCurrentContext().getEndDate();
        m_enrollManager = new EnrollmentManager(getBroker(), getPrivilegeSet(), getOrganization());
        m_firstDayMembers = m_enrollManager.getMembershipAsOf(m_schoolYearStartDate, getOrganization());
        m_lastInSessionDateMap = new HashMap<String, HashMap<String, PlainDate>>();

        // Load Parameters
        m_calculateTotals = false;
        if (getParameter(CALCULATE_TOTALS_PARAM) != null) {
            m_calculateTotals = ((Boolean) getParameter(CALCULATE_TOTALS_PARAM)).booleanValue();
        }
        m_includeInactiveStudents = false;
        if (getParameter(INCLUDE_INACTIVE_STUDENTS_PARAM) != null) {
            m_includeInactiveStudents = ((Boolean) getParameter(INCLUDE_INACTIVE_STUDENTS_PARAM)).booleanValue();
        }
        m_includeStudentNames = false;
        if (getParameter(INCLUDE_STUDENT_NAMES_PARAM) != null) {
            m_includeStudentNames = ((Boolean) getParameter(INCLUDE_STUDENT_NAMES_PARAM)).booleanValue();
        }
        if (getParameter(REPORT_DATE_PARAM) != null) {
            m_reportDate = (PlainDate) getParameter(REPORT_DATE_PARAM);
        }
        if (getParameter(REPORT_TYPE_PARAM) != null) {
            m_reportType = (Integer) getParameter(REPORT_TYPE_PARAM);
        }
        m_requireMemberDay = false;
        if (getParameter(REQUIRE_MEMBER_DAY_PARAM) != null) {
            m_requireMemberDay = ((Boolean) getParameter(REQUIRE_MEMBER_DAY_PARAM)).booleanValue();
        }
        if (getParameter(SUMMER_START_DATE_PARAM) != null) {
            m_summerStartDate = (PlainDate) getParameter(SUMMER_START_DATE_PARAM);
        }
        if (getParameter(SUMMER_END_DATE_PARAM) != null) {
            m_summerEndDate = (PlainDate) getParameter(SUMMER_END_DATE_PARAM);
        }
        m_updateRecords = false;
        if (getParameter(UPDATE_RECORDS_PARAM) != null) {
            m_updateRecords = ((Boolean) getParameter(UPDATE_RECORDS_PARAM)).booleanValue();
        }
        if (getParameter(MOY_DATE_PARAM) != null) {
            m_middleOfYearDate = (PlainDate) getParameter(MOY_DATE_PARAM);
        }

        /*
         * Set up converters, formatters, reference lookup tables, and other database-intense
         * operations. We do this once outside the student loop to improve performance.
         */
        m_integerConverter =
                ConverterFactory.getConverterForClass(Converter.INTEGER_CONVERTER, Locale.getDefault(), true);

        // Load Alias database field Names
        m_orgDistrictNumberField = translateAliasToJavaName(ALIAS_I4SEE_040_DISTRICT_NUMBER, true);
        m_orgSauNumberField = translateAliasToJavaName(ALIAS_I4SEE_030_SAU_NUMBER, true);
        m_progCateContextIdField = translateAliasToJavaName(ALIAS_I4SEE_CATE_CONTEXT_ID, true);
        m_progCateEntryCodeField = translateAliasToJavaName(ALIAS_I4SEE_240_CATE_ENTRY_CODE, true);
        m_progCateEntryDateField = translateAliasToJavaName(ALIAS_I4SEE_230_CATE_ENTRY_DATE, true);
        m_progCateExitCodeField = translateAliasToJavaName(ALIAS_I4SEE_260_CATE_EXIT_CODE, true);
        m_progCateExitDateField = translateAliasToJavaName(ALIAS_I4SEE_250_CATE_EXIT_DATE, true);
        m_schoolExcludeField = translateAliasToJavaName(ALIAS_SCHOOL_EXCLUDE, true);
        m_schoolIdField = translateAliasToJavaName(ALIAS_I4SEE_050_SCHOOL_ID, true);
        m_studentAdjustedSchoolCodeField = translateAliasToJavaName(ALIAS_ADJUSTED_SCHOOL_NUMBER, true);
        m_studentDiplomaTypeField = translateAliasToJavaName(ALIAS_I4SEE_620_DIPLOMA_TYPE, true);
        m_studentExcludeField = translateAliasToJavaName(ALIAS_STUDENT_EXCLUDE, true);
        m_studentHomelessCodeField = translateAliasToJavaName(ALIAS_I4SEE_500_HOMELESS_CD, true);
        m_studentPostGradField = translateAliasToJavaName(ALIAS_I4SEE_610_PORT_GRAD, true);
        m_studentPromotedIndField = translateAliasToJavaName(ALIAS_I4SEE_510_PROMOTED_IND, true);
        m_studentReportStatusField = translateAliasToJavaName(ALIAS_I4SEE_STATUS, true);
        m_studentCateStatusField = translateAliasToJavaName(ALIAS_I4SEE_CATE_STATUS, true);
        m_studentResidentialHomeField = translateAliasToJavaName(ALIAS_I4SEE_580_RESIDENTIAL_HOME, true);
        m_studentDaysInAttendanceS1 = translateAliasToJavaName(ALIAS_I4SEE_1764_ATTENDANCE_S1, true);
        m_studentDaysInAttendanceS2 = translateAliasToJavaName(ALIAS_I4SEE_1765_ATTENDANCE_S2, true);
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadActiveSchedules() {
        Collection<SisSchool> schools = m_schoolMap.values();

        for (SisSchool school : schools) {
            m_scheduleMap.put(school.getOid(), school.getActiveSchedule());
        }
    }

    /**
     * loads existing Student Program Participation records for the CATE program.
     */
    private void loadCATEStudentProgramParticipation() {
        String contextId = getOrganization().getCurrentContext().getContextId();

        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_CATE_PROGRAM_CODE);
        criteria.addEqualTo(m_progCateContextIdField, contextId);
        QueryByCriteria query = new QueryByCriteria(StudentProgramParticipation.class, criteria);

        m_existingPrograms = getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID,
                getBroker().getCount(query));
    }

    /**
     * Loads all the school beans in the district.
     */
    private void loadSchools() {
        X2Criteria schoolCriteria = new X2Criteria();
        QueryByCriteria schoolQuery = new QueryByCriteria(SisSchool.class, schoolCriteria);

        m_schoolMap = getBroker().getMapByQuery(schoolQuery, X2BaseBean.COL_OID, getBroker().getCount(schoolQuery));
    }

}
