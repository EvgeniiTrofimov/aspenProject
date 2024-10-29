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
package com.x2dev.procedures.statereporting.ct;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.procedures.statereporting.ct.CTStudentHelper.StudentSasidRecord;
import com.x2dev.sis.model.beans.HealthImmunizationDose;
import com.x2dev.sis.model.beans.HealthImmunizationSeries;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Connecticut state report for SASID export.
 * This class implements the data export for CT SASID export.
 *
 * @author X2 Development Corporation
 */
public class CT18SasidRegister extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the SASID export.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class SasidRegisterEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        CT18SasidRegister m_registerData = null;
        List<StudentSasidRecord> m_sasidRecords = new ArrayList<StudentSasidRecord>();

        /**
         * Instantiates a new sasid register entity.
         */
        /*
         * Public no argument constructor for dynamic instantiation.
         */
        public SasidRegisterEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Return current record.
         *
         * @return
         */
        public StudentSasidRecord getCurrentRecord() {
            return m_sasidRecords.get(getCurrentRow());
        }

        /**
         * Return previous record if exists.
         *
         * @return
         */
        public StudentSasidRecord getPreviousRecord() {
            StudentSasidRecord previousRecord = null;
            int currentRow = getCurrentRow();
            if (currentRow > 0) {
                previousRecord = m_sasidRecords.get(currentRow - 1);
            }
            return previousRecord;
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
            m_registerData = (CT18SasidRegister) data;
            SisStudent student = (SisStudent) bean;
            List<StudentSasidRecord> candidateRecords =
                    m_registerData.m_ctHelper.getStudentInfo(student).getSasidRecords();
            for (StudentSasidRecord candidate : candidateRecords) {
                boolean addToExport = true;
                PlainDate dateToCheck = null;
                if (candidate.getEntryDate() != null) {
                    dateToCheck = candidate.getEntryDate();
                } else if (candidate.getExitDate() != null) {
                    dateToCheck = candidate.getExitDate();
                }
                addToExport = dateToCheck != null && dateToCheck.after(m_registerData.m_priorReportDate)
                        && !dateToCheck.after(m_registerData.m_reportDate);
                if (addToExport && m_registerData.m_requireMemberDay.booleanValue()) {
                    addToExport = candidate.getMembershipDays() != null && candidate.getMembershipDays().size() > 0;
                }
                if (addToExport && m_registerData.m_recordTypeParam != null) {
                    addToExport = m_registerData.m_recordTypeParam.equalsIgnoreCase(candidate.getRecordType());
                }
                if (addToExport) {
                    m_sasidRecords.add(candidate);
                }
            }
            setRowCount(m_sasidRecords.size());
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
     * This retriever will allow us to filter values based on the type of record we generated.
     * We pass in the allowed types for a field.
     */
    protected class FilterByType implements FieldRetriever {
        public static final String CALC_ID = "SASID-FILTER";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object value = null;
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            String type = record.getRecordType();
            String param = (String) field.getParameter();
            if (param.indexOf(type) > -1) {
                value = getProperty(entity.getBean(), field.getBeanPath());
            }
            return value;
        }
    }

    /**
     * Retrieve the exit type.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitType implements FieldRetriever {
        public static final String CALC_ID = "SASID-EXIT-TYPE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = null;
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            if (record.getRecordType().equals(CTStudentHelper.SASID_TYPE_C)) {
                StudentSasidRecord previousRecord = sasidEntity.getPreviousRecord();
                if (previousRecord != null
                        && !Objects.equals(record.getFacilityCode1(), previousRecord.getFacilityCode1())) {
                    value = record.getExitType();
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the exit status.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveExitStatus implements FieldRetriever {
        public static final String CALC_ID = "SASID-EXIT-STATUS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((SasidRegisterEntity) entity).getCurrentRecord().getExitStatus();
        }
    }

    /**
     * Retrieve the school Id from the membership record.
     * This is the school for the Student Enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFacilityCodes implements FieldRetriever {
        public static final String CALC_ID = "SASID-FACILITY";
        public static final String CALC_PARAM_CODE_1 = "CODE_1";
        public static final String CALC_PARAM_CODE_2 = "CODE_2";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String facilityCodeState = null;
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            if (!record.getRecordType().equals(CTStudentHelper.SASID_TYPE_U)) {
                if (CALC_PARAM_CODE_1.equals(field.getParameter())) {
                    facilityCodeState = record.getFacilityCode1();
                } else if (CALC_PARAM_CODE_2.equals(field.getParameter())) {
                    if (!StringUtils.isEmpty(record.getFacilityCode1())
                            && !StringUtils.isEmpty(record.getFacilityCode2())
                            && !record.getFacilityCode1().equals(record.getFacilityCode2())) {
                        facilityCodeState = record.getFacilityCode2();
                    }
                }
            }
            return facilityCodeState;
        }
    }

    /**
     * For other record type this value is based on the YOG in the enrollment record corresponding
     * to this export record.
     * If the YOG in the enrollment record matches the YOG for the student,
     * use the state reference code from the grade level of the student. If the YOG is different,
     * calculate the value base on the YOG in the enrollment record and find the appropriate state
     * reference code value.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveGradeLevel implements FieldRetriever {
        public static final String CALC_ID = "SASID-GRADE";

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
            return (((SasidRegisterEntity) entity).getCurrentRecord()).getGradeLevel();
        }
    }

    /**
     * Retrieve the hispanic indicator from the person record.
     * Only return for Register or Change records.
     * Blank for Unregister records.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveHispanicOrLatinoIndicator implements FieldRetriever {
        public static final String CALC_ID = "SASID-HISPANIC-IND";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            SisStudent student = (SisStudent) entity.getBean();
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            String indicator = null;
            if (!CTStudentHelper.SASID_TYPE_U.equals(record.getRecordType())) {
                indicator = student.getPerson().getHispanicLatinoIndicator() ? STRING_Y : STRING_N;
            }
            return indicator;
        }
    }

    /**
     * Retrieve the district enrollment date.
     *
     * This is the date at which the student first entered a school and began membership in
     * the district in the format MMDDYYYY where "M" = Month, "D" = Day, "Y" = Year. If a
     * student has re-entered your district, it will reflect the date of re-entry. In either
     * case the date must be after their membership in a prior district ends. This is a
     * mandatory field in the Register module and disallowed in the rest of the collections.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembershipDate implements FieldRetriever {
        public static final String CALC_ID = "SASID-DATE";
        public static final String CALC_PARAM_ENTRY = "ENTRY";
        public static final String CALC_PARAM_ENTRY_CHANGE = "ENTRY_CHANGE";
        public static final String CALC_PARAM_EXIT = "EXIT";
        public static final String CALC_PARAM_EXIT_CHANGE = "EXIT_CHANGE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            Object membershipDate = null;
            String param = (String) field.getParameter();
            if (field.getFormatter() != null) {
                if ((CALC_PARAM_ENTRY.equals(param) && record.getRecordType().equals(CTStudentHelper.SASID_TYPE_R))) {
                    membershipDate = record.getEntryDate();
                } else if (CALC_PARAM_EXIT.equals(param)
                        && record.getRecordType().equals(CTStudentHelper.SASID_TYPE_U)) {
                    membershipDate = record.getExitDate();
                } else if (CALC_PARAM_ENTRY_CHANGE.equals(param)
                        && (record.getRecordType().equals(CTStudentHelper.SASID_TYPE_R)
                                || record.getRecordType().equals(CTStudentHelper.SASID_TYPE_C))) {
                    membershipDate = record.getEntryDate();
                } else if (CALC_PARAM_EXIT_CHANGE.equals(param)
                        && (record.getRecordType().equals(CTStudentHelper.SASID_TYPE_U)
                                || record.getRecordType().equals(CTStudentHelper.SASID_TYPE_C))) {
                    membershipDate = record.getExitDate();
                }
            }
            return membershipDate;
        }
    }

    /**
     * Retrieve membership days for a student.
     * This retrieves the MembershipAttendance for the current segment and applies to one school.
     *
     * This returns one of three values based on the Field parameter (Integer):
     * 1 = Days in attendance (student membership - student absences)
     * 2 = Days absent
     * 3 = Days not in membership (school membership days - student membership days)
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveMembershipDays implements FieldRetriever {
        public static final String CALC_ID = "SASID-MEMBER-DAYS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            String membershipCountAsString = null;
            if (!record.getRecordType().equals(CTStudentHelper.SASID_TYPE_R)) {
                int attendCount = 0;
                if (param != null && param.equals("PRESENT")) {
                    // Get days in attendance count. This is membership minus absences.
                    List<PlainDate> stdMemb = record.getMembershipDays();
                    BigDecimal stdAbsCount = record.getAbsencesCount();
                    if (stdMemb != null && stdAbsCount != null) {
                        attendCount = stdMemb.size() - stdAbsCount.intValue();
                        if (attendCount < 0.0) {
                            entity.addRetrievalError(field.getFieldId(),
                                    new StateReportValidationError(entity, field,
                                            "Attendance less than zero",
                                            "Attendance=" + STYLE_BOLD + Double.toString(attendCount) + STYLE_END));
                            attendCount = 0;
                        }
                    }
                } else if (param != null && param.equals("MEMBERSHIP")) {
                    // Get all membership days in this school for this student.
                    List<PlainDate> stdMemb = record.getMembershipDays();
                    if (stdMemb != null) {
                        attendCount = stdMemb.size();
                    }
                }
                membershipCountAsString = Integer.toString(attendCount);
            }
            return membershipCountAsString;
        }
    }

    /***
     * For Greenwich, nexus district is a student in SPED or a value is entered in a nexus field.
     * For others, use the student sped status code to determine active.
     *
     * If there is not value in the nexus field for a SPED value, use the district code.
     */
    protected class RetrieveNexus implements FieldRetriever {
        public static final String CALC_ID = "SASID-NEXUS";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String nexus = null;
            String nexusExitDate = entity.getFieldValue(EXPORT_FIELD_NEXUS_EXIT_DATE);
            if (StringUtils.isEmpty(nexusExitDate)) {
                String defaultNexusDistrict = entity.getFieldValue(EXPORT_FIELD_DISTRICT);
                SisStudent student = (SisStudent) entity.getBean();
                String sped = (String) student.getFieldValueByBeanPath(m_fieldStdPsis16);
                if (!StringUtils.isEmpty(sped)) {
                    String spedState = lookupStateValue(SisStudent.class, m_fieldStdPsis16, sped);
                    if (STRING_Y.equalsIgnoreCase(spedState)) {
                        nexus = defaultNexusDistrict;
                    }
                }
                String nexusFieldValue = (String) student.getFieldValueByBeanPath(m_fieldStdNexus);
                if (!StringUtils.isEmpty(nexusFieldValue)) {
                    String stateNexusCode = lookupStateValue(SisStudent.class, m_fieldStdNexus, nexusFieldValue);
                    if (!StringUtils.isEmpty(stateNexusCode)) {
                        nexus = stateNexusCode;
                    }
                }
            }
            return nexus;
        }
    }

    /***
     * For Greenwich, nexus district is a student in SPED or a value is entered in a nexus field.
     * If there is not value in the nexus field for a SPED value, use the district code.
     * The parameter passed in is the default nexus code for students that do not have one set.
     */
    protected class RetrieveNexusDate implements FieldRetriever {
        public static final String CALC_ID = "SASID-NEXUS-DATE";
        public static final String CALC_PARAM_ENTRY = "NEXUS_ENTRY";
        public static final String CALC_PARAM_EXIT = "NEXUS_EXIT";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PlainDate nexusDate = null;
            SisStudent student = (SisStudent) entity.getBean();
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            String param = (String) field.getParameter();
            nexusDate = (PlainDate) getProperty(student, field.getBeanPath());
            if (nexusDate == null) {
                if (record.getRecordType().equals(CTStudentHelper.SASID_TYPE_R) && CALC_PARAM_ENTRY.equals(param)) {
                    nexusDate = record.getEntryDate();
                } else if (record.getRecordType().equals(CTStudentHelper.SASID_TYPE_U)
                        && CALC_PARAM_EXIT.equals(param)) {
                    nexusDate = record.getExitDate();
                }
            }
            return nexusDate;
        }
    }


    /**
     * Retrieve the polio vaccination date.
     *
     * Date of student?s first polio vaccination in the format MMDDYYYY
     * where "M" = Month, "D" = Day, "Y" = Year. This is a tie-breaker element.
     * This field is optional for register and disallowed for change and unregister.
     *
     * @author X2 Development Corporation
     */
    protected class RetrievePolioDate implements FieldRetriever {
        public static final String CALC_ID = "SASID-POLIO-DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            String studentOid = entity.getBean().getOid();
            Object polioDate = null;
            if (record.getRecordType().equals(CTStudentHelper.SASID_TYPE_R)) {
                if (m_studentPolioImmunization.containsKey(studentOid)) {
                    Collection<HealthImmunizationDose> immunizations = m_studentPolioImmunization.get(studentOid);
                    for (HealthImmunizationDose immunization : immunizations) {
                        polioDate = immunization.getDate();
                        break;
                    }
                }
            }
            return polioDate;
        }
    }

    /**
     * Returns an indicator if the person is included in the specified race.
     * This implementation looks up all race codes for a person and finds
     * the race entry for the specified race. It returns a true/false value
     * indicating the presence of the race code record.
     *
     * The calculation parameter should be a string with three characters:
     * character 1:
     * The character to return if the requested race code is present.
     * character 2:
     * The character to return if the requested race code is not present.
     * character(s) 3+:
     * The reference code state code value in the reference table for race codes.
     * In CT, these are arbitraily defined for this report and NOT defined
     * by the CT DOE, this is (based on values from MA):
     * "1" - Caucasian
     * "2" - African
     * "4" - Asian
     * "8" - Native/Alaskan
     * "16" - Pacific
     *
     * Ex: "YN4" searches for the Asian code, returns "Y" if present, "N" otherwise.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {
        public static final String CALC_ID = "SASID-RACE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            // do not report race if record type = U
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            String raceCode = null;
            if (!CTStudentHelper.SASID_TYPE_U.equals(record.getRecordType())) {
                String param = (String) field.getParameter();
                if (!StringUtils.isEmpty(param) && param.length() > 2) {
                    String trueChar = param.substring(0, 1);
                    String falseChar = param.substring(1, 2);
                    String requestCode = param.substring(2);
                    SisStudent student = (SisStudent) entity.getBean();
                    raceCode = falseChar;
                    Collection<Race> races = m_ctHelper.getRaces(student);
                    if (!StringUtils.isEmpty(requestCode) && races != null) {
                        for (Race race : races) {
                            if (requestCode.equals(
                                    data.lookupStateValue(Race.class, Race.COL_RACE_CODE, race.getRaceCode()))) {
                                raceCode = trueChar;
                                break;
                            }
                        }
                    }
                }
            }
            return raceCode;
        }
    }


    /**
     * The Class RetrieveResidentTown.
     */
    protected class RetrieveResidentTown implements FieldRetriever {
        public static final String CALC_ID = "SASID-RES-TOWN";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            String defaultValue = entity.getFieldValue(EXPORT_FIELD_DISTRICT);
            SisStudent std = (SisStudent) entity.getBean();
            String residentTownStateCode = null;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            if (!record.getRecordType().equals(CTStudentHelper.SASID_TYPE_U)) {
                String resTown = (String) std.getFieldValueByBeanPath(m_fieldStdResidentTown);
                if (!StringUtils.isEmpty(resTown)) {
                    residentTownStateCode = lookupStateValue(SisStudent.class, m_fieldStdResidentTown, resTown);
                }
                if (StringUtils.isEmpty(residentTownStateCode)) {
                    residentTownStateCode = defaultValue;
                }
            }
            return residentTownStateCode;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        public static final String CALC_ID = "SASID-CLEAN";

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
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            if (!CTStudentHelper.SASID_TYPE_U.equals(record.getRecordType())) {
                value = (String) getProperty(entity.getBean(), field.getBeanPath());
                if (value != null) {
                    Matcher matcher = m_illegalNameCharacters.matcher(value);
                    value = matcher.replaceAll("");
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the record type.
     *
     * This returns one of the three values:
     * R = Register
     * C = Change
     * U = Unregister
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveType implements FieldRetriever {
        public static final String CALC_ID = "SASID-REC-TYPE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            SasidRegisterEntity sasidEntity = (SasidRegisterEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            return record.getRecordType();
        }
    }

    /**
     * Aliases
     */
    private static final String ALIAS_STD_NEXUS = "NEXUS";
    private static final String ALIAS_STD_PSIS16 = "PSIS16";
    private static final String ALIAS_STD_RES_TOWN = "PSIS04";

    /**
     * Input parameters.
     */
    private static final String INPUT_PARAM_INCLUDE_HEADING = "includeHeading";
    private static final String INPUT_PARAM_PRIOR_REPORT_DATE = "priorReportDate";
    private static final String INPUT_PARAM_REPORT_DATE = "reportDate";
    private static final String INPUT_PARAM_REQUIRE_MEMBER_DAY = "requireMemberDay";
    private static final String INPUT_PARAM_RECORD_TYPE = "recordType";
    private static final String INPUT_PARAM_SASID_SELECTION = "sasidSelection";

    /**
     * Other internal constants
     */
    private static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    private static final String EXPORT_FIELD_DISTRICT = "REPORTING_DISTRICT";
    private static final String EXPORT_FIELD_NEXUS_EXIT_DATE = "NEXUS_EXIT_DATE";
    private static final String POLIO_SERIES_DEFINITION_OID = "himX2IPV";
    private static final String STRING_Y = "Y";
    private static final String STRING_N = "N";

    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected CTStudentHelper m_ctHelper;
    protected String m_fieldStdNexus;
    protected String m_fieldStdPsis16;
    protected String m_fieldStdResidentTown;
    protected Pattern m_illegalNameCharacters;
    protected PlainDate m_priorReportDate;
    protected String m_recordTypeParam;
    protected PlainDate m_reportDate;
    protected Boolean m_requireMemberDay;
    protected Map<String, Collection<HealthImmunizationDose>> m_studentPolioImmunization;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        String heading = "";
        Boolean includeHeading = (Boolean) getParameter(INPUT_PARAM_INCLUDE_HEADING);
        if (includeHeading != null && includeHeading.booleanValue()) {
            heading = super.getHeading();
        }
        return heading;
    }

    /**
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @throws X2BaseException
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();
        /*
         * Get core parameters
         */
        m_priorReportDate = (PlainDate) getParameter(INPUT_PARAM_PRIOR_REPORT_DATE);
        m_reportDate = (PlainDate) getParameter(INPUT_PARAM_REPORT_DATE);
        m_requireMemberDay = (Boolean) getParameter(INPUT_PARAM_REQUIRE_MEMBER_DAY);
        m_recordTypeParam = (String) getParameter(INPUT_PARAM_RECORD_TYPE);
        // all records
        if (m_recordTypeParam.equalsIgnoreCase("A")) {
            m_recordTypeParam = null;
        }
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);

        m_ctHelper = new CTStudentHelper(this, m_reportDate);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            /*
             * Build query object that will be used to retrieve export students.
             */
            X2Criteria userCriteria = new X2Criteria();
            // Option for empty, non-empty or all sasid.
            Integer sasidSelection = (Integer) getParameter(INPUT_PARAM_SASID_SELECTION);
            if (sasidSelection != null) {
                if (sasidSelection.intValue() == 1) {
                    userCriteria.addEmpty(Student.COL_STATE_ID, getBroker().getPersistenceKey());
                } else if (sasidSelection.intValue() == 2) {
                    userCriteria.addNotEmpty(Student.COL_STATE_ID, getBroker().getPersistenceKey());
                }
            }
            m_ctHelper.getStudentCriteria().addAndCriteria(userCriteria);
            QueryByCriteria studentQuery = m_ctHelper.getStudentQuery(true);

            // Set the query to be used for student selection.
            setQuery(studentQuery);
            setEntityClass(SasidRegisterEntity.class);

            int count = getBroker().getCount(studentQuery);
            // Load Polio immunization map.
            loadPolioImmunizations(m_ctHelper.getStudentCriteria(), count);
            // Build maps of retriever functions and validator functions
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveMembershipDate.CALC_ID, new RetrieveMembershipDate());
            calcs.put(RetrieveExitStatus.CALC_ID, new RetrieveExitStatus());
            calcs.put(RetrieveExitType.CALC_ID, new RetrieveExitType());
            calcs.put(RetrieveFacilityCodes.CALC_ID, new RetrieveFacilityCodes());
            calcs.put(FilterByType.CALC_ID, new FilterByType());
            calcs.put(RetrieveHispanicOrLatinoIndicator.CALC_ID, new RetrieveHispanicOrLatinoIndicator());
            calcs.put(RetrieveMembershipDays.CALC_ID, new RetrieveMembershipDays());
            calcs.put(RetrieveNexus.CALC_ID, new RetrieveNexus());
            calcs.put(RetrieveNexusDate.CALC_ID, new RetrieveNexusDate());
            calcs.put(RetrievePolioDate.CALC_ID, new RetrievePolioDate());
            calcs.put(RetrieveRace.CALC_ID, new RetrieveRace());
            calcs.put(RetrieveType.CALC_ID, new RetrieveType());
            calcs.put(RetrieveResidentTown.CALC_ID, new RetrieveResidentTown());
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            HashMap validators = new HashMap<String, FieldValidator>();
            super.addCalcs(calcs);
            super.addValidators(validators);
        }
    }

    /**
     * Init fields' bean paths
     */
    private void initializeFields() {
        m_fieldStdResidentTown = translateAliasToJavaName(ALIAS_STD_RES_TOWN, true);
        m_fieldStdNexus = translateAliasToJavaName(ALIAS_STD_NEXUS, true);
        m_fieldStdPsis16 = translateAliasToJavaName(ALIAS_STD_PSIS16, true);
    }

    /**
     * Load student immunizations into a map by student Oid.
     *
     * @param studentCriteria Criteria
     * @param count int
     */
    private void loadPolioImmunizations(Criteria studentCriteria, int count) {
        /*
         * Load student polio immunization.
         */
        SubQuery subQuery = new SubQuery(Student.class, X2BaseBean.COL_OID, studentCriteria);
        Criteria polioCriteria = new Criteria();
        polioCriteria.addEqualTo(HealthImmunizationDose.REL_IMMUNIZATION_SERIES + PATH_DELIMITER +
                HealthImmunizationSeries.COL_IMMUNIZATION_DEFINITION_OID, POLIO_SERIES_DEFINITION_OID);
        polioCriteria.addIn(HealthImmunizationDose.COL_STUDENT_OID, subQuery);
        QueryByCriteria polioQuery = new QueryByCriteria(HealthImmunizationDose.class, polioCriteria);
        polioQuery.addOrderByAscending(HealthImmunizationDose.COL_DATE);
        m_studentPolioImmunization =
                getBroker().getGroupedCollectionByQuery(polioQuery, HealthImmunizationDose.COL_STUDENT_OID, count);
    }
}
