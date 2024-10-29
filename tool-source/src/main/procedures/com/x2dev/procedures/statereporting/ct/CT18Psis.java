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

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.procedures.statereporting.ct.CTStudentHelper.StudentSasidRecord;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentAttendance;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.converters.Converter;
import com.x2dev.utils.converters.ConverterFactory;
import com.x2dev.utils.converters.SystemStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Connecticut PSIS State Report
 * The PSIS state report is run 3 times a year, October, January, and June (end of school).
 * The report captures students that are active on the date the report is run.
 *
 *
 * @author X2 Development Corporation
 */
public class CT18Psis extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the PSIS Export.
     *
     * @author X2 Development Corporation
     */
    public static class PsisEntity extends StateReportEntity {
        /*
         * Cached values for retrievers to share.
         */
        BigDecimal m_totalAbsencesCount = BigDecimal.ZERO;
        Set<PlainDate> m_totalMembershipDays = new HashSet();
        CT18Psis m_psisData = null;
        StudentSasidRecord m_record;

        /**
         * Instantiates a new psis entity.
         */
        public PsisEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the absences count.
         *
         * @return the absences count
         */
        public BigDecimal getAbsencesCount() {
            return m_totalAbsencesCount;
        }

        /**
         * Gets the current record.
         *
         * @return Student sasid record
         */
        public StudentSasidRecord getCurrentRecord() {
            return m_record;
        }

        /**
         * Returns the display name of the represented entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            StringBuilder buffer = new StringBuilder();
            SisStudent student = (SisStudent) getBean();
            buffer.append(student.getNameView());
            buffer.append(" [");
            buffer.append(student.getLocalId());
            buffer.append("]");
            return buffer.toString();
        }

        /**
         * @return
         */
        public Set<PlainDate> getMembershipDays() {
            return m_totalMembershipDays;
        }

        /**
         * Initialize the entity.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#intitialize(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.beans.X2BaseBean)
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            SisStudent student = (SisStudent) bean;
            m_psisData = (CT18Psis) data;
            List<StudentSasidRecord> candidateRecords =
                    m_psisData.m_ctHelper.getStudentInfo(student).getSasidRecords();
            for (StudentSasidRecord candidateRecord : candidateRecords) {
                if (candidateRecord.getRecordType().equals(CTStudentHelper.SASID_TYPE_U) &&
                        candidateRecord.getExitDate() != null &&
                        candidateRecord.getExitDate().before(m_psisData.m_reportDate)) {
                    m_record = null;
                } else if (!StringUtils.isEmpty(candidateRecord.getFacilityCode1()) &&
                        candidateRecord.getEntryDate() != null &&
                        !candidateRecord.getEntryDate().after(m_psisData.m_reportDate)) {
                    m_record = candidateRecord;
                }
            }
            if (m_record != null && m_record.getExitDate() != null
                    && m_record.getExitDate().before(m_psisData.m_reportDate) &&
                    (m_record.getEntryDate() == null || m_record.getEntryDate().after(m_psisData.m_reportDate))) {
                m_record = null;
            }
            if (m_record == null || (data.isSchoolContext() && m_record.getSchool() != null
                    && !data.getSchool().getOid().equals(m_record.getSchool().getOid()))) {
                setRowCount(0);
            } else {
                String facilityCode = m_record.getFacilityCode1();

                for (StudentSasidRecord candidateRecord : candidateRecords) {
                    if (facilityCode.equals(candidateRecord.getFacilityCode1())) {
                        m_totalAbsencesCount = m_totalAbsencesCount.add(candidateRecord.getAbsencesCount());
                        m_totalMembershipDays.addAll(candidateRecord.getMembershipDays());
                    }
                    if (candidateRecord == m_record) {
                        break;
                    }
                }
            }
        }

        /**
         * If update calculated fields is set, save new values into the bean.
         *
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#postProcess()
         */
        @Override
        public void postProcess() {
            Boolean updateRecords = (Boolean) getData().getParameter(PARAM_UPDATE_RECORDS);

            /*
             * If the update flag is set, update calculated values into the student records.
             */
            if (updateRecords != null ? updateRecords.booleanValue() : false) {
                try {
                    setIntegerField(EXPORT_FIELD_PSIS_29_MEMBERSHIP_DAYS);
                    setIntegerField(EXPORT_FIELD_PSIS_30_MEMBERSHIP_DAYS);

                    if (getBean().isDirty()) {
                        getData().getBroker().saveBeanForced(getBean());
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
         * Sets the integer field.
         *
         * @param fieldName void
         * @throws IllegalAccessException exception
         * @throws InvocationTargetException exception
         * @throws NoSuchMethodException exception
         */
        private void setIntegerField(String fieldName)
                throws IllegalAccessException, InvocationTargetException, NoSuchMethodException {
            FieldDefinition field = getData().getFieldDefinition(fieldName);
            if (field != null && field.getBeanPath().charAt(0) != StateReportData.LABEL_PREFIX_CHAR) {
                ModelProperty prop = new ModelProperty(getBean().getClass(), field.getBeanPath(),
                        getData().getBroker().getPersistenceKey());
                DataDictionaryField dictionaryField =
                        getData().getDataDictionary().findDataDictionaryField(prop.getFieldId());
                if (dictionaryField != null) {
                    String stringValue = getFieldValue(fieldName);
                    if (!StringUtils.isEmpty(stringValue)) {
                        String format = WebUtils.generateFormat(dictionaryField,
                                LocalizationCache.getPrimarySystemLocale(getData().getBroker().getPersistenceKey()));
                        Converter baseConverter = ConverterFactory.getConverterForClass(
                                dictionaryField.getEffectiveJavaType(),
                                LocalizationCache.getPrimarySystemLocale(getData().getBroker().getPersistenceKey()),
                                dictionaryField.isString(), format);
                        Converter javaTypeConverter = ConverterFactory.getConverterForClass(
                                dictionaryField.getEffectiveJavaType(),
                                LocalizationCache.getPrimarySystemLocale(getData().getBroker().getPersistenceKey()),
                                false, format);
                        if (baseConverter instanceof SystemStringConverter) {
                            SystemStringConverter converter = ((SystemStringConverter) baseConverter);
                            if (converter != null) {
                                Object value = javaTypeConverter.stringToJava(stringValue);
                                stringValue = converter.getSystemString(value);
                            }
                        }
                        if (stringValue != null) {
                            PropertyUtils.setProperty(getBean(), field.getBeanPath(), stringValue);
                        }
                    }
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
     * Retriever for a student's days in attendance. Assumes values are already loaded into the
     * absence map.
     */
    protected class RetrieveAttendanceDays implements FieldRetriever {
        private static final String CALC_ID = "PSIS-ATTENDANCE-DAYS";

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
            PsisEntity psisEntity = (PsisEntity) entity;
            StudentSasidRecord record = psisEntity.getCurrentRecord();
            int attendanceDays = record.getAbsencesCount().intValue();
            Object attendance = null;
            if (m_includeMembershipAndAttendanceDays.booleanValue()) {
                attendance = Integer.valueOf(attendanceDays);
            } else {
                attendance = "";
            }
            return attendance;
        }
    }

    /**
     * Retrieve if student has Ell program.
     */
    protected class RetrieveEnglishLanguageLearner extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-EL";
        private static final String ELL_ALIAS = "PSIS14";
        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        protected String m_fieldStdEll;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve english language learner.
         */
        public RetrieveEnglishLanguageLearner() {
            super();

            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ELL_ALIAS);
            if (dictionaryField != null) {
                m_fieldStdEll = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_PGM_ENGLISH_LANGUAGE_LERNER);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            String value = STRING_N;
            if (m_fieldStdEll != null) {
                Object item = getPropertyAsJavaType(entity.getBean(), m_fieldStdEll);
                if (item != null) {
                    if (item instanceof Boolean) {
                        if (((Boolean) item).booleanValue()) {
                            value = STRING_Y;
                        }
                    } else if (item instanceof String) {
                        String strValue = (String) item;
                        if (m_hasRefTable) {
                            strValue = lookupStateValue(SisStudent.class, m_fieldStdEll, strValue);
                        }
                        if (YES_VALUES.contains(strValue)) {
                            value = STRING_Y;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /**
     * Retrieves ELL program state code.
     */
    protected class RetrieveEnglishLanguageLearnerProgramCode extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-EL-PROG";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_PGM_ENGLISH_LANGUAGE_LERNER);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) {
            SisStudent student = (SisStudent) entity.getBean();
            String ellPgmValue = StringUtils.isEmpty(m_fieldStdEllProgram) ? null
                    : (String) student.getFieldValueByBeanPath(m_fieldStdEllProgram);
            String value = null;
            if (!StringUtils.isEmpty(ellPgmValue)) {
                value = lookupStateValue(SisStudent.class, m_fieldStdEllProgram, ellPgmValue);
            }
            return value != null ? value : "";
        }
    }

    /**
     * Retrieve the school Id from the membership record.
     * This is the school for the Student Enrollment record.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFacilityCodes implements FieldRetriever {
        public static final String CALC_ID = "PSIS-FACILITY";
        public static final String CALC_PARAM_CODE_1 = "CODE_1";
        public static final String CALC_PARAM_CODE_2 = "CODE_2";

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
            String facilityCodeState = null;
            PsisEntity sasidEntity = (PsisEntity) entity;
            StudentSasidRecord record = sasidEntity.getCurrentRecord();
            if (CALC_PARAM_CODE_1.equals(field.getParameter())) {
                facilityCodeState = record.getFacilityCode1();
            } else if (CALC_PARAM_CODE_2.equals(field.getParameter())) {
                if (!StringUtils.isEmpty(record.getFacilityCode1())
                        && !StringUtils.isEmpty(record.getFacilityCode2())
                        && !record.getFacilityCode1().equals(record.getFacilityCode2())) {
                    facilityCodeState = record.getFacilityCode2();
                }
            }
            return facilityCodeState;
        }
    }

    /**
     * Inner class implementing a field retriever for gifted and talented.
     */
    protected class RetrieveGiftedandTalented extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-GIFTED-TALENTED";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_PGM_GIFTED_AND_TALENTED);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) {
            SisStudent student = (SisStudent) entity.getBean();
            String giftValue = StringUtils.isEmpty(m_fieldStdGifTal) ? null
                    : (String) student.getFieldValueByBeanPath(m_fieldStdGifTal);
            String value = null;
            if (!StringUtils.isEmpty(giftValue)) {
                value = lookupStateValue(SisStudent.class, m_fieldStdGifTal, giftValue);
            }
            return value != null ? value : CODE_NOT_GIFTED_OR_TALENTED;
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
        public static final String CALC_ID = "PSIS-GRADE";

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
            return (((PsisEntity) entity).getCurrentRecord()).getGradeLevel();
        }
    }

    /**
     * The Class RetrieveImmigrantStatus.
     */
    protected class RetrieveImmigrantStatus extends RetrieveProgramCode {
        private static final String ALIAS_IMMIGRANT_STATUS = "all-std-ImmigrantStatus";
        private static final String CALC_ID = "PSIS-IMMIGRANT";
        private static final String NO = "N";
        private static final String YES = "Y";

        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");

        private String m_fieldImmigrantStatus;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve immigrant status.
         */
        public RetrieveImmigrantStatus() {
            super();
            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_IMMIGRANT_STATUS);
            if (dictionaryField != null) {
                m_fieldImmigrantStatus = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_PGM_IMMIGRANT_STATUS);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            String value = NO;
            if (!StringUtils.isEmpty(m_fieldImmigrantStatus)) {
                Object item = getPropertyAsJavaType(entity.getBean(), m_fieldImmigrantStatus);
                if (item != null) {
                    if (item instanceof Boolean) {
                        if (((Boolean) item).booleanValue()) {
                            value = YES;
                        }
                    } else if (item instanceof String) {
                        String strValue = (String) item;
                        if (m_hasRefTable) {
                            strValue = lookupStateValue(SisStudent.class, m_fieldImmigrantStatus, strValue);
                        }
                        if (YES_VALUES.contains(strValue)) {
                            value = YES;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
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
        public static final String CALC_ID = "PSIS-MEMBER-DAYS";

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
            String param = (String) field.getParameter();
            PsisEntity sasidEntity = (PsisEntity) entity;
            double attendCount = 0.0;
            if ("PRESENT".equals(param)) {
                // Get days in attendance count. This is membership minus absences.
                Set<PlainDate> stdMemb = sasidEntity.getMembershipDays();
                BigDecimal stdAbsCount = sasidEntity.getAbsencesCount();
                if (stdMemb != null && stdAbsCount != null) {
                    attendCount = stdMemb.size() - stdAbsCount.doubleValue();
                    if (attendCount < 0.0) {
                        entity.addRetrievalError(field.getFieldId(),
                                new StateReportValidationError(entity, field,
                                        "Attendance less than zero",
                                        "Attendance=" + STYLE_BOLD + Double.toString(attendCount) + STYLE_END));
                        attendCount = 0;
                    }
                }
            } else if ("MEMBERSHIP".equals(param)) {
                // Get all membership days in this school for this student.
                Set<PlainDate> stdMemb = sasidEntity.getMembershipDays();
                if (stdMemb != null) {
                    attendCount = stdMemb.size();
                }
            }
            return m_includeMembershipAndAttendanceDays.booleanValue() ? BigDecimal.valueOf(attendCount) : "";
        }
    }

    /**
     * The Class RetrieveMilitaryFamily.
     */
    protected class RetrieveMilitaryFamily extends RetrieveProgramCode {
        private static final String ALIAS_MILITARY_FAMILY = "all-std-MilitaryFamily";
        private static final String CALC_ID = "PSIS-MILITARY";
        private static final String NO = "N";
        private static final String YES = "Y";
        private final List<String> YES_VALUES = Arrays.asList("Y", "YES", "01");
        private String m_fieldMilitaryFamily;
        private boolean m_hasRefTable;

        /**
         * Instantiates a new retrieve military family.
         */
        public RetrieveMilitaryFamily() {
            super();
            DataDictionaryField dictionaryField =
                    getDataDictionary().findDataDictionaryFieldByAlias(ALIAS_MILITARY_FAMILY);
            if (dictionaryField != null) {
                m_fieldMilitaryFamily = dictionaryField.getJavaName();
                m_hasRefTable = dictionaryField.hasReferenceTable();
            }
        }

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            addEqualToPgmField(StudentProgramParticipation.COL_PROGRAM_CODE, CODE_PGM_MILITARY_FAMILY);
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            String value = NO;
            if (!StringUtils.isEmpty(m_fieldMilitaryFamily)) {
                Object item = getPropertyAsJavaType(entity.getBean(), m_fieldMilitaryFamily);
                if (item != null) {
                    if (item instanceof Boolean) {
                        if (((Boolean) item).booleanValue()) {
                            value = YES;
                        }
                    } else if (item instanceof String) {
                        String strValue = (String) item;
                        if (m_hasRefTable) {
                            strValue = lookupStateValue(SisStudent.class, m_fieldMilitaryFamily, strValue);
                        }
                        if (YES_VALUES.contains(strValue)) {
                            value = YES;
                        }
                    }
                }
            }
            return value;
        }

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#isIndicator()
         */
        @Override
        public boolean isIndicator() {
            return true;
        }
    }

    /***
     * For Greenwich, nexus district is a student in SPED or a value is entered in a nexus field.
     * For others, use the student sped status code to determine active.
     *
     * If there is not value in the nexus field for a SPED value, use the district code.
     */
    protected class RetrieveNexus implements FieldRetriever {
        public static final String CALC_ID = "PSIS-NEXUS";

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
            String nexus = null;
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
            return nexus;
        }
    }

    /**
     * The Class RetrievePrekNumericValue.
     */
    protected class RetrievePrekNumericValue implements FieldRetriever {
        private static final String CAL_ID = "PSIS-PREK-NUMERIC";
        private static final String FIELD_GRADE_CODE = "GRADE_CODE";
        private final List<String> REPORTABLE_GRADES = Arrays.asList("P3", "PK");

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
            String gradeCode = entity.getFieldValue(FIELD_GRADE_CODE);
            if (REPORTABLE_GRADES.contains(gradeCode)) {
                SisStudent student = (SisStudent) entity.getBean();
                String beanPath = data.translateAliasToJavaName((String) field.getParameter(), false);
                value = data.getPropertyAsJavaType(student, beanPath);
            }
            return value;
        }
    }

    /**
     * Retrieve if a student is in PREK program.
     */
    protected class RetrievePrekProgramStatus implements FieldRetriever {
        private static final String CAL_ID = "PSIS-PREK-PROGRAM";

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
            SisStudent student = (SisStudent) entity.getBean();
            String gradeLevel = student.getGradeLevel();
            String preKProgramStatus = null;
            boolean setPreKProgramStatus = false;
            if (PREK_GRADE_1.equals(gradeLevel) || PREK_GRADE_2.equals(gradeLevel)) {
                setPreKProgramStatus = true;
            }
            if (setPreKProgramStatus) {
                String programStatus =
                        data.lookupFederalValue(SisStudent.class, SisStudent.COL_GRADE_LEVEL, gradeLevel);
                if (!StringUtils.isEmpty(programStatus)) {
                    preKProgramStatus = programStatus;
                }
            }
            if (StringUtils.isEmpty(preKProgramStatus)) {
                String status = StringUtils.isEmpty(m_fieldStdPrekStatus) ? null
                        : (String) student.getFieldValueByBeanPath(m_fieldStdPrekStatus);

                if (!StringUtils.isEmpty(status)) {
                    status = lookupStateValue(SisStudent.class, m_fieldStdPrekStatus, status);
                }
                preKProgramStatus = !StringUtils.isEmpty(status) ? status : "00";
            }
            return preKProgramStatus;
        }
    }

    /**
     * The rule for the getValue function is first test if the student is contained in the set of
     * programs.
     * If they are, return the proper value from the program. If they are not, return the value
     * based on the student alias.
     *
     * @author Follett Software Company
     */
    protected abstract class RetrieveProgramCode implements FieldRetriever {
        protected X2Criteria m_pgmCriteria;
        protected Map<String, List<StudentProgramParticipation>> m_stdPgmMap;

        /**
         * Instantiates a new retrieve program code.
         */
        public RetrieveProgramCode() {
            applyCriteria();
            initPgmMap();
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData data, StateReportEntity entity, FieldDefinition field) throws
         *      X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = null;
            SisStudent student = (SisStudent) entity.getBean();

            if (m_stdPgmMap.containsKey(student.getOid())) {
                if (isIndicator()) {
                    value = "Y";
                } else {
                    List<StudentProgramParticipation> pgms = m_stdPgmMap.get(student.getOid());
                    StudentProgramParticipation programParticipation = pgms.get(0);
                    String programCode = programParticipation.getProgramCode();
                    if (pgms.size() > 1) {
                        if (!m_pgmsErrorsMap.containsKey(student.getOid())) {
                            Map<String, StateReportValidationError> errorMap =
                                    new HashMap<String, StateReportValidationError>();
                            errorMap.put(programCode, new StateReportValidationError(entity, field,
                                    "Student has more that one active programs with code = " + programCode,
                                    "LASID=" + STYLE_BOLD + student.getLocalId() + STYLE_END));
                            m_pgmsErrorsMap.put(student.getOid(), errorMap);
                        } else {
                            Map<String, StateReportValidationError> errorMap = m_pgmsErrorsMap.get(student.getOid());
                            errorMap.put(programCode, new StateReportValidationError(entity, field,
                                    "Student has more that one active programs with code = " + programCode,
                                    "LASID=" + STYLE_BOLD + student.getLocalId() + STYLE_END));
                        }
                    }
                    if (!StringUtils.isEmpty(programCode)) {
                        value = lookupStateValue(StudentProgramParticipation.class,
                                StudentProgramParticipation.COL_PROGRAM_CODE, programCode);
                    }
                }
            } else {
                value = getCustomValue(entity);
            }
            return value;
        }

        /**
         * Adds the equal to pgm field.
         *
         * @param beanPath String
         * @param value String
         */
        protected void addEqualToPgmField(String beanPath, String value) {
            if (m_pgmCriteria == null) {
                m_pgmCriteria = new X2Criteria();
                m_pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
                X2Criteria endDateCriteria = new X2Criteria();
                endDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
                X2Criteria emptyEndDateCriteria = new X2Criteria();
                emptyEndDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE,
                        getBroker().getPersistenceKey());
                endDateCriteria.addOrCriteria(emptyEndDateCriteria);
                m_pgmCriteria.addAndCriteria(endDateCriteria);
            }
            m_pgmCriteria.addEqualTo(beanPath, value);
        }

        /**
         * Apply criteria.
         */
        protected abstract void applyCriteria();

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         */
        protected abstract String getCustomValue(StateReportEntity entity) throws X2BaseException;

        /**
         * Checks if is indicator.
         *
         * @return true, if is indicator
         */
        protected boolean isIndicator() {
            return false;
        }

        /**
         * Inits the pgm map.
         */
        private void initPgmMap() {
            QueryByCriteria pgmQuery = new QueryByCriteria(StudentProgramParticipation.class, m_pgmCriteria);
            pgmQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
            m_stdPgmMap =
                    getBroker().getGroupedCollectionByQuery(pgmQuery, StudentProgramParticipation.COL_STUDENT_OID, 200);
        }
    }

    /**
     * Retrieve for a student's SPED placement status. Returns 'Y' if the student has an active SPED
     * status, 'N' otherwise.
     */
    protected class RetrieveSpedPlacement implements FieldRetriever {
        private static final String CAL_ID = "PSIS-SPED";

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
            boolean isSped = false;
            SisStudent student = (SisStudent) entity.getBean();
            PsisEntity psisEntity = (PsisEntity) entity;
            StudentSasidRecord record = psisEntity.getCurrentRecord();
            String spedActiveCode =
                    PreferenceManager.getPreferenceValue(record.getSchool(), SisPreferenceConstants.SPED_ACTIVE_CODE);
            if (spedActiveCode.equals(student.getSpedStatusCode())) {
                isSped = true;
            }
            return Boolean.valueOf(isSped);
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {
        private static final String CALC_ID = "PSIS-STRIP-CHARS";

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
            if (value != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(value);
                value = matcher.replaceAll("");
            }
            return value;
        }
    }

    /**
     * The Class RetrieveTitle1.
     */
    protected class RetrieveTitle1 extends RetrieveProgramCode {
        private static final String CALC_ID = "PSIS-TITLE1";
        private static final String NO = "N";
        private static final String TITLE1_SCH_TYPE_01 = "01";
        private static final String TITLE1_SCH_TYPE_02 = "02";
        private static final String YES = "Y";

        /**
         * Apply criteria.
         *
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#applyCriteria()
         */
        @Override
        public void applyCriteria() {
            if (m_pgmCriteria == null) {
                m_pgmCriteria = new X2Criteria();
                m_pgmCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_schoolYearEndDate);
                m_pgmCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_schoolYearStartDate);
                DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class,
                        StudentProgramParticipation.COL_PROGRAM_CODE);
                X2Criteria refCodeCriteria = new X2Criteria();
                refCodeCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
                refCodeCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, CODE_PGM_TITLE1_STATE_CODE);
                m_pgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE,
                        new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, refCodeCriteria));
            }
        }

        /**
         * Gets the custom value.
         *
         * @param entity StateReportEntity
         * @return String
         * @throws X2BaseException exception
         * @see com.x2dev.procedures.statereporting.ct.CT18Psis.RetrieveProgramCode#getCustomValue(com.follett.fsc.core.k12.tools.stateexports.StateReportEntity)
         */
        @Override
        public String getCustomValue(StateReportEntity entity) throws X2BaseException {
            return NO;
        }

        /**
         * Gets the field value.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @return Object
         * @throws X2BaseException exception
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever.getFieldValue(
         *      StateReportData data, StateReportEntity entity, FieldDefinition field) throws
         *      X2BaseException
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String value = NO;
            PsisEntity sasidEntity = (PsisEntity) entity;
            SisStudent student = (SisStudent) sasidEntity.getBean();
            School school = sasidEntity.getCurrentRecord().getSchool();
            String title1SchType = null;
            if (!StringUtils.isEmpty(m_fieldSklTitle1Type)) {
                if (school != null) {
                    title1SchType = (String) school.getFieldValueByBeanPath(m_fieldSklTitle1Type);
                    if (StringUtils.isEmpty(title1SchType)) {
                        value = null;
                    } else {
                        if (TITLE1_SCH_TYPE_02.equals(title1SchType)) {
                            value = YES;
                        } else if (TITLE1_SCH_TYPE_01.equals(title1SchType)) {
                            if (m_fieldStdTitle1Participant != null) {
                                if (BooleanAsStringConverter.TRUE
                                        .equals(student.getFieldValueByBeanPath(m_fieldStdTitle1Participant))) {
                                    value = YES;
                                } else {
                                    value = NO;
                                }
                            } else {
                                value = (String) super.getFieldValue(data, entity, field);
                                if (!NO.equals(value)) {
                                    value = YES;
                                }
                            }
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Returns a field value that has been stripped of illegal characters for a name field.
     * For first and last names.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTitle1PrgType implements FieldRetriever {
        private static final String CALC_ID = "PSIS-TITLE1-TYPE";

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
            Object value = null;
            PsisEntity sasidEntity = (PsisEntity) entity;
            School school = sasidEntity.getCurrentRecord().getSchool();
            if (!StringUtils.isEmpty(m_fieldSklTitle1Type) && school != null) {
                value = school.getFieldValueByBeanPath(m_fieldSklTitle1Type);
            }
            if (TITLE1_TYPE_01.equals(value) && entity.getFieldValue(EXPORT_FIELD_TITLE_I_PART).equals(STRING_N)) {
                value = null;
            }
            return value;
        }
    }

    /**
     * Returns a truant indicator based on student attendance.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveTruant implements FieldRetriever {
        private static final String CALC_ID = "PSIS-TRUANT";
        private static final String NO = "N";
        private static final int TRAUNT_BY_MONTH_THRESHOLD = 4;
        private static final int TRAUNT_BY_YEAR_THRESHOLD = 10;
        private static final String YES = "Y";

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
            String value = NO;
            if (CYCLE_OCTOBER.equalsIgnoreCase(m_cycle)) {
                return null;
            }
            SisStudent student = (SisStudent) entity.getBean();
            String gradeCode = entity.getFieldValue(EXPORT_FIELD_GRADE_CODE);
            if (CODE_STD_GRADE_STATE_CODE_P3.equals(gradeCode)
                    || CODE_STD_GRADE_STATE_CODE_PK.equals(gradeCode)) {
                return NO;
            }
            List<StudentAttendance> attendanceList = m_studentAttendanceMap.get(student.getOid());
            if (attendanceList != null && !attendanceList.isEmpty()) {
                Calendar cal = Calendar.getInstance();
                int monthPrev = 0;
                int trauntCountByMonth = 0;
                int trauntCountByYear = 0;
                boolean isKGrade = CODE_STD_GRADE_STATE_CODE_KF.equals(gradeCode)
                        || CODE_STD_GRADE_STATE_CODE_KH.equals(gradeCode);
                for (StudentAttendance attendance : attendanceList) {
                    if (YES.equals(value)) {
                        break;
                    }
                    if (isKGrade && student.getPerson().getAgeAsOfDate(attendance.getDate()) < 5) {
                        continue;
                    }
                    trauntCountByYear += 1;
                    if (trauntCountByYear >= TRAUNT_BY_YEAR_THRESHOLD) {
                        value = YES;
                    } else {
                        cal.setTime(attendance.getDate());
                        int month = cal.get(Calendar.MONTH);
                        if (month != monthPrev) {
                            trauntCountByMonth = 0;
                            monthPrev = month;
                        }
                        trauntCountByMonth += 1;
                        if (trauntCountByMonth >= TRAUNT_BY_MONTH_THRESHOLD) {
                            value = YES;
                        }
                    }
                }
            }
            return value;
        }
    }

    /**
     * Validate for duplicate programs of the same type on a single student.
     */
    protected class ValidatePgm implements FieldValidator {
        private static final String VAL_ID = "VAL-PGMS";

        /**
         * Gets the field validation.
         *
         * @param data StateReportData
         * @param entity StateReportEntity
         * @param field FieldDefinition
         * @param value String
         * @return Collection
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
            SisStudent student = (SisStudent) entity.getBean();
            String param = (String) field.getParameter();
            if (m_pgmsErrorsMap.get(student.getOid()) != null
                    && m_pgmsErrorsMap.get(student.getOid()).get(param) != null) {
                errors.add(m_pgmsErrorsMap.get(student.getOid()).get(param));
            }
            return errors;
        }
    }

    /**
     * Constants.
     */
    protected static final String ALIAS_PGM_ELL_CODE = "ELL_CODE";
    protected static final String ALIAS_SKL_TITLE1_TYPE = "all-skl-Title1SchoolType";
    protected static final String ALIAS_STD_ELL_PGM = "PSIS15";
    protected static final String ALIAS_STD_GIFTED_TALENTED = "PSIS27";
    protected static final String ALIAS_STD_NEXUS = "NEXUS";
    protected static final String ALIAS_STD_PREK_STATUS = "PSIS20";
    protected static final String ALIAS_STD_PSIS16 = "PSIS16";
    protected static final String ALIAS_STD_TITLE1_PARTICIPANT = "all-std-Title1Participant";
    protected static final String CODE_NOT_GIFTED_OR_TALENTED = "01";
    protected static final String CODE_PGM_ENGLISH_LANGUAGE_LERNER = "ESL";
    protected static final String CODE_PGM_GIFTED_AND_TALENTED = "ALP";
    protected static final String CODE_PGM_IMMIGRANT_STATUS = "IMMIGRANT";
    protected static final String CODE_PGM_MILITARY_FAMILY = "MILITARY";
    protected static final String CODE_PGM_TITLE1_STATE_CODE = "T1";
    protected static final String CODE_STD_GRADE_STATE_CODE_KF = "KF";
    protected static final String CODE_STD_GRADE_STATE_CODE_KH = "KH";
    protected static final String CODE_STD_GRADE_STATE_CODE_P3 = "P3";
    protected static final String CODE_STD_GRADE_STATE_CODE_PK = "PK";
    protected static final String CYCLE_JUNE = "june";
    protected static final String CYCLE_OCTOBER = "october";
    protected static final String EXPORT_FIELD_DISTRICT = "REPORTING_DISTRICT";
    protected static final String EXPORT_FIELD_GRADE_CODE = "GRADE_CODE";
    protected static final String EXPORT_FIELD_PSIS_29_MEMBERSHIP_DAYS = "MEMBERSHIP_DAYS";
    protected static final String EXPORT_FIELD_PSIS_30_MEMBERSHIP_DAYS = "ATTENDANCE_DAYS";
    protected static final String EXPORT_FIELD_TITLE_I_PART = "TITLE I PARTICIPATION";
    protected static final String ILLEGAL_NAME_CHARACTERS = "[_\\W&&[^-'.\\s]]";
    protected static final String PARAM_CYCLE = "cycle";
    protected static final String PARAM_INCLUDE_HEADING = "includeHeading";
    protected static final String PARAM_INCLUDE_MEMBERSHIP_AND_ATTENDANCE_DAYS = "includeMembershipandAttendanceDays";
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_REQUIRE_MEMBER_DAY = "requireMemberDay";
    protected static final String PARAM_UPDATE_RECORDS = "updateRecords";
    protected static final String PREK_GRADE_1 = "PK";
    protected static final String PREK_GRADE_2 = "P3";
    protected static final String RESIDENT_TOWN_ALIAS = "PSIS04";
    protected static final String STRING_N = "N";
    protected static final String STRING_Y = "Y";
    protected static final String TITLE1_TYPE_01 = "01";
    /**
     * Supporting instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected CTStudentHelper m_ctHelper;
    protected String m_fieldStdEllProgram;
    protected String m_fieldStdGifTal;
    protected String m_fieldStdNexus;
    protected String m_fieldStdPrekStatus;
    protected String m_fieldStdPsis16;
    protected String m_fieldStdSpedFacility;
    protected String m_cycle;
    protected Pattern m_illegalNameCharacters;
    protected Boolean m_includeMembershipAndAttendanceDays;
    protected Map<String, Map<String, StateReportValidationError>> m_pgmsErrorsMap =
            new HashMap<String, Map<String, StateReportValidationError>>();
    protected PlainDate m_reportDate;
    protected Boolean m_requireMemberDay;
    protected PlainDate m_schoolYearEndDate;
    protected PlainDate m_schoolYearStartDate;
    protected Map<String, List<StudentAttendance>> m_studentAttendanceMap;
    protected String m_fieldSklTitle1Type;
    protected String m_fieldStdTitle1Participant;

    /**
     * Gets the heading.
     *
     * @return String
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#getHeading()
     */
    @Override
    public String getHeading() {
        String heading = "";
        Boolean includeHeading = (Boolean) getParameter(PARAM_INCLUDE_HEADING);
        if (includeHeading != null && includeHeading.booleanValue()) {
            heading = super.getHeading();
        }
        return heading;
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        initializeFields();
        /*
         * Build helper object.
         */

        m_ctHelper = new CTStudentHelper(this, m_reportDate);
        m_ctHelper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_INPUT, Boolean.TRUE);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_APPLY_EXCLUDE, Boolean.TRUE);
        m_ctHelper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        if (getSetupErrors().size() == 0) {
            setQuery(m_ctHelper.getStudentQuery(true));
            // Set the entity class
            setEntityClass(PsisEntity.class);
            // Build maps of retriever functions and validator functions
            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            calcs.put(RetrieveSpedPlacement.CAL_ID, new RetrieveSpedPlacement());
            calcs.put(RetrieveMembershipDays.CALC_ID, new RetrieveMembershipDays());
            calcs.put(RetrieveAttendanceDays.CALC_ID, new RetrieveAttendanceDays());
            calcs.put(RetrievePrekNumericValue.CAL_ID, new RetrievePrekNumericValue());
            calcs.put(RetrievePrekProgramStatus.CAL_ID, new RetrievePrekProgramStatus());
            calcs.put(RetrieveGiftedandTalented.CALC_ID, new RetrieveGiftedandTalented());
            calcs.put(RetrieveFacilityCodes.CALC_ID, new RetrieveFacilityCodes());
            calcs.put(RetrieveNexus.CALC_ID, new RetrieveNexus());
            calcs.put(RetrieveGradeLevel.CALC_ID, new RetrieveGradeLevel());
            calcs.put(RetrieveEnglishLanguageLearner.CALC_ID, new RetrieveEnglishLanguageLearner());
            calcs.put(RetrieveEnglishLanguageLearnerProgramCode.CALC_ID,
                    new RetrieveEnglishLanguageLearnerProgramCode());
            calcs.put(RetrieveImmigrantStatus.CALC_ID, new RetrieveImmigrantStatus());
            calcs.put(RetrieveMilitaryFamily.CALC_ID, new RetrieveMilitaryFamily());
            calcs.put(RetrieveTitle1.CALC_ID, new RetrieveTitle1());
            calcs.put(RetrieveTitle1PrgType.CALC_ID, new RetrieveTitle1PrgType());
            calcs.put(RetrieveTruant.CALC_ID, new RetrieveTruant());
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidatePgm.VAL_ID, new ValidatePgm());
            super.addCalcs(calcs);
            super.addValidators(validators);

            initializeAttendanceMap();

        }
    }

    /**
     * Initialize students attendance map .
     */
    private void initializeAttendanceMap() {

        // Load student attendance.
        X2Criteria studentAttendanceCriteria = new X2Criteria();
        studentAttendanceCriteria.addEqualTo(StudentAttendance.COL_ABSENT_INDICATOR, Boolean.TRUE);
        studentAttendanceCriteria.addEqualTo(StudentAttendance.COL_EXCUSED_INDICATOR, Boolean.FALSE);
        studentAttendanceCriteria.addGreaterOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getStartDate());
        studentAttendanceCriteria.addLessOrEqualThan(StudentAttendance.COL_DATE, getCurrentContext().getEndDate());
        studentAttendanceCriteria.addNotIn(StudentAttendance.COL_REASON_CODE, m_ctHelper.getPresentAttendanceCodes());

        studentAttendanceCriteria.addIn(StudentAttendance.REL_STUDENT,
                new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_ctHelper.getStudentCriteria()));
        QueryByCriteria studentAttendanceQuery =
                new QueryByCriteria(StudentAttendance.class, studentAttendanceCriteria);
        studentAttendanceQuery.addOrderBy(StudentAttendance.COL_STUDENT_OID, true);
        studentAttendanceQuery.addOrderBy(StudentAttendance.COL_DATE, false);

        m_studentAttendanceMap =
                getBroker().getGroupedCollectionByQuery(studentAttendanceQuery, StudentAttendance.COL_STUDENT_OID, 50);

    }

    /**
     * Initialize member fields.
     */
    private void initializeFields() {
        m_fieldStdPsis16 = translateAliasToJavaName(ALIAS_STD_PSIS16, true);
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        m_fieldStdEllProgram = translateAliasToJavaName(ALIAS_STD_ELL_PGM, false);
        m_fieldStdGifTal = translateAliasToJavaName(ALIAS_STD_GIFTED_TALENTED, false);
        m_fieldStdPrekStatus = translateAliasToJavaName(ALIAS_STD_PREK_STATUS, false);
        m_fieldStdNexus = translateAliasToJavaName(ALIAS_STD_NEXUS, true);
        m_includeMembershipAndAttendanceDays = (Boolean) getParameter(PARAM_INCLUDE_MEMBERSHIP_AND_ATTENDANCE_DAYS);
        m_requireMemberDay = (Boolean) getParameter(PARAM_REQUIRE_MEMBER_DAY);
        m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
        m_cycle = (String) getParameter(PARAM_CYCLE);
        Boolean updateRecords = (Boolean) getParameter(PARAM_UPDATE_RECORDS);
        if (!m_includeMembershipAndAttendanceDays.booleanValue() && updateRecords.booleanValue()) {
            addSetupError("Invalid Input",
                    "if \"Update student records with calculated values\" is checked then \"Include membership and attendance days\" must be checked.");
        }
        m_schoolYearStartDate = getOrganization().getCurrentContext().getStartDate();
        m_schoolYearEndDate = getOrganization().getCurrentContext().getEndDate();
        m_fieldStdTitle1Participant = translateAliasToJavaName(ALIAS_STD_TITLE1_PARTICIPANT, false);
        m_fieldSklTitle1Type = translateAliasToJavaName(ALIAS_SKL_TITLE1_TYPE, true);
    }
}
