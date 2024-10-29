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

import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Export procedure for WA Ethnicity.
 *
 * @author X2 Development Corporation
 */

public class WAEthnicity extends StateReportData {
    /**
     * Entity class for WA Ethnicity export.
     */
    public static class WAEthnicityEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        List<Integer> m_raceCodes;
        WAEthnicity m_sdData;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WAEthnicityEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the Race record for the current index.
         *
         * @return Race
         */
        public Integer getRaceCode() {
            return m_raceCodes.get(getCurrentRow());
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

            SisStudent student = (SisStudent) getBean();
            m_sdData = (WAEthnicity) data;

            // Determine if we should report this student. Use same criteria as District Student.
            List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, m_sdData);

            // Lookup all race codes.
            m_raceCodes = new ArrayList<Integer>();
            if (districtSpans.size() > 0) {
                Collection<Race> races = m_sdData.m_helper.getRaces(student);

                // Check all race codes for hispanic/latino type codes.
                if (races != null) {
                    Iterator<Race> iterator = races.iterator();
                    while (iterator.hasNext()) {
                        Race race = iterator.next();
                        String raceCode = m_sdData.lookupReferenceCodeByBeanPath(Race.class, Race.COL_RACE_CODE,
                                race.getRaceCode(), ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (StringUtils.isNumeric(raceCode) && Integer.parseInt(raceCode) < 200) {
                            m_raceCodes.add(Integer.valueOf(raceCode));
                        }
                    }
                }

                // If no code is provided, use the hispanic/latino indicator.
                if (m_raceCodes.isEmpty()) {
                    boolean hispanicLatinoIndicator = false;
                    if (student.getPerson() != null) {
                        hispanicLatinoIndicator = student.getPerson().getHispanicLatinoIndicator();
                    }
                    if (hispanicLatinoIndicator) {
                        m_raceCodes.add(CODE_OTHER_HISPANIC_LATINO);
                    } else {
                        m_raceCodes.add(CODE_NOT_HISPANIC_LATINO);
                    }
                }
            }

            setRowCount(m_raceCodes.size());
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

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. indistrict
         * transfer.
         *
         * @param student Student
         * @param sdData WAEthnicity
         * @return List<DistrictEnrollmentSpan>
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAEthnicity sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;

            removeExcludedSchoolRecords(enrollmentSpans);
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                if (isMember(span)) {
                    /*
                     * Check if the span is a no-show span. Do not include no-show spans.
                     * A no-show span represents an enrollment where the student never showed up.
                     * It is identified by a withdrawal code that has NS in the local code of the
                     * reference table.
                     */
                    boolean noShow = false;
                    StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                    if (enrollment != null) {
                        String withdrawalCode = enrollment.getEnrollmentCode();
                        withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode,
                                withdrawalCode, ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                        if (CODE_NO_SHOW.equals(withdrawalCode)) {
                            noShow = true;
                        }
                    }

                    if (!noShow) {
                        // Check the span for entry type (internal or external)
                        enrollment = span.getFirstActiveEnrollment();
                        if (enrollment != null) {
                            String code = enrollment.getEnrollmentCode();
                            String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                    ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                            if (CODE_NEW_TO_DISTRICT.equals(stateCode) || CODE_RETURN_TO_DISTRICT.equals(stateCode)) {
                                if (currentSpan != null &&
                                        (currentSpan.m_exitDate == null ||
                                                !sdData.getCurrentContext().getStartDate()
                                                        .after(currentSpan.m_exitDate))) {
                                    districtSpans.add(currentSpan);
                                }
                                currentSpan = null;

                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                            } else {
                                if (currentSpan == null) {
                                    currentSpan = new DistrictEnrollmentSpan();
                                    currentSpan.m_entryEnrollment = enrollment;
                                }
                            }
                            enrollment = span.getFirstInactiveEnrollment();
                            if (enrollment != null) {
                                currentSpan.m_exitDate = enrollment.getEnrollmentDate();
                            } else {
                                currentSpan.m_exitDate = span.getLastActiveDate();
                            }
                        }
                    }
                }
            }
            if (currentSpan != null &&
                    (currentSpan.m_exitDate == null ||
                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
                districtSpans.add(currentSpan);
            }

            return districtSpans;
        }

        /**
         * Returns the first active date.
         *
         * @param span The span
         * @return PlainDate The first active date
         */
        private PlainDate getFirstActiveDate(StudentEnrollmentSpan span) {
            PlainDate firstActiveDate = null;

            if (span != null && span.getFirstActiveEnrollment() != null) {
                firstActiveDate = span.getFirstActiveEnrollment().getEnrollmentDate();
            }

            return firstActiveDate;
        }

        /**
         * Returns the last active date.
         *
         * @param span The span
         * @return PlainDate The last active date
         */
        private PlainDate getLastActiveDate(StudentEnrollmentSpan span) {
            PlainDate lastActiveDate = null;

            if (span != null && span.getFirstInactiveEnrollment() != null) {
                lastActiveDate = span.getFirstInactiveEnrollment().getEnrollmentDate();
            }

            return lastActiveDate;
        }

        /**
         * Returns true if the span is a member.
         *
         * @param span The span
         *
         * @return boolean True if the span is a member
         */
        private boolean isMember(StudentEnrollmentSpan span) {
            PlainDate firstActiveDate = getFirstActiveDate(span);
            PlainDate lastActiveDate = getLastActiveDate(span);
            boolean isMember = true;

            if (firstActiveDate != null && lastActiveDate != null && firstActiveDate.equals(lastActiveDate)) {
                if (!m_sdData.m_memberOnEntryDate && !m_sdData.m_memberOnWithdrawalDate) {
                    isMember = false;
                } else if (m_sdData.m_memberOnEntryDate != m_sdData.m_memberOnWithdrawalDate) {
                    isMember = false;
                }
            }

            return isMember;
        }

        /**
         * remove any enrollment spans that have a no-show withdrawal code.
         * this is identified by having NS in the local code of the withdrawal code.
         *
         * @param enrollmentSpans List<StudentEnrollmentSpan>
         */
        private void removeExcludedSchoolRecords(List<StudentEnrollmentSpan> enrollmentSpans) {
            Iterator<StudentEnrollmentSpan> spanIterator = enrollmentSpans.iterator();
            while (spanIterator.hasNext()) {
                StudentEnrollmentSpan span = spanIterator.next();
                StudentEnrollment enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    if (!m_sdData.includeSchool(enrollment.getSchoolOid())) {
                        spanIterator.remove();
                    }
                }
            }
        }
    }

    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    protected static class DistrictEnrollmentSpan {
        StudentEnrollment m_entryEnrollment;
        PlainDate m_exitDate;
    }

    /**
     * Returns the EthnicityCode information.
     */
    protected class RetrieveEthnicityCode implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            return ((WAEthnicityEntity) entity).getRaceCode();
        }
    }

    /**
     * Validate the ethnicity code.
     */
    protected class ValidateEthnicityCode implements FieldValidator {
        private static final String VAL_ID = "ETH-CODE-VAL";
        private List<String> m_uniqueNonHispanic = new ArrayList<String>();

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldValidator#getFieldValidation(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition, java.lang.String)
         */
        @Override
        public Collection<StateReportValidationError> getFieldValidation(StateReportData data,
                                                                         StateReportEntity entity,
                                                                         FieldDefinition field,
                                                                         String value) {
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>();
            WAEthnicityEntity waEntity = (WAEthnicityEntity) entity;
            Integer raceCode = waEntity.getRaceCode();

            if (raceCode.equals(CODE_OTHER_HISPANIC_LATINO)) {
                errors.add(new StateReportValidationError(entity, field,
                        "There is no suitable Ethnicity Code available for the student",
                        "Student's HispanicLatinoIndicator = " + STYLE_BOLD + "true" + STYLE_END));
            }

            if (raceCode.equals(CODE_NOT_HISPANIC_LATINO)) {
                SisStudent student = (SisStudent) waEntity.getBean();
                String studentOid = student.getOid();
                if (m_uniqueNonHispanic.contains(studentOid)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "If Ethnicity Code equals '10' (Non-Hispanic), there cannot be other records in this file for the same student",
                            "Student = " + STYLE_BOLD + student.getNameView() + STYLE_END));
                }
                m_uniqueNonHispanic.add(studentOid);
            }

            return errors;
        }
    }

    /*
     * Constants: Codes
     */
    protected static final String CODE_NEW_TO_DISTRICT = "1";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final Integer CODE_NOT_HISPANIC_LATINO = Integer.valueOf(10);
    protected static final Integer CODE_OTHER_HISPANIC_LATINO = Integer.valueOf(90);
    protected static final String CODE_RETURN_TO_DISTRICT = "2";

    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";

    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";

    /*
     * Instance variables.
     */
    protected String m_excludeSchool;
    protected StudentHistoryHelper m_helper;
    protected boolean m_memberOnEntryDate;
    protected boolean m_memberOnWithdrawalDate;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected Map m_excludeSchoolMap;

    /**
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize()
     */
    /*
     * Initialize the data module.
     * Initialize necessary working resources.
     * Define query for students to load.
     * Define list of field definitions for the export.
     *
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     * com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    public void initialize() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_memberOnEntryDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(this.getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
        m_memberOnWithdrawalDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(this.getOrganization(),
                SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);


        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            loadSchoolExcludeMap();
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(WAEthnicityEntity.class);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ETH-CODE", new RetrieveEthnicityCode());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put(ValidateEthnicityCode.VAL_ID, new ValidateEthnicityCode());
            super.addValidators(validators);
        }
    }

    /**
     * Loads a map of schools that have been selected to be excluded from state reporting. (exclude
     * from reporting on school table is selected)
     */
    private void loadSchoolExcludeMap() {
        X2Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addEqualTo(m_excludeSchool, BooleanAsStringConverter.TRUE);
        BeanQuery query = new BeanQuery(School.class, schoolCriteria);
        m_excludeSchoolMap = getBroker().getGroupedCollectionByQuery(query, X2BaseBean.COL_OID, 128);
    }

    /**
     * Checks if schoolId given is a school to exclude. If that school is excluded this method will
     * return false ie - !included.
     *
     * @param schoolOid String
     * @return boolean
     */
    public boolean includeSchool(String schoolOid) {
        return (m_excludeSchoolMap == null) || !m_excludeSchoolMap.containsKey(schoolOid);
    }

}
