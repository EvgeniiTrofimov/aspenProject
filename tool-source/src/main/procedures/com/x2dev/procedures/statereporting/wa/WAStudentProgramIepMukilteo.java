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

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.BeanQuery;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.OrganizationManager;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisSchool;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.DateUtils;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export procedure for Washington State CEDARS
 * Student Special Education Programs file.
 *
 * ********************************************
 * This is a modified version made for Mukilteo
 * ********************************************
 *
 * @author X2 Development Corporation
 *
 */
public class WAStudentProgramIepMukilteo extends StateReportData {
    /**
     * Entity record for the Student Program IEP export.
     */
    public static class WAStudentProgramIepEntity extends StateReportEntity {

        /**
         * The calculated school for the program.
         */
        private SisSchool m_school;

        /**
         * The calculated exit date for students who have withdrawn.
         */
        private PlainDate m_exitDate;
        private String m_exitReason;

        /**
         * List of valid special ed programs this student have
         */
        private ArrayList<StudentProgramParticipation> m_validPgms;

        /**
         * Exit enrollment code (if withdrawn)
         */
        private String m_exitEnrollmentCode;

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.follett.fsc.core.k12.tools.stateexports.StateReportEntity#getEntityName()
         */
        @Override
        public String getEntityName() {
            String name = null;

            StudentProgramParticipation program = getRecentValidPgm();
            if (program != null) {
                SisStudent student = program.getStudent();

                name = student.getNameView() +
                        " [LASID: " + student.getLocalId() +
                        ", SASID: " + student.getStateId() +
                        "] " + program.getProgramCode();
            }

            return name;
        }

        /**
         * Initialize method.
         * Determine if the program overlaps an active span to get the appropriate school at the
         * time.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            WAStudentProgramIepMukilteo spData = (WAStudentProgramIepMukilteo) data;
            SisStudent student = (SisStudent) bean;

            // collect all the valid special ed programs
            Collection<StudentProgramParticipation> pgms = spData.m_stdPgmsMap.get(student.getOid());
            m_validPgms = collectValidSpedPrograms(pgms);

            // if there are any valid ones, we'll use the most recent one
            StudentProgramParticipation recentPgm = null;
            if (m_validPgms != null && m_validPgms.size() > 0) {
                recentPgm = getRecentValidPgm();
                if (recentPgm != null) {
                    List<StudentEnrollmentSpan> enrollmentSpans =
                            spData.m_helper.getStudentEnrollmentSpans(student, true);

                    // Skip the program if the student is not in the District Student file.
                    List<DistrictEnrollmentSpan> districtSpans = getDistrictSpans(student, spData);
                    if (districtSpans.size() == 0) {
                        setRowCount(0);
                    } else {
                        // Get most recent span
                        DistrictEnrollmentSpan span = districtSpans.get(districtSpans.size() - 1);
                        if (span.m_exitEnrollment != null) {
                            m_exitDate = null;
                            m_exitReason = null;
                            // Get exit reason as a translation of the withdrawal code.
                            StudentEnrollment withdrawal = span.m_exitEnrollment;
                            if (withdrawal != null) {
                                String code = withdrawal.getEnrollmentCode();
                                code = data.lookupReferenceCodeByRefTbl(spData.m_refTableWithdrawalCode, code,
                                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                                if ("G0".equals(code) || "GA".equals(code)) {
                                    m_exitReason = spData.m_exitReasonCodesMap.get("B");
                                    m_exitDate = span.m_exitDate;
                                }
                            }
                        }
                    }

                    // Find the enrollment school and last exit date.
                    boolean oneIsNull = false;
                    for (StudentEnrollmentSpan span : enrollmentSpans) {
                        if (recentPgm.getEndDate() == null ||
                                (span.getFirstActiveDate() != null &&
                                        recentPgm.getEndDate() != null &&
                                        !span.getFirstActiveDate().after(recentPgm.getEndDate()))) {
                            if (span.getLastActiveDate() == null ||
                                    (span.getLastActiveDate() != null &&
                                            recentPgm.getStartDate() != null &&
                                            !span.getLastActiveDate().before(recentPgm.getStartDate()))) {
                                m_school = span.getSchool();
                            }
                        }
                        if (span.getLastActiveDate() == null) {
                            oneIsNull = true;
                            m_exitDate = null;
                        } else if (!oneIsNull && (m_exitDate == null || m_exitDate.before(span.getLastActiveDate()))) {
                            m_exitDate = span.getLastActiveDate();
                            m_exitEnrollmentCode = span.getFirstInactiveEnrollment().getEnrollmentCode();
                        }
                    }
                    if (m_school == null) {
                        m_school = student.getSchool();
                    }
                } else {
                    setRowCount(0);
                }
            } else // if there's no valid pgm, we skip
            {
                setRowCount(0);
            }
        }

        /**
         * Setup the valid special ed programs into a list
         * <p>
         * I expect <code>pgms</code> to be the student's special ed programs sorted by DESCENDING
         * date order,
         * i.e. it is from most-recent to least-recent (newest to oldest)
         * <p>
         * Valid means the exit reason != "Ineligible" or disability != "00"
         *
         * @param pgms Collection<StudentProgramParticipation>
         * @return ArrayList
         */
        private ArrayList<StudentProgramParticipation> collectValidSpedPrograms(Collection<StudentProgramParticipation> pgms) {
            ArrayList<StudentProgramParticipation> validPgms = new ArrayList<StudentProgramParticipation>();

            if (pgms != null && pgms.size() > 0) {
                for (StudentProgramParticipation pgm : pgms) {
                    String pgmExitReason = (String) pgm.getFieldValueByAlias(ALIAS_EXIT_REASON);
                    String pgmDisability = (String) pgm.getFieldValueByAlias(ALIAS_DISABILITY);

                    // convert to state code and pad left (length 2) with 0's
                    if (pgmDisability != null) {
                        int stateCodeRefTypeOrdinal = ExportFormatField.ReferenceMapTypeCode.STATE.ordinal();
                        String pgmDisabilityStateCode = getData().lookupReferenceCodeByAlias(ALIAS_DISABILITY,
                                pgmDisability,
                                stateCodeRefTypeOrdinal);
                        pgmDisability = StringUtils.padLeft(pgmDisabilityStateCode, 2, '0');
                    }

                    if (!("Ineligible".equals(pgmExitReason) && "00".equals(pgmDisability))) {
                        validPgms.add(pgm);
                    }
                }
            }

            return validPgms;
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
         * Get the most recent valid special ed program
         * <p>
         * This just gets the first record in the list of valid special ed programs
         * <p>
         * i.e., the list is ordered from most recent to earliest
         *
         * @return Student program participation
         */
        protected StudentProgramParticipation getRecentValidPgm() {
            StudentProgramParticipation recentPgm = m_validPgms.get(0);

            PlainDate endDate = recentPgm.getEndDate();
            if (endDate != null && endDate.before(getData().getCurrentContext().getStartDate())) {
                recentPgm = null;
            }

            return recentPgm;
        }

        /**
         * Get the most earliest valid special ed program
         * <p>
         * This just gets the last record in the list of valid special ed programs
         * <p>
         * i.e., the list is ordered from most recent to earliest
         *
         * @return Student program participation
         */
        protected StudentProgramParticipation getEarliestValidPgm() {
            return m_validPgms.get(m_validPgms.size() - 1);
        }

        /**
         * Return the calculated exit date. This is the latest exit date for all
         * enrollment spans, or null if a span is still open (active).
         *
         * @return PlainDate
         */
        protected PlainDate getExitDate() {
            return m_exitDate;
        }

        /**
         * Return the calculated exit reason. This is from the latest exit enrollment span
         * withdrawal code.
         *
         * @return String
         */
        protected String getExitReason() {
            return m_exitReason;
        }

        /**
         * Return the enrollment code for the student's supposedly exit
         * <p>
         * Used for determining if the student was graduated.
         *
         * @return String
         */
        protected String getExitEnrollmentCode() {
            return m_exitEnrollmentCode;
        }

        /**
         * Return the school associated with the specified program.
         *
         * @return SisSchool
         */
        protected SisSchool getSchool() {
            return m_school;
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. in district
         * transfer.
         *
         * @param student Student
         * @param sdData WAStudentProgramIepMukilteo
         * @return List<DistrictEnrollmentSpan>
         */
        protected List<DistrictEnrollmentSpan> getDistrictSpans(Student student, WAStudentProgramIepMukilteo sdData) {
            List<StudentEnrollmentSpan> enrollmentSpans = sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();
            DistrictEnrollmentSpan currentSpan = null;
            for (StudentEnrollmentSpan span : enrollmentSpans) {
                /*
                 * Check if the span is a no-show span. Do not include no-show spans.
                 * A no-show span represents an enrollment where the student never showed up.
                 * It is identified by a withdrawal code that has NS in the local code of the
                 * reference table.
                 */
                boolean doNotShow = false;
                StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
                if (enrollment != null) {
                    String withdrawalCode = enrollment.getEnrollmentCode();
                    withdrawalCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableWithdrawalCode, withdrawalCode,
                            ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                    if (CODE_NO_SHOW.equals(withdrawalCode)) {
                        doNotShow = true;
                    }
                }

                /*
                 * Check if the enrollment span is non-primary. If so, do not include.
                 */
                enrollment = span.getFirstActiveEnrollment();
                if (enrollment != null) {
                    String nonPrimary = (String) enrollment.getFieldValueByBeanPath(sdData.m_fieldNonPrimaryIndicator);
                    if (BooleanAsStringConverter.TRUE.equals(nonPrimary)) {
                        doNotShow = true;
                    }
                }

                if (!doNotShow) {
                    // Check the span for entry type (internal or external)
                    enrollment = span.getFirstActiveEnrollment();
                    if (enrollment != null) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = sdData.lookupReferenceCodeByRefTbl(sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                        if ("1".equals(stateCode) || "2".equals(stateCode)) {
                            if (currentSpan != null &&
                                    (currentSpan.m_exitDate == null ||
                                            !sdData.getCurrentContext().getStartDate().after(currentSpan.m_exitDate))) {
                                districtSpans.add(currentSpan);
                            }
                            currentSpan = null;

                            currentSpan = new DistrictEnrollmentSpan();
                            currentSpan.m_entryEnrollment = enrollment;
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
                        } else {
                            if (currentSpan == null) {
                                currentSpan = new DistrictEnrollmentSpan();
                                currentSpan.m_entryEnrollment = enrollment;
                            }
                            currentSpan.m_exitDate = span.getLastActiveDate();
                            currentSpan.m_exitEnrollment = span.getFirstInactiveEnrollment();
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
    }

    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    protected static class DistrictEnrollmentSpan {
        StudentEnrollment m_entryEnrollment;
        PlainDate m_exitDate;
        StudentEnrollment m_exitEnrollment;
    }

    /**
     * Returns calculated values for the program from the entity.
     * <p>
     * Param:
     * <dl>
     *
     * <dt>SCHOOL</dt>
     * <dd>The school id (LocationId)</dd>
     * 
     * <dt>LRE_CODE</dt>
     * <dd>Student's most RECENT valid special ed program's PGM_FIELDA_009</dd>
     * 
     * <dt>START_DATE</dt>
     * <dd>Student's most RECENT valid special ed program's PGM_FIELDA_019.
     * If that's empty, then use Student's EARLIEST valid special ed program's PGM_START_DATE</dd>
     * 
     * <dt>EXIT_DATE</dt>
     * <dd>1. Student's most RECENT valid special ed program's PGM_END_DATE.
     * 2. If that's empty... check the student's withdrawn Enrollment record...
     * a. If that withdrawn enrollment record's enrollment code is a graduating state code (i.e,
     * "G0"), then use that enrollment date.
     * b. If that withdrawn enrollment record's enrollment code is not a graduate state code (i.e.,
     * anything not "G0"), then leave empty.</dd>
     * 
     * <dt>EXIT_REASON</dt>
     * <dd>1. Check if the student withdrew with a graduating state code, "G0". If so, use "3".
     * 2. If not, use the most RECENT valid special ed program's PGM_FIELDB_001.</dd>
     * 
     * <dt>INITIAL_REFER_DATE</dt>
     * <dd>Student's most RECENT valid special ed program's PGM_FIELDA_018</dd>
     * 
     * <dt>INITIAL_ELIG_DATE</dt>
     * <dd>Student's most RECENT valid special ed program's PGM_FIELDA_019</dd>
     * 
     * <dt>LAST_REVIEW_DATE</dt>
     * <dd>Student's most RECENT valid special ed program's PGM_FIELDA_011</dd>
     * 
     * <dt>LAST_EVAL_DATE</dt>
     * <dd>Student's most RECENT valid special ed program's PGM_FIELDA_010</dd>
     * 
     * </dl>
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveProgram implements FieldRetriever {
        private static final String PARAM_LAST_EVAL_DATE = "LAST_EVAL_DATE";
        private static final String PARAM_LAST_REVIEW_DATE = "LAST_REVIEW_DATE";
        private static final String PARAM_INITIAL_ELIG_DATE = "INITIAL_ELIG_DATE";
        private static final String PARAM_INITIAL_REFER_DATE = "INITIAL_REFER_DATE";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            WAStudentProgramIepEntity spEntity = (WAStudentProgramIepEntity) entity;
            String parameter = (String) field.getParameter();
            StudentProgramParticipation program = spEntity.getRecentValidPgm();

            Object value = null;
            if (PARAM_SCHOOL.equals(parameter)) {
                SisSchool school = spEntity.getSchool();
                value = school.getSchoolId();
            } else if (PARAM_LRE_CODE.equals(parameter)) {
                // return LRE code state code
                value = program.getFieldValueByAlias(ALIAS_LRE_CODE);
                if (value != null && !value.toString().isEmpty()) {
                    value = lookupStateValue(StudentProgramParticipation.class, m_javaNameLreCode, value.toString());
                }
            } else if (PARAM_START_DATE.equals(parameter)) {
                // use sped initial eligibility date
                value = program.getFieldA019();
                if (value != null) {
                    value = DateUtils.getDate(value.toString());
                }

                // if value is still empty, use the earliest valid special ed program and use
                // pgm_start_date instead
                if (value == null) {
                    program = spEntity.getEarliestValidPgm();
                    value = program.getStartDate();
                }
            } else if (PARAM_EXIT_DATE.equals(parameter)) {
                PlainDate exitDate = program.getEndDate(); // get the end date from the program
                if (exitDate == null) {
                    // if that doesn't exist, then check if the student graduated. if yes, use the
                    // withdrawn exit date
                    String exitEnrollmentCode = spEntity.getExitEnrollmentCode();
                    if (m_graduatedCodes.contains(exitEnrollmentCode)) {
                        exitDate = spEntity.getExitDate();
                    }
                }
                value = exitDate;
            } else if (PARAM_EXIT_REASON.equals(parameter)) {
                // get whatever is in the program's exit reason field
                value = program.getFieldValueByAlias(ALIAS_EXIT_REASON);
                if (value != null && !value.toString().isEmpty()) {
                    value = lookupStateValue(StudentProgramParticipation.class, m_javaNameExitReason, value.toString());
                }

                if (value == null) {
                    // otherwise, check if student graduated and use "3" if so
                    String exitEnrollmentCode = spEntity.getExitEnrollmentCode();
                    if (exitEnrollmentCode != null && m_graduatedCodes.contains(exitEnrollmentCode)) {
                        value = "3"; // set exit reason if the student already graduated
                    }
                }

            } else if (PARAM_INITIAL_REFER_DATE.equals(parameter)) {
                // sped initial referral date
                value = program.getFieldA018();
                if (value != null) {
                    value = DateUtils.getDate(value.toString());
                }
            } else if (PARAM_INITIAL_ELIG_DATE.equals(parameter)) {
                // sped initial eligibility date
                value = program.getFieldA019();
                if (value != null) {
                    value = DateUtils.getDate(value.toString());
                }
            } else if (PARAM_LAST_REVIEW_DATE.equals(parameter)) {
                // sped last iep review date
                value = program.getFieldA011();
                if (value != null) {
                    value = DateUtils.getDate(value.toString());
                }
            } else if (PARAM_LAST_EVAL_DATE.equals(parameter)) {
                // sped last evaluation date
                value = program.getFieldA010();
                if (value != null) {
                    value = DateUtils.getDate(value.toString());
                }
            }

            return value;
        }
    }

    /**
     * Performs field validation for the qualification code and exit reason.
     */
    protected class ValidateProgram implements FieldValidator {

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
            WAStudentProgramIepEntity spEntity = (WAStudentProgramIepEntity) entity;
            StudentProgramParticipation program = spEntity.getRecentValidPgm();
            String parameter = (String) field.getParameter();
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);
            if (PARAM_EXIT_REASON.equals(parameter)) {
                if (StringUtils.isEmpty(value)) {
                    if (program.getEndDate() != null) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Exit Reason Required",
                                "An exit reason is required whenever the exit date is populated"));
                    }
                }
            }
            return errors;
        }
    }

    protected static final String ALIAS_EXIT_REASON = "DOE EXIT REASON";
    protected static final String ALIAS_DISABILITY = "DOE DISABILITY";
    protected static final String ALIAS_LRE_CODE = "DOE LRE CODE";
    protected static final String ALIAS_NON_PRIMARY = "DOE NON PRIMARY";
    protected static final String ALIAS_EXCLUDE_PGM = "DOE EXCLUDE PGM";
    /*
     * Constants: Parameters, Codes.
     */
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String CODE_SPED = "SPED";
    protected static final String PARAM_EXIT_DATE = "EXIT_DATE";
    protected static final String PARAM_EXIT_REASON = "EXIT_REASON";
    protected static final String PARAM_SCHOOL = "SCHOOL";
    protected static final String PARAM_LRE_CODE = "LRE_CODE";
    protected static final String PARAM_START_DATE = "START_DATE";

    /*
     * Instance variables.
     */
    protected Map<String, String> m_exitReasonCodesMap;
    protected String m_fieldNonPrimaryIndicator;
    protected String m_fieldExitReason;
    protected String m_fieldExcludePgm;
    protected StudentHistoryHelper m_helper;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected PlainDate m_reportDate;
    protected Map<String, Collection<StudentProgramParticipation>> m_stdPgmsMap;
    protected String m_javaNameExitReason;
    protected String m_javaNameDisability;
    protected String m_javaNameLreCode;
    protected Set<String> m_graduatedCodes;

    /**
     * Initialize the data module.
     * Initialize program map.
     * Set up field retrievers
     *
     * @throws X2BaseException exception
     * @see com.follett.fsc.core.k12.tools.stateexports.StateReportData#initialize(java.util.Map,
     *      com.follett.fsc.core.k12.business.X2Broker)
     */
    @Override
    protected void initialize() throws X2BaseException {
        initializeFields();
        m_reportDate = new PlainDate(OrganizationManager.getTimeZone(getOrganization()));
        m_graduatedCodes = getGraduatedCodes();
        m_javaNameDisability = translateAliasToJavaName(ALIAS_DISABILITY, true);
        m_javaNameExitReason = translateAliasToJavaName(ALIAS_EXIT_REASON, true);
        m_javaNameLreCode = translateAliasToJavaName(ALIAS_LRE_CODE, true);
        if (getSetupErrors().size() == 0) {
            // Setup helper class and set the query
            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
            setQuery(studentQuery);
            setEntityClass(WAStudentProgramIepEntity.class);

            // Create program selection criteria and setup std-oid -> iep programs map
            X2Criteria programCriteria = getProgramCriteria(m_helper.getStudentCriteria());
            BeanQuery query = new BeanQuery(StudentProgramParticipation.class, programCriteria);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
            m_stdPgmsMap =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 128);

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("PGM-INFO", new RetrieveProgram());
            super.addCalcs(calcs);

            HashMap validators = new HashMap<String, FieldValidator>();
            validators.put("PGM-VALIDATE", new ValidateProgram());
            super.addValidators(validators);
        }
    }

    /**
     * Get withdrawn codes for exit reason.
     *
     * @return Sets the
     */
    private Set<String> getGraduatedCodes() {
        Set<String> results = new HashSet<String>();

        Set<String> validGraduatingCodesSet = new HashSet<String>(Arrays.asList(new String[] {"G0"}));
        DataDictionaryField enrollmentCodeField =
                getDataDictionaryField(StudentEnrollment.class, StudentEnrollment.COL_ENROLLMENT_CODE);
        ReferenceTable enrollmentCodeRefTable = enrollmentCodeField.getReferenceTable();
        if (enrollmentCodeRefTable != null) {
            Map<String, ReferenceCode> codeMap = enrollmentCodeRefTable.getCodeMap();
            for (Entry<String, ReferenceCode> e : codeMap.entrySet()) {
                ReferenceCode code = e.getValue();
                String stateCode = code.getStateCode();
                if (stateCode != null && validGraduatingCodesSet.contains(stateCode)) {
                    results.add(e.getKey());
                }
            }
        } else {
            addSetupError("No enrollment codes", "Trying to get withdrawal codes.  Couldn't.");
        }

        return results;
    }

    /**
     * Returns the Student Programs Participation selection criteria.
     *
     * @param studentCriteria X2Criteria
     * @return X2Criteria
     */
    private X2Criteria getProgramCriteria(X2Criteria studentCriteria) {
        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, CODE_SPED);
            SubQuery query = new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, criteria);
            reportableCodes = getBroker().getSubQueryCollectionByQuery(query);
        }

        if (reportableCodes != null && reportableCodes.size() > 0) {
            // Student subquery for active students this year.
            SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

            criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, reportableCodes);
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);

            if (isSchoolContext()) {
                criteria.addEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.COL_SCHOOL_OID, getSchool().getOid());
            } else {
                criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_INACTIVE_INDICATOR, Boolean.TRUE);
                criteria.addNotEqualTo(StudentProgramParticipation.REL_STUDENT + PATH_DELIMITER +
                        Student.REL_SCHOOL + PATH_DELIMITER +
                        School.COL_ARCHIVE_INDICATOR, Boolean.TRUE);
            }

            if (m_fieldExcludePgm != null) {
                criteria.addNotEqualTo(m_fieldExcludePgm, BooleanAsStringConverter.TRUE);
            }

            applyInputCriteria(criteria, false, StudentProgramParticipation.REL_STUDENT);
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code, SPED");
        }

        return criteria;
    }

    /**
     * Loads field aliases.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_fieldExitReason = translateAliasToJavaName(ALIAS_EXIT_REASON, true);
        m_fieldNonPrimaryIndicator = translateAliasToJavaName(ALIAS_NON_PRIMARY, true);
        m_fieldExcludePgm = translateAliasToJavaName(ALIAS_EXCLUDE_PGM, false);

        // Lookup a reverse map of ELL exit reasons state code to user code.
        m_exitReasonCodesMap = new HashMap<String, String>();
        DataDictionaryField field = getDataDictionaryField(StudentProgramParticipation.class, m_fieldExitReason);
        if (field != null) {
            ReferenceTable refTable = field.getReferenceTable();
            if (refTable != null) {
                for (ReferenceCode code : refTable.getReferenceCodes()) {
                    if (!StringUtils.isEmpty(code.getStateCode())) {
                        m_exitReasonCodesMap.put(code.getStateCode(), code.getCode());
                    }
                }
            }
        }
    }
}
