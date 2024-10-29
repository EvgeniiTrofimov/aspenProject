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
import com.follett.fsc.core.k12.beans.School;
import com.follett.fsc.core.k12.beans.Student;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.IepDisability;
import com.x2dev.sis.model.beans.SisPreferenceConstants;
import com.x2dev.sis.model.beans.SisStudent;
import com.x2dev.sis.model.beans.StudentEnrollment;
import com.x2dev.sis.model.beans.StudentProgramParticipation;
import com.x2dev.sis.model.beans.Transcript;
import com.x2dev.sis.tools.stateexports.StudentEnrollmentSpan;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.math.BigDecimal;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Export Data Module for WA District Student.
 *
 * @author X2 Development Corporation
 */
public class WADistrictStudent extends StateReportData {
    /**
     * Entity class for WA District Student export.
     *
     */
    public static class WADistrictStudentEntity extends StateReportEntity {
        /**
         * Entity instance variables.
         */
        BigDecimal m_creditAttempted;
        BigDecimal m_creditEarned;
        List<DistrictEnrollmentSpan> m_districtSpans;
        WADistrictStudent m_sdData;

        boolean m_memberOnEntryDate;
        boolean m_memberOnWithdrawalDate;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public WADistrictStudentEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the m_creditAttempted record.
         *
         * @return BigDecimal
         */
        public BigDecimal getCreditAttempted() {
            return m_creditAttempted;
        }

        /**
         * Returns the m_creditEarned record.
         *
         * @return BigDecimal
         */
        public BigDecimal getCreditEarned() {
            return m_creditEarned;
        }

        /**
         * Returns the StudentEnrollmentSpan record for the current index.
         *
         * @return DistrictEnrollmentSpan
         */
        public DistrictEnrollmentSpan getEnrollmentSpan() {
            return m_districtSpans.get(getCurrentRow());
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
            DistrictEnrollmentSpan span = getEnrollmentSpan();
            if (span != null && span.m_entryEnrollment != null && span.m_entryEnrollment.getSchool() != null) {
                name += span.m_entryEnrollment.getSchool().getName();
            }

            return name;
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
         * @return List<StudentProgramParticipation>
         */
        public List<StudentProgramParticipation> getSpedPrograms() {
            List<StudentProgramParticipation> pgms =
                    ((WADistrictStudent) getData()).m_spedPrograms.get(getBean().getOid());
            List<StudentProgramParticipation> validPgms = new ArrayList<StudentProgramParticipation>();

            if (pgms != null && pgms.size() > 0) {
                for (StudentProgramParticipation pgm : pgms) {
                    String pgmExitReason = (String) pgm.getFieldValueByAlias(ALIAS_EXIT_REASON);
                    String pgmDisability = (String) pgm.getFieldValueByAlias(ALIAS_DISABILITY);

                    if (!(PGM_INELIGIBLE_REASON.equals(pgmExitReason) && PGM_DISABILITY_CODE.equals(pgmDisability))) {
                        validPgms.add(pgm);
                    }
                }
            }

            return validPgms;
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

            m_sdData = (WADistrictStudent) data;
            loadCredits(bean);

            m_memberOnEntryDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_sdData.getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_ENTRY_DATE)).booleanValue();
            m_memberOnWithdrawalDate = Boolean.valueOf(PreferenceManager.getPreferenceValue(m_sdData.getOrganization(),
                    SisPreferenceConstants.STUDENT_MEMBER_ON_WITHDRAWAL_DATE)).booleanValue();

            // Calculate district enrollment spans from individual enrollment spans.
            m_districtSpans = getDistrictSpans((Student) bean);

            setRowCount(m_districtSpans.size());
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
         * Adds the current district span to the district spans if the conditions are met.
         *
         * @param districtSpans All the distrit spans
         * @param currentDistrictSpan The current district span
         */
        private void addDistrictSpan(List<DistrictEnrollmentSpan> districtSpans,
                                     DistrictEnrollmentSpan currentDistrictSpan) {
            if (currentDistrictSpan != null &&
                    (currentDistrictSpan.m_exitDate == null ||
                            !m_sdData.getCurrentContext().getStartDate().after(currentDistrictSpan.m_exitDate))) {
                districtSpans.add(currentDistrictSpan);
            }
        }

        /**
         * Calculate district enrollment spans from the student school enrollment spans.
         * Look for spans with withdrawal codes that represent district withdrawal vs. indistrict
         * transfer.
         *
         * @param student The student
         *
         * @return List<DistrictEnrollmentSpan> The district enrollment spans
         */
        private List<DistrictEnrollmentSpan> getDistrictSpans(Student student) {
            List<StudentEnrollmentSpan> enrollmentSpans = m_sdData.m_helper.getStudentEnrollmentSpans(student, false);
            List<DistrictEnrollmentSpan> districtSpans = new ArrayList<DistrictEnrollmentSpan>();

            DistrictEnrollmentSpan currentDistrictSpan = getCurrentDistrictSpan(enrollmentSpans, districtSpans);

            addDistrictSpan(districtSpans, currentDistrictSpan);

            return districtSpans;
        }

        /**
         *
         * Returns the current district span.
         *
         * @param enrollmentSpans The enrollment spans
         * @param districtSpans The district spans
         *
         * @return DistrictEnrollmentSpan The current district span
         */
        private DistrictEnrollmentSpan getCurrentDistrictSpan(List<StudentEnrollmentSpan> enrollmentSpans,
                                                              List<DistrictEnrollmentSpan> districtSpans) {
            DistrictEnrollmentSpan currentDistrictSpan = null;

            for (StudentEnrollmentSpan enrollmentSpan : enrollmentSpans) {
                if (!isExcludedSchool(enrollmentSpan) && !isNoShow(enrollmentSpan) && isMember(enrollmentSpan)) {
                    StudentEnrollment enrollment = enrollmentSpan.getFirstActiveEnrollment();

                    // Check the span for entry type (internal or external)
                    if (enrollment != null && m_sdData.includeSchool(enrollment.getSchool().getSchoolId())) {
                        String code = enrollment.getEnrollmentCode();
                        String stateCode = m_sdData.lookupReferenceCodeByRefTbl(m_sdData.m_refTableEnrollmentCode, code,
                                ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                        if (CODE_1.equals(stateCode) || CODE_2.equals(stateCode)) {
                            addDistrictSpan(districtSpans, currentDistrictSpan);

                            currentDistrictSpan = resetDistrictSpan(enrollment);

                            currentDistrictSpan.m_exitDate = getExitDate(enrollmentSpan);
                        } else {
                            if (currentDistrictSpan == null) {
                                currentDistrictSpan = resetDistrictSpan(enrollment);
                            }

                            currentDistrictSpan.m_exitDate = getExitDate(enrollmentSpan);
                        }
                    }
                }
            }

            return currentDistrictSpan;
        }

        /**
         * Resets the district span.
         *
         * @param enrollment The enrollment record
         * @return DistrictEnrollmentSpan The district enrollment span
         */
        private DistrictEnrollmentSpan resetDistrictSpan(StudentEnrollment enrollment) {
            DistrictEnrollmentSpan districtSpan;
            districtSpan = new DistrictEnrollmentSpan();
            districtSpan.m_entryEnrollment = enrollment;

            return districtSpan;
        }

        /**
         * Returns the exit date.
         *
         * @param enrollmentSpan The span
         * @return PlainDate The exit date
         */
        private PlainDate getExitDate(StudentEnrollmentSpan enrollmentSpan) {
            PlainDate exitDate = null;

            StudentEnrollment enrollment = enrollmentSpan.getFirstInactiveEnrollment();

            if (enrollment != null) {
                exitDate = enrollment.getEnrollmentDate();
            } else {
                exitDate = enrollmentSpan.getLastActiveDate();
            }

            return exitDate;
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
                if (!m_memberOnEntryDate && !m_memberOnWithdrawalDate) {
                    isMember = false;
                } else if (m_memberOnEntryDate != m_memberOnWithdrawalDate) {
                    isMember = false;
                }
            }

            return isMember;
        }

        /**
         * Returns true if any enrollment spans have a no-show withdrawal code which
         * are identified by having NS in the local code of the withdrawal record.
         *
         * @param span StudentEnrollmentSpan
         * @return true, if is no show
         */
        private boolean isNoShow(StudentEnrollmentSpan span) {
            boolean isNoShow = false;

            StudentEnrollment enrollment = span.getFirstInactiveEnrollment();
            if (enrollment != null) {
                String withdrawalCode = enrollment.getEnrollmentCode();
                withdrawalCode = getData().lookupReferenceCodeByRefTbl(m_sdData.m_refTableWithdrawalCode,
                        withdrawalCode, ExportFormatField.ReferenceMapTypeCode.LOCAL.ordinal());
                if (CODE_NO_SHOW.equals(withdrawalCode)) {
                    isNoShow = true;
                }
            }

            return isNoShow;
        }

        /**
         * Returns true of the span related to an excluded school.
         *
         * @param span The span
         * @return boolean True if the span should be excluded
         */
        private boolean isExcludedSchool(StudentEnrollmentSpan span) {
            boolean isExcluded = false;

            StudentEnrollment enrollment = span.getFirstActiveEnrollment();
            if (enrollment != null) {
                if (!m_sdData.includeSchool(enrollment.getSchoolOid())) {
                    isExcluded = true;
                }
            }

            return isExcluded;
        }

        /**
         *
         * Loads the attempted and earned credits to memory for the given bean.
         *
         * @param bean The bean
         */
        private void loadCredits(X2BaseBean bean) {
            double ttlCreditAttempted = 0;
            double ttlCreditEarned = 0;

            List<Transcript> transcripts = m_sdData.m_helper.getStudentTranscripts((SisStudent) bean);
            if (transcripts != null) {
                for (Transcript transcript : transcripts) {
                    BigDecimal credit = null;
                    BigDecimal creditEarned = transcript.getTotalCredit();

                    // get Potential Credit
                    if (!StringUtils.isEmpty(transcript.getPotentialCredit())
                            && StringUtils.isNumeric(transcript.getPotentialCredit())) {
                        try {
                            credit = new BigDecimal(transcript.getPotentialCredit());
                        } catch (NumberFormatException nfe) {
                            // nothing. The grade is not numeric.
                        }
                    }

                    if (credit == null) {
                        if (transcript.getSchoolCourse() != null) {
                            credit = transcript.getSchoolCourse().getCredit();
                        } else {
                            credit = BigDecimal.valueOf(0.0d);
                        }
                    }

                    // Credit earned cannot be greater than credit attempted.
                    if (creditEarned != null && (credit == null || creditEarned.compareTo(credit) > 0)) {
                        creditEarned = credit;
                    }

                    if (credit != null) {
                        ttlCreditAttempted = ttlCreditAttempted + credit.doubleValue();
                    }
                    if (creditEarned != null) {
                        ttlCreditEarned = ttlCreditEarned + creditEarned.doubleValue();
                    }
                }
            }

            m_creditAttempted = new BigDecimal(ttlCreditAttempted);
            m_creditEarned = new BigDecimal(ttlCreditEarned);
        }
    }

    /**
     * A district span, similar to the student enrollment span, but covering all activity in a
     * district.
     * This will encompass one or more enrollment spans.
     */
    // TODO rename to something that makes sense
    protected static class DistrictEnrollmentSpan {
        StudentEnrollment m_entryEnrollment;
        PlainDate m_exitDate;
    }

    /*
     * Aliases for fields to look up.
     */
    protected static final String ALIAS_BIRTH_COUNTRY = "DOE BIRTH COUNTRY";
    protected static final String ALIAS_DISABILITY = "DOE DISABILITY";
    protected static final String ALIAS_DISTRICT_HOME = "DOE DISTRICT HOME";
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EXCLUDE_SCHOOL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_EXIT_REASON = "DOE EXIT REASON";
    protected static final String ALIAS_HOME_SCHOOLED = "DOE HOME SCHOOLED CODE";
    protected static final String ALIAS_IS_HOMELESS = "DOE IS HOMELESS";
    protected static final String ALIAS_INITIAL_USA_DATE = "DOE INITIAL USA DATE";
    protected static final String ALIAS_NUMMONNONUSATTEND = "DOE NUMMONNONUSATTEND";
    protected static final String ALIAS_NUMMONUSATTEND = "DOE NUMMONUSATTEND";
    protected static final String ALIAS_STD_MILITARY_IND = "DOE FAMILY MILITARY STATUS";
    protected static final String ALIAS_PRIVATE_SCHOOLED = "DOE PRIVATE SCHOOLED CODE";


    /*
     * Code values
     */
    protected static final String CODE_NEW_TO_DISTRICT = "1";
    protected static final String CODE_NO_SHOW = "NS";
    protected static final String CODE_RETURN_TO_DISTRICT = "2";

    /*
     * Export format field names
     */
    protected static final String FIELD_BIRTH_COUNTRY = "Birth Country";
    protected static final String FIELD_GRADE_LEVEL = "Grade Level";

    /*
     * User input parameters
     */
    protected static final String PARAM_BELLEVUE_DISTRICT_START_DATE = "bellevueFirstDayFix";
    protected static final String PARAM_EXCLUDE_SCHOOL = "excludeSchool";
    protected static final String PARAM_REPORT_DATE = "reportDate";



    /*
     * Student Program local codes.
     */
    // protected static final String PGM_FARMS = "FRM";
    // protected static final String PGM_ELL = "ELL";
    protected static final String PGM_FOSTER = "FST";
    protected static final String PGM_HOMELESS = "HML";
    // protected static final String PGM_504 = "504";
    protected static final String PGM_SPED = "SPED";
    // protected static final String PGM_TITLE1 = "T1";
    protected static final String PGM_IMMIGRANT = "T3I";

    /*
     * Other constants
     */
    private static final String CODE_1 = "1";
    private static final String CODE_2 = "2";
    private static final String PGM_DISABILITY_CODE = "00";
    private static final String PGM_INELIGIBLE_REASON = "Ineligible";

    /*
     * Instance variables.
     * These are protected rather than private so they can be accessed by the inner classes.
     */
    protected String m_fieldBirthCountry;
    protected String m_fieldDisabilityCode;
    protected String m_fieldDistrictHome;
    protected String m_fieldDistrictId;
    protected String m_fieldHomelessStatus;
    protected String m_fieldHomeSchooled;
    protected String m_fieldMilitaryStatus;
    protected String m_fieldMonthsNonUS;
    protected String m_fieldMonthsUS;
    protected String m_fieldPrivateSchooled;
    protected String m_fieldUSEntryDate;
    protected StudentHistoryHelper m_helper;
    protected Map<String, Collection<String>> m_programCodeMap;
    protected Map<String, List<StudentProgramParticipation>> m_programs;
    protected PlainDate m_reportDate;
    protected PlainDate m_districtStartOverrideDate;
    protected String m_refTableEnrollmentCode;
    protected String m_refTableWithdrawalCode;
    protected Map<String, List<StudentProgramParticipation>> m_spedPrograms;
    protected String m_excludeSchool;
    protected Map m_excludeSchoolMap;

    /**
     * This retriever will clean up a string by removing invalid characters.
     * It accepts a regular expression as a parameter that identifies characters or patterns to
     * remove.
     */
    protected class RetrieveClean implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String param = (String) field.getParameter();
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());

            if (!StringUtils.isEmpty(param) && !StringUtils.isEmpty(value)) {
                value = value.replace(param, "");
            }
            return value;
        }
    }

    /**
     * Retrieve the students transcript credits from the transcript table.
     */
    protected class RetrieveCredit implements FieldRetriever {
        private final String PARAM_ATTEMPTED = "ATTEMPTED";
        private final String PARAM_EARNED = "EARNED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String parameter = (String) field.getParameter();
            Object value = null;

            if (PARAM_ATTEMPTED.equals(parameter)) {
                value = ((WADistrictStudentEntity) entity).getCreditAttempted();
            } else if (PARAM_EARNED.equals(parameter)) {
                value = ((WADistrictStudentEntity) entity).getCreditEarned();
            }
            return value;
        }
    }

    /**
     * Retrieve the students disability from the student disability table.
     */
    protected class RetrieveDisability implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String disabilityCode = null;
            SisStudent student = (SisStudent) entity.getBean();
            Collection<IepDisability> iepDisabilities = student.getIepDisability();
            Object value = null;

            if (iepDisabilities.size() > 0) {
                for (IepDisability disability : iepDisabilities) {
                    if (disability.getPrimaryIndicator() == true) {
                        disabilityCode = disability.getDisabilityCode();
                        break;
                    }
                    if (disabilityCode == null) {
                        disabilityCode = disability.getDisabilityCode();
                    }
                }
            }

            value = lookupReferenceCodeByBeanPath(IepDisability.class, IepDisability.COL_DISABILITY_CODE,
                    disabilityCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

            return value;
        }
    }

    /**
     * Retrieve the students enrollment information from their enrollment record.
     */
    protected class RetrieveEnrollment implements FieldRetriever {
        private final String PARAM_DIST_CODE = "DIST_CODE";
        private final String PARAM_ENTRY_DATE = "ENTRY_DATE";
        private final String PARAM_EXIT_DATE = "EXIT_DATE";
        private final String PARAM_HOME_SCHOOLED = "HOME_SCHOOLED";
        private final String PARAM_PRIVATE_SCHOOLED = "PRIVATE_SCHOOLED";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String districtCode = null;
            String isHomeSchooled = null;
            String isPrivateSchooled = null;
            String parameter = (String) field.getParameter();
            DistrictEnrollmentSpan span = ((WADistrictStudentEntity) entity).getEnrollmentSpan();
            Object value = null;

            if (span != null) {
                if (PARAM_DIST_CODE.equals(parameter)) {
                    districtCode = (String) span.m_entryEnrollment.getFieldValueByBeanPath(m_fieldDistrictHome);
                    if (!StringUtils.isEmpty(districtCode)) {
                        districtCode = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldDistrictHome,
                                districtCode, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    }
                    if (StringUtils.isEmpty(districtCode)) {
                        districtCode = (String) data.getOrganization().getFieldValueByBeanPath(m_fieldDistrictId);
                    }
                    value = districtCode;
                } else if (PARAM_ENTRY_DATE.equals(parameter)) {
                    StudentEnrollment enrollment = span.m_entryEnrollment;
                    if (enrollment != null &&
                            (m_districtStartOverrideDate == null ||
                                    !m_districtStartOverrideDate.equals(enrollment.getEnrollmentDate()))) {
                        value = enrollment.getEnrollmentDate();
                    }
                } else if (PARAM_EXIT_DATE.equals(parameter)) {
                    value = span.m_exitDate;
                } else if (PARAM_HOME_SCHOOLED.equals(parameter)) {
                    isHomeSchooled = (String) span.m_entryEnrollment.getFieldValueByBeanPath(m_fieldHomeSchooled);
                    value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldHomeSchooled,
                            isHomeSchooled, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                } else if (PARAM_PRIVATE_SCHOOLED.equals(parameter)) {
                    isPrivateSchooled = (String) span.m_entryEnrollment.getFieldValueByBeanPath(m_fieldPrivateSchooled);
                    value = data.lookupReferenceCodeByBeanPath(StudentEnrollment.class, m_fieldPrivateSchooled,
                            isPrivateSchooled, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                }
            }
            return value;
        }
    }

    /**
     * Retrieve student's military status.
     */
    protected class RetrieveMilitaryInd implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            Object value = "X";
            WADistrictStudent dsData = (WADistrictStudent) data;
            SisStudent std = (SisStudent) entity.getBean();
            String localCode = (String) std.getFieldValueByBeanPath(dsData.m_fieldMilitaryStatus);

            if (!StringUtils.isEmpty(localCode)) {
                value = lookupReferenceCodeByBeanPath(SisStudent.class, dsData.m_fieldMilitaryStatus, localCode,
                        ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
            }
            return value;
        }
    }

    /**
     * Retrieve program related values from the student programs.
     */
    protected class RetrieveProgram implements FieldRetriever {
        private static final String PARAM_DISABILITY = "DISABILITY";
        private static final String PARAM_FOSTER = "FOSTER";
        private static final String PARAM_HOMELESS = "HOMELESS";
        private static final String PARAM_IMMIGRANT = "IMMIGRANT";
        private static final String PARAM_IMM_1ST_US = "IMM_1ST_US";
        private static final String PARAM_IMM_MON_US = "IMM_MON_US";
        private static final String PARAM_IMM_MON_NON_US = "IMM_MON_NON_US";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String parameter = (String) field.getParameter();
            WADistrictStudent sdData = (WADistrictStudent) data;
            Object value = null;

            if (PARAM_FOSTER.equals(parameter)) {
                // A FOSTER program that covers report date.
                value = null;
                List<StudentProgramParticipation> programs = getPrograms(PGM_FOSTER, entity.getBean().getOid(), sdData);
                for (StudentProgramParticipation program : programs) {
                    if (program.getEndDate() == null ||
                            !program.getEndDate().before(m_reportDate)) {
                        value = Boolean.TRUE;
                        break;
                    }
                }
            } else if (PARAM_IMMIGRANT.equals(parameter) ||
                    PARAM_IMM_1ST_US.equals(parameter) ||
                    PARAM_IMM_MON_US.equals(parameter) ||
                    PARAM_IMM_MON_NON_US.equals(parameter)) {
                // An IMMIGRANT program that covers report date.
                value = null;
                List<StudentProgramParticipation> programs =
                        getPrograms(PGM_IMMIGRANT, entity.getBean().getOid(), sdData);
                for (StudentProgramParticipation program : programs) {
                    if (program.getEndDate() == null ||
                            !program.getEndDate().before(m_reportDate)) {
                        if (PARAM_IMMIGRANT.equals(parameter)) {
                            // 14-15 changes: no longer needed.
                            // value = Boolean.TRUE;
                        } else if (PARAM_IMM_1ST_US.equals(parameter)) {
                            value = sdData.getPropertyAsJavaType(program, sdData.m_fieldUSEntryDate);
                        } else if (PARAM_IMM_MON_US.equals(parameter)) {
                            // 14-15 changes: no longer needed.
                            // value = sdData.getPropertyAsJavaType(program,
                            // sdData.m_fieldMonthsUS);
                        } else if (PARAM_IMM_MON_NON_US.equals(parameter)) {
                            value = sdData.getPropertyAsJavaType(program, sdData.m_fieldMonthsNonUS);
                        }
                        break;
                    }
                }
            } else if (PARAM_HOMELESS.equals(parameter)) {
                // First HOMELESS program in the school year.
                value = null;
                List<StudentProgramParticipation> programs =
                        getPrograms(PGM_HOMELESS, entity.getBean().getOid(), sdData);
                if (programs != null && programs.size() > 0) {
                    StudentProgramParticipation program = programs.get(0);
                    String homelessCode = (String) program.getFieldValueByBeanPath(sdData.m_fieldHomelessStatus);
                    homelessCode = sdData.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                            sdData.m_fieldHomelessStatus, homelessCode,
                            ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                    value = homelessCode;
                }
            } else if (PARAM_DISABILITY.equals(parameter)) {
                // Active SPED program.
                value = null;
                // 14-15 changes: no longer needed.
                // WADistrictStudentEntity sdEntity = (WADistrictStudentEntity) entity;
                // List<StudentProgramParticipation> programs = sdEntity.getSpedPrograms();
                //
                // if (programs.size() > 0)
                // {
                // StudentProgramParticipation program = programs.get(0);
                // String disabilityCode = (String)
                // program.getFieldValueByBeanPath(sdData.m_fieldDisabilityCode);
                // disabilityCode =
                // sdData.lookupReferenceCodeByBeanPath(StudentProgramParticipation.class,
                // sdData.m_fieldDisabilityCode, disabilityCode,
                // ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());
                // value = disabilityCode;
                // }
            }

            return value;
        }

        /**
         * Return a list of StudentProgramParticipation for the student that have the local code
         * provided.
         *
         * @param localCode String
         * @param studentOid String
         * @param data WADistrictStudent
         * @return List<StudentProgramParticipation>
         */
        private List<StudentProgramParticipation> getPrograms(String localCode,
                                                              String studentOid,
                                                              WADistrictStudent data) {
            List<StudentProgramParticipation> localPrograms = new ArrayList<StudentProgramParticipation>();
            Collection<String> programCodes = data.m_programCodeMap.get(localCode);
            List<StudentProgramParticipation> programs = data.m_programs.get(studentOid);
            if (programs != null && programCodes != null) {
                for (StudentProgramParticipation program : programs) {
                    if (programCodes.contains(program.getProgramCode())) {
                        localPrograms.add(program);
                    }
                }
            }

            return localPrograms;
        }
    }

    /**
     * Retrieves the Graduation requirements Year of Graduation or Expected Year of Graduation.
     * <p>
     * Graduation requirements Year of Graduation field (in field.getBeanPath()).
     * If the field provided is empty, and the student is grade 9 or higher,
     * then retrieve the students YOG instead. This is indicated by hsOnly = true.
     * <p>
     * Expected Year of Graduation field (in field.getBeanPath()).
     * If the field provided is empty then retrieve the students YOG instead.
     * This is indicated by hsOnly = false.
     */
    protected class RetrieveYog implements FieldRetriever {
        private List<String> m_hsGrades = Arrays.asList("09", "10", "11", "12");
        private boolean m_hsOnly = false;

        /**
         * Constructor
         * Set the High School only indicator.
         * Grad requirements only needs to report for high school.
         * Expected grad year can apply to any grade level.
         *
         * @param hsOnly boolean
         */
        protected RetrieveYog(boolean hsOnly) {
            m_hsOnly = hsOnly;
        }

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String gradeLevel = entity.getFieldValue(FIELD_GRADE_LEVEL);
            String value = (String) data.getProperty(entity.getBean(), field.getBeanPath());
            if (value != null) {
                value = value.trim();
            }
            if (StringUtils.isEmpty(value) && (!m_hsOnly || m_hsGrades.contains(gradeLevel))) {
                SisStudent student = (SisStudent) entity.getBean();
                value = Integer.toString(student.getYog());
            }

            return value;
        }
    }

    /**
     * Validate the Credits Attempted.
     */
    protected class ValidateCreditsAttempted implements FieldValidator {
        private static final String FIELD_CREDITS_EARNED = "Credits Earned";
        private static final String CALC_ID = "CREDITS-ATT-VAL";

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

            if (!StringUtils.isEmpty(value) && StringUtils.isNumeric(value)) {
                double doubleValue = Double.parseDouble(value);
                String creditEarned = entity.getFieldValue(FIELD_CREDITS_EARNED);
                if (!StringUtils.isEmpty(creditEarned) && StringUtils.isNumeric(creditEarned)) {
                    double credEarned = Double.parseDouble(creditEarned);
                    if (doubleValue < credEarned) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " must be equal to or greater than the Credits Earned",
                                field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                        "; Credits Earned = " + STYLE_BOLD + creditEarned + STYLE_END));
                    }
                }
            }
            return errors;
        }
    }


    /**
     * Validate the graduation requirements year is set for any student in grade 9-12.
     */
    protected class ValidateGraduationRequirementsYear implements FieldValidator {

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

            if (StringUtils.isEmpty(value)) {
                String grade = entity.getFieldValue(FIELD_GRADE_LEVEL);
                if (StringUtils.isNumeric(grade)) {
                    int gradeNum = Integer.parseInt(grade);
                    if (gradeNum >= 9 && gradeNum <= 12) {
                        errors.add(new StateReportValidationError(entity, field,
                                field.getFieldId() + " is required for grades 9-12",
                                "grade = " + STYLE_BOLD + grade + STYLE_END));
                    }
                }
            }
            return errors;
        }
    }


    /**
     * Validate the Initial USA Place Date.
     */
    protected class ValidateUSAPlace implements FieldValidator {
        private static final String CALC_ID = "US-DATE-VAL";
        private static final String VALUE_USA = "USA";

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

            if (StringUtils.isEmpty(value)) {
                String bearThCountry = entity.getFieldValue(FIELD_BIRTH_COUNTRY);
                if (!StringUtils.isEmpty(bearThCountry) && !bearThCountry.equals(VALUE_USA)) {

                    errors.add(new StateReportValidationError(entity, field,
                            field.getFieldId() + " if Birth Country is other than USA, information is required",
                            field.getFieldId() + " = " + STYLE_BOLD + value + STYLE_END +
                                    "; Birth Country = " + STYLE_BOLD + bearThCountry + STYLE_END));

                }
            }
            return errors;
        }
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
        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);
        initializeFields();
        if (((Boolean) getParameter(PARAM_EXCLUDE_SCHOOL)).booleanValue()) {
            loadSchoolExcludeMap();
        }
        /*
         * Build helper object.
         */
        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_LOAD_ALL_TRANSCRIPT, Boolean.TRUE);

        if (getParameter(PARAM_BELLEVUE_DISTRICT_START_DATE) != null) {
            m_districtStartOverrideDate = (PlainDate) Date.valueOf("2013-09-03");
        }

        /*
         * If no errors so far, continue with query.
         */
        if (getSetupErrors().size() == 0) {
            // Set the query to be used for student selection.
            setQuery(m_helper.getStudentQuery(false));

            setEntityClass(WADistrictStudentEntity.class);

            loadPrograms();
            loadSpedPrograms(m_helper.getStudentCriteria());

            // Build a map of calculations/retrievers
            HashMap calcs = new HashMap<String, FieldRetriever>();
            calcs.put("DS-CLEAN", new RetrieveClean());
            calcs.put("DS-CREDIT", new RetrieveCredit());
            calcs.put("DS-DISABILITY", new RetrieveDisability());
            calcs.put("DS-ENROLLMENT", new RetrieveEnrollment());
            calcs.put("DS-PROGRAM", new RetrieveProgram());
            calcs.put("DS-GRADYOG", new RetrieveYog(true));
            calcs.put("DS-EXPYOG", new RetrieveYog(false));
            calcs.put("DS-MILITARY", new RetrieveMilitaryInd());
            super.addCalcs(calcs);

            // Build a map of validators
            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("DS-GRADREQYR-VAL", new ValidateGraduationRequirementsYear());
            validators.put(ValidateUSAPlace.CALC_ID, new ValidateUSAPlace());
            validators.put(ValidateCreditsAttempted.CALC_ID, new ValidateCreditsAttempted());
            super.addValidators(validators);
        }
    }

    /**
     * Lookup field aliases and paths.
     */
    private void initializeFields() {
        m_refTableEnrollmentCode =
                PreferenceManager.getPreferenceValue(getOrganization(), SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        m_refTableWithdrawalCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_WITHDRAWAL_CODES);

        m_fieldDistrictId = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldDistrictHome = translateAliasToJavaName(ALIAS_DISTRICT_HOME, true);
        m_fieldBirthCountry = translateAliasToJavaName(ALIAS_BIRTH_COUNTRY, true);
        m_fieldMilitaryStatus = translateAliasToJavaName(ALIAS_STD_MILITARY_IND, true);

        // Enrollment aliases.
        m_fieldPrivateSchooled = translateAliasToJavaName(ALIAS_PRIVATE_SCHOOLED, true);
        m_fieldHomeSchooled = translateAliasToJavaName(ALIAS_HOME_SCHOOLED, true);
        // Program aliases.
        m_fieldDisabilityCode = translateAliasToJavaName(ALIAS_DISABILITY, true);
        m_fieldHomelessStatus = translateAliasToJavaName(ALIAS_IS_HOMELESS, true);
        m_fieldUSEntryDate = translateAliasToJavaName(ALIAS_INITIAL_USA_DATE, true);
        m_fieldMonthsNonUS = translateAliasToJavaName(ALIAS_NUMMONNONUSATTEND, true);
        m_fieldMonthsUS = translateAliasToJavaName(ALIAS_NUMMONUSATTEND, true);
        m_excludeSchool = translateAliasToJavaName(ALIAS_EXCLUDE_SCHOOL, true);
    }

    /**
     * Load a map by studentOid of student program participation records that contain a specific set
     * of codes.
     * Programs must be one of the known codes for ELL, FARMS, FOSTER, HOMELESS, 504, SPED,
     * IMMIGRANT
     */
    private void loadPrograms() {
        // Get program codes that are relevant to this export, map by local code.
        List<String> programLocalCodes = Arrays.asList(PGM_FOSTER, PGM_HOMELESS, PGM_IMMIGRANT); // (PGM_FARMS,
                                                                                                 // PGM_SPED,
                                                                                                 // PGM_ELL,
                                                                                                 // PGM_504);
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        X2Criteria criteria = new X2Criteria();
        criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
        criteria.addIn(ReferenceCode.COL_LOCAL_CODE, programLocalCodes);
        QueryByCriteria reportQuery = new QueryByCriteria(ReferenceCode.class, criteria);
        Map<String, Collection<ReferenceCode>> programRefCodeMap =
                getBroker().getGroupedCollectionByQuery(reportQuery, ReferenceCode.COL_LOCAL_CODE, 10);

        // Get collections of reference codes by local code group.
        // Get all reference codes into one list.
        m_programCodeMap = new HashMap<String, Collection<String>>();
        List<String> refCodes = new ArrayList<String>();
        for (String programCode : programRefCodeMap.keySet()) {
            Collection<String> programCodes = new ArrayList<String>();
            m_programCodeMap.put(programCode, programCodes);
            Collection<ReferenceCode> refLocalCodes = programRefCodeMap.get(programCode);
            for (ReferenceCode code : refLocalCodes) {
                programCodes.add(code.getCode());
            }
            refCodes.addAll(programCodes);
        }

        // Load student subquery to identify students reporting.
        X2Criteria stdCriteria = m_helper.getStudentCriteria();
        SubQuery studentSubquery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, stdCriteria);

        // Load program records for reporting students and programs into a map.
        if (refCodes != null && refCodes.size() > 0) {
            criteria = new X2Criteria();
            criteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubquery);
            criteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, refCodes);
            criteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getCurrentContext().getStartDate());
            criteria2.addOrCriteria(criteria3);
            criteria.addAndCriteria(criteria2);
            BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
            query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
            query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
            m_programs =
                    getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 1024);
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code: HML, SPED");
        }
    }

    /**
     * Returns the Student Programs Participation selection criteria.
     *
     * @param studentCriteria X2Criteria
     * @return X2Criteria
     */
    private void loadSpedPrograms(X2Criteria studentCriteria) {
        X2Criteria criteria = null;
        DataDictionaryField field =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        String referenceTableOid = field.getReferenceTableOid();
        Collection<String> reportableCodes = null;
        if (!StringUtils.isEmpty(referenceTableOid)) {
            criteria = new X2Criteria();
            criteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            criteria.addEqualTo(ReferenceCode.COL_LOCAL_CODE, PGM_SPED);
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

            X2Criteria criteria2 = new X2Criteria();
            X2Criteria criteria3 = new X2Criteria();
            criteria2.addIsNull(StudentProgramParticipation.COL_END_DATE);
            criteria3.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE,
                    getCurrentContext().getStartDate());
            criteria2.addOrCriteria(criteria3);
            criteria.addAndCriteria(criteria2);

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

            applyInputCriteria(criteria, false, StudentProgramParticipation.REL_STUDENT);
        } else {
            addSetupError("No reportable program codes", "Student Programs, reference table, local code, SPED");
        }

        BeanQuery query = new BeanQuery(StudentProgramParticipation.class, criteria);
        query.addOrderBy(StudentProgramParticipation.COL_STUDENT_OID, true);
        query.addOrderBy(StudentProgramParticipation.COL_START_DATE, false);
        m_spedPrograms =
                getBroker().getGroupedCollectionByQuery(query, StudentProgramParticipation.COL_STUDENT_OID, 128);
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
