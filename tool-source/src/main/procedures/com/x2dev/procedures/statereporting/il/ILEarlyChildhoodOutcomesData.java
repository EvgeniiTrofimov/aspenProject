/*
 * ====================================================================
 *
 * Follett Software Company
 *
 * Copyright (c) 2017 Follett Software Company
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without a written agreement
 * from Follett Software Company.
 *
 * ====================================================================
 */
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.Race;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentContact;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.SystemPreferenceDefinition;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelBroker;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.PreferenceManager;
import com.follett.fsc.core.k12.business.StudentManager;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryUtils;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.follett.fsc.core.k12.web.WebUtils;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.sis.tools.stateexports.StudentScheduleSpan;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for Student Demographic/Enrollment.
 *
 * @author X2 Development Corporation
 */
public class ILEarlyChildhoodOutcomesData extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class StudentDemographicEntity extends StateReportEntity {
        /**
         * The FTE of each school The index is the same as that of secondary school.
         * The 0 index is for primary school. Higher indexes corespond to the secodnary schools if
         * any.
         */
        List<Float> m_fte = null;
        List<String> m_schools = null;

        /**
         * The effective Entry student enrollment record for report date.
         */
        StudentEnrollment m_enrollment = null;

        /**
         * All the "Free/Reduced Lunch" programs for this entry by descending start date order
         */
        List<StudentProgramParticipation> m_programs = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Indicator the student has an outplacement service school.
         */
        private boolean m_hasServiceSchool = false;

        /**
         * Ratings record for student defined by UDA
         */
        private Map<String, UserDefinedTableA> m_ratingsMap = new HashMap<>();

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public StudentDemographicEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * Returns the FTE for the current school.
         *
         * @return Float
         */
        public Float getCurrentFte() {
            Float fte = null;
            if ((m_fte != null) && (m_fte.size() > getCurrentRow())) {
                fte = m_fte.get(getCurrentRow());
            }
            return fte;
        }

        /**
         * Returns the student school object for the current row.
         * The current row would be null for current row == 0 (representing the primary school),
         * and one of the student school list schools for the remaining rows > 0, if any.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if ((m_schools != null) && (m_schools.size() >= getCurrentRow())) {
                school = m_schools.get(getCurrentRow());
            }
            return school;
        }

        /**
         * Initialize and increment counter
         *
         * If there is no recent entry enrollment record, ignore it.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);

            ILEarlyChildhoodOutcomesData sdData = (ILEarlyChildhoodOutcomesData) data;
            SisStudent student = (SisStudent) bean;

            // Do not report students not active on report date.
            m_enrollment = sdData.m_helper.getEnrollmentForDate(student.getOid(), sdData.m_reportDate, "EYSW");

            boolean includeStudent = false;
            // Based in "Report Date" the export shall include only student in Grade Level "14", if
            // it's childhood export.
            if (sdData.m_isChildhood.booleanValue()) {
                int yog = 0;
                if (m_enrollment != null) {
                    yog = m_enrollment.getYog();
                    if (yog == 0) {
                        yog = student.getYog();
                    }
                }

                includeStudent = sdData.getGradeLevel(student, yog) != null &&
                        "14".equals(sdData.getGradeLevel(student, yog).getStateCode());
            } else {
                includeStudent = true;
            }

            if ((m_enrollment == null) || !sdData.m_activeCode.equals(m_enrollment.getStatusCode()) ||
                    !includeStudent) {
                setRowCount(0);
                return;
            }
            if (sdData.m_isCo) {
                if (sdData.getStudentEntryRating(student.getOid()) != null) {
                    m_ratingsMap.put(RATING_TYPE_ENTRY, sdData.getStudentEntryRating(student.getOid()));
                }
                if (sdData.getStudentProgressRating(student.getOid()) != null) {
                    m_ratingsMap.put(RATING_TYPE_PROGRESS, sdData.getStudentProgressRating(student.getOid()));
                }
                if (m_ratingsMap.keySet().isEmpty()) {
                    setRowCount(0);
                    return;
                }
            }
            // Get effective "E"ntry enrollment record for this date.
            m_enrollment = sdData.m_helper.getEnrollmentForDate(student.getOid(), sdData.m_reportDate,
                    "EYS");
            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(sdData.m_excludeSklField))) {
                m_rcdtsMap = lookupOverrides();

                // keep count of records
                sdData.m_totalStudentCount++;

                // look at schedules, count classes in each school and calculate
                // fte by class count.
                List<StudentScheduleSpan> spans = sdData.m_helperSched.getStudentScheduleSpans(student);
                calculateFte(spans, sdData);
                setRowCount(m_schools.size());
            } else {
                setRowCount(0);
            }
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
                    ", SASID: " + student.getStateId() + "]";

            return name;
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
         * From the student schedule on report date,
         * gather class counts by school for all classes.
         * <p>
         * 1. count classes in each school.
         * <br>
         * 2. calculate FTE values by school.
         * <br>
         * 3. If the student has no schedule and has an outplacement,
         * then report the outplacement school with a FTE of 1.0.
         *
         *
         * @param spans List<StudentScheduleSpan>
         * @param sdData ILStudentDemographicsData
         * @return List<Float>
         */

        private void calculateFte(List<StudentScheduleSpan> spans, ILEarlyChildhoodOutcomesData sdData) {
            m_fte = new ArrayList<Float>();
            m_schools = new ArrayList<String>();

            // Get schedule spans and count classes.
            int[] classCounts = new int[50];
            int totalCount = 0;

            if (m_hasServiceSchool) {
                String schoolId = m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);

                // No schedule and has outplacement, use outplacement school and 1.0
                m_schools.add(schoolId);
                m_fte.add(Float.valueOf(1.0f));
            } else {
                for (StudentScheduleSpan span : spans) {
                    if (!span.getEntryDate().after(sdData.m_reportDate) &&
                            !span.getExitDate().before(sdData.m_reportDate)) {
                        boolean found = false;
                        SisSchool spanSchool = span.getSection().getSchedule().getSchool();
                        String spanSchoolId = (String) spanSchool.getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                        if (!StringUtils.isEmpty(spanSchoolId)) {
                            for (int i = 0; i < m_schools.size(); i++) {
                                if (m_schools.get(i).equals(spanSchoolId)) {
                                    classCounts[i]++;
                                    totalCount++;
                                    found = true;
                                    break;
                                }
                            }
                            if (!found) {
                                int i = m_schools.size();
                                m_schools.add(spanSchoolId);
                                classCounts[i]++;
                                totalCount++;
                            }
                        }
                    }
                }

                // Calculate FTE for scheduled classes.
                if (m_schools.size() > 0 && totalCount > 0) {
                    for (int i = 0; i < m_schools.size(); i++) {
                        m_fte.add(Float.valueOf((float) classCounts[i] / (float) totalCount));
                    }
                }
            }
            if (m_schools.isEmpty() && m_enrollment != null && m_enrollment.getSchool() != null) {
                String rcdts = (String) m_enrollment.getSchool().getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                m_schools.add(rcdts);
                m_fte.add(Float.valueOf(1.0f));
            }
        }

        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            ILEarlyChildhoodOutcomesData sd = (ILEarlyChildhoodOutcomesData) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (sd.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = sd.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode = sd.lookupStateValue(StudentEnrollment.class, sd.m_fieldServiceSchoolCode,
                            serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                    m_hasServiceSchool = true;
                }
            }

            return calcValueMap;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_EARLY_INTERVENTION = "DOE EARLY INTER";
    protected static final String ALIAS_REF_CFC = "DOE EC REFERRAL BY CFC";
    protected static final String ALIAS_EI_NUMBER = "DOE EI NUMBER";
    protected static final String ALIAS_ENR_TYPE_FOR_Y = "DOE ENROLLMENT TYPE";
    protected static final String ALIAS_ENR_SKL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_ENR_SKL_SERVICE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_EXCLUDE_STD = "DOE EXCLUDE STD";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_IEP_IND = "DOE IEP IND";
    protected static final String ALIAS_LOW_INCOME = "DOE FRL LOW INC IND";
    protected static final String ALIAS_LUNCH_STATUS = "DOE LUNCH STATUS";
    protected static final String ALIAS_PRIVATE_SCHOOL_IND = "DOE PRIVATE SCHOOL IND";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_REFERRAL_BY_CFC = "DOE CFC REF";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_SKL_NON_CALC_FTE = "DOE NON-CALCULATING FTE";
    protected static final String ALIAS_SKL_WIDE_TITLE = "DOE SCHOOLWIDE TITLE 1";
    protected static final String ALIAS_SKL_WIDE_FARMS = "DOE SCHOOL-WIDE FARMS";
    protected static final String ALIAS_STD_TITLE = "DOE TITLE 1 IND";

    /*
     * UDA Aliases
     */
    protected static final String ALIAS_DDX_RAT_ACT_MEET_NEEDS = "all-uda-ECActionToMeetNeeds";
    protected static final String ALIAS_DDX_RAT_ACT_MEET_NEEDS_PR = "all-uda-ECMadePRMeetNeeds";
    protected static final String ALIAS_DDX_RAT_ADMIN_PART = "all-uda-ECAdminParticipate";
    protected static final String ALIAS_DDX_RAT_DATE = "all-uda-ECRatingDate";
    protected static final String ALIAS_DDX_RAT_KNOW_SKILLS = "all-uda-ECKnowledgeSkills";
    protected static final String ALIAS_DDX_RAT_KNOW_SKILLS_PR = "all-uda-ECPRKnowledgeSkills";
    protected static final String ALIAS_DDX_RAT_LANG_PATH = "all-uda-ECSpeechLangPath";
    protected static final String ALIAS_DDX_RAT_OTHER_PROV = "all-uda-ECOtherProvider";
    protected static final String ALIAS_DDX_RAT_PAR_INV = "all-uda-ECParentInvolved";
    protected static final String ALIAS_DDX_RAT_PR_SOC_RELAT = "all-uda-ECMadePRSocialRelation";
    protected static final String ALIAS_DDX_RAT_PRIM_ASM = "all-uda-ECPRAssessment";
    protected static final String ALIAS_DDX_RAT_PSYCH_WORKER = "all-uda-ECPsychSocialWorker";
    protected static final String ALIAS_DDX_RAT_SOC_RELS = "all-uda-ECSocialRelations";
    protected static final String ALIAS_DDX_RAT_SKL = "all-uda-ECRatingSchool";
    protected static final String ALIAS_DDX_RAT_TEACHER_PART = "all-uda-ECTeacherParticipate";
    protected static final String ALIAS_DDX_RAT_TYPE = "all-uda-ECRatingType";

    /*
     * Columns
     */
    protected static final String COLUMN_ENTRY_GRADE_LEVEL = "Entry/Grade Level";
    protected static final String COLUMN_IEP_INDICATOR = "IEP Indicator";
    protected static final String COLUMN_LEP_INDICATOR = "LEP Indicator";
    protected static final String COLUMN_EARLY_INTERVENTION = "Early Intervention";
    protected static final String COLUMN_REFERRAL_BY_CFC = "Referral by CFC";

    private static final String EXPORT_TYPE_CHILD = "IL Demographics and Early Childhood";
    private static final String EXPORT_TYPE_CHILD_V2 = "IL Demographics and Early Childhood V2";
    private static final String EXPORT_TYPE_CHILD_OUTCOMES = "Early Childhood Outcomes";
    private static final String EXPORT_TYPE_DEMO = "Student Demographics";

    /*
     * Input Parameters
     */
    private static final String INPUT_PARAM_PROCEDURE_ID = "procedureId";

    /*
     * Parameters
     */
    private static final String PARAM_START_DATE = "startDate";
    private static final String PARAM_REPORT_DATE = "reportDate";
    private static final String PARAM_IS_CHILDHOOD = "isChildhood";
    private static final String PARAM_IS_CHILDHOOD_V2 = "isChildhoodV2";

    /*
     * Procedures IDs
     */
    private static final String PROCEDURE_ID_CO = "EXPDATA-IL-STDEMO-CO";

    /*
     * Other internal constants
     */
    protected static final String ACTIVE_IEP_CODE = "01";
    protected static final String CONSTANT_01 = "01";
    protected static final String CONSTANT_02 = "02";
    protected static final String DDX_ID_RATINGS = "IL EC Outcomes";
    protected static final String ILLEGAL_NAME_CHARACTERS = "[^A-Za-z ]";
    protected static final String RATING_TYPE_ENTRY = "Entry";
    protected static final String RATING_TYPE_PROGRESS = "Progress";
    protected static final String REPORTABLE_CODE = "report";
    protected static final String STATE_CODE_BIOMOTHER_DOB = "01";
    /*
     * Instance variables
     */
    protected String m_activeCode;
    protected Collection<String> m_activeIepCodes = new ArrayList<String>();
    // Need for early childhood export
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected SimpleDateFormat m_dateFormatString = new SimpleDateFormat("yyyy-MM-dd");
    protected String m_referralByCFCField;
    protected String m_excludeStdField;
    protected String m_excludeSklField;
    protected String m_fieldDistrictCode;
    protected String m_fieldEnrSklHome;
    protected String m_fieldEnrSklService;
    protected String m_fieldEnrTypeForY;
    protected String m_fieldLowIncInd;
    protected String m_fieldPrivateSchoolInd;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldSklNonCalcFte;
    protected String m_fieldSklWideFarms;
    protected String m_fieldSklWideTitle;
    protected String m_fieldStdTitle;
    protected Pattern m_illegalNameCharacters = Pattern.compile(ILLEGAL_NAME_CHARACTERS);
    protected Boolean m_isChildhood;
    protected boolean m_isChildhoodV2;
    protected boolean m_isCo;
    protected PlainDate m_startDate;
    protected Map<String, UserDefinedTableA> m_stdEntryRatingsMap;
    protected Map<String, UserDefinedTableA> m_stdProgressRatingsMap;
    protected Map<String, ReferenceCode> m_referenceGradeCodeMap;
    protected PlainDate m_reportDate;
    protected Map<String, IepData> m_stdActiveIepMap = new HashMap<String, IepData>();

    /*
     * UDA fields
     */
    protected String m_udaActionToMeetNeeds;
    protected String m_udaActionToMeetNeedsPR;
    protected String m_udaAdminParticipate;
    protected String m_udaECMadePRSocialRelations;
    protected String m_udaKnowledgeSkills;
    protected String m_udaKnowledgeSkillsPR;
    protected String m_udaOtherProvider;
    protected String m_udaParentInvolved;
    protected String m_udaPrimAssessment;
    protected String m_udaPsychSocialWorker;
    protected String m_udaRatingDate;
    protected String m_udaRatingSchool;
    protected String m_udaRatingType;
    protected String m_udaSpeechLangPath;
    protected String m_udaSocialRelations;
    protected String m_udaTeacherParticipate;
    protected Map<String, Map<String, ReferenceCode>> m_udaRefCodesMap = new HashMap<>();
    /**
     * Helper class:
     * For student selection by enrollment.
     * For Student schedule span.
     */
    protected StudentHistoryHelper m_helper;
    protected StudentHistoryHelper m_helperSched;

    /**
     * Keep track of number of students
     */
    protected int m_totalStudentCount;

    /**
     * A map of student assessments, for use in retrieving LEP
     */
    protected Map<String, Collection<StudentAssessment>> m_assessmentMap =
            new HashMap<String, Collection<StudentAssessment>>();

    /**
     * A map of reference codes for enrollment types, for use in the entry type retriever
     */
    protected Map<String, ReferenceCode> m_enrollmentCodes;

    /**
     * A map of student program participations ("FRL/Low Income Indicator"), for use in the LowInc
     * retriever
     */
    protected Map<String, List<StudentProgramParticipation>> m_lowIncMap;

    /**
     * A map of student program participations state ("HMLS"), for use in the Homeless Ind retriever
     */
    protected Map<String, Collection<StudentProgramParticipation>> m_participationHomelessMap;

    /**
     * A map of student program participations ("ESL"), for use in the LEP retriever
     */
    protected Map<String, Collection<StudentProgramParticipation>> m_participationMap;

    /**
     * A map of reference codes for race codes, for use in the race code retriever.
     */
    protected Map<String, ReferenceCode> m_raceCodes;

    /**
     * A map of reference codes for relationship codes, for use in biomother dob retriever
     */
    protected Map<String, ReferenceCode> m_relationsCodes;

    /**
     * A map of reference codes for RCDTS serving school codes, for use in RCDTS retriever
     */
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();


    /**
     * Retrieve DOB of studentContact marked with state reference code = "01".
     */
    protected class RetrieveBiomotherDOB implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            for (StudentContact contact : student.getContacts()) {
                String relCode = contact.getRelationshipCode();
                if (!StringUtils.isEmpty(relCode)) {
                    ReferenceCode referenceCode = m_relationsCodes.get(relCode);
                    if ((referenceCode != null) && STATE_CODE_BIOMOTHER_DOB.equals(referenceCode.getStateCode())) {
                        return contact.getPerson().getDob();
                    }
                }
            }
            return EMPTY_STRING;
        }
    }

    /**
     * Retrieve the student's most recent enrollment's date.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEntryDate implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            PlainDate enrollmentDate = null;
            PlainDate startDate = data.getOrganization().getCurrentContext().getStartDate();
            StudentEnrollment recentEnrollment = ((StudentDemographicEntity) entity).getEffectiveEnrollment();
            if (recentEnrollment != null) {
                enrollmentDate = recentEnrollment.getEnrollmentDate();
                SisSchool school = recentEnrollment.getSchool();
                if (school.getActiveSchedule() != null) {
                    startDate = school.getActiveSchedule().getStartDate();
                }
            }

            if ((enrollmentDate == null) || enrollmentDate.before(startDate)) {
                enrollmentDate = startDate;
            }

            return enrollmentDate;
        }
    }

    /**
     * Retrieve the student's most recent enrollment's entry type state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEntryType implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String entryCode = "";
            StudentEnrollment enrollment = ((StudentDemographicEntity) entity).getEffectiveEnrollment();
            if (enrollment != null) {
                String enrollmentCode = enrollment.getEnrollmentCode();
                if (m_enrollmentCodes.containsKey(enrollmentCode)) {
                    ReferenceCode refCode = m_enrollmentCodes.get(enrollmentCode);
                    entryCode = refCode.getStateCode();
                }
            }
            return entryCode;
        }
    }

    /**
     * Retrieve the student's most recent enrollment's entry type state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveEntryTypeV2 implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            String entryCode = "";
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool skl = student.getSchool();
            StudentEnrollment enrollment = null;
            if (BooleanAsStringConverter.TRUE
                    .equals(skl.getFieldValueByBeanPath(m_fieldSklNonCalcFte))) {
                enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, StudentEnrollment.ENTRY);
            } else {
                enrollment = m_helper.getEnrollmentForDate(student.getOid(), m_reportDate, "EY");
            }
            if (enrollment != null) {
                if (StudentEnrollment.YOG_CHANGE.equals(enrollment.getEnrollmentType()) && !StringUtils
                        .isEmpty(entryCode = (String) enrollment.getFieldValueByBeanPath(m_fieldEnrTypeForY))) {
                    entryCode = lookupStateValue(StudentEnrollment.class, m_fieldEnrTypeForY, entryCode);
                } else {
                    String enrollmentCode = enrollment.getEnrollmentCode();
                    if (m_enrollmentCodes.containsKey(enrollmentCode)) {
                        ReferenceCode refCode = m_enrollmentCodes.get(enrollmentCode);
                        entryCode = refCode.getStateCode();
                    }
                }
            }
            return entryCode;
        }
    }
    /**
     * Retrieve the student's Title 1 Indicator.
     * If student is enrolled in school where school.DOE SCHOOLWIDE TITLE I = True, return '11' for
     * the students.
     * Otherwise, look to student.DOE TITLE 1 IND.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveFrlLowIndForV2Childhood implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool skl = student.getSchool();
            Object value = "02";
            if (BooleanAsStringConverter.TRUE.equals(skl.getFieldValueByBeanPath(m_fieldSklWideFarms))) {
                value = "01";
            } else if (m_lowIncMap.get(student.getOid()) != null && !m_lowIncMap.get(student.getOid()).isEmpty()) {
                value = "01";
            }
            return value;
        }
    }

    /**
     * Returns the calculated FTE for the current school.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveFte implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            return ((StudentDemographicEntity) entity).getCurrentFte();
        }
    }

    /**
     * Retrieve the student's Homeless status based on existing PGMs
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveHomelessInd implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {

            SisStudent student = (SisStudent) entity.getBean();
            return m_participationHomelessMap.get(student.getOid()) != null
                    && !m_participationHomelessMap.get(student.getOid()).isEmpty() ? Boolean.valueOf(true)
                            : Boolean.valueOf(false);
        }
    }

    /**
     * Retrieve the student's IEP status based on existing IEPs with iepStatus = "Active" on report
     * date
     * or
     * stdSpedStatus.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveIDEA implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean isIep = false;

            SisStudent student = (SisStudent) entity.getBean();
            /*
             * Apply a hard rule that when student.[DOE PRIVATE SCHOOL IND] = True, that IEP
             * Indicator
             * (File position 240) must = 02 in the output.
             */
            String privateSchoolInd = (String) student.getFieldValueByBeanPath(m_fieldPrivateSchoolInd);

            if (BooleanAsStringConverter.TRUE.equals(privateSchoolInd)) {
                return Boolean.valueOf(true);
            }

            if (m_stdActiveIepMap.containsKey(student.getOid())) {
                isIep = true;
            } else {
                if (!StringUtils.isEmpty(student.getSpedStatusCode()) &&
                        m_activeIepCodes.contains(student.getSpedStatusCode())) {
                    isIep = true;
                }
            }

            return Boolean.valueOf(isIep);
        }
    }

    /**
     * Retrieves a Boolean - true if the value in the bean path is equal to the state value in the
     * calc
     * parameter, false otherwise.
     *
     * @author Follett Software Company
     */
    public class RetrieveIep implements FieldRetriever {

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
            Boolean result = Boolean.FALSE;

            SisStudent student = (SisStudent) entity.getBean();

            /*
             * Apply a hard rule that when student.[DOE PRIVATE SCHOOL IND] = True, that IEP
             * Indicator
             * (File position 240) must = 02 in the output.
             */
            String privateSchoolInd = (String) student.getFieldValueByBeanPath(m_fieldPrivateSchoolInd);

            if (!BooleanAsStringConverter.TRUE.equals(privateSchoolInd)) {
                String path = field.getBeanPath();

                try {
                    Object propertyValue = WebUtils.getProperty(entity.getBean(), path);
                    String value = lookupStateValue(entity.getBean().getClass(), path, (String) propertyValue);
                    result = Boolean.valueOf(field.getParameter().equals(value));
                } catch (X2BaseException e) {
                    StateReportValidationError error =
                            new StateReportValidationError(entity, field, e.getMessage(), path);
                    entity.addRetrievalError(field.getFieldId(), error);
                }
            }

            return result;
        }
    }

    /**
     * Retrieve the student's LEP status based on their program participation. If there exists a
     * program participation that is ESL and the report date falls within:
     *
     * (1) - between its start date and end date, or
     * (2) - after the start date if an end date is not specified
     *
     * Then the student is considered having an LEP
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveLep implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentProgramParticipation> participations = m_participationMap.get(student.getOid());
            Boolean isInLep = Boolean.FALSE;
            if (participations != null /* && checkAccessScore(student.getOid()) */) {
                for (StudentProgramParticipation participation : participations) {
                    Date participationStartDate = participation.getStartDate();
                    Date participationEndDate = participation.getEndDate();

                    if ((participationEndDate != null) &&
                            !m_reportDate.before(participationStartDate) &&
                            !m_reportDate.after(participationEndDate)) {
                        isInLep = Boolean.TRUE;
                        break;
                    } else if ((participationEndDate == null) &&
                            !m_reportDate.before(participationStartDate)) {
                        isInLep = Boolean.TRUE;
                        break;
                    }
                }
            }

            return isInLep;
        }
    }

    /**
     * Retrieve the student's FRL/Low Income Indicator.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveLowIncInd implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            boolean lowIncInd = false;

            SisStudent student = (SisStudent) entity.getBean();
            Collection<StudentProgramParticipation> participations = m_lowIncMap.get(student.getOid());
            if (participations != null && !participations.isEmpty()) {
                lowIncInd = true;
            }
            if (!StringUtils.isEmpty(m_fieldLowIncInd)) {
                lowIncInd = BooleanAsStringConverter.TRUE.equals(student.getFieldValueByBeanPath(m_fieldLowIncInd));
            }

            return Boolean.valueOf(lowIncInd);
        }
    }

    /**
     * Retrieve the student's native language's state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveNativeLang implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            String nativeLanguage = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (StringUtils.isEmpty(nativeLanguage)) {
                if (!StringUtils.isEmpty(student.getHomeLanguageCode())) {
                    nativeLanguage = student.getHomeLanguageCode();
                }
            }

            return nativeLanguage;
        }
    }

    /**
     * Retrieve the student's race's state code. If the student has the Hispanic/Latino indicator
     * checked,
     * return the Hispanic state code. If the student has more than 1 race, return the
     * "Two or More Race"
     * state code. Otherwise, return the student's race's state code.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRace implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {

            String raceCode = "";
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();
            if ((person != null) && person.getHispanicLatinoIndicator()) {
                raceCode = "11";
            } else {
                Collection<Race> races = m_helper.getRaces(student);
                if (races != null) {
                    if (races.size() > 1) {
                        raceCode = "17";
                    } else {
                        for (Race race : races) {
                            if (m_raceCodes.containsKey(race.getRaceCode())) {
                                ReferenceCode refCode = m_raceCodes.get(race.getRaceCode());
                                raceCode = refCode.getStateCode() != null ? refCode.getStateCode() : "";
                            }
                        }
                    }
                }
            }

            return raceCode;
        }
    }

    /**
     * Retrieves data for entry rating from UDA.
     *
     * @author Follett Software Company
     */
    public class RetrieveRatingEntry implements FieldRetriever {

        protected static final String CALC_ID = "DEMO-ER";

        /*
         * Calc params for ER
         */
        protected static final String CALC_PARAM_ER_DATE = "ER_DATE";
        protected static final String CALC_PARAM_ER_ECA_ADMIN = "ER_ECA_ADMIN";
        protected static final String CALC_PARAM_ER_SKL_HOME = "ER_SKL_HOME";
        protected static final String CALC_PARAM_ER_KNOW_SKILL = "ER_KNOW_SKILL";
        protected static final String CALC_PARAM_ER_OWN_NEEDS = "ER_OWN_NEEDS";
        protected static final String CALC_PARAM_ER_PAR_INV = "ER_PAR_INV";
        protected static final String CALC_PARAM_ER_POS_REL = "ER_POS_REL";
        protected static final String CALC_PARAM_ER_PROVIDER_PART = "ER_PROVIDER_PART";
        protected static final String CALC_PARAM_ER_SPEECH_PART = "ER_SPEECH_PART";
        protected static final String CALC_PARAM_ER_TEACH_PART = "ER_TEACH_PART";
        protected static final String CALC_PARAM_ER_WORKER_PART = "ER_WORKER_PART";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ILEarlyChildhoodOutcomesData sdData = (ILEarlyChildhoodOutcomesData) data;
            StudentDemographicEntity sdEntity = (StudentDemographicEntity) entity;
            Object value = null;
            UserDefinedTableA ratingEntry = sdEntity.m_ratingsMap.get(RATING_TYPE_ENTRY);
            if (ratingEntry != null) {
                String param = (String) field.getParameter();
                if (CALC_PARAM_ER_DATE.equals(param)) {
                    if (m_udaRatingDate != null && ratingEntry.getFieldValueByBeanPath(m_udaRatingDate) != null) {
                        Object ratingDate = ratingEntry.getFieldValueByBeanPath(m_udaRatingDate);
                        if (ratingDate instanceof String) {
                            try {
                                value = m_dateFormatString.parse((String) ratingDate);
                            } catch (ParseException e) {
                                value = "Incorrect";
                            }
                        } else if (ratingDate instanceof Date) {
                            value = ratingEntry.getFieldValueByBeanPath(m_udaRatingDate);
                        }
                    }
                } else if (CALC_PARAM_ER_SKL_HOME.equals(param)) {
                    value = m_udaRatingSchool != null
                            ? getStateValueByBeanPath(ratingEntry, sdData, sdData.m_udaRatingSchool)
                            : null;
                    if (value == null) {
                        Object ratingDate = ratingEntry.getFieldValueByBeanPath(m_udaRatingDate);
                        Date dateToOperate = null;
                        if (ratingDate instanceof String) {
                            try {
                                dateToOperate = m_dateFormatString.parse((String) ratingDate);
                            } catch (ParseException e) {
                                dateToOperate = null;
                            }
                        } else if (ratingDate instanceof Date) {
                            dateToOperate = (Date) ratingEntry.getFieldValueByBeanPath(m_udaRatingDate);
                        }
                        if (dateToOperate != null) {
                            StudentEnrollment enrToOperate =
                                    sdData.m_helper.getEnrollmentForDate(entity.getBean().getOid(),
                                            new PlainDate(dateToOperate), "ES");
                            if (enrToOperate != null) {
                                String code = (String) enrToOperate.getFieldValueByBeanPath(sdData.m_fieldEnrSklHome);
                                if (!com.x2dev.utils.StringUtils.isEmpty(code)) {
                                    value = sdData.lookupStateValue(StudentEnrollment.class, sdData.m_fieldEnrSklHome,
                                            code);
                                }
                                if (value == null && enrToOperate.getSchool() != null) {
                                    value = enrToOperate.getSchool().getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                                }
                            }
                        }
                    }
                } else if (CALC_PARAM_ER_POS_REL.equals(param)) {
                    value = m_udaSocialRelations != null
                            ? getStateValueByBeanPath(ratingEntry, sdData, sdData.m_udaSocialRelations)
                            : null;
                } else if (CALC_PARAM_ER_KNOW_SKILL.equals(param)) {
                    value = m_udaKnowledgeSkills != null
                            ? getStateValueByBeanPath(ratingEntry, sdData, sdData.m_udaKnowledgeSkills)
                            : null;
                } else if (CALC_PARAM_ER_OWN_NEEDS.equals(param)) {
                    value = m_udaActionToMeetNeeds != null
                            ? getStateValueByBeanPath(ratingEntry, sdData, sdData.m_udaActionToMeetNeeds)
                            : null;
                } else if (CALC_PARAM_ER_PAR_INV.equals(param)) {
                    value = m_udaParentInvolved != null
                            ? getStateValueByBeanPath(ratingEntry, sdData, sdData.m_udaParentInvolved)
                            : null;
                } else if (CALC_PARAM_ER_ECA_ADMIN.equals(param)) {
                    value = m_udaAdminParticipate != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingEntry.getFieldValueByBeanPath(m_udaAdminParticipate)) ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_ER_TEACH_PART.equals(param)) {
                    value = m_udaTeacherParticipate != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingEntry.getFieldValueByBeanPath(m_udaTeacherParticipate))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_ER_WORKER_PART.equals(param)) {
                    value = m_udaPsychSocialWorker != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingEntry.getFieldValueByBeanPath(m_udaPsychSocialWorker))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_ER_SPEECH_PART.equals(param)) {
                    value = m_udaSpeechLangPath != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingEntry.getFieldValueByBeanPath(m_udaSpeechLangPath))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_ER_PROVIDER_PART.equals(param)) {
                    value = m_udaOtherProvider != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingEntry.getFieldValueByBeanPath(m_udaOtherProvider))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                }
            }
            return value;
        }
    }

    /**
     * Retrieves data for progress rating from UDA.
     *
     * @author Follett Software Company
     */
    public class RetrieveRatingProgress implements FieldRetriever {

        protected static final String CALC_ID = "DEMO-PR";

        /*
         * Calc params for ER
         */
        protected static final String CALC_PARAM_PR_DATE = "PR_DATE";
        protected static final String CALC_PARAM_PR_ECA_ADMIN = "PR_ECA_ADMIN";
        protected static final String CALC_PARAM_PR_SKL_HOME = "PR_SKL_HOME";
        protected static final String CALC_PARAM_PR_KNOW_SKILL = "PR_KNOW_SKILL";
        protected static final String CALC_PARAM_PR_KNOW_SKILL_PR = "PR_KNOW_SKILL_PR";
        protected static final String CALC_PARAM_PR_OWN_NEEDS = "PR_OWN_NEEDS";
        protected static final String CALC_PARAM_PR_OWN_NEEDS_PR = "PR_OWN_NEEDS_PR";
        protected static final String CALC_PARAM_PR_PAR_INV = "PR_PAR_INV";
        protected static final String CALC_PARAM_PR_POS_REL = "PR_POS_REL";
        protected static final String CALC_PARAM_PR_POS_REL_PR = "PR_POS_REL_PR";
        protected static final String CALC_PARAM_PR_PRIM_ASM = "PR_PRIM_ASM";
        protected static final String CALC_PARAM_PR_PROVIDER_PART = "PR_PROVIDER_PART";
        protected static final String CALC_PARAM_PR_SPEECH_PART = "PR_SPEECH_PART";
        protected static final String CALC_PARAM_PR_TEACH_PART = "PR_TEACH_PART";
        protected static final String CALC_PARAM_PR_WORKER_PART = "PR_WORKER_PART";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            ILEarlyChildhoodOutcomesData sdData = (ILEarlyChildhoodOutcomesData) data;
            StudentDemographicEntity sdEntity = (StudentDemographicEntity) entity;
            Object value = null;
            UserDefinedTableA ratingProgress = sdEntity.m_ratingsMap.get(RATING_TYPE_PROGRESS);
            if (ratingProgress != null) {
                String param = (String) field.getParameter();
                if (CALC_PARAM_PR_DATE.equals(param)) {
                    if (m_udaRatingDate != null && ratingProgress.getFieldValueByBeanPath(m_udaRatingDate) != null) {
                        Object ratingDate = ratingProgress.getFieldValueByBeanPath(m_udaRatingDate);
                        if (ratingDate instanceof String) {
                            try {
                                value = m_dateFormatString.parse((String) ratingDate);
                            } catch (ParseException e) {
                                value = "Incorrect";
                            }
                        } else if (ratingDate instanceof Date) {
                            value = ratingProgress.getFieldValueByBeanPath(m_udaRatingDate);
                        }
                    }
                } else if (CALC_PARAM_PR_SKL_HOME.equals(param)) {
                    value = m_udaRatingSchool != null
                            ? getStateValueByBeanPath(ratingProgress, sdData, sdData.m_udaRatingSchool)
                            : null;
                    if (value == null) {
                        Object ratingDate = ratingProgress.getFieldValueByBeanPath(m_udaRatingDate);
                        Date dateToOperate = null;
                        if (ratingDate instanceof String) {
                            try {
                                dateToOperate = m_dateFormatString.parse((String) ratingDate);
                            } catch (ParseException e) {
                                dateToOperate = null;
                            }
                        } else if (ratingDate instanceof Date) {
                            dateToOperate = (Date) ratingProgress.getFieldValueByBeanPath(m_udaRatingDate);
                        }
                        if (dateToOperate != null) {
                            StudentEnrollment enrToOperate =
                                    sdData.m_helper.getEnrollmentForDate(entity.getBean().getOid(),
                                            new PlainDate(dateToOperate), "ES");
                            if (enrToOperate != null) {
                                String code = (String) enrToOperate.getFieldValueByBeanPath(sdData.m_fieldEnrSklHome);
                                if (!com.x2dev.utils.StringUtils.isEmpty(code)) {
                                    value = sdData.lookupStateValue(StudentEnrollment.class, sdData.m_fieldEnrSklHome,
                                            code);
                                }
                                if (value == null && enrToOperate.getSchool() != null) {
                                    value = enrToOperate.getSchool().getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                                }
                            }
                        }
                    }
                } else if (CALC_PARAM_PR_POS_REL.equals(param)) {
                    value = m_udaSocialRelations != null
                            ? getStateValueByBeanPath(ratingProgress, sdData, sdData.m_udaSocialRelations)
                            : null;
                } else if (CALC_PARAM_PR_POS_REL_PR.equals(param)) {
                    value = m_udaECMadePRSocialRelations != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaECMadePRSocialRelations))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_KNOW_SKILL.equals(param)) {
                    value = m_udaKnowledgeSkills != null
                            ? getStateValueByBeanPath(ratingProgress, sdData, sdData.m_udaKnowledgeSkills)
                            : null;
                } else if (CALC_PARAM_PR_KNOW_SKILL_PR.equals(param)) {
                    value = m_udaKnowledgeSkillsPR != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaKnowledgeSkillsPR))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_OWN_NEEDS.equals(param)) {
                    value = m_udaActionToMeetNeeds != null
                            ? getStateValueByBeanPath(ratingProgress, sdData, sdData.m_udaActionToMeetNeeds)
                            : null;
                } else if (CALC_PARAM_PR_OWN_NEEDS_PR.equals(param)) {
                    value = m_udaActionToMeetNeedsPR != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaActionToMeetNeedsPR))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_PRIM_ASM.equals(param)) {
                    value = m_udaPrimAssessment != null
                            ? getStateValueByBeanPath(ratingProgress, sdData, sdData.m_udaPrimAssessment)
                            : null;
                } else if (CALC_PARAM_PR_PAR_INV.equals(param)) {
                    value = m_udaParentInvolved != null
                            ? getStateValueByBeanPath(ratingProgress, sdData, sdData.m_udaParentInvolved)
                            : null;
                } else if (CALC_PARAM_PR_ECA_ADMIN.equals(param)) {
                    value = m_udaAdminParticipate != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaAdminParticipate))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_TEACH_PART.equals(param)) {
                    value = m_udaTeacherParticipate != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaTeacherParticipate))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_WORKER_PART.equals(param)) {
                    value = m_udaPsychSocialWorker != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaPsychSocialWorker))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_SPEECH_PART.equals(param)) {
                    value = m_udaSpeechLangPath != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaSpeechLangPath))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                } else if (CALC_PARAM_PR_PROVIDER_PART.equals(param)) {
                    value = m_udaOtherProvider != null ? (BooleanAsStringConverter.TRUE
                            .equals(ratingProgress.getFieldValueByBeanPath(m_udaOtherProvider))
                                    ? CONSTANT_01
                                    : CONSTANT_02)
                            : CONSTANT_02;
                }
            }
            return value;
        }
    }

    /**
     * Retrieve the RCDTS of the student's home/serving school
     *
     * Parameter this retriever accepts:
     * - "H" for the home school's RCDTS
     * - "S" for the serving school's RCDTS.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveRcdts implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            StudentDemographicEntity sdEntity = (StudentDemographicEntity) entity;
            StudentEnrollment primEnr = sdEntity.getEffectiveEnrollment();
            ILEarlyChildhoodOutcomesData sdData = (ILEarlyChildhoodOutcomesData) data;
            String rcdts = null;
            if (primEnr != null) {
                if (param.equals("H") && sdEntity.getEffectiveEnrollment() != null
                        && sdEntity.getEffectiveEnrollment().getSchool() != null) {
                    if (primEnr != null) {
                        String codeForNonFte = (String) primEnr.getFieldValueByBeanPath(sdData.m_fieldEnrSklHome);
                        if (!StringUtils.isEmpty(codeForNonFte)) {
                            rcdts = sdData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklHome, codeForNonFte);
                        }
                    }
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                    }
                } else if (param.equals("S")) {
                    String servingCode = (String) primEnr.getFieldValueByBeanPath(m_fieldEnrSklService);
                    if (!StringUtils.isEmpty(servingCode)) {
                        rcdts = sdData.lookupStateValue(StudentEnrollment.class, m_fieldEnrSklService, servingCode);
                    }
                    if (StringUtils.isEmpty(rcdts)) {
                        rcdts = (String) primEnr.getSchool().getFieldValueByBeanPath(sdData.m_fieldSchoolCode);
                    }
                }
            }
            return rcdts;
        }
    }

    /**
     * Returns the calculated Referral by CFC.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveReferralByCFC implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            Boolean value = (Boolean) data.getPropertyAsJavaType(student, m_referralByCFCField);

            return value != null && value.booleanValue() ? "01" : "";
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

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
            String cleanValue = null;
            String nameValue = (String) getProperty(entity.getBean(), field.getBeanPath());
            if (nameValue != null) {
                Matcher matcher = m_illegalNameCharacters.matcher(nameValue);
                cleanValue = matcher.replaceAll("");
            } else {
                cleanValue = "";
            }

            return cleanValue;
        }
    }

    /**
     * Retrieve the student's Title 1 Indicator.
     * If student is enrolled in school where school.DOE SCHOOLWIDE TITLE I = True, return '11' for
     * the students.
     * Otherwise, look to student.DOE TITLE 1 IND.
     *
     * @author X2 Development Corporation
     *
     */
    protected class RetrieveTitle1ForV2Childhood implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            SisStudent student = (SisStudent) entity.getBean();
            SisSchool skl = student.getSchool();
            Object value = "13";
            if (BooleanAsStringConverter.TRUE.equals(skl.getFieldValueByBeanPath(m_fieldSklWideTitle))) {
                value = "11";
            } else if (student.getFieldValueByBeanPath(m_fieldStdTitle) != null) {
                value = student.getFieldValueByBeanPath(m_fieldStdTitle);
            }
            return value;
        }
    }

    /**
     * Career/Tech Ed Indicator for Pre-K thru Grade 8 must be set to "No."
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateCareerTechEd implements FieldValidator {

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
            String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            if (gradeLevel.matches("14|15|0[0-8]") && value.equals("1")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Career/Tech Ed Indicator for Pre-K thru Grade 8 must be set to No.",
                        "Grade Level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", Career/Tech Ed Ind = " + STYLE_BOLD + "0" + value +
                                STYLE_END));
            }

            return errors;
        }

    }

    /**
     * Validates the student's most recent enrollment's entry date.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateEntryDate implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();
            PlainDate studentDob = null;
            try {
                studentDob = person.getDob();
            } catch (NullPointerException npe) {
                System.out.println(student.getNameView());
            }
            PlainDate enrollmentDate = null;

            // Enrollment Date cannot be a future date
            try {
                enrollmentDate = new PlainDate(m_dateFormat.parse(value));
            } catch (ParseException e1) {
                errors.add(new StateReportValidationError(entity, field,
                        "Entry date needs to be in MM/dd/yyyy format", "Entry date = " +
                                STYLE_BOLD + value +
                                STYLE_END));
            }

            if (enrollmentDate != null) {
                if (enrollmentDate.after(m_reportDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Enrollment Date cannot be a future date",
                            "Enrollment date = " + STYLE_BOLD +
                                    m_dateFormat.format(value) + STYLE_END));
                }

                // All students must be at least 3 years old on the 1st day of
                // class.
                int ageOfStudentOn1stDay = student.getPerson().getAgeAsOfDate(enrollmentDate);
                if (ageOfStudentOn1stDay < 3) {

                    String studentDobAsString = null;
                    if (studentDob != null) {
                        studentDobAsString = m_dateFormat.format(studentDob);
                    } else {
                        studentDobAsString = "empty";
                    }

                    errors.add(new StateReportValidationError(entity, field,
                            "All students must be at least 3 years old on the 1st day of class",
                            "Student's DOB = " + STYLE_BOLD + studentDobAsString +
                                    STYLE_END +
                                    ", Entry date = " + STYLE_BOLD + value + STYLE_END));
                }

                // Pre-K students with IEP = No. Must be less than 5 years old
                // on September 1 of the current school year
                int currentSchoolYear = student.getOrganization1().getCurrentContext().getSchoolYear();
                String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
                String hasIep = entity.getFieldValue(COLUMN_IEP_INDICATOR);
                if (gradeLevel.equals("14") && hasIep.equals(BooleanAsStringConverter.FALSE)) {
                    Calendar cal = Calendar.getInstance();
                    cal.set(Calendar.MONTH, Calendar.SEPTEMBER);
                    cal.set(Calendar.DAY_OF_MONTH, 1);
                    cal.set(Calendar.YEAR, currentSchoolYear - 1);
                    cal.add(Calendar.YEAR, -5);
                    PlainDate sept1ofCurSchYear = new PlainDate(cal.getTimeInMillis());

                    int ageOnSept1 = student.getPerson().getAgeAsOfDate(sept1ofCurSchYear);
                    if (5 <= ageOnSept1) {
                        errors.add(new StateReportValidationError(entity, field,
                                "Pre-K students with IEP must not be less than 5 years old on September 1 of the current school year",
                                "Student's DOB = " + STYLE_BOLD +
                                        m_dateFormat.format(studentDob) + STYLE_END));
                    }
                }

            } else {
                errors.add(new StateReportValidationError(entity, field,
                        "Student does not have an enrollment record",
                        ""));
            }

            return errors;
        }
    }

    /**
     * Validates the student's FTE (Full-time Equivalency).
     *
     * @author X2 Development Corporation
     */
    protected class ValidateFte implements FieldValidator {

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
            String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            double fte = Double.parseDouble(value);

            // FTE for Pre-K (Code 14) Entry/Grade Level must be 1.0
            if (gradeLevel.equals("14") && (fte != 1.00)) {
                errors.add(new StateReportValidationError(entity, field,
                        "FTE for Pre-K (Code 14) Entry/Grade Level must be 1.0",
                        "Grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", FTE = " + STYLE_BOLD + value + STYLE_END));
            }

            // FTE for Kindergarten (Code 15) Entry/Grade Level must be 0.5 for
            // half-day or 1.0 for full-day
            if (gradeLevel.equals("15") && ((fte != 0.5) || (fte != 1.0))) {
                errors.add(new StateReportValidationError(entity, field,
                        "FTE for Kindergarten (Code 15) Entry/Grade Level must be 0.5 for half-day or 1.0 for full-day",
                        "Grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", FTE = " + STYLE_BOLD + value + STYLE_END));
            }

            // FTE must be in a range of 0 and 1
            if ((fte <= 0) || (fte > 1)) {
                errors.add(new StateReportValidationError(entity, field,
                        "FTE is out of range.  Must be greater than 0 and less than or equal to 1.00",
                        "FTE = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validates the student's native language.
     *
     * @author X2 Development Corporation
     */
    protected class ValidateLang implements FieldValidator {

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
            Collection<StateReportValidationError> errors = new ArrayList<StateReportValidationError>(1);

            // If Student is enrolled as Pre-K, the Native language must be
            // included.
            String entryLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            if ((entryLevel != null) && entryLevel.equals("14") && (StringUtils.isEmpty(value))) {
                errors.add(new StateReportValidationError(entity, field,
                        "If student is enrolled as Pre-K, the Native language must be included",
                        "Native Language = " + STYLE_BOLD + value
                                + STYLE_END));
            }

            // If Student is enrolled as LEP the Native language must be
            // included and cannot be English
            String isLEP = entity.getFieldValue(COLUMN_LEP_INDICATOR);
            if ("1".equals(isLEP) && "000".equals(value)) {
                errors.add(new StateReportValidationError(entity, field,
                        "If Student is enrolled as LEP the Native language must be included and cannot be English",
                        "Native Language = " + STYLE_BOLD + value
                                + STYLE_END + ", LEP = " + isLEP));
            }

            return errors;
        }
    }

    /**
     * SES indicator for Birth to 3 and Pre-K must be set to 'No.'
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateSes implements FieldValidator {

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
            String gradeLevel = entity.getFieldValue(COLUMN_ENTRY_GRADE_LEVEL);
            if (gradeLevel.matches("00|14") && value.equals("1")) {
                errors.add(new StateReportValidationError(entity, field,
                        "SES indicator for Pre-K must be set to 'No.'",
                        "Grade level = " + STYLE_BOLD + gradeLevel + STYLE_END +
                                ", SES = " + STYLE_BOLD + "0" + value + STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate the student's age on report date
     *
     * Applies to: Reading First, Reading Improv. Ind, Elig. for Imm Edu, & Century 21 Indicator
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateStudentAge implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();

            // Students ages Birth to 3 should be set to "No"
            int ageOnReportDate = student.getPerson().getAgeAsOfDate(m_reportDate);
            if ((ageOnReportDate < 3) && !value.equals("2")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Students ages Birth to 3 years of age should be set to 'No'",
                        "Student's age = " + STYLE_BOLD + student.getPerson().getAge() +
                                STYLE_END));
            }

            return errors;
        }
    }

    /**
     * Validate the student's age on report date
     *
     * Applies to: Title 1st Indicator.
     *
     * @author Follett Software Company
     */
    protected class ValidateTitle1Age implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();

            int ageOnReportDate = student.getPerson().getAgeAsOfDate(m_reportDate);
            if ((ageOnReportDate < 3) && !value.equals("13")) {
                errors.add(new StateReportValidationError(entity, field,
                        "Students ages Birth to 3 years of age should be set to '13'",
                        "Student's age = " + STYLE_BOLD + student.getPerson().getAge() +
                                STYLE_END + ", " +
                                "Title 1 = " + STYLE_BOLD + value + STYLE_END));
            }
            return errors;
        }

    }

    /**
     * Validation rule: "data is valid only when Early Intervention, Referral by CFC = Yes".
     */
    protected class ValidateEINumber implements FieldValidator {
        private final String VALIDATION_ERROR = "Student's " + ALIAS_EI_NUMBER + " should be filled only then "
                + ALIAS_EARLY_INTERVENTION + " and " + ALIAS_REFERRAL_BY_CFC
                + " set to \"yes\".";

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
            String earlyInter = entity.getFieldValue(COLUMN_EARLY_INTERVENTION);
            String referralByCFC = entity.getFieldValue(COLUMN_REFERRAL_BY_CFC);

            if ((!BooleanAsStringConverter.TRUE.equals(earlyInter) ||
                    !BooleanAsStringConverter.TRUE.equals(referralByCFC)) &&
                    !StringUtils.isEmpty(value)) {
                StringBuilder errorDetail = new StringBuilder();
                errorDetail.append("Early Intervention Ind: ");
                errorDetail.append(earlyInter);
                errorDetail.append(", Referral by CFC Ind: ");
                errorDetail.append(referralByCFC);
                errorDetail.append(", EI Number: ");
                errorDetail.append(value);

                errors.add(new StateReportValidationError(entity, field, VALIDATION_ERROR, errorDetail.toString()));
            }
            return errors;
        }
    }

    /**
     * Validation rule: "data is valid only when Early Intervention, Referral by CFC = Yes".
     */
    protected class ValidateEligDeterDate implements FieldValidator {
        private final String VALIDATION_ERROR = "Student's [DOE ELIG DATE] should be filled only then "
                + ALIAS_EARLY_INTERVENTION + " and " + ALIAS_REFERRAL_BY_CFC
                + " set to \"yes\".";

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
            String earlyInter = entity.getFieldValue(COLUMN_EARLY_INTERVENTION);
            String referralByCFC = entity.getFieldValue(COLUMN_REFERRAL_BY_CFC);

            if ((!BooleanAsStringConverter.TRUE.equals(earlyInter) ||
                    !BooleanAsStringConverter.TRUE.equals(referralByCFC)) &&
                    !StringUtils.isEmpty(value)) {
                StringBuilder errorDetail = new StringBuilder();
                errorDetail.append("Early Intervention Ind: ");
                errorDetail.append(earlyInter);
                errorDetail.append(", Referral by CFC Ind: ");
                errorDetail.append(referralByCFC);
                errorDetail.append(", Elig Deter Date: ");
                errorDetail.append(value);

                errors.add(new StateReportValidationError(entity, field, VALIDATION_ERROR, errorDetail.toString()));
            }
            return errors;
        }
    }

    /**
     * Validation rule: "data is valid only when Early Intervention, Referral by CFC = Yes".
     */
    protected class ValidateReasonForDelay implements FieldValidator {
        private final String VALIDATION_ERROR = "Student's [DOE TRANS DELAY REASON] should be filled only then "
                + ALIAS_EARLY_INTERVENTION + " and " + ALIAS_REFERRAL_BY_CFC
                + " set to \"yes\".";

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
            String earlyInter = entity.getFieldValue(COLUMN_EARLY_INTERVENTION);
            String referralByCFC = entity.getFieldValue(COLUMN_REFERRAL_BY_CFC);

            if ((!BooleanAsStringConverter.TRUE.equals(earlyInter) ||
                    !BooleanAsStringConverter.TRUE.equals(referralByCFC)) &&
                    !StringUtils.isEmpty(value)) {
                StringBuilder errorDetail = new StringBuilder();
                errorDetail.append("Early Intervention Ind: ");
                errorDetail.append(earlyInter);
                errorDetail.append(", Referral by CFC Ind: ");
                errorDetail.append(referralByCFC);
                errorDetail.append(", Reason For Delay: ");
                errorDetail.append(value);

                errors.add(new StateReportValidationError(entity, field, VALIDATION_ERROR, errorDetail.toString()));
            }
            return errors;
        }
    }

    /**
     * Validation rule: "data is valid only when Early Intervention, Referral by CFC, and IEP =
     * Yes".
     */
    protected class ValidateIEPCompletionDate implements FieldValidator {
        private final String VALIDATION_ERROR = "Student's [IEP COMP DATE] should be filled only then "
                + ALIAS_EARLY_INTERVENTION + " and " + ALIAS_REFERRAL_BY_CFC
                + " and " + ALIAS_IEP_IND + " set to \"yes\".";

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
            String earlyInter = entity.getFieldValue(COLUMN_EARLY_INTERVENTION);
            String referralByCFC = entity.getFieldValue(COLUMN_REFERRAL_BY_CFC);
            String indicatorIEP = (String) entity.getBean().getFieldValueByAlias(ALIAS_IEP_IND);

            if ((!BooleanAsStringConverter.TRUE.equals(earlyInter) ||
                    !BooleanAsStringConverter.TRUE.equals(referralByCFC) ||
                    !"Active".equals(indicatorIEP)) &&
                    !StringUtils.isEmpty(value)) {
                StringBuilder errorDetail = new StringBuilder();
                errorDetail.append("Early Intervention Ind: ");
                errorDetail.append(earlyInter);
                errorDetail.append(", Referral by CFC Ind: ");
                errorDetail.append(referralByCFC);
                errorDetail.append(", IEP Ind: ");
                errorDetail.append(indicatorIEP);
                errorDetail.append(", IEP Completion Date: ");
                errorDetail.append(value);

                errors.add(new StateReportValidationError(entity, field, VALIDATION_ERROR, errorDetail.toString()));
            }
            return errors;
        }
    }

    /**
     * Validation rule: "data is valid only when Early Intervention, Referral by CFC, and IEP =
     * Yes".
     */
    protected class ValidateDateServicesBegan implements FieldValidator {
        private final String VALIDATION_ERROR = "Student's [DOE SERV DATE] should be filled only then "
                + ALIAS_EARLY_INTERVENTION + " and " + ALIAS_REFERRAL_BY_CFC
                + " and " + ALIAS_IEP_IND + " set to \"yes\".";

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
            String earlyInter = entity.getFieldValue(COLUMN_EARLY_INTERVENTION);
            String referralByCFC = entity.getFieldValue(COLUMN_REFERRAL_BY_CFC);
            String indicatorIEP = (String) entity.getBean().getFieldValueByAlias(ALIAS_IEP_IND);

            if ((!BooleanAsStringConverter.TRUE.equals(earlyInter) ||
                    !BooleanAsStringConverter.TRUE.equals(referralByCFC) ||
                    !"Active".equals(indicatorIEP)) &&
                    !StringUtils.isEmpty(value)) {
                StringBuilder errorDetail = new StringBuilder();
                errorDetail.append("Early Intervention Ind: ");
                errorDetail.append(earlyInter);
                errorDetail.append(", Referral by CFC Ind: ");
                errorDetail.append(referralByCFC);
                errorDetail.append(", IEP Ind: ");
                errorDetail.append(indicatorIEP);
                errorDetail.append(", Services Began Date: ");
                errorDetail.append(value);

                errors.add(new StateReportValidationError(entity, field, VALIDATION_ERROR, errorDetail.toString()));
            }
            return errors;
        }
    }

    /**
     * Validation rule: "data is valid only when Early Intervention = Yes".
     */
    protected class ValidateReferralByCFC implements FieldValidator {
        private final String CFC_ERROR = "Student's " + ALIAS_REFERRAL_BY_CFC + " should be filled only then "
                + ALIAS_EARLY_INTERVENTION + " set to \"yes\".";

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
            String earlyInter = entity.getFieldValue(COLUMN_EARLY_INTERVENTION);
            if (!BooleanAsStringConverter.TRUE.equals(earlyInter) && !StringUtils.isEmpty(value)) {
                StringBuilder errorDetail = new StringBuilder();
                errorDetail.append("Early Intervention Ind: ");
                errorDetail.append(earlyInter);
                errorDetail.append(", Referral by CFC Ind: ");
                errorDetail.append(value);

                errors.add(new StateReportValidationError(entity, field, CFC_ERROR, errorDetail.toString()));
            }
            return errors;
        }
    }

    /**
     * Initialize.
     *
     * @throws X2BaseException
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() throws X2BaseException {
        super.initialize();
        m_isChildhood = (Boolean) getParameter(PARAM_IS_CHILDHOOD) == null ? Boolean.FALSE
                : (Boolean) getParameter(PARAM_IS_CHILDHOOD);
        m_isChildhoodV2 = getParameter(PARAM_IS_CHILDHOOD_V2) == null ? false
                : ((Boolean) getParameter(PARAM_IS_CHILDHOOD_V2)).booleanValue();
        m_isCo = PROCEDURE_ID_CO.equals(getParameter(INPUT_PARAM_PROCEDURE_ID));

        if (m_isChildhood.booleanValue()) {
            loadGradeCodes();
        }
        initializeFields();
        if (m_isCo) {
            initializeFieldsForUDA();
        }
        setEntityClass(StudentDemographicEntity.class);
        m_startDate = (PlainDate) getParameter(PARAM_START_DATE);
        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        if (m_reportDate == null) {
            m_reportDate = new PlainDate();
        }

        m_helper = new StudentHistoryHelper(this);
        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_SNAPSHOT);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);

        if (m_isChildhoodV2) {
            Collection<String> gradeCodesForV2 = new ArrayList<>();
            for (Entry<String, ReferenceCode> entry : m_referenceGradeCodeMap.entrySet()) {
                if ("14".equals(entry.getValue().getStateCode())) {
                    gradeCodesForV2.add(entry.getKey());
                }
            }
            if (!gradeCodesForV2.isEmpty()) {
                m_helper.getStudentCriteria().addIn(SisStudent.COL_GRADE_LEVEL, gradeCodesForV2);
            } else {
                m_helper.getStudentCriteria().addEqualTo(SisStudent.COL_GRADE_LEVEL, "__dummy__");
            }
        }

        applyInputCriteria(m_helper.getStudentCriteria(), true, null);

        m_helperSched = new StudentHistoryHelper(this);
        m_helperSched.setStudentSelectionMode(StudentHistoryHelper.MODE_SCHEDULE_SPANS);
        m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_startDate);
        m_helperSched.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
        applyInputCriteria(m_helperSched.getStudentCriteria(), true, null);

        // Set the query to be used for student selection.
        QueryByCriteria studentQuery = m_helper.getStudentQuery(false);
        SubQuery subQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        setQuery(studentQuery);

        loadHMLSProgramsByStudent(subQuery);

        // Get enrollment reference codes for use in the entry type
        String referenceTableOid = PreferenceManager.getPreferenceValue(getOrganization(),
                SisPreferenceConstants.ENROLLMENT_ENTRY_CODES);
        if (referenceTableOid != null) {
            X2Criteria enrollmentCriteria = new X2Criteria();
            enrollmentCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, referenceTableOid);
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, enrollmentCriteria);
            m_enrollmentCodes = getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 5);
        }

        // Get race code reference codes for use in the race retriever.
        DataDictionaryField raceCodeField = getDataDictionaryField(Race.class, Race.COL_RACE_CODE);
        if ((raceCodeField != null) && !StringUtils.isEmpty(raceCodeField.getReferenceTableOid())) {
            m_raceCodes = getReferenceCodes(raceCodeField.getReferenceTableOid());
        }

        // Map of student ACCESS scores by studentOid
        Criteria assessmentCriteria = new Criteria();
        assessmentCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER +
                AssessmentDefinition.COL_NAME, "ACCESS");
        assessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, subQuery);
        QueryByCriteria assessmentQuery = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
        assessmentQuery.addOrderByAscending(StudentAssessment.COL_DATE);

        Map<String, ReferenceCode> programCode = new HashMap<String, ReferenceCode>();
        DataDictionaryField programCodeField = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        if ((programCodeField != null) && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, programCriteria);
            programCode = getBroker().getMapByQuery(query, ReferenceCode.COL_STATE_CODE, 4);
        }
        String ellCode = programCode.get("ELL").getCode();

        // Map of student program participations that has "ELL" as its code by
        // studentOid
        Criteria participationCriteria = new Criteria();
        participationCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, ellCode);
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
        QueryByCriteria participationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, participationCriteria);
        participationQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_participationMap = getBroker().getGroupedCollectionByQuery(participationQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 512);

        // Map of IepData on report date with active status
        X2Criteria activeIepCriteria = new X2Criteria();

        X2Criteria endDateCriteria = new X2Criteria();
        endDateCriteria.addGreaterOrEqualThan(IepData.COL_END_DATE, m_reportDate);
        X2Criteria orCriteria = new X2Criteria();
        orCriteria.addEmpty(IepData.COL_END_DATE, getBroker().getPersistenceKey());
        endDateCriteria.addOrCriteria(orCriteria);

        activeIepCriteria.addLessOrEqualThan(IepData.COL_START_DATE, m_reportDate);
        activeIepCriteria.addAndCriteria(endDateCriteria);

        activeIepCriteria.addIn(IepData.COL_STATUS_CODE, Arrays.asList(
                Integer.valueOf(IepData.StatusCode.ACTIVE.ordinal()),
                Integer.valueOf(IepData.StatusCode.PREVIOUS.ordinal())));
        QueryByCriteria iepQuery = new QueryByCriteria(IepData.class, activeIepCriteria);
        m_stdActiveIepMap = getBroker().getMapByQuery(iepQuery, IepData.COL_STUDENT_OID, 200);

        DataDictionaryField iepStatusField = getDataDictionaryField(SisStudent.class, SisStudent.COL_SPED_STATUS_CODE);
        if (iepStatusField != null && !StringUtils.isEmpty(iepStatusField.getReferenceTableOid())) {
            Map<String, ReferenceCode> referenceCodes = getReferenceCodes(iepStatusField.getReferenceTableOid());
            Collection<ReferenceCode> codes = referenceCodes.values();
            for (ReferenceCode code : codes) {
                String stateCode = code.getStateCode();
                if (ACTIVE_IEP_CODE.equals(stateCode)) {
                    m_activeIepCodes.add(code.getCode());
                }
            }
        }

        // Map of student program participations that has "FARMS" as its state code by
        // studentOid

        DataDictionaryField pgmCodeField = getDataDictionaryField(StudentProgramParticipation.class,
                StudentProgramParticipation.COL_PROGRAM_CODE);
        ReferenceTable pgmCodesRefTable = pgmCodeField.getReferenceTable();
        ArrayList<String> stateFarmsCodes = new ArrayList<String>();
        if (pgmCodesRefTable != null) {
            Collection<ReferenceCode> pgmCodes = pgmCodesRefTable.getReferenceCodes();
            for (ReferenceCode pgmCode : pgmCodes) {
                if ("FARMS".equals(pgmCode.getStateCode())) {
                    stateFarmsCodes.add(pgmCode.getCode());
                }
            }
        }

        X2Criteria programsLowIncCriteria = new X2Criteria();
        programsLowIncCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE, m_reportDate);
        X2Criteria pgmEndDateCriteria = new X2Criteria();
        pgmEndDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
        X2Criteria pgmEndDateNullCriteria = new X2Criteria();
        pgmEndDateNullCriteria.addNotEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
        pgmEndDateCriteria.addOrCriteria(pgmEndDateNullCriteria);
        programsLowIncCriteria.addAndCriteria(pgmEndDateCriteria);
        programsLowIncCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, stateFarmsCodes);
        programsLowIncCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, subQuery);
        QueryByCriteria programLowIncQuery =
                new QueryByCriteria(StudentProgramParticipation.class, programsLowIncCriteria);
        programLowIncQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_lowIncMap = getBroker().getGroupedCollectionByQuery(programLowIncQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 2048);

        // Additional rule for secondary OUTPLACEMENT school
        X2Criteria secondaryOutplacementCriteria = new X2Criteria();
        SubQuery studentSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
        secondaryOutplacementCriteria.addIn(StudentSchool.COL_STUDENT_OID, studentSubQuery);
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.REL_SCHOOL + PATH_DELIMITER + SisSchool.COL_NAME,
                "OUTPLACEMENT");
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_TYPE, Integer.valueOf(StudentSchool.SECONDARY));
        secondaryOutplacementCriteria.addEqualTo(StudentSchool.COL_DISTRICT_CONTEXT_OID,
                getOrganization().getCurrentContextOid());
        secondaryOutplacementCriteria.addLessOrEqualThan(StudentSchool.COL_START_DATE, m_reportDate);
        X2Criteria sskEndDate = new X2Criteria();
        sskEndDate.addGreaterOrEqualThan(StudentSchool.COL_END_DATE, m_reportDate);
        X2Criteria sskEndDateNull = new X2Criteria();
        sskEndDateNull.addEmpty(StudentSchool.COL_END_DATE, getBroker().getPersistenceKey());
        sskEndDate.addOrCriteria(sskEndDateNull);
        secondaryOutplacementCriteria.addAndCriteria(sskEndDate);

        QueryByCriteria secondaryOutplacementQuery =
                new QueryByCriteria(StudentSchool.class, secondaryOutplacementCriteria);
        QueryIterator iter = getBroker().getIteratorByQuery(secondaryOutplacementQuery);
        try {
            while (iter.hasNext()) {
                StudentSchool item = (StudentSchool) iter.next();
                m_secondaryOutplacementSchoolMap.put(item.getStudentOid(),
                        (String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool));
            }
        } finally {
            iter.close();
        }

        // Build a map of calculations/retrievers.
        HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
        calcs.put("DEMO-STRIPCHAR", new RetrieveStripNameChar());
        calcs.put("DEMO-RACE", new RetrieveRace());
        calcs.put("DEMO-RCDTS", new RetrieveRcdts());
        calcs.put("DEMO-ENTRY-DATE", new RetrieveEntryDate());
        calcs.put("DEMO-ENTRY-TYPE", new RetrieveEntryType());
        calcs.put("DEMO-ENTRY-TYPE-V2", new RetrieveEntryTypeV2());
        calcs.put("DEMO-NATIVE-LANG", new RetrieveNativeLang());
        calcs.put("DEMO-LEP", new RetrieveLep());
        calcs.put("DEMO-FTE", new RetrieveFte());
        calcs.put("BIOMOTHER-DOB", new RetrieveBiomotherDOB());
        calcs.put("DEMO-IEP", new RetrieveIep());
        calcs.put("DEMO-LOW-INC", new RetrieveLowIncInd());
        calcs.put("DEMO-IDEA", new RetrieveIDEA());
        calcs.put("TITLE-1-IND-V2", new RetrieveTitle1ForV2Childhood());
        calcs.put("FRL-LOW-IND-V2", new RetrieveFrlLowIndForV2Childhood());
        calcs.put("DEMO-HMLS-IND", new RetrieveHomelessInd());
        calcs.put("DEMO-REF-CFC", new RetrieveReferralByCFC());
        calcs.put(RetrieveRatingEntry.CALC_ID, new RetrieveRatingEntry());
        calcs.put(RetrieveRatingProgress.CALC_ID, new RetrieveRatingProgress());
        super.addCalcs(calcs);

        // Build a map of validators
        HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
        validators.put("DEMO-LANG", new ValidateLang());
        validators.put("DEMO-V-ENTRY-DATE", new ValidateEntryDate());
        validators.put("DEMO-FTE", new ValidateFte());
        validators.put("DEMO-AGE", new ValidateStudentAge());
        validators.put("DEMO-CAREER-TECH", new ValidateCareerTechEd());
        validators.put("DEMO-TITLE1", new ValidateTitle1Age());
        validators.put("DEMO-REFERRAL-BY-CFC", new ValidateReferralByCFC());
        validators.put("DEMO-EI-NUMBER", new ValidateEINumber());
        validators.put("DEMO-ELIG-DATE", new ValidateEligDeterDate());
        validators.put("DEMO-REASON-DELAY", new ValidateReasonForDelay());
        validators.put("DEMO-IEP-COMPL-DATE", new ValidateIEPCompletionDate());
        validators.put("DEMO-DATE-SERVICE", new ValidateDateServicesBegan());
        super.addValidators(validators);
    }

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        String exportType = null;

        if (m_isChildhood != null && m_isChildhood.booleanValue()) {
            exportType = m_isChildhoodV2 ? EXPORT_TYPE_CHILD_V2 : EXPORT_TYPE_CHILD;
        } else {
            exportType = EXPORT_TYPE_DEMO;
        }

        if (m_isCo) {
            exportType = EXPORT_TYPE_CHILD_OUTCOMES;
        }

        StringBuilder heading = new StringBuilder(100);
        heading.append(exportType);
        heading.append(',');
        heading.append(m_totalStudentCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        if (m_reportDate != null) {
            heading.append(m_dateFormat.format(m_reportDate));
        }
        heading.append(',');
        if (getOrganization() != null) {
            heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        heading.append("\n");

        return heading.toString();
    }

    /**
     * If an ACCESS score exists with overall proficiency level of 4.8 or higher
     * AND with literacy proficiency level of 4.2 or higher
     *
     * Used by RetrieveLep
     *
     * @param studentOid String
     * @return true if overall proficiency level < 4.8 AND literacy proficiency level < 4.2
     */
    protected boolean checkAccessScore(String studentOid) {
        StudentAssessment assessment = (StudentAssessment) m_assessmentMap.get(studentOid);
        boolean result = false;
        if (assessment != null) {
            try {
                // TODO: Change when we can use extended dictionaries
                double overallProfLvl = Double.parseDouble(assessment.getFieldA011());
                double literacyProfLvl = Double.parseDouble(assessment.getFieldA013());
                if ((overallProfLvl < 4.8) || (literacyProfLvl < 4.2)) {
                    result = true;
                }
            } catch (NullPointerException npe) {
                result = false;
            } catch (NumberFormatException nfe) {
                result = false;
            }
        }
        return result;
    }

    /**
     * Calculate grade code based on students yog.
     *
     * @param student SisStudent
     * @param yog int
     * @return Reference code
     */
    protected ReferenceCode getGradeLevel(SisStudent student, int yog) {
        ReferenceCode gradeCode = null;
        if (yog == student.getYog()) {
            String gradeLevel = student.getGradeLevel();
            gradeCode = m_referenceGradeCodeMap.get(gradeLevel);
        } else {
            ModelBroker broker = new ModelBroker(getPrivilegeSet());
            TreeMap sortedGradeLevels = StudentManager.buildGradeLevelMap(broker);
            int maxGradeLevel = StudentManager.getMaxGradeLevel(getBroker());
            List<String> matchingGradeLevels = StudentManager.getMatchingGradeLevels(maxGradeLevel, yog,
                    getCurrentContext().getSchoolYear(), sortedGradeLevels);
            for (String matchingGradeLevel : matchingGradeLevels) {
                gradeCode = m_referenceGradeCodeMap.get(matchingGradeLevel);
                if (gradeCode != null) {
                    break;
                }
            }
        }
        return gradeCode;
    }

    /**
     * Gets the student entry rating.
     *
     * @param oid String
     * @return UserDefinedTableA
     */
    protected UserDefinedTableA getStudentEntryRating(String oid) {
        if (m_stdEntryRatingsMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(m_udaRatingDate, getBroker().getPersistenceKey());
            criteria.addEqualTo(m_udaRatingType, RATING_TYPE_ENTRY);
            criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID_RATINGS);
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
            criteria.addIn(UserDefinedTableA.COL_STUDENT_OID, stdSubQuery);
            QueryByCriteria query = new QueryByCriteria(UserDefinedTableA.class, criteria);
            query.addOrderByAscending(m_udaRatingDate);
            m_stdEntryRatingsMap =
                    getBroker().getMapByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 1024);
        }
        return m_stdEntryRatingsMap.get(oid);
    }

    /**
     * Gets the student progress rating.
     *
     * @param oid String
     * @return UserDefinedTableA
     */
    protected UserDefinedTableA getStudentProgressRating(String oid) {
        if (m_stdProgressRatingsMap == null) {
            X2Criteria criteria = new X2Criteria();
            criteria.addNotEmpty(m_udaRatingDate, getBroker().getPersistenceKey());
            criteria.addEqualTo(m_udaRatingType, RATING_TYPE_PROGRESS);
            criteria.addEqualTo(UserDefinedTableA.REL_EXTENDED_DATA_DICTIONARY + ModelProperty.PATH_DELIMITER
                    + ExtendedDataDictionary.COL_ID, DDX_ID_RATINGS);
            SubQuery stdSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());
            criteria.addIn(UserDefinedTableA.COL_STUDENT_OID, stdSubQuery);
            QueryByCriteria query = new QueryByCriteria(UserDefinedTableA.class, criteria);
            query.addOrderByAscending(m_udaRatingDate);
            m_stdProgressRatingsMap =
                    getBroker().getMapByQuery(query, UserDefinedTableA.COL_STUDENT_OID, 1024);
        }
        return m_stdProgressRatingsMap.get(oid);
    }

    /**
     * Generate the filename for this export.
     *
     * @return <district name>_<report date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        if (getOrganization() != null) {
            fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        }
        fileName.append("_");
        if (m_reportDate != null) {
            fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        }
        fileName.append("_");
        fileName.append("001.txt");

        return fileName.toString();
    }

    /**
     * Check if value by bean path is not empty and return state value.
     *
     * @param data
     * @param std
     * @param path
     * @return
     */
    private String getStateValueByBeanPath(UserDefinedTableA rating,
                                           ILEarlyChildhoodOutcomesData data,
                                           String path) {
        String value = null;
        String code = (String) rating.getFieldValueByBeanPath(path);
        if (!StringUtils.isEmpty(code) && m_udaRefCodesMap.get(path) != null
                && m_udaRefCodesMap.get(path).get(code) != null) {
            value = m_udaRefCodesMap.get(path).get(code).getStateCode();
        }
        return value;
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_activeCode = PreferenceManager.getPreferenceValue(getOrganization(),
                SystemPreferenceDefinition.STUDENT_ACTIVE_CODE);

        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldLowIncInd = translateAliasToJavaName(ALIAS_LOW_INCOME, true);
        m_fieldPrivateSchoolInd = translateAliasToJavaName(ALIAS_PRIVATE_SCHOOL_IND, true);
        m_excludeStdField = translateAliasToJavaName(ALIAS_EXCLUDE_STD, false);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        DataDictionaryField fieldRelationCode = getDataDictionaryField(StudentContact.class,
                StudentContact.COL_RELATIONSHIP_CODE);
        m_relationsCodes = getReferenceCodes(fieldRelationCode.getReferenceTableOid());
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldSklWideTitle = translateAliasToJavaName(ALIAS_SKL_WIDE_TITLE, true);
        m_fieldStdTitle = translateAliasToJavaName(ALIAS_STD_TITLE, true);
        m_fieldSklWideFarms = translateAliasToJavaName(ALIAS_SKL_WIDE_FARMS, true);
        m_fieldSklNonCalcFte = translateAliasToJavaName(ALIAS_SKL_NON_CALC_FTE, true);
        m_fieldEnrTypeForY = translateAliasToJavaName(ALIAS_ENR_TYPE_FOR_Y, true);
        m_referralByCFCField = translateAliasToJavaName(ALIAS_REF_CFC, true);
        m_fieldEnrSklHome = translateAliasToJavaName(ALIAS_ENR_SKL_HOME, true);
        m_fieldEnrSklService = translateAliasToJavaName(ALIAS_ENR_SKL_SERVICE, true);
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFieldsForUDA() {
        DataDictionary stdDictByDdx = DataDictionaryUtils.getExtendedDictionaryById(DDX_ID_RATINGS, getBroker());
        if (stdDictByDdx != null) {
            DataDictionaryField dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_SKL);
            if (dictField != null) {
                m_udaRatingSchool = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaRatingSchool, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_SKL
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_DATE);
            if (dictField != null) {
                m_udaRatingDate = dictField.getJavaName();
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_DATE
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_SOC_RELS);
            if (dictField != null) {
                m_udaSocialRelations = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaSocialRelations, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_SOC_RELS
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_KNOW_SKILLS);
            if (dictField != null) {
                m_udaKnowledgeSkills = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaKnowledgeSkills, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_KNOW_SKILLS
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_KNOW_SKILLS_PR);
            if (dictField != null) {
                m_udaKnowledgeSkillsPR = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaKnowledgeSkillsPR, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_KNOW_SKILLS_PR
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_ACT_MEET_NEEDS);
            if (dictField != null) {
                m_udaActionToMeetNeeds = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaActionToMeetNeeds, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_ACT_MEET_NEEDS
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_ACT_MEET_NEEDS_PR);
            if (dictField != null) {
                m_udaActionToMeetNeedsPR = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaActionToMeetNeedsPR, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_ACT_MEET_NEEDS_PR
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_PAR_INV);
            if (dictField != null) {
                m_udaParentInvolved = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaParentInvolved, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_PAR_INV
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_ADMIN_PART);
            if (dictField != null) {
                m_udaAdminParticipate = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaAdminParticipate, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_ADMIN_PART
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_TEACHER_PART);
            if (dictField != null) {
                m_udaTeacherParticipate = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaTeacherParticipate, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_TEACHER_PART
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_PSYCH_WORKER);
            if (dictField != null) {
                m_udaPsychSocialWorker = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaPsychSocialWorker, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_PSYCH_WORKER
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_LANG_PATH);
            if (dictField != null) {
                m_udaSpeechLangPath = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaSpeechLangPath, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_LANG_PATH
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_OTHER_PROV);
            if (dictField != null) {
                m_udaOtherProvider = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaOtherProvider, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_OTHER_PROV
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_TYPE);
            if (dictField != null) {
                m_udaRatingType = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaRatingType, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_TYPE
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_PR_SOC_RELAT);
            if (dictField != null) {
                m_udaECMadePRSocialRelations = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaECMadePRSocialRelations, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_PR_SOC_RELAT
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
            dictField =
                    stdDictByDdx.findDataDictionaryFieldByAlias(ALIAS_DDX_RAT_PRIM_ASM);
            if (dictField != null) {
                m_udaPrimAssessment = dictField.getJavaName();
                if (dictField.hasReferenceTable()) {
                    m_udaRefCodesMap.put(m_udaPrimAssessment, dictField.getReferenceTable().getCodeMap());
                }
            } else {
                addSetupError("", "Alias = " + ALIAS_DDX_RAT_PRIM_ASM
                        + " was not found for extended dictionary with ID = " + DDX_ID_RATINGS);
            }
        }
    }

    /**
     * Load grade codes.
     */
    private void loadGradeCodes() {
        DataDictionary dictionary = DataDictionary.getDistrictDictionary(getBroker().getPersistenceKey());
        ModelProperty prop =
                new ModelProperty(SisStudent.class, SisStudent.COL_GRADE_LEVEL, getBroker().getPersistenceKey());
        DataDictionaryField field = dictionary.findDataDictionaryField(prop.getFieldId());
        ReferenceTable referenceTable = field.getReferenceTable();
        m_referenceGradeCodeMap = referenceTable.getCodeMap();
    }

    /**
     * Load map of homeless programs keyed on student oid.
     *
     * @param studentSubQuery
     */
    private void loadHMLSProgramsByStudent(SubQuery studentSubQuery) {
        Collection<String> programCodes = new ArrayList<String>();
        DataDictionaryField programCodeField =
                getDataDictionaryField(StudentProgramParticipation.class, StudentProgramParticipation.COL_PROGRAM_CODE);
        if (programCodeField != null && !StringUtils.isEmpty(programCodeField.getReferenceTableOid())) {
            X2Criteria programCriteria = new X2Criteria();
            programCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, programCodeField.getReferenceTableOid());
            programCriteria.addEqualTo(ReferenceCode.COL_STATE_CODE, "HMLS");
            QueryByCriteria query = new QueryByCriteria(ReferenceCode.class, programCriteria);
            programCodes.addAll(getBroker().getMapByQuery(query, ReferenceCode.COL_CODE, 10).keySet());
        }

        // Map of student program participations that has "ELL" as its code by studentOid
        Criteria participationCriteria = new Criteria();
        if (!programCodes.isEmpty()) {
            participationCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, programCodes);
        } else {
            participationCriteria.addEqualTo(StudentProgramParticipation.COL_PROGRAM_CODE, "___dummy___");
        }
        participationCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getStartDate());
        participationCriteria.addLessOrEqualThan(StudentProgramParticipation.COL_START_DATE,
                getCurrentContext().getEndDate());
        participationCriteria.addIn(StudentProgramParticipation.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria participationQuery =
                new QueryByCriteria(StudentProgramParticipation.class, participationCriteria);
        participationQuery.addOrderByDescending(StudentProgramParticipation.COL_START_DATE);
        m_participationHomelessMap = getBroker().getGroupedCollectionByQuery(participationQuery,
                StudentProgramParticipation.COL_STUDENT_OID, 512);
    }

}