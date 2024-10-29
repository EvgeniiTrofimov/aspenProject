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
package com.x2dev.procedures.statereporting.il;

import static com.follett.fsc.core.k12.business.ModelProperty.PATH_DELIMITER;
import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.DistrictSchoolYearContext;
import com.follett.fsc.core.k12.beans.ExportFormatField;
import com.follett.fsc.core.k12.beans.ExtendedDataDictionary;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReferenceCode;
import com.follett.fsc.core.k12.beans.ReferenceTable;
import com.follett.fsc.core.k12.beans.StudentSchool;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.FieldValidator;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.tools.stateexports.StateReportValidationError;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.StringUtils;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;

/**
 * Illinois state export procedure for ELL Screener
 *
 * Searches for student assessments whose names have a local code of "ELL" .
 *
 * @author X2 Development Corporation
 */
public class ELLScreener extends StateReportData {
    /**
     * Implementation of StateReportEntity to be used by the IL.
     * This must be a public static inner class with a public no argument
     * constructor so it can be instantiated through reflection.
     *
     * @author X2 Development Corporation
     */
    public static class ELLEntity extends StateReportEntity {
        private static final String ENG_LNG_000 = "000";
        /**
         * Assessments for this entity.
         */
        StudentAssessment m_assessment = null;

        Collection<StudentProgramParticipation> m_ellPrograms = null;

        /**
         * ENTRY enrollment for this entity.
         */
        StudentEnrollment m_enrollment = null;

        /**
         * ELLScreener data.
         */
        ELLScreener m_esData = null;

        /**
         * Map of the student's RCDTS information
         */
        Map<String, String> m_rcdtsMap = null;

        /**
         * Public no argument constructor for dynamic instantiation.
         */
        public ELLEntity() {
            // public no argument constructor for dynamic instantiation.
        }

        /**
         * Gets the current assessment.
         *
         * @return assessments if student has appropriate assessments, otherwise null.
         */
        public StudentAssessment getCurrentAssessment() {
            return m_assessment;
        }

        /**
         * Gets the current school.
         *
         * @return StudentSchool
         */
        public String getCurrentSchool() {
            String school = null;
            if (m_enrollment != null && m_enrollment.getSchool() != null) {
                school = (String) m_enrollment.getSchool().getFieldValueByBeanPath(m_esData.m_fieldSchoolCode);
            }

            return school;
        }

        /**
         * Generate a display name to print on the validation report for the entity.
         *
         * @return String
         * @see com.x2dev.sis.tools.stateexports.StateReportEntity#getEntityName()
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
         * Initialize and increment counter.
         *
         * @param data StateReportData
         * @param bean X2BaseBean
         * @throws X2BaseException exception
         */
        @Override
        public void intitialize(StateReportData data, X2BaseBean bean) throws X2BaseException {
            super.intitialize(data, bean);
            m_esData = (ELLScreener) data;
            SisStudent student = (SisStudent) bean;
            m_enrollment = m_esData.m_helper.getEnrollmentForDate(student.getOid(), m_esData.m_reportDate, "E");
            if (m_enrollment != null && m_enrollment.getSchool() != null &&
                    !BooleanAsStringConverter.TRUE
                            .equals(m_enrollment.getSchool().getFieldValueByBeanPath(m_esData.m_excludeSklField))
                    &&
                    matchSchoolIfSchoolContext(student)) {
                m_rcdtsMap = lookupOverrides();

                Collection<StudentAssessment> assessments =
                        ((ELLScreener) data).getStudentAssessments(student.getOid());

                m_ellPrograms = m_esData.m_studentsEllPgmsMap.get(student.getOid());

                if (assessments.size() > 0) {
                    // keep track of rows
                    m_esData.m_totalRowCount++;
                    m_assessment = assessments.iterator().next();
                } else {
                    String lngCode = student.getHomeLanguageCode();

                    String stateLngCode = m_esData.lookupReferenceCodeByBeanPath(SisStudent.class,
                            SisStudent.COL_HOME_LANGUAGE_CODE, lngCode, STATE_ORIGINAL);

                    if (stateLngCode != null && !stateLngCode.equals(ENG_LNG_000)) {
                        m_esData.m_totalRowCount++;
                    } else {
                        if (m_ellPrograms == null || m_ellPrograms.isEmpty()) {
                            setRowCount(0);
                        }
                    }
                }
            } else {
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

        /**
         * Returns the Entry enrollment record.
         *
         * @return StudentEnrollment
         */
        public StudentEnrollment getEffectiveEnrollment() {
            return m_enrollment;
        }

        /**
         * Student has assessment.
         *
         * @return true if student has appropriate assessments, otherwise false.
         */
        public boolean studentHasAssessment() {
            return m_assessment != null ? true : false;
        }

        /**
         * Student has ell program.
         *
         * @return true if student has appropriate assessments, otherwise false.
         */
        public boolean studentHasEllProgram() {
            return m_ellPrograms != null ? true : false;
        }

        /**
         * Lookup district and school codes, including overrides on the enrollment record.
         *
         * @return Map
         */
        private Map<String, String> lookupOverrides() {
            Map<String, String> calcValueMap = new HashMap<String, String>();
            ELLScreener esData = (ELLScreener) getData();

            // do state lookups
            String serviceSchoolCode = null;
            if (esData.m_secondaryOutplacementSchoolMap.containsKey(m_enrollment.getStudentOid())) {
                String serviceSchoolRefCode = esData.m_secondaryOutplacementSchoolMap.get(m_enrollment.getStudentOid());
                if (serviceSchoolRefCode != null && !serviceSchoolRefCode.isEmpty()) {
                    serviceSchoolCode =
                            esData.lookupStateValue(StudentEnrollment.class, esData.m_fieldServiceSchoolCode,
                                    serviceSchoolRefCode);
                    calcValueMap.put(ALIAS_SERVICE_SCHOOL_CODE, serviceSchoolCode);
                }
            }

            return calcValueMap;
        }

        /**
         * Match school if school context.
         *
         * @param student SisStudent
         * @return true, if successful
         */
        private boolean matchSchoolIfSchoolContext(SisStudent student) {
            boolean theSameSchool = true;
            if (m_esData.isSchoolContext()) {
                SisSchool school = student.getSchool();
                if (school == null) {
                    theSameSchool = false;
                } else {
                    if (!m_esData.getSchool().equals(school)) {
                        theSameSchool = false;
                    }
                }
            }
            return theSameSchool;
        }
    }

    /*
     * Aliases
     */
    protected static final String ALIAS_DISTRICT_ID = "DOE DISTRICT ID";
    protected static final String ALIAS_DOE_SCHOOL_HOME = "DOE SCHOOL HOME";
    protected static final String ALIAS_DOE_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_DOE_SCRN_LIT_PROF = "DOE SCRN LIT PROF";
    protected static final String ALIAS_DOE_SCRN_LISTEN_PROF = "DOE SCRN LISTEN PROF";
    protected static final String ALIAS_DOE_SCRN_LISTEN_RAW = "DOE SCRN LISTEN RAW";
    protected static final String ALIAS_DOE_SCRN_OVERALL_PROF = "DOE SCRN OVERALL PROF";
    protected static final String ALIAS_DOE_SCRN_ORAL_PROF = "DOE SCRN ORAL PROF";
    protected static final String ALIAS_DOE_SCRN_READ_PROF = "DOE SCRN READ PROF";
    protected static final String ALIAS_DOE_SCRN_READ_RAW = "DOE SCRN READ RAW";
    protected static final String ALIAS_DOE_SCRN_SPEAK_PROF = "DOE SCRN SPEAK PROF";
    protected static final String ALIAS_DOE_SCRN_SPEAK_RAW = "DOE SCRN SPEAK RAW";
    protected static final String ALIAS_DOE_SCRN_WRITE_PROF = "DOE SCRN WRITE PROF";
    protected static final String ALIAS_DOE_SCRN_WRITE_RAW = "DOE SCRN WRITE RAW";
    protected static final String ALIAS_EXCLUDE_SKL = "DOE EXCLUDE SKL";
    protected static final String ALIAS_GRADE_LVL = "DOE GRADE AT TESTING";
    protected static final String ALIAS_RCDTS_FOR_SERVING_SCHOOL = "DOE OUTPLACEMENT DI";
    protected static final String ALIAS_SCHOOL_ID = "DOE SCHOOL ID";
    protected static final String ALIAS_SCR_DATE_TAKEN = "DOE ELL SCREENER DATE TAKEN";
    protected static final String ALIAS_SCR_TAKEN = "DOE ELL SCREENER TAKEN";
    protected static final String ALIAS_SERVICE_SCHOOL_CODE = "DOE SCHOOL SERVICE";
    protected static final String ALIAS_SCRN_SEMESTER = "DOE SCRN SEMESTER";

    private static final String DDX_PGM_ELL_CODE = "PGM-ELL";

    /*
     * Parameters
     */
    protected static final String PARAM_REPORT_DATE = "reportDate";
    protected static final String PARAM_QUERY_BY = "queryBy1";
    protected static final String PARAM_QUERY_STRING = "queryString1";
    protected static final String PARAM_REPORT_START_DATE = "reportStartDate";

    /**
     * Other constants
     */
    protected static final Collection SEMESTER_CODES = Arrays.asList("S1", "S2");
    protected static final String STATE_CODE_HOME_LANGUAGE_ENG = "000";
    /*
     * Instance variables
     */
    protected DistrictSchoolYearContext m_context;
    protected SimpleDateFormat m_dateFormat = new SimpleDateFormat("MM/dd/yyyy");
    protected String m_ddxPgmEllOid;
    protected String m_excludeSklField;
    protected String m_fieldScrnSemester;
    protected String m_fieldAssessmentGradeLvl;
    protected String m_fieldDateTaken;
    protected String m_fieldDistrictCode;
    protected String m_fieldEslInd;
    protected String m_fieldRcdtsForServingSchool;
    protected String m_fieldSchoolCode;
    protected String m_fieldSchoolHome;
    protected String m_fieldSchoolId;
    protected String m_fieldScrTaken;
    protected String m_fieldServiceSchoolCode;
    protected String m_fieldScrnLstRaw;
    protected String m_fieldScrnLstProf;
    protected String m_fieldScrnSpkRaw;
    protected String m_fieldScrnSpkProf;
    protected String m_fieldScrnReadRaw;
    protected String m_fieldScrnReadProf;
    protected String m_fieldScrnWriteRaw;
    protected String m_fieldScrnWriteProf;
    protected String m_fieldScrnLitProf;
    protected String m_fieldScrnOralProf;
    protected String m_fieldScrnOverProf;
    protected StudentHistoryHelper m_helper;
    protected PlainDate m_reportDate;
    protected Map<String, Schedule> m_scheduleMap;
    protected Map<String, String> m_secondaryOutplacementSchoolMap = new HashMap<String, String>();
    protected Map<String, Collection<StudentAssessment>> m_studentsAssessmentsMap;
    protected Map<String, Collection<StudentProgramParticipation>> m_studentsEllPgmsMap;
    protected PlainDate m_reportStartDate;
    protected SimpleDateFormat m_sqlDateFormat = new SimpleDateFormat("yyyy-MM-dd");
    private Map<PeriodSchoolYearContext, DistrictSchoolYearContext> m_yearContextMap =
            new HashMap<PeriodSchoolYearContext, DistrictSchoolYearContext>();

    private Map<String, Integer> m_schoolMaxGradeMap = new HashMap<String, Integer>();
    protected Map<String, ScheduleTermDate> m_termDatesMap;
    protected Map<String, Collection<ScheduleTerm>> m_termMap;

    /**
     * Keep track of number of ESL assessments
     */
    int m_totalRowCount;

    /**
     * A map of student enrollments (Collection[StudentEnrollment]) by student oid
     *
     * Sorted in descending order. (First entry is recent, last entry is earliest)
     */
    Map<String, List<StudentEnrollment>> m_enrollmentMap;


    /**
     * Retrieves data from current StudentAssessment.
     */
    protected class Assessment implements FieldRetriever {
        public final static String PARAM_DATE_TAKEN = "DATE_TKN";
        public final static String PARAM_FINAL_DET = "FNL_DET";
        public final static String PARAM_GRADE_LVL = "GRD_LVL";
        public final static String PARAM_LIST_LVL = "LIST_LVL";
        public final static String PARAM_LIST_SCORE = "LIST_SCORE";
        public final static String PARAM_LITER_LVL = "LITER_LVL";
        public final static String PARAM_ORAL_LVL = "ORAL_LVL";
        public final static String PARAM_OVERALL_CPL = "OVERALL_CPL";
        public final static String PARAM_READING_LVL = "READ_LVL";
        public final static String PARAM_READING_SCORE = "READ_SCORE";
        public final static String PARAM_SCR_TAKEN = "SCR_TKN";
        public final static String PARAM_SEMESTER = "SEMESTER";
        public final static String PARAM_SPEAK_LVL = "SPEAK_LVL";
        public final static String PARAM_SPEAK_SCORE = "SPEAK_SCORE";
        public final static String PARAM_WRITING_LVL = "WRIT_LVL";
        public final static String PARAM_WRITING_SCORE = "WRIT_SCORE";

        private static final String EMPTY = "";
        private static final String PROGRAM_CODE_ELL = "ELL";
        private static final String ZERO = "0";
        private static final String _01 = "01";
        private static final String _02 = "02";

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field)
                throws X2BaseException {
            Object retrievedValue = null;

            ELLEntity ellScrEntity = (ELLEntity) entity;
            ELLScreener ellScrData = (ELLScreener) data;
            SisStudent student = (SisStudent) ellScrEntity.getBean();

            StudentAssessment currentAssessment = ellScrEntity.getCurrentAssessment();

            if (PARAM_SEMESTER.equals(field.getParameter())) {

                String scrnSemester = null;
                if (ellScrEntity.studentHasAssessment()) {
                    scrnSemester = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnSemester);
                    AssessmentColumnDefinition columnDefinition = getStdAssesmentColumnByAlias(ALIAS_SCRN_SEMESTER);
                    scrnSemester = lookupReferenceCodeByRefTbl(columnDefinition.getReferenceTableOid(), scrnSemester,
                            STATE_ORIGINAL);

                }

                if (!StringUtils.isEmpty(scrnSemester)) {
                    retrievedValue = scrnSemester;
                } else {
                    String currSchoolOid = ((SisStudent) ellScrEntity.getBean()).getSchool().getOid();
                    Schedule schedule = m_scheduleMap.get(currSchoolOid);

                    if (schedule != null) {
                        Collection<ScheduleTerm> terms = m_termMap.get(schedule.getOid());

                        if (terms != null && !terms.isEmpty()) {
                            for (ScheduleTerm term : terms) {
                                if (m_termDatesMap.containsKey(term.getOid())) {
                                    String code = term.getCode();
                                    retrievedValue = lookupStateValue(term.getClass(), ScheduleTerm.COL_CODE, code);
                                    break;
                                }
                            }
                        }
                    }
                }
            } else if (PARAM_DATE_TAKEN.equals(field.getParameter())) {
                retrievedValue = m_context.getStartDate();

                if (ellScrEntity.studentHasAssessment()) {
                    PlainDate dateTaken = (PlainDate) currentAssessment.getFieldValueByBeanPath(m_fieldDateTaken);

                    retrievedValue = dateTaken;
                } else {

                    StudentEnrollment studentEnrollment = getNearEnrollment(student.getEnrollments(),
                            new ArrayList<String>(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE)),
                            m_reportDate, ellScrData, true);
                    if (studentEnrollment == null) {
                        studentEnrollment = getNearEnrollment(student.getEnrollments(), new ArrayList<String>(
                                Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE)),
                                m_reportDate, ellScrData, false);
                    }
                    if (studentEnrollment != null) {
                        if (studentEnrollment.getEnrollmentDate().after((Date) retrievedValue)) {
                            retrievedValue = studentEnrollment.getEnrollmentDate();
                        }
                    }
                }
            }

            else if (PARAM_GRADE_LVL.equals(field.getParameter())) {
                if (ellScrEntity.studentHasAssessment()) {
                    String gradeLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldAssessmentGradeLvl);

                    if (gradeLvl == null) {
                        PlainDate takenDate = (PlainDate) currentAssessment.getFieldValueByBeanPath(m_fieldDateTaken);
                        if (takenDate != null) {
                            gradeLvl = calculateGradeLvlByTakenDate(student, ellScrData, takenDate);
                        }
                    }

                    String stateValueOfGradeLvl = ellScrData.lookupStateValue(StudentAssessment.class,
                            m_fieldAssessmentGradeLvl,
                            gradeLvl);
                    retrievedValue = stateValueOfGradeLvl;
                } else {
                    String gradeLvl = ((SisStudent) ellScrEntity.getBean()).getGradeLevel();
                    String stateValueOfGradeLvl = ellScrData.lookupStateValue(SisStudent.class,
                            SisStudent.COL_GRADE_LEVEL,
                            gradeLvl);

                    retrievedValue = stateValueOfGradeLvl;
                }
            }
            if (ellScrEntity.studentHasAssessment()) {
                if (PARAM_SCR_TAKEN.equals(field.getParameter())) {
                    String scrTaken = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrTaken);
                    AssessmentColumnDefinition columnDefinition = getStdAssesmentColumnByAlias(ALIAS_SCR_TAKEN);

                    String stateValueOfSrcTaken = lookupReferenceCodeByRefTbl(columnDefinition.getReferenceTableOid(),
                            scrTaken, ExportFormatField.ReferenceMapTypeCode.STATE.ordinal());

                    retrievedValue = stateValueOfSrcTaken;
                } else if (PARAM_LIST_SCORE.equals(field.getParameter())) {
                    String listScore = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnLstRaw);

                    retrievedValue = listScore;
                } else if (PARAM_LIST_LVL.equals(field.getParameter())) {
                    String listLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnLstProf);

                    retrievedValue = listLvl;
                } else if (PARAM_SPEAK_SCORE.equals(field.getParameter())) {
                    String speakScore = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnSpkRaw);

                    retrievedValue = speakScore;
                } else if (PARAM_SPEAK_LVL.equals(field.getParameter())) {
                    String speakLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnSpkProf);

                    retrievedValue = speakLvl;
                } else if (PARAM_READING_SCORE.equals(field.getParameter())) {
                    String readScore = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnReadRaw);

                    retrievedValue = readScore;
                } else if (PARAM_READING_LVL.equals(field.getParameter())) {
                    String readLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnReadProf);

                    retrievedValue = readLvl;
                } else if (PARAM_WRITING_SCORE.equals(field.getParameter())) {
                    String writScore = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnWriteRaw);

                    retrievedValue = writScore;
                } else if (PARAM_WRITING_LVL.equals(field.getParameter())) {
                    String writLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnWriteProf);

                    retrievedValue = writLvl;
                } else if (PARAM_FINAL_DET.equals(field.getParameter())) {
                    retrievedValue = _02;

                    StudentEnrollment studentEnrollment = getNearEnrollment(student.getEnrollments(),
                            new ArrayList<String>(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE)),
                            m_reportDate, ellScrData, true);
                    if (studentEnrollment == null) {
                        studentEnrollment = getNearEnrollment(student.getEnrollments(), new ArrayList<String>(
                                Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE)),
                                m_reportDate, ellScrData, false);
                    }
                    if (studentEnrollment != null) {
                        for (StudentProgramParticipation participation : student.getProgramParticipation()) {
                            if (PROGRAM_CODE_ELL.equals(participation.getProgramCode()) ||
                                    (!StringUtils.isEmpty(m_ddxPgmEllOid)
                                            && m_ddxPgmEllOid.equals(participation.getExtendedDataDictionaryOid()))) {
                                if (m_reportDate.after(participation.getStartDate()) &&
                                        (participation.getEndDate() == null ||
                                                m_reportDate.before(participation.getEndDate()) ||
                                                m_reportDate.equals(participation.getEndDate()))) {
                                    retrievedValue = _01;
                                    break;
                                }
                            }
                        }
                    }
                } else if (PARAM_ORAL_LVL.equals(field.getParameter())) {
                    String oralLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnOralProf);

                    retrievedValue = oralLvl;
                } else if (PARAM_OVERALL_CPL.equals(field.getParameter())) {
                    String overCPL = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnOverProf);

                    retrievedValue = overCPL;
                } else if (PARAM_LITER_LVL.equals(field.getParameter())) {
                    String literLvl = (String) currentAssessment.getFieldValueByBeanPath(m_fieldScrnLitProf);

                    retrievedValue = literLvl;
                }
            } else if (ellScrEntity.studentHasEllProgram()) {
                if (PARAM_FINAL_DET.equals(field.getParameter())) {
                    retrievedValue = _01;
                }
            }

            return retrievedValue;
        }

        /**
         * Calculate grade lvl by taken date.
         *
         * @param student SisStudent
         * @param ellScrData ELLScreener
         * @param takenDate PlainDate
         * @return String
         */
        private String calculateGradeLvlByTakenDate(SisStudent student, ELLScreener ellScrData, PlainDate takenDate) {
            String returnValue = null;
            List<StudentEnrollment> enrollments = m_enrollmentMap.get(student.getOid());



            StudentEnrollment nearEnrollmentRecord = getNearEnrollment(enrollments,
                    new ArrayList<String>(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE)),
                    takenDate, ellScrData, true);

            if (nearEnrollmentRecord == null) {
                nearEnrollmentRecord = getNearEnrollment(enrollments,
                        new ArrayList<String>(Arrays.asList(StudentEnrollment.ENTRY, StudentEnrollment.YOG_CHANGE)),
                        takenDate, ellScrData, false);
            }

            if (nearEnrollmentRecord != null) {

                int maxGradeLvl = ellScrData.getSchoolMaxGradeMap().get(nearEnrollmentRecord.getSchoolOid()).intValue();
                int yogInNearEnrollment = nearEnrollmentRecord.getYog();

                Integer enrollmentYear =
                        getSchoolYearByDate(nearEnrollmentRecord.getEnrollmentDate(), ellScrData.getYearContextMap());
                Integer takenYear = getSchoolYearByDate(takenDate, ellScrData.getYearContextMap());

                if (enrollmentYear != null && takenYear != null) {
                    int gradelvlInNearEnrollment = maxGradeLvl - (yogInNearEnrollment - enrollmentYear.intValue());
                    int takenGradeLvl = gradelvlInNearEnrollment + (takenYear.intValue() - enrollmentYear.intValue());

                    if (takenGradeLvl < 10) {
                        returnValue = ZERO + takenGradeLvl;
                    } else {
                        returnValue = EMPTY + takenGradeLvl;
                    }
                }
            }


            return returnValue;
        }

        /**
         * Gets the near enrollment.
         *
         * @param enrollments Collection<StudentEnrollment>
         * @param types List<String>
         * @param takenDate PlainDate
         * @param ellScrData ELLScreener
         * @param isNearNext boolean
         * @return Student enrollment
         */
        private StudentEnrollment getNearEnrollment(Collection<StudentEnrollment> enrollments,
                                                    List<String> types,
                                                    PlainDate takenDate,
                                                    ELLScreener ellScrData,
                                                    boolean isNearNext) {
            StudentEnrollment nearEnrollmentRecord = null;
            for (StudentEnrollment enrollment : enrollments) {
                String enrollmentType = enrollment.getEnrollmentType();
                if (types.contains(enrollmentType) &&
                        ellScrData.getSchoolMaxGradeMap().containsKey(enrollment.getSchoolOid())) {
                    PlainDate enrollDate = enrollment.getEnrollmentDate();
                    if (!isNearNext && enrollDate != null && !enrollDate.after(takenDate)) {
                        nearEnrollmentRecord = enrollment;
                        break;
                    }

                    if (isNearNext && enrollDate != null && !enrollDate.before(takenDate)) {
                        if (nearEnrollmentRecord == null
                                || !enrollDate.after(nearEnrollmentRecord.getEnrollmentDate())) {
                            nearEnrollmentRecord = enrollment;
                        }

                    }

                }
            }

            return nearEnrollmentRecord;
        }

        /**
         * Gets the school year by date.
         *
         * @param date PlainDate
         * @param yearContextMap Map<PeriodSchoolYearContext,DistrictSchoolYearContext>
         * @return Integer
         */
        private Integer getSchoolYearByDate(PlainDate date,
                                            Map<PeriodSchoolYearContext, DistrictSchoolYearContext> yearContextMap) {
            Integer returnValue = null;
            DistrictSchoolYearContext schoolYearContext = null;
            for (PeriodSchoolYearContext period : yearContextMap.keySet()) {
                if (period.isBetween(date)) {
                    schoolYearContext = yearContextMap.get(period);
                    break;
                }
            }
            if (schoolYearContext == null) {
                Criteria criteria = new X2Criteria();
                criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, date);
                criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, date);
                QueryByCriteria byCriteria = new QueryByCriteria(DistrictSchoolYearContext.class, criteria);
                schoolYearContext = (DistrictSchoolYearContext) getBroker().getBeanByQuery(byCriteria);
                if (schoolYearContext != null) {
                    PeriodSchoolYearContext period =
                            new PeriodSchoolYearContext(schoolYearContext.getStartDate(),
                                    schoolYearContext.getEndDate());
                    yearContextMap.put(period, schoolYearContext);
                }
            }

            if (schoolYearContext != null) {
                returnValue = Integer.valueOf(schoolYearContext.getSchoolYear());
            }

            return returnValue;
        }
    }

    /**
     * The Class PeriodSchoolYearContext.
     */
    protected class PeriodSchoolYearContext {
        private PlainDate m_start;
        private PlainDate m_end;

        /**
         * Instantiates a new period school year context.
         *
         * @param start PlainDate
         * @param end PlainDate
         */
        public PeriodSchoolYearContext(PlainDate start, PlainDate end) {
            m_start = start;
            m_end = end;
        }

        /**
         * Checks if is between.
         *
         * @param date PlainDate
         * @return true, if is between
         */
        public boolean isBetween(PlainDate date) {
            boolean returnValue = false;
            if (m_start != null && m_end != null && date != null &&
                    (m_start.before(date) || m_start.equals(date)) &&
                    (m_end.after(date) || m_end.equals(date))) {
                returnValue = true;
            }
            return returnValue;
        }

        /**
         * @see java.lang.Object#toString()
         */
        @Override
        public String toString() {
            return "PeriodSchoolYearContext class " + m_start + " " + m_end;
        }

        /**
         * @see java.lang.Object#hashCode()
         */
        /*
         *
         */
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + getOuterType().hashCode();
            result = prime * result + ((m_end == null) ? 0 : m_end.hashCode());
            result = prime * result + ((m_start == null) ? 0 : m_start.hashCode());
            return result;
        }

        /**
         * @see java.lang.Object#equals(java.lang.Object)
         */
        /*
         *
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            PeriodSchoolYearContext other = (PeriodSchoolYearContext) obj;
            if (!getOuterType().equals(other.getOuterType())) {
                return false;
            }
            if (m_end == null) {
                if (other.m_end != null) {
                    return false;
                }
            } else if (!m_end.equals(other.m_end)) {
                return false;
            }
            if (m_start == null) {
                if (other.m_start != null) {
                    return false;
                }
            } else if (!m_start.equals(other.m_start)) {
                return false;
            }
            return true;
        }

        /**
         * Gets the outer type.
         *
         * @return ELL screener
         */
        private ELLScreener getOuterType() {
            return ELLScreener.this;
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
    protected class RetrieveRCDTS implements FieldRetriever {

        /**
         * @see com.follett.fsc.core.k12.tools.stateexports.FieldRetriever#getFieldValue(com.follett.fsc.core.k12.tools.stateexports.StateReportData,
         *      com.follett.fsc.core.k12.tools.stateexports.StateReportEntity,
         *      com.follett.fsc.core.k12.tools.stateexports.FieldDefinition)
         */
        @Override
        public Object getFieldValue(StateReportData data, StateReportEntity entity, FieldDefinition field) {
            String param = (String) field.getParameter();
            ELLEntity esEntity = (ELLEntity) entity;
            String rcdts = null;
            if (param.equals("H") && esEntity.getEffectiveEnrollment() != null
                    && esEntity.getEffectiveEnrollment().getSchool() != null) {
                ELLScreener esData = (ELLScreener) data;

                rcdts = (String) esEntity.getEffectiveEnrollment().getFieldValueByBeanPath(esData.m_fieldSchoolHome);

                if (!StringUtils.isEmpty(rcdts)) {
                    rcdts = esData.lookupStateValue(StudentEnrollment.class, esData.m_fieldSchoolHome, rcdts);
                }

                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = (String) esEntity.getEffectiveEnrollment().getSchool()
                            .getFieldValueByBeanPath(m_fieldSchoolId);
                }
            } else if (param.equals("S")) {
                rcdts = esEntity.m_rcdtsMap.get(ALIAS_SERVICE_SCHOOL_CODE);
                if (StringUtils.isEmpty(rcdts)) {
                    rcdts = esEntity.getCurrentSchool();
                }
            }
            return rcdts;
        }
    }

    /**
     * Retrieve a bean property and strip off invalid characters.
     *
     * @author X2 Development Corporation
     */
    protected class RetrieveStripNameChar implements FieldRetriever {

        protected static final String CALC_ID = "SRC-STRIPCHAR";

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
                cleanValue = nameValue.replaceAll("[.,\'\"]", "");
            } else {
                cleanValue = "";
            }
            return cleanValue;
        }
    }

    /**
     * Student must be at least 3 years old on the day of test being taken, and is not a future
     * date.
     *
     * @author X2 Development Corporation
     *
     */
    protected class ValidateDateTestTaken implements FieldValidator {

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
            SisStudent student = (SisStudent) entity.getBean();
            SisPerson person = student.getPerson();

            try {
                Date studentDob = m_dateFormat.parse(entity.getFieldValue("Birth Date"));
                PlainDate testTakenDate = new PlainDate(m_dateFormat.parse(value));

                // date ell screener test taken must be 3 years greater than student's birth date
                if (person.getAgeAsOfDate(testTakenDate) < 3) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Student must be 3 years old when they take the test",
                            "Date test taken = " + STYLE_BOLD + value + STYLE_END + ", student's date of birth = "
                                    + STYLE_BOLD +
                                    m_dateFormat.format(studentDob) + STYLE_END));
                }

                // test taken date cannot be in the future
                if (testTakenDate.after(m_reportDate)) {
                    errors.add(new StateReportValidationError(entity, field,
                            "Date ELL screener test taken must not be a future date",
                            "Date test taken = " + STYLE_BOLD + value + STYLE_END));
                }
            } catch (ParseException pe) {
                errors.add(new StateReportValidationError(entity, field,
                        "Dates formatted incorrectly",
                        "Student DOB = " + STYLE_BOLD + entity.getFieldValue("Birth Date") +
                                ", Test Taken Date = " + STYLE_BOLD + value + STYLE_END));
            }

            return errors;
        }

    }

    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    static final int STATE_ORIGINAL = ExportFormatField.ReferenceMapTypeCode.STATE.ordinal();

    /**
     * The header prints out the name of the export, total count of records,
     * the file name, and the date.
     *
     * @return String
     */
    @Override
    public String getHeading() {
        StringBuilder heading = new StringBuilder(100);
        heading.append("ELL Screener");
        heading.append(',');
        heading.append(m_totalRowCount);
        heading.append(',');
        heading.append(getFileName());
        heading.append(',');
        heading.append(m_dateFormat.format(m_reportDate));
        heading.append(',');
        heading.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        heading.append("\n");
        return heading.toString();
    }


    /**
     * Gets the school max grade map.
     *
     * @return Map
     */
    public Map<String, Integer> getSchoolMaxGradeMap() {
        return m_schoolMaxGradeMap;
    }

    /**
     * Gets the student assessments.
     *
     * @param stdOid String
     * @return student assessments by student's oid.
     */
    public Collection<StudentAssessment> getStudentAssessments(String stdOid) {
        Collection<StudentAssessment> studentAssessments = new ArrayList<StudentAssessment>();

        if (m_studentsAssessmentsMap.get(stdOid) != null) {
            studentAssessments = m_studentsAssessmentsMap.get(stdOid);
        }

        return studentAssessments;
    }

    /**
     * Gets the year context map.
     *
     * @return Map
     */
    public Map<PeriodSchoolYearContext, DistrictSchoolYearContext> getYearContextMap() {
        return m_yearContextMap;
    }

    /**
     * Initialize.
     *
     * @see com.x2dev.sis.tools.stateexports.StateReportData#initialize()
     */
    @Override
    public void initialize() {

        initializeFields();
        initializeSchoolMaxGradeMap();
        setEntityClass(ELLEntity.class);

        m_reportDate = ((PlainDate) getParameter(PARAM_REPORT_DATE));
        m_reportStartDate = ((PlainDate) getParameter(PARAM_REPORT_START_DATE));
        if (getSetupErrors().isEmpty()) {
            X2Criteria ddxPgmEllCriteria = new X2Criteria();
            ddxPgmEllCriteria.addEqualTo(ExtendedDataDictionary.COL_ID, DDX_PGM_ELL_CODE);
            QueryByCriteria ddxPgmEllQuery = new QueryByCriteria(ExtendedDataDictionary.class, ddxPgmEllCriteria);
            ExtendedDataDictionary ddx = (ExtendedDataDictionary) getBroker().getBeanByQuery(ddxPgmEllQuery);
            if (ddx != null) {
                m_ddxPgmEllOid = ddx.getOid();
            }

            m_helper = new StudentHistoryHelper(this);
            m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);
            if (m_reportStartDate != null) {
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_BEGIN_DATE, m_reportStartDate);
            }
            if (m_reportDate != null) {
                m_helper.setSelectionProperty(StudentHistoryHelper.PROPERTY_END_DATE, m_reportDate);
            }

            initializeCtxByReportDate();
            loadSchedules();
            loadScheduleTerms();
            loadScheduleTermDates();

            setQuery(m_helper.getStudentQuery(true));
            SubQuery studentSubQuery =
                    new SubQuery(SisStudent.class, X2BaseBean.COL_OID, m_helper.getStudentCriteria());

            // build the query for assessments.
            m_studentsAssessmentsMap = initStudentsAssessments(studentSubQuery);

            // initialize programs where state code is "ELL" and end date is empty or => report date
            DataDictionaryField pgmCode =
                    getDataDictionary().findDataDictionaryField(StudentProgramParticipation.class.getName(),
                            StudentProgramParticipation.COL_PROGRAM_CODE);
            ReferenceTable refTable = pgmCode.getReferenceTable();
            Collection<ReferenceCode> pgmCodes = refTable.getReferenceCodes();
            ArrayList<String> ellCodes = new ArrayList<String>();
            for (ReferenceCode curPgmCode : pgmCodes) {
                if ("ELL".equals(curPgmCode.getStateCode())) {
                    ellCodes.add(curPgmCode.getCode());
                }
            }
            X2Criteria ellPgmCriteria = new X2Criteria();
            X2Criteria pgmEndDateCriteria = new X2Criteria();
            pgmEndDateCriteria.addEmpty(StudentProgramParticipation.COL_END_DATE, getBroker().getPersistenceKey());
            X2Criteria orPgmEndDateCriteria = new X2Criteria();
            orPgmEndDateCriteria.addGreaterOrEqualThan(StudentProgramParticipation.COL_END_DATE, m_reportDate);
            pgmEndDateCriteria.addOrCriteria(orPgmEndDateCriteria);
            ellPgmCriteria.addAndCriteria(pgmEndDateCriteria);
            ellPgmCriteria.addIn(StudentProgramParticipation.COL_PROGRAM_CODE, ellCodes);
            QueryByCriteria ellPgmQuery = new QueryByCriteria(StudentProgramParticipation.class, ellPgmCriteria);
            m_studentsEllPgmsMap = getBroker().getGroupedCollectionByQuery(ellPgmQuery,
                    StudentProgramParticipation.COL_STUDENT_OID, 200);

            m_enrollmentMap = initStudentsEnrollments(studentSubQuery);
            m_secondaryOutplacementSchoolMap = initSecondOutplSchools(studentSubQuery);

            HashMap<String, FieldRetriever> calcs = new HashMap<String, FieldRetriever>();
            calcs.put("ELL-SCRNR-RCDTS", new RetrieveRCDTS());
            calcs.put("ASSESS", new Assessment());
            calcs.put(RetrieveStripNameChar.CALC_ID, new RetrieveStripNameChar());
            super.addCalcs(calcs);

            HashMap<String, FieldValidator> validators = new HashMap<String, FieldValidator>();
            validators.put("ELL-SCRNR-DATE-TAKEN", new ValidateDateTestTaken());
            super.addValidators(validators);
        }
    }

    /**
     * find student assessment column definition from alias.
     *
     * @param alias String
     * @return Assessment column definition
     */
    protected AssessmentColumnDefinition getStdAssesmentColumnByAlias(String alias) {
        X2Criteria asmColumnCriteria = new X2Criteria();
        if (!StringUtils.isEmpty(alias)) {
            asmColumnCriteria.addEqualTo(AssessmentColumnDefinition.COL_ALIAS, alias);
        } else {
            asmColumnCriteria.addEqualTo(X2BaseBean.COL_OID, "___zzz___");
        }
        QueryByCriteria acdQuery = new QueryByCriteria(AssessmentColumnDefinition.class, asmColumnCriteria);
        return getBroker().getBeanByQuery(acdQuery);
    }

    /**
     * find stdAssesmentField in data dictionary by assessmentColumnDefinition alias.
     *
     * @param alias String
     * @param isRequired boolean
     * @return String
     */
    protected String getStdAssesmentFieldByAlias(String alias, boolean isRequired) {
        String returnValue = null;
        AssessmentColumnDefinition assessmentColumnDefinition = getStdAssesmentColumnByAlias(alias);
        DataFieldConfig dataFieldConfig = null;
        if (assessmentColumnDefinition != null) {
            dataFieldConfig = assessmentColumnDefinition.getDataFieldConfig();

            if (dataFieldConfig != null) {
                DataDictionaryField dataDictionaryField =
                        getDataDictionary().findDataDictionaryField(dataFieldConfig.getDataFieldOid());
                returnValue = dataDictionaryField.getSystemDataField().getJavaName();
            }
        }

        if (isRequired && returnValue == null) {
            String aliasMsg =
                    LocalizationCache.getMessages(getBroker().getPersistenceKey()).getMessage(ERROR_ALIAS_LOOKUP);
            addSetupError(aliasMsg, alias);
        }
        return returnValue;
    }


    /**
     * Returns the criteria that retrieves all assessments that should be included in the export.
     *
     * @param studentSubQuery SubQuery
     * @return Criteria
     */
    private Criteria getAssessmentCriteria(SubQuery studentSubQuery) {
        X2Criteria assessmentCriteria = new X2Criteria();

        assessmentCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentSubQuery);
        // get the reference codes the assessment definition screener taken
        AssessmentColumnDefinition screenerTakenCD = getStdAssesmentColumnByAlias(ALIAS_SCR_TAKEN);

        X2Criteria screenerTakenCriteria = new X2Criteria();
        if (screenerTakenCD != null && screenerTakenCD.getReferenceTableOid() != null) {
            screenerTakenCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID,
                    screenerTakenCD.getReferenceTableOid());
            screenerTakenCriteria.addNotEmpty(ReferenceCode.COL_STATE_CODE, getBroker().getPersistenceKey());
            screenerTakenCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
        }
        SubQuery screenerTakenSubQuery =
                new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, screenerTakenCriteria);
        assessmentCriteria.addIn(m_fieldScrTaken, screenerTakenSubQuery);

        assessmentCriteria.addEqualTo(
                StudentAssessment.REL_ASSESSMENT_DEFINITION + PATH_DELIMITER + AssessmentDefinition.COL_SUBJECT, "ELL");

        // get all assessments starting from a specific date
        m_reportStartDate = ((PlainDate) getParameter(PARAM_REPORT_START_DATE));
        if (m_reportStartDate != null) {
            assessmentCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, m_reportStartDate);
        }

        return assessmentCriteria;
    }

    /**
     * Generate a filename.
     *
     * @return <district na>_<date>_001.csv
     */
    private String getFileName() {
        StringBuilder fileName = new StringBuilder();
        fileName.append(getOrganization().getFieldValueByBeanPath(m_fieldDistrictCode));
        fileName.append("_");
        fileName.append(new SimpleDateFormat("MMddyyyy").format(m_reportDate));
        fileName.append("_");
        fileName.append("001.csv");
        return fileName.toString();
    }

    /**
     * Init CTX by repot date.
     */
    private void initializeCtxByReportDate() {
        Criteria criteria = new X2Criteria();
        criteria.addGreaterOrEqualThan(DistrictSchoolYearContext.COL_END_DATE, m_reportDate);
        criteria.addLessOrEqualThan(DistrictSchoolYearContext.COL_START_DATE, m_reportDate);

        m_context = (DistrictSchoolYearContext) getBroker()
                .getBeanByQuery(new QueryByCriteria(DistrictSchoolYearContext.class, criteria));
    }

    /**
     * Sets the Java names (bean paths) for all the exported fields.
     */
    private void initializeFields() {
        m_fieldSchoolCode = translateAliasToJavaName(ALIAS_SCHOOL_ID, true);
        m_fieldSchoolHome = translateAliasToJavaName(ALIAS_DOE_SCHOOL_HOME, true);
        m_fieldSchoolId = translateAliasToJavaName(ALIAS_DOE_SCHOOL_ID, true);
        m_fieldServiceSchoolCode = translateAliasToJavaName(ALIAS_SERVICE_SCHOOL_CODE, true);
        m_fieldDistrictCode = translateAliasToJavaName(ALIAS_DISTRICT_ID, true);
        m_fieldRcdtsForServingSchool = translateAliasToJavaName(ALIAS_RCDTS_FOR_SERVING_SCHOOL, true);
        m_fieldAssessmentGradeLvl = translateAliasToJavaName(ALIAS_GRADE_LVL, true);
        m_fieldScrTaken = getStdAssesmentFieldByAlias(ALIAS_SCR_TAKEN, true);
        m_fieldDateTaken = translateAliasToJavaName(ALIAS_SCR_DATE_TAKEN, true);
        m_excludeSklField = translateAliasToJavaName(ALIAS_EXCLUDE_SKL, true);
        m_fieldScrnSemester = getStdAssesmentFieldByAlias(ALIAS_SCRN_SEMESTER, true);

        m_fieldScrnLstRaw = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_LISTEN_RAW, true);
        m_fieldScrnLstProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_LISTEN_PROF, true);
        m_fieldScrnSpkRaw = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_SPEAK_RAW, true);
        m_fieldScrnSpkProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_SPEAK_PROF, true);
        m_fieldScrnReadRaw = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_READ_RAW, true);
        m_fieldScrnReadProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_READ_PROF, true);
        m_fieldScrnWriteRaw = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_WRITE_RAW, true);
        m_fieldScrnWriteProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_WRITE_PROF, true);
        m_fieldScrnLitProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_LIT_PROF, true);
        m_fieldScrnOralProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_ORAL_PROF, true);
        m_fieldScrnOverProf = getStdAssesmentFieldByAlias(ALIAS_DOE_SCRN_OVERALL_PROF, true);
    }

    /**
     * Initialize school max grade map.
     */
    private void initializeSchoolMaxGradeMap() {
        Criteria schoolCriteria = new X2Criteria();
        schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
        schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

        Iterator<SisSchool> schoolIterator =
                getBroker().getIteratorByQuery(new QueryByCriteria(SisSchool.class, schoolCriteria));
        while (schoolIterator.hasNext()) {
            SisSchool school = schoolIterator.next();
            String schoolOid = school.getOid();
            int startGrade = school.getStartGrade();
            int numberOfGrades = school.getNumberOfGrades();
            int maxGrade = numberOfGrades + startGrade - 1;
            m_schoolMaxGradeMap.put(schoolOid, Integer.valueOf(maxGrade));
        }
    }

    /**
     * Loads the active schedule for each school.
     */
    private void loadSchedules() {
        X2Criteria schCriteria = new X2Criteria();
        if (isSchoolContext()) {
            schCriteria.addEqualTo(Schedule.COL_SCHOOL_OID, getSchool().getOid());
        } else {
            Criteria schoolCriteria = new X2Criteria();
            schoolCriteria.addNotEqualTo(SisSchool.COL_INACTIVE_INDICATOR, Boolean.TRUE);
            schoolCriteria.addNotEqualTo(SisSchool.COL_ARCHIVE_INDICATOR, Boolean.TRUE);

            SubQuery sklSubQuery = new SubQuery(SisSchool.class, X2BaseBean.COL_OID, schoolCriteria);
            schCriteria.addIn(Schedule.COL_SCHOOL_OID, sklSubQuery);
        }

        if (m_context == null) {
            initializeCtxByReportDate();
        }

        schCriteria.addEqualTo(Schedule.REL_ACTIVE_SCHOOL_SCHEDULE_CONTEXTS + PATH_DELIMITER
                + SchoolScheduleContext.COL_DISTRICT_CONTEXT_OID, m_context.getOid());

        QueryByCriteria schQuery = new QueryByCriteria(Schedule.class, schCriteria);
        m_scheduleMap = getBroker().getMapByQuery(schQuery, Schedule.COL_SCHOOL_OID, 1024);
    }

    /**
     * Initialize Schedule Terms map keyed on Schedule Oid.
     */
    private void loadScheduleTerms() {
        X2Criteria trmCriteria = new X2Criteria();

        X2Criteria semesterCodesCriteria = new X2Criteria();
        DataDictionaryField field = getDataDictionaryField(ScheduleTerm.class, ScheduleTerm.COL_CODE);

        if (field != null && field.getReferenceTableOid() != null) {
            semesterCodesCriteria.addEqualTo(ReferenceCode.COL_REFERENCE_TABLE_OID, field.getReferenceTableOid());
            semesterCodesCriteria.addIn(ReferenceCode.COL_STATE_CODE, SEMESTER_CODES);
            semesterCodesCriteria.addNotEqualTo(ReferenceCode.COL_DISABLED_INDICATOR, Boolean.TRUE);
        }
        SubQuery semesterCodesSubQuery =
                new SubQuery(ReferenceCode.class, ReferenceCode.COL_CODE, semesterCodesCriteria);

        trmCriteria.addIn(ScheduleTerm.COL_CODE, semesterCodesSubQuery);

        if (m_scheduleMap == null) {
            loadSchedules();
        }

        if (!m_scheduleMap.isEmpty()) {
            Collection<Schedule> schs = m_scheduleMap.values();
            Collection<String> schOids = new ArrayList<String>();

            for (Schedule sch : schs) {
                schOids.add(sch.getOid());
            }

            trmCriteria.addIn(ScheduleTerm.REL_SCHEDULE, schOids);
        } else {
            trmCriteria.addEqualTo(ScheduleTerm.COL_SCHEDULE_OID, "__dummy__");
        }

        m_termMap = getBroker().getGroupedCollectionByQuery(new QueryByCriteria(ScheduleTerm.class, trmCriteria),
                ScheduleTerm.COL_SCHEDULE_OID, 1024);
    }

    /**
     * Initialize Schedule Term Dates map keyed on Schedule Term Oid.
     */
    private void loadScheduleTermDates() {
        X2Criteria tmdCriteria = new X2Criteria();

        if (m_termMap == null) {
            loadScheduleTerms();
        }

        if (!m_termMap.isEmpty()) {
            Collection<String> trmOids = new ArrayList<String>();

            for (Entry<String, Collection<ScheduleTerm>> entry : m_termMap.entrySet()) {
                for (ScheduleTerm term : entry.getValue()) {
                    trmOids.add(term.getOid());
                }
            }

            tmdCriteria.addIn(ScheduleTermDate.COL_SCHEDULE_TERM_OID, trmOids);
        } else {
            tmdCriteria.addEqualTo(ScheduleTermDate.COL_SCHEDULE_TERM_OID, "__dummy__");
        }

        tmdCriteria.addLessOrEqualThan(ScheduleTermDate.COL_START_DATE, m_reportDate);
        tmdCriteria.addGreaterOrEqualThan(ScheduleTermDate.COL_END_DATE, m_reportDate);

        m_termDatesMap = getBroker().getMapByQuery(new QueryByCriteria(ScheduleTermDate.class, tmdCriteria),
                ScheduleTermDate.COL_SCHEDULE_TERM_OID, 1024);
    }

    /**
     * Initializes secondary outplacement schools for students.
     *
     * @param studentSubQuery SubQuery
     * @return Map
     */
    private Map<String, String> initSecondOutplSchools(SubQuery studentSubQuery) {
        Map<String, String> secondOutplacSchoolMap = new HashMap<String, String>();

        Criteria secondaryOutplacementCriteria = new X2Criteria();
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
                secondOutplacSchoolMap.put(item.getStudentOid(),
                        (String) item.getFieldValueByBeanPath(m_fieldRcdtsForServingSchool));
            }
        } finally {
            iter.close();
        }

        return secondOutplacSchoolMap;
    }

    /**
     * Map of students enrollments by student oid's.
     *
     * @param studentSubQuery SubQuery
     * @return Map
     */
    private Map<String, List<StudentEnrollment>> initStudentsEnrollments(SubQuery studentSubQuery) {
        Criteria enrollmentCriteria = new Criteria();
        enrollmentCriteria.addIn(StudentEnrollment.COL_STUDENT_OID, studentSubQuery);
        QueryByCriteria enrollmentQuery = new QueryByCriteria(StudentEnrollment.class, enrollmentCriteria);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_ENROLLMENT_DATE);
        enrollmentQuery.addOrderByDescending(StudentEnrollment.COL_TIMESTAMP);
        return getBroker().getGroupedCollectionByQuery(enrollmentQuery, StudentEnrollment.COL_STUDENT_OID, 128);
    }

    /**
     * Initializes map of assessments keyed on student oid.
     *
     * @param studentSubQuery SubQuery
     * @return Map
     */
    private Map<String, Collection<StudentAssessment>> initStudentsAssessments(SubQuery studentSubQuery) {
        Criteria assessmentCriteria = getAssessmentCriteria(studentSubQuery);
        QueryByCriteria assessmentQuery = new QueryByCriteria(StudentAssessment.class, assessmentCriteria);
        assessmentQuery.addOrderBy(StudentAssessment.COL_DATE, false);
        return getBroker().getGroupedCollectionByQuery(assessmentQuery, StudentAssessment.COL_STUDENT_OID, 256);
    }
}
