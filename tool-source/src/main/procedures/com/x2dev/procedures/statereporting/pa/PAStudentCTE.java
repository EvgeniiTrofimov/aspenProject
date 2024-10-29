/*
 * ====================================================================
 *
 * X2 Development Corporation
 *
 * Copyright (c) 2002-2013 X2 Development Corporation.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, is not permitted without express written agreement
 * from X2 Development Corporation.
 *
 * ====================================================================
 */

/* DEBUG */

package com.x2dev.procedures.statereporting.pa;

import com.follett.fsc.core.framework.persistence.SubQuery;
import com.follett.fsc.core.framework.persistence.X2Criteria;
import com.follett.fsc.core.k12.beans.DataFieldConfig;
import com.follett.fsc.core.k12.beans.QueryIterator;
import com.follett.fsc.core.k12.beans.ReportQueryIterator;
import com.follett.fsc.core.k12.beans.X2BaseBean;
import com.follett.fsc.core.k12.business.ModelProperty;
import com.follett.fsc.core.k12.business.X2Broker;
import com.follett.fsc.core.k12.business.dictionary.DataDictionary;
import com.follett.fsc.core.k12.business.dictionary.DataDictionaryField;
import com.follett.fsc.core.k12.business.localization.LocalizationCache;
import com.follett.fsc.core.k12.tools.stateexports.FieldDefinition;
import com.follett.fsc.core.k12.tools.stateexports.FieldRetriever;
import com.follett.fsc.core.k12.tools.stateexports.StateReportData;
import com.follett.fsc.core.k12.tools.stateexports.StateReportEntity;
import com.follett.fsc.core.k12.web.UserDataContainer;
import com.follett.fsc.core.k12.web.nav.FilterException;
import com.x2dev.sis.model.beans.*;
import com.x2dev.sis.model.business.GraduationManager;
import com.x2dev.sis.tools.stateexports.StudentHistoryHelper;
import com.x2dev.utils.X2BaseException;
import com.x2dev.utils.converters.BooleanAsStringConverter;
import com.x2dev.utils.types.PlainDate;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.apache.commons.lang3.StringUtils;
import org.apache.ojb.broker.query.Criteria;
import org.apache.ojb.broker.query.QueryByCriteria;
import org.apache.ojb.broker.query.ReportQueryByCriteria;
import org.jdom.JDOMException;

/**
 * Class for student CTE export.
 */
public class PAStudentCTE extends StateReportData {
    /**
     * Entity class for student CTE export.
     */
    public static class PAStudentCTEEntity extends StateReportEntity {

        /**
         * The Class GraduationStatus.
         */
        public class GraduationStatus {
            private HashMap<String, List<SchoolCourse>> m_coursesGainedCredit =
                    new HashMap<String, List<SchoolCourse>>();
            private HashMap<String, List<SchoolCourse>> m_coursesRequesting = new HashMap<String, List<SchoolCourse>>();
            private HashMap<String, List<SchoolCourse>> m_coursesTaken = new HashMap<String, List<SchoolCourse>>();
            private HashMap<String, List<SchoolCourse>> m_coursesTaking = new HashMap<String, List<SchoolCourse>>();
            private HashMap<String, List<String>> m_coursesToTranscripts = new HashMap<String, List<String>>();
            private HashMap<String, Double> m_creditsByCourse = new HashMap<String, Double>();
            private HashMap<String, Double> m_creditsFuture = new HashMap<String, Double>();
            private HashMap<String, Double> m_creditsGained = new HashMap<String, Double>();
            private HashMap<String, Double> m_creditsInProgress = new HashMap<String, Double>();
            private HashMap<String, Double> m_creditsRequired = new HashMap<String, Double>();
            private HashMap<String, Double> m_creditsWaived = new HashMap<String, Double>();
            private Map<String, Map<String, String>> m_equivalentCourseDesc =
                    new HashMap<String, Map<String, String>>();
            private HashMap<String, String> m_gradeLevelByCourse = new HashMap<String, String>();
            private Map<String, Map<String, Object>> m_otherRequirementValues =
                    new HashMap<String, Map<String, Object>>();
            private HashMap<String, Double> m_rawCreditsGained = new HashMap<String, Double>();
            private List<String> m_satisfiedOtherRequirementOids = new ArrayList<String>();

            /**
             * Instantiates a new graduation status.
             *
             * @param userData UserDataContainer
             * @param broker X2Broker
             * @param student SisStudent
             * @param programStudiesOid String
             * @throws FilterException exception
             * @throws JDOMException exception
             * @throws IOException Signals that an I/O exception has occurred.
             */
            GraduationStatus(UserDataContainer userData, X2Broker broker, SisStudent student, String programStudiesOid)
                    throws FilterException, JDOMException, IOException {
                /*
                 * Get a map of the courses with partial credit course requirements.
                 */
                X2Criteria partialCourseReqCriteria = new X2Criteria();
                partialCourseReqCriteria.addEqualTo(GraduationCourseRequirement.REL_REQUIREMENT + "."
                        + GraduationRequirement.COL_PROGRAM_STUDIES_OID, programStudiesOid);
                partialCourseReqCriteria.addNotEqualTo(GraduationCourseRequirement.COL_PARTIAL_CREDIT,
                        Double.valueOf("0.0"));

                QueryByCriteria partialQuery =
                        new QueryByCriteria(GraduationCourseRequirement.class, partialCourseReqCriteria);
                Map<String, List<GraduationCourseRequirement>> partialCourseRequirments = broker
                        .getGroupedCollectionByQuery(partialQuery, GraduationCourseRequirement.COL_COURSE_OID, 100);

                GraduationManager manager = new GraduationManager(broker);
                manager.determineGraduationStatus(student,
                        userData,
                        programStudiesOid,
                        m_coursesGainedCredit,
                        m_coursesTaken,
                        m_coursesTaking,
                        m_coursesRequesting,
                        m_coursesToTranscripts,
                        m_creditsGained,
                        m_rawCreditsGained,
                        m_creditsWaived,
                        m_creditsRequired,
                        m_creditsByCourse,
                        m_creditsInProgress,
                        m_creditsFuture,
                        m_gradeLevelByCourse,
                        false, // projectFuture,
                        partialCourseRequirments,
                        m_equivalentCourseDesc,
                        m_otherRequirementValues,
                        m_satisfiedOtherRequirementOids);
            }

            /**
             * Gets the technical credit gained.
             *
             * @return Big decimal
             */
            public BigDecimal getTechnicalCreditGained() {
                return getCreditValue(m_creditsGained);
            }

            /**
             * Gets the technical credit required.
             *
             * @return Big decimal
             */
            public BigDecimal getTechnicalCreditRequired() {
                return getCreditValue(m_creditsRequired);
            }

            /**
             * Gets the technical transcripts.
             *
             * @return List
             */
            public List<Transcript> getTechnicalTranscripts() {
                List<Transcript> transcripts = new LinkedList();
                for (Entry<String, List<SchoolCourse>> entry : m_coursesTaken.entrySet()) {
                    String grqOid = entry.getKey();
                    GraduationRequirement grq = m_data.getBroker().getBeanByOid(GraduationRequirement.class, grqOid);
                    if (grq != null && BooleanAsStringConverter.TRUE
                            .equals(grq.getFieldValueByAlias(ALIAS_GRQ_TECHNICAL_INDICATOR))) {
                        for (SchoolCourse crs : entry.getValue()) {
                            List<String> trnOids = m_coursesToTranscripts.get(crs.getOid());
                            if (trnOids != null && !trnOids.isEmpty()) {
                                for (String trnOid : trnOids) {
                                    Transcript trn = m_data.getBroker().getBeanByOid(Transcript.class, trnOid);
                                    if (trn != null) {
                                        transcripts.add(trn);
                                    }
                                }
                            }
                        }
                    }
                }
                return transcripts;
            }

            /**
             * Gets the credit value.
             *
             * @param map HashMap<String,Double>
             * @return Big decimal
             */
            private BigDecimal getCreditValue(HashMap<String, Double> map) {
                BigDecimal value = BigDecimal.ZERO;
                for (String grqOid : map.keySet()) {
                    GraduationRequirement grq = m_data.getBroker().getBeanByOid(GraduationRequirement.class, grqOid);
                    if (grq != null && BooleanAsStringConverter.TRUE
                            .equals(grq.getFieldValueByAlias(ALIAS_GRQ_TECHNICAL_INDICATOR))) {
                        value = value.add(BigDecimal.valueOf(map.get(grqOid)));
                    }
                }
                return value;
            }
        }

        private static final String CTE_STATUS_DEFAULT = "10";
        private static final String CTE_STATUS_OUT_OF_DIST = "22";
        private static final String CTE_STATUS_IN_DIST = "28";
        private static final String CTE_STATUS_NONGRAD_SENIOR_ALL_COMPLETE_NOCTI = "30";
        private static final String CTE_STATUS_GRAD_SENIOR_ALL_COMPLETE_NOCTI = "40";
        private static final String CTE_STATUS_GRAD_SENIOR_NON_COMPLETER = "60";
        private static final String CTE_STATUS_DROPS_OUT = "71";
        private static final String CTE_STATUS_DECEASED = "80";

        private static final String ENR_CODE_STUDENT_DECEASED = "WD06";
        private static final String ENR_CODE_STUDENT_EXPELLED = "WD01";
        private static final String ENR_CODE_STUDENT_TRANSFERRED_OUT_OF_DST = "WD02";
        private static final String ENR_CODE_STUDENT_TRANSFERRED_IN_DST = "WD11";

        private static final String PARAM_USER_DATA = "userDataContainer";

        private static final String SENIOR_STUDENT_GRADE = "12";

        private static final BigDecimal NUMBER_OF_CREDIT_HOURS = new BigDecimal(120);
        private static final BigDecimal ONE_HUNDRED_PERCENT = new BigDecimal(100);
        private static final BigDecimal ONE_PERCENT = new BigDecimal(1);

        private PAStudentCTE m_data = null;
        private GraduationProgram m_gpr;
        private GraduationStatus m_graduationStatus;
        private ArrayList<SisSchool> m_schools = null;
        private SisStudent m_student = null;

        /**
         * Gets the cte status.
         *
         * @param currentSchool SisSchool
         * @return status type code.
         */
        public String getCteStatus(SisSchool currentSchool) {
            String status = null;
            boolean stdComplMoreThanTenPcnt = (getPcntOfPosCompleted().compareTo(ONE_PERCENT) > 0);
            if (stdComplMoreThanTenPcnt) {
                status = CTE_STATUS_DEFAULT;

                String gradDateString = (String) m_student.getFieldValueByAlias(m_data.m_fieldStdGradDate);
                boolean studentGraduated = !StringUtils.isEmpty(gradDateString);
                boolean studentIsSenior = m_student.getGradeLevel().equals(SENIOR_STUDENT_GRADE);
                boolean studentCompletedAllHrs = getPcntOfPosCompleted().equals(ONE_HUNDRED_PERCENT);
                boolean studentTakenNocti = studentTakenNOCTI();

                if (!studentGraduated && studentIsSenior && studentCompletedAllHrs && studentTakenNocti) {
                    status = CTE_STATUS_NONGRAD_SENIOR_ALL_COMPLETE_NOCTI;
                }

                if (studentGraduated && studentIsSenior && studentCompletedAllHrs && studentTakenNocti) {
                    status = CTE_STATUS_GRAD_SENIOR_ALL_COMPLETE_NOCTI;
                }

                if (studentGraduated && studentIsSenior && !(studentCompletedAllHrs && studentTakenNocti)) {
                    status = CTE_STATUS_GRAD_SENIOR_NON_COMPLETER;
                }

                StudentEnrollment lastEnrollment =
                        m_data.m_helper.getEnrollmentForDate(m_student.getOid(), m_data.m_reportDate, "EWSY");
                String enrollmentCode =
                        (String) lastEnrollment.getFieldValueByBeanPath(m_data.m_fieldEnrSchoolEnrStatus);

                boolean stdTransferredOutOfDistrict = ENR_CODE_STUDENT_TRANSFERRED_OUT_OF_DST.equals(enrollmentCode);
                boolean stdTransferredInDistrict = ENR_CODE_STUDENT_TRANSFERRED_IN_DST.equals(enrollmentCode);
                boolean stdDeceased = ENR_CODE_STUDENT_DECEASED.equals(enrollmentCode);
                boolean stdDropsOut = ENR_CODE_STUDENT_EXPELLED.equals(enrollmentCode);
                if (stdTransferredOutOfDistrict) {
                    status = CTE_STATUS_OUT_OF_DIST;
                } else if (stdTransferredInDistrict) {
                    status = CTE_STATUS_IN_DIST;
                } else if (stdDeceased) {
                    status = CTE_STATUS_DECEASED;
                } else if (stdDropsOut) {
                    status = CTE_STATUS_DROPS_OUT;
                }
            }

            return status;
        }

        /**
         * Gets the cte std assessment.
         *
         * @return student assessment with CTE Industry Credentials
         */
        public StudentAssessment getCteStdAssessment() {
            return m_data.getCteAssessmByStdOidMap(m_student.getOid());
        }

        /**
         * Gets the cte std assessment data dictionary.
         *
         * @return extendent data dictionary for CTE Assessment.
         */
        public DataDictionary getCteStdAssessmentDataDictionary() {
            DataDictionary extendedDataDictionary = null;
            StudentAssessment cteAssessment = getCteStdAssessment();
            if (cteAssessment != null) {
                extendedDataDictionary = DataDictionary.getDistrictDictionary(cteAssessment.getExtendedDataDictionary(),
                        m_data.getBroker().getPersistenceKey());
            }
            return extendedDataDictionary;
        }

        /**
         * Gets the current school.
         *
         * @return current school.
         */
        public SisSchool getCurrentSchool() {
            return m_schools.get(getCurrentRow());
        }

        /**
         * Gets the num of pos hrs completed.
         *
         * @return number of hours completed.
         */
        public BigDecimal getNumOfPosHrsCompleted() {
            return m_graduationStatus.getTechnicalCreditGained().multiply(NUMBER_OF_CREDIT_HOURS);
        }

        /**
         * Gets the pcnt of pos completed.
         *
         * @return percentage of POS completed for current school.
         */
        public BigDecimal getPcntOfPosCompleted() {
            BigDecimal prcntgCompleted = BigDecimal.ZERO;

            BigDecimal creditsRequired = m_graduationStatus.getTechnicalCreditRequired();
            if (creditsRequired.compareTo(BigDecimal.ZERO) > 0) {
                BigDecimal totalCredReqHrs = creditsRequired.multiply(NUMBER_OF_CREDIT_HOURS);
                BigDecimal totalCredComplHrs =
                        m_graduationStatus.getTechnicalCreditGained().multiply(NUMBER_OF_CREDIT_HOURS);

                prcntgCompleted = (totalCredComplHrs.multiply(ONE_HUNDRED_PERCENT)).divide(totalCredReqHrs, 2,
                        RoundingMode.HALF_UP);
                if (prcntgCompleted.compareTo(ONE_HUNDRED_PERCENT) > 0) {
                    prcntgCompleted = new BigDecimal(100);
                }
            }

            prcntgCompleted = prcntgCompleted.setScale(2, RoundingMode.CEILING);
            return prcntgCompleted;
        }

        /**
         * Gets the program of study.
         *
         * @return student's program of study.
         */
        public GraduationProgram getProgramOfStudy() {
            return m_gpr;
        }

        /**
         * Gets the task list completion.
         *
         * @param currentSchool SisSchool
         * @return return true if student has completed all hours and completed NOCTI, otherwise
         *         false.
         */
        public String getTaskListCompletion(SisSchool currentSchool) {
            String listComplete = "N/A";
            if (studentHasCteAssessmentWithCPC()) {
                listComplete = "N";

                boolean studentCompletedAllHrs = getPcntOfPosCompleted().equals(ONE_HUNDRED_PERCENT);
                boolean studentTakenNocti = studentTakenNOCTI();

                if (studentCompletedAllHrs && studentTakenNocti) {
                    listComplete = "Y";
                }
            }

            return listComplete;
        }

        /**
         * Intitialize.
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

            m_student = (SisStudent) bean;

            m_data = (PAStudentCTE) data;

            setRowCount(0);
            String gprOid = m_data.getStudentsWithPrograms().get(m_student.getOid());
            if (!StringUtils.isEmpty(gprOid)) {
                m_gpr = m_data.getBroker().getBeanByOid(GraduationProgram.class, gprOid);
                if (m_gpr != null) {
                    try {
                        m_graduationStatus =
                                new GraduationStatus((UserDataContainer) m_data.getParameter(PARAM_USER_DATA),
                                        data.getBroker(), m_student, getProgramOfStudy().getOid());
                    } catch (JDOMException | IOException e) {
                        throw new X2BaseException(e);
                    }
                    if (studentTakenAgCoursesThisYear()) {
                        setRowCount(m_schools.size());
                    }
                }
            }

        }

        /**
         * Student has cte assessment with CPC.
         *
         * @return true if student has CTE assessment with Career Program Category, otherwise false.
         */
        public boolean studentHasCteAssessmentWithCPC() {
            boolean studHasCteAssessWithCip = false;

            StudentAssessment stdAssessment = getCteStdAssessment();
            if (stdAssessment != null) {
                studHasCteAssessWithCip = (stdAssessment.getFieldValueByAlias(ALIAS_DOE_CAREER_PROGRAM,
                        getCteStdAssessmentDataDictionary()) != null);
            }

            return studHasCteAssessWithCip;
        }

        /**
         * Student taken ag courses this year.
         *
         * @return true if student has taken ag courses this year, otherwise false.
         */
        private boolean studentTakenAgCoursesThisYear() {
            boolean studentTakenAgCoursesThisYear = false;
            m_schools = new ArrayList<SisSchool>();

            List<Transcript> transcripts = m_graduationStatus.getTechnicalTranscripts();
            if (transcripts != null) {
                for (Transcript transcript : transcripts) {
                    String context = transcript.getDistrictContextOid();
                    if (!StringUtils.isEmpty(context)) {
                        if (context.equals(m_data.getCurrentContext().getOid())) {
                            studentTakenAgCoursesThisYear = true;
                            if (transcript.getSchool() != null && !m_schools.contains(transcript.getSchool())) {
                                m_schools.add(transcript.getSchool());
                            }
                        }
                    }
                }
            }
            return studentTakenAgCoursesThisYear;
        }

        /**
         * Student taken NOCTI.
         *
         * @return true if student taken NOCTI, otherwise false.
         */
        private boolean studentTakenNOCTI() {
            boolean studentTakenNocti = false;

            if (studentHasCteAssessmentWithCPC()) {
                DataDictionary assessmentDictionary = getCteStdAssessmentDataDictionary();
                StudentAssessment cteAssessment = getCteStdAssessment();

                String studentTakenNoctiAsString =
                        (String) cteAssessment.getFieldValueByAlias(PAStudentCTE.ALIAS_CTE_NOCTI_IND,
                                assessmentDictionary);
                if (BooleanAsStringConverter.TRUE.equals(studentTakenNoctiAsString)) {
                    studentTakenNocti = true;
                }
            }

            return studentTakenNocti;
        }
    }

    /**
     * Field retriever.
     */
    public static class RetrieveCTEInfo implements FieldRetriever {
        private static final String CALC_PARAM_AGRICULT_EXP_IND = "AGR_EXP_IND";
        private static final String CALC_PARAM_CIP_CODE = "CIP_CODE";
        private static final String CALC_PARAM_CIP_LOCATION_CODE = "LOC_CODE";
        private static final String CALC_PARAM_COOPER_WORK_IND = "CPRTV_IND";
        private static final String CALC_PARAM_CTE_STATUS_TYPE_CODE = "CTE_STATUS";
        private static final String CALC_PARAM_CUMULATIVE_POSTSEC_CRED_EARNED = "CMLTV_EARNED";
        private static final String CALC_PARAM_INTERNSHIP_INDICATOR = "INT_IND";
        private static final String CALC_PARAM_JOB_EXPLORATION_INDICATOR = "JOB_EXP_IND";
        private static final String CALC_PARAM_NUMBER_OF_PGM_HRS_COMPLETED = "HRS_NUM";
        private static final String CALC_PARAM_PRCNTG_OF_PPG_COMPLETED = "PRCNTG_CMPLT";
        private static final String CALC_PARAM_POS_TASK_LIST_COMPLETION_IND = "CMPLTN_IND";
        private static final String CALC_PARAM_REGISTERED_APPRENTICE_IND = "APP_IND";
        private static final String CALC_PARAM_SKLS_PONSORED_EE_IND = "EE_IND";
        private static final String CALC_PARAM_WORK_BASED_EXP_IND = "EXP_IND";

        private static final String STRING_EMPTY = "";
        private static final String STRING_POINT = ".";

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
            PAStudentCTE cteData = (PAStudentCTE) data;
            PAStudentCTEEntity cteEntity = (PAStudentCTEEntity) entity;
            StudentAssessment cteAssessment = cteEntity.getCteStdAssessment();
            Object value = null;
            if (field.getParameter().equals(CALC_PARAM_CIP_LOCATION_CODE)) {
                value = cteEntity.getCurrentSchool().getSchoolId();
            } else if (field.getParameter().equals(CALC_PARAM_CIP_CODE)) {
                String cipCode = (String) cteEntity.getProgramOfStudy().getFieldValueByBeanPath(cteData.m_fieldCipCode);
                value = cipCode.replace(STRING_POINT, STRING_EMPTY);
            } else if (field.getParameter().equals(CALC_PARAM_CTE_STATUS_TYPE_CODE)) {
                value = cteEntity.getCteStatus(cteEntity.getCurrentSchool());
            } else if (field.getParameter().equals(CALC_PARAM_NUMBER_OF_PGM_HRS_COMPLETED)) {
                value = cteEntity.getNumOfPosHrsCompleted();
            } else if (field.getParameter().equals(CALC_PARAM_PRCNTG_OF_PPG_COMPLETED)) {
                value = cteEntity.getPcntOfPosCompleted();
            } else if (field.getParameter().equals(CALC_PARAM_POS_TASK_LIST_COMPLETION_IND)) {
                value = cteEntity.getTaskListCompletion(cteEntity.getCurrentSchool());
            }
            if (cteAssessment != null) {
                if (field.getParameter().equals(CALC_PARAM_REGISTERED_APPRENTICE_IND)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmApprentice);
                } else if (field.getParameter().equals(CALC_PARAM_INTERNSHIP_INDICATOR)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmInternship);
                } else if (field.getParameter().equals(CALC_PARAM_COOPER_WORK_IND)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmCoopWork);
                } else if (field.getParameter().equals(CALC_PARAM_JOB_EXPLORATION_INDICATOR)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmJobExplr);
                } else if (field.getParameter().equals(CALC_PARAM_AGRICULT_EXP_IND)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAgExp);
                } else if (field.getParameter().equals(CALC_PARAM_SKLS_PONSORED_EE_IND)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmSklsEnterprise);
                } else if (field.getParameter().equals(CALC_PARAM_WORK_BASED_EXP_IND)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmWorkBasedInd);
                } else if (cteEntity.studentHasCteAssessmentWithCPC()
                        && field.getParameter().equals(CALC_PARAM_CUMULATIVE_POSTSEC_CRED_EARNED)) {
                    value = cteAssessment.getFieldValueByBeanPath(cteData.m_fieldAsmPostSecCredits);
                }
            }
            return value;
        }
    }

    /**
     * Constants: Aliases, Fields, IDs, Parameters
     */
    public static final String ALIAS_CIP_NUMBER = "DOE CIP NUMBER";
    public static final String ALIAS_CTE_AG_EXPERIENCE = "DOE AG EXPERIENCE";
    public static final String ALIAS_CTE_APPRENTICE = "DOE CTE APPRENTICE";
    public static final String ALIAS_CTE_CML_POSTSEC_CREDITS = "DOE POSTSEC CREDITS";
    public static final String ALIAS_CTE_CO_OP_WORK = "DOE CTE CO OP WORK";
    public static final String ALIAS_CTE_INTERNSHIP = "DOE CTE INTERNSHIP";
    public static final String ALIAS_CTE_JOB_EXPLR = "DOE CTE JOB EXPLR";
    public static final String ALIAS_CTE_NOCTI_IND = "DOE NOCTI IND";
    public static final String ALIAS_CTE_SKLS_ENTERPRISE = "DOE CTE SCH ENTERPRISE";
    public static final String ALIAS_CTE_WB_EXP = "DOE CTE WB EXP";
    public static final String ALIAS_DOE_CAREER_PROGRAM = "DOE CAREER PROGRAM";
    public static final String ALIAS_DOE_CREDIT_HRS = "DOE CREDIT HRS";
    public static final String ALIAS_GRQ_TECHNICAL_INDICATOR = "all-grq-TechnicalIndicator";
    public static final String ALIAS_STD_GRAD_DATE = "all-std-GradDate";
    public static final String ALIAS_ENR_SCHOOL_ENR_STATUS = "pa-enr-SchoolEnrollmentStatus";
    private static final String CTE_CALC = "CTE_CALC";
    private static final String CTE_INDUSTRY_CREDENTIALS = "CTE CREDENTIALS";
    private static final String ERROR_ALIAS_LOOKUP = "error.state.report.alias";
    private static final String GENERAL_FAMILY_CONSUMER_SCIENCE = "19.0101";
    private static final String PARAM_REPORT_DATE = "reportDate";

    protected String m_fieldCipCode = null;
    protected String m_fieldCreditHrs = null;
    protected String m_fieldAgExp;
    protected String m_fieldAsmApprentice;
    protected String m_fieldAsmCoopWork;
    protected String m_fieldAsmInternship;
    protected String m_fieldAsmJobExplr;
    protected String m_fieldAsmPostSecCredits;
    protected String m_fieldAsmSklsEnterprise;
    protected String m_fieldAsmWorkBasedInd;
    protected String m_fieldStdGradDate;
    protected String m_fieldEnrSchoolEnrStatus;

    /**
     * Instance variables.
     */
    Map<String, Collection<StudentAssessment>> m_assessmentsByStdOidMap = null;
    StudentHistoryHelper m_helper = null;
    Map<String, String> m_posOidByStdOidMap = null;
    PlainDate m_reportDate = null;

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

        m_reportDate = (PlainDate) getParameter(PARAM_REPORT_DATE);

        m_posOidByStdOidMap = getStudentsWithPrograms();

        m_helper = new StudentHistoryHelper(this);

        m_helper.setStudentSelectionMode(StudentHistoryHelper.MODE_STUDENT_ACTIVE_ANY_TIME);

        X2Criteria studentCriteria = m_helper.getStudentCriteria();
        Collection<String> studentsHavingPosOids = m_posOidByStdOidMap.keySet();
        studentCriteria.addIn(X2BaseBean.COL_OID, studentsHavingPosOids);

        m_assessmentsByStdOidMap = getCteAssessmentsOfStudents(studentCriteria);

        setQuery(m_helper.getStudentQuery(true));

        setEntityClass(PAStudentCTEEntity.class);

        registerFieldRetrievers();
    }

    /**
     * Gets the cte assessm by std oid map.
     *
     * @param stdOid String
     * @return StudentAssessment by student oid.
     */
    protected StudentAssessment getCteAssessmByStdOidMap(String stdOid) {
        StudentAssessment stdAssessment = null;

        Collection<StudentAssessment> stdAssessments = m_assessmentsByStdOidMap.get(stdOid);

        if (stdAssessments != null) {
            for (StudentAssessment firstAssessment : stdAssessments) {
                stdAssessment = firstAssessment;
                break;
            }
        }

        return stdAssessment;
    }

    /**
     * Gets the pos oid by student oid.
     *
     * @param studentOid String
     * @return oid of program of study by passed student oid.
     */
    protected String getPosOidByStudentOid(String studentOid) {
        return m_posOidByStdOidMap.get(studentOid);
    }

    /**
     * find student assessment column definition from alias.
     *
     * @param alias String
     * @return Assessment column definition
     */
    protected AssessmentColumnDefinition getStdAssesmentColumnByAlias(String alias) {
        AssessmentColumnDefinition assessmentColumnDefinition = null;
        if (alias != null) {
            Criteria assesmColumnCriteria = new X2Criteria();
            assesmColumnCriteria.addEqualTo(AssessmentColumnDefinition.COL_ALIAS, alias);
            QueryIterator queryIterator = getBroker()
                    .getIteratorByQuery(new QueryByCriteria(AssessmentColumnDefinition.class, assesmColumnCriteria));
            if (queryIterator.hasNext()) {
                assessmentColumnDefinition = (AssessmentColumnDefinition) queryIterator.next();
            }
        }
        return assessmentColumnDefinition;
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
     * Gets the cte assessments of students.
     *
     * @param studentCriteria X2Criteria
     * @return Map
     */
    private Map<String, Collection<StudentAssessment>> getCteAssessmentsOfStudents(X2Criteria studentCriteria) {
        SubQuery studentsSubQuery = new SubQuery(SisStudent.class, X2BaseBean.COL_OID, studentCriteria);

        X2Criteria assessmentsCriteria = new X2Criteria();
        assessmentsCriteria.addIn(StudentAssessment.COL_STUDENT_OID, studentsSubQuery);
        assessmentsCriteria.addGreaterOrEqualThan(StudentAssessment.COL_DATE, getCurrentContext().getStartDate());
        assessmentsCriteria.addLessOrEqualThan(StudentAssessment.COL_DATE, m_reportDate);
        assessmentsCriteria.addEqualTo(StudentAssessment.REL_ASSESSMENT_DEFINITION +
                ModelProperty.PATH_DELIMITER +
                AssessmentDefinition.COL_ID, CTE_INDUSTRY_CREDENTIALS);

        QueryByCriteria assessmentsQuery = new QueryByCriteria(StudentAssessment.class, assessmentsCriteria);

        return getBroker().getGroupedCollectionByQuery(assessmentsQuery, StudentAssessment.COL_STUDENT_OID, 256);
    }

    /**
     * Get all students with Pos.
     *
     * @return Hash map
     */
    private HashMap<String, String> getStudentsWithPrograms() {
        HashMap<String, String> stdPos = new HashMap<String, String>();

        X2Criteria stdWithPosCriteria = new X2Criteria();

        stdWithPosCriteria.addNotEmpty(GraduationStudentProgram.REL_PROGRAM_STUDIES +
                ModelProperty.PATH_DELIMITER +
                m_fieldCipCode, getBroker().getPersistenceKey());
        stdWithPosCriteria.addNotEqualTo(GraduationStudentProgram.REL_PROGRAM_STUDIES +
                ModelProperty.PATH_DELIMITER +
                m_fieldCipCode, GENERAL_FAMILY_CONSUMER_SCIENCE);

        String pathTechnicalIndicator = translateAliasToJavaName(ALIAS_GRQ_TECHNICAL_INDICATOR, true);
        if (!StringUtils.isEmpty(pathTechnicalIndicator)) {
            X2Criteria criteriaRequirements = new X2Criteria();
            criteriaRequirements.addEqualTo(pathTechnicalIndicator, BooleanAsStringConverter.TRUE);
            SubQuery subQuery = new SubQuery(GraduationRequirement.class, GraduationRequirement.COL_PROGRAM_STUDIES_OID,
                    criteriaRequirements);
            stdWithPosCriteria.addIn(GraduationStudentProgram.REL_PROGRAM_STUDIES +
                    ModelProperty.PATH_DELIMITER +
                    X2BaseBean.COL_OID, subQuery);
        }

        String[] fields = {GraduationStudentProgram.COL_STUDENT_OID,
                GraduationStudentProgram.COL_PROGRAM_STUDIES_OID};

        ReportQueryByCriteria studentWithPosQuery = new ReportQueryByCriteria(GraduationStudentProgram.class,
                fields, stdWithPosCriteria);

        ReportQueryIterator stdWithPosIterator = getBroker().getReportQueryIteratorByQuery(studentWithPosQuery);

        try {
            final int FIELD_STUDENT_OID = 0;
            final int FIELD_PROGRAM_OID = 1;

            while (stdWithPosIterator.hasNext()) {
                Object[] currentFields = (Object[]) stdWithPosIterator.next();

                String studentOid = (String) currentFields[FIELD_STUDENT_OID];
                String programOid = (String) currentFields[FIELD_PROGRAM_OID];

                stdPos.put(studentOid, programOid);
            }
        } finally {
            stdWithPosIterator.close();
        }

        return stdPos;
    }

    /**
     * Lookup field aliases and paths.
     * Get data from input definition
     */
    private void initializeFields() {
        m_fieldCipCode = translateAliasToJavaName(ALIAS_CIP_NUMBER, true);
        m_fieldCreditHrs = translateAliasToJavaName(ALIAS_DOE_CREDIT_HRS, true);
        m_fieldStdGradDate = translateAliasToJavaName(ALIAS_STD_GRAD_DATE, true);
        m_fieldEnrSchoolEnrStatus = translateAliasToJavaName(ALIAS_ENR_SCHOOL_ENR_STATUS, true);
        m_fieldAsmInternship = getStdAssesmentFieldByAlias(ALIAS_CTE_INTERNSHIP, true);
        m_fieldAsmApprentice = getStdAssesmentFieldByAlias(ALIAS_CTE_APPRENTICE, true);
        m_fieldAsmCoopWork = getStdAssesmentFieldByAlias(ALIAS_CTE_CO_OP_WORK, true);
        m_fieldAsmJobExplr = getStdAssesmentFieldByAlias(ALIAS_CTE_JOB_EXPLR, true);
        m_fieldAgExp = getStdAssesmentFieldByAlias(ALIAS_CTE_AG_EXPERIENCE, true);
        m_fieldAsmSklsEnterprise = getStdAssesmentFieldByAlias(ALIAS_CTE_SKLS_ENTERPRISE, true);
        m_fieldAsmWorkBasedInd = getStdAssesmentFieldByAlias(ALIAS_CTE_WB_EXP, true);
        m_fieldAsmPostSecCredits = getStdAssesmentFieldByAlias(ALIAS_CTE_CML_POSTSEC_CREDITS, true);
    }

    /**
     * Register custom fieldRetrievers.
     */
    private void registerFieldRetrievers() {
        HashMap calcs = new HashMap<String, FieldRetriever>();
        calcs.put(CTE_CALC, new RetrieveCTEInfo());
        super.addCalcs(calcs);
    }
}
